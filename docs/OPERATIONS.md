# Operations Guide — VSAM Pervasive Encryption Key Rotation

## Table of Contents

1. [System Requirements](#system-requirements)
2. [Pre-Requisites](#pre-requisites)
3. [Upload the Script to the Mainframe](#upload-the-script-to-the-mainframe)
4. [Allocate the REXX Library](#allocate-the-rexx-library)
5. [Prepare the Input Dataset](#prepare-the-input-dataset)
6. [Execute via TSO (Interactive)](#execute-via-tso-interactive)
7. [Execute via Batch JCL](#execute-via-batch-jcl)
8. [Review the Output Report](#review-the-output-report)

---

## System Requirements

| Component | Requirement |
|-----------|-------------|
| Operating System | IBM z/OS 2.3 or later |
| TSO/E | Required for interactive execution |
| IDCAMS | Must be available under TSO and in batch |
| RACF | Required for key label management and dataset access |
| Pervasive Encryption | z/OS Data Set Encryption must be enabled |
| Storage Classes | SMS storage classes mapped to encryption key labels must be defined |

---

## Pre-Requisites

Before running the script the following must be in place:

### RACF Key Labels

The target encryption key labels must already be defined in RACF. The script derives the key year from the key label name by extracting a 4-digit numeric component. For example, a label named `ICSF.KEYRING.2025.AES` would yield year `2025`.

Verify a key label exists:

```
RACDCERT LISTRING(ICSF.KEYRING.2025.AES) ID(your-racf-id)
```

### SMS Storage Classes

A storage class that maps to the target encryption key label must be defined. Work with your storage administrator to confirm the correct storage class name to use for the target key year before building the input file.

### RACF Dataset Permissions

The TSO user or batch job ID executing the script must have:

- `READ` access to all source VSAM datasets in the input list
- `ALTER` access to all source VSAM datasets (required for rename/delete)
- `ALTER` access to the high-level qualifier used for temporary datasets (`DSNAME.#NE`, `DSNAME.#OL`)
- `UPDATE` access to write the output report dataset

---

## Upload the Script to the Mainframe

The script must reside in a partitioned dataset (PDS) on the mainframe. Choose one of the upload methods below.

### Option A — FTP

From a workstation with network access to the mainframe:

```
ftp <mainframe-hostname>
```

Once connected, switch to ASCII mode and upload:

```ftp
ascii
quote site lrecl=80 recfm=fb blksize=3200
put peupdate.rexx 'YOUR.HLQ.REXLLIB(PEUPDATE)'
quit
```

> Use `ascii` mode (not `binary`) so that line endings are converted correctly for z/OS.

### Option B — SFTP / SCP (z/OS OpenSSH)

If z/OS OpenSSH is configured:

```bash
sftp user@mainframe-hostname
put peupdate.rexx //'YOUR.HLQ.REXLLIB(PEUPDATE)'
```

Or using `scp`:

```bash
scp peupdate.rexx user@mainframe-hostname://'YOUR.HLQ.REXLLIB(PEUPDATE)'
```

### Option C — IND$FILE (3270 Terminal Emulator)

From IBM Personal Communications, Micro Focus Rumba, or similar:

1. Open a TSO session
2. Use the terminal emulator's **Send File** function
3. Set transfer options:
   - **Mode:** Text (ASCII → EBCDIC conversion)
   - **Host file:** `YOUR.HLQ.REXLLIB(PEUPDATE)`
   - **Record format:** Fixed
   - **LRECL:** 80
4. Select `peupdate.rexx` from your workstation and send

---

## Allocate the REXX Library

If the target PDS does not already exist, allocate it first. Run the following JCL:

```jcl
//ALLOCLIB JOB (ACCOUNT),'ALLOC REXX LIB',CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID
//STEP1    EXEC PGM=IEFBR14
//REXLLIB  DD DSN=YOUR.HLQ.REXLLIB,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,5,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200,DSORG=PO)
```

Replace `YOUR.HLQ.REXLLIB` with your site's high-level qualifier and library name.

---

## Prepare the Input Dataset

The script reads from a sequential dataset. The first record must be the 4-digit target key year, followed by one VSAM cluster name per record. `.DATA` and `.INDEX` suffixes are stripped automatically.

### Allocate the input dataset

```jcl
//ALLOCIN  JOB (ACCOUNT),'ALLOC INPUT DS',CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID
//STEP1    EXEC PGM=IEFBR14
//INFILE   DD DSN=YOUR.HLQ.KEYROT.INPUT,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(1,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
```

### Populate the input dataset

Use ISPF Edit (option 2) to open `YOUR.HLQ.KEYROT.INPUT` and enter content in this format:

```
2025
PROD.PAYROLL.VSAM
PROD.CUSTOMER.MASTER
PROD.CLAIMS.KSDS
PROD.ARCHIVE.ESDS
```

- Line 1: target key year (4 digits, must be numeric)
- Lines 2+: fully-qualified VSAM cluster names (no quotes, no `.DATA`/`.INDEX` suffix)
- Blank lines are skipped
- Dataset names exceeding 40 characters are skipped with a warning

---

## Execute via TSO (Interactive)

### Add the REXX library to your SYSEXEC concatenation

```tso
ALLOCATE FILE(SYSEXEC) DA('YOUR.HLQ.REXLLIB') SHR REUSE
```

Or add it to your logon procedure's SYSEXEC/SYSPROC concatenation permanently.

### Run the script

```tso
%PEUPDATE YOUR.HLQ.KEYROT.INPUT YOUR.HLQ.KEYROT.REPORT
```

Or invoke it explicitly without relying on SYSEXEC:

```tso
EXEC 'YOUR.HLQ.REXLLIB(PEUPDATE)' 'YOUR.HLQ.KEYROT.INPUT YOUR.HLQ.KEYROT.REPORT'
```

Output is written to the TSO terminal as datasets are processed. The report dataset is created at the end of the run.

---

## Execute via Batch JCL

Running in batch is the preferred method for large input lists or scheduled key rotation jobs.

### Standard batch execution

```jcl
//VSAMKEY  JOB (ACCOUNT),'VSAM KEY ROTATION',CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID,REGION=0M
//*
//* VSAM Pervasive Encryption Key Rotation
//* Reads:  YOUR.HLQ.KEYROT.INPUT   (first line = target year,
//*                                   remaining lines = dataset names)
//* Writes: YOUR.HLQ.KEYROT.REPORT  (results report)
//*
//STEP1    EXEC PGM=IKJEFT01,DYNAMNBR=200
//STEPLIB  DD DISP=SHR,DSN=SYS1.LINKLIB
//SYSEXEC  DD DISP=SHR,DSN=YOUR.HLQ.REXLLIB
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSTSIN  DD *
  %PEUPDATE YOUR.HLQ.KEYROT.INPUT YOUR.HLQ.KEYROT.REPORT
/*
```

### With pre-allocated input and report datasets

If the input dataset is already populated and you want to pre-allocate the report:

```jcl
//VSAMKEY  JOB (ACCOUNT),'VSAM KEY ROTATION',CLASS=A,MSGCLASS=X,
//         NOTIFY=&SYSUID,REGION=0M
//*-------------------------------------------------------------------
//* STEP 0: Pre-allocate the output report dataset
//*-------------------------------------------------------------------
//ALLOCRPT EXEC PGM=IEFBR14
//REPORT   DD DSN=YOUR.HLQ.KEYROT.REPORT,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(5,5)),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)
//*-------------------------------------------------------------------
//* STEP 1: Run key rotation
//*-------------------------------------------------------------------
//KEYROT   EXEC PGM=IKJEFT01,DYNAMNBR=200,
//         COND=(0,NE,ALLOCRPT)
//SYSEXEC  DD DISP=SHR,DSN=YOUR.HLQ.REXLLIB
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSTSIN  DD *
  %PEUPDATE YOUR.HLQ.KEYROT.INPUT YOUR.HLQ.KEYROT.REPORT
/*
```

> **Note:** `DYNAMNBR=200` is required because the script performs dynamic dataset allocations (ALLOC/FREE) for each VSAM dataset processed. Increase this value if processing very large input lists.

### JCL parameters to adjust per site

| Parameter | Description |
|-----------|-------------|
| `(ACCOUNT)` | Replace with your site's job accounting information |
| `CLASS=A` | Job class — adjust to match your site's batch class |
| `YOUR.HLQ.REXLLIB` | PDS containing `PEUPDATE` member |
| `YOUR.HLQ.KEYROT.INPUT` | Input dataset with target year and dataset list |
| `YOUR.HLQ.KEYROT.REPORT` | Output report dataset (created by the script) |

---

## Review the Output Report

After the job completes, view the report dataset from ISPF:

```tso
BROWSE 'YOUR.HLQ.KEYROT.REPORT'
```

Or from ISPF option 1 (View) / option 2 (Edit).

The report is structured as follows:

```
================================================================================
VSAM PERVASIVE ENCRYPTION KEY ROTATION REPORT
Target Key Year: 2025
Run Date: 2025-06-01  Time: 14:32:10
================================================================================

SUCCESSFULLY CONVERTED (Count: N)
--------------------------------------------------------------------------------
PROD.PAYROLL.VSAM - KSDS - Converted from 2024 to 2025
...

SKIPPED - ALREADY CURRENT (Count: N)
...

SKIPPED - NOT ENCRYPTED (Count: N)
...

SKIPPED - IN USE (Count: N)
...

FAILED CONVERSIONS (Count: N)
...

SUMMARY
--------------------------------------------------------------------------------
Total Datasets Processed: N
Successful Conversions:   N
Already Current:          N
Not Encrypted:            N
In Use:                   N
Failed:                   N
================================================================================
```

### Return codes

| RC | Meaning |
|----|---------|
| 0  | Script completed normally |
| 8  | Fatal startup error (bad arguments, cannot open input file, invalid year) |

Individual dataset failures are captured in the report and do not affect the overall return code. Review the **FAILED CONVERSIONS** section and `SYSTSPRT` sysout for details on any failures.

### Critical failure — manual recovery

If a dataset rename fails at Step 4, the script stops processing that dataset and logs a critical error. In this state:

- Original data is preserved in `DSNAME.#OL`
- New encrypted data is in `DSNAME.#NE`
- The original dataset name (`DSNAME`) may not exist

To recover manually:

```tso
/* Option 1: restore original and retry later */
ALTER 'DSNAME.#OL' NEWNAME('DSNAME')
DELETE 'DSNAME.#NE' CLUSTER PURGE

/* Option 2: complete the conversion manually */
ALTER 'DSNAME.#NE' NEWNAME('DSNAME')
DELETE 'DSNAME.#OL' CLUSTER PURGE
```

Run these via IDCAMS (TSO or JCL) after confirming data integrity in the `#NE` copy.

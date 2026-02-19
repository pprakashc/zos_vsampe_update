# zos_vsampe_update

REXX script for bulk VSAM Pervasive Encryption (PE) key rotation on IBM z/OS mainframe systems.

## Overview

`peupdate.rexx` automates the process of rotating encryption keys across a list of VSAM datasets. It inspects each dataset's current encryption key label, compares it against a target key year, and re-encrypts those that are out of date by defining a new cluster, copying the data, and renaming datasets atomically.

## Requirements

- IBM z/OS with TSO/E
- IDCAMS (Access Method Services) available under TSO
- RACF (or equivalent) key labels configured for the target encryption year
- Appropriate RACF permissions to allocate, define, copy, rename, and delete VSAM datasets

## Usage

Run from TSO as a REXX exec:

```
TSO %VSAMKEY input.dataset output.report
```

**Arguments:**

| Argument | Description |
|----------|-------------|
| `input.dataset` | A sequential dataset containing the target key year on line 1, followed by one VSAM cluster name per line |
| `output.report` | Name of the report dataset to be created with results |

**Input file format:**

```
2025
PROD.PAYROLL.VSAM
PROD.CUSTOMER.DATA
PROD.CLAIMS.KSDS
```

## Conversion Process

For each dataset in the input list, the script performs these checks before converting:

1. **Existence check** — runs `LISTCAT` to confirm the dataset is cataloged
2. **Contention check** — attempts an exclusive allocation; skips if the dataset is in use
3. **Encryption check** — skips datasets that are not encrypted
4. **Key year check** — skips datasets already using the target key year

If all checks pass, the conversion runs in four steps:

| Step | Action |
|------|--------|
| 1 | Define a new VSAM cluster (`DSNAME.#NE`) with identical attributes under the target storage class |
| 2 | Copy all records from the original to the new cluster using IDCAMS `REPRO` (skipped if empty) |
| 3 | Rename the original cluster to `DSNAME.#OL` (backup) |
| 4 | Rename `DSNAME.#NE` to the original name |

> **Critical failure:** If Step 4 fails, the script halts and flags a manual recovery situation. The original data will be in `DSNAME.#OL` and the new data in `DSNAME.#NE`.

## Supported Dataset Types

| Type | Description |
|------|-------------|
| KSDS | Key-Sequenced Data Set (with index) |
| ESDS | Entry-Sequenced Data Set (non-indexed) |
| RRDS | Relative-Record Data Set (numbered) |
| LDS  | Linear Data Set |
| ZFS  | z/OS File System dataset |

All attributes (CI size, record size, key length/position, freespace, shareoptions, space allocation, SPEED/RECOVERY, REUSE, UNIQUE, ERASE) are read from the existing dataset via `LISTCAT ALL` and used to recreate the cluster identically.

## Output Report

After processing all datasets, a categorized report is written to the specified output dataset with the following sections:

- **Successfully Converted** — datasets re-encrypted with the new key year
- **Skipped - Already Current** — datasets already using the target key year
- **Skipped - Not Encrypted** — datasets with pervasive encryption disabled
- **Skipped - In Use** — datasets that could not be exclusively allocated
- **Failed Conversions** — datasets where a step in the conversion failed
- **Summary** — totals for all categories

## Repository Layout

```
.
├── src/
│   └── peupdate.rexx    # Main REXX script
├── docs/                # Additional documentation
└── README.md
```

## Contributing

See `CONTRIBUTING.md` for guidelines.

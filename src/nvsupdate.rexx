/* REXX */
/**********************************************************************/
/* NON-VSAM PERVASIVE ENCRYPTION KEY ROTATION                        */
/*                                                                    */
/* Supported types: PS (sequential), PDS, PDSE                       */
/*                                                                    */
/* Conversion process per dataset:                                    */
/*   1. Rename original  -> DSNAME.#OL  (backup)                     */
/*   2. ALLOC new        -> DSNAME.#NE  LIKE(#OL) STORCLAS(target)   */
/*      SMS inherits space/DCB from model and applies new key label   */
/*   3. Copy #OL -> #NE  (IEBGENER for PS, IEBCOPY for PDS/PDSE)    */
/*      Skipped if dataset is empty                                   */
/*   4. Rename DSNAME.#NE -> original name                           */
/*                                                                    */
/* On success: DSNAME.#OL remains as backup - delete manually        */
/* On failure: cleanup performed where possible; critical failures    */
/*             require manual recovery (see report)                   */
/**********************************************************************/

ARG INPUT_FILE OUTPUT_RPT

IF INPUT_FILE = '' | OUTPUT_RPT = '' THEN DO
   SAY 'Usage: TSO %NVSUPDT input.dataset output.report'
   SAY ''
   SAY 'Input file format (line 1 = target year and storage class):'
   SAY '  2025 ENCSC25'
   SAY '  PROD.PAYROLL.QSAM'
   SAY '  PROD.MEMBER.PDS'
   EXIT 8
END

/* Initialize counters */
TOTAL_COUNT     = 0
SUCCESS_COUNT   = 0
ALREADY_CURRENT = 0
NOT_ENCRYPTED   = 0
IN_USE_COUNT    = 0
FAILED_COUNT    = 0
SKIPPED_COUNT   = 0

SUCCESS_LIST. = ''
ALREADY_LIST. = ''
NOT_ENC_LIST. = ''
IN_USE_LIST.  = ''
FAILED_LIST.  = ''
SKIPPED_LIST. = ''

/* Get timestamp */
PARSE VALUE DATE('S') TIME() WITH START_DATE START_TIME .
RUN_DATE = SUBSTR(START_DATE,1,4)'-'SUBSTR(START_DATE,5,2)'-'SUBSTR(START_DATE,7,2)
RUN_TIME = SUBSTR(START_TIME,1,2)':'SUBSTR(START_TIME,4,2)':'SUBSTR(START_TIME,7,2)

SAY '======================================================================='
SAY 'NON-VSAM PERVASIVE ENCRYPTION KEY ROTATION'
SAY 'Started: 'RUN_DATE' 'RUN_TIME
SAY '======================================================================='
SAY ''

/* Read input file */
SAY 'Reading input file: 'INPUT_FILE
ADDRESS TSO "ALLOC F(INFILE) DA('"INPUT_FILE"') SHR REUSE"
IF RC <> 0 THEN DO
   SAY 'ERROR: Cannot allocate input file. RC='RC
   EXIT 8
END

ADDRESS TSO "EXECIO * DISKR INFILE (STEM INPUT. FINIS"
ADDRESS TSO "FREE F(INFILE)"

IF RC <> 0 THEN DO
   SAY 'ERROR: Cannot read input file. RC='RC
   EXIT 8
END

/* Parse line 1: TARGET_YEAR TARGET_STORCLAS */
PARSE VALUE STRIP(INPUT.1) WITH TARGET_YEAR TARGET_STORCLAS .

IF DATATYPE(TARGET_YEAR) <> 'NUM' | LENGTH(TARGET_YEAR) <> 4 THEN DO
   SAY 'ERROR: Invalid target year on line 1: 'TARGET_YEAR
   SAY '       Expected format: YEAR STORCLAS (e.g. 2025 ENCSC25)'
   EXIT 8
END

IF TARGET_STORCLAS = '' THEN DO
   SAY 'ERROR: Target storage class missing from line 1'
   SAY '       Expected format: YEAR STORCLAS (e.g. 2025 ENCSC25)'
   EXIT 8
END

SAY 'Target encryption key year:   'TARGET_YEAR
SAY 'Target SMS storage class:     'TARGET_STORCLAS
SAY ''

IF INPUT.0 < 2 THEN DO
   SAY 'ERROR: No dataset names found in input file'
   EXIT 8
END

/* Process each dataset */
DO I = 2 TO INPUT.0
   DSNAME = STRIP(INPUT.I)

   IF DSNAME = '' THEN ITERATE

   IF LENGTH(DSNAME) > 44 THEN DO
      SAY 'WARNING: Dataset name exceeds 44 characters, skipping: 'DSNAME
      ITERATE
   END

   TOTAL_COUNT = TOTAL_COUNT + 1

   SAY 'Processing ['TOTAL_COUNT']: 'DSNAME

   CALL PROCESS_DATASET DSNAME

   SAY ''
END

/* Generate report */
CALL GENERATE_REPORT

SAY ''
SAY '======================================================================='
SAY 'Processing complete. Report written to: 'OUTPUT_RPT
SAY '======================================================================='

EXIT 0

/**********************************************************************/
/* PROCESS_DATASET                                                    */
/**********************************************************************/
PROCESS_DATASET: PROCEDURE EXPOSE SUCCESS_COUNT ALREADY_CURRENT,
                 NOT_ENCRYPTED IN_USE_COUNT FAILED_COUNT SKIPPED_COUNT,
                 SUCCESS_LIST. ALREADY_LIST. NOT_ENC_LIST.,
                 IN_USE_LIST. FAILED_LIST. SKIPPED_LIST.,
                 TARGET_YEAR TARGET_STORCLAS DS_INFO.

   DSNAME = ARG(1)

   /* Get dataset info: encryption status, DSORG, empty check */
   CALL GET_DATASET_INFO DSNAME

   IF DS_INFO.EXISTS <> 'YES' THEN DO
      FAILED_COUNT = FAILED_COUNT + 1
      FAILED_LIST.FAILED_COUNT = DSNAME' - Dataset does not exist or is not cataloged'
      SAY '  FAILED: Dataset does not exist'
      RETURN
   END

   /* Redirect VSAM datasets to peupdate.rexx */
   IF DS_INFO.VSAM = 'YES' THEN DO
      SKIPPED_COUNT = SKIPPED_COUNT + 1
      SKIPPED_LIST.SKIPPED_COUNT = DSNAME' - VSAM dataset (use peupdate.rexx)'
      SAY '  SKIPPED: VSAM dataset - use peupdate.rexx instead'
      RETURN
   END

   /* Only PS and PO supported */
   IF DS_INFO.DSORG <> 'PS' & DS_INFO.DSORG <> 'PO' THEN DO
      SKIPPED_COUNT = SKIPPED_COUNT + 1
      SKIPPED_LIST.SKIPPED_COUNT = DSNAME' - Unsupported DSORG ('DS_INFO.DSORG')'
      SAY '  SKIPPED: Unsupported dataset organization ('DS_INFO.DSORG')'
      RETURN
   END

   /* Check encryption */
   IF DS_INFO.ENCRYPTED <> 'YES' THEN DO
      NOT_ENCRYPTED = NOT_ENCRYPTED + 1
      NOT_ENC_LIST.NOT_ENCRYPTED = DSNAME
      SAY '  SKIPPED: Dataset not encrypted'
      RETURN
   END

   /* Display current state */
   IF DS_INFO.DSORG = 'PO' THEN
      SAY '  Type: 'DS_INFO.DSORG' ('DS_INFO.DSNTYPE')  Members: 'DS_INFO.MEMBERS
   ELSE
      SAY '  Type: 'DS_INFO.DSORG'  Empty: 'DS_INFO.EMPTY
   SAY '  Current key year: 'DS_INFO.KEY_YEAR', Target year: 'TARGET_YEAR

   /* Already on target key */
   IF DS_INFO.KEY_YEAR = TARGET_YEAR THEN DO
      ALREADY_CURRENT = ALREADY_CURRENT + 1
      ALREADY_LIST.ALREADY_CURRENT = DSNAME
      SAY '  SKIPPED: Already using target key year'
      RETURN
   END

   /* Check if dataset is in use */
   IF DS_INFO.DSORG = 'PS' THEN
      ADDRESS TSO "ALLOC F(TESTDD) DA('"DSNAME"') OLD REUSE"
   ELSE
      ADDRESS TSO "ALLOC F(TESTDD) DA('"DSNAME"') SHR REUSE"

   IF RC <> 0 THEN DO
      IN_USE_COUNT = IN_USE_COUNT + 1
      IN_USE_LIST.IN_USE_COUNT = DSNAME
      SAY '  SKIPPED: Dataset in use'
      RETURN
   END
   ADDRESS TSO "FREE F(TESTDD)"

   /* Perform conversion */
   SAY '  Starting conversion...'
   CALL CONVERT_DATASET DSNAME

   IF RESULT = 0 THEN DO
      SUCCESS_COUNT = SUCCESS_COUNT + 1
      IF DS_INFO.DSORG = 'PO' THEN
         DS_TYPE_DESC = DS_INFO.DSORG'/'DS_INFO.DSNTYPE
      ELSE
         DS_TYPE_DESC = DS_INFO.DSORG
      SUCCESS_LIST.SUCCESS_COUNT = DSNAME' - 'DS_TYPE_DESC,
         ' - Converted from 'DS_INFO.KEY_YEAR' to 'TARGET_YEAR
      SAY '  SUCCESS: Conversion completed'
   END
   ELSE DO
      FAILED_COUNT = FAILED_COUNT + 1
      FAILED_LIST.FAILED_COUNT = DSNAME' - Conversion failed'
      SAY '  FAILED: Conversion failed'
   END

   RETURN

/**********************************************************************/
/* GET_DATASET_INFO - LISTCAT for encryption, LISTDSI for type/empty */
/**********************************************************************/
GET_DATASET_INFO: PROCEDURE EXPOSE DS_INFO.

   DSNAME = ARG(1)

   /* Initialize */
   DS_INFO.          = ''
   DS_INFO.EXISTS    = 'NO'
   DS_INFO.VSAM      = 'NO'
   DS_INFO.ENCRYPTED = 'NO'
   DS_INFO.KEY_YEAR  = ''
   DS_INFO.DSORG     = ''
   DS_INFO.DSNTYPE   = ''
   DS_INFO.EMPTY     = 'NO'
   DS_INFO.MEMBERS   = '0'

   /* --- LISTCAT ALL to check existence and get encryption info --- */
   ADDRESS TSO "ALLOC F(SYSIN) UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
               "RECFM(F,B) LRECL(80) BLKSIZE(80)"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(10,5) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"

   QUEUE " LISTCAT ENTRIES('"DSNAME"') ALL"
   ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"
   ADDRESS TSO "CALL *(IDCAMS)"
   LISTCAT_RC = RC
   ADDRESS TSO "EXECIO * DISKR SYSPRINT (STEM LC. FINIS"
   ADDRESS TSO "FREE F(SYSIN,SYSPRINT)"

   IF LISTCAT_RC <> 0 THEN DO
      DS_INFO.EXISTS = 'NO'
      RETURN
   END

   DS_INFO.EXISTS = 'YES'

   /* Parse LISTCAT output */
   DO I = 1 TO LC.0
      LINE = STRIP(LC.I)

      /* Detect VSAM entries */
      IF POS('CLUSTER -------',LINE) > 0 | POS('AIX -------',LINE) > 0 THEN
         DS_INFO.VSAM = 'YES'

      /* Encryption flag */
      IF POS('DATA SET ENCRYPTION',LINE) > 0 THEN DO
         IF POS('YES',LINE) > 0 THEN DS_INFO.ENCRYPTED = 'YES'
         ELSE                        DS_INFO.ENCRYPTED = 'NO'
      END

      /* Key label -> extract year */
      IF POS('DATA SET KEY LABEL',LINE) > 0 THEN DO
         START_POS = POS('DATA SET KEY LABEL',LINE) + LENGTH('DATA SET KEY LABEL')
         TEMP = SUBSTR(LINE,START_POS)
         KEY_LABEL = ''
         DO J = 1 TO LENGTH(TEMP)
            CHAR = SUBSTR(TEMP,J,1)
            IF CHAR <> '-' & CHAR <> ' ' THEN DO
               KEY_LABEL = SUBSTR(TEMP,J)
               LEAVE
            END
         END
         DS_INFO.KEY_YEAR = EXTRACT_YEAR(STRIP(KEY_LABEL))
      END
   END

   /* --- LISTDSI for DSORG, DSNTYPE, and empty check --- */
   X = LISTDSI("'"DSNAME"'" "SMSINFO NORECALL")

   IF X <= 4 THEN DO                     /* 0=ok, 4=warning, 8+=error */
      DS_INFO.DSORG   = STRIP(SYSDSORG)
      DS_INFO.DSNTYPE = STRIP(SYSDSNTYPE) /* LIBRARY=PDSE, blank=PDS or PS */
      DS_INFO.MEMBERS = STRIP(SYSMEMBERS)

      IF DS_INFO.DSORG = 'PS' THEN DO
         IF SYSUSED = 0 THEN DS_INFO.EMPTY = 'YES'
      END
      ELSE IF DS_INFO.DSORG = 'PO' THEN DO
         IF SYSMEMBERS = 0 THEN DS_INFO.EMPTY = 'YES'
      END
   END

   RETURN

/**********************************************************************/
/* CONVERT_DATASET - Rename, allocate LIKE, copy, rename back        */
/**********************************************************************/
CONVERT_DATASET: PROCEDURE EXPOSE DS_INFO. TARGET_STORCLAS,
                 FAILED_LIST. FAILED_COUNT

   DSNAME  = ARG(1)
   OLDNAME = DSNAME'.#OL'
   NEWNAME = DSNAME'.#NE'

   /* Step 1: Rename original -> #OL */
   SAY '  Step 1: Renaming 'DSNAME' to 'OLDNAME
   CALL RENAME_DATASET OLDNAME OLDNAME  /* dummy init */
   CALL RENAME_DATASET DSNAME OLDNAME
   IF RESULT <> 0 THEN DO
      SAY '  ERROR: Initial rename failed - dataset is unchanged'
      RETURN 8
   END

   /* Step 2: Allocate #NE using LIKE(#OL) - SMS applies target storage class */
   SAY '  Step 2: Allocating 'NEWNAME
   SAY '          LIKE('OLDNAME') STORCLAS('TARGET_STORCLAS')'
   ADDRESS TSO "ALLOC F(NEWDSDD) DSN('"NEWNAME"') NEW CATALOG",
               "LIKE('"OLDNAME"') STORCLAS("TARGET_STORCLAS")"
   ALLOC_RC = RC
   ADDRESS TSO "FREE F(NEWDSDD)"

   IF ALLOC_RC <> 0 THEN DO
      SAY '  ERROR: Allocation of new dataset failed (RC='ALLOC_RC')'
      SAY '  Restoring original name...'
      CALL RENAME_DATASET OLDNAME DSNAME
      RETURN 8
   END

   /* Step 3: Copy data (skip if empty) */
   IF DS_INFO.EMPTY = 'YES' THEN DO
      SAY '  Step 3: Dataset is empty - skipping copy'
   END
   ELSE DO
      IF DS_INFO.DSORG = 'PS' THEN DO
         SAY '  Step 3: Copying sequential data using IEBGENER...'
         CALL COPY_PS OLDNAME NEWNAME
      END
      ELSE DO
         SAY '  Step 3: Copying 'DS_INFO.MEMBERS' member(s) using IEBCOPY...'
         CALL COPY_PDS OLDNAME NEWNAME
      END

      IF RESULT <> 0 THEN DO
         SAY '  ERROR: Copy failed - cleaning up and restoring original...'
         /* Delete the incomplete #NE */
         ADDRESS TSO "ALLOC F(SYSIN) UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
                     "RECFM(F,B) LRECL(80) BLKSIZE(800)"
         ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS",
                     "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"
         QUEUE " DELETE '"NEWNAME"' PURGE"
         ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"
         ADDRESS TSO "CALL *(IDCAMS)"
         ADDRESS TSO "FREE F(SYSIN,SYSPRINT)"
         /* Restore original name */
         CALL RENAME_DATASET OLDNAME DSNAME
         RETURN 8
      END
   END

   /* Step 4: Rename #NE -> original name */
   SAY '  Step 4: Renaming 'NEWNAME' to 'DSNAME
   CALL RENAME_DATASET NEWNAME DSNAME
   IF RESULT <> 0 THEN DO
      SAY '  CRITICAL ERROR: Final rename failed!'
      SAY '  MANUAL INTERVENTION REQUIRED:'
      SAY '    - Original data is in: 'OLDNAME
      SAY '    - New encrypted data is in: 'NEWNAME
      SAY '    - Run: ALTER (table below) to complete or roll back'
      RETURN 8
   END

   SAY '  Conversion completed successfully'
   SAY '  Backup retained at: 'OLDNAME' (delete when satisfied)'
   RETURN 0

/**********************************************************************/
/* RENAME_DATASET - Rename cataloged non-VSAM dataset via IDCAMS     */
/**********************************************************************/
RENAME_DATASET: PROCEDURE

   OLDNAME = ARG(1)
   NEWNAME = ARG(2)

   ADDRESS TSO "ALLOC F(SYSIN) UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
               "RECFM(F,B) LRECL(80) BLKSIZE(800)"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"

   QUEUE " ALTER '"OLDNAME"' NEWNAME('"NEWNAME"')"
   ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"

   ADDRESS TSO "CALL *(IDCAMS)"
   IDCAMS_RC = RC

   IF IDCAMS_RC <> 0 THEN DO
      SAY '  ERROR: RENAME 'OLDNAME' -> 'NEWNAME' failed (RC='IDCAMS_RC')'
      ADDRESS TSO "EXECIO * DISKR SYSPRINT (STEM ERR. FINIS"
      DO I = 1 TO ERR.0
         IF STRIP(ERR.I) <> '' THEN SAY '    'ERR.I
      END
   END

   ADDRESS TSO "FREE F(SYSIN,SYSPRINT)"
   RETURN IDCAMS_RC

/**********************************************************************/
/* COPY_PS - Copy sequential dataset using IEBGENER                  */
/**********************************************************************/
COPY_PS: PROCEDURE

   SOURCE = ARG(1)
   TARGET = ARG(2)

   ADDRESS TSO "ALLOC F(SYSUT1)   DA('"SOURCE"') SHR REUSE"
   ADDRESS TSO "ALLOC F(SYSUT2)   DA('"TARGET"') OLD REUSE"
   ADDRESS TSO "ALLOC F(SYSIN)    DUMMY"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"

   ADDRESS TSO "CALL *(IEBGENER)"
   COPY_RC = RC

   IF COPY_RC <> 0 THEN DO
      SAY '  ERROR: IEBGENER failed (RC='COPY_RC')'
      ADDRESS TSO "EXECIO * DISKR SYSPRINT (STEM ERR. FINIS"
      DO I = 1 TO ERR.0
         IF STRIP(ERR.I) <> '' THEN SAY '    'ERR.I
      END
   END

   ADDRESS TSO "FREE F(SYSUT1,SYSUT2,SYSIN,SYSPRINT)"
   RETURN COPY_RC

/**********************************************************************/
/* COPY_PDS - Copy PDS or PDSE using IEBCOPY                         */
/**********************************************************************/
COPY_PDS: PROCEDURE

   SOURCE = ARG(1)
   TARGET = ARG(2)

   ADDRESS TSO "ALLOC F(SYSIN)    UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
               "RECFM(F,B) LRECL(80) BLKSIZE(800)"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"
   ADDRESS TSO "ALLOC F(SYSUT1)   DA('"SOURCE"') SHR REUSE"
   ADDRESS TSO "ALLOC F(SYSUT2)   DA('"TARGET"') OLD REUSE"
   ADDRESS TSO "ALLOC F(SYSUT3)   UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS"
   ADDRESS TSO "ALLOC F(SYSUT4)   UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS"

   QUEUE " COPY OUTDD=SYSUT2,INDD=SYSUT1"
   ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"

   ADDRESS TSO "CALL *(IEBCOPY)"
   COPY_RC = RC

   IF COPY_RC <> 0 THEN DO
      SAY '  ERROR: IEBCOPY failed (RC='COPY_RC')'
      ADDRESS TSO "EXECIO * DISKR SYSPRINT (STEM ERR. FINIS"
      DO I = 1 TO ERR.0
         IF STRIP(ERR.I) <> '' THEN SAY '    'ERR.I
      END
   END

   ADDRESS TSO "FREE F(SYSIN,SYSPRINT,SYSUT1,SYSUT2,SYSUT3,SYSUT4)"
   RETURN COPY_RC

/**********************************************************************/
/* EXTRACT_YEAR - Extract 4-digit year from key label                */
/**********************************************************************/
EXTRACT_YEAR: PROCEDURE

   KEY_LABEL = ARG(1)
   YEAR = ''
   TEMP = KEY_LABEL

   DO WHILE LENGTH(TEMP) > 0
      PARSE VAR TEMP COMPONENT '.' TEMP
      COMPONENT = STRIP(COMPONENT)
      IF LENGTH(COMPONENT) = 4 & DATATYPE(COMPONENT) = 'NUM' THEN
         YEAR = COMPONENT
   END

   IF LENGTH(TEMP) = 4 & DATATYPE(TEMP) = 'NUM' THEN
      YEAR = TEMP

   RETURN YEAR

/**********************************************************************/
/* GENERATE_REPORT                                                    */
/**********************************************************************/
GENERATE_REPORT: PROCEDURE EXPOSE OUTPUT_RPT,
                 SUCCESS_COUNT ALREADY_CURRENT NOT_ENCRYPTED,
                 IN_USE_COUNT FAILED_COUNT SKIPPED_COUNT TOTAL_COUNT,
                 SUCCESS_LIST. ALREADY_LIST. NOT_ENC_LIST.,
                 IN_USE_LIST. FAILED_LIST. SKIPPED_LIST.,
                 TARGET_YEAR TARGET_STORCLAS RUN_DATE RUN_TIME

   ADDRESS TSO "ALLOC F(REPORT) DA('"OUTPUT_RPT"') NEW CATALOG",
               "SPACE(5,5) TRACKS RECFM(F,B,A) LRECL(133) BLKSIZE(1330)"

   CALL WRITE_RPT COPIES('=',80)
   CALL WRITE_RPT 'NON-VSAM PERVASIVE ENCRYPTION KEY ROTATION REPORT'
   CALL WRITE_RPT 'Target Key Year:      'TARGET_YEAR
   CALL WRITE_RPT 'Target Storage Class: 'TARGET_STORCLAS
   CALL WRITE_RPT 'Run Date: 'RUN_DATE'  Time: 'RUN_TIME
   CALL WRITE_RPT COPIES('=',80)
   CALL WRITE_RPT ''

   CALL WRITE_RPT 'SUCCESSFULLY CONVERTED (Count: 'SUCCESS_COUNT')'
   CALL WRITE_RPT COPIES('-',80)
   IF SUCCESS_COUNT = 0 THEN CALL WRITE_RPT '(None)'
   ELSE DO I = 1 TO SUCCESS_COUNT
      CALL WRITE_RPT SUCCESS_LIST.I
   END
   CALL WRITE_RPT ''

   CALL WRITE_RPT 'SKIPPED - ALREADY CURRENT (Count: 'ALREADY_CURRENT')'
   CALL WRITE_RPT COPIES('-',80)
   IF ALREADY_CURRENT = 0 THEN CALL WRITE_RPT '(None)'
   ELSE DO I = 1 TO ALREADY_CURRENT
      CALL WRITE_RPT ALREADY_LIST.I' - Already using 'TARGET_YEAR' key'
   END
   CALL WRITE_RPT ''

   CALL WRITE_RPT 'SKIPPED - NOT ENCRYPTED (Count: 'NOT_ENCRYPTED')'
   CALL WRITE_RPT COPIES('-',80)
   IF NOT_ENCRYPTED = 0 THEN CALL WRITE_RPT '(None)'
   ELSE DO I = 1 TO NOT_ENCRYPTED
      CALL WRITE_RPT NOT_ENC_LIST.I' - Dataset not encrypted'
   END
   CALL WRITE_RPT ''

   CALL WRITE_RPT 'SKIPPED - IN USE (Count: 'IN_USE_COUNT')'
   CALL WRITE_RPT COPIES('-',80)
   IF IN_USE_COUNT = 0 THEN CALL WRITE_RPT '(None)'
   ELSE DO I = 1 TO IN_USE_COUNT
      CALL WRITE_RPT IN_USE_LIST.I' - Dataset in use'
   END
   CALL WRITE_RPT ''

   CALL WRITE_RPT 'SKIPPED - UNSUPPORTED TYPE OR VSAM (Count: 'SKIPPED_COUNT')'
   CALL WRITE_RPT COPIES('-',80)
   IF SKIPPED_COUNT = 0 THEN CALL WRITE_RPT '(None)'
   ELSE DO I = 1 TO SKIPPED_COUNT
      CALL WRITE_RPT SKIPPED_LIST.I
   END
   CALL WRITE_RPT ''

   CALL WRITE_RPT 'FAILED CONVERSIONS (Count: 'FAILED_COUNT')'
   CALL WRITE_RPT COPIES('-',80)
   IF FAILED_COUNT = 0 THEN CALL WRITE_RPT '(None)'
   ELSE DO I = 1 TO FAILED_COUNT
      CALL WRITE_RPT FAILED_LIST.I
   END
   CALL WRITE_RPT ''

   CALL WRITE_RPT 'SUMMARY'
   CALL WRITE_RPT COPIES('-',80)
   CALL WRITE_RPT 'Total Datasets Processed: 'TOTAL_COUNT
   CALL WRITE_RPT 'Successful Conversions:   'SUCCESS_COUNT
   CALL WRITE_RPT 'Already Current:          'ALREADY_CURRENT
   CALL WRITE_RPT 'Not Encrypted:            'NOT_ENCRYPTED
   CALL WRITE_RPT 'In Use:                   'IN_USE_COUNT
   CALL WRITE_RPT 'Skipped (type/VSAM):      'SKIPPED_COUNT
   CALL WRITE_RPT 'Failed:                   'FAILED_COUNT
   CALL WRITE_RPT COPIES('=',80)
   CALL WRITE_RPT 'NOTE: Backup datasets (DSNAME.#OL) are retained after successful'
   CALL WRITE_RPT '      conversions. Review and delete once satisfied with results.'
   CALL WRITE_RPT COPIES('=',80)

   ADDRESS TSO "EXECIO 0 DISKW REPORT (FINIS"
   ADDRESS TSO "FREE F(REPORT)"

   RETURN

/**********************************************************************/
/* WRITE_RPT - Write one line to the report dataset                  */
/**********************************************************************/
WRITE_RPT: PROCEDURE
   LINE = ARG(1)
   QUEUE LINE
   ADDRESS TSO "EXECIO 1 DISKW REPORT"
   RETURN

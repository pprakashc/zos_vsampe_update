/* REXX */
/**********************************************************************/
/* VSAM PERVASIVE ENCRYPTION KEY ROTATION                            */
/**********************************************************************/

/* Parse arguments */
ARG INPUT_FILE OUTPUT_RPT

IF INPUT_FILE = '' | OUTPUT_RPT = '' THEN DO
   SAY 'Usage: TSO %VSAMKEY input.dataset output.report'
   EXIT 8
END

/* Initialize counters */
TOTAL_COUNT = 0
SUCCESS_COUNT = 0
ALREADY_CURRENT = 0
NOT_ENCRYPTED = 0
IN_USE_COUNT = 0
FAILED_COUNT = 0

SUCCESS_LIST. = ''
ALREADY_LIST. = ''
NOT_ENC_LIST. = ''
IN_USE_LIST. = ''
FAILED_LIST. = ''

/* Get timestamp */
PARSE VALUE DATE('S') TIME() WITH START_DATE START_TIME .
RUN_DATE = SUBSTR(START_DATE,1,4)'-'SUBSTR(START_DATE,5,2)'-'SUBSTR(START_DATE,7,2)
RUN_TIME = SUBSTR(START_TIME,1,2)':'SUBSTR(START_TIME,4,2)':'SUBSTR(START_TIME,7,2)

SAY '======================================================================='
SAY 'VSAM PERVASIVE ENCRYPTION KEY ROTATION'
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

/* Get target key year from first line */
TARGET_YEAR = STRIP(SUBSTR(INPUT.1,1,4))
IF DATATYPE(TARGET_YEAR) <> 'NUM' | LENGTH(TARGET_YEAR) <> 4 THEN DO
   SAY 'ERROR: Invalid target year in first line: 'TARGET_YEAR
   EXIT 8
END

SAY 'Target encryption key year: 'TARGET_YEAR
SAY ''

IF INPUT.0 < 2 THEN DO
   SAY 'ERROR: No dataset names found in input file'
   EXIT 8
END

/* Process each dataset */
DO I = 2 TO INPUT.0
   DSNAME = STRIP(INPUT.I)
   
   IF DSNAME = '' THEN ITERATE
   
   /* Remove .DATA or .INDEX suffix */
   IF RIGHT(DSNAME,5) = '.DATA' THEN DSNAME = LEFT(DSNAME,LENGTH(DSNAME)-5)
   IF RIGHT(DSNAME,6) = '.INDEX' THEN DSNAME = LEFT(DSNAME,LENGTH(DSNAME)-6)
   
   IF LENGTH(DSNAME) > 40 THEN DO
      SAY 'WARNING: Dataset name exceeds 40 characters, skipping: 'DSNAME
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
                 NOT_ENCRYPTED IN_USE_COUNT FAILED_COUNT,
                 SUCCESS_LIST. ALREADY_LIST. NOT_ENC_LIST.,
                 IN_USE_LIST. FAILED_LIST. TARGET_YEAR VSAM_PARMS.

   DSNAME = ARG(1)
   
   /* Check dataset exists */
   CALL LISTCAT_DATASET DSNAME
   IF RESULT <> 0 THEN DO
      FAILED_COUNT = FAILED_COUNT + 1
      FAILED_LIST.FAILED_COUNT = DSNAME' - Dataset does not exist'
      SAY '  FAILED: Dataset does not exist'
      RETURN
   END
   
   /* Check contention */
   ADDRESS TSO "ALLOC F(TESTDD) DA('"DSNAME"') OLD REUSE"
   IF RC <> 0 THEN DO
      IN_USE_COUNT = IN_USE_COUNT + 1
      IN_USE_LIST.IN_USE_COUNT = DSNAME
      SAY '  SKIPPED: Dataset in use'
      RETURN
   END
   ADDRESS TSO "FREE F(TESTDD)"
   
   /* Check if encrypted and get key year */
   CALL GET_ENCRYPTION_INFO DSNAME

   IF ENCRYPTED = 'NO' THEN DO
      NOT_ENCRYPTED = NOT_ENCRYPTED + 1
      NOT_ENC_LIST.NOT_ENCRYPTED = DSNAME
      SAY '  SKIPPED: Dataset not encrypted'
      RETURN
   END
   
   SAY '  Current key year: 'KEY_YEAR', Target year: 'TARGET_YEAR
   
   IF KEY_YEAR = TARGET_YEAR THEN DO
      ALREADY_CURRENT = ALREADY_CURRENT + 1
      ALREADY_LIST.ALREADY_CURRENT = DSNAME
      SAY '  SKIPPED: Already using target key year'
      RETURN
   END
   
   /* Perform conversion */
   SAY '  Starting conversion process...'
   CALL CONVERT_VSAM DSNAME
   
   IF RESULT = 0 THEN DO
      SUCCESS_COUNT = SUCCESS_COUNT + 1
      SUCCESS_LIST.SUCCESS_COUNT = DSNAME' - 'VSAM_PARMS.TYPE' - Converted from 'KEY_YEAR' to 'TARGET_YEAR
      SAY '  SUCCESS: Conversion completed'
   END
   ELSE DO
      FAILED_COUNT = FAILED_COUNT + 1
      FAILED_LIST.FAILED_COUNT = DSNAME' - Conversion failed'
      SAY '  FAILED: Conversion failed'
   END
   
   RETURN

/**********************************************************************/
/* LISTCAT_DATASET - Run LISTCAT on dataset                          */
/**********************************************************************/
LISTCAT_DATASET: PROCEDURE
   DSNAME = ARG(1)
   
   ADDRESS TSO "ALLOC F(SYSIN) UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
               "RECFM(F,B) LRECL(80) BLKSIZE(80)"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"
   
   QUEUE " LISTCAT ENTRIES('"DSNAME"') ALL"
   ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"
   
   ADDRESS TSO "CALL *(IDCAMS)"
   IDCAMS_RC = RC
   
   ADDRESS TSO "FREE F(SYSIN,SYSPRINT)"
   
   RETURN IDCAMS_RC

/**********************************************************************/
/* GET_ENCRYPTION_INFO - Extract encryption info from LISTCAT        */
/**********************************************************************/
GET_ENCRYPTION_INFO: PROCEDURE EXPOSE ENCRYPTED KEY_YEAR VSAM_PARMS.
   DSNAME = ARG(1)
   
   /* Initialize */
   ENCRYPTED = 'NO'
   KEY_YEAR = ''
   VSAM_PARMS. = ''
   VSAM_PARMS.REC_TOTAL = ''  /* Add this to track record count */
   
   /* Run LISTCAT */
   ADDRESS TSO "ALLOC F(SYSIN) UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
               "RECFM(F,B) LRECL(80) BLKSIZE(80)"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(10,5) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"
   
   QUEUE " LISTCAT ENTRIES('"DSNAME"') ALL"
   ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"
   
   ADDRESS TSO "CALL *(IDCAMS)"
   
   ADDRESS TSO "EXECIO * DISKR SYSPRINT (STEM LC. FINIS"
   ADDRESS TSO "FREE F(SYSIN,SYSPRINT)"
   
   /* Parse output */
   IN_CLUSTER = 'NO'
   IN_DATA = 'NO'
   IN_INDEX = 'NO'
   
   DO I = 1 TO LC.0
      LINE = STRIP(LC.I)
     
      /* Detect sections */
      IF POS('CLUSTER -------',LINE) > 0 THEN DO
         IN_CLUSTER = 'YES'
         IN_DATA = 'NO'
         IN_INDEX = 'NO'
      END
     
      IF POS('DATA -------',LINE) > 0 & POS('CLUSTER',LINE) = 0 THEN DO
         IN_CLUSTER = 'NO'
         IN_DATA = 'YES'
         IN_INDEX = 'NO'
      END
     
      IF POS('INDEX ------',LINE) > 0 THEN DO
         IN_CLUSTER = 'NO'
         IN_DATA = 'NO'
         IN_INDEX = 'YES'
      END
     
      /* Skip INDEX section except for CISIZE */
      IF IN_INDEX = 'YES' THEN DO
         IF POS('CISIZE',LINE) > 0 & VSAM_PARMS.INDEX_CISIZE = '' THEN DO
            VSAM_PARMS.INDEX_CISIZE = EXTRACT_VALUE(LINE,'CISIZE')
         END
         ITERATE
      END
     
      /* CLUSTER section - process encryption data */
      IF IN_CLUSTER = 'YES' THEN DO
         IF POS('DATA SET ENCRYPTION',LINE) > 0 THEN DO
            TEMP_LINE = SUBSTR(LINE, POS('DATA SET ENCRYPTION',LINE))
            IF POS('YES',TEMP_LINE) > 0 THEN DO
               ENCRYPTED = 'YES'
            END
            ELSE IF POS('NO',TEMP_LINE) > 0 THEN DO
               ENCRYPTED = 'NO'
            END
         END
         
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
            KEY_LABEL = STRIP(KEY_LABEL)
            KEY_YEAR = EXTRACT_YEAR(KEY_LABEL)
         END
         
         IF POS('STORAGECLASS',LINE) > 0 & VSAM_PARMS.STORAGECLASS = '' THEN DO
            VSAM_PARMS.STORAGECLASS = EXTRACT_VALUE(LINE,'STORAGECLASS')
         END
         
         IF POS('LOG',LINE) > 0 & POS('LOGSTREAMID',LINE) = 0 THEN DO
            IF POS('UNDO',LINE) > 0 THEN VSAM_PARMS.LOG = 'UNDO'
            IF POS('ALL',LINE) > 0 THEN VSAM_PARMS.LOG = 'ALL'
         END
         
         IF POS('RLS IN USE',LINE) > 0 THEN DO
            IF POS('YES',LINE) > 0 THEN VSAM_PARMS.RLS = 'YES'
            ELSE VSAM_PARMS.RLS = 'NO'
         END
      END
     
      /* DATA section */
      IF IN_DATA = 'YES' THEN DO
         /* Capture REC-TOTAL from STATISTICS section */
         IF POS('REC-TOTAL',LINE) > 0 & VSAM_PARMS.REC_TOTAL = '' THEN DO
            VSAM_PARMS.REC_TOTAL = EXTRACT_VALUE(LINE,'REC-TOTAL')
         END
         
         IF POS('KEYLEN',LINE) > 0 & VSAM_PARMS.KEYLEN = '' THEN DO
            VSAM_PARMS.KEYLEN = EXTRACT_VALUE(LINE,'KEYLEN')
         END
         
         IF POS('RKP',LINE) > 0 & VSAM_PARMS.RKP = '' THEN DO
            VSAM_PARMS.RKP = EXTRACT_VALUE(LINE,'RKP')
         END
         
         IF POS('AVGLRECL',LINE) > 0 & VSAM_PARMS.AVGLRECL = '' THEN DO
            VSAM_PARMS.AVGLRECL = EXTRACT_VALUE(LINE,'AVGLRECL')
         END
         
         IF POS('MAXLRECL',LINE) > 0 & VSAM_PARMS.MAXLRECL = '' THEN DO
            VSAM_PARMS.MAXLRECL = EXTRACT_VALUE(LINE,'MAXLRECL')
         END
         
         IF POS('CISIZE',LINE) > 0 & VSAM_PARMS.CISIZE = '' THEN DO
            VSAM_PARMS.CISIZE = EXTRACT_VALUE(LINE,'CISIZE')
         END
         
         IF POS('BUFSPACE',LINE) > 0 & VSAM_PARMS.BUFSPACE = '' THEN DO
            VSAM_PARMS.BUFSPACE = EXTRACT_VALUE(LINE,'BUFSPACE')
         END
         
         /* Detect dataset type - check in priority order, only set once */  
         IF VSAM_PARMS.TYPE = '' THEN DO                                      
            /* Check for ZFS and LINEAR on same line - ZFS takes priority */  
            IF POS('ZFS',LINE) > 0 THEN DO                                    
               VSAM_PARMS.TYPE = 'ZFS'                                        
            END                                                              
            ELSE IF POS('NUMBERED',LINE) > 0 THEN DO                          
               VSAM_PARMS.TYPE = 'RRDS'                                      
            END                                                              
            ELSE IF POS('INDEXED',LINE) > 0 THEN DO                          
               VSAM_PARMS.TYPE = 'KSDS'                                      
            END                                                              
            ELSE IF POS('NONINDEXED',LINE) > 0 THEN DO                        
               VSAM_PARMS.TYPE = 'ESDS'                                      
            END                                                              
            ELSE IF POS('LINEAR',LINE) > 0 THEN DO                            
               VSAM_PARMS.TYPE = 'LDS'                                        
            END                                                              
         END                                                                  
                                                                             
         IF VSAM_PARMS.TYPE = 'LDS' THEN DO                                  
         /* Further check if its zfs when linear found */                    
            IF POS('ZFS',LINE) > 0 THEN DO                                    
               VSAM_PARMS.TYPE = 'ZFS'                                        
            END                                                              
         END                                                                  
                 
         IF POS('SHROPTNS',LINE) > 0 & VSAM_PARMS.SHROPTNS = '' THEN DO
            START_POS = POS('SHROPTNS(',LINE) + 9
            END_POS = POS(')',LINE,START_POS)
            IF END_POS > START_POS THEN DO
               VSAM_PARMS.SHROPTNS = SUBSTR(LINE,START_POS,END_POS-START_POS)
            END
         END
         
         IF POS('FREESPACE-%CI',LINE) > 0 & VSAM_PARMS.FREECI = '' THEN DO
            VSAM_PARMS.FREECI = EXTRACT_VALUE(LINE,'FREESPACE-%CI')
         END
         
         IF POS('FREESPACE-%CA',LINE) > 0 & VSAM_PARMS.FREECA = '' THEN DO
            VSAM_PARMS.FREECA = EXTRACT_VALUE(LINE,'FREESPACE-%CA')
         END
         
         IF POS('SPACE-TYPE',LINE) > 0 & VSAM_PARMS.SPACETYPE = '' THEN DO
            VSAM_PARMS.SPACETYPE = EXTRACT_VALUE(LINE,'SPACE-TYPE')
         END
         
         IF POS('SPACE-PRI',LINE) > 0 & VSAM_PARMS.SPACEPRI = '' THEN DO
            VSAM_PARMS.SPACEPRI = EXTRACT_VALUE(LINE,'SPACE-PRI')
         END
         
         IF POS('SPACE-SEC',LINE) > 0 & VSAM_PARMS.SPACESEC = '' THEN DO
            VSAM_PARMS.SPACESEC = EXTRACT_VALUE(LINE,'SPACE-SEC')
         END
         
         /* Parse ATTRIBUTES line for additional parameters */
         IF POS('SHROPTNS',LINE) > 0 THEN DO
            /* SPEED vs RECOVERY */
            IF POS('SPEED',LINE) > 0 THEN DO
               VSAM_PARMS.SPEED_RECOVERY = 'SPEED'
            END
            ELSE IF POS('RECOVERY',LINE) > 0 THEN DO
               VSAM_PARMS.SPEED_RECOVERY = 'RECOVERY'
            END
           
            /* REUSE vs NOREUSE */
            IF POS('NOREUSE',LINE) > 0 THEN DO
               VSAM_PARMS.REUSE = 'NOREUSE'
               
               /* UNIQUE only applies when NOREUSE is used */
               IF POS('UNIQUE',LINE) > 0 THEN DO
                  VSAM_PARMS.UNIQUE = 'UNIQUE'
               END
            END
            ELSE IF POS('REUSE',LINE) > 0 THEN DO
               VSAM_PARMS.REUSE = 'REUSE'
               VSAM_PARMS.UNIQUE = ''
            END
           
            /* ERASE vs NOERASE */
            IF POS('NOERASE',LINE) > 0 THEN DO
               VSAM_PARMS.ERASE = 'NOERASE'
            END
            ELSE IF POS('ERASE',LINE) > 0 & POS('NOERASE',LINE) = 0 THEN DO
               VSAM_PARMS.ERASE = 'ERASE'
            END
         END
      END
   END
   
   RETURN

/**********************************************************************/
/* EXTRACT_VALUE - Extract value after keyword (handles dashes)      */
/**********************************************************************/
EXTRACT_VALUE: PROCEDURE
   LINE = ARG(1)
   KEYWORD = ARG(2)
   
   START_POS = POS(KEYWORD,LINE)
   IF START_POS = 0 THEN RETURN ''
   
   START_POS = START_POS + LENGTH(KEYWORD)
   TEMP = SUBSTR(LINE,START_POS)
   
   VALUE = ''
   DO I = 1 TO LENGTH(TEMP)
      CHAR = SUBSTR(TEMP,I,1)
      IF CHAR <> '-' & CHAR <> ' ' THEN DO
         REMAINING = SUBSTR(TEMP,I)
         VALUE = WORD(REMAINING,1)
         LEAVE
      END
   END
   
   RETURN VALUE

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
      IF LENGTH(COMPONENT) = 4 & DATATYPE(COMPONENT) = 'NUM' THEN DO
         YEAR = COMPONENT
      END
   END
   
   IF LENGTH(TEMP) = 4 & DATATYPE(TEMP) = 'NUM' THEN DO
      YEAR = TEMP
   END
   
   RETURN YEAR

/**********************************************************************/
/* CONVERT_VSAM - Define new, copy, rename                           */
/**********************************************************************/
CONVERT_VSAM: PROCEDURE EXPOSE VSAM_PARMS. FAILED_LIST. FAILED_COUNT
   DSNAME = ARG(1)
   NEWNAME = DSNAME'.#NE'
   OLDNAME = DSNAME'.#OL'
   
   /* Define new dataset */
   SAY '  Step 1: Defining 'NEWNAME
   CALL DEFINE_VSAM NEWNAME
   IF RESULT <> 0 THEN DO
      FAILED_COUNT = FAILED_COUNT + 1
      FAILED_LIST.FAILED_COUNT = DSNAME' - DEFINE failed (RC='RESULT')'
      RETURN 8
   END
   
   /* Copy data - skip if dataset is empty */
   IF VSAM_PARMS.REC_TOTAL = '0' THEN DO
      SAY '  Step 2: Dataset is empty (REC-TOTAL=0), skipping copy step'
   END
   ELSE DO
      SAY '  Step 2: Copying data (REC-TOTAL='VSAM_PARMS.REC_TOTAL')...'
      CALL REPRO_VSAM DSNAME, NEWNAME
      IF RESULT <> 0 THEN DO
         SAY '  ERROR: Copy operation failed, cleaning up temporary dataset...'
         CALL DELETE_VSAM NEWNAME
         FAILED_COUNT = FAILED_COUNT + 1
         FAILED_LIST.FAILED_COUNT = DSNAME' - REPRO failed (RC='RESULT'), cleanup completed'
         RETURN 8
      END
   END
   
   /* Rename current to #OL */
   SAY '  Step 3: Renaming current to 'OLDNAME
   CALL RENAME_VSAM DSNAME, OLDNAME
   IF RESULT <> 0 THEN DO
      SAY '  ERROR: Rename to backup failed, cleaning up...'
      CALL DELETE_VSAM NEWNAME
      FAILED_COUNT = FAILED_COUNT + 1
      FAILED_LIST.FAILED_COUNT = DSNAME' - Backup rename failed (RC='RESULT'), cleanup completed'
      RETURN 8
   END
   
   /* Rename #NE to current */
   SAY '  Step 4: Renaming new to 'DSNAME
   CALL RENAME_VSAM NEWNAME, DSNAME
   IF RESULT <> 0 THEN DO
      SAY '  CRITICAL ERROR: Final rename failed!'
      SAY '  MANUAL INTERVENTION REQUIRED:'
      SAY '    - Original data is in: 'OLDNAME
      SAY '    - New encrypted data is in: 'NEWNAME
      SAY '    - Current dataset name: 'DSNAME' (may not exist)'
      FAILED_COUNT = FAILED_COUNT + 1
      FAILED_LIST.FAILED_COUNT = DSNAME' - Final rename failed (RC='RESULT') - MANUAL RECOVERY NEEDED'
      RETURN 8
   END
   
   SAY '  Conversion completed successfully'
   RETURN 0

/**********************************************************************/
/* DEFINE_VSAM - Define new VSAM cluster                             */
/**********************************************************************/
DEFINE_VSAM: PROCEDURE EXPOSE VSAM_PARMS.
   NEWNAME = ARG(1)
   
   ADDRESS TSO "ALLOC F(SYSIN) UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
               "RECFM(F,B) LRECL(80) BLKSIZE(800)"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(10,10) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"
   
   /* Build DEFINE command using parsed attributes */
   IF VSAM_PARMS.TYPE = 'KSDS' THEN DO
      QUEUE " DEFINE CLUSTER (NAME('"NEWNAME"') -"
      QUEUE "  INDEXED -"
      QUEUE "  KEYS("VSAM_PARMS.KEYLEN" "VSAM_PARMS.RKP") -"
      QUEUE "  RECORDSIZE("VSAM_PARMS.AVGLRECL" "VSAM_PARMS.MAXLRECL") -"
      QUEUE "  CONTROLINTERVALSIZE("VSAM_PARMS.CISIZE") -"
      QUEUE "  STORCLAS("VSAM_PARMS.STORAGECLASS") -"
     
      IF VSAM_PARMS.BUFSPACE <> '' & VSAM_PARMS.BUFSPACE <> '0' THEN DO
         QUEUE "  BUFFERSPACE("VSAM_PARMS.BUFSPACE") -"
      END
     
      IF VSAM_PARMS.SHROPTNS <> '' THEN DO
         QUEUE "  SHAREOPTIONS("VSAM_PARMS.SHROPTNS") -"
      END
      ELSE DO
         QUEUE "  SHAREOPTIONS(2,3) -"
      END
     
      IF VSAM_PARMS.FREECI <> '' & VSAM_PARMS.FREECA <> '' THEN DO
         QUEUE "  FREESPACE("VSAM_PARMS.FREECI" "VSAM_PARMS.FREECA") -"
      END
      ELSE DO
         QUEUE "  FREESPACE(0 0) -"
      END
     
      /* Space allocation */
      IF VSAM_PARMS.SPACETYPE = 'CYLINDER' THEN DO
         QUEUE "  CYLINDERS("VSAM_PARMS.SPACEPRI" "VSAM_PARMS.SPACESEC") -"
      END
      ELSE IF VSAM_PARMS.SPACETYPE = 'TRACK' THEN DO
         QUEUE "  TRACKS("VSAM_PARMS.SPACEPRI" "VSAM_PARMS.SPACESEC") -"
      END
      ELSE DO
         QUEUE "  CYLINDERS(3 1) -"
      END
     
      /* Use parsed attributes instead of hardcoded values */
      IF VSAM_PARMS.SPEED_RECOVERY <> '' THEN DO
         QUEUE "  "VSAM_PARMS.SPEED_RECOVERY" -"
      END
     
      IF VSAM_PARMS.REUSE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.REUSE" -"
      END
     
      IF VSAM_PARMS.UNIQUE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.UNIQUE" -"
      END
     
      IF VSAM_PARMS.ERASE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.ERASE") -"
      END
      ELSE DO
         QUEUE ") -"
      END
     
      QUEUE " DATA (NAME('"NEWNAME".DATA')) -"
     
      IF VSAM_PARMS.INDEX_CISIZE <> '' THEN DO
         QUEUE " INDEX (NAME('"NEWNAME".INDEX') -"
         QUEUE "   CONTROLINTERVALSIZE("VSAM_PARMS.INDEX_CISIZE"))"
      END
      ELSE DO
         QUEUE " INDEX (NAME('"NEWNAME".INDEX'))"
      END
   END
   ELSE IF VSAM_PARMS.TYPE = 'ZFS' THEN DO
      QUEUE " DEFINE CLUSTER (NAME('"NEWNAME"') -"
      QUEUE "  ZFS -"
      QUEUE "  CONTROLINTERVALSIZE("VSAM_PARMS.CISIZE") -"
      QUEUE "  STORCLAS("VSAM_PARMS.STORAGECLASS") -"
     
      IF VSAM_PARMS.BUFSPACE <> '' & VSAM_PARMS.BUFSPACE <> '0' THEN DO
         QUEUE "  BUFFERSPACE("VSAM_PARMS.BUFSPACE") -"
      END
     
      IF VSAM_PARMS.SHROPTNS <> '' THEN DO
         QUEUE "  SHAREOPTIONS("VSAM_PARMS.SHROPTNS") -"
      END
      ELSE DO
         QUEUE "  SHAREOPTIONS(3,3) -"  /* ZFS typically uses (3,3) */
      END
     
      /* Space allocation */
      IF VSAM_PARMS.SPACETYPE = 'CYLINDER' THEN DO
         QUEUE "  CYLINDERS("VSAM_PARMS.SPACEPRI" "VSAM_PARMS.SPACESEC") -"
      END
      ELSE IF VSAM_PARMS.SPACETYPE = 'TRACK' THEN DO
         QUEUE "  TRACKS("VSAM_PARMS.SPACEPRI" "VSAM_PARMS.SPACESEC") -"
      END
      ELSE DO
         QUEUE "  CYLINDERS(200 50) -"  /* ZFS datasets typically larger */
      END
     
      /* Use parsed attributes */
      IF VSAM_PARMS.SPEED_RECOVERY <> '' THEN DO
         QUEUE "  "VSAM_PARMS.SPEED_RECOVERY" -"
      END
      ELSE DO
         QUEUE "  RECOVERY -"  /* ZFS datasets typically use RECOVERY */
      END
     
      IF VSAM_PARMS.REUSE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.REUSE" -"
      END
      ELSE DO
         QUEUE "  NOREUSE -"
      END
     
      IF VSAM_PARMS.UNIQUE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.UNIQUE" -"
      END
      ELSE DO
         QUEUE "  UNIQUE -"
      END
     
      IF VSAM_PARMS.ERASE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.ERASE") -"
      END
      ELSE DO
         QUEUE "  NOERASE) -"
      END
     
      QUEUE " DATA (NAME('"NEWNAME".DATA'))"
   END
   ELSE IF VSAM_PARMS.TYPE = 'LDS' THEN DO
      QUEUE " DEFINE CLUSTER (NAME('"NEWNAME"') -"
      QUEUE "  LINEAR -"
      QUEUE "  CONTROLINTERVALSIZE("VSAM_PARMS.CISIZE") -"
      QUEUE "  STORCLAS("VSAM_PARMS.STORAGECLASS") -"
     
      IF VSAM_PARMS.BUFSPACE <> '' & VSAM_PARMS.BUFSPACE <> '0' THEN DO
         QUEUE "  BUFFERSPACE("VSAM_PARMS.BUFSPACE") -"
      END
     
      IF VSAM_PARMS.SHROPTNS <> '' THEN DO
         QUEUE "  SHAREOPTIONS("VSAM_PARMS.SHROPTNS") -"
      END
      ELSE DO
         QUEUE "  SHAREOPTIONS(3,3) -"
      END
     
      /* Space allocation */
      IF VSAM_PARMS.SPACETYPE = 'CYLINDER' THEN DO
         QUEUE "  CYLINDERS("VSAM_PARMS.SPACEPRI" "VSAM_PARMS.SPACESEC") -"
      END
      ELSE IF VSAM_PARMS.SPACETYPE = 'TRACK' THEN DO
         QUEUE "  TRACKS("VSAM_PARMS.SPACEPRI" "VSAM_PARMS.SPACESEC") -"
      END
      ELSE DO
         QUEUE "  CYLINDERS(200 50) -"
      END
     
      /* Use parsed attributes */
      IF VSAM_PARMS.SPEED_RECOVERY <> '' THEN DO
         QUEUE "  "VSAM_PARMS.SPEED_RECOVERY" -"
      END
      ELSE DO
         QUEUE "  RECOVERY -"  /* LINEAR datasets typically use RECOVERY */
      END
     
      IF VSAM_PARMS.REUSE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.REUSE" -"
      END
      ELSE DO
         QUEUE "  NOREUSE -"
      END
     
      IF VSAM_PARMS.UNIQUE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.UNIQUE" -"
      END
      ELSE DO
         QUEUE "  UNIQUE -"
      END
     
      IF VSAM_PARMS.ERASE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.ERASE") -"
      END
      ELSE DO
         QUEUE "  NOERASE) -"
      END
     
      QUEUE " DATA (NAME('"NEWNAME".DATA'))"
   END
   ELSE DO  /* ESDS */
      QUEUE " DEFINE CLUSTER (NAME('"NEWNAME"') -"
      QUEUE "  NONINDEXED -"
      QUEUE "  RECORDSIZE("VSAM_PARMS.AVGLRECL" "VSAM_PARMS.MAXLRECL") -"
      QUEUE "  CONTROLINTERVALSIZE("VSAM_PARMS.CISIZE") -"
      QUEUE "  STORCLAS("VSAM_PARMS.STORAGECLASS") -"
     
      IF VSAM_PARMS.BUFSPACE <> '' & VSAM_PARMS.BUFSPACE <> '0' THEN DO
         QUEUE "  BUFFERSPACE("VSAM_PARMS.BUFSPACE") -"
      END
     
      IF VSAM_PARMS.SHROPTNS <> '' THEN DO
         QUEUE "  SHAREOPTIONS("VSAM_PARMS.SHROPTNS") -"
      END
      ELSE DO
         QUEUE "  SHAREOPTIONS(2,3) -"
      END
     
      IF VSAM_PARMS.FREECI <> '' & VSAM_PARMS.FREECA <> '' THEN DO
         QUEUE "  FREESPACE("VSAM_PARMS.FREECI" "VSAM_PARMS.FREECA") -"
      END
      ELSE DO
         QUEUE "  FREESPACE(0 0) -"
      END
     
      IF VSAM_PARMS.SPACETYPE = 'CYLINDER' THEN DO
         QUEUE "  CYLINDERS("VSAM_PARMS.SPACEPRI" "VSAM_PARMS.SPACESEC") -"
      END
      ELSE IF VSAM_PARMS.SPACETYPE = 'TRACK' THEN DO
         QUEUE "  TRACKS("VSAM_PARMS.SPACEPRI" "VSAM_PARMS.SPACESEC") -"
      END
      ELSE DO
         QUEUE "  CYLINDERS(3 1) -"
      END
     
      IF VSAM_PARMS.SPEED_RECOVERY <> '' THEN DO
         QUEUE "  "VSAM_PARMS.SPEED_RECOVERY" -"
      END
      ELSE DO
         QUEUE "  SPEED -"
      END
     
      IF VSAM_PARMS.REUSE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.REUSE" -"
      END
      ELSE DO
         QUEUE "  NOREUSE -"
      END
     
      IF VSAM_PARMS.UNIQUE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.UNIQUE" -"
      END
      ELSE DO
         QUEUE "  UNIQUE -"
      END
     
      IF VSAM_PARMS.ERASE <> '' THEN DO
         QUEUE "  "VSAM_PARMS.ERASE") -"
      END
      ELSE DO
         QUEUE "  NOERASE) -"
      END
     
      QUEUE " DATA (NAME('"NEWNAME".DATA'))"
   END
   
   ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"
   
   ADDRESS TSO "CALL *(IDCAMS)"
   IDCAMS_RC = RC
   
   IF IDCAMS_RC <> 0 THEN DO
      SAY '  ERROR: DEFINE failed with RC='IDCAMS_RC
      ADDRESS TSO "EXECIO * DISKR SYSPRINT (STEM ERR. FINIS"
      DO I = 1 TO ERR.0
         IF STRIP(ERR.I) <> '' THEN SAY '    'ERR.I
      END
   END
   
   ADDRESS TSO "FREE F(SYSIN,SYSPRINT)"
   
   RETURN IDCAMS_RC

/**********************************************************************/
/* REPRO_VSAM - Copy data between datasets                           */
/**********************************************************************/
REPRO_VSAM: PROCEDURE
   SOURCE = ARG(1)
   TARGET = ARG(2)
   
   ADDRESS TSO "ALLOC F(SYSIN) UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
               "RECFM(F,B) LRECL(80) BLKSIZE(800)"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"
   ADDRESS TSO "ALLOC F(INPUT) DA('"SOURCE"') SHR REUSE"
   ADDRESS TSO "ALLOC F(OUTPUT) DA('"TARGET"') SHR REUSE"
   
   QUEUE " REPRO INFILE(INPUT) OUTFILE(OUTPUT)"
   ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"
   
   ADDRESS TSO "CALL *(IDCAMS)"
   IDCAMS_RC = RC
   
   IF IDCAMS_RC <> 0 THEN DO
      SAY '  ERROR: REPRO failed with RC='IDCAMS_RC
      ADDRESS TSO "EXECIO * DISKR SYSPRINT (STEM ERR. FINIS"
      DO I = 1 TO ERR.0
         IF STRIP(ERR.I) <> '' THEN SAY '    'ERR.I
      END
   END
   
   ADDRESS TSO "FREE F(SYSIN,SYSPRINT,INPUT,OUTPUT)"
   
   RETURN IDCAMS_RC

/**********************************************************************/
/* RENAME_VSAM - Rename VSAM cluster and components                  */
/**********************************************************************/
RENAME_VSAM: PROCEDURE EXPOSE VSAM_PARMS.
   OLDNAME = ARG(1)
   NEWNAME = ARG(2)
   
   ADDRESS TSO "ALLOC F(SYSIN) UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
               "RECFM(F,B) LRECL(80) BLKSIZE(800)"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"
   
   QUEUE " ALTER '"OLDNAME".DATA' -"
   QUEUE "   NEWNAME('"NEWNAME".DATA')"
   
   IF VSAM_PARMS.TYPE = 'KSDS' THEN DO
      QUEUE " ALTER '"OLDNAME".INDEX' -"
      QUEUE "   NEWNAME('"NEWNAME".INDEX')"
   END
   
   QUEUE " ALTER '"OLDNAME"' -"
   QUEUE "   NEWNAME('"NEWNAME"')"
   
   ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"
   
   ADDRESS TSO "CALL *(IDCAMS)"
   IDCAMS_RC = RC
   
   IF IDCAMS_RC <> 0 THEN DO
      SAY '  ERROR: RENAME from 'OLDNAME' to 'NEWNAME' failed with RC='IDCAMS_RC
      ADDRESS TSO "EXECIO * DISKR SYSPRINT (STEM ERR. FINIS"
      DO I = 1 TO ERR.0
         IF STRIP(ERR.I) <> '' THEN SAY '    'ERR.I
      END
   END
   
   ADDRESS TSO "FREE F(SYSIN,SYSPRINT)"
   
   RETURN IDCAMS_RC

/**********************************************************************/
/* DELETE_VSAM - Delete failed dataset                               */
/**********************************************************************/
DELETE_VSAM: PROCEDURE
   DSNAME = ARG(1)
   
   SAY '  Cleaning up: Deleting temporary dataset 'DSNAME
   
   ADDRESS TSO "ALLOC F(SYSIN) UNIT(VIO) NEW REUSE SPACE(1,1) TRACKS",
               "RECFM(F,B) LRECL(80) BLKSIZE(800)"
   ADDRESS TSO "ALLOC F(SYSPRINT) UNIT(VIO) NEW REUSE SPACE(5,5) TRACKS",
               "RECFM(F,B,A) LRECL(121) BLKSIZE(1210)"
   
   QUEUE " DELETE '"DSNAME"' CLUSTER PURGE"
   ADDRESS TSO "EXECIO "QUEUED()" DISKW SYSIN (FINIS"
   
   ADDRESS TSO "CALL *(IDCAMS)"
   DELETE_RC = RC
   
   IF DELETE_RC = 0 THEN DO
      SAY '  Cleanup successful: 'DSNAME' has been deleted'
   END
   ELSE DO
      SAY '  WARNING: Cleanup failed (RC='DELETE_RC'), manual deletion may be needed for: 'DSNAME
      ADDRESS TSO "EXECIO * DISKR SYSPRINT (STEM ERR. FINIS"
      DO I = 1 TO ERR.0
         IF STRIP(ERR.I) <> '' THEN SAY '    'ERR.I
      END
   END
   
   ADDRESS TSO "FREE F(SYSIN,SYSPRINT)"
   
   RETURN DELETE_RC

/**********************************************************************/
/* GENERATE_REPORT                                                    */
/**********************************************************************/
GENERATE_REPORT: PROCEDURE EXPOSE OUTPUT_RPT SUCCESS_COUNT ALREADY_CURRENT,
                 NOT_ENCRYPTED IN_USE_COUNT FAILED_COUNT TOTAL_COUNT,
                 SUCCESS_LIST. ALREADY_LIST. NOT_ENC_LIST.,
                 IN_USE_LIST. FAILED_LIST. TARGET_YEAR,
                 RUN_DATE RUN_TIME
   
   ADDRESS TSO "ALLOC F(REPORT) DA('"OUTPUT_RPT"') NEW CATALOG",
               "SPACE(5,5) TRACKS RECFM(F,B,A) LRECL(133) BLKSIZE(1330)"
   
   CALL WRITE_RPT COPIES('=',80)
   CALL WRITE_RPT 'VSAM PERVASIVE ENCRYPTION KEY ROTATION REPORT'
   CALL WRITE_RPT 'Target Key Year: 'TARGET_YEAR
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
   CALL WRITE_RPT 'Successful Conversions: 'SUCCESS_COUNT
   CALL WRITE_RPT 'Already Current: 'ALREADY_CURRENT
   CALL WRITE_RPT 'Not Encrypted: 'NOT_ENCRYPTED
   CALL WRITE_RPT 'In Use: 'IN_USE_COUNT
   CALL WRITE_RPT 'Failed: 'FAILED_COUNT
   CALL WRITE_RPT COPIES('=',80)
   
   ADDRESS TSO "EXECIO 0 DISKW REPORT (FINIS"
   ADDRESS TSO "FREE F(REPORT)"
   
   RETURN

/**********************************************************************/
/* WRITE_RPT - Write line to report                                  */
/**********************************************************************/
WRITE_RPT: PROCEDURE
   LINE = ARG(1)
   QUEUE LINE
   ADDRESS TSO "EXECIO 1 DISKW REPORT"
   RETURN
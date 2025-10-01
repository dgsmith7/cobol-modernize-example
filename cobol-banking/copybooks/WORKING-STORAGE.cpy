      *********************************************************
      * WORKING-STORAGE.cpy - Common Working Storage Variables
      * This copybook contains common variables used across
      * multiple programs in the bank ledger system
      *********************************************************
      
      *    Return code and status variables
       01  WS-RETURN-CODE              PIC 9(2) VALUE ZERO.
       01  WS-FILE-STATUS              PIC X(2) VALUE SPACES.
       01  WS-BACKUP-RECORD            PIC X(70).
       01  WS-ERROR-FLAG               PIC X(1) VALUE 'N'.
           88  ERROR-OCCURRED          VALUE 'Y'.
           88  NO-ERROR                VALUE 'N'.
      
      *    Date and time work variables
       01  WS-CURRENT-DATE.
           05  WS-YEAR                 PIC 9(4).
           05  WS-MONTH                PIC 9(2).
           05  WS-DAY                  PIC 9(2).
       01  WS-CURRENT-TIME.
           05  WS-HOUR                 PIC 9(2).
           05  WS-MINUTE               PIC 9(2).
           05  WS-SECOND               PIC 9(2).
       01  WS-DATE-NUMERIC             PIC 9(8).
       01  WS-TIME-NUMERIC             PIC 9(6).
      
      *    Transaction counter for unique ID generation
       01  WS-TRANSACTION-COUNTER      PIC 9(15) VALUE ZERO.
      
      *    Command line parameter processing
       01  WS-PARM-LENGTH              PIC S9(4) COMP.
       01  WS-PARM-DATA                PIC X(100).
       01  WS-COMMAND                  PIC X(20).
       01  WS-ACCOUNT-PARM             PIC 9(10).
       01  WS-AMOUNT-PARM              PIC 9(10)V99.
       01  WS-TO-ACCOUNT-PARM          PIC 9(10).
      
      *    Display and formatting variables
       01  WS-DISPLAY-AMOUNT           PIC Z,ZZZ,ZZ9.99.
       01  WS-DISPLAY-BALANCE          PIC -Z,ZZZ,ZZ9.99.
       01  WS-EDIT-DATE                PIC 99/99/9999.
       01  WS-EDIT-TIME                PIC X(8).
      
      *    Error message table
       01  WS-ERROR-MESSAGES.
           05  FILLER PIC X(50) VALUE 
              'ACCOUNT NOT FOUND                         '.
           05  FILLER PIC X(50) VALUE 
              'INSUFFICIENT FUNDS                        '.
           05  FILLER PIC X(50) VALUE 
              'INVALID ACCOUNT NUMBER                    '.
           05  FILLER PIC X(50) VALUE 
              'INVALID AMOUNT                           '.
           05  FILLER PIC X(50) VALUE 
              'FILE ACCESS ERROR                        '.
           05  FILLER PIC X(50) VALUE 
              'ACCOUNT ALREADY EXISTS                   '.
           05  FILLER PIC X(50) VALUE 
              'INVALID COMMAND PARAMETERS               '.
       01  WS-ERROR-TABLE REDEFINES WS-ERROR-MESSAGES.
           05  WS-ERROR-MSG            PIC X(50) OCCURS 7 TIMES.
      
      *    Constants
       01  WS-CONSTANTS.
           05  MAX-DEPOSIT             PIC 9(8)V99 VALUE 99999.99.
           05  MAX-WITHDRAWAL          PIC 9(8)V99 VALUE 50000.00.
           05  MIN-BALANCE             PIC S9(8)V99 VALUE -1000.00.
      
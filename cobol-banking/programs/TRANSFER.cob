       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSFER.
      *********************************************************
      * TRANSFER UTILITY - Bank Transfer Processing
      * This program handles money transfers between accounts
      * with proper validation and atomic transactions
      *********************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "data/ACCOUNTS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACC-NUMBER
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT TRANSACTION-FILE ASSIGN TO "data/TRANSACT.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT COUNTER-FILE ASSIGN TO "data/COUNTER.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD  ACCOUNT-FILE.
           COPY "copybooks/ACCOUNT-RECORD.cpy".
       
       FD  TRANSACTION-FILE.
           COPY "copybooks/TRANSACTION-RECORD.cpy".
       
       FD  COUNTER-FILE.
       01  COUNTER-RECORD              PIC 9(15).
       
       WORKING-STORAGE SECTION.
           COPY "copybooks/WORKING-STORAGE.cpy".
       
      *    Transfer-specific working storage
       01  WS-FROM-ACCOUNT-REC.
           05  WS-FROM-ACC-NUMBER      PIC 9(10).
           05  WS-FROM-CUSTOMER-NAME   PIC X(30).
           05  WS-FROM-BALANCE         PIC S9(10)V99 COMP-3.
           05  WS-FROM-STATUS          PIC X(1).
           05  WS-FROM-OPEN-DATE       PIC 9(8).
           05  FILLER                  PIC X(15).
       
       01  WS-TO-ACCOUNT-REC.
           05  WS-TO-ACC-NUMBER        PIC 9(10).
           05  WS-TO-CUSTOMER-NAME     PIC X(30).
           05  WS-TO-BALANCE           PIC S9(10)V99 COMP-3.
           05  WS-TO-STATUS            PIC X(1).
           05  WS-TO-OPEN-DATE         PIC 9(8).
           05  FILLER                  PIC X(15).
       
       01  WS-ORIGINAL-FROM-BALANCE    PIC S9(10)V99 COMP-3.
       01  WS-ORIGINAL-TO-BALANCE      PIC S9(10)V99 COMP-3.
       01  WS-BACKUP-BALANCE           PIC S9(10)V99 COMP-3.
       01  WS-TRANSFER-SUCCESS-FLAG    PIC X(1) VALUE 'N'.
           88  TRANSFER-SUCCESSFUL     VALUE 'Y'.
           88  TRANSFER-FAILED         VALUE 'N'.
       
       LINKAGE SECTION.
       01  LS-FROM-ACCOUNT             PIC 9(10).
       01  LS-TO-ACCOUNT               PIC 9(10).
       01  LS-TRANSFER-AMOUNT          PIC 9(10)V99.
       01  LS-RETURN-CODE              PIC 9(2).
       
       PROCEDURE DIVISION USING LS-FROM-ACCOUNT
                               LS-TO-ACCOUNT
                               LS-TRANSFER-AMOUNT
                               LS-RETURN-CODE.
       
       MAIN-TRANSFER-PROCESSING.
      *    Initialize
           PERFORM INITIALIZATION
           
      *    Validate transfer parameters
           PERFORM VALIDATE-TRANSFER-PARAMS
           
      *    Execute transfer if validation passed
           IF NO-ERROR
               PERFORM EXECUTE-TRANSFER
           END-IF
           
      *    Set return code
           IF ERROR-OCCURRED
               MOVE 8 TO LS-RETURN-CODE
           ELSE
               MOVE 0 TO LS-RETURN-CODE
           END-IF
           
           GOBACK.
       
       INITIALIZATION.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           COMPUTE WS-DATE-NUMERIC = 
               WS-YEAR * 10000 + WS-MONTH * 100 + WS-DAY
           COMPUTE WS-TIME-NUMERIC = 
               WS-HOUR * 10000 + WS-MINUTE * 100 + WS-SECOND
           SET NO-ERROR TO TRUE.
       
       VALIDATE-TRANSFER-PARAMS.
      *    Check for valid account numbers
           IF LS-FROM-ACCOUNT = ZERO OR LS-TO-ACCOUNT = ZERO
               DISPLAY "Error: Valid source and destination "
                       "accounts required"
               SET ERROR-OCCURRED TO TRUE
               EXIT PARAGRAPH
           END-IF
           
      *    Check that accounts are different
           IF LS-FROM-ACCOUNT = LS-TO-ACCOUNT
               DISPLAY "Error: Cannot transfer to same account"
               SET ERROR-OCCURRED TO TRUE
               EXIT PARAGRAPH
           END-IF
           
      *    Check for valid amount
           IF LS-TRANSFER-AMOUNT <= ZERO
               DISPLAY "Error: Transfer amount must be positive"
               SET ERROR-OCCURRED TO TRUE
               EXIT PARAGRAPH
           END-IF
           
      *    Check transfer limits
           IF LS-TRANSFER-AMOUNT > MAX-WITHDRAWAL
               DISPLAY "Error: Transfer amount exceeds "
                       "maximum limit"
               SET ERROR-OCCURRED TO TRUE
               EXIT PARAGRAPH
           END-IF.
       
       EXECUTE-TRANSFER.
      *    Open account file for update
           OPEN I-O ACCOUNT-FILE
           
      *    Read and validate FROM account
           MOVE LS-FROM-ACCOUNT TO ACC-NUMBER
           READ ACCOUNT-FILE KEY IS ACC-NUMBER
               INVALID KEY
                   DISPLAY "Error: Source account " LS-FROM-ACCOUNT 
                           " not found"
                   SET ERROR-OCCURRED TO TRUE
                   CLOSE ACCOUNT-FILE
                   EXIT PARAGRAPH
           END-READ
           
      *    Save FROM account data
           MOVE ACCOUNT-RECORD TO WS-FROM-ACCOUNT-REC
           MOVE WS-FROM-BALANCE TO WS-ORIGINAL-FROM-BALANCE
           
      *    Validate FROM account status and balance
           IF WS-FROM-STATUS NOT = "A"
               DISPLAY "Error: Source account is not active"
               SET ERROR-OCCURRED TO TRUE
               CLOSE ACCOUNT-FILE
               EXIT PARAGRAPH
           END-IF
           
           COMPUTE WS-BACKUP-BALANCE = 
               WS-FROM-BALANCE - LS-TRANSFER-AMOUNT
           IF WS-BACKUP-BALANCE < MIN-BALANCE
               DISPLAY "Error: Insufficient funds in "
                       "source account"
               SET ERROR-OCCURRED TO TRUE
               CLOSE ACCOUNT-FILE
               EXIT PARAGRAPH
           END-IF
           
      *    Read and validate TO account
           MOVE LS-TO-ACCOUNT TO ACC-NUMBER
           READ ACCOUNT-FILE KEY IS ACC-NUMBER
               INVALID KEY
                   DISPLAY "Error: Destination account " LS-TO-ACCOUNT 
                           " not found"
                   SET ERROR-OCCURRED TO TRUE
                   CLOSE ACCOUNT-FILE
                   EXIT PARAGRAPH
           END-READ
           
      *    Save TO account data
           MOVE ACCOUNT-RECORD TO WS-TO-ACCOUNT-REC
           MOVE WS-TO-BALANCE TO WS-ORIGINAL-TO-BALANCE
           
      *    Validate TO account status
           IF WS-TO-STATUS NOT = "A"
               DISPLAY "Error: Destination account is "
                       "not active"
               SET ERROR-OCCURRED TO TRUE
               CLOSE ACCOUNT-FILE
               EXIT PARAGRAPH
           END-IF
           
      *    Perform the atomic transfer
           PERFORM ATOMIC-TRANSFER
           
           CLOSE ACCOUNT-FILE.
       
       ATOMIC-TRANSFER.
      *    Debit the FROM account
           MOVE WS-FROM-ACCOUNT-REC TO ACCOUNT-RECORD
           SUBTRACT LS-TRANSFER-AMOUNT FROM ACC-BALANCE
           REWRITE ACCOUNT-RECORD
               INVALID KEY
                   DISPLAY "Error: Failed to update source account"
                   SET ERROR-OCCURRED TO TRUE
                   EXIT PARAGRAPH
           END-REWRITE
           
      *    Credit the TO account
           MOVE WS-TO-ACCOUNT-REC TO ACCOUNT-RECORD
           ADD LS-TRANSFER-AMOUNT TO ACC-BALANCE
           REWRITE ACCOUNT-RECORD
               INVALID KEY
      *            Rollback the FROM account update
                   DISPLAY "Error: Failed to update "
                           "destination account"
                   DISPLAY "Rolling back transaction..."
                   MOVE WS-FROM-ACCOUNT-REC TO ACCOUNT-RECORD
                   MOVE WS-ORIGINAL-FROM-BALANCE TO ACC-BALANCE
                   REWRITE ACCOUNT-RECORD
                   SET ERROR-OCCURRED TO TRUE
                   EXIT PARAGRAPH
           END-REWRITE
           
      *    Both updates successful - record the transactions
           SET TRANSFER-SUCCESSFUL TO TRUE
           PERFORM RECORD-TRANSFER-TRANSACTIONS
           
      *    Display success message
           MOVE LS-TRANSFER-AMOUNT TO WS-DISPLAY-AMOUNT
           DISPLAY "Transfer of $" WS-DISPLAY-AMOUNT 
                   " completed successfully"
           DISPLAY "From account: " LS-FROM-ACCOUNT
           DISPLAY "To account: " LS-TO-ACCOUNT.
       
       RECORD-TRANSFER-TRANSACTIONS.
      *    Record debit transaction for FROM account
           PERFORM GET-NEXT-TRANSACTION-ID
           OPEN EXTEND TRANSACTION-FILE
           
           MOVE WS-TRANSACTION-COUNTER TO TXN-ID
           MOVE WS-DATE-NUMERIC TO TXN-DATE
           MOVE WS-TIME-NUMERIC TO TXN-TIME
           MOVE LS-FROM-ACCOUNT TO TXN-FROM-ACCOUNT
           MOVE LS-TO-ACCOUNT TO TXN-TO-ACCOUNT
           MOVE "T" TO TXN-TYPE
           MOVE LS-TRANSFER-AMOUNT TO TXN-AMOUNT
           MOVE "TRANSFER OUT" TO TXN-DESCRIPTION
           MOVE "P" TO TXN-STATUS
           
           WRITE TRANSACTION-RECORD
           
      *    Record credit transaction for TO account
           PERFORM GET-NEXT-TRANSACTION-ID
           
           MOVE WS-TRANSACTION-COUNTER TO TXN-ID
           MOVE WS-DATE-NUMERIC TO TXN-DATE
           MOVE WS-TIME-NUMERIC TO TXN-TIME
           MOVE LS-TO-ACCOUNT TO TXN-FROM-ACCOUNT
           MOVE LS-FROM-ACCOUNT TO TXN-TO-ACCOUNT
           MOVE "T" TO TXN-TYPE
           MOVE LS-TRANSFER-AMOUNT TO TXN-AMOUNT
           MOVE "TRANSFER IN" TO TXN-DESCRIPTION
           MOVE "P" TO TXN-STATUS
           
           WRITE TRANSACTION-RECORD
           
           CLOSE TRANSACTION-FILE.
       
       GET-NEXT-TRANSACTION-ID.
      *    Get next transaction ID from counter file
           OPEN INPUT COUNTER-FILE
           READ COUNTER-FILE
               AT END
                   MOVE 1 TO WS-TRANSACTION-COUNTER
               NOT AT END
                   MOVE COUNTER-RECORD TO WS-TRANSACTION-COUNTER
                   ADD 1 TO WS-TRANSACTION-COUNTER
           END-READ
           CLOSE COUNTER-FILE
           
      *    Update counter file
           OPEN OUTPUT COUNTER-FILE
           MOVE WS-TRANSACTION-COUNTER TO COUNTER-RECORD
           WRITE COUNTER-RECORD
           CLOSE COUNTER-FILE.
       
       END PROGRAM TRANSFER.
      
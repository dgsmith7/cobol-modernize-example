       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKLEDG.
      *********************************************************
      * BANK LEDGER SYSTEM - Main Program
      * This program provides command-line interface for banking
      * operations including deposits, withdrawals, transfers,
      * balance inquiries, and account management.
      *
      * Usage: BANKLEDG COMMAND ACCOUNT-NUM [ADDITIONAL-ARGS]
      * Commands:
      *   CREATE account-num "Customer Name" initial-balance
      *   DEPOSIT account-num amount
      *   WITHDRAW account-num amount  
      *   TRANSFER from-account to-account amount
      *   BALANCE account-num
      *   HISTORY account-num
      *********************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Account master file - indexed for proper record management
           SELECT ACCOUNT-FILE ASSIGN TO "data/ACCOUNTS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACC-NUMBER
               FILE STATUS IS WS-FILE-STATUS.
      
      *    Transaction history file - sequential append
           SELECT TRANSACTION-FILE ASSIGN TO "data/TRANSACT.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
      
      *    Transaction counter file for unique ID generation
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
       
      *    Additional working storage for main program
       01  WS-MAIN-RETURN-CODE         PIC 9(2) VALUE ZERO.
       01  WS-MAIN-FLAGS.
           05  WS-ACCOUNT-FOUND-FLAG   PIC X(1) VALUE 'N'.
               88  ACCOUNT-FOUND       VALUE 'Y'.
               88  ACCOUNT-NOT-FOUND   VALUE 'N'.
           05  WS-VALID-COMMAND-FLAG   PIC X(1) VALUE 'N'.
               88  VALID-COMMAND       VALUE 'Y'.
               88  INVALID-COMMAND     VALUE 'N'.
       
       01  WS-BACKUP-BALANCE           PIC S9(10)V99 COMP-3.
       01  WS-FROM-BALANCE             PIC S9(10)V99 COMP-3.
       01  WS-TO-BALANCE               PIC S9(10)V99 COMP-3.
       01  WS-CURRENT-BALANCE          PIC S9(10)V99 COMP-3.
       01  WS-COMMAND-LINE             PIC X(100).
       01 WS-TRANSACTION-COUNT    PIC 9(5) VALUE 0.
       01 WS-EOF-FLAG             PIC X VALUE 'N'.
          88 WS-EOF               VALUE 'Y'.
          88 WS-NOT-EOF           VALUE 'N'.

       PROCEDURE DIVISION.
       
       MAIN-PROCESSING.
      *    Initialize the program
           PERFORM INITIALIZATION
           
      *    Parse command line parameters
           PERFORM PARSE-PARAMETERS
           
      *    Execute the requested command
           IF VALID-COMMAND
               EVALUATE WS-COMMAND
                   WHEN "CREATE"
                       PERFORM CREATE-ACCOUNT
                   WHEN "DEPOSIT"
                       PERFORM DEPOSIT-PROCESSING
                   WHEN "WITHDRAW"
                       PERFORM WITHDRAW-PROCESSING
                   WHEN "TRANSFER"
                       PERFORM TRANSFER-PROCESSING
                   WHEN "BALANCE"
                       PERFORM BALANCE-INQUIRY
                   WHEN "HISTORY"
                       PERFORM TRANSACTION-HISTORY
                   WHEN "LIST"
                       PERFORM LIST-ALL-ACCOUNTS
                   WHEN OTHER
                       DISPLAY "Invalid command: " WS-COMMAND
                       MOVE 8 TO WS-MAIN-RETURN-CODE
               END-EVALUATE
           ELSE
               PERFORM DISPLAY-USAGE
               MOVE 8 TO WS-MAIN-RETURN-CODE
           END-IF
           
      *    Clean up and exit
           PERFORM TERMINATION
           STOP RUN RETURNING WS-MAIN-RETURN-CODE.
       
       INITIALIZATION.
      *    Get current date and time for transactions
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           
      *    Format date and time as numeric values
           COMPUTE WS-DATE-NUMERIC = 
               WS-YEAR * 10000 + WS-MONTH * 100 + WS-DAY
           COMPUTE WS-TIME-NUMERIC = 
               WS-HOUR * 10000 + WS-MINUTE * 100 + WS-SECOND
           
      *    Initialize error flag
           SET NO-ERROR TO TRUE.
       
       PARSE-PARAMETERS.
      *    Accept command line arguments
           ACCEPT WS-COMMAND-LINE FROM COMMAND-LINE
           MOVE WS-COMMAND-LINE TO WS-PARM-DATA
      *    Initialize parameters
           MOVE SPACES TO WS-COMMAND
           MOVE ZERO TO WS-ACCOUNT-PARM
           MOVE SPACES TO WS-CUSTOMER-NAME
           MOVE ZERO TO WS-AMOUNT-PARM
           MOVE ZERO TO WS-TO-ACCOUNT-PARM
           
      *    Simple parsing - extract first word as command
           UNSTRING WS-PARM-DATA DELIMITED BY SPACE
               INTO WS-COMMAND
           
      *    Handle CREATE command specially
           IF WS-COMMAND = "CREATE"
               PERFORM PARSE-CREATE-PARAMETERS
           ELSE
      *        Handle other commands with different parameter orders
               EVALUATE WS-COMMAND
                   WHEN "TRANSFER"
      *                TRANSFER fromAccount toAccount amount
                       UNSTRING WS-PARM-DATA DELIMITED BY SPACE
                           INTO WS-COMMAND
                                WS-ACCOUNT-PARM
                                WS-TO-ACCOUNT-PARM
                                WS-TEMP-AMOUNT
                       PERFORM PARSE-AMOUNT-VALUE
                   WHEN OTHER
      *                Other commands: COMMAND account amount
                       UNSTRING WS-PARM-DATA DELIMITED BY SPACE
                           INTO WS-COMMAND
                                WS-ACCOUNT-PARM
                                WS-TEMP-AMOUNT
                       PERFORM PARSE-AMOUNT-VALUE
               END-EVALUATE
           END-IF
           
      *    Convert command to uppercase
           INSPECT WS-COMMAND REPLACING ALL "a" BY "A"
           INSPECT WS-COMMAND REPLACING ALL "b" BY "B"
           INSPECT WS-COMMAND REPLACING ALL "c" BY "C"
           INSPECT WS-COMMAND REPLACING ALL "d" BY "D"
           INSPECT WS-COMMAND REPLACING ALL "e" BY "E"
           INSPECT WS-COMMAND REPLACING ALL "f" BY "F"
           INSPECT WS-COMMAND REPLACING ALL "g" BY "G"
           INSPECT WS-COMMAND REPLACING ALL "h" BY "H"
           INSPECT WS-COMMAND REPLACING ALL "i" BY "I"
           INSPECT WS-COMMAND REPLACING ALL "j" BY "J"
           INSPECT WS-COMMAND REPLACING ALL "k" BY "K"
           INSPECT WS-COMMAND REPLACING ALL "l" BY "L"
           INSPECT WS-COMMAND REPLACING ALL "m" BY "M"
           INSPECT WS-COMMAND REPLACING ALL "n" BY "N"
           INSPECT WS-COMMAND REPLACING ALL "o" BY "O"
           INSPECT WS-COMMAND REPLACING ALL "p" BY "P"
           INSPECT WS-COMMAND REPLACING ALL "q" BY "Q"
           INSPECT WS-COMMAND REPLACING ALL "r" BY "R"
           INSPECT WS-COMMAND REPLACING ALL "s" BY "S"
           INSPECT WS-COMMAND REPLACING ALL "t" BY "T"
           INSPECT WS-COMMAND REPLACING ALL "u" BY "U"
           INSPECT WS-COMMAND REPLACING ALL "v" BY "V"
           INSPECT WS-COMMAND REPLACING ALL "w" BY "W"
           INSPECT WS-COMMAND REPLACING ALL "x" BY "X"
           INSPECT WS-COMMAND REPLACING ALL "y" BY "Y"
           INSPECT WS-COMMAND REPLACING ALL "z" BY "Z"
           
      *    Validate command
           IF WS-COMMAND = "CREATE" OR "DEPOSIT" OR "WITHDRAW" OR
              "TRANSFER" OR "BALANCE" OR "HISTORY" OR "LIST"
               SET VALID-COMMAND TO TRUE
           ELSE
               SET INVALID-COMMAND TO TRUE
           END-IF.
       
       PARSE-CREATE-PARAMETERS.
      *    Parse CREATE command: CREATE account-num customer-name amount
      *    Simple 4-token format without quotes
           
           MOVE SPACES TO WS-CUSTOMER-NAME
           MOVE SPACES TO WS-TEMP-AMOUNT
           
      *    Parse 4 tokens: CREATE ACCOUNT NAME AMOUNT
           UNSTRING WS-PARM-DATA DELIMITED BY SPACE
               INTO WS-COMMAND
                    WS-TEMP-ACCOUNT
                    WS-CUSTOMER-NAME
                    WS-TEMP-AMOUNT
           END-UNSTRING
           
      *    Convert account number
           IF WS-TEMP-ACCOUNT IS NUMERIC
               MOVE WS-TEMP-ACCOUNT TO WS-ACCOUNT-PARM
           ELSE
               MOVE ZERO TO WS-ACCOUNT-PARM
           END-IF
           
      *    Parse amount
           IF WS-TEMP-AMOUNT NOT = SPACES
               COMPUTE WS-AMOUNT-PARM = FUNCTION NUMVAL(WS-TEMP-AMOUNT)
           ELSE
               MOVE 0.00 TO WS-AMOUNT-PARM
           END-IF.
       
       CREATE-ACCOUNT.
      *    Validate parameters for account creation
           IF WS-ACCOUNT-PARM = ZERO OR WS-CUSTOMER-NAME = SPACES
               DISPLAY "Error: Account number and customer name "
                       "required"
               MOVE 8 TO WS-MAIN-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
      *    Check if account already exists using indexed access
           OPEN I-O ACCOUNT-FILE
           IF WS-FILE-STATUS = "35"
      *        File doesn't exist, create it
               CLOSE ACCOUNT-FILE
               OPEN OUTPUT ACCOUNT-FILE
               CLOSE ACCOUNT-FILE
               OPEN I-O ACCOUNT-FILE
           END-IF
           
           MOVE WS-ACCOUNT-PARM TO ACC-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
      *            Account doesn't exist - we can create it
                   MOVE WS-CUSTOMER-NAME TO ACC-CUSTOMER-NAME
                   MOVE WS-AMOUNT-PARM TO ACC-BALANCE
                   MOVE "A" TO ACC-STATUS
                   MOVE WS-DATE-NUMERIC TO ACC-OPEN-DATE
                   
                   WRITE ACCOUNT-RECORD
                   IF WS-FILE-STATUS NOT = "00"
                       DISPLAY "Error writing account record"
                       DISPLAY "File status: " WS-FILE-STATUS
                       MOVE 8 TO WS-MAIN-RETURN-CODE
                   ELSE
                       MOVE ACC-BALANCE TO WS-DISPLAY-BALANCE
                       DISPLAY "Account " ACC-NUMBER " created for "
                               ACC-CUSTOMER-NAME
                       DISPLAY "Initial balance: $" WS-DISPLAY-BALANCE
                       MOVE 0 TO WS-MAIN-RETURN-CODE
                   END-IF
               NOT INVALID KEY
                   DISPLAY "Error: Account " WS-ACCOUNT-PARM 
                           " already exists"
                   MOVE 8 TO WS-MAIN-RETURN-CODE
           END-READ
           CLOSE ACCOUNT-FILE.
       
       DEPOSIT-PROCESSING.
      *    Validate deposit parameters
           IF WS-ACCOUNT-PARM = ZERO OR WS-AMOUNT-PARM <= ZERO
               DISPLAY "Error: Valid account number and amount required"
               MOVE 8 TO WS-MAIN-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
      *    Check deposit limit
           IF WS-AMOUNT-PARM > MAX-DEPOSIT
               DISPLAY "Error: Deposit amount exceeds maximum limit"
               MOVE 8 TO WS-MAIN-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
      *    Read, update and rewrite account using indexed file
           OPEN I-O ACCOUNT-FILE
           MOVE WS-ACCOUNT-PARM TO ACC-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY "Error: Account " WS-ACCOUNT-PARM 
                           " not found"
                   MOVE 8 TO WS-MAIN-RETURN-CODE
               NOT INVALID KEY
                   IF ACC-STATUS NOT = "A"
                       DISPLAY "Error: Account is not active"
                       MOVE 8 TO WS-MAIN-RETURN-CODE
                   ELSE
                       ADD WS-AMOUNT-PARM TO ACC-BALANCE
                       REWRITE ACCOUNT-RECORD
                       IF WS-FILE-STATUS = "00"
                           MOVE WS-AMOUNT-PARM TO WS-DISPLAY-AMOUNT
                           DISPLAY "Deposit processed"
                           DISPLAY WS-ACCOUNT-PARM
                           DISPLAY WS-DISPLAY-AMOUNT
                           MOVE ACC-BALANCE TO WS-DISPLAY-BALANCE
                           DISPLAY WS-DISPLAY-BALANCE
                           PERFORM RECORD-TRANSACTION
                           MOVE 0 TO WS-MAIN-RETURN-CODE
                       ELSE
                           DISPLAY "Error updating account"
                           MOVE 8 TO WS-MAIN-RETURN-CODE
                       END-IF
                   END-IF
           END-READ
           CLOSE ACCOUNT-FILE.
       
       WITHDRAW-PROCESSING.
      *    Validate withdrawal parameters
           IF WS-ACCOUNT-PARM = ZERO OR WS-AMOUNT-PARM <= ZERO
               DISPLAY "Error: Valid account number and amount required"
               MOVE 8 TO WS-MAIN-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
      *    Check withdrawal limit
           IF WS-AMOUNT-PARM > MAX-WITHDRAWAL
               DISPLAY "Error: Withdrawal amount exceeds maximum limit"
               MOVE 8 TO WS-MAIN-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *    Read, validate, update and rewrite account using indexed file
           OPEN I-O ACCOUNT-FILE
           MOVE WS-ACCOUNT-PARM TO ACC-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY "Error: Account " WS-ACCOUNT-PARM 
                           " not found"
                   MOVE 8 TO WS-MAIN-RETURN-CODE
               NOT INVALID KEY
                   IF ACC-STATUS NOT = "A"
                       DISPLAY "Error: Account is not active"
                       MOVE 8 TO WS-MAIN-RETURN-CODE
                   ELSE
                       IF ACC-BALANCE < WS-AMOUNT-PARM
                           DISPLAY "Error: Insufficient funds"
                           MOVE 8 TO WS-MAIN-RETURN-CODE
                       ELSE
                           SUBTRACT WS-AMOUNT-PARM FROM ACC-BALANCE
                           REWRITE ACCOUNT-RECORD
                           IF WS-FILE-STATUS = "00"
                               MOVE WS-AMOUNT-PARM TO WS-DISPLAY-AMOUNT
                               DISPLAY "Withdrawal processed"
                               DISPLAY WS-ACCOUNT-PARM
                               DISPLAY WS-DISPLAY-AMOUNT
                               MOVE ACC-BALANCE TO WS-DISPLAY-BALANCE
                               DISPLAY WS-DISPLAY-BALANCE
                               PERFORM RECORD-TRANSACTION
                               MOVE 0 TO WS-MAIN-RETURN-CODE
                           ELSE
                               DISPLAY "Error updating account"
                               MOVE 8 TO WS-MAIN-RETURN-CODE
                           END-IF
                       END-IF
                   END-IF
           END-READ
           CLOSE ACCOUNT-FILE.

       TRANSFER-PROCESSING.
      *    Validate transfer parameters
           IF WS-ACCOUNT-PARM = ZERO OR WS-TO-ACCOUNT-PARM = ZERO OR
              WS-AMOUNT-PARM <= ZERO
               DISPLAY "Error: Valid from-account, to-account, and "
                       "amount required"
               MOVE 8 TO WS-MAIN-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
      *    Implement transfer using indexed file operations
           OPEN I-O ACCOUNT-FILE
           
      *    Read and validate FROM account
           MOVE WS-ACCOUNT-PARM TO ACC-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY "Error: From account " WS-ACCOUNT-PARM 
                           " not found"
                   MOVE 8 TO WS-MAIN-RETURN-CODE
                   CLOSE ACCOUNT-FILE
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   IF ACC-STATUS NOT = "A"
                       DISPLAY "Error: From account is not active"
                       MOVE 8 TO WS-MAIN-RETURN-CODE
                       CLOSE ACCOUNT-FILE
                       EXIT PARAGRAPH
                   END-IF
                   IF ACC-BALANCE < WS-AMOUNT-PARM
                       DISPLAY "Error: Insufficient funds"
                       MOVE 8 TO WS-MAIN-RETURN-CODE
                       CLOSE ACCOUNT-FILE
                       EXIT PARAGRAPH
                   END-IF
                   MOVE ACC-BALANCE TO WS-FROM-BALANCE
           END-READ
           
      *    Read and validate TO account
           MOVE WS-TO-ACCOUNT-PARM TO ACC-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY "Error: To account " WS-TO-ACCOUNT-PARM 
                           " not found"
                   MOVE 8 TO WS-MAIN-RETURN-CODE
                   CLOSE ACCOUNT-FILE
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   IF ACC-STATUS NOT = "A"
                       DISPLAY "Error: To account is not active"
                       MOVE 8 TO WS-MAIN-RETURN-CODE
                       CLOSE ACCOUNT-FILE
                       EXIT PARAGRAPH
                   END-IF
                   MOVE ACC-BALANCE TO WS-TO-BALANCE
           END-READ
           
      *    Record the transfer transaction
           PERFORM RECORD-TRANSACTION

      *    Update FROM account (subtract amount)
           MOVE WS-ACCOUNT-PARM TO ACC-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY "Error reading from account for update"
                   MOVE 8 TO WS-MAIN-RETURN-CODE
                   CLOSE ACCOUNT-FILE
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   SUBTRACT WS-AMOUNT-PARM FROM ACC-BALANCE
                   REWRITE ACCOUNT-RECORD
                   MOVE ACC-BALANCE TO WS-FROM-BALANCE
           END-READ
           
      *    Update TO account (add amount)
           MOVE WS-TO-ACCOUNT-PARM TO ACC-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY "Error reading to account for update"
                   MOVE 8 TO WS-MAIN-RETURN-CODE
                   CLOSE ACCOUNT-FILE
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   ADD WS-AMOUNT-PARM TO ACC-BALANCE
                   REWRITE ACCOUNT-RECORD
                   MOVE ACC-BALANCE TO WS-TO-BALANCE
           END-READ
           
           CLOSE ACCOUNT-FILE
           
      *    Display results
           DISPLAY "TRANSFER COMPLETED"
           DISPLAY "From Account: " WS-ACCOUNT-PARM
           DISPLAY "To Account: " WS-TO-ACCOUNT-PARM  
           DISPLAY "Amount: " WS-AMOUNT-PARM
           DISPLAY "From Balance: " WS-FROM-BALANCE
           DISPLAY "To Balance: " WS-TO-BALANCE
           
           MOVE 0 TO WS-MAIN-RETURN-CODE.
       
       BALANCE-INQUIRY.
      *    Validate account parameter
           IF WS-ACCOUNT-PARM = ZERO
               DISPLAY "Error: Account number required"
               MOVE 8 TO WS-MAIN-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
      *    Open indexed account file and read by key
           OPEN INPUT ACCOUNT-FILE
           MOVE WS-ACCOUNT-PARM TO ACC-NUMBER
           READ ACCOUNT-FILE
               INVALID KEY
                   DISPLAY "Error: Account " WS-ACCOUNT-PARM 
                           " not found"
                   MOVE 8 TO WS-MAIN-RETURN-CODE
               NOT INVALID KEY
                   MOVE ACC-BALANCE TO WS-DISPLAY-BALANCE
                   DISPLAY "Account: " ACC-NUMBER
                   DISPLAY "Customer: " ACC-CUSTOMER-NAME
                   DISPLAY "Balance: $" WS-DISPLAY-BALANCE
                   DISPLAY "Status: " ACC-STATUS
                   MOVE 0 TO WS-MAIN-RETURN-CODE
           END-READ
           CLOSE ACCOUNT-FILE.
       
       TRANSACTION-HISTORY.
      *    Validate account parameter
           IF WS-ACCOUNT-PARM = ZERO
               DISPLAY "Error: Account number required"
               MOVE 8 TO WS-MAIN-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
      *    Display transaction history inline
           PERFORM DISPLAY-TRANSACTION-HISTORY-INLINE.
       
       RECORD-TRANSACTION.
      *    Generate unique transaction ID
           PERFORM GET-NEXT-TRANSACTION-ID
           
      *    Open transaction file for append
           OPEN EXTEND TRANSACTION-FILE
           
      *    Build transaction record
           MOVE WS-TRANSACTION-COUNTER TO TXN-ID
           MOVE WS-DATE-NUMERIC TO TXN-DATE
           MOVE WS-TIME-NUMERIC TO TXN-TIME
           MOVE WS-ACCOUNT-PARM TO TXN-FROM-ACCOUNT
           MOVE ZERO TO TXN-TO-ACCOUNT
           
      *    Set transaction type based on command
           EVALUATE WS-COMMAND
               WHEN "DEPOSIT"
                   MOVE "D" TO TXN-TYPE
                   MOVE "DEPOSIT" TO TXN-DESCRIPTION
               WHEN "WITHDRAW"
                   MOVE "W" TO TXN-TYPE
                   MOVE "WITHDRAWAL" TO TXN-DESCRIPTION
               WHEN "TRANSFER"
                   MOVE "T" TO TXN-TYPE
                   MOVE "TRANSFER" TO TXN-DESCRIPTION
                   MOVE WS-TO-ACCOUNT-PARM TO TXN-TO-ACCOUNT
           END-EVALUATE
           
           MOVE WS-AMOUNT-PARM TO TXN-AMOUNT
           MOVE "P" TO TXN-STATUS
           
      *    Write transaction record
           WRITE TRANSACTION-RECORD
           
           CLOSE TRANSACTION-FILE.
       
       GET-NEXT-TRANSACTION-ID.
      *    Try to read existing counter
           OPEN INPUT COUNTER-FILE
           READ COUNTER-FILE
               AT END
                   MOVE 1 TO WS-TRANSACTION-COUNTER
               NOT AT END
                   MOVE COUNTER-RECORD TO WS-TRANSACTION-COUNTER
                   ADD 1 TO WS-TRANSACTION-COUNTER
           END-READ
           CLOSE COUNTER-FILE
           
      *    Write updated counter
           OPEN OUTPUT COUNTER-FILE
           MOVE WS-TRANSACTION-COUNTER TO COUNTER-RECORD
           WRITE COUNTER-RECORD
           CLOSE COUNTER-FILE.
       

       
       DISPLAY-USAGE.
           DISPLAY " "
           DISPLAY "BANK LEDGER SYSTEM - Usage Instructions"
           DISPLAY "======================================"
           DISPLAY " "
           DISPLAY "CREATE account-num customer-name initial-balance"
           DISPLAY "  Example: CREATE 1234567890 Customer_Name 250.00"
           DISPLAY " "
           DISPLAY "DEPOSIT account-num amount"
           DISPLAY "  Example: DEPOSIT 1234567890 250.50"
           DISPLAY " "
           DISPLAY "WITHDRAW account-num amount"
           DISPLAY "  Example: WITHDRAW 1234567890 100.00"
           DISPLAY " "
           DISPLAY "TRANSFER from-account to-account amount"
           DISPLAY "  Example: TRANSFER 1234567890 9876543210 500.00"
           DISPLAY " "
           DISPLAY "BALANCE account-num"
           DISPLAY "  Example: BALANCE 1234567890"
           DISPLAY " "
           DISPLAY "HISTORY account-num"
           DISPLAY "  Example: HISTORY 1234567890"
           DISPLAY " "
           DISPLAY "LIST"
           DISPLAY "  Example: LIST"
           DISPLAY " ".
       
       DISPLAY-TRANSACTION-HISTORY-INLINE.
           DISPLAY " "
           DISPLAY "TRANSACTION HISTORY FOR ACCOUNT: " WS-ACCOUNT-PARM
           DISPLAY "---------------------------------------------"
           OPEN INPUT TRANSACTION-FILE
           MOVE 0 TO WS-TRANSACTION-COUNT
           SET WS-NOT-EOF TO TRUE
           PERFORM UNTIL WS-EOF
               READ TRANSACTION-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       IF TXN-FROM-ACCOUNT = WS-ACCOUNT-PARM OR
                          TXN-TO-ACCOUNT = WS-ACCOUNT-PARM
                           ADD 1 TO WS-TRANSACTION-COUNT
                           DISPLAY "Date: " TXN-DATE
                                   " Time: " TXN-TIME
                                   " Type: " TXN-TYPE
                                   " Amount: " TXN-AMOUNT
                                   " Desc: " TXN-DESCRIPTION
                                   " Status: " TXN-STATUS
                       END-IF
               END-READ
           END-PERFORM
           CLOSE TRANSACTION-FILE
           IF WS-TRANSACTION-COUNT = 0
               DISPLAY "No transactions found for this account."
           END-IF
           DISPLAY " ".       
       
       LIST-ALL-ACCOUNTS.
           OPEN INPUT ACCOUNT-FILE
           DISPLAY "ACCOUNT-NUMBER CUSTOMER-NAME      BALANCE    STATUS"
           DISPLAY "==================================================="
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ ACCOUNT-FILE NEXT
                   AT END
                       CONTINUE
                   NOT AT END
                       DISPLAY ACC-NUMBER " " ACC-CUSTOMER-NAME 
                       DISPLAY" " ACC-BALANCE " " ACC-STATUS
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE.

       PARSE-AMOUNT-VALUE.
      *    Convert amount using NUMVAL function for decimal handling
           IF WS-TEMP-AMOUNT NOT = SPACES
               COMPUTE WS-AMOUNT-PARM = FUNCTION NUMVAL(WS-TEMP-AMOUNT)
           ELSE
               MOVE 0.00 TO WS-AMOUNT-PARM
           END-IF.
       
       TERMINATION.
      *    Cleanup activities if needed
           CONTINUE.
       
       END PROGRAM BANKLEDG.
      
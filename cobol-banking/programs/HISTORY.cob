       IDENTIFICATION DIVISION.
       PROGRAM-ID. HISTORY.
      *********************************************************
      * TRANSACTION HISTORY UTILITY
      * This program displays transaction history for a given
      * account number with formatting and summary information
      *********************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "data/TRANSACT.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT ACCOUNT-FILE ASSIGN TO "data/ACCOUNTS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACC-NUMBER
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD  TRANSACTION-FILE.
           COPY "copybooks/TRANSACTION-RECORD.cpy".
       
       FD  ACCOUNT-FILE.
           COPY "copybooks/ACCOUNT-RECORD.cpy".
       
       WORKING-STORAGE SECTION.
           COPY "copybooks/WORKING-STORAGE.cpy".
       
      *    History-specific working storage
       01  WS-SEARCH-ACCOUNT           PIC 9(10).
       01  WS-TRANSACTION-COUNT        PIC 9(5) VALUE ZERO.
       01  WS-TOTAL-DEPOSITS           PIC 9(10)V99 VALUE ZERO.
       01  WS-TOTAL-WITHDRAWALS        PIC 9(10)V99 VALUE ZERO.
       01  WS-TOTAL-TRANSFERS-IN       PIC 9(10)V99 VALUE ZERO.
       01  WS-TOTAL-TRANSFERS-OUT      PIC 9(10)V99 VALUE ZERO.
       
       01  WS-FORMATTED-DATE           PIC 99/99/9999.
       01  WS-FORMATTED-TIME           PIC X(8).
       01  WS-FORMATTED-AMOUNT         PIC Z,ZZZ,ZZ9.99.
       01  WS-TYPE-DESCRIPTION         PIC X(15).
       
       LINKAGE SECTION.
       01  LS-ACCOUNT-NUMBER           PIC 9(10).
       01  LS-RETURN-CODE              PIC 9(2).
       
       PROCEDURE DIVISION USING LS-ACCOUNT-NUMBER LS-RETURN-CODE.
       
       MAIN-HISTORY-PROCESSING.
      *    Initialize
           PERFORM INITIALIZATION
           
      *    Validate account exists
           PERFORM VALIDATE-ACCOUNT
           
      *    Display transaction history if account is valid
           IF NO-ERROR
               PERFORM DISPLAY-TRANSACTION-HISTORY
               PERFORM DISPLAY-SUMMARY-TOTALS
               MOVE 0 TO LS-RETURN-CODE
           ELSE
               MOVE 8 TO LS-RETURN-CODE
           END-IF
           
           GOBACK.
       
       INITIALIZATION.
           MOVE LS-ACCOUNT-NUMBER TO WS-SEARCH-ACCOUNT
           SET NO-ERROR TO TRUE
           MOVE ZERO TO WS-TRANSACTION-COUNT
           MOVE ZERO TO WS-TOTAL-DEPOSITS
           MOVE ZERO TO WS-TOTAL-WITHDRAWALS
           MOVE ZERO TO WS-TOTAL-TRANSFERS-IN
           MOVE ZERO TO WS-TOTAL-TRANSFERS-OUT.
       
       VALIDATE-ACCOUNT.
      *    Check if account exists and get account info
           OPEN INPUT ACCOUNT-FILE
           READ ACCOUNT-FILE KEY IS WS-SEARCH-ACCOUNT
               INVALID KEY
                   DISPLAY "Error: Account " WS-SEARCH-ACCOUNT 
                           " not found"
                   SET ERROR-OCCURRED TO TRUE
               NOT INVALID KEY
                   DISPLAY " "
                   DISPLAY "TRANSACTION HISTORY FOR ACCOUNT: " 
                           ACC-NUMBER
                   DISPLAY "CUSTOMER: " ACC-CUSTOMER-NAME
                   MOVE ACC-OPEN-DATE TO WS-DATE-NUMERIC
                   PERFORM FORMAT-DATE
                   DISPLAY "ACCOUNT OPENED: " WS-FORMATTED-DATE
                   DISPLAY " "
           END-READ
           CLOSE ACCOUNT-FILE.
       
       DISPLAY-TRANSACTION-HISTORY.
      *    Display header
           DISPLAY "DATE       TIME     TYPE           AMOUNT      " &
                   "DESCRIPTION          STATUS"
           DISPLAY "==========================================" &
                   "=================================="
           
      *    Open transaction file and read all records
           OPEN INPUT TRANSACTION-FILE
           
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ TRANSACTION-FILE
                   AT END
                       CONTINUE
                   NOT AT END
      *                Check if this transaction involves our account
                       IF TXN-FROM-ACCOUNT = WS-SEARCH-ACCOUNT OR
                          TXN-TO-ACCOUNT = WS-SEARCH-ACCOUNT
                           PERFORM DISPLAY-TRANSACTION-DETAIL
                           PERFORM UPDATE-SUMMARY-TOTALS
                           ADD 1 TO WS-TRANSACTION-COUNT
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE TRANSACTION-FILE
           
      *    Display message if no transactions found
           IF WS-TRANSACTION-COUNT = ZERO
               DISPLAY "No transactions found for this account"
           END-IF.
       
       DISPLAY-TRANSACTION-DETAIL.
      *    Format date from YYYYMMDD to MM/DD/YYYY
           MOVE TXN-DATE TO WS-DATE-NUMERIC
           PERFORM FORMAT-DATE
           
      *    Format time from HHMMSS to HH:MM:SS
           MOVE TXN-TIME TO WS-TIME-NUMERIC
           PERFORM FORMAT-TIME
           
      *    Format amount with commas and decimal
           MOVE TXN-AMOUNT TO WS-FORMATTED-AMOUNT
           
      *    Determine transaction description
           EVALUATE TXN-TYPE
               WHEN "D"
                   MOVE "DEPOSIT" TO WS-TYPE-DESCRIPTION
               WHEN "W"
                   MOVE "WITHDRAWAL" TO WS-TYPE-DESCRIPTION
               WHEN "T"
                   IF TXN-FROM-ACCOUNT = WS-SEARCH-ACCOUNT
                       MOVE "TRANSFER OUT" TO WS-TYPE-DESCRIPTION
                   ELSE
                       MOVE "TRANSFER IN" TO WS-TYPE-DESCRIPTION
                   END-IF
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-TYPE-DESCRIPTION
           END-EVALUATE
           
      *    Display the formatted transaction line
           DISPLAY WS-FORMATTED-DATE " " WS-FORMATTED-TIME " " 
                   WS-TYPE-DESCRIPTION " " WS-FORMATTED-AMOUNT " " 
                   TXN-DESCRIPTION " " TXN-STATUS.
       
       UPDATE-SUMMARY-TOTALS.
      *    Add to appropriate total based on transaction type
           EVALUATE TXN-TYPE
               WHEN "D"
                   ADD TXN-AMOUNT TO WS-TOTAL-DEPOSITS
               WHEN "W"
                   ADD TXN-AMOUNT TO WS-TOTAL-WITHDRAWALS
               WHEN "T"
                   IF TXN-FROM-ACCOUNT = WS-SEARCH-ACCOUNT
                       ADD TXN-AMOUNT TO WS-TOTAL-TRANSFERS-OUT
                   ELSE
                       ADD TXN-AMOUNT TO WS-TOTAL-TRANSFERS-IN
                   END-IF
           END-EVALUATE.
       
       DISPLAY-SUMMARY-TOTALS.
           IF WS-TRANSACTION-COUNT > ZERO
               DISPLAY " "
               DISPLAY "TRANSACTION SUMMARY:"
               DISPLAY "==================="
               DISPLAY "Total Transactions: " WS-TRANSACTION-COUNT
               
               IF WS-TOTAL-DEPOSITS > ZERO
                   MOVE WS-TOTAL-DEPOSITS TO WS-FORMATTED-AMOUNT
                   DISPLAY "Total Deposits:     $" WS-FORMATTED-AMOUNT
               END-IF
               
               IF WS-TOTAL-WITHDRAWALS > ZERO
                   MOVE WS-TOTAL-WITHDRAWALS TO WS-FORMATTED-AMOUNT
                   DISPLAY "Total Withdrawals:  $" WS-FORMATTED-AMOUNT
               END-IF
               
               IF WS-TOTAL-TRANSFERS-IN > ZERO
                   MOVE WS-TOTAL-TRANSFERS-IN TO WS-FORMATTED-AMOUNT
                   DISPLAY "Total Transfers In: $" WS-FORMATTED-AMOUNT
               END-IF
               
               IF WS-TOTAL-TRANSFERS-OUT > ZERO
                   MOVE WS-TOTAL-TRANSFERS-OUT TO WS-FORMATTED-AMOUNT
                   DISPLAY "Total Transfers Out:$" WS-FORMATTED-AMOUNT
               END-IF
           END-IF.
       
       FORMAT-DATE.
      *    Convert YYYYMMDD to MM/DD/YYYY format
           DIVIDE WS-DATE-NUMERIC BY 10000 GIVING WS-YEAR
               REMAINDER WS-DATE-NUMERIC
           DIVIDE WS-DATE-NUMERIC BY 100 GIVING WS-MONTH
               REMAINDER WS-DAY
           
           MOVE WS-MONTH TO WS-FORMATTED-DATE(1:2)
           MOVE "/" TO WS-FORMATTED-DATE(3:1)
           MOVE WS-DAY TO WS-FORMATTED-DATE(4:2)
           MOVE "/" TO WS-FORMATTED-DATE(6:1)
           MOVE WS-YEAR TO WS-FORMATTED-DATE(7:4).
       
       FORMAT-TIME.
      *    Convert HHMMSS to HH:MM:SS format
           DIVIDE WS-TIME-NUMERIC BY 10000 GIVING WS-HOUR
               REMAINDER WS-TIME-NUMERIC
           DIVIDE WS-TIME-NUMERIC BY 100 GIVING WS-MINUTE
               REMAINDER WS-SECOND
           
           MOVE WS-HOUR TO WS-FORMATTED-TIME(1:2)
           MOVE ":" TO WS-FORMATTED-TIME(3:1)
           MOVE WS-MINUTE TO WS-FORMATTED-TIME(4:2)
           MOVE ":" TO WS-FORMATTED-TIME(6:1)
           MOVE WS-SECOND TO WS-FORMATTED-TIME(7:2).
       
       END PROGRAM HISTORY.
      
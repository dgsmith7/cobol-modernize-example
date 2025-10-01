      *********************************************************
      * TRANSACTION-RECORD.cpy - Transaction Record Structure
      * This copybook defines the layout for transaction records
      * stored in the transaction history file (TRANSACT.DAT)
      *********************************************************
       01  TRANSACTION-RECORD.
      *    Transaction ID - unique 15 digit identifier
           05  TXN-ID                  PIC 9(15).
      *    Transaction date - YYYYMMDD format
           05  TXN-DATE                PIC 9(8).
      *    Transaction time - HHMMSS format
           05  TXN-TIME                PIC 9(6).
      *    Source account number for transaction
           05  TXN-FROM-ACCOUNT        PIC 9(10).
      *    Destination account (for transfers) or zeros
           05  TXN-TO-ACCOUNT          PIC 9(10).
      *    Transaction type: D=Deposit, W=Withdrawal, T=Transfer
           05  TXN-TYPE                PIC X(1).
      *    Transaction amount - always positive
           05  TXN-AMOUNT              PIC 9(10)V99 COMP-3.
      *    Transaction description/memo
           05  TXN-DESCRIPTION         PIC X(40).
      *    Processing status: P=Processed, F=Failed, R=Reversed
           05  TXN-STATUS              PIC X(1).
      *    Filler for future expansion
           05  FILLER                  PIC X(10).
      *    Record length: 15+8+6+10+10+1+6+40+1+10 = 107 bytes
      
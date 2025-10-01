      *********************************************************
      * ACCOUNT-RECORD.cpy - Account Master Record Structure
      * This copybook defines the layout for account records
      * stored in the account master file (ACCOUNT.DAT)
      *********************************************************
       01  ACCOUNT-RECORD.
      *    Account number - 10 digit numeric identifier
           05  ACC-NUMBER              PIC 9(10).
      *    Customer name - up to 30 characters
           05  ACC-CUSTOMER-NAME       PIC X(30).
      *    Account balance - signed decimal with 2 decimal places
      *    S9(10)V99 allows for balances up to $99,999,999.99
           05  ACC-BALANCE             PIC S9(10)V99 COMP-3.
      *    Account status - A=Active, C=Closed, F=Frozen
           05  ACC-STATUS              PIC X(1).
      *    Date account opened - YYYYMMDD format
           05  ACC-OPEN-DATE           PIC 9(8).
      *    Filler for future expansion and record alignment
           05  FILLER                  PIC X(15).
      *    Record length: 10+30+6+1+8+15 = 70 bytes
      
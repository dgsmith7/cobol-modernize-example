# COBOL Bank Ledger System

> **Note:** This README documents the COBOL core system only. For the REST API and React web UI, see the READMEs in `api-server/` and `cobol-banking-ui/`.

A comprehensive banking system written in COBOL that demonstrates classic mainframe programming techniques for financial applications. This system provides complete account management, transaction processing, and reporting capabilities using COBOL-friendly sequential files.

## Features

- **Account Management**: Create, maintain, and query customer accounts
- **Transaction Processing**: Handle deposits, withdrawals, and transfers
- **Data Integrity**: Atomic transactions with rollback capabilities (see notes below)
- **Transaction History**: Complete audit trail of all banking operations
- **Command-Line Interface**: Parameter-driven execution for easy integration
- **Error Handling**: Comprehensive validation and error reporting
- **File Management**: Efficient sequential file operations with append-based updates

## System Architecture

The system consists of several modular COBOL programs:

### Core Programs

1. **BANKLEDG.cob** - Main program providing CLI interface
2. **TRANSFER.cob** - Specialized transfer processing with atomic transactions (see notes)
3. **HISTORY.cob** - Transaction history reporting utility

### Data Structures (Copybooks)

1. **ACCOUNT-RECORD.cpy** - Account master record layout
2. **TRANSACTION-RECORD.cpy** - Transaction record structure
3. **WORKING-STORAGE.cpy** - Common variables and constants

### Data Files

1. **ACCOUNTS.DAT** - Sequential account master file with append-based updates
2. **TRANSACT.DAT** - Sequential transaction history file
3. **COUNTER.DAT** - Transaction ID counter file

## File Layouts

### Account Record (70 bytes)

```
Position  Size  Field               Description
1-10      10    ACC-NUMBER          Account number (numeric)
11-40     30    ACC-CUSTOMER-NAME   Customer name
41-46     6     ACC-BALANCE         Account balance (COMP-3)
47        1     ACC-STATUS          A=Active, C=Closed, F=Frozen
48-55     8     ACC-OPEN-DATE       Date opened (YYYYMMDD)
56-70     15    FILLER              Reserved for expansion
```

### Transaction Record (107 bytes)

```
Position  Size  Field               Description
1-15      15    TXN-ID              Unique transaction ID
16-23     8     TXN-DATE            Date (YYYYMMDD)
24-29     6     TXN-TIME            Time (HHMMSS)
30-39     10    TXN-FROM-ACCOUNT    Source account
40-49     10    TXN-TO-ACCOUNT      Destination account
50        1     TXN-TYPE            D=Deposit, W=Withdrawal, T=Transfer
51-56     6     TXN-AMOUNT          Amount (COMP-3)
57-96     40    TXN-DESCRIPTION     Description/memo
97        1     TXN-STATUS          P=Processed, F=Failed, R=Reversed
98-107    10    FILLER              Reserved for expansion
```

## Installation and Setup

### Prerequisites

- **GnuCOBOL** (formerly OpenCOBOL) compiler
- Unix/Linux environment with bash shell
- Basic understanding of COBOL and file systems

### Installation Steps

1. **Clone or download the system files**

   ```bash
   git clone [repository-url] cobol-banking
   cd cobol-banking
   ```

2. **Make scripts executable**

   ```bash
   chmod +x compile.sh demo.sh
   ```

3. **Compile the programs**

   ```bash
   ./compile.sh
   ```

   This creates executable files in the `bin/` directory.

## Usage

### Command Syntax

```bash
./bin/BANKLEDG COMMAND PARAMETERS
```

### Available Commands

The COBOL Bank Ledger System responds to the following commands:

#### CREATE - Create New Account

```bash
./bin/BANKLEDG CREATE account-number "Customer Name" initial-balance
```

**Parameters:**

- `account-number`: 10-digit account number (must be unique)
- `Customer Name`: Customer name in quotes (up to 30 characters)
- `initial-balance`: Starting balance (minimum $10.00)

**Examples:**

```bash
./bin/BANKLEDG CREATE 1234567890 "John Doe" 1000.00
./bin/BANKLEDG CREATE 9876543210 "Jane Smith" 2500.75
```

**Output:**

```
Account 1234567890 created for John Doe
Initial balance: $     1,000.00
```

#### DEPOSIT - Add Money to Account

```bash
./bin/BANKLEDG DEPOSIT account-number amount
```

**Parameters:**

- `account-number`: Existing 10-digit account number
- `amount`: Deposit amount (maximum $99,999.99)

**Examples:**

```bash
./bin/BANKLEDG DEPOSIT 1234567890 250.50
./bin/BANKLEDG DEPOSIT 9876543210 1000.00
```

**Output:**

```
Deposit of $      250.50 processed for account 1234567890
New balance: $     1,250.50
```

#### WITHDRAW - Remove Money from Account

```bash
./bin/BANKLEDG WITHDRAW account-number amount
```

**Parameters:**

- `account-number`: Existing 10-digit account number
- `amount`: Withdrawal amount (subject to minimum balance rules)

**Examples:**

```bash
./bin/BANKLEDG WITHDRAW 1234567890 100.00
./bin/BANKLEDG WITHDRAW 9876543210 50.25
```

**Output:**

```
Withdrawal of $      100.00 processed for account 1234567890
New balance: $     1,150.50
```

#### TRANSFER - Move Money Between Accounts

```bash
./bin/BANKLEDG TRANSFER from-account to-account amount
```

**Parameters:**

- `from-account`: Source account (10-digit account number)
- `to-account`: Destination account (10-digit account number)
- `amount`: Transfer amount (same limits as withdrawal)

**Examples:**

```bash
./bin/BANKLEDG TRANSFER 1234567890 9876543210 500.00
./bin/BANKLEDG TRANSFER 9876543210 1234567890 250.00
```

**Output:**

```
Transfer functionality requires full implementation
For demo: use separate DEPOSIT/WITHDRAW operations
```

#### LIST - List All Accounts

```bash
./bin/BANKLEDG LIST
```

**Output:**

```
ACCOUNT-NUMBER CUSTOMER-NAME                BALANCE     STATUS
=============================================================
1234567890     John Doe                     1000.00     A
...
```

#### BALANCE - Check Account Balance

```bash
./bin/BANKLEDG BALANCE account-number
```

**Parameters:**

- `account-number`: Existing 10-digit account number

**Examples:**

```bash
./bin/BANKLEDG BALANCE 1234567890
./bin/BANKLEDG BALANCE 9876543210
```

**Output:**

```
Account: 1234567890
Customer: John Doe
Balance: $     1,150.50
Status: A
```

#### HISTORY - View Transaction History

```bash
./bin/BANKLEDG HISTORY account-number
```

**Parameters:**

- `account-number`: Existing 10-digit account number

**Examples:**

```bash
./bin/BANKLEDG HISTORY 1234567890
./bin/BANKLEDG HISTORY 9876543210
```

**Output:**

```
TRANSACTION HISTORY FOR ACCOUNT: 1234567890
---------------------------------------------
Date: 20251003 Time: 093722 Type: D Amount: 0000000250.50 Desc: DEPOSIT                                  Status: P
Date: 20251003 Time: 093722 Type: W Amount: 0000000100.00 Desc: WITHDRAWAL                               Status: P
Date: 20251003 Time: 093722 Type: T Amount: 0000000200.00 Desc: TRANSFER                                 Status: P
```

### Command Return Codes

All commands return standard Unix exit codes:

- **0**: Success - operation completed successfully
- **8**: Error - invalid parameters, account not found, insufficient funds, etc.

### Parameter Validation

The system validates all input parameters:

- **Account numbers** must be exactly 10 digits
- **Customer names** must be enclosed in quotes and ≤ 30 characters
- **Amounts** must be positive numbers with up to 2 decimal places
- **Required parameters**: missing parameters cause immediate error

## Demo

Run the included demonstration script to see all features:

```bash
./demo.sh
```

This script will:

1. Create sample accounts
2. Perform various transactions
3. Display account balances
4. Show transaction histories
5. Demonstrate error handling

## Business Rules and Limits

### Account Rules

- Account numbers must be 10 digits
- Customer names up to 30 characters
- Account status: A=Active, C=Closed, F=Frozen
- Only active accounts can process transactions

### Transaction Limits

- **Maximum Deposit**: $99,999.99 per transaction
- **Maximum Withdrawal**: $50,000.00 per transaction
- **Minimum Balance**: -$1,000.00 (allows overdraft)
- **Transfer Limit**: Same as withdrawal limit

### Data Validation

- Positive amounts required for all transactions
- Account existence verified before processing
- Sufficient funds checked for withdrawals/transfers
- Atomic transfers with automatic rollback on failure (see notes)

## Error Handling

The system provides comprehensive error checking:

- **Invalid Account Numbers**: Non-existent accounts rejected
- **Insufficient Funds**: Withdrawals/transfers blocked if balance too low
- **Invalid Amounts**: Zero or negative amounts rejected
- **File Errors**: Database corruption or access issues reported
- **Business Rule Violations**: Transaction limits enforced
- **Parameter Validation**: Missing or malformed parameters caught

## Technical Implementation

### File Access Methods

- **Sequential Files**: ACCOUNTS.DAT uses sequential organization for GnuCOBOL compatibility
- **Append-based Updates**: Account updates create new records with most recent data
- **Sequential Processing**: All file operations use sequential access patterns
- **Search Operations**: Linear search through files to find account records

### Transaction Processing

- **Atomic Operations**: Transfer operations are all-or-nothing (see notes)
- **Rollback Capability**: Failed transfers automatically reverse (see notes)
- **Audit Trail**: All operations logged with timestamps
- **Unique IDs**: Sequential transaction numbering

### Performance Considerations

- Sequential file access for reliable GnuCOBOL compatibility
- Append-based updates for transaction logging
- Efficient COBOL data types (COMP-3 for decimals)
- Most recent record lookup for current account state

## File Structure

```
cobol-banking/
├── copybooks/
│   ├── ACCOUNT-RECORD.cpy      # Account record layout
│   ├── TRANSACTION-RECORD.cpy  # Transaction record layout
│   └── WORKING-STORAGE.cpy     # Common working storage
├── programs/
│   ├── BANKLEDG.cob           # Main banking program
│   ├── TRANSFER.cob           # Transfer utility
│   └── HISTORY.cob            # History utility
├── data/                      # Data files (created at runtime)
│   ├── ACCOUNTS.DAT           # Account master file
│   ├── TRANSACT.DAT           # Transaction history
│   └── COUNTER.DAT            # Transaction counter
├── bin/                       # Compiled executables
│   ├── BANKLEDG               # Main program
│   ├── TRANSFER               # Transfer module
│   └── HISTORY                # History module
├── compile.sh                 # Compilation script
├── demo.sh                    # Demonstration script
└── README.md                  # This file
```

## Extending the System

The modular design allows for easy extensions:

### Additional Features

- **Interest Calculation**: Add periodic interest processing
- **Account Types**: Checking, savings, credit accounts
- **Transaction Categories**: Expense categorization
- **Reporting**: Monthly statements, account summaries
- **Security**: PIN verification, access logging

### Integration Points

- **Database Connectivity**: Replace flat files with RDBMS
- **Web Interface**: A REST API and React front end are provided in this repository (see `api-server/` and `cobol-banking-ui/`)
- **Batch Processing**: Nightly processing jobs
- **External Systems**: Integration with other banking modules

## Troubleshooting

### Common Issues

1. **Compilation Errors**

   - Ensure GnuCOBOL is installed: `cobc --version`
   - Check COPYBOOK path settings
   - Verify file permissions on source files

2. **Runtime Errors**

   - Check data directory exists and is writable
   - Verify account numbers are 10 digits
   - Ensure amounts are properly formatted

3. **File Access Problems**
   - Check file permissions in data/ directory
   - Verify disk space availability
   - Ensure no file locks from previous runs

### Debug Mode

Add debug statements to programs for troubleshooting:

```cobol
DISPLAY "DEBUG: Account = " ACC-NUMBER
DISPLAY "DEBUG: Balance = " ACC-BALANCE
```

## Educational Value

This system demonstrates key COBOL programming concepts:

- **File Handling**: Sequential file operations and append-based updates
- **Data Definition**: Copybooks and record structures
- **Business Logic**: Financial calculations and validations
- **Error Handling**: Comprehensive exception management
- **Modular Design**: Separate programs for specific functions
- **CLI Programming**: Parameter processing and user interaction

## Contributing

To enhance this system:

1. Fork the repository
2. Create feature branches
3. Follow COBOL naming conventions
4. Add comprehensive comments
5. Test thoroughly with edge cases
6. Update documentation

## License

This educational project is provided as-is for learning purposes. Feel free to modify and distribute according to your needs.

---

## Complete Command Reference

### Quick Command Summary

| Command  | Syntax                                         | Purpose                     |
| -------- | ---------------------------------------------- | --------------------------- |
| CREATE   | `./bin/BANKLEDG CREATE account "name" balance` | Create new account          |
| DEPOSIT  | `./bin/BANKLEDG DEPOSIT account amount`        | Add money to account        |
| WITHDRAW | `./bin/BANKLEDG WITHDRAW account amount`       | Remove money from account   |
| TRANSFER | `./bin/BANKLEDG TRANSFER from to amount`       | Move money between accounts |
| LIST     | `./bin/BANKLEDG LIST`                          | List all accounts           |
| BALANCE  | `./bin/BANKLEDG BALANCE account`               | Check account balance       |
| HISTORY  | `./bin/BANKLEDG HISTORY account`               | View transaction history    |

### Error Messages

Common error messages and their meanings:

- `"Error: Account number required"` - Missing account parameter
- `"Error: Account XXXXXXXXXX not found"` - Account doesn't exist
- `"Error: Account is not active"` - Account status is not 'A'
- `"Error: Insufficient funds"` - Withdrawal exceeds available balance
- `"Error: Cannot open account file"` - File system or permission issue
- `"Invalid command"` - Command not recognized

### System Files

The system creates and maintains these data files:

- `data/ACCOUNTS.DAT` - Account master file (sequential format)
- `data/TRANSACT.DAT` - Transaction history file
- `data/COUNTER.DAT` - Transaction ID counter

**Note**: This is an educational banking system for learning COBOL programming. It is not intended for production use without additional security, audit, and compliance features required for real financial applications.

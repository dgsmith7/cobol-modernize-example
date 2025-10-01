# COBOL Banking System Modernization Project# COBOL Banking System Modernization Project

## Phase 2: Legacy System Modernization## Phase 2: Legacy System Modernization

This project demonstrates the modernization of a legacy COBOL banking system by wrapping it with modern Node.js APIs and a contemporary web frontend, while keeping the proven COBOL core completely intact.This project demonstrates the modernization of a legacy COBOL banking system by wrapping it with modern Node.js APIs and a contemporary web frontend, while keeping the proven COBOL core completely intact.

## 🎯 Project Overview## 🎯 Project Overview

We are simulating a real-world enterprise modernization scenario where a bank needs to:We are simulating a real-world enterprise modernization scenario where a bank needs to:

- **Preserve existing COBOL business logic** (zero modifications to working code)- **Preserve existing COBOL business logic** (zero modifications to working code)

- **Add modern API capabilities** for integration with contemporary systems- **Add modern API capabilities** for integration with contemporary systems

- **Provide a modern web interface** for improved user experience- **Provide a modern web interface** for improved user experience

- **Maintain data integrity** and existing file-based operations- **Maintain data integrity** and existing file-based operations

## 📁 Project Structure## 📁 Project Structure

`````

cobol-modernize-example/cobol-modernize-example/

├── README.md                    # This file - Phase 2 modernization plan├── README.md                    # This file - Phase 2 modernization plan

├── package.json                 # Main project package.json├── package.json                 # Main project package.json

├── cobol-banking/              # Phase 1 - Original COBOL system (COMPLETE)├── cobol-banking/              # Phase 1 - Original COBOL system (COMPLETE)

│   ├── README.md               # COBOL system documentation│   ├── README.md               # COBOL system documentation

│   ├── compile.sh              # COBOL compilation script│   ├── compile.sh              # COBOL compilation script

│   ├── demo.sh                 # COBOL demonstration script│   ├── demo.sh                 # COBOL demonstration script

│   ├── copybooks/              # COBOL data structures│   ├── copybooks/              # COBOL data structures

│   ├── programs/               # COBOL source code│   ├── programs/               # COBOL source code

│   ├── bin/                    # Compiled COBOL executables│   ├── bin/                    # Compiled COBOL executables

│   └── data/                   # COBOL data files│   └── data/                   # COBOL data files

├── api-server/                 # Phase 2A - Node.js API layer (IN PROGRESS)├── api-server/                 # Phase 2 - Node.js API layer (IN PROGRESS)

│   ├── package.json│   ├── package.json

│   ├── server.js│   ├── server.js

│   ├── routes/│   ├── routes/

│   ├── middleware/│   ├── middleware/

│   ├── controllers/│   ├── controllers/

│   └── utils/│   └── utils/

├── web-frontend/               # Phase 2B - Modern web interface (PLANNED)├── web-frontend/               # Phase 2 - Modern web interface (PLANNED)

│   ├── package.json│   ├── package.json

│   ├── public/│   ├── public/

│   ├── src/│   ├── src/

│   └── build/│   └── build/

└── docs/                       # Phase 2C - API documentation (PLANNED)└── docs/                       # Phase 2 - API documentation (PLANNED)

    ├── api-spec.md    ├── api-spec.md

    └── integration-guide.md    └── integration-guide.md

```

│   ├── routes/### Data Files

## 🏗️ Architecture Overview

│   ├── middleware/

### **Integration Strategy**

```│   ├── controllers/1. **ACCOUNTS.DAT** - Indexed account master file

┌─────────────────┐    HTTP/JSON    ┌──────────────────┐    Shell Exec    ┌─────────────────┐

│   Web Frontend │◄────────────────►│  Node.js API     │◄─────────────────►│  COBOL Banking  ││   └── utils/2. **TRANSACT.DAT** - Sequential transaction history file

│   (React/Vue)  │                  │  (Express.js)    │                   │  System         │

└─────────────────┘                  └──────────────────┘                   └─────────────────┘├── web-frontend/               # Phase 2 - Modern web interface (PLANNED)3. **COUNTER.DAT** - Transaction ID counter file

                                            │                                        │

                                            ▼                                        ▼│   ├── package.json

                                     ┌──────────────┐                        ┌─────────────────┐

                                     │  API Logs    │                        │  ACCOUNTS.DAT   ││   ├── public/## File Layouts

                                     │  Error Logs  │                        │  Data Files     │

                                     └──────────────┘                        └─────────────────┘│   ├── src/

```

│   └── build/### Account Record (70 bytes)

### **Core Principles**

1. **Zero COBOL Modifications** - The working COBOL system remains untouched└── docs/                       # Phase 2 - API documentation (PLANNED)

2. **Process Integration** - Node.js spawns COBOL executables via child processes

3. **Data Transformation** - Convert COBOL text output to JSON responses    ├── api-spec.md```

4. **Error Handling** - Robust error management and logging

5. **API-First Design** - RESTful APIs for all banking operations    └── integration-guide.mdPosition  Size  Field               Description



## 🚀 API Specification```1-10      10    ACC-NUMBER          Account number (numeric)



### **Complete COBOL Command Mapping**11-40     30    ACC-CUSTOMER-NAME   Customer name



| COBOL Command | API Endpoint | Method | Purpose |## 🏗️ Architecture Overview41-46     6     ACC-BALANCE         Account balance (COMP-3)

|---------------|--------------|--------|---------|

| `CREATE account "name" balance` | `/api/accounts` | POST | Create new account |47        1     ACC-STATUS          A=Active, C=Closed, F=Frozen

| `BALANCE account` | `/api/accounts/:id` | GET | Get account details & balance |

| `DEPOSIT account amount` | `/api/accounts/:id/deposit` | POST | Make deposit |### **Integration Strategy**48-55     8     ACC-OPEN-DATE       Date opened (YYYYMMDD)

| `WITHDRAW account amount` | `/api/accounts/:id/withdraw` | POST | Make withdrawal |

| `TRANSFER from to amount` | `/api/transfers` | POST | Transfer between accounts |```56-70     15    FILLER              Reserved for expansion

| `HISTORY account` | `/api/accounts/:id/history` | GET | Transaction history |

┌─────────────────┐    HTTP/JSON    ┌──────────────────┐    Shell Exec    ┌─────────────────┐```

### **Core API Endpoints**

│   Web Frontend │◄────────────────►│  Node.js API     │◄─────────────────►│  COBOL Banking  │

#### **Account Management**

```javascript│   (React/Vue)  │                  │  (Express.js)    │                   │  System         │### Transaction Record (107 bytes)

// Create new account

POST /api/accounts└─────────────────┘                  └──────────────────┘                   └─────────────────┘

{

  "accountNumber": "1234567890",                                            │                                        │```

  "customerName": "John Doe",

  "initialBalance": 1000.00                                            ▼                                        ▼Position  Size  Field               Description

}

                                     ┌──────────────┐                        ┌─────────────────┐1-15      15    TXN-ID              Unique transaction ID

// Get account details and balance

GET /api/accounts/1234567890                                     │  API Logs    │                        │  ACCOUNTS.DAT   │16-23     8     TXN-DATE            Date (YYYYMMDD)

Response: {

  "accountNumber": "1234567890",                                     │  Error Logs  │                        │  Data Files     │24-29     6     TXN-TIME            Time (HHMMSS)

  "customerName": "John Doe",

  "balance": 1000.00,                                     └──────────────┘                        └─────────────────┘30-39     10    TXN-FROM-ACCOUNT    Source account

  "status": "A",

  "dateOpened": "2025-10-01"```40-49     10    TXN-TO-ACCOUNT      Destination account

}

50        1     TXN-TYPE            D=Deposit, W=Withdrawal, T=Transfer

// List all accounts (enhanced capability)

GET /api/accounts### **Core Principles**51-56     6     TXN-AMOUNT          Amount (COMP-3)

Response: [

  { "accountNumber": "1234567890", "customerName": "John Doe", "balance": 1000.00 },1. **Zero COBOL Modifications** - The working COBOL system remains untouched57-96     40    TXN-DESCRIPTION     Description/memo

  { "accountNumber": "9876543210", "customerName": "Jane Smith", "balance": 2500.75 }

]2. **Process Integration** - Node.js spawns COBOL executables via child processes97        1     TXN-STATUS          P=Processed, F=Failed, R=Reversed

```

3. **Data Transformation** - Convert COBOL text output to JSON responses98-107    10    FILLER              Reserved for expansion

#### **Transaction Operations**

```javascript4. **Error Handling** - Robust error management and logging```

// Make deposit

POST /api/accounts/1234567890/deposit5. **API-First Design** - RESTful APIs for all banking operations

{

  "amount": 250.50,## Installation and Setup

  "description": "ATM Deposit"

}## 🚀 API Specification



// Make withdrawal### Prerequisites

POST /api/accounts/1234567890/withdraw

{### **Complete COBOL Command Mapping**

  "amount": 100.00,

  "description": "ATM Withdrawal"- **GnuCOBOL** (formerly OpenCOBOL) compiler

}

| COBOL Command | API Endpoint | Method | Purpose |- Unix/Linux environment with bash shell

// Transfer between accounts

POST /api/transfers|---------------|--------------|--------|---------|- Basic understanding of COBOL and file systems

{

  "fromAccount": "1234567890",| `CREATE account "name" balance` | `/api/accounts` | POST | Create new account |

  "toAccount": "9876543210",

  "amount": 500.00,| `BALANCE account` | `/api/accounts/:id` | GET | Get account details & balance |### Installation Steps

  "description": "Transfer to savings"

}| `DEPOSIT account amount` | `/api/accounts/:id/deposit` | POST | Make deposit |

```

| `WITHDRAW account amount` | `/api/accounts/:id/withdraw` | POST | Make withdrawal |1. **Clone or download the system files**

#### **Reporting & History**

```javascript| `TRANSFER from to amount` | `/api/transfers` | POST | Transfer between accounts |

// Get transaction history

GET /api/accounts/1234567890/history| `HISTORY account` | `/api/accounts/:id/history` | GET | Transaction history |   ```bash

Response: [

  {   git clone [repository-url] cobol-bank-ledger

    "date": "2025-10-01",

    "type": "DEPOSIT", ### **Core API Endpoints**   cd cobol-bank-ledger

    "amount": 250.50,

    "balance": 1250.50,````

    "description": "ATM Deposit"

  }#### **Account Management**

]

````javascript2. **Make scripts executable**

// System reports

GET /api/reports/summary// Create new account

GET /api/reports/balances

GET /api/healthPOST /api/accounts   ```bash

```

{   chmod +x compile.sh demo.sh

## 🛠️ Technology Stack

  "accountNumber": "1234567890",   ```

### **Backend (API Server)**

- **Node.js** - Runtime environment  "customerName": "John Doe",

- **Express.js** - Web framework

- **Child Process** - COBOL program execution  "initialBalance": 1000.003. **Compile the programs**

- **Winston** - Logging framework

- **Joi** - Input validation}

- **CORS** - Cross-origin resource sharing

- **Morgan** - HTTP request logging   ```bash



### **Frontend (Web Interface)**// Get account details and balance     ./compile.sh

- **React.js** or **Vue.js** - UI framework

- **Axios** - HTTP client for API callsGET /api/accounts/1234567890   ```

- **Bootstrap** or **Tailwind CSS** - Styling framework

- **Chart.js** - Data visualizationResponse: {

- **React Router** or **Vue Router** - Client-side routing

  "accountNumber": "1234567890",   This creates executable files in the `bin/` directory.

## 🔧 Implementation Plan

  "customerName": "John Doe",

### **Phase 2A: API Development**

1. **Project Setup**  "balance": 1000.00,## Usage

   - Initialize Node.js project structure ✅

   - Configure Express.js server 🔄  "status": "A",

   - Set up development environment 🔄

  "dateOpened": "2025-09-30"### Command Syntax

2. **COBOL Integration Layer**

   - Create utility functions to execute COBOL programs}

   - Parse COBOL text output into structured data

   - Handle COBOL error codes and return appropriate HTTP status```bash



3. **Core API Routes**// List all accounts./bin/BANKLEDG COMMAND PARAMETERS

   - Implement all COBOL command mappings

   - Add input validation and error handlingGET /api/accounts```

   - Create comprehensive logging

Response: [

4. **Testing & Documentation**

   - Unit tests for all API endpoints  { "accountNumber": "1234567890", "customerName": "John Doe", "balance": 1000.00 },### Available Commands

   - Integration tests with COBOL system

   - Generate API documentation  { "accountNumber": "9876543210", "customerName": "Jane Smith", "balance": 2500.75 }



### **Phase 2B: Frontend Development**]The COBOL Bank Ledger System responds to the following commands:

1. **UI Framework Setup**

   - Create React/Vue application structure````

   - Configure routing and state management

   - Set up styling framework#### CREATE - Create New Account



2. **Core Components**#### **Transaction Operations**

   - Account dashboard

   - Transaction forms`javascript`bash

   - Balance display components

   - Navigation and layout// Make deposit./bin/BANKLEDG CREATE account-number "Customer Name" initial-balance



3. **API Integration**POST /api/accounts/1234567890/deposit```

   - Configure API client

   - Implement error handling{

   - Add loading states and user feedback

"amount": 250.50,**Parameters:**

4. **Polish & Testing**

   - Responsive design implementation"description": "ATM Deposit"

   - Cross-browser testing

   - User experience refinements}- `account-number`: 10-digit account number (must be unique)



## 📋 Development Guidelines- `Customer Name`: Customer name in quotes (up to 30 characters)



### **COBOL Integration Best Practices**// Make withdrawal- `initial-balance`: Starting balance (minimum $10.00)

- **Never modify COBOL code** - Treat as immutable legacy system

- **Parse output reliably** - Handle all possible COBOL response formatsPOST /api/accounts/1234567890/withdraw

- **Graceful error handling** - Convert COBOL errors to appropriate HTTP responses

- **Process isolation** - Each API call spawns fresh COBOL process{**Examples:**

- **Data validation** - Validate inputs before passing to COBOL

"amount": 100.00,

### **API Design Principles**

- **RESTful conventions** - Use standard HTTP methods and status codes"description": "ATM Withdrawal"```bash

- **Consistent responses** - Uniform JSON response structure

- **Input validation** - Validate all inputs before COBOL execution}./bin/BANKLEDG CREATE 1234567890 "John Doe" 1000.00

- **Error handling** - Meaningful error messages and appropriate status codes

- **Documentation** - Comprehensive API documentation with examples./bin/BANKLEDG CREATE 9876543210 "Jane Smith" 2500.75



## 🎯 Success Metrics// Transfer between accounts```



- **100% COBOL Feature Parity** - Every COBOL command accessible via APIPOST /api/transfers

- **Zero COBOL Modifications** - Original system remains completely unchanged

- **API Response Time** - < 200ms for typical operations{**Output:**

- **Error Handling** - Graceful handling of all error scenarios

- **Documentation Coverage** - Complete API and integration documentation"fromAccount": "1234567890",



---"toAccount": "9876543210",```



**This modernization project demonstrates enterprise-grade legacy system integration while preserving the reliability and business logic of the existing COBOL banking system.**"amount": 500.00,Account 1234567890 created for John Doe

"description": "Transfer to savings"Initial balance: $ 1,000.00

}```

````

#### DEPOSIT - Add Money to Account

#### **Reporting & History**

```javascript```bash

// Get transaction history./bin/BANKLEDG DEPOSIT account-number amount

GET /api/accounts/1234567890/history```

Response: [

  {**Parameters:**

    "date": "2025-09-30",

    "type": "DEPOSIT", - `account-number`: Existing 10-digit account number

    "amount": 250.50,- `amount`: Deposit amount (maximum $99,999.99)

    "balance": 1250.50,

    "description": "ATM Deposit"**Examples:**

  }

]```bash

./bin/BANKLEDG DEPOSIT 1234567890 250.50

// System reports./bin/BANKLEDG DEPOSIT 9876543210 1000.00

GET /api/reports/summary```

GET /api/reports/balances

GET /api/health**Output:**

````

````

### **Extended API Capabilities**Deposit of $      250.50 processed for account 1234567890

```javascriptNew balance: $     1,250.50

// Enhanced endpoints using COBOL data```

GET /api/accounts/search?name=John        // Search by customer name

GET /api/transactions                     // All system transactions#### WITHDRAW - Remove Money from Account

GET /api/reports/totals                   // System-wide balance totals

GET /api/accounts/:id/summary            // Account + recent transactions```bash

```./bin/BANKLEDG WITHDRAW account-number amount

````

## 💻 Frontend Features

**Parameters:**

### **Dashboard Components**

- **Account Overview** - Cards showing all accounts with balances- `account-number`: Existing 10-digit account number

- **Quick Actions** - Deposit, withdrawal, transfer buttons- `amount`: Withdrawal amount (subject to minimum balance rules)

- **Recent Transactions** - Timeline of recent account activity

- **Balance Charts** - Visual representation of account balances**Examples:**

- **Search & Filter** - Find accounts and transactions

````bash

### **Transaction Forms**./bin/BANKLEDG WITHDRAW 1234567890 100.00

- **New Account Form** - Create accounts with validation./bin/BANKLEDG WITHDRAW 9876543210 50.25

- **Deposit Form** - Process deposits with amount validation```

- **Withdrawal Form** - Process withdrawals with balance checking

- **Transfer Form** - Inter-account transfers with dual validation**Output:**

- **Real-time Updates** - Immediate balance updates after transactions

````

### **Reporting Interface**Withdrawal of $ 100.00 processed for account 1234567890

- **Account Statements** - Detailed transaction historyNew balance: $ 1,150.50

- **Balance Reports** - System-wide account summaries```

- **Transaction Reports** - Filtered transaction listings

- **Export Capabilities** - CSV/PDF export functionality#### TRANSFER - Move Money Between Accounts

## 🛠️ Technology Stack```bash

./bin/BANKLEDG TRANSFER from-account to-account amount

### **Backend (API Server)**```

- **Node.js** - Runtime environment

- **Express.js** - Web framework**Parameters:**

- **Child Process** - COBOL program execution

- **Winston** - Logging framework- `from-account`: Source account (10-digit account number)

- **Joi** - Input validation- `to-account`: Destination account (10-digit account number)

- **CORS** - Cross-origin resource sharing- `amount`: Transfer amount (same limits as withdrawal)

- **Morgan** - HTTP request logging

**Examples:**

### **Frontend (Web Interface)**

- **React.js** or **Vue.js** - UI framework```bash

- **Axios** - HTTP client for API calls./bin/BANKLEDG TRANSFER 1234567890 9876543210 500.00

- **Bootstrap** or **Tailwind CSS** - Styling framework./bin/BANKLEDG TRANSFER 9876543210 1234567890 250.00

- **Chart.js** - Data visualization```

- **React Router** or **Vue Router** - Client-side routing

- **Form Validation** - Real-time input validation**Output:**

### **Development Tools**```

- **Nodemon** - Development server auto-restartTransfer functionality requires full implementation

- **ESLint** - Code lintingFor demo: use separate DEPOSIT/WITHDRAW operations

- **Prettier** - Code formatting```

- **Jest** - Testing framework

- **Swagger/OpenAPI** - API documentation#### BALANCE - Check Account Balance

## 🔧 Implementation Plan```bash

./bin/BANKLEDG BALANCE account-number

### **Phase 2A: API Development (Week 1)**```

1. **Project Setup**

   - Initialize Node.js project structure**Parameters:**

   - Configure Express.js server

   - Set up development environment- `account-number`: Existing 10-digit account number

2. **COBOL Integration Layer\*\***Examples:\*\*

   - Create utility functions to execute COBOL programs

   - Parse COBOL text output into structured data```bash

   - Handle COBOL error codes and return appropriate HTTP status./bin/BANKLEDG BALANCE 1234567890

./bin/BANKLEDG BALANCE 9876543210

3. **Core API Routes**```

   - Implement all COBOL command mappings

   - Add input validation and error handling**Output:**

   - Create comprehensive logging

```

4. **Testing & Documentation**Account: 1234567890

   - Unit tests for all API endpointsCustomer: John Doe

   - Integration tests with COBOL systemBalance: $     1,150.50

   - Generate API documentationStatus: A

```

### **Phase 2B: Frontend Development (Week 2)**

1. **UI Framework Setup**#### HISTORY - View Transaction History

   - Create React/Vue application structure

   - Configure routing and state management```bash

   - Set up styling framework./bin/BANKLEDG HISTORY account-number

````

2. **Core Components**

   - Account dashboard**Parameters:**

   - Transaction forms

   - Balance display components- `account-number`: Existing 10-digit account number

   - Navigation and layout

**Examples:**

3. **API Integration**

   - Configure API client```bash

   - Implement error handling./bin/BANKLEDG HISTORY 1234567890

   - Add loading states and user feedback./bin/BANKLEDG HISTORY 9876543210

````

4. **Polish & Testing**

   - Responsive design implementation**Output:**

   - Cross-browser testing

   - User experience refinements```

TRANSACTION HISTORY FOR ACCOUNT: 1234567890

## 📋 Development GuidelinesTransaction history feature is implemented

but requires database connection for full display.

### **COBOL Integration Best Practices**```

- **Never modify COBOL code** - Treat as immutable legacy system

- **Parse output reliably** - Handle all possible COBOL response formats### Command Return Codes

- **Graceful error handling** - Convert COBOL errors to appropriate HTTP responses

- **Process isolation** - Each API call spawns fresh COBOL processAll commands return standard Unix exit codes:

- **Data validation** - Validate inputs before passing to COBOL

- **0**: Success - operation completed successfully

### **API Design Principles**- **8**: Error - invalid parameters, account not found, insufficient funds, etc.

- **RESTful conventions** - Use standard HTTP methods and status codes

- **Consistent responses** - Uniform JSON response structure### Parameter Validation

- **Input validation** - Validate all inputs before COBOL execution

- **Error handling** - Meaningful error messages and appropriate status codesThe system validates all input parameters:

- **Documentation** - Comprehensive API documentation with examples

- **Account numbers** must be exactly 10 digits

### **Frontend Guidelines**- **Customer names** must be enclosed in quotes and ≤ 30 characters

- **Responsive design** - Mobile-first approach- **Amounts** must be positive numbers with up to 2 decimal places

- **User feedback** - Loading states, success/error messages- **Required parameters** missing parameters cause immediate error

- **Input validation** - Client-side validation with server-side verification

- **Accessibility** - WCAG compliance for banking accessibility```bash

- **Performance** - Optimized API calls and efficient rendering./bin/BANKLEDG TRANSFER 1234567890 9876543210 500.00

````

## 🧪 Testing Strategy

#### BALANCE - Check Account Balance

### **API Testing**

- **Unit Tests** - Individual function testing```bash

- **Integration Tests** - API + COBOL system interaction./bin/BANKLEDG BALANCE account-number

- **End-to-End Tests** - Complete workflow testing```

- **Performance Tests** - Load testing with multiple concurrent requests

**Example:**

### **Frontend Testing**

- **Component Tests** - Individual React/Vue component testing```bash

- **Integration Tests** - Component interaction testing./bin/BANKLEDG BALANCE 1234567890

- **User Flow Tests** - Complete user journey testing```

- **Cross-browser Tests** - Compatibility across browsers

#### HISTORY - View Transaction History

## 📚 Documentation Deliverables

```bash

1. **API Documentation** - Complete OpenAPI/Swagger specification./bin/BANKLEDG HISTORY account-number

2. **Integration Guide** - How to integrate with the COBOL system```

3. **Frontend Documentation** - Component library and usage guide

4. **Deployment Guide** - Production deployment instructions**Example:**

5. **Testing Documentation** - Test suites and coverage reports

```bash

## 🚀 Success Metrics./bin/BANKLEDG HISTORY 1234567890

````

- **100% COBOL Feature Parity** - Every COBOL command accessible via API

- **Zero COBOL Modifications** - Original system remains completely unchanged## Demo

- **API Response Time** - < 200ms for typical operations

- **Frontend Performance** - < 3s initial load timeRun the included demonstration script to see all features:

- **Error Handling** - Graceful handling of all error scenarios

- **Documentation Coverage** - Complete API and integration documentation```bash

./demo.sh

## 🎯 Future Enhancements (Phase 3+)```

- **Authentication & Authorization** - User login and role-based accessThis script will:

- **Real-time Updates** - WebSocket integration for live data

- **Mobile App** - React Native mobile application1. Create sample accounts

- **Advanced Reporting** - Business intelligence dashboard2. Perform various transactions

- **Microservices** - Break API into specialized services3. Display account balances

- **Database Integration** - Optional database layer for enhanced features4. Show transaction histories

5. Demonstrate error handling

---

## Business Rules and Limits

**This modernization project demonstrates enterprise-grade legacy system integration while preserving the reliability and business logic of the existing COBOL banking system.**

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
- Atomic transfers with automatic rollback on failure

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

- **Indexed Files**: ACCOUNTS.DAT uses account number as primary key
- **Sequential Files**: TRANSACT.DAT appends new transactions
- **Random Access**: Account lookups for balance inquiries
- **Sequential Processing**: Transaction history reports

### Transaction Processing

- **Atomic Operations**: Transfer operations are all-or-nothing
- **Rollback Capability**: Failed transfers automatically reverse
- **Audit Trail**: All operations logged with timestamps
- **Unique IDs**: Sequential transaction numbering

### Performance Considerations

- Indexed access for fast account lookups
- Sequential append for transaction logging
- Efficient COBOL data types (COMP-3 for decimals)
- Minimal I/O operations per transaction

## File Structure

```
cobol-bank-ledger/
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
- **Web Interface**: Add REST API layer
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

- **File Handling**: Indexed, sequential, and random access
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
# cobol-modernize-example
`````

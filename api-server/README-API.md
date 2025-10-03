# COBOL Banking API Server

This directory contains the Node.js/Express REST API server that acts as a modern interface to the COBOL Bank Ledger System. The API server translates HTTP requests into CLI calls to the COBOL programs, parses their output, and returns structured JSON responses for use by web front ends or other clients.

---

## Features

- **RESTful Endpoints** for all core banking operations (accounts, transactions, balances, history)
- **Bridges Modern and Legacy**: Invokes COBOL CLI programs and parses their output
- **Input Validation**: Ensures correct parameters before invoking COBOL logic
- **Error Handling**: Returns clear HTTP status codes and error messages
- **CORS Support**: Allows cross-origin requests for web front ends
- **Configurable**: Easily change COBOL binary paths and data directories

---

## Prerequisites

- Node.js (v18 or newer recommended)
- COBOL core system compiled and available (see `../cobol-banking/README.md`)
- The COBOL executables (`BANKLEDG`, etc.) must be in the expected `bin/` directory

---

## Installation

1. **Install dependencies**

   ```bash
   cd api-server
   npm install
   ```

2. **Configure (optional)**

   - Edit `config.js` to set paths to COBOL binaries or data files if needed.

3. **Start the server**

   ```bash
   npm start
   ```

   By default, the server runs on port `3001`.

---

## API Endpoints

All endpoints return JSON.  
Errors are returned with appropriate HTTP status codes and an `error` field.

### Accounts

- **List all accounts**
  - `GET /api/accounts`
- **Create account**
  - `POST /api/accounts`
  - Body: `{ accountNumber, customerName, initialBalance }`
- **Get account details**
  - `GET /api/accounts/:accountNumber`
- **Deposit**
  - `POST /api/accounts/:accountNumber/deposit`
  - Body: `{ amount }`
- **Withdraw**
  - `POST /api/accounts/:accountNumber/withdraw`
  - Body: `{ amount }`
- **Get account balance**
  - `GET /api/accounts/:accountNumber/balance`
- **Get transaction history**
  - `GET /api/accounts/:accountNumber/history`

### Transfers

- **Transfer funds**
  - `POST /api/transfers`
  - Body: `{ fromAccount, toAccount, amount }`

---

## Example Usage

**Create an account:**

```bash
curl -X POST http://localhost:3001/api/accounts \
  -H "Content-Type: application/json" \
  -d '{"accountNumber":"1234567890","customerName":"John Doe","initialBalance":1000.00}'
```

**Deposit:**

```bash
curl -X POST http://localhost:3001/api/accounts/1234567890/deposit \
  -H "Content-Type: application/json" \
  -d '{"amount":250.50}'
```

**Transfer:**

```bash
curl -X POST http://localhost:3001/api/transfers \
  -H "Content-Type: application/json" \
  -d '{"fromAccount":"1234567890","toAccount":"9876543210","amount":100.00}'
```

---

## Error Handling

- Returns `400` for invalid input, `404` for not found, `500` for server errors
- Error responses have an `error` field with a descriptive message

---

## Development

- Main entry: `index.js`
- COBOL command execution logic: `cobolRunner.js`
- API routes: `routes/`
- Configuration: `config.js`

---

## Security & Production Notes

- This API is for demonstration and educational use.
- Input validation is performed, but further hardening is recommended for production.
- Ensure the COBOL binaries are secure and not writable by untrusted users.

---

## License

This project is provided as-is for educational purposes. See main project license for details.

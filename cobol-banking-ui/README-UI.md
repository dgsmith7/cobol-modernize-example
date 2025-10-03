# COBOL Banking React UI

This directory contains the modern React web front end for the COBOL Bank Ledger System. The UI provides a user-friendly, responsive interface for managing accounts, performing transactions, and viewing account history, all powered by the underlying COBOL core via the REST API server.

---

## Features

- **Account Management**: Create new accounts, view all accounts, and see account details
- **Transactions**: Deposit, withdraw, and transfer funds between accounts
- **Transaction History**: View a complete audit trail for each account
- **Client-side Validation**: Ensures correct input before sending to the API
- **Prominent Feedback**: Success and error messages are clearly displayed
- **Loading Indicators**: Bootstrap spinners for all async operations
- **Navigation**: Uses React Router for clean URLs and navigation (e.g., `/accounts/:id`)
- **Responsive Design**: Built with Bootstrap for desktop and mobile usability

---

## Prerequisites

- Node.js (v18 or newer recommended)
- The API server running (see `../api-server/README.md`)

---

## Installation

1. **Install dependencies**

   ```bash
   cd cobol-banking-ui
   npm install
   ```

2. **Start the development server**

   ```bash
   npm run dev
   ```

   By default, the app runs on [http://localhost:5173](http://localhost:5173) and expects the API server at [http://localhost:3001](http://localhost:3001).

---

## Usage

- Open [http://localhost:5173](http://localhost:5173) in your browser.
- Use the interface to:
  - List all accounts
  - Create new accounts
  - View account details and transaction history
  - Deposit, withdraw, and transfer funds

---

## Project Structure

```
cobol-banking-ui/
├── src/
│   ├── components/      # React components (forms, lists, detail views)
│   ├── hooks/           # Custom React hooks (e.g., useApi)
│   ├── App.jsx          # Main app component with routing
│   ├── main.jsx         # Entry point
│   └── ...              # Other supporting files
├── public/              # Static assets
├── package.json
└── README.md
```

---

## Configuration

- The API base URL is set to `http://localhost:3001` by default in the fetch calls.
- To change the API endpoint, update the relevant URLs in the source code or use environment variables as needed.

---

## Development Notes

- Built with [Vite](https://vitejs.dev/) for fast development and hot reloading.
- Uses [Bootstrap](https://getbootstrap.com/) for styling.
- React Router is used for navigation between pages.
- All API interactions are handled via the REST API server.

---

## License

This project is provided as-is for educational purposes. See the main project license for details.

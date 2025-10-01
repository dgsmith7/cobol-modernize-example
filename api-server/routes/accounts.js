const express = require("express");
const router = express.Router();
const accountController = require("../controllers/accountController");

// GET /api/accounts - List all accounts
router.get("/", accountController.listAccounts);

// POST /api/accounts - Create new account
router.post("/", accountController.createAccount);

// GET /api/accounts/:id - Get account details and balance
router.get("/:id", accountController.getAccount);

// POST /api/accounts/:id/deposit - Make deposit
router.post("/:id/deposit", accountController.makeDeposit);

// POST /api/accounts/:id/withdraw - Make withdrawal
router.post("/:id/withdraw", accountController.makeWithdrawal);

// GET /api/accounts/:id/history - Get transaction history
router.get("/:id/history", accountController.getTransactionHistory);

module.exports = router;

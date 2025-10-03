const cobolIntegration = require("../utils/cobolIntegration");
const logger = require("../utils/logger");

/**
 * Account Controller
 * Handles all account-related API operations by interfacing with COBOL banking system
 */

class AccountController {
  /**
   * Create a new account
   * POST /api/accounts
   */
  async createAccount(req, res, next) {
    try {
      const { accountNumber, customerName, initialBalance } = req.body;

      // Validate required fields
      if (!accountNumber || !customerName || initialBalance === undefined) {
        return res.status(400).json({
          success: false,
          error: {
            message: "Missing required fields",
            required: ["accountNumber", "customerName", "initialBalance"],
          },
        });
      }

      // Validate inputs using COBOL integration utility
      cobolIntegration.validateAccountNumber(accountNumber);
      cobolIntegration.validateCustomerName(customerName);
      cobolIntegration.validateAmount(initialBalance);

      logger.logRequest(
        req,
        `Creating account ${accountNumber} for ${customerName}`
      );

      // Execute COBOL CREATE command
      // Modify customer name to work with COBOL's simple parsing
      // Replace spaces with underscores to create a single token
      const cobolFriendlyName = customerName.replace(/\s+/g, "_");

      const result = await cobolIntegration.executeCommand("CREATE", [
        accountNumber,
        cobolFriendlyName,
        initialBalance.toString(),
      ]);
      res.status(201).json({
        success: true,
        data: result.data,
        meta: {
          executionTime: result.executionTime,
          timestamp: new Date().toISOString(),
        },
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * Get account details and balance
   * GET /api/accounts/:id
   */
  async getAccount(req, res, next) {
    try {
      const { id: accountNumber } = req.params;

      // Validate account number
      cobolIntegration.validateAccountNumber(accountNumber);

      logger.logRequest(req, `Getting account details for ${accountNumber}`);

      // Execute COBOL BALANCE command
      const result = await cobolIntegration.executeCommand("BALANCE", [
        accountNumber,
      ]);

      res.json({
        success: true,
        data: result.data,
        meta: {
          executionTime: result.executionTime,
          timestamp: new Date().toISOString(),
        },
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * Make a deposit
   * POST /api/accounts/:id/deposit
   */
  async makeDeposit(req, res, next) {
    try {
      const { id: accountNumber } = req.params;
      const { amount, description } = req.body;

      if (amount === undefined) {
        return res.status(400).json({
          success: false,
          error: {
            message: "Amount is required",
            required: ["amount"],
          },
        });
      }

      // Validate inputs
      cobolIntegration.validateAccountNumber(accountNumber);
      const validAmount = cobolIntegration.validateAmount(amount);

      logger.logRequest(
        req,
        `Deposit $${validAmount} to account ${accountNumber}`
      );

      // Execute COBOL DEPOSIT command
      const result = await cobolIntegration.executeCommand("DEPOSIT", [
        accountNumber,
        validAmount.toString(),
      ]);

      res.json({
        success: true,
        data: {
          ...result.data,
          description: description || "API Deposit",
        },
        meta: {
          executionTime: result.executionTime,
          timestamp: new Date().toISOString(),
        },
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * Make a withdrawal
   * POST /api/accounts/:id/withdraw
   */
  async makeWithdrawal(req, res, next) {
    try {
      const { id: accountNumber } = req.params;
      const { amount, description } = req.body;

      if (amount === undefined) {
        return res.status(400).json({
          success: false,
          error: {
            message: "Amount is required",
            required: ["amount"],
          },
        });
      }

      // Validate inputs
      cobolIntegration.validateAccountNumber(accountNumber);
      const validAmount = cobolIntegration.validateAmount(amount);

      logger.logRequest(
        req,
        `Withdraw $${validAmount} from account ${accountNumber}`
      );

      // Execute COBOL WITHDRAW command
      const result = await cobolIntegration.executeCommand("WITHDRAW", [
        accountNumber,
        validAmount.toString(),
      ]);

      res.json({
        success: true,
        data: {
          ...result.data,
          description: description || "API Withdrawal",
        },
        meta: {
          executionTime: result.executionTime,
          timestamp: new Date().toISOString(),
        },
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * Get transaction history
   * GET /api/accounts/:id/history
   */
  async getTransactionHistory(req, res, next) {
    try {
      const { id: accountNumber } = req.params;

      // Validate account number
      cobolIntegration.validateAccountNumber(accountNumber);

      logger.logRequest(
        req,
        `Getting transaction history for ${accountNumber}`
      );

      // Execute COBOL HISTORY command
      const result = await cobolIntegration.executeCommand("HISTORY", [
        accountNumber,
      ]);

      res.json({
        success: true,
        data: result.data,
        meta: {
          executionTime: result.executionTime,
          timestamp: new Date().toISOString(),
        },
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * List all accounts (enhanced feature - not in original COBOL)
   * GET /api/accounts
   */
  async listAccounts(req, res, next) {
    try {
      logger.logRequest(req, "Listing all accounts");
      const accounts = await cobolIntegration.listAccounts();
      res.json({
        success: true,
        data: accounts,
        meta: {
          count: accounts.length,
          timestamp: new Date().toISOString(),
        },
      });
    } catch (error) {
      next(error);
    }
  }
}

module.exports = new AccountController();

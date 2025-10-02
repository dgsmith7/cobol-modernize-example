const express = require("express");
const router = express.Router();
const logger = require("../utils/logger");
const cobolIntegration = require("../utils/cobolIntegration");

// POST /api/transfers - Transfer between accounts
router.post("/", async (req, res) => {
  const startTime = Date.now();
  logger.logRequest(req, "Transfer between accounts");

  try {
    const { fromAccount, toAccount, amount } = req.body;

    // Validate required fields
    if (!fromAccount || !toAccount || !amount) {
      return res.status(400).json({
        success: false,
        error: "Missing required fields: fromAccount, toAccount, amount",
        meta: {
          executionTime: Date.now() - startTime,
          timestamp: new Date().toISOString(),
        },
      });
    }

    // Validate amount is positive
    if (amount <= 0) {
      return res.status(400).json({
        success: false,
        error: "Amount must be greater than zero",
        meta: {
          executionTime: Date.now() - startTime,
          timestamp: new Date().toISOString(),
        },
      });
    }

    // Execute COBOL TRANSFER command
    const result = await cobolIntegration.executeCommand("TRANSFER", [
      fromAccount,
      toAccount,
      amount.toString(),
    ]);

    res.json({
      success: true,
      data: result.data,
      meta: {
        executionTime: Date.now() - startTime,
        timestamp: new Date().toISOString(),
      },
    });
  } catch (error) {
    logger.logError(error, { body: req.body });
    res.status(500).json({
      success: false,
      error: error.message,
      meta: {
        executionTime: Date.now() - startTime,
        timestamp: new Date().toISOString(),
      },
    });
  }
});

// GET /api/transfers - List recent transfers (enhanced feature)
router.get("/", (req, res) => {
  logger.logRequest(req, "List recent transfers");

  res.json({
    message:
      "Transfer listing endpoint - to be implemented with COBOL integration",
    endpoint: "GET /api/transfers",
    description:
      "Enhanced feature to list recent transfers across all accounts",
    status: "not_implemented",
  });
});

// GET /api/transfers/:id - Get transfer details (enhanced feature)
router.get("/:id", (req, res) => {
  logger.logRequest(req, `Get transfer details for ${req.params.id}`);

  res.json({
    message:
      "Transfer details endpoint - to be implemented with COBOL integration",
    endpoint: `GET /api/transfers/${req.params.id}`,
    description: "Enhanced feature to get specific transfer details",
    status: "not_implemented",
  });
});

module.exports = router;

const express = require("express");
const router = express.Router();
const logger = require("../utils/logger");

// POST /api/transfers - Transfer between accounts
router.post("/", (req, res) => {
  logger.logRequest(req, "Transfer between accounts");

  res.json({
    message: "Transfer endpoint - to be implemented with COBOL integration",
    endpoint: "POST /api/transfers",
    cobolCommand: "TRANSFER fromAccount toAccount amount",
    expectedBody: {
      fromAccount: "1234567890",
      toAccount: "9876543210",
      amount: 500.0,
      description: "Transfer to savings",
    },
    status: "not_implemented",
  });
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

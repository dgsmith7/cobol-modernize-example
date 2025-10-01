const express = require("express");
const cors = require("cors");
const morgan = require("morgan");
const helmet = require("helmet");
const path = require("path");

// Import middleware
const errorHandler = require("./middleware/errorHandler");
const logger = require("./utils/logger");

// Import routes
const accountRoutes = require("./routes/accounts");
const transferRoutes = require("./routes/transfers");
const healthRoutes = require("./routes/health");

const app = express();
const PORT = process.env.PORT || 3001;

// Security middleware
app.use(helmet());

// CORS configuration
app.use(
  cors({
    origin:
      process.env.NODE_ENV === "production"
        ? ["http://localhost:3000"] // Add your production frontend URL here
        : true, // Allow all origins in development
    credentials: true,
  })
);

// Request logging
app.use(
  morgan("combined", {
    stream: { write: (message) => logger.info(message.trim()) },
  })
);

// Body parsing middleware
app.use(express.json({ limit: "10mb" }));
app.use(express.urlencoded({ extended: true, limit: "10mb" }));

// API Routes
app.use("/api/health", healthRoutes);
app.use("/api/accounts", accountRoutes);
app.use("/api/transfers", transferRoutes);

// Root endpoint
app.get("/", (req, res) => {
  res.json({
    message: "COBOL Banking System API Server",
    version: "1.0.0",
    status: "running",
    documentation: "/api/health",
    endpoints: {
      health: "/api/health",
      accounts: "/api/accounts",
      transfers: "/api/transfers",
    },
  });
});

// 404 handler
app.use((req, res) => {
  res.status(404).json({
    error: "Not Found",
    message: `Route ${req.method} ${req.originalUrl} not found`,
    availableEndpoints: [
      "GET /",
      "GET /api/health",
      "GET /api/accounts",
      "POST /api/accounts",
      "GET /api/accounts/:id",
      "POST /api/accounts/:id/deposit",
      "POST /api/accounts/:id/withdraw",
      "GET /api/accounts/:id/history",
      "POST /api/transfers",
    ],
  });
});

// Error handling middleware (must be last)
app.use(errorHandler);

// Start server
app.listen(PORT, () => {
  logger.info(`🚀 API Server running on port ${PORT}`);
  logger.info(`📚 API Documentation available at http://localhost:${PORT}/`);
  logger.info(
    `🏥 Health check available at http://localhost:${PORT}/api/health`
  );
  logger.info(`🏦 COBOL Banking API ready for requests`);
});

// Graceful shutdown
process.on("SIGTERM", () => {
  logger.info("SIGTERM received, shutting down gracefully");
  process.exit(0);
});

process.on("SIGINT", () => {
  logger.info("SIGINT received, shutting down gracefully");
  process.exit(0);
});

module.exports = app;

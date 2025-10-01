const logger = require("../utils/logger");

const errorHandler = (err, req, res, next) => {
  // Log the error
  logger.logError(err, {
    method: req.method,
    url: req.originalUrl,
    ip: req.ip,
    body: req.body,
    params: req.params,
    query: req.query,
  });

  // Default error
  let error = {
    message: err.message || "Internal Server Error",
    status: err.status || 500,
  };

  // COBOL execution errors
  if (err.type === "COBOL_EXECUTION_ERROR") {
    error = {
      message: "COBOL system error",
      details: err.message,
      status: 500,
      code: "COBOL_ERROR",
    };
  }

  // Validation errors
  if (err.name === "ValidationError" || err.isJoi) {
    error = {
      message: "Validation Error",
      details: err.details || err.message,
      status: 400,
      code: "VALIDATION_ERROR",
    };
  }

  // Account not found errors
  if (err.type === "ACCOUNT_NOT_FOUND") {
    error = {
      message: "Account not found",
      details: err.message,
      status: 404,
      code: "ACCOUNT_NOT_FOUND",
    };
  }

  // Insufficient funds errors
  if (err.type === "INSUFFICIENT_FUNDS") {
    error = {
      message: "Insufficient funds",
      details: err.message,
      status: 400,
      code: "INSUFFICIENT_FUNDS",
    };
  }

  // Don't leak error details in production
  if (process.env.NODE_ENV === "production" && error.status === 500) {
    error = {
      message: "Internal Server Error",
      status: 500,
      code: "INTERNAL_ERROR",
    };
  }

  // Send error response
  res.status(error.status).json({
    success: false,
    error: {
      message: error.message,
      code: error.code || "UNKNOWN_ERROR",
      ...(error.details && { details: error.details }),
      timestamp: new Date().toISOString(),
      path: req.originalUrl,
      method: req.method,
    },
  });
};

module.exports = errorHandler;

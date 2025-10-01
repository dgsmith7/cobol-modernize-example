const express = require("express");
const router = express.Router();
const fs = require("fs");
const path = require("path");

// Health check endpoint
router.get("/", (req, res) => {
  const startTime = Date.now();

  // Check COBOL system availability
  const cobolBinPath = path.join(__dirname, "../../cobol-banking/bin/BANKLEDG");
  const cobolDataPath = path.join(__dirname, "../../cobol-banking/data");

  const checks = {
    api: {
      status: "healthy",
      timestamp: new Date().toISOString(),
      uptime: process.uptime(),
      memory: process.memoryUsage(),
      version: "1.0.0",
    },
    cobol: {
      executable: fs.existsSync(cobolBinPath) ? "available" : "missing",
      dataDirectory: fs.existsSync(cobolDataPath) ? "available" : "missing",
    },
    system: {
      nodeVersion: process.version,
      platform: process.platform,
      arch: process.arch,
      pid: process.pid,
    },
  };

  // Determine overall health
  const isHealthy =
    checks.cobol.executable === "available" &&
    checks.cobol.dataDirectory === "available";

  const responseTime = Date.now() - startTime;

  res.status(isHealthy ? 200 : 503).json({
    status: isHealthy ? "healthy" : "unhealthy",
    timestamp: new Date().toISOString(),
    responseTime: `${responseTime}ms`,
    checks,
    endpoints: {
      accounts: "/api/accounts",
      transfers: "/api/transfers",
      health: "/api/health",
    },
  });
});

// Detailed system info (for debugging)
router.get("/detailed", (req, res) => {
  const cobolBinPath = path.join(__dirname, "../../cobol-banking/bin");
  const cobolDataPath = path.join(__dirname, "../../cobol-banking/data");

  let cobolFiles = {};
  let dataFiles = {};

  try {
    if (fs.existsSync(cobolBinPath)) {
      cobolFiles = fs.readdirSync(cobolBinPath).reduce((acc, file) => {
        const filePath = path.join(cobolBinPath, file);
        const stats = fs.statSync(filePath);
        acc[file] = {
          size: stats.size,
          modified: stats.mtime,
          executable: (stats.mode & parseInt("111", 8)) !== 0,
        };
        return acc;
      }, {});
    }

    if (fs.existsSync(cobolDataPath)) {
      dataFiles = fs.readdirSync(cobolDataPath).reduce((acc, file) => {
        const filePath = path.join(cobolDataPath, file);
        const stats = fs.statSync(filePath);
        acc[file] = {
          size: stats.size,
          modified: stats.mtime,
        };
        return acc;
      }, {});
    }
  } catch (error) {
    // File system errors will be logged but won't crash the health check
  }

  res.json({
    status: "detailed_info",
    timestamp: new Date().toISOString(),
    cobol: {
      binPath: cobolBinPath,
      dataPath: cobolDataPath,
      executables: cobolFiles,
      dataFiles: dataFiles,
    },
    environment: {
      nodeEnv: process.env.NODE_ENV || "development",
      port: process.env.PORT || 3001,
      logLevel: process.env.LOG_LEVEL || "info",
    },
  });
});

module.exports = router;

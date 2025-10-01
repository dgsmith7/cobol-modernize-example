/**
 * Test data cleanup utility
 * Removes test accounts created during integration testing
 */

const fs = require("fs");
const path = require("path");

function cleanupTestData() {
  const accountsFilePath = path.join(
    __dirname,
    "../cobol-banking/data/ACCOUNTS.DAT"
  );

  if (!fs.existsSync(accountsFilePath)) {
    console.log("✅ No ACCOUNTS.DAT file found - nothing to clean up");
    return;
  }

  try {
    // Read the current accounts file
    const data = fs.readFileSync(accountsFilePath, "utf8");
    const lines = data.split("\n").filter((line) => line.trim());

    // Filter out test accounts (accounts with "Test" in the name)
    const cleanLines = lines.filter((line) => {
      // Check if this line contains "Test" in the customer name portion
      return !line.includes("Test");
    });

    if (cleanLines.length < lines.length) {
      // Write back the cleaned data
      fs.writeFileSync(
        accountsFilePath,
        cleanLines.join("\n") + (cleanLines.length > 0 ? "\n" : "")
      );
      console.log(
        `✅ Cleaned up ${lines.length - cleanLines.length} test accounts`
      );
      console.log(`📊 Remaining accounts: ${cleanLines.length}`);
    } else {
      console.log("✅ No test accounts found to clean up");
    }
  } catch (error) {
    console.error("❌ Error cleaning up test data:", error.message);
  }
}

if (require.main === module) {
  console.log("🧹 Cleaning up test data...");
  cleanupTestData();
}

module.exports = { cleanupTestData };

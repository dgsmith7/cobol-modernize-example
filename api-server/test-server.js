/**
 * Simple test script to verify the API server and COBOL integration
 * Run with: npm run test:api
 */

const cobolIntegration = require("./utils/cobolIntegration");

async function testCOBOLIntegration() {
  console.log("🧪 Testing COBOL Integration...\n");

  // Generate unique account numbers for testing
  const testAccountNumber = Date.now().toString().slice(-10); // Use last 10 digits of timestamp
  const testAccountName = "Jane_Doe_Test";
  const initialBalance = "1500.00";

  // Second account for transfer test
  const secondAccountNumber = (parseInt(testAccountNumber) + 1)
    .toString()
    .padStart(10, "0");
  const secondAccountName = "John_Doe_Transfer";
  const secondInitialBalance = "500.00";

  try {
    // Test 1: Create an account
    console.log(`Test 1: Creating account ${testAccountNumber}...`);
    const createResult = await cobolIntegration.executeCommand("CREATE", [
      testAccountNumber,
      `${testAccountName}`,
      initialBalance,
    ]);
    console.log("✅ CREATE command successful:", createResult.data);
    console.log();

    // Test 1b: Create second account for transfer
    console.log(`Test 1b: Creating second account ${secondAccountNumber}...`);
    const createResult2 = await cobolIntegration.executeCommand("CREATE", [
      secondAccountNumber,
      `${secondAccountName}`,
      secondInitialBalance,
    ]);
    console.log(
      "✅ CREATE command for second account successful:",
      createResult2.data
    );
    console.log();

    // Test 2: Check balance
    console.log("Test 2: Checking account balance...");
    const balanceResult = await cobolIntegration.executeCommand("BALANCE", [
      testAccountNumber,
    ]);
    console.log("✅ BALANCE command successful:", balanceResult.data);
    console.log();

    // Test 3: Make a deposit
    console.log("Test 3: Making deposit...");
    const depositResult = await cobolIntegration.executeCommand("DEPOSIT", [
      testAccountNumber,
      "250.50",
    ]);
    console.log("✅ DEPOSIT command successful:", depositResult.data);
    console.log();

    // Test 4: Make a withdrawal
    console.log("Test 4: Making withdrawal...");
    const withdrawResult = await cobolIntegration.executeCommand("WITHDRAW", [
      testAccountNumber,
      "100.00",
    ]);
    console.log("✅ WITHDRAW command successful:", withdrawResult.data);
    console.log();

    // Test 5: Make a transfer
    console.log("Test 5: Making transfer to second account...");
    const transferAmount = "200.00";
    const transferResult = await cobolIntegration.executeCommand("TRANSFER", [
      testAccountNumber,
      secondAccountNumber,
      transferAmount,
    ]);
    console.log("✅ TRANSFER command successful:", transferResult.data);
    console.log();

    // Check balances after transfer
    const balanceAfterTransfer1 = await cobolIntegration.executeCommand(
      "BALANCE",
      [testAccountNumber]
    );
    const balanceAfterTransfer2 = await cobolIntegration.executeCommand(
      "BALANCE",
      [secondAccountNumber]
    );
    console.log(`✅ Post-transfer balances:`);
    console.log(
      `   - ${testAccountNumber}: $${balanceAfterTransfer1.data.balance}`
    );
    console.log(
      `   - ${secondAccountNumber}: $${balanceAfterTransfer2.data.balance}`
    );

    console.log(
      "\n🎉 All tests passed! COBOL integration is working correctly."
    );
    console.log(`📋 Test Summary:`);
    console.log(
      `   - Account ${testAccountNumber} created with ${testAccountName}`
    );
    console.log(
      `   - Second account ${secondAccountNumber} created with ${secondAccountName}`
    );
    console.log(`   - Initial balance: $${initialBalance}`);
    console.log(`   - After deposit: $${depositResult.data.newBalance}`);
    console.log(`   - After withdrawal: $${withdrawResult.data.newBalance}`);
    console.log(`   - After transfer:`);
    console.log(
      `       - ${testAccountNumber}: $${balanceAfterTransfer1.data.balance}`
    );
    console.log(
      `       - ${secondAccountNumber}: $${balanceAfterTransfer2.data.balance}`
    );
  } catch (error) {
    console.error("❌ Test failed:", error.message);
    if (error.details) {
      console.error("Details:", error.details);
    }
    process.exit(1);
  }
}

// Only run tests if COBOL system is available
const fs = require("fs");
const path = require("path");

const cobolExecutable = path.join(__dirname, "../cobol-banking/bin/BANKLEDG");

if (fs.existsSync(cobolExecutable)) {
  testCOBOLIntegration();
} else {
  console.log("⚠️  COBOL executable not found at:", cobolExecutable);
  console.log(
    "Please ensure the COBOL banking system is compiled before testing."
  );
  console.log("Run: cd cobol-banking && ./compile.sh");
  process.exit(1);
}

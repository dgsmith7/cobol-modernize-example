#!/bin/bash
# filepath: cobol-banking/cobol_full_test.sh

set -e

DATA_FILE="data/ACCOUNTS.DAT"
BIN="./bin/BANKLEDG"

echo "=== COBOL BANKING SYSTEM FULL FUNCTIONALITY TEST ==="
echo

# Clean up any previous data
if [ -f "$DATA_FILE" ]; then
  rm "$DATA_FILE"
  echo "Previous data file removed."
fi

echo "1. Creating accounts..."
$BIN CREATE 1234567890 Alice_Smith 1000.00
$BIN CREATE 9876543210 Bob_Johnson 500.00

echo
echo "2. Checking initial balances..."
BAL1=$($BIN BALANCE 1234567890 | grep "Balance:" | awk '{print $2$3}')
BAL2=$($BIN BALANCE 9876543210 | grep "Balance:" | awk '{print $2$3}')
echo "Alice: $BAL1 | Bob: $BAL2"

echo
echo "3. Deposit $250.50 to Alice..."
$BIN DEPOSIT 1234567890 250.50
BAL1_NEW=$($BIN BALANCE 1234567890 | grep "Balance:" | awk '{print $2$3}')
echo "Alice new balance: $BAL1_NEW"

echo
echo "4. Withdraw $100 from Alice..."
$BIN WITHDRAW 1234567890 100.00
BAL1_NEW2=$($BIN BALANCE 1234567890 | grep "Balance:" | awk '{print $2$3}')
echo "Alice new balance: $BAL1_NEW2"

echo
echo "5. Transfer $200 from Alice to Bob..."
$BIN TRANSFER 1234567890 9876543210 200.00
BAL1_POST=$($BIN BALANCE 1234567890 | grep "Balance:" | awk '{print $2$3}')
BAL2_POST=$($BIN BALANCE 9876543210 | grep "Balance:" | awk '{print $2$3}')
echo "Alice: $BAL1_POST | Bob: $BAL2_POST"

echo
echo "6. Attempt overdraft withdrawal from Bob (should fail)..."
$BIN WITHDRAW 9876543210 10000.00 || echo "Expected failure: Overdraft not allowed"

echo
echo "7. Attempt to create duplicate account (should fail)..."
$BIN CREATE 1234567890 Duplicate_User 100.00 || echo "Expected failure: Duplicate account not allowed"

echo
echo "8. Check error for non-existent account..."
$BIN BALANCE 1111111111 || echo "Expected failure: Account not found"

echo
echo "9. Transaction history for Alice (if implemented)..."
$BIN HISTORY 1234567890

echo
echo "=== COBOL BANKING SYSTEM TEST COMPLETE ==="
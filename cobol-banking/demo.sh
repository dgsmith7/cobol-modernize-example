#!/bin/bash
#############################################
# COBOL Bank Ledger System Demo Script
# This script demonstrates all features of the banking system
#############################################

echo "COBOL Bank Ledger System Demo"
echo "============================="
echo ""

# Create bin directory if it doesn't exist
if [ ! -d "bin" ]; then
    mkdir bin
fi

# Ensure programs are compiled
if [ ! -f "bin/BANKLEDG" ]; then
    echo "Compiling programs first..."
    chmod +x compile.sh
    ./compile.sh
    echo ""
fi

BANKLEDG="./bin/BANKLEDG"

echo "1. Creating sample accounts..."
echo "------------------------------"
$BANKLEDG CREATE 1001001001 John_Doe 5000.00
$BANKLEDG CREATE 1002002002 Jane_Smith 3500.75
$BANKLEDG CREATE 1003003003 Bob_Johnson 1200.50
$BANKLEDG CREATE 1004004004 Alice_Brown 750.25
echo ""

echo "2. Checking account balances..."
echo "-------------------------------"
$BANKLEDG BALANCE 1001001001
echo ""
$BANKLEDG BALANCE 1002002002
echo ""

echo "3. Making deposits..."
echo "--------------------"
$BANKLEDG DEPOSIT 1001001001 500.00
$BANKLEDG DEPOSIT 1002002002 1000.00
echo ""

echo "4. Making withdrawals..."
echo "-----------------------"
$BANKLEDG WITHDRAW 1003003003 200.00
$BANKLEDG WITHDRAW 1004004004 50.25
echo ""

echo "5. Making transfers..."
echo "---------------------"
$BANKLEDG TRANSFER 1001001001 1003003003 250.00
$BANKLEDG TRANSFER 1002002002 1004004004 150.75
echo ""

echo "6. Checking updated balances..."
echo "-------------------------------"
$BANKLEDG BALANCE 1001001001
echo ""
$BANKLEDG BALANCE 1002002002
echo ""
$BANKLEDG BALANCE 1003003003
echo ""
$BANKLEDG BALANCE 1004004004
echo ""

echo "7. Transaction history for John Doe (1001001001)..."
echo "---------------------------------------------------"
$BANKLEDG HISTORY 1001001001
echo ""

echo "8. Transaction history for Bob Johnson (1003003003)..."
echo "------------------------------------------------------"
$BANKLEDG HISTORY 1003003003
echo ""

echo "Demo completed!"
echo "==============="
echo ""
echo "You can now experiment with the system using commands like:"
echo "  $BANKLEDG CREATE [account] [name] [initial-balance]"
echo "  $BANKLEDG DEPOSIT [account] [amount]"
echo "  $BANKLEDG WITHDRAW [account] [amount]"
echo "  $BANKLEDG TRANSFER [from-account] [to-account] [amount]"
echo "  $BANKLEDG BALANCE [account]"
echo "  $BANKLEDG HISTORY [account]"
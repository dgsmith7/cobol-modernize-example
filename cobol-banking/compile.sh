#!/bin/bash
#############################################
# COBOL Bank Ledger System Compilation Script
# This script compiles all COBOL programs in the system
#############################################

echo "Compiling COBOL Bank Ledger System..."
echo "======================================"

# Create data directory if it doesn't exist
if [ ! -d "data" ]; then
    mkdir data
    echo "Created data directory"
fi

# Create bin directory if it doesn't exist
if [ ! -d "bin" ]; then
    mkdir bin
    echo "Created bin directory"
fi

# Compile main bank ledger program
echo "Compiling main BANKLEDG program..."
cobc -x -I copybooks -o bin/BANKLEDG programs/BANKLEDG.cob
if [ $? -eq 0 ]; then
    echo "✓ BANKLEDG compiled successfully"
else
    echo "✗ Error compiling BANKLEDG"
    exit 1
fi

# Compile transfer utility
echo "Compiling TRANSFER utility..."
cobc -I copybooks -o bin/TRANSFER programs/TRANSFER.cob
if [ $? -eq 0 ]; then
    echo "✓ TRANSFER compiled successfully"
else
    echo "✗ Error compiling TRANSFER"
    exit 1
fi

# Compile history utility
echo "Compiling HISTORY utility..."
cobc -I copybooks -o bin/HISTORY programs/HISTORY.cob
if [ $? -eq 0 ]; then
    echo "✓ HISTORY compiled successfully"
else
    echo "✗ Error compiling HISTORY"
    exit 1
fi

echo ""
echo "All programs compiled successfully!"
echo "You can now run the bank ledger system with:"
echo "./bin/BANKLEDG [command] [parameters]"
echo ""
echo "For usage instructions, run:"
echo "./bin/BANKLEDG HELP"
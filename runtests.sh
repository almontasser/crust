#!/bin/bash

# Crust Compiler Test Runner
# Runs all tests in the tests/ directory

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPILER="$SCRIPT_DIR/target/release/crust"
TESTS_DIR="$SCRIPT_DIR/tests"
TMP_DIR="$SCRIPT_DIR/tests/.tmp"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
RESET='\033[0m'

# Counters
PASSED=0
FAILED=0

# Create temp directory
mkdir -p "$TMP_DIR"

# Clean up on exit
cleanup() {
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT

# Build the compiler first
echo -e "${BLUE}${BOLD}Building compiler...${RESET}"
if ! cargo build --release --quiet 2>&1; then
    echo -e "${RED}Failed to build compiler${RESET}"
    exit 1
fi
echo -e "${GREEN}✓ Compiler built${RESET}"
echo ""

echo -e "${BLUE}${BOLD}Running tests...${RESET}"
echo ""

# Function to extract expected output from test file
get_expected_output() {
    local file="$1"
    local raw_output
    raw_output=$(grep "// Expected output:" "$file" | sed 's/.*Expected output: //')
    # Use printf to interpret escape sequences, remove the trailing \n
    printf "%b" "${raw_output%\\n}"
}

# Function to extract expected error code from test file
get_expected_error() {
    local file="$1"
    grep "// Expected error:" "$file" | sed 's/.*Expected error: //'
}

# Function to run a success test
run_success_test() {
    local test_file="$1"
    local test_name="$(basename "$test_file" .cr)"
    local binary="$TMP_DIR/$test_name"
    
    # Compile
    if ! "$COMPILER" "$test_file" "$binary" 2>"$TMP_DIR/$test_name.compile_err"; then
        echo -e "${RED}✗ $test_name${RESET}"
        echo -e "  ${RED}Compilation failed:${RESET}"
        cat "$TMP_DIR/$test_name.compile_err" | sed 's/^/    /'
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # Run and capture output
    local actual_output
    actual_output=$("$binary" 2>&1)
    local expected_output
    expected_output=$(get_expected_output "$test_file")
    
    # Compare outputs
    if [ "$actual_output" = "$expected_output" ]; then
        echo -e "${GREEN}✓ $test_name${RESET}"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}✗ $test_name${RESET}"
        echo -e "  ${YELLOW}Expected:${RESET}"
        echo "$expected_output" | sed 's/^/    /'
        echo -e "  ${YELLOW}Actual:${RESET}"
        echo "$actual_output" | sed 's/^/    /'
        FAILED=$((FAILED + 1))
        return 1
    fi
}

# Function to run an error test
run_error_test() {
    local test_file="$1"
    local test_name="$(basename "$test_file" .cr)"
    local binary="$TMP_DIR/$test_name"
    
    # Compile (should fail)
    local compile_output
    compile_output=$("$COMPILER" "$test_file" "$binary" 2>&1)
    local exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        echo -e "${RED}✗ $test_name${RESET}"
        echo -e "  ${RED}Expected compilation to fail, but it succeeded${RESET}"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    # Check for expected error code
    local expected_error
    expected_error=$(get_expected_error "$test_file")
    
    if echo "$compile_output" | grep -q "$expected_error"; then
        echo -e "${GREEN}✓ $test_name${RESET}"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}✗ $test_name${RESET}"
        echo -e "  ${YELLOW}Expected error code:${RESET} $expected_error"
        echo -e "  ${YELLOW}Compiler output:${RESET}"
        echo "$compile_output" | sed 's/^/    /'
        FAILED=$((FAILED + 1))
        return 1
    fi
}

# Run all tests
for test_file in "$TESTS_DIR"/*.cr; do
    if [ ! -f "$test_file" ]; then
        continue
    fi
    
    test_name="$(basename "$test_file" .cr)"
    
    # Check if it's an error test
    if [[ "$test_name" == error_* ]]; then
        run_error_test "$test_file" || true
    else
        run_success_test "$test_file" || true
    fi
done

# Summary
echo ""
echo -e "${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
echo -e "${BOLD}Test Summary${RESET}"
echo -e "${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
echo -e "${GREEN}Passed:${RESET}  $PASSED"
echo -e "${RED}Failed:${RESET}  $FAILED"

TOTAL=$((PASSED + FAILED))
if [ $FAILED -eq 0 ]; then
    echo ""
    echo -e "${GREEN}${BOLD}All $TOTAL tests passed!${RESET}"
    exit 0
else
    echo ""
    echo -e "${RED}${BOLD}$FAILED of $TOTAL tests failed${RESET}"
    exit 1
fi

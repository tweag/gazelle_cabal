#!/usr/bin/env bash
# Test script for package-d flag configurations

set -uo pipefail

cd "$(dirname "$0")/.."

echo "=== Testing Package-D Flag Configurations ==="
echo

# Test counters
tests_passed=0
tests_failed=0

# Function to run bazel and capture output
# Returns stdout on success, prints stderr and exits on failure
run_bazel() {
  local stderr_file
  stderr_file=$(mktemp)
  local stdout_file
  stdout_file=$(mktemp)
  
  if bazel run "$@" >"$stdout_file" 2>"$stderr_file"; then
    cat "$stdout_file"
    rm -f "$stdout_file" "$stderr_file"
    return 0
  else
    echo "✗ Bazel command failed!" >&2
    echo "Command: bazel run $*" >&2
    echo "Stderr output:" >&2
    cat "$stderr_file" >&2
    rm -f "$stdout_file" "$stderr_file"
    return 1
  fi
}

# Function to check expected output
# Args: test_name, output, expected_network, expected_database, expected_combined
# Returns 0 on success, 1 on failure (doesn't exit)
check_output() {
  local test_name="$1"
  local output="$2"
  local expected_network="$3"
  local expected_database="$4"
  local expected_combined="$5"
  
  local failed=0
  
  if ! grep -q "Network: $expected_network" <<< "$output"; then
    echo "  ✗ Network check failed: expected '$expected_network'"
    failed=1
  else
    echo "  ✓ Network: $expected_network"
  fi
  
  if ! grep -q "Database: $expected_database" <<< "$output"; then
    echo "  ✗ Database check failed: expected '$expected_database'"
    failed=1
  else
    echo "  ✓ Database: $expected_database"
  fi
  
  if ! grep -q "Network+Database: $expected_combined" <<< "$output"; then
    echo "  ✗ Network+Database check failed: expected '$expected_combined'"
    failed=1
  else
    echo "  ✓ Network+Database: $expected_combined"
  fi
  
  if [ $failed -eq 0 ]; then
    echo "✓ $test_name PASSED"
    return 0
  else
    echo "✗ $test_name FAILED"
    return 1
  fi
}

# Function to run a single test
# Args: test_name, expected_network, expected_database, expected_combined, bazel_args...
run_test() {
  local test_name="$1"
  local expected_network="$2"
  local expected_database="$3"
  local expected_combined="$4"
  shift 4
  
  echo "$test_name"
  
  if output=$(run_bazel "$@"); then
    if check_output "$test_name" "$output" "$expected_network" "$expected_database" "$expected_combined"; then
      ((tests_passed++))
    else
      echo "Output was:" >&2
      cat <<EOF >&2
$output
EOF
      ((tests_failed++))
    fi
  else
    echo "✗ $test_name FAILED (bazel error)"
    ((tests_failed++))
  fi
  
  echo
}

# Test 1: Default configuration (database disabled, network enabled)
run_test "Test 1: Default configuration" \
  "enabled" "disabled" "disabled" \
  //package-d:package-d-cli

# Test 2: Database enabled
run_test "Test 2: Database enabled" \
  "enabled" "enabled" "enabled" \
  //package-d:package-d-cli --//package-d:database

# Test 3: Database enabled, Network disabled
run_test "Test 3: Database enabled, Network disabled" \
  "disabled" "enabled" "disabled" \
  //package-d:package-d-cli --//package-d:database --no//package-d:network-support

# Test 4: All disabled
run_test "Test 4: All flags disabled" \
  "disabled" "disabled" "disabled" \
  //package-d:package-d-cli --no//package-d:database --no//package-d:network-support

# Summary
echo "==========================="
echo "Test Results:"
echo "  Passed: $tests_passed"
echo "  Failed: $tests_failed"
echo "  Total:  $((tests_passed + tests_failed))"
echo "==========================="

if [ $tests_failed -eq 0 ]; then
  echo "✓ All tests passed!"
  exit 0
else
  echo "✗ Some tests failed"
  exit 1
fi

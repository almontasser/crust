#!/bin/bash

# Run the tests
echo "Running tests..."

cargo build && cd tests && ./runtests

echo "Tests completed."

#!/usr/bin/env bash
# Build and run the Crust tests inside a Docker container.
# Uses the accompanying Dockerfile to set up the environment.
set -e

# Build the Docker image (tagged 'crust-tests')
docker build -t crust-tests .

# Execute the tests in a disposable container
docker run --rm crust-tests

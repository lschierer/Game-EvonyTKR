#!/bin/bash
set -e

# Cleanup function
cleanup() {
    echo "Shutting down processes..."
    kill $(jobs -p) 2>/dev/null || true
    wait
}

# Set trap for cleanup
trap cleanup EXIT INT TERM

# Start Minion worker in background


# Start web server with watchexec
echo "Starting web server with minimal file watching..."
watchexec --exts css,tt,js,pm -w lib/ -w templates/ -w share/public/ -w share/collections/data/ --restart ./bin/server.pl --mode development
#./bin/game-evonytkr daemon -m development
# This will run cleanup when watchexec exits

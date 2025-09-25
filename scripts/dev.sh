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
echo "Starting Minion workers..."
for i in $(seq 1 10); do
  ./bin/game-evonytkr minion worker -j 2 &
  WORKER_PID=$!
  echo "Worker PID: $WORKER_PID"
done


# Start web server with watchexec
echo "Starting web server with file watching..."
watchexec --exts css,pm,ep,js -w lib/ -w bin/ -w share/templates/ -w share/public/ -w share/collections/data/ --restart ./bin/game-evonytkr daemon -m development

# This will run cleanup when watchexec exits

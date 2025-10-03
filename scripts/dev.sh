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

if [ -f minion.db ]; then
  rm -fv minion.db*
fi

touch minion.db

nice -n 10 ./bin/game-evonytkr minion worker -j 15 -s 1 -S 1 &
WORKER_PID=$!
echo "Worker PID: $WORKER_PID"


# Start web server with watchexec
echo "Starting web server with file watching..."
watchexec --exts css,pm,ep,js -w lib/ -w bin/ -w share/templates/ -w share/public/ -w share/collections/data/ --restart ./bin/game-evonytkr daemon -m development
#./bin/game-evonytkr daemon -m development
# This will run cleanup when watchexec exits

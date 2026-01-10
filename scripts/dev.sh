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

# Clean up Redis persistence data for fresh start
rm -f ./var/redis.rdb ./var/persistence.db*

touch minion.db

export MOJO_MODE='development';
export MOJO_RENDERER_DEBUG=1;
#/opt/homebrew/opt/memcached/bin/memcached -l localhost &
#export MEM_RESULT=$?;
#if [ $MEM_RESULT -eq 0 ]; then
#  echo "memcached launched successfully"
#else
#  echo "memcached failed to launch $MEM_RESULT"
#  exit $MEM_RESULT;
#fi

#nice -n 10 ./bin/game-evonytkr minion worker -j 10 -s -10 -S 50 &
#nice -n 10 ./bin/game-evonytkr minion worker -j 2 -s 1  &
#nice -n 10 ./bin/game-evonytkr minion worker -j 2  &
#nice -n 10 ./bin/game-evonytkr minion worker -j 1 &
#WORKER_PID=$!
#echo "Worker PID: $WORKER_PID"


# Start web server with watchexec
echo "Starting web server with minimal file watching..."
watchexec --exts css,tt,js -w lib/ -w templates/ -w share/public/ -w share/collections/data/ --restart ./bin/server2.pl --mode development
#./bin/game-evonytkr daemon -m development
# This will run cleanup when watchexec exits

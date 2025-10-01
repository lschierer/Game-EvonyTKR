#!/bin/bash -x
set -e

eval "$(/opt/mojo/.local/bin/mise activate bash)"
export PATH="/opt/mojo/.local/bin/:$HOME/bin:$PATH"

cd /opt/mojo/app || exit 2

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
nice -n 10 ./bin/game-evonytkr minion worker -j 10 -I 5 &
WORKER_RETURN=$?
WORKER_PID=$!
if [! -z "$WORKER_RETURN" ]; then
  echo "worker returned $WORKER_RETURN";
  exit $WORKER_RETURN;
fi

hypnotoad ./bin/game-evonytkr

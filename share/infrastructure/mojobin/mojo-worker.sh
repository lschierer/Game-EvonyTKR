#!/bin/bash
set -e

source ${HOME}/.bash_profile || exit 1

cleanup() {
    echo "Shutting down workers..."
    kill $(jobs -p) 2>/dev/null || true
    wait
}

# Change to app directory
cd /opt/mojo/app || exit 2

./bin/game-evonytkr minion worker -j 6 -s 1 -S 1 -L 4 &
WORKER_RETURN=$?
WORKER_PID=$!
if [! -z "$WORKER_RETURN" ]; then
  echo "worker returned $WORKER_RETURN";
  exit $WORKER_RETURN;
else
  renice --priority 10 --pid ${WORKER_PID}
fi

wait

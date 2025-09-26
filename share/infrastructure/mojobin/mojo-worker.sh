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

# Run the workears
for i in {1..3}; do
  echo "Starting worker $i"
  ./bin/game-evonytkr minion worker -j 15 -I 5 &
  HR=$?;
  if [! -z "$HR" ]; then
    echo "worker returned $HR";
    exit $HR;
  fi
done

wait

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
./bin/game-evonytkr minion worker -j 5 -I 2 &
if [! -z "$HR" ]; then
  echo "worker returned $HR";
  exit $HR;
fi

wait

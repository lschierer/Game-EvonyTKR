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



hypnotoad ./bin/game-evonytkr

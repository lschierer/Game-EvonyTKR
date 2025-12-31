#!/bin/bash
set -e

source ${HOME}/.bash_profile || exit 1
cd /opt/mojo/app || exit 2

# Auto-detect MOJO_MODE from deployed config file
if [ -f game-evony_t_k_r.production.yml ]; then
  export MOJO_MODE=production
elif [ -f game-evony_t_k_r.staging.yml ]; then
  export MOJO_MODE=staging
else
  # Fallback for local development
  export MOJO_MODE=development
fi

echo "[minion-job.sh] Using MOJO_MODE=${MOJO_MODE}"

./bin/game-evonytkr minion job "$@"

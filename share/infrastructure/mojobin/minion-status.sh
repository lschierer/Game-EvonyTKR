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

echo "[minion-status.sh] Using MOJO_MODE=${MOJO_MODE}"

./bin/game-evonytkr eval 'say ref(app->minion->backend)'
./bin/game-evonytkr eval 'say app->minion->backend->sqlite->db->dbh->sqlite_db_filename'

./bin/game-evonytkr eval 'say app->minion->backend->sqlite->db->dbh->{sqlite_db_filename}'
#ls -la $(./bin/game-evonytkr eval 'say app->minion->backend->sqlite->db->dbh->{sqlite_db_filename}')

./bin/game-evonytkr minion job -s

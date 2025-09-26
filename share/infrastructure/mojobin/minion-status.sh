#!/bin/bash
set -e

source ${HOME}/.bash_profile || exit 1
cd /opt/mojo/app || exit 2
./bin/game-evonytkr eval 'say ref(app->minion->backend)'
./bin/game-evonytkr eval 'say app->minion->backend->sqlite->db->dbh->sqlite_db_filename'

./bin/game-evonytkr eval 'say app->minion->backend->sqlite->db->dbh->{sqlite_db_filename}'
#ls -la $(./bin/game-evonytkr eval 'say app->minion->backend->sqlite->db->dbh->{sqlite_db_filename}')

./bin/game-evonytkr minion job -s

#!/bin/bash
set -e

source ${HOME}/.bash_profile || exit 1
cd /opt/mojo/app || exit 2

./bin/game-evonytkr minion job "$@"

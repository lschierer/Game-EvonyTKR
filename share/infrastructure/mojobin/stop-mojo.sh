#!/bin/bash -x

source ${HOME}/.bash_profile || exit 1

cd /opt/mojo/app || exit 2

hypnotoad -s ./bin/game-evonytkr

HR=$?;

if [ -z "$HR" ]; then
 exit 0;
else
 echo "hypno returned $HR";
 exit $HR;
fi


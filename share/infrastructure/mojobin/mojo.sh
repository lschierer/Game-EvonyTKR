#!/bin/bash -x

eval "$(/opt/mojo/.local/bin/mise activate bash)"
export PATH="/opt/mojo/.local/bin/:$HOME/bin:$PATH"

cd /opt/mojo/app || exit 2

hypnotoad ./bin/game-evonytkr

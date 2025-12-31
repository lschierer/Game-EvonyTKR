#!/bin/bash 

eval "$(/opt/mojo/.local/bin/mise activate bash)"
export PATH="/opt/mojo/.local/bin/:$HOME/bin:$PATH"


cd /opt/mojo/app || exit 2
/usr/bin/env perl --version
pwd

./bin/game-evonytkr eval 'print "$_: $INC{$_}\n" for sort keys %INC'

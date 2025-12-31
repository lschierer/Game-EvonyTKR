#!/bin/bash 

eval "$(mise activate bash)"
/usr/bin/env perl --version

#cd /opt/mojo/app || exit 2

./bin/game-evonytkr eval 'print "$_: $INC{$_}\n" for sort keys %INC' > /tmp/ec2-modules.txt


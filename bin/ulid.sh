#!/usr/bin/env bash

export CWD=`pwd`

echo $CWD

export PERL=`which perl`

$PERL $CWD/src/lib/ulid.pl



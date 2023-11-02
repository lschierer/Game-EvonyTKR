#!/usr/bin/env bash

export PNPM=`which pnpm`

export CWD=`pwd`

echo $CWD

$PNPM ts-node $CWD/src/lib/ulid.js


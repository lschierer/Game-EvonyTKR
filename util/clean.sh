#!/bin/sh

LOCATION=$( dirname -- "${BASH_SOURCE[0]}" );

find $LOCATION/../.vscode/ -type d -iname .build -delete

dzil clean


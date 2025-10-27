#!/bin/bash

INPUTDIR=./dist/json/
OUTPUTDIR=./dist/schemas

[[ -d ./dist ]] || mkdir ./dist
if [[ -d $OUTPUTDIR ]]; then
  rm -rf $OUTPUTDIR
  mkdir $OUTPUTDIR
else
  mkdir $OUTPUTDIR
fi

find $INPUTDIR -type d -mindepth 1 -maxdepth 1 | while read -r fd; do
  DIR=`basename "$fd"`
  pnpm quicktype -l schema --src-lang json --src "$fd" > "$OUTPUTDIR/$DIR.schema.json"
done

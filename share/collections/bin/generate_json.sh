#!/bin/bash

[[ -d ./dist ]] || mkdir ./dist
if [[ -d ./dist/json ]]; then
  rm -rf ./dist/json
  mkdir ./dist/json
else
  mkdir ./dist/json
fi

find data -type d -mindepth 1 -maxdepth 1 | while read -r path; do
  DIR=`basename "$path"`
  echo "DIR is $DIR";
  [[ -d "./dist/json/$DIR" ]] || mkdir "./dist/json/$DIR"
  OUTDIR="./dist/json/$DIR"
  find "$path" -type f -name '*.yaml' | while read -r file; do
    out=`basename "$file" .yaml`;
    yq -o json "$file" > "$OUTDIR/$out.json"
  done
done

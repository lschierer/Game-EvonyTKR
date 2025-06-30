#!/bin/bash

INPUTDIR=./dist/json
OUTPUTDIR=./dist/schemas
TEMP=`mktemp`

[[ -d ./dist ]] || mkdir ./dist
if [[ -d $OUTPUTDIR ]]; then
  rm -rf $OUTPUTDIR/*.ts
else
  mkdir $OUTPUTDIR
fi

find $INPUTDIR -type d -mindepth 1 -maxdepth 1 | while read -r fd; do
  echo "fd is '$fd'"
  DIR=`basename "$fd"`
  TYPE=`echo "$DIR" | tr -d '[:blank:]'`
  cd "$fd";
  json_files=(*.json)
  if [ ${#json_files[@]} -eq 0 ]; then
    echo "No JSON files found in the current directory."
    exit 1
  fi

  combined_json=$(jq -s '.' "${json_files[@]}")
  cd $OLDPWD;
  echo "$combined_json" > $TEMP;
  pnpm quicktype -l typescript-zod --src-lang json --src $TEMP --top-level $TYPE > "$OUTPUTDIR/$DIR.ts"
  gsed -i -E 's/Schema//g' "$OUTPUTDIR/$DIR.ts"
  gsed -i -E 's/Enum//g' "$OUTPUTDIR/$DIR.ts"
  rm $TEMP;
done

#!/usr/bin/env bash

step1=$(find lib/ bin/ -type f -exec egrep -h '^\s*(parent|use|require)' {} \;) || exit 1

step2=$(echo "$step1" | grep -v required ) || exit 2

step3=$(echo "$step2" | sed 's/^[ \t]*//' | cut -d ' ' -f 2 | cut -d ';' -f 1) || exit 3

step4=$(echo "$step3" | sort -u ) || exit 4

step5=$(echo "$step4" | egrep -v '^(v5.40|feature|parent|overload|experimental|Game::EvonyTKR|utf8)') || exit 5

echo "$step5" | while read -r line; do

  echo "$line" | gsed -E 's/(.+)/"\1" => "0",/' | tr \" \'
done

#!/usr/bin/env bash

 step1=$(find lib/ script/ -type f -exec egrep -h '^\s*(use|require)' {} \;) || exit 1

 step2=$(echo "$step1" | sed 's/^[ \t]*//' | cut -d ' ' -f 2 | cut -d ';' -f 1) || exit 2

 step3=$(echo "$step2" | sort -u ) || exit 3

 step4=$(echo "$step3" | egrep -v '^(v5.40|feature|namespace|parent|overload|experimental|Game::EvonyTKR|utf8)') || exit 4

 echo "$step4"

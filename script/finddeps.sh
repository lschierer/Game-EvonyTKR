#!/usr/bin/env bash

 find . -type f -exec egrep '(use|require)' {} \; | sed 's/^[ \t]*//' | cut -d ' ' -f 2 | sort -u | egrep -v '^(v5.40|namespace|lib|experimental|Game::EvonyTKR|utf8)'

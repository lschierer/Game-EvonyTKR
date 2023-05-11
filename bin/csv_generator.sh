#!/bin/bash

cat ground.txt | cut -d / -f 1 | cat -n | sed -E -e 's/[[:space:]]+/ /g; s/^[[:space:]]*//; s/[[:space:]]/,/; s/[[:space:]]$//' > g1
cat ground.txt | cut -d / -f 2 | cut -d - -f 1| cat -n | sed -E -e 's/[[:space:]]+/ /g; s/^[[:space:]]*//; s/[[:space:]]/,/; s/[[:space:]]$//' > g2
cat ground.txt | cut -d / -f 2 | cut -d - -f 2- | cat -n | sed -E -e 's/[[:space:]]+/ /g; s/^[[:space:]]*//; s/[[:space:]]/,/; s/[[:space:]]$//' > g3

join -t, g1 g2> gt
join -t, gt g3 > ground_attack.csv
rm g1 g2 g3 gt


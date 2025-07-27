#!/bin/bash

echo "$1"  |
  perl -CSDA -pe 's/[\x{0022}\x{0027}\x{2018}\x{2019}\x{201C}\x{201D}\x{0060}\x{00B4}]//g; $_ = lc $_' | # remove fancy quotes, ascii quotes, and their double equivalents. make lower case.
  gsed -E 's/-/ /g' |
  xargs -I{} swipl -q -s share/prolog/Game/EvonyTKR/Shared/buff_parser.pl \
    -g "tokenize_and_parse('{}', Buffs), print_buffs_json(Buffs), halt." |
  grep -v DEBUG |  #without this jq complains.
  jq -s '.' |
  yq -P

#!/usr/bin/env bash

export TEMPDIR=`mktemp -d`
export CWD=`pwd`

for i in $CWD/src/content/generals/[A-Z]*.yaml; do
  echo $i;
  j=`basename $i .yaml`;
  OUTPUT="$TEMPDIR/$j.mdx"
  TITLE=$(grep -m 1 name src/content/generals/$j.yaml | cut -d ':' -f 2-)
  shopt -s extglob
  TITLE="${TITLE##*( )}"
  shopt -u extglob
  NAME=$(echo "$TITLE" | tr -s [:blank:] '_' | tr -d  '.')
  echo '---' > $OUTPUT

  {
    echo "title: $TITLE" ;
    echo "tableOfContents: false";
    echo '---';
    echo 'import General from "../../../../components/General.astro"';
    echo ' '
    echo "<General generalEntry='$NAME'/>"
  } >> "$OUTPUT"
done

mv $TEMPDIR/*.mdx $CWD/src/content/docs/generals/details/
rm -rf ${TEMPDIR}


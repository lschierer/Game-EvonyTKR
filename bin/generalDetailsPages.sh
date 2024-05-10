#!/usr/bin/env bash 

export TEMPDIR=`mktemp -d`
export CWD=`pwd`
find $CWD/src/content/generals/ -iname '*.yaml' | grep -v  '_' > ${TEMPDIR}/list;
wc -l ${TEMPDIR}/list

cat ${TEMPDIR}/list | while IFS= read -r file ; do
  echo "${file}";
  j=`basename "$file" .yaml`;
  OUTPUT="$TEMPDIR/$j.mdx"
  TITLE=$(grep -m 1 name "${file}" | cut -d ':' -f 2-)
  shopt -s extglob
  TITLE="${TITLE##*( )}"
  shopt -u extglob
  NAME=$(echo "$TITLE" | tr -s [:blank:] )
  echo '---' > "$OUTPUT"

  {
    echo "title: $TITLE" ;
    echo "tableOfContents: false";
    echo '---';
    echo 'import General from "../../../../components/General.astro"';
    echo ' '
    echo "<General generalEntry='$j'/>"
  } >> "$OUTPUT"
done 

mv ${TEMPDIR}/*.mdx $CWD/src/content/docs/generals/details/
#rm -rf ${TEMPDIR}

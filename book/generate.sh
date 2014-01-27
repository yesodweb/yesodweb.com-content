#!/bin/bash -ex

rm -rf generated-xml
mkdir generated-xml
for f in chapters/*.asciidoc
do
    BN=`basename $f`
    FILEID="${BN%.asciidoc}"
    DEST="generated-xml/${FILEID}.xml"
    asciidoc -b docbook45 --attribute=idprefix="${FILEID}_" -o "$DEST" "$f"
done

#!/bin/bash -ex

rm -rf generated-xml
mkdir generated-xml
for f in chapters/*.asciidoc
do
    BN=`basename $f`
    DEST=generated-xml/${BN%.asciidoc}.xml
    asciidoc -b docbook45 -o $DEST $f
done

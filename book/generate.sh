#!/bin/bash -ex

#rm -rf generated-xml
mkdir -p generated-xml
for f in *.asciidoc
do
    BN=`basename $f`
    FILEID="${BN%.asciidoc}"
    if [ "$FILEID" != "yesod-web-framework-book" -a "$FILEID" != "pr01" -a "$FILEID" != "book" ]
    then
        DEST="generated-xml/${FILEID}.xml"

        if [[ ! -f "$DEST" ]] || [[ "$f" -nt "$DEST" ]]
        then
            asciidoc -b docbook45 --attribute=idprefix="${FILEID}_" -o tmp "$f"
            runghc strip-article-info.hs tmp
            mv tmp "$DEST"
        fi
    fi
done

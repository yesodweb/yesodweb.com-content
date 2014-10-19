#!/bin/bash -ex

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#rm -rf generated-xml
mkdir -p $DIR/../generated-xml
for f in $DIR/../asciidoc/*.asciidoc
do
    BN=`basename $f`
    FILEID="${BN%.asciidoc}"
    if [ "$FILEID" != "pr01" -a "$FILEID" != "book" ]
    then
        DEST="$DIR/../generated-xml/${FILEID}.xml"

        if [[ ! -f "$DEST" ]] || [[ "$f" -nt "$DEST" ]]
        then
            asciidoc -b docbook45 --attribute=idprefix="${FILEID}_" -o tmp "$f"
            runghc $DIR/strip-article-info.hs tmp
            mv tmp "$DEST"
        fi
    fi
done

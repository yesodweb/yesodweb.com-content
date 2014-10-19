#!/bin/bash -ex

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR/../..

git subtree pull --prefix=book/asciidoc/ ~/writing/oreilly/ master          
git branch -D just-asciidoc || true
git subtree split --prefix=book/asciidoc/ -b just-asciidoc
cd ~/writing/oreilly
git pull $DIR/../.. just-asciidoc
git push

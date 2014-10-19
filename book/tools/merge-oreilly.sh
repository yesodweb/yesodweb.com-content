#!/bin/bash -ex

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR/../..

git subtree pull --prefix=book/asciidoc/ ~/writing/oreilly/ master          
git subtree push --prefix=book/asciidoc/ ~/writing/oreilly/ master

#!/bin/bash -ex

cp ../../sites/yesodweb.com/content-1.4/book/*.asciidoc .
cp ../../sites/yesodweb.com/content-1.4/book/images/* images
rm yesod-web-framework-book.asciidoc
git checkout pr01.asciidoc
git checkout conduits.asciidoc # FIXME figure out what to do with this chapter

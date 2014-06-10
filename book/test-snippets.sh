#!/bin/bash -ex

./generate.sh
cabal install xml-conduit classy-prelude-conduit sphinx xml-hamlet markdown yesod persistent-sqlite persistent-postgresql --force-reinstalls -j \
    --constraint 'wai < 3.0' # FIXME temporary
runghc extract-code.hs
(cd extracted && ghc --make *.hs)

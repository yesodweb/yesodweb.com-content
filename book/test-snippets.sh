#!/bin/bash -ex

./generate.sh
cabal install xml-conduit classy-prelude-conduit sphinx xml-hamlet markdown yesod persistent-sqlite persistent-postgresql monadcryptorandom wai-conduit --force-reinstalls -j --max-backjumps=-1
runghc extract-code.hs
(cd extracted && ghc --make *.hs)

#!/bin/bash -ex

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
$DIR/generate.sh
cabal install xml-conduit classy-prelude-conduit sphinx xml-hamlet markdown yesod persistent-sqlite persistent-postgresql monadcryptorandom wai-conduit esqueleto --force-reinstalls -j --max-backjumps=-1
runghc $DIR/extract-code.hs
(cd extracted && ghc --make *.hs)

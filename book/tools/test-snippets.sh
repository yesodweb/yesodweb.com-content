#!/usr/bin/env bash

set =eux

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
$DIR/generate.sh
#cabal install xml-conduit classy-prelude-conduit sphinx xml-hamlet markdown yesod persistent-sqlite persistent-postgresql monadcryptorandom wai-conduit esqueleto --force-reinstalls -j --max-backjumps=-1
stack --resolver lts-11.3 runghc \
      --package classy-prelude-conduit \
      --package markdown \
      --package sphinx \
      $DIR/extract-code.hs
(cd extracted && stack --resolver lts-11.3 ghc --package classy-prelude-yesod *.hs)

from juju2013/saucy-base
maintainer Greg Weber

RUN apt-get install -y adduser
RUN adduser --disabled-password --gecos "haskell,666" haskell
RUN echo "Defaults:haskell !requiretty" >> /etc/sudoers

RUN apt-get update
RUN apt-get install -y haskell-platform

RUN apt-get install -y asciidoc

# Postgres
RUN apt-get install -y postgresql postgresql-client postgresql-contrib libpq-dev

# Sqlite
RUN apt-get install -y sqlite3 libsqlite3-dev

# # when the building step is done, run the given <image>, mounting this directory inside
# sudo docker run --name yesodcontent -v `pwd`:/home/haskell -t -i <image> /bin/bash
#
# # switch to the haskell user in the image and its home directory
# su haskell
# cd
#
# # install the latest cabal
# RUN cabal update && cabal install Cabal cabal-install
# PATH=~/.cabal/bin:$PATH
# hash -r

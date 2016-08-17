I just made a minor tweak to the [yesodweb.com-content
repository](https://github.com/yesodweb/yesodweb.com-content), which contains
all of the content that gets displayed on this site, including this blog post
and the entire Yesod book. This repo contains both the original asciidoc
version of the content, as well as the XML docbook files that are generated
from it. Though that's a slightly contentious decision, storing it this way
avoids having to have the asciidoc tools installed on the server hosting the
website.

Until now, I've had to manually generate the XML from time to time after making
updates or merging pull requests. However, doing this kind of manual
maintenance is annoying. Much better to use automated systems. And whenever
possible, I like to go for Travis CI. The setup is pretty simple, but involves
some finer points that I thought may be interesting to others (not to mention
good documentation for myself in the future).

## Encrypted deployment key

In order to push from Travis back to Github, we need to have an SSH key. Travis
allows us to put encrypted content into the repo, which [anyone can see
themselves](https://github.com/yesodweb/yesodweb.com-content/blob/master/id_rsa.enc).
Within Travis, this file can be decrypted and then placed in the `~/.ssh`
directory with the following commands:

```
mkdir -p $HOME/.ssh
openssl aes-256-cbc -K $encrypted_92ac0cbbb1f3_key -iv $encrypted_92ac0cbbb1f3_iv -in id_rsa.enc -out id_rsa -d
mv id_rsa $HOME/.ssh
chmod 400 $HOME/.ssh/id_rsa
```

If you want to create your own encrypted files for Travis, you'll want to use
[the Travis command line interface](https://github.com/travis-ci/travis.rb)'s
encrypt-file command.

## Get necessary prerequisites installed

In order to build the XML files, we need three tools: asciidoc, GHC, and Stack.
The first two can be installed via apt, but until
[travis-ci/apt-source-whitelist#7](https://github.com/travis-ci/apt-source-whitelist/pull/7)
is merged, we need to install Stack more manually. This is all still pretty
easy to get set up:

```yaml
addons:
  apt:
    packages:
    - asciidoc
    - ghc-7.10.3
    sources:
    - hvr-ghc

install:
- export PATH=$HOME/.local/bin:/opt/ghc/7.10.3/bin:$PATH
- mkdir -p $HOME/.local/bin
- curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
```

## Generate the XML

This part's easy: we wipe out the original XML files and run the generate.sh
script:

```
- rm -f book/generated-xml/*
- book/tools/generate.sh
- git diff
```

We run `git diff` at the end to provide some useful feedback during PRs of the
resulting XML difference.

## Commit and push

This is where the magic happens. Let's look at the code (a bash script inlined in the YAML for Travis):

```
- |
  if [ $TRAVIS_PULL_REQUEST != false ]
  then
    echo Not pushing diff for a pull request
  elif [ -n "$(git status --porcelain)" ]
  then
    mkdir -p $HOME/.ssh
    openssl aes-256-cbc -K $encrypted_92ac0cbbb1f3_key -iv $encrypted_92ac0cbbb1f3_iv -in id_rsa.enc -out id_rsa -d
    mv id_rsa $HOME/.ssh
    chmod 400 $HOME/.ssh/id_rsa
    git config --global user.email "michael+travis@snoyman.com"
    git config --global user.name "Travis job for yesodweb/yesodweb.com-content"
    git add -A
    git commit -m "Travis auto-generate XML files, $(date --utc --iso=sec)"
    git push git@github.com:yesodweb/yesodweb.com-content.git HEAD:$TRAVIS_BRANCH
  else
    echo No changes present
  fi
```

If we're looking at a pull request, we don't want to ever push to the branch.
(Travis will also prevent us from making a silly mistake here, since the
decryption key we need to get the SSH key won't be available.) We also check if
there are any changes to the XML files. But in the case of a non-PR build that
has changes, we:

1. Set up the SSH key, as we described above
2. Commit all changes locally
3. Push the changes to the current branch (1$TRAVIS_BRANCH`)

## Full file

You can look at the current version of the .travis.yml [on
Github](https://github.com/yesodweb/yesodweb.com-content/blob/master/.travis.yml).
Here's the content at the time of writing:

```yaml
language: c
sudo: false

cache:
  directories:
  - $HOME/.stack

addons:
  apt:
    packages:
    - asciidoc
    - ghc-7.10.3
    sources:
    - hvr-ghc

install:
- export PATH=$HOME/.local/bin:/opt/ghc/7.10.3/bin:$PATH
- mkdir -p $HOME/.local/bin
- curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

script:
- rm -f book/generated-xml/*
- book/tools/generate.sh
- git diff
- |
  if [ $TRAVIS_PULL_REQUEST != false ]
  then
    echo Not pushing diff for a pull request
  elif [ -n "$(git status --porcelain)" ]
  then
    mkdir -p $HOME/.ssh
    openssl aes-256-cbc -K $encrypted_92ac0cbbb1f3_key -iv $encrypted_92ac0cbbb1f3_iv -in id_rsa.enc -out id_rsa -d
    mv id_rsa $HOME/.ssh
    chmod 400 $HOME/.ssh/id_rsa
    git config --global user.email "michael+travis@snoyman.com"
    git config --global user.name "Travis job for yesodweb/yesodweb.com-content"
    git add -A
    git commit -m "Travis auto-generate XML files, $(date --utc --iso=sec)"
    git push git@github.com:yesodweb/yesodweb.com-content.git HEAD:$TRAVIS_BRANCH
  else
    echo No changes present
  fi
```

There's nothing particularly complicated or earth-shattering about this
approach, but hopefully putting it all together like this can help others
implement this themselves more easily.

Yesterday, I [noticed a bunch of Travis build
failures](https://plus.google.com/+MichaelSnoyman/posts/e5drS8sBStz) with the
cabal error message "does not exist." As [I covered last
month](http://www.yesodweb.com/blog/2015/06/cabals-does-not-exist-error-message),
this is typically due to a download failure when trying to install packages.
This leaves some ambiguity as to where the download failure originated from:
was the client (in this case, Travis) having network issues, or was there a
problem on Hackage's end? (Or, of course, any one of dozens of other possible
explanations.)

I have no hard data to prove that the failure is from the Hackage side, but
anecdotal evidence suggests that it's Hackage. In particular, Gregory Collins
reports that [the Snap build
bot](http://buildbot.snapframework.io/job/snap-server/ghc_ver=7.6.3,variant=portable/65/console)
is also having issues. When we had these issues in FP Haskell Center, we
resolved it by switching to [an S3
mirror](https://www.fpcomplete.com/blog/2015/03/hackage-mirror), so I've
decided to start migrating my Travis jobs over to this mirror as well. This is
fairly straightforward. First, add a new script to your repo containing:

```shell
#!/bin/sh

set -eux

mkdir -p $HOME/.cabal
cat > $HOME/.cabal/config <<EOF
remote-repo: hackage.haskell.org:http://hackage.fpcomplete.com/
remote-repo-cache: $HOME/.cabal/packages
jobs: \$ncpus
EOF
```

I called this file `.travis-setup.sh`, but you can give it whatever name you
want. Just make sure to set it as executable. Then, just make sure to run this
script from your `.travis.yml` script, likely in the `before_install` section,
e.g.:

```yaml
before_install:
 - export PATH=$HOME/.cabal/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH
 - ./.travis-setup.sh
```

My first rollout for this is [the stack
repository](https://github.com/commercialhaskell/stack). My plan is to keep
this blog post up-to-date with the best instructions for doing this, so if you
have recommendations on how to improve this setup, please report back to me (or
just [send a pull
request](https://github.com/yesodweb/yesodweb.com-content/tree/master/blog/2015/07).

Well Typed's [TUF
implementation](http://www.well-typed.com/blog/2015/07/hackage-security-alpha/)
is planned to allow cabal to download from mirrors automatically, which will
hopefully fix this problem in the future. But this change is a useful stop-gap
measure. The only downside I'm aware of is that the mirror can take up to 30
minutes to sync with Hackage, so you may end up with a slightly older package
index than if you used Hackage itself.

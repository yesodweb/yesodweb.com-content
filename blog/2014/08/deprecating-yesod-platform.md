I want to deprecate the yesod-platform, and instead switch to Stackage server
as the recommended installation method for Yesod for end users. To explain why,
let me explain the purpose of yesod-platform, the problems I've encountered
maintaining it, and how [Stackage
Server](https://www.fpcomplete.com/blog/2014/08/announcing-stackage-server) can
fit in. I'll also explain some unfortunate complications with Stackage Server.

## Why yesod-platform exists

Imagine a simpler Yesod installation path:

1. `cabal yesod-bin`, which provides the `yesod` executable.
2. `yesod init` to create a scaffolding.
3. `cabal install` inside that directory, which downloads and installs all of the necessary dependencies.

This in fact used to be the installation procedure, more or less. However, this
led to a number of user problems:

* Back in the earlier days of cabal-install, it was difficult for the dependency solver to find a build plan in this situation. Fortunately, cabal-install has improved drastically since then.
    * This *does* still happen occasionally, especially with packages with restrictive upper bounds. Using `--max-backjumps=-1` usually fixes that.
* It sometimes happens that an upstream package from Yesod breaks Yesod, either by changing an API accidentally, or by introducing a runtime bug.

This is where yesod-platform comes into play. Instead of leaving it up to
cabal-install to track down a consistent build plan, it specifies exact
versions of all depedencies to ensure a consistent build plan.

## Conflicts with GHC deps/Haskell Platform

Yesod depends on aeson. So logically, yesod-platform should have a strict
dependency on aeson. We try to always use the newest versions of dependencies,
so today, that would be `aeson == 0.8.0.0`. In turn, this demands `text >=
1.1.1.0`. However, if you look at [the Haskell Platform
changelog](https://www.haskell.org/platform/changelog.html), there's no version
of the platform that provides a new enough version of `text` to support that
constraint.

yesod-platform could instead specify an older version of aeson, but that would
unnecessarily constrain users who aren't sticking to the Haskell Platform
versions (which, in my experience, is the majority of users). This would also
cause more dependency headaches down the road, as you'd now also need to force
older versions of packages like criterion.

To avoid this conflict, yesod-platform has taken the approach of simply
omitting constraints on any packages in the platform, as well as any packages
with strict bounds on those packages. And if you look at yesod-platform today,
you'll that there is no mention of aeson or text.

A similar issue pops up for packages that are a dependency of the GHC package
(a.k.a., GHC-the-library). The primary problem there is the binary package. In
this case, the allowed version of the package depends on which version of GHC
is being used, not the presence or absence of the Haskell Platform.

This results in two problems:

* It's very difficult to maintain this list of excluded packages correctly. I
  get large number of bug reports about these kinds of build plan problems.

* We're giving up quite a bit of the guaranteed buildability that
  yesod-platform was supposed to provide. If aeson 0.7.0.4 (as an example)
  doesn't work with yesod-form, yesod-platform won't be able to prevent such a
  build plan from happening.

There's also an issue with the inability to specify dependencies on
executable-only packages, like alex, happy, and yesod-bin.

## Stackage Server

Stackage Server solves exactly the same problem. It provides a consistent set
of packages that can be installed together. Unlike yesod-platform, it can be
distinguished based on GHC version. And it's far simpler to maintain. Firstly,
I'm already maintaining Stackage Server full time. And secondly, all of the
testing work is handled by a very automated process.

So here's what I'm proposing: I'll deprecate the yesod-platform package, and change the Yesod quickstart guide to have the following instructions:

* Choose an appropriate Stackage snapshot from stackage.org
* Modify your cabal config file appropriately
* `cabal install yesod-bin alex happy`
* Use `yesod init` to set up a scaffolding
* `cabal install --enable-tests` in the new directory

For users wishing to live on more of a bleeding edge, the option is always
available to simply not use Stackage. Such a usage will give more control over
package versions, but will also lack some stability.

## The problems

There are a few issues that need to be ironed out.

*   [cabal sandbox does not allow changing the remote-repo](https://github.com/haskell/cabal/issues/1884). Fortunately, [Luite seems to have this solved](http://www.reddit.com/r/haskell/comments/2djs0f/announcing_stackage_server/cjq79ar), so hopefully this won't be a problem for long. Until then, you can either use a single Stackage snapshot for all your development, or use a separate sandboxing technology like hsenv.

*   Haskell Platform conflicts still exist. The problem I mentioned above with aeson and text is a real problem. The theoretically correct solution is to create a Stackage snapshot for GHC 7.8 + Haskell Platform. And if there's demand for that, I'll bite the bullet and do it, but [it's not an easy bullet to bite](https://plus.google.com/116553865628071717889/posts/aZXEfqwPP25). But frankly, I'm not hearing a lot of users saying that they want to peg Haskell Platform versions specifically.

    In fact, the only users who really seem to want to stick to Haskell Platform versions are Windows users, and the main reason for this is the complexity in installing the network package on Windows. I think there are three possible solutions to this issue, without forcing Windows users onto old versions of packages:

    1. [Modify the network package to be easier to install on Windows.](http://comments.gmane.org/gmane.comp.lang.haskell.libraries/22577) I really hope this has some progress. If this is too unstable to be included in the official Hackage release, we could instead have an experimental Stackage snapshot for Windows with that modification applied.
    2. Tell Windows users to simply bypass Stackage and yesod-platform, with the possibility of more build problems on that platform.
        * We could similarly recommend Windows users develop in a Linux virtual machine/Docker image.
    3. Provide a Windows distribution of GHC + cabal-install + network. With the newly split network/network-uri, this is a serious possibility.

Despite these issues, I think Stackage Server is a definite improvement on
yesod-platform on Linux and Mac, and will likely still improve the situation on
Windows, once we figure out the Haskell Platform problems.

I'm not making any immediate changes. I'd very much like to hear people using
Yesod on various operating systems to see how these changes will affect them.

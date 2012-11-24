About two weeks ago, I wrote [a blog post giving a proposal to solve Cabal
hell](http://www.yesodweb.com/blog/2012/11/solving-cabal-hell) for end users.
Let me summarise some main points of my proposal:

* Cabal isn't really the problem at this point (so Cabal hell is really a
  misnomer). The problem is that Hackage itself is unstable.

* Hackage *should* be unstable, that's its purpose.

* Many end users simply need access to a subset of Hackage which is guaranteed
  to play well together.

* The Haskell Platform goes a step beyond that, by placing high quality
  standards on its packages. This is a good move for the Platform, but doesn't
  cover user needs enough.

* In other words, we need a level between Hackage and the Haskell Platform.

I received a lot of very positive feedback, and so I'd like to get things
rolling. To that end, I've [created a Github
repo](https://github.com/snoyberg/stackage) for this project. (Note: I've
called it "Stackage" == Stable Hackage for now, but I don't really like that
name at all. If someone can come up with a better name, please let me know.)

The most important file is
[Stackage.Config](https://github.com/snoyberg/stackage/blob/master/Stackage/Config.hs),
and in particular the `stablePackages` value. This is where we would specify
which packages should be included in the package set. My theory is that
collaboration on this project would work as pull requests: if someone wants to
add a package to the repo, they submit a pull request that adds the new
package, along with a comment saying who will be maintaining that package.

There are really two aspects to using Stackage: building the repos, and using a
set of built repos. The idea is that we'll do the former as a community process
(maybe on a dedicated build server, or a series of servers with different
OSes), place the repos on some publicly available server, and then an end user
would just need to do:

```bash
cabal install stackage # just needs to be run once
stackage update
```

From then on, the user would be guaranteed to never enter dependency hell when
installing our blessed packages. For our purposes, the blessed packages would
be the list of stable packages and all of their dependencies.

If you want to get started with trying out the code, you can try the following:

```bash
cabal update
cabal install cabal-dev
git clone https://github.com/snoyberg/stackage
git submodule update --init # get the Haskell Platform files
runghc app/stackage.hs build # takes a *long* time
runghc app/stackage.hs init # modifies your ~/.cabal/config file
```

I've only tested this on Linux for now. I'm fairly certain we'll run into
issues on other platforms, either due to differences in where Cabal stores
things or missing platform-specific dependencies. If people can give the code a
shot on Mac and Windows and file issues (or better: send pull requests), that
would be a great way to move forward.

A few other minor details:

* This is built as a superset of the Haskell Platform, so it pegs the versions
  of HP packages to the versions released in the platform itself.

*   There are actually two repos generated: stackage contains the blessed
    packages only, with only one version of each, and stackage-extra contains
    all packages which are neither distributed with GHC nor in the blessed list.
    This means that you can install (in theory) any package on Hackage, though you
    can still run into dependency hell with the -extra list.

So how can we move forward on this? There are a few important ways:

*   Add more packages. I've included Yesod and all its dependencies. As Yesod
    users know, that already encompasses a lot of the most commonly used
    packages out there. But having wider coverage is even better. I have a feeling
    adding in Snap and Happstack should be relatively painless, since we overlap
    quite a bit in library usage.

*   Start hosting prepared repositories somewhere, and then release `stackage`
    onto Hackage. We clearly need to have some more testing done before this
    can happen.

*   Set up automated, cross-platform testing. It would be wonderful if, before
    each release, every package was compiled and tested on all the major
    operating systems and versions of GHC.

*   Running the build procedure still reports errors for a number of packages.
    I've been sending a lot of pull requests over the past few days to try and
    get those corrected, but there are still some more out there. I'm hoping that
    by having this kind of automated tool running regularly, we'll be able to spot
    problems in packages quickly and alert the maintainers. To make the system
    great, I'm hoping that maintainers will be able to help out by making necessary
    changes to their packages.

*   It would be great to have Linux distribution maintainers on board with this
    initiative. Having the same set of stable packages available on multiple
    platforms would be great, and hopefully this project will allow us all to pool
    resources.

But I want to make one thing clear. For this project to succeed, it has to have
wide-spread community support. It's possible to make a tool that solves the
dependency problem for just the Yesod world, but that's much less valuable than
something the whole community participates in. I'm looking forward to getting
input from the community at large.

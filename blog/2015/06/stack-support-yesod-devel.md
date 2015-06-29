Since [the release of the stack build
tool](https://www.fpcomplete.com/blog/2015/06/announcing-first-public-beta-stack),
I think I've received four different requests and bug reports about stack
support in Yesod. The sticking point is that `yesod devel` has not- until now-
had support for using stack. The original plan was to wait for Njagi Mwaniki's
Google Summer of Code project to finish the new ide-backend based yesod devel.
However, demand for this feature was too high, so I'm happy to announce
official stack support in yesod-bin-1.4.11.

This blog post should be consider a beta announcement for using Yesod with
stack. Please test and report issues. Also, the workflow is not yet perfected.
Once we get [stack new](https://github.com/commercialhaskell/stack/issues/137)
written and [add ide-backend
support](https://github.com/commercialhaskell/stack/issues/232), things will be
much smoother. For now, here's the workflow:

```
# Install both yesod-bin and cabal-install. Both are still necessary
$ stack install yesod-bin-1.4.11 cabal-install
# Initialize yoru project
$ stack exec yesod init
$ cd new-directory
# Create stack.yaml
$ stack init
# Build your project to get al dependencies
# Also rebuild yesod-bin with the current GHC, just to be safe
$ stack build yesod-bin-1.4.11 .
# Now run yesod devel
$ stack exec yesod devel
```

Like I said, this is a little kludgy right now, but will smooth out over time.

## Technical details

If you're curious in how I added this support, the [commit is really
short](https://github.com/yesodweb/yesod/commit/a7cccf2a7c5df8b26da9ea4fdcb6bac5ab3a3b75).
And there's not actually anything stack-specific in here, it's just working
around a well known limitation in cabal which makes it incompatible with the
`GHC_PACKAGE_PATH` environment variable. All we do in `yesod devel` is:

* Detect the `GHC_PACKAGE_PATH` variable
* Turn it into `--package-db` arugments to `cabal configure`
* Remove `GHC_PACKAGE_PATH` from the environment of the cabal process

It would be nice if cabal got this functionality itself in the future, but I
believe the current implementation was a design goal of the cabal team.

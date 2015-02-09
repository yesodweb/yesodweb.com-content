A few weeks ago I received a [bug report against
streaming-commons](https://github.com/fpco/streaming-commons/issues/16). Since
then, the details of what we discovered when discussing this report have been
bothering me quite a bit, as they expose a lot of the brittleness of the
Haskell toolchain. I'm documenting all of these aspects now to make clear how
fragile our tooling in, and thereby explain why I think Stackage is so vital to
our community.

In this blog post, I'm going to describe six separate problems I've identified
when looking into this issue, and explain how Stackage (or some similar
deterministic build system) would have protected users against these problems
had it been employed.

## The story

streaming-commons is a library that provides helper utilities for a number of
different streaming concepts, one of them being a streaming way to convert
blaze-builder `Builder`s to filled `ByteString` buffers. Since blaze-builder
was released a few years ago, a new set of modules was added to the bytestring
package in version 0.10 known as a "bytestring builder." I asked one of the
engineers at FP Complete, Manny Borsboom, to start working on a new module for
streaming-commons to provide similar functionality for bytestring builder.

And now we run into the first problem with the Haskell toolchain. You would
think that we should just add a lower bound on `bytestring >= 0.10` in the
streaming-commons.cabal file. However, setting restrictive lower bounds on
ghc-package dependencies [can be a
problem](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds).
Fortunately, Leon Smith [already solved this problem for us with
bytestring-builder](http://hackage.haskell.org/package/bytestring-builder),
which provides a compatibility layer for older bytestrings (much like Ed's
[transformers-compat](http://hackage.haskell.org/package/transformers-compat)).
The idea is that, when compiled against an older version of bytestring, the
bytestring-builder package provides the necessary missing modules, and
otherwise does nothing.

When Manny wrote his changes to streaming-commons, he added a dependency on
bytestring-builder. We then proceeded to test this on multiple versions of GHC
via Travis CI and Herbert's
[multi-ghc-travis](https://github.com/hvr/multi-ghc-travis). Everything
compiled and passed tests, so we shipped the updated version.

However, that [original bug report I linked
to](https://github.com/fpco/streaming-commons/issues/16)- reported by Ozgun
Ataman- told us there was a problem with GHC 7.6. This was pretty surprising,
given that we'd *tested* on GHC 7.6. Fortunately Lane Seppala [discovered the
culprit](https://github.com/fpco/streaming-commons/issues/16#issuecomment-70584708):
the Cabal library. It turns out that installing a new version of the Cabal
library causes the build of streaming-commons to break, whereas our tests just
used the default version of Cabal shipped with GHC 7.6. (We'll get back to
*why* that broke things in a bit.)

After some digging, [Manny
discovered](https://github.com/fpco/streaming-commons/issues/16#issuecomment-70694389)
the deeper cause of the problem: [Bryan O'Sullivan
reported](https://github.com/lpsmith/bytestring-builder/issues/1) an issue a
year ago where- when using a new version of the Cabal library-
bytestring-builder does *not* in fact provide it's compatibility modules. This
leads us to our second really annoying issue: this known bug existed for almost
a year without resolution, and since it only occurs in unusual circumstances,
was not detected by any of our automated tooling.

The reason this bug existed though is by far the most terrifying thing I saw in
this process: the Cabal library silently changed the semantics of one of its
fields in the 1.18 (or 1.20? I'm not sure) release. You see, bytestring-builder
was detecting which version of bytestring it was compiled against by inspecting
the `configConstraints` field (you can [see the code yourself on
Hackage](http://hackage.haskell.org/package/bytestring-builder-0.10.4.0.1/src/Setup.hs)).
And starting in Cabal 0.19.1 (a development release), that field was no longer
being populated. As a result, as soon as that newer Cabal library was
installed, the bytestring-builder package became worse than useless.

As an aside, this points to another painful aspect of our toolchain: there is
no way to specify constraints on dependencies used in custom `Setup.hs` files.
That's actually a much more painful issue than it may sound like, but I'll skip
diving into it for now.

The fix for this was [relatively
simple](https://github.com/manny-fp/bytestring-builder/commit/7788ab3df311e5053573a6354118a90a6f01454a):
use some flag logic in the cabal file instead of a complicated custom
`Setup.hs` file. (Once this pull request was merged in and released, it *did*
fix the original bug report.) But don't take this as a critique of Leon's
choice of a complicated `Setup.hs` file. Because in reality, the flag trick-
while the "standard" solution to this problem- [broke cabal-install's
dependency solver for quite a
while](https://github.com/haskell/cabal/issues/1855). To be fair, I'm *still*
not completely convinced that the bug is fixed, but for now that bug is the
lesser of two evils vs the Cabal library bug.

And finally, based on the bug report from Ozgun, it seems like an internal
build failed based on all of this occurring. This has been a constant criticism
I've made about the way we generally do builds in the Haskell world. Rarely is
reproducibility a part of the toolchain. To quote Ozgun:

> We are in fact quite careful in dependency management with lower-upper bounds
> on most outside packages, so breakages like this are unexpected.

And many people feel that this is the way things should be. But as this
discussion hopefully emphasizes, just playing with lower and upper bounds is
*not* sufficient to avoid build failures in general. In this case, we're
looking at a piece of software that was broken by a change in a library that
*it didn't depend on*, namely Cabal, since our tooling makes an implicit
dependency on that library, and we have no way of placing bounds on it.

## The case for Stackage

So here are the toolchain problems I've identified above:

1. Tight coupling between GHC version of some core libraries like bytestring.
2. A known issue lasting undocumented for a corner case for over a year, without any indication on the Hackage page that we should be concerned.
3. The Cabal library silently changed the semantics of a field, causing complete breakage of a package.
4. cabal-install's solver gets confused by standard flag usage, at least in slightly older versions.
5. Not all dependencies are actually specified in a cabal file. At the very least, the Cabal library version is unconstrained, and any other packages used by Setup.hs.
6. Default Haskell toolchain doesn't protect us against these kinds of problems, or give us any concept of reproducibility.

Stackage completely solves (2), (3), (5), and (6) for end users. By specifying
all library versions used, and then testing all of those versions together, we
avoid many possible corner cases of weird library interactions, and provide a
fully reprodible build. (Note the Stackage doesn't solve *all* such cases:
operating system, system libraries, executables, etc are still unpinned. That's
why FP Complete is [working on Docker-based
tooling](https://www.fpcomplete.com/blog/2015/01/fp-complete-software-pipeline).)

(1) is highly mitigated by Stackage because, even though the tight coupling
still exists, Stackage is provided a set of packages that take that coupling
into account for you, so you're not stuck trying to put the pieces together
yourself.

As for (4)... Stackage *helps* the situation by making the job of the
solver simpler by pinning down version numbers. Unfortunately, there are still
potential gotchas when encountering solver bugs. Sometimes we end up needing to
implement [terribly awkward solutions to work around those
bugs](https://github.com/haskell/cabal/issues/1855#issuecomment-43635727).

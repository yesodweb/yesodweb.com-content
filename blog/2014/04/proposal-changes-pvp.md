As I [mentioned two posts
ago](http://www.yesodweb.com/blog/2014/03/package-consolidation), there was a
[serious discussion on the libraries mailing
list](http://www.haskell.org/pipermail/libraries/2014-February/022114.html)
about the [Package Versioning
Policy](http://www.haskell.org/haskellwiki/Package_versioning_policy) (PVP).

This blog post presents some concrete changes I'd like to see to the PVP to
make it better for both general consumers of Hackage, and for library authors
as well. I'll start off with a summary of the changes, and then give the
explanations:

1.  The *goal* of the PVP needs to be clarified. Its purpose is *not* to ensure
    reproducible builds of non-published software, but rather to provide for
    more reliable builds of libraries on Hackage. Reproducible builds should be
    handled exclusively through version freezing, the only known technique to
    actually give the necessary guarantees.

2.  Upper bounds should not be included on non-upgradeable packages, such as
    base and template-haskell (are there others?). Alternatively, we should
    establish some accepted upper bound on these packages, e.g. many people place
    base < 5 on their code.

3.  We should be distinguishing between mostly-stable packages and unstable
    packages. For a package like text, if you simply import Data.Text (Text,
    pack, reverse), or some other sane subset, there's no need for upper bounds.

    Note that this doesn't provide a hard-and-fast rule like the current PVP, but is
    rather a matter of discretion. Communication between library authors and users (via
    documentation or other means) would be vital to making this work well.

4.  For a package version A.B.C, a bump in A or B indicates some level of
    breaking change. As an opt-in approach, package authors are free to
    associated meaning to A and B beyond what the PVP requires. Users of these
    packages are free to rely on the guarantees provided by package authors when
    placing upper bounds.

    Note that this is very related to point (3).

## Reproducible builds

There are a number of simple cases that can result in PVP-compliant code not
being buildable. These aren't just hypothetical cases; in my experience as both
a package author and Stackage maintainer, I've seen these come up.

*   Package foo version 1.0 provides an instance for MonadFoo for IO and
    Identity. Version 1.1 removes the IO instance for some reason. Package bar
    provides a function:

    ```haskell
    bar :: MonadFoo m => Int -> m Double
    ```

    Package bar compiles with both version 1.0 and 1.1 of foo, and therefore
    (following the PVP) adds a constraint to its cabal file `foo >= 1.0 && < 1.2`.

    Now a user decides to use the bar package. The user never imports anything from
    foo, and therefore has no listing for foo in the cabal file. The user code
    depends on the IO instance for MonadFoo. When compiled with foo 1.0, everything
    works fine. However, when compiled with foo 1.1, the code no longer compiles.

*   Similarly, instead of typeclass instances, the same situation can occur
    with module export lists. Consider version 1.0 of foo which provides:

    ```haskell
    module Foo (foo1, foo2) where
    ```

    Version 1.1 removes the foo2 export. The bar package reexports the entire Foo
    module, and then a user package imports the module from bar. If the user
    package uses the foo2 function, it will compile when foo-1.0 is used, but not
    when foo-1.1 is used.

In both of these cases, the issue is the same: transitive dependencies are not
being clamped down. The PVP makes an assumption that the entire interface for a
package can be expressed in its version number, which is not true. I see three
possible solutions to this:

1.  Try to push even more of a burden onto package authors, and somehow make
    them guarantee that their interface is completely immune to changes
    elsewhere in the stack. This kind of change was proposed on the libraries list.
    I'm strongly opposed to some kind of change like this: it makes authors' lives
    harder, and makes it very difficult to provide backwards compatibility in
    libraries. Imagine if transformers 0.4 adds a new MonadIO instance; the logical
    extreme of this position would be to disallow a library from working with both
    transformers 0.3 and 0.4, which will split Hackage in two.

2.  Modify the PVP so that instead of listing just direct dependencies, authors
    are required to list all transitive dependencies as well. So it would be a
    violation to depend on bar without explicitly listing foo in the dependency
    list. This will work, and be incredibly difficult to maintain. It will also
    greatly increase the time it takes for a new version of a deep dependency to be
    usable due to the number of authors who will have to bump version bounds.

3.  Transfer responsibility for this to package users: if you first built your
    code against foo 1.0, you should freeze that information and continue
    building against foo 1.0, regardless of the presence of new versions of foo.
    Not only does this increase reproducibility, it's just common sense: it's
    entirely possible that new versions of a library will introduce a runtime bug,
    performance regression, or even fix a bug that your code depends on. Why should
    the reliability of my code base be dependent on the actions of some third party
    that I have no control over?

## Non-upgradeable packages

There are some packages which ship with GHC and cannot be upgraded. I'm aware
of at least base and template-haskell, though perhaps there are others
(haskell98 and haskell2010?). In the past, there was good reason to place upper
bounds on base, specifically with the base 3/4 split. However, we haven't had
that experience in a while, and don't seem to be moving towards doing that
again. In today's world, we end up with the following options:

* Place upper bounds on base to indicate "I haven't tested this with newer
  versions of GHC." This then makes it difficult for users to test out that
  package with newer versions of GHC.
* Leave off upper bounds on base. Users may then try to install a package onto
  a version of GHC on which the package hasn't been tested, which will result
  in either (1) everything working (definitely the common case based on my
  Stackage maintenance), or (2) getting a compilation error.

I've heard two arguments to push us in the direction of keeping the upper
bounds in this case, so I'd like to address them both:

*   cabal error messages are easier to understand than GHC error messages. I have two problems with that:
    *   I disagree: cabal error messages are terrible. (I'm told this will be fixed in the next version of cabal.) Take the following output as a sample:

        ```
        cabal: Could not resolve dependencies:
        trying: 4Blocks-0.2
        rejecting: base-4.6.0.1/installed-8aa... (conflict: 4Blocks => base>=2 && <=4)
        rejecting: base-4.6.0.1, 4.6.0.0, 4.5.1.0, 4.5.0.0, 4.4.1.0, 4.4.0.0, 4.3.1.0,
        4.3.0.0, 4.2.0.2, 4.2.0.1, 4.2.0.0, 4.1.0.0, 4.0.0.0, 3.0.3.2, 3.0.3.1 (global
        constraint requires installed instance)
        ```

        I've seen a number of users file bug reports not understanding that
        this message means "you have the wrong version of GHC."

    *   Even if the error messages were more user-friendly, they make it more
        difficult to fix the actual problem: the code doesn't compile with the
        new version of GHC. Often times, I've been able to report an error message to a
        library author and, without necessarily even downloading the new version of
        GHC, he/she has been able to fix the problem.

*   Using upper bounds in theory means that cabal will be able to revert to an
    older version of the library that is compatible with the new version of
    GHC. However, I find it highly unlikely that there's often- if ever- a case
    where an older version of a library is compatible with a later version of GHC.

## Mostly-stable, and finer-grained versioning

I'll combine the discussion of the last two points. I think the heart of the
PVP debates really comes from mostly-stable packages. Let's contrast with the
extremes. Consider a library which is completely stable, never has a breaking
change, and has stated with absolute certainty that it never will again. Does
*anyone* care about upper bounds on this library? They're irrelevant! I'd have
no problem with including an upper bound, and I doubt even the staunchest PVP
advocates would really claim it's a problem to leave it off.

On the other hand, consider an extremely unstable library, which is releasing
massively breaking changes on a weekly basis. I would certainly agree in that
case that an upper bound on that library is highly prudent.

The sticking point is the middle ground. Consider the following code snippet:

```haskell
import Data.Text (Text, pack)

foo :: Text
foo = pack "foo"
```

According to the PVP as it stands today, this snippet requires an upper bound
of `< 1.2` on the text package. But let's just play the odds here: does anyone
actually believe there's a real chance that the next iteration of `text` will
break this code snippet? I highly doubt it; this is a stable subset of the text
API, and I doubt it will ever be changing. The same can be said of large
subsets of many other packages.

By putting in upper bounds in these cases, we run a very real risk of
bifurcating Hackage into "those demanding the new text version for some new
feature" vs "those who haven't yet updated their upper bounds to allow the new
version of text."

The PVP currently takes an extremely conservative viewpoint on this, with the
goal of solving just one problem: making sure code that compiles now continues
to compile. As I demonstrated above, it doesn't actually solve that problem
completely. And in addition, in this process, it has created other problems,
such as this bifurcation.

So my proposal is that, instead of creating rigid rules like "always put an
upper bound no matter what," we allow some common sense into the process, and
also let package authors explicitly say "you can rely on this API not
changing."

## Identifying the problem

In the past few weeks, there has been a flurry of discussion around making
Cabal better. The main issue on the table is Cabal dependency hell. Most
discussions on the topic are based on the premise that if we just "fix Cabal"
then dependency hell will disappear. I want to make a completely different
claim: for the average user, Cabal is already working fairly reliably (though
I'm sure some improvements can be made).

Let's clarify who this "average user" is. I'm not talking about the developer
who's working on some new library to be released to Hackage. Anyone publishing
Haskell libraries is already outside the average range. For that user, powerful
sandboxing utilities are a highly desirable feature.
(By sandbox, I mean the ability to have totally isolated Haskell library builds.)
But for my theoretical
average user, sandboxing is just one extra thing he/she has to learn.

In my world, the average user is someone who has heard about this Haskell
thing, is interested trying out some library (I'll use Yesod as the example
since (1) I know a lot about it and (2) it's a common pain point in Cabal), and
wants to get going as quickly and easily as possible. Sandboxing achieves
*neither* of those goals: it's not quick, because it requires installing extra
tools and compiling libraries multiple times, and it's not easy because there's
a high cognitive overhead to understanding which tools to use and how to use
them.

In other words: the average developer is a __beginner__.

So let's disect a common case of dependency hell. (Real life example coming.)
Alice comes along and decides she's going to try Yesod. She installs the
Haskell platform, runs `cabal update && cabal install yesod-platform`, and gets
everything installed. No problems occur at all. She's developing along, and
decides to test out Fay. So she types in `cabal install fay`. Assuming this
occured before October 28, she'll run into a conflict: `fay` depends on
`language-ecmascript`, which depends on `data-default` version 0.4. But Yesod
is installed with `data-default` version 0.5. Instant Cabal Hell!

To clarify: the problem we're facing is entirely about __version bounds__. This
problem is triggered by Yesod having a version bound which includes
`data-default` 0.5, while `language-ecmascript` does not include version 0.5.
Cabal eagerly tries to install Yesod with the most recent version of all
relevant packages that fit the constraints of Yesod, but doesn't know that Fay
will be thrown into the mix later.

So who's at fault here? Let's list the culprits and their alibis:

-   Yesod, for using `data-default` version 0.5. Can't really blame a package
    for using another package. Besides, Yesod will install with *any* version of `data-default`,
    you would just need to give `Cabal` a specific constraint to override its default
    of using the newest version.

-   `language-ecmascript`, for putting on restrictive upper bounds. This is a
    hot topic for debate in the community: should we preemptively put on upper
    version bounds, or not? But let's forget about the preemptive nature for a
    moment. What if the authors knew with certainty that their package wouldn't
    work with `data-default` version 0.5? Are we going to blame them for writing a
    package that isn't perfectly forwards compatible with all future versions of
    dependencies? Certainly not. And can we blame them for not immediately
    releasing a new version of their library once `data-default` was updated? No,
    that's not fair either.

    (I've been on the receiving end of such demands. As a package maintainer,
    it's just an impossibly high bar to try and reach, and we can't expect it of
    anyone.)

-   Cabal, for not just automatically reinstalling Yesod with `data-default`
    version 0.4. Maybe... but that might break a whole bunch of other code.
    Hermetic builds might help here, but I'm going to make a bit of a baseless
    assertion here: there's no way we could ever create a tool that can efficiently
    and correctly handle all such reinstallation cases.

-   Cabal, for installing Yesod with `data-default` 0.5 instead of 0.4. It
    should have known that I'd want to try out Fay next. I think Yoda said it
    best, with "Strong am I in the force, but not that strong."

It would seem we've run out of scapegoats... or have we?

## Blame Hackage

The real problem is that Hackage is maintaining conflicting packages! How dare
it tell Cabal about Yesod and Fay if they can't coexist. Hackage could simply
reject packages which conflict with existing dependencies, and cull existing
packages which use outdated dependencies.

Hackage has an alibi as well: it's doing exactly the job it's supposed to be
doing! Hackage says nothing about stability of code. It's a place for
developers to upload code. It doesn't have rigorous requirements for entry,
which is a __good thing__. It encourages experimentation and lets users test
out new ideas easily.

So here's the thesis of my post: __All of our tools are working correctly, but
we're using them for the wrong purpose__.

## Four levels of package stability

Let me describe four levels of stability in packages. The lines are not always
so clear-cut, and therefore it's easy to imagine in-between levels.
Nonetheless, I think this breakdown is useful.

1.  Packages that live in source control. There are no guarantees that the code is
    usable in any way, much less that it interoperates with other packages.
    There are no clearly defined version numbers either, essentially just
    meaningless SHA hashes.

2.  Individual packages that have been released as functional, but not necessarily
    guaranteed to play nice with others. The definition of "functional" is very
    much up to debate. It could mean anything from "Hey, I came up with this idea
    five minutes ago and the code compiles" to "We have a rigorous test suite."
    Each package author has his/her own definition of "good to go."

3.  A set of packages that have been vetted as working nicely together. Minimally,
    this would mean they all install; ideally, they would pass a set of
    integration tests as well. This requires efforts of some trusted group of
    people to perform this vetting.

4.  A subset of vetted, interoperable packages
    that are recommended for developers to use. This would include
    support and documentation.

Hackage is currently providing level 2 stability. Let me reiterate: this is
*exactly* what Hackage should be doing, and I don't want to change that at all.
The Haskell Platform lives at level 4, providing a small subset of known good
and working packages. But there's nothing sitting at level 3. As a result, the
job of the Haskell Platform team is much harder than it need to be, and users
looking for more power than the HP provides are thrown back into the level 2
immediately.

My claim is that for the "average developer," stability levels 3 or 4 are far
more valuable.  The remainder of this post is a description of how I believe
the community can achieve this goal.

## Get a list of target packages

The goal of this project shouldn't be to encompass the entirety of Hackage. For
one, we would almost certainly fail. There are simply cases where no resolution
could ever be achieved (e.g., `lens` requires `transformers` 3.0 while `expat-enumerator`
cannot use `transformers` 3.0). But this project will require responsive
package maintainers (as we'll see in a bit). So arbitrary packages shouldn't
just be thrown into the mix.

Instead, a developer should have to apply for a package to be included in this
set of target packages. This developer would then be the contact person if any
problems arise. I'll volunteer today as the contact on the `yesod` package, for
example.

## Try to find a compatible set of versions

Now we come back to the upper bounds issue. If we're lucky, all of the packages
included in the set of targets will work with the newest version of the
dependencies available on Hackage. But that may not always be the case. The
simplest response would be to ask maintainers to bump their dependencies. But
some complications will arise:

1.  What about transitive dependencies? In the example I started with, suppose
    that Fay was a target package. But the restrictive dependency on
    `data-default` came from `language-ecmascript`. In this case, I think it is the
    responsibility of the Fay maintainer to pursue one of the following:

    * Get the `language-ecmascript` maintainers to release an updated version
      of their package. In many cases, this will be trivial, and happily
      accepted upstream.

    * Remove the dependency on `language-ecmascript`, possibly by forking the
      package.

    * Remove Fay from the set of target packages until the situation can be
      resolved.

2.  What about massively disruptive releases? One example was the
    `transformers` 0.3 release, which still has some dependency hell remnants.
    In that case, however, upgrading was usually a simple matter of adjusting a
    `cabal` file. But a more significant example was the change (about a year ago)
    from `conduit` 0.3 to 0.4. That change required significant code rewrites, and
    therefore the transition needed to be handled smoothly.

    My recommendation would be that in both cases, the package curators give a
    deadline by which all packages must switch to the newer version of the
    dependency. The length of time given should depend on the complexity of the
    upgrade.

3.  Suppose a new version of a package is released, but is not intended to be
    widely used yet. To harp on `conduit` again, `conduit` 0.5 was released
    significantly before the rest of the `conduit`/`wai`/Yesod ecosystem was
    updated. In such a case, the developer releasing the package should be able to
    blacklist the new version for a certain amount of time until he/she decides it
    should be moved into live mode.

## Compile and test

Once an acceptable set of packages has been achieved, they should all be
simultaneously compiled, and all of their test suites run. Ideally, this would
be done on multiple operating systems and versions of GHC. Also ideally, as
this project matures, it will begin to include a large set of integration
tests.

## Snapshot

We now have a list of packages and versions which are guaranteed (to some
extent) to work together correctly. Take this information and build up a
00-index.tar file. In other words, create a fake Hackage repository (we'll see
why in the next step).

In a truly ideal world, we'd have the same kind of major/minor version
breakdown of this package set as with normal Haskell packages. The idea would
be that we'd create a snapshot and name it version 5.1. If we get some bugfixes
for a certain package, we can included that updated version and release a new
snapshot named version 5.1.1. But short-term, a simple date-based release
system would be sufficient.

## Point new users to this database

New Haskellers should then be pointed at this modified Hackage database instead
of the official Hackage database. The upshot is that it will now be impossible
to enter dependency hell.

One downside to this approach is that it greatly limits which packages you can
use. You can only install packages from the small subset of Hackage. Ideally
we'd like to allow users to install some packages from "greater Hackage"
without introducing too much dependency hell.

A possible addition we could make to achieve this goal is to have an additional
"extras" repo available, which will include all of Hackage which is not part of
the database. The important point is that we would only include a single
version of the blessed packages. So if `bytestring` 0.10.0 is in our set of
packages, then no other version of `bytestring` would be in the extras repo. By
doing so, we make it impossible for the core packages to enter dependency hell,
though the extras packages in theory could.

## Next steps

This is certainly an ambitious project, but I think it's easily within reach. I
follow a very similar procedure already for creating the
`yesod-platform` meta-package, and a lot of that code can be reused. The
coordination amongst different maintainers could be handled on Github with
branches and pull requests. And since this is just a thin layer on top of
Hackage, there's a low upfront cost.

Before we dive into this, I hope the community looks at this proposal seriously
to determine if it will solve our problems. If you think it won't, challenge
the proposal. We want to improve the state of Haskell for new developers as
much as possible.

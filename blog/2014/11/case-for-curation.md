A number of months back there was a long series of discussions around the
Package Versioning Policy (PVP), and in particular the policy of putting in
preemptive upper bounds (that is to say, placing upper bounds before it is
*proven* that they are necessary). Eventually, the conversation died down, and
I left some points unsaid in the interests of letting that conversation die.
Now that Neil Mitchell [kicked up the dust
again](http://www.reddit.com/r/haskell/comments/2m14a4/neil_mitchells_haskell_blog_upper_bounds_or_not/),
I may as well get a few ideas out there.

__tl;dr__ Stackage is simply better tooling, and we should be using better
tooling instead of arguing policy. The PVP upper bounds advocates are arguing
for a world without sin, and such a world doesn't exist.

This blog post will be a bit unusual. Since I'm so used to seeing questions,
criticisms, and misinformation on this topic, I'm going to interject commonly
stated memes throughout this blog post and answer them directly. Hopefully this
doesn't cause too much confusion.

As most people reading this are probably aware, I manage the [Stackage
project](https://github.com/fpco/stackage).  I have a firm belief that the PVP
discussions we've been having are, essentially, meaningless for the general use
case, and simply improving our tool chain is the right answer. Stackage is one
crucial component of that improvement.

*But Stackage doesn't really help end users, it's nothing more than a CI system
for Hackage.* The initial Stackage work may have counted as that, but Stackage
was never intended to just be behind the scenes. [Stackage
server](http://www.stackage.org) provides a very user-friendly solution for
solving packaging problems. While I hope to continue improving the ecosystem-
together with the Haskell Platform and Cabal maintainers- Stackage server is
already a huge leap forward for most users today. (See also: [GPS Haskell](http://www.ozonehouse.com/mark/platform/GPS-Haskell-HIW2014.pdf).)

The PVP is composed of multiple ideas. I'd like to break it into:

1. A method for versioning packages based on API changes.
2. How lower bounds should be set on dependencies.
3. How upper bounds should be set on dependencies.

Just about everyone I've spoken to agrees with the PVP on (1) and (2), the only
question comes up with point (3). The arguments go like this: preemptive upper
bounds add a lot of maintainer overhead by requiring them to upload new
versions of packages to relax version bounds regularly. (This is somewhat
mitigated by the new cabal file editing feature of Hackage, but that has its
own problems.) On the other hand, to quote some people on Reddit:

> I'd rather make a release that relaxes bounds rather than have EVERY previous version suddenly become unusable for folks

> that upper bounds should not be viewed as handcuffs, but rather as useful information about the range of dependencies that is known to work. This information makes the solver's job easier. If you don't provide them, your packages are guaranteed to break as t -> ∞.

These statements are simply false. I can guarantee you with absolute certainty
that, regardless of the presence of upper bounds, I will be able to continue to
build software written against yesod 1.4 (or any other library/version I'm
using today) indefinitely. I may have to use the same compiler version and
fiddle with shared libraries a bit if I update my OS.  But this notion that
packages magically break is simply false.

*But I have some code that built two months ago, and I opened it today and it
doesn't work!* I didn't say that the standard Haskell toolchain supports this
correctly. I'm saying that the absence of upper bounds doesn't guarantee that a
problem will exist.

Without dancing around the issue any further, let me cut to the heart of the
problem: our toolchain makes it the job of every end user to find a consistent
build plan. Finding such a build plan is inherently a hard problem, so why are
we pushing the work downstream? Furthermore, it's terrible practice for working
with teams. The entire team should be working in the same package environment,
not each working on "whatever cabal-install decided it should try to build
today."

There's a well known, time-tested solution to this problem: curation. It's
simple: we have a central person/team/organization that figures out consistent
sets of packages, and then provides them to downstream users. Downstream users
then never have to deal with battling against large sets of dependencies.

*But isn't curation a difficult, time-consuming process? How can the Haskell
community support that?* Firstly, that's not really an important question,
since the curation is *already happening*. Even if it took a full-time staff of
10 people working around the clock, if the work is already done, it's done. In
practice, now that the Stackage infrastructure is in place, curation probably
averages out to 2 hours of my time a week, unless Edward Kmett decides to
release a new version of one of his packages.

This constant arguing around PVP upper bounds truly baffles me, because every
discussion I've seen of it seems to *completely disregard* the fact that
there's an improved toolchain around for which all of the PVP upper bound
arguments are simply null and void. And let me clarify that statement: I'm not
saying Stackage *answers* the PVP upper bound question. I'm saying that- for
the vast majority of users- Stackage makes the answer to the question
*irrelevant*. If you are using Stackage, it makes not one bit of difference to
you whether a package has upper bounds or not.

And for the record, Stackage isn't the only solution to the problem that makes
the PVP upper bound question irrelevant. Having cabal-install automatically
determine upper bounds based on upload dates is entirely possible. I in fact
already implemented such a system, and sent it for review to two of the
staunchest PVP-upper-bounds advocates I interact with. I didn't actually
receive any concrete feedback.

So that brings me back to my point: why are we constantly arguing about this
issue which clearly has good arguments on both sides, when we could instead
just upgrade our tooling and do away with the problem?

*But surely upper bounds do affect some users, right?* For one, it affects the
people doing curation itself (that's me). I can tell you without any doubt that
PVP upper bounds makes my life more difficult during curation. I've figured out
ways to work around it, so I don't feel like trying to convince people to
change their opinions. It also affects people who aren't using Stackage or some
other improved tooling. And my question to those people is: why not?

I'd like to close by addressing the idea that the PVP is "a solution."
Obviously that's a vague statement, because we have to define "the problem." So
I'll define the problem as: someone types `cabal install foo` and it doesn't
install. Let me count the ways that PVP upper bounds fail to completely solve
this problem:

1. The newest release of `foo` may have a bug in it, and cabal has no way of knowing it.
2. One of the dependencies of `foo` may have a bug in it, and for whatever reason cabal chooses that version.
3. `foo` doesn't include PVP upper bounds and a new version of a dependency breaks it. (See comment below if you don't like this point.)
4. Some of the dependencies of `foo` don't include PVP upper bounds, and a new version of the transitive dependencies break things.
5. There's a semantic change in a point release which causes tests to fail. (You *do* test your environments before using them, right? Because Stackage does.)
6. I've been really good and included PVP upper bounds, and only depended on packages that include PVP upper bounds. But I slipped up and had a mistake in a cabal file once. Now cabal chooses an invalid build plan.
7. All of the truly legitimate reasons why the build may fail: no version of the package was ever buildable, no version of the package was ever compatible with your version of GHC or OS, it requires something installed system wide that you don't have, etc.

*That's not fair, point (3) says that the policy doesn't help if you don't
follow it, that's a catch 22!* Nope, that's exactly my point. A policy on its
own does not enforce anything. A tooling solution *can* enforce invariants.
Claiming that the PVP will simply solve dependency problems is built around the
idea of universal compliance, lack of mistakes, *historical* compliance, and
the PVP itself covering all possible build issues. None of these claims hold up
in the real world.

To go comically over the top: assuming the PVP will solve dependency problems
is hoping to live in a world without sin. We must accept the PVP into our
hearts. If we have a build problem, we must have faith that it is because we
did not trust the PVP truly enough. The sin is not with the cabal dependency
solver, it is with ourselves. If we ever strayed from the path of the PVP, we
must repent of our evil ways, and return unto the PVP, for the PVP is good. I'm
a religious man. My religion just happens to not be the PVP.

I'm not claiming that Stackage solves every single reason why a build fails.
The points under (7), for example, are not addressed. However, maybe of the
common problems people face- and, I'd argue, the vast majority of issues that
confuse and plague users- are addressed by simply moving over to Stackage.

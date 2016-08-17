I recently
[wrote a new Stack feature and blogged about it](https://www.fpcomplete.com/blog/2015/09/stack-pvp). The
feature is about adding support for
[PVP bounds](https://wiki.haskell.org/Package_versioning_policy) in
cabal files. I did my very best to avoid stirring up anything
controversial. But as is usual when the PVP comes up,
[a disagreement broke out on Reddit about version bounds](https://www.reddit.com/r/haskell/comments/3ls6d4/stack_and_the_pvp/cv92v6k). This
essentially comes down to two different ways to view an upper bound on
a package:

* We've tested, and know for certain that the new version of a
  dependency is incompatible with our package
* I can't guarantee that any new versions of a dependency will be
  compatible with our package

If you look through the history of PVP debates, you'll see that this
argument comes up over and over again. I'm going to make a bold
statement: if a core feature to how we do package management is so
easily confused, there's a problem with how we're using the feature. I
made an offhand comment about this on Twitter:

<blockquote class="twitter-tweet" lang="en"><p lang="en" dir="ltr">Instead of cabal file version ranges, we should have a set of &quot;built with&quot; versions per package. Let tooling generate and interpret the data</p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/646892537601351680">September 24, 2015</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

Based on the positive feedback to that tweet, I'm moving ahead with
making a more official proposal.

## How PVP bounds work today

Here's the theory of how you're supposed to write PVP bounds in a file:

* Test your package against a range of dependencies. For example,
  let's say we tested with text-1.1.2 and text-1.2.0.3
* Modify the build-depends in your .cabal file to say `text >= 1.1.2
  && < 1.3`, based on the fact that it's known to work with at least
  version 1.1.2, and unknown to work on anything than major version
  1.2.
* Next time you make a release, go through this whole process again.

PVP detractors will respond with a few points:

* You don't know that your package won't work with text-1.1.1
* You don't know that your package won't work with text-1.3
* You don't know for certain that your package will work with
  text-1.1.3, text-1.2.0.0, or text-1.2.1 (yes, it _should_ based on
  PVP rules, but mistakes can happen.
* Repeating this testing/updating process manually each time you make
  a code change is tedious and error-prone.

## Collect the real information

If you notice, what we did in the above was extract the cabal file's
metadata (version bounds) from what we actually know (known versions
that the package works with). I'm going to propose a change: let's
capture that real information, instead of the proxy data. The data
could go into the cabal file itself, a separate metadata file, or a
completely different database. In fact, the data doesn't even need to
be provided by the package author. Stackage Nightly, for instance,
would be a wonderful source of this information.

A dependency solver - perhaps even cabal-install's - would then be
able to extract exactly the same version bound information we have
today from this data. We could consider it an intersection between the
.cabal file version bounds and the external data. Or, we could ignore
.cabal file bounds and jump straight to the database. Or we could even
get more adventurous, e.g. preferring known-good build plans (based on
build plan history).

In theory, this functionality - if done as a separate database from
the .cabal files themselves - means that on-Hackage revisions would be
much less important, possibly even a feature that no one needs in the
future.

## Automation!

And here's the best part: this doesn't require authors to do
anything. We can automate the entire process. There could even be
build servers sitting and churning constantly trying to find
combinations that build together. We've seen already how difficult it
is to get authors to adopt a policy. The best policy is one that can
be automated and machine run.

## Downsides

Problems I've thought of so far:

* Some packages (notably Edward Kmett's) have a versioning scheme
  which expresses more information than the PVP itself, and therefore
  the generated version bounds from this scheme may be too strict. But
  that won't necessarily be a problem, since a build server will be
  able to just test new versions as they come out.
* This very blog post may start a flame war again, which I sincerely
  hope _doesn't_ happen.

## Adoption

In order for this to really happen, we need:

1. Broad support for the idea
2. Changes to the cabal-install dependency solver (or an alternate
   replacement)
3. Central infrasturcture for tracking the build successes
4. Tooling support for generating the build success information

And to be blunt: this is not a problem that actually affects me right
now, or most people I work with (curation is in a sense a simplified
version of this). If the response is essentially "don't want it,"
let's just drop it. But if people relying on version bounds today
think that this may be a good path forward, let's pursue it.

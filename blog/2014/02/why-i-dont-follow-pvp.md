I've been considering writing this blog post for a while, but a [recent Haskell
libraries thread](http://www.haskell.org/pipermail/libraries/2014-February/022114.html) finally put me over the top. Note: I'm only
going to discuss the [Package Versioning Policy
(PVP)](http://www.haskell.org/haskellwiki/Package_versioning_policy) here, and
ignore the other topic from that mailing list thread (identifier naming), as I
consider that a very separate topic, and one which I'm not nearly as passionate
on.

Let me start off with my qualifications for discussing this subject:

* I actively maintain [over 100 packages](http://packdeps.haskellers.com/feed?needle=snoy) on Hackage.
* I maintain the [Stackage](https://github.com/fpco/stackage) project, and therefore have firsthand experience with package compatibility issues.
* I [manage a large Haskell application](https://www.fpcomplete.com/), and have managed other such applications in the past.

So I consider myself both a library author and end user, and due to Stackage
have gotten to see a lot of interdependencies between packages. My very short
summary is:

__The PVP doesn't work.__

The slightly longer summary is:

* PVP places a large maintenance burden on library authors.
* PVP optimizes for preventing the wrong kind of build issues.
* The usual reason for users to demand the PVP is the result of bad development practices.

## Bad development practices

Let me start with that last point, because it's the easiest to address. The
situation goes as follows: a user puts together an application, which is not
going to be released to Hackage. He works on this codebase on January 1. He
writes his code to use the foo package version 1.0, which in turn depends on
bar version 1.0.  He builds the code, tests the code, everything works, and he
deploys it.

Sometime during the month of January, a new breaking version of bar is released
(say 2.0). foo 1.0 does not compile with this version of bar. So when our user
comes back in February and tries to rebuild his application, it doesn't work.
"If only the foo maintainer had followed the PVP and put an upper bound on bar,
this never would have happened," he laments.

While true, this is ignoring the true flaw: our user allowed his production
codebase to depend on the development practices of a third party. This is a
completely flawed practice. What happens if there's an updated version of foo
(say 1.0.0.1) which accidentally introduces a bug? Or fixes buggy behavior that
the user depended on? Or simply changed the functionality of the library
without any change to the API?

None of these are good, and the maintainer for foo should try very hard to
avoid them. But the fact is that developers are human, and humans make
mistakes. And trusting the build integrity of your code to the random actions
of a third party that you have no control over is not responsible development.

At FP Complete, we have a file listing the exact versions of all our
dependencies. Occassionally, I'll go through that file and update versions for
important bug fixes, new features, or performance improvements we need. But
it's a manual process which I consciously take part in. I make certain that we
run a full automated test suite, and that we perform manual testing before
deploying that code to production.

Said another way: if you're using the PVP to make sure your production code
builds, you've done nothing at all to make sure that the built code actually
does what you think it does. If you've written and tested your code against
version 1.0.0 of foo, your build system shouldn't automatically upgrade you to
version 1.0.0.1, even if the PVP says that this is a perfectly safe operation.

My standard link for this topic is [Greg Weber's blog post about Docmunch's
approach](http://blog.docmunch.com/blog/2013/haskell-version-lockdown). It
would be wonderful if this version locking functionality could be included in
cabal in the future.

## Build compatibility: a numbers game

The PVP is designed to ensure that no two incompatible packages will ever have
an install attempted. Even this goal isn't fully realized by the PVP: there are
holes regarding re-exports for upstream dependencies and typeclass instances.
(I can't go into more detail on these if people are interested.)
But let's pretend for the moment that the PVP works perfectly for its given
target, and further assume that no one ever makes any PVP-related mistakes.

Let's talk about the famed diamond dependency issue, both with and without the
PVP. Suppose the following development trend:

1. foo-1 is released.
2. bar-1 is released, depending on foo 1.
3. foo-2 is released, which breaks backwards compatibility in some way.
4. baz-1 is released, which depends on features only present in foo-2 (i.e., not in foo-1).
5. A user tries to write a project that users bar and baz.

In the world of strict PVP adherence, bar-1 will have a strict upper bound on
foo, and therefore will not allow itself to be installed with version 2 of foo.
But baz *requires* version 2 of foo, and therefore there's no way to get a
build plan. cabal will immediately fail.

In the non-PVP world, bar-1 would have no upper bound on foo. Therefore, cabal
will attempt to install foo-2, bar-1, and baz-2. Will this succeed? Maybe,
maybe not. foo-2 broke backwards compatibility in some way, but __there's no
guarantee that this broke bar-1__. And based on my Stackage experience, the
vast majority of the time the code will continue to compile and run without
issue.

So in the diamond dependency case, we have two possibilities. In the PVP world,
we're guaranteed that no compilation can occur. In the non-PVP world, we can
try to compile, and in many cases the compilation will succeed. In my eyes,
that's a clear win for dropping the PVP.

On the other hand, let's talk about a use case which gives the PVP an advantage:

1. foo-1 is released.
2. bar-1 is released, depending on foo 1.
3. foo-2 is released, which breaks backwards compatibility in some way.
4. baz-1 is released, __and will work with either foo-1 or foo-2__.
5. A user tries to write a project that users bar and baz.

In this case, the PVP-adhering system will select version 1 of foo, which is
guaranteed to work for both bar and baz. So you get a good build for certain.
In the non-PVP system, cabal will attempt to use foo-2, which may or may not
work, depending on how backwards-incompatible the change in foo was. Advantage:
PVP.

The questions are:

* Which of these scenarios comes up more often in real life?
* Which of these scenarios requires more effort from package maintainers to fix?
* Which of these most negatively impacts users?

I can only argue from experience on that first question. The vast majority of
backwards-incompatible changes do __not__ break their
downstream dependencies. For example, can anyone provide a single example of a
package that was broken by the text 1.0 and 1.1 releases? I know of none. But
[I opened up an issue three months
ago](https://github.com/fpco/stackage/issues/153) about text 1.0, and there are
*still* dependencies that have to be updated. And as soon as a single package
is released which depends on a new text 1.0 feature, users will need to make a
choice between using that new package or continuing to use existing,
PVP-adhering packages.

From a maintainers perspective, I can again relate personal experiences, and
I'd encourage others to do so as well. When all of my packages followed the
PVP, a new release of a package like text could easily require a full day of
changing cabal files, recompiling, discovering an upstream dependency with a
strict upper bound, reporting to upstream, sending pull requests, and other
such nonsense. So in these non-breaking circumstances, the maintenance time for
PVP is __huge__.

What about the other side, where a new version *does* break a package? In the
non-PVP world, I need to release a new version of the package which either (1)
adds in an upper bound, or (2) makes the package compatible with the new
version. The maintenance time involved in (1) is trivial, but (2) can take some
time. But that isn't a PVP issue: maintaining compatibility with multiple
versions of dependencies is a valuable feature of a library, and is not
affected at all by adherence to the PVP.

So maintenance wise, the difference comes down to: occassionally patch a
library to add in an upper bound, versus having to update every dependent
package each time a breaking change is made. IMO, the advantage is clear:
non-PVP.

From the user perspective, the PVP provides the (partial) guarantee that typing
`cabal install X` will just work. In the non-PVP world, builds will fail more
often. On the flip side, a user trying to combine different packages is more
likely to run into diamond dependency problems with the PVP.

But user problems can be mitigated through other means than the PVP:

* As I started off with, make sure you pin down the exact versions of your
  dependencies when doing a build.
* Library maintainers need to ensure that their packages compile with the latest versions of their dependencies. Doing this manually is a huge pain. But this is *exactly* the benefit of adding your package to [Stackage](https://github.com/fpco/stackage): there's an automated system that will test your code for you.
* This is a bigger request, but I'll raise it anyway: target more backwards compatibility! If you have a function `foo` that takes two parameters, and you realize you'd like the option to pass in a third, don't change `foo`, just add a separate function and deprecate `foo`. If you think this is likely to happen more often, use [a settings type](http://www.yesodweb.com/book/settings-types).

## Non-upgradeable and mostly-stable packages

There's one issue that repeatedly causes huge amounts of issues: upper bounds
on non-upgradeable packages (e.g., base). Imagine I'm writing a package, and have tested it against base
version 4.6. So, like a good PVP follower, I put in my cabal file `base >= 4.6
&& < 4.7`. Let's see the result of this:

* Let's assume my code really doesn't work with base version 4.7. Then users
  will get an error message from cabal about version numbers and exit. Without
  the version bounds, users would get an error message explaining why the code
  doesn't compile with base version 4.7.
* And if my code really does work with base version 4.7, then users get a cabal
  error message for no reason!

The problem here is that there are a number of library (base, template-haskell)
which come with GHC and cannot be changed. If a package has a version bound on
one of those packages, there's nothing cabal can do to try and resolve it. Its
only recourse is to try and install a different version of the package. Again,
from experience, there is rarely an older version of package `foo` which works
with base 4.7 when the newer version of `foo` does not.

The other related issue is mostly-stable packages. The PVP draws no distinction
between a package like `bytestring`, which is highly stable, and experimental
packages. For an experimental package,
there's a lot more logic to including upper bounds, as the numbers game I
describe above is skewed. But consider the code below:

```haskell
import qualified Data.ByteString as S

foo = S.ByteString -> S.ByteString
foo = S.map (+ 1)
```

When you talk about strict PVP adherence, you need to assume that the next version of bytestring may:

* Rename Data.ByteString to something else.
* Rename the type ByteString to something else.
* Change the name or type signature of `map`.

I'd be willing to wager that none of these will ever happen. Does anyone see a
world where this isn't the case? My point here is: let's not be automatons
following some policy because "the Haskell wiki says so." Use your judgement:
if you import `Data.Text` to use the `Text` data type in a signature, and rely
on its `Monoid` instance, don't put an upper bound on text. I'd be willing to
wager quite a bit that text will never break those assumptions.

## Summary

The PVP solves one problem pretty well (not perfectly), but at huge costs.
There are other solutions to reducing the problems we're facing, and we should
not fanatically adhere to a policy like the PVP. Many of us who have dropped
PVP compliance have done so not because we're "sloppy" or "don't care about
compatibility", but becasue we believe the PVP causes more harm than good. I
encourage others to consider making the switch as well.

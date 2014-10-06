Haskell's Prelude is changing to favor using Foldable/Traversable instead of just lists.
Many Haskellers are concerned that upcoming changes to the Prelude could

* break existing code
* make maintaining code more difficult
* decrease beginner friendliness

Lets discuss these concerns


## Stability and the Prelude design space

[Neil Mitchell writes](http://neilmitchell.blogspot.co.uk/2014/10/why-traversablefoldable-should-not-be.html):

```
Step 3: Incorporate feedback

I expect that will result in a significant number of revisions, and perhaps significant departures from the original design. Note that the classy-prelude package is following the steps above, and has had 38 revisions on Hackage so far.
```

As a contributor to and user of classy-prelude, I wanted to point out something about this statement. Most of these revisions are minor and backwards compatible consisting of bug-fixes or something like adding a non-default implementation of a typeclass method or an additional typeclass instance. A better data point is the number of major revision releases. classy-prelude is at release 0.10 now, so that would be 10.

Neil [mentions classy-prelude a second time](http://neilmitchell.blogspot.it/2014/10/how-to-rewrite-prelude.html):

```
The classy-prelude work has gone in that direction, and I wish them luck, but the significant changes they've already iterated through suggest the design space is quite large.
```

Of these 10 changes, I would only consider one to be major and represent any kind of change in the design space: the [0.6 release basing classy-prelude on the mono-traversable package](http://www.yesodweb.com/blog/2013/09/classy-mono).
Some of the version bumps represent a change in code organization between mono-traversable and classy-prelude. One change that required a bump is [a change to the type signature of mapM_](
http://www.yesodweb.com/blog/2014/05/foldable-mapm-maybe-recursive) that should be made in the Prelude but probably never will because it will break existing code.
The major change in the 0.6 release is very similar to the upcoming changes in the Prelude, except that classy-prelude (via mono-traversable) works on monomorphic structures such as Text in addition to polymorphic structures.
So I would not consider the design space to be large for classy-prelude or for other prelude replacements.
classy-prelude before 0.6 and most other Prelude replacements or type-class conveniences have not worked out very well.
There is only 1 design that has worked well (incorporating Foldable and Traversable), and that is the design being incorporated into the new Prelude.

Neil also writes about other Haskell abstractions:

```
We already have that problem with the Control.Arrow module, which (as far as most people use it), is just a pile of tuple combinators. But unlike other tuple combinators, these are ones whose type signature can't be understood. When I want to use &&& or ***
```

I want to point out that classy-prelude solved some of this issue by [exporting Data.BiFunctor](https://github.com/snoyberg/classy-prelude/commit/cdf041f005ead464bee1ddc48e3f9e0096080cf7) instead of functions from Data.Arrow, which required a version bump from 0.9.x to 0.10. I also want to point out that these kinds of arguments are straw man arguments. Every proposed abstraction to Haskell has its own advantages and dis-advantages. Because of the need for backwards compatibility, we are going to be able to point to abstractions in the Prelude that are not being used in the right way for a long time. However, Foldable/Traversable is the only serious contender for abstracting over data structures in Haskell. It has stood the test of time so far, but it has not seen a lot of use yet because everyone is initially directed to just use lists for everything, and the next instinct when using other data structures in the current environment is to use qualified names.


## Iterating the new design

One proposed remedy for dealing with change is trying to release the new Prelude in an iterative way.
This could be a good idea, but in practice it is very difficult to implement: most users are still going to `import Prelude` rather than trying out something different and giving their feedback.
A better approach than holding it back is to use a design that makes it easier for new releases to make backwards incompatible changes.
One approach to this could be at the package level the way that `base-compat` operates.
Another approach that could be useful to library authors is incorporate versioning at the module level.

Something to keep in mind though is that because the new Prelude needs to try to work with the old Prelude,
there are not that many options in the design space.
classy-prelude has had the luxury of being able to re-think every Haskell wart.
So it was able to remove all partial functions and use Text instead of String in many places.
But that process is very difficult for the actual Prelude, which is severly constrained.



## Why change? Module qualified names and generic code.

The motivation for classy-prelude was to confront one of Haskell's most glaring warts: name-spacing and the need to qualify functions.
We could certainly have our IDE automatically write import statements, but we still end up with needing to use module qualified names.
This isn't really an acceptable way to program.
I have not seen another language where this extra line noise is considered good style.
For Haskell to move forward and be as convenient to use as other programming languages, there are 2 solutions I know of.

1) change the language
2) make it convenient to write generic code

Changing the language so that module qualification is not needed is arguably a much better approach.
This is the case in Object-Oriented languages, and possible in languages very similar to Haskell such as Frege that figure out how to disambiguate a function based on the data type being used.
I think this would be a great change to Haskell, but the idea was rejected by Simon Peyton Jones himself during the discussion on fixing Haskell records because it is not compatible with how Haskell's type system operates today. Simon did propose [Type directed name resolution](https://ghc.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) which I always though was a great idea, but that proposal was not able to get off the ground in part because changing Haskell's dot operator proved too controversial.

So the only practical option I know of is to focus on #2.
Being able to write generic code is an important issue in of itself.
Programmers in most other mainstream languages write code that operates on multiple data structures of a particular shape,
but Haskell programmers are still specializing a lot of their interfaces.


## Lists are holding Haskell back

It is taken by many to be a truism that programming everything with lists makes things simpler or at least easier for new Haskell programmers.
I have found this statement to be no different than 99% of things given the glorious "simple" label: the "simplicity" is not extensible, does not even live up to its original use case, and ends up creating its own incidental complexity.

I used to frequently warp the functions I wrote to fit the mold of Haskell's list.
Now that I use classy-prelude I think about the data structure that is needed. Or often I start with a list, eventually discover that something such as appending is needed, and I am able to quickly change the function to operate on a different data structure.

Using an associative list is an extreme example of using the wrong data structure where lookup is O(n) instead of constant or O(log(n)).
But by warping a function I am really talking about writing a function in a way to reduce list appends or doing a double reverse instead of using a more natural [DList](http://hackage.haskell.org/package/dlist) or a [Seq](http://hackage.haskell.org/package/containers/docs/Data-Sequence.html).
This warping process probably involves performing recursion by hand instead of re-using higher-order functions. 
As a library developer, I would like to start exposing interfaces that allow my users to use different data structures, but I know that it is also going to cause some inconvenience because of the current state of the Prelude.

Neil writes that he had an opposite experience:

```
I have taken over a project which made extensive use of the generalised traverse and sequence functions. Yes, the code was concise, but it was read-only, and even then, required me to "trust" that the compiler and libraries snapped together properly.
```

This kind of report is very worrying and it is something we should take very seriously.
Any you certainly cannot tell someone that their actual experience was wrong.
However, it is human nature to over-generalize our experiences just as it was the nature of the code author in question to over-generalize functions.
In order to have a productive discussion about this, we need to see (at least a sample or example of) the code in question.
Otherwise we are only left guessing at what mistakes the author made.

In general I would suggest specializing your application code to lists or other specific structures (this can always be done with type signatures) until there is a need for more abstraction, and that could be a big part of the problem in this case.

It would be really great to start having these discussions now based off of actual code examples and to have a community guide that explains the missing common sense for how to use abstractions appropriately.

The uncertainty discussed here is the heart of the matter, and talking about what is good for beginners is largely a distraction.


## Haskell is for programmers first, students in their first class in functional programming second

This might sound hostile to beginners. In fact, the opposite is the case!
The programming languages that are taught to beginners are the very same ones that are used in industry.
The first programming language taught to me at school was C, and it was not because it was optimized out of the box for beginners.

So the way to attract more beginners is simply to become more popular with industry.
Haskell has already reached the growth rate limit of a language that is popular for the sake of learning about programming.

That being said, we do need to do a lot of things to make the experience as nice as possible for beginners, and an alternative prelude for beginners could be a great idea.
But making this the *default* that holds back progress hurts everyone in the long-run.


## It isn't Beginner vs. Expert anyways

The [most up-voted comment on Reddit](http://www.reddit.com/r/haskell/comments/2icjmf/how_to_rewrite_the_prelude/cl0xdyr) states:

```
What other languages have different standard libraries for people learning and people not learning? What could be a greater way to confuse learners, waste their time and make them think this language is a joke than presenting them with n different standard libraries?
```

I will add my own assertion here: Haskell *is* confusing today because the Prelude is in a backward state that no longer reflects several important best practices (for example, Neil had to create the Safe package!) and it does not hold up once you write more than a trivial amount of code in your module.

We also need to keep in mind that using Haskell can be difficult for beginners precisely for some of the same reasons that it is painful for experts.
And the same reason these new changes will be more difficult for beginners (mental overhead of using the Foldable/Traversable abstraction instead of just lists) will also create difficulties for non-beginners.

So the changes to the Prelude are going to make some aspects a better for beginners or existing users and others harder.

If we really want to improve Haskell for beginners we need to stop creating a false dichotomy between beginner and expert.
We also need to empower committees to make forward progress rather than letting minority objections stall all forward progress.


## Improving the library process

Some have expressed being surprised to learn about what is going on in the Haskell libraries committee at a late stage.
On the other hand, I doubt that hearing more objections earlier would actually be helpful, because the libraries process has not learned from GHC.

Take a look at the extensive documentation around proposed changes to improve Haskell's [record system](https://ghc.haskell.org/trac/ghc/wiki/Records).
Creating a solution to Haskell's problem with records was a very difficult process.
There were several designs that looked good in a rough sketch form, but that had issues when explored in thorough detail on the wiki.
More importantly, the wiki helped summarize and explain a discussion that was extremely laborious to read and impossible to understand by looking through a mail list.

Before creating a non-minor change to GHC, there is a convention of creating a wiki page (certainly it isn't always done).
At a minimum there is a Trac ticket that can serve a somewhat similar purpose.

My suggestion is that the libraries process use the existing GHC or Haskell wiki to create a page for every non-minor change.
The page for Foldable/Traversable would explain

* what is being changed
* which changes create a breakage
* how type errors have changed
* how library code is affected
* how user code is affected
* best practices for using Foldable/Traversable 

Right now we are stuck in a loop of repeating the same points that were already made in the original discussion of the proposal.
Given a wiki page, Neil and others could point out the down-sides of the proposal with actual code and have their voice heard in a productive way that builds up our body of knowledge.

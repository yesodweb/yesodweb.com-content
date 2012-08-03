First a message from our sponsors: Yesod 1.1 is fully tested and ready for
release. I'm holding off until the beginning of next week, since I hate making
major releases on a Friday (if there are any bugs, I won't be around on
Saturday to fix them). If you're really eager to check it out, you can get the
latest and greatest from Github.

------------------------

I promised to write this blog post a week or two ago, but never got around to
it. Dan Burton reminded me about it yesterday... so here's paying off my debt
:).

So far, most of the discussion around
[classy-prelude](http://hackage.haskell.org/package/classy-prelude) has focused
around the "classy" bits. As Felipe mentioned a few weeks ago, there's a lot
more going on than just the classy parts. So I'd like to break open the
discussion more and explain what exactly is going on.

Before we start, I think I need to give a bit of an explanation of my
development strategy, as I think some people don't understand my approach. When
attacking a problem for the first time, I throw in the kitchen sink. Many
people prefer to take a more iterative approach, adding features one step at a
time after a lot of careful testing. And in many cases, I think that's the only
valid approach.

But for a project like `classy-prelude`, I'm interested in being much more
experimental. That means I'm happy to throw in some half-baked ideas to see if
they work. Don't mistake that as me saying I fully endorse and recommend this
approach. On the contrary, as I hope this post makes clear, I'm highly
skeptical of some the choices I made. But I'm also a firm believer that we
won't know if the choices were bad unless we try them out.

So without further ado, let's break down the `classy-prelude`, starting with
the "obvious good" category and moving further into the unknown. (And I think
it goes without saying that statements like "obviously good" are subjective.)

## Win: Full compatibility with standard libraries

I saw the contrary claim made many times in various discussions, so let's put
it to rest. Unlike other prelude replacements, `classy-prelude` does *not* try
to fix flaws in the base package. `Functor` is not a superclass of `Monad`,
`fail` still exists, and `Num` is unchanged. That's not to say that I think
that the base package did everything correctly (I don't), but rather, by
sticking to the standard typeclasses, `classy-prelude` code should work with
any existing library without a hitch.

From a user perspective: you should be able to use `classy-prelude` in one
module, ditch it in another, depend on some packages that use it and others
that don't, without any problem. In fact, I've done exactly that in some of my
code. If you use `classy-prelude` in your library, your library users should
not be affected beyond the fact that they have to install `classy-prelude`.

As an aside, I'd like to see some of those bigger changes make it into base
ultimately, and I think an alternate prelude is a great way to play around with
the ideas. But such an alternate prelude wouldn't be much use for real-world
use, which is why I haven't taken that route with `classy-prelude`.

## Win: Don't export partial functions

I really wish `Prelude` didn't export `head`, `tail`, and a bunch of other
partial functions. So `classy-prelude` doesn't. More generally, I've been
taking a whitelist approach to which `Prelude` functions are exported. If I've
left out one of your favorite functions, it's likely because I just didn't get
around to it yet, not due to any hatred of it. If there's some total function
in `Prelude` that you'd like added, just send a pull request.

## Win: Export commonly used functions from other modules

I prefer using `<$>` to `fmap` in most circumstances. I like to use `first` and
`&&&`, and a huge number of my modules import `mapMaybe`. All of these
functions/operators have names that are well recognized in the community, I
believe are unambiguous, and are very generally useful. Since the main purpose
of `classy-prelude` is to save you from extra keystrokes, let's just export
them all by default.

The selection here is clearly going to be pretty opinionated. And the list
isn't really that exhaustive right now (again, pull requests welcome). But
while we can argue over exactly *which* functions should be exported, I think
this is a pretty solid advantage.

## Win: Export datatypes

How many modules have you written with lines looking like:

    import Data.ByteString (ByteString)

And how many times do you just import `Data.ByteString` qualified and use
`S.ByteString`? How often is `ByteString` imported from `Data.ByteString.Lazy`
instead? And do you make the alias for the module `S` or `B`?

These are all thoroughly uninteresting questions. Typing out the code is
boring. And trying to remember which convention is used in the particular
module you're working on is just a pain. So in `classy-prelude`, we have a
simple convention: export the datatype. If there's a lazy variant, prefix it
with `L`. So we end up with `ByteString`, `LByteString`, `Text`, `LText`,
`Map`, `HashMap`, and so on.

There's nothing earth-shattering here. It just removes an annoyance.

## Probably a win: Generalize using existing typeclasses

I like `Monoid` a lot. I think it's a great typeclass. And I think it's a shame
that in `Prelude`, `concat` only works for lists and not arbitrary `Monoid`s.
Similarly, I wish that `++` was just `mappend`. So in `classy-prelude`, I've
done just that.

This is a bit controversial, because it can make error messages a bit more
confusing. But let me reinforce something that I probably didn't clarify
enough: `classy-prelude` is *not* intended for beginners. I don't think that a
Haskell tutorial should be using it for examples. `classy-prelude` is intended
for Haskellers who are already battle-tested with GHC's error messages, and
won't shy away from some more general types.

One change I didn't make, but was considering, was using `.` and `id` from
`Control.Category`. I'm not sure how much of a practical benefit that would
provide people, while making error messages likely much more complex. But if
people want that change, let's discuss it.

## More debatable win: recommend better datatypes

I've gone on record many times saying that I dislike `type FilePath = [Char]`.
[system-filepath](http://hackage.haskell.org/package/system-filepath) is
(yet another) wonderful package by John Millikin, and I use it extensively in my
personal code. (It's also making its way more solidly into the Yesod ecosystem,
though we frankly don't do that much filesystem access in Yesod.)

In `classy-prelude`, I export `system-filepath`'s `FilePath` type. This is
likely the most "breaking change" we have, though it doesn't really prevent you
from continuing to use `[Char]` for all your filepaths. `classy-prelude` also
exports a bunch of the functions and operators for manipulating filepaths, like
`basename` and `</>`.

Again, this is an opinionated decision. But I think moving over to
`system-filepath` could be a huge win for the Haskell community in general, and
regardless of `classy-prelude`'s future, I hope it catches on.

## More controversial: create a bunch of new typeclasses

Now we get to the fun stuff. `classy-prelude` defines a bunch of new
typeclasses for functions which are commonly imported qualified. I mentioned it
previously, but I don't think my point got across, so I'll say it again. The
purpose here has *nothing* to do with equational reasoning. This is purely a
technique for name overloading, nothing more.

Let's expand on that a bit. One of the nice things about typeclasses is that
they are usually associated with a set of laws that define their behavior. This
lets us write code against a typeclass and know, based on the laws, how it will
behave. We don't need to know if we're dealing with a list of a `Set`, we know
that `mappend empty foo` is the same as `foo`.

That's not my purpose here. The purpose is purely about name overloading. I
haven't stated a semantics for what `lookup` needs to do, because the answer is
*it depends on the container*. This means that `classy-prelude` is not
decreasing the cognitive load of the programmer in any meaningful way. If there
are semantic differences between the `insert` functions for a `HashMap` and a
`Map`, you still need to know about them. (To my knowledge, no such difference
exists.)

So let's say it again: the *only* purpose for this technique is to allow name
overloading.

Based on this, I think it's fair to claim that `classy-prelude` doesn't make
code more buggy due to its lack of associated laws. The libraries you're using
*already* don't conform to a set of laws. `classy-prelude` is just making it
easier to use them. If you write type-generic code and try to use it for
multiple container types (not a practice I recommend), it's possible that
you'll get buggy code due to semantic differences. But the same applies if you
write a function using `Data.Set`, copy-paste it, and then replace `Data.Set`
with `Data.HashSet`.

All that said, I'm not saying that a set of associated laws isn't a good thing.
I'd love to add them. And if `classy-prelude` continues, I'm sure we will add
them. But for the initial proof-of-concept experimental release, I don't think
it was worth the investment of time.

One final idea here is to leverage some existing collections of typeclasses
(Edward Kmett's [reducers](http://hackage.haskell.org/package/reducers) package
in particular) instead of defining our own typeclasses. Again, for the
proof-of-concept, that idea was premature, but going forward I'm hoping to look
into it. There are obstacles we'd have to overcome (like ensuring good
performance), but I don't think anything is insurmountable.

## Very controversial: make those typeclasses work with `conduit`

I was surprised not to see as much of a discussion about this point as the
previous one. In my mind, this was by far the most controversial choice in
`classy-prelude`. It's fair to say that I put it in more out of curiosity if it
would work than anything else.

Let's take `filter` as an example. A relatively simple typeclass to model it
would look like:

    class CanFilter container element | container -> element where
        filter :: (element -> bool) -> container -> container

This would work for lists, `ByteString`, `Set`, and so on. However, here's the
(simplified) signature of `filter` from `Data.Conduit.List`:

    filter :: (i -> Bool) -> Conduit i m i

This doesn't fit in with our `CanFilter` class above. `CanFilter` has a
function that takes two arguments, whereas in `conduit` `filter` has just one
argument.

So here's the trick. We recognize that the first argument is the same (a
predicate), and just leverage currying. Recognizing that `CanFilter`'s `filter`
can be viewed as a function of one argument returning a function, we split it
up into two typeclasses:

    class CanFilter result element where
        filter :: (element -> bool) -> result
    class CanFilterFunc container element | container -> element where
        filterFunc :: (element -> bool) -> container -> container

And then we define a single instance of `CanFilter` that uses `CanFilterFunc`
for all cases where we return a function:

    instance (container ~ container', CanFilterFunc container element)
      => CanFilter (container -> container') element where
        filter = filterFunc

(If you're curious about the equality constraint, see [this excellent Stack
Overflow
answer](http://stackoverflow.com/questions/11553705/haskell-equality-constraint-in-instance).)

We can then separately define an instance for `conduit`, which does not overlap
at all, and allows us to reuse the name `filter` in significantly different use
cases. Frankly, I think it's pretty cool that we have this kind of flexibility
in the type class system. But from a practical standpoint, I'm not sure it's
really a great trade-off:

*   Since the two `filter` concepts work fairly differently, it may be more
    confusing than anything else to lump them together.

*   Error messages get significantly more confusing.

*   I don't think that `conduit` usage in this sense is prevalent enough to
    warrant the costs.

In other words, if there's something I'd want to cut out first from
classy-prelude, it's the gymnastics which it pulls to accomodate `conduit`
instances. It was definitely a fun part of the experiment, and I'm glad to have
tested it. If anyone has an opinion either way on this, let me know.

## Moving forward

There are a number of minor bugs in the typeclass definitions in
`classy-prelude`, requiring more explicit type signatures than should be
necessary in some cases. Those kinds of things are easily fixed. If people find
specific examples, please bring them to my attention.

Dan Burton started a project called
[ModularPrelude](https://github.com/DanBurton/modular-prelude/tree/master/ModularPrelude)
which takes a different approach to the namespace issue, replicating first
class modules via the record system. I think it's definitely an interesting
approach, and think it can coexist very well with `classy-prelude`. But it's
not enough for my taste: I prefer the relative terseness of typeclass-based
code.

However, putting that project together with my points in this post, I think
it's important to note that there is a very large part of `classy-prelude`
which has nothing to do with typeclasses. Dan called this `BasicPrelude.hs`,
and I think it makes a lot of sense to provide that separately (though at least
for now, I'll keep it in the same package for convenience).

The idea is simple: I think a lot of people agree with me that some major
aspects of this prelude are no-brainers, and would like to use them. People are
understably more wary of the experimental bits. (As I've said before, so am I:
I won't allow `classy-prelude` to be used in `yesod` in its current state.) So
let's create a stable basis, and encourage experimentation. As I mentioned at
the beginning, since we have full compatibility with `base`, there's no real
concern of fragmentation.

Once I remove the `conduit` aspects of the library, I think it will open it up
for much better analysis of typeclass laws. It might be possible to completely
drop a number of typeclasses and use the `reducers` versions instead. For
others, more experimentation might be necessary.

And as usual: feedback is welcome, especially the constructive kind :).

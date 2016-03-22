This blog post came out of some discussions I had with my coworkers at FP
Complete. I realized I have a number of reasons I prefer working with a
typeclass based libraries (such as `classy-prelude`) that I've never put down
in writing anywhere. I decided to write up this blog post to collect all of my
thoughts in one place.

This same FP Complete discussion is also what fueled my Tweet on the subject:

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Would you like a Haskell typeclass that provided a Map-like interface compatible with Map, HashMap, and IntMap?</p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/709700807344713728">March 15, 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

## Common practice

This is hardly a strong argument, but I thought I'd get this out of the way
first. It's common practice in many other languages to program to abstract
interfaces instead of concrete types. Java is likely the best example of this,
though Python's duck-typing, Go's interfaces, and C++ templates are good
examples too. In most language ecosystems, a culture of programming against
something abstract took over a while ago.

Based on the fact that I'm a Haskell developer, I obviously don't put too much
faith in the best practices of Java, Python, Go, and C++. Nonetheless, the fact
that this pattern is repeated in many places, it's worth taking the experiences
of others into account when making choices for ourselves.

Also, choosing a programming paradigm already familiar to people from other
languages can help with language adoption. This isn't just idle speculation: I
_have_ heard new Haskellers express confusion about the lack of abstract
interfaces for some common datatypes.

## Uniform interfaces

One of the nicest things about writing code against well-designed libraries is
that the ideas, intuitions, and names translate between them. For example,
whether I'm working with a list, `Vector`, `Seq`, or `ByteString`, I know that
I can use `foldr` over the contents of the container. I don't have to wonder if
the author of the library decided to call the function `reduce` instead, or
changed the order of the arguments to the `foldr` function. My gut instincts
about this function carries through.

Unfortunately, that statement does not work universally. I chose `foldr` as an
example of a good citizen, but there are many other functions which are not so
uniform:

* The `mapM_` function is not provided by the `Data.ByteString` module. (I
  actually [opened a PR about
  it](https://github.com/haskell/bytestring/pull/9).) Sure, I can get the same
  functionality via `foldr`, but it's extra cognitive overhead for the author
  versus a simple `mapM_` call, and extra overhead for the reader to understand
  the goal of the code.
* The `Data.HashMap.Strict` API is missing a number of functions that are
  available in `Data.Map` and `Data.IntMap`, and which cannot be trivially
  reimplemented.

By contrast, when we use a typeclass-based interface (like `Foldable` or
`Traversable`), we get the following benefits:

* All the functions I know are available to me. I can use `Data.Foldable.mapM_`
  for any instance of `Foldable`, even if the author of that data type never
  considered `mapM_`.
* Add-on packages can provide new combinators without ever knowing about the
  data types I care about. The best example of this is `Monad`: there are
  dozens (if not hundreds or thousands) of useful `Monad`-based combinators
  sprinkled across Hackage, and these will work for arbitrary monadic datatypes I
  write in my own application.
* We can't accidentally end up with conflicting type signatures or semantics
  for a function name. Some real-world examples of this are:

    * `Data.Map.toList` behaves very differently from `Data.Foldable.toList`
    * `Data.ByteString.split` versus `Data.Text.split` (text's `split` is
      closer to `Data.ByteString.splitWith`)

By programming against interfaces, a developer learns an interface once, gets
access to a large array of convenience functions, and implementors can get away
with providing less functionality themselves.

## No qualified imports

Many people claim that programming to typeclasses is really just about
laziness: the right way to do this is to import everything qualified. The
previous section shows it's about more than just laziness, but let me address
qualified imports head-on:

* "Just laziness" is still a valid argument: why do extra work if it's not
  necessary?
* When I'm not using `classy-prelude`, I will often times in the middle of
  coding realize I need access to, for example, `Data.Map`. I now need to break
  my train-of-thought from my current code to:

    * Check if `Data.Map` is currently imported
    * If not, add it to the imports
    * Check if `containers` is in the cabal file
    * If not, add it to the build-depends in the cabal file
* The above steps are even more confusing for new users
* On multi-person projects, inconsistent qualified import names are a real
  problem.

To expand on the last point: I've worked with many different people, and seen
plenty of disagreements on import naming. Consider all of the following:

```haskell
import qualified Data.Map as Map
import qualified Data.Map as M

import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.MVar as MVar

import qualified Data.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString as BS

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy as L
```

This is a small subset of the confusion I've seen. We have the same modules
with different qualified names, and the same qualified names being shared by
multiple modules. The upshot of this is that, each time I'm working on a
different module, I need to remember which qualified import name is being used
here. Standardizing this would be a great solution, but the problem is that we
already tried it and it failed: most of the modules I listed above give a
recommended short name associated with them, but people (myself included) do
not follow it.

## Gives us more information

Speaking in terms of type classes will often times tell us more than using a
concrete type. My favorite example of this is `IO`. Consider:

```haskell
foo :: IO a -> IO a
```

We know virtually nothing about `foo`. However, if instead we had:

```haskell
foo :: Monad m => m a -> m a
```

We now know a lot more. There are no monadic side-effects being added by `foo`
itself (though the provided action may perform some). We still don't know
everything: how many times is the supplied action being called, for example?
But it's still a great start.

What I find really interesting is, despite what you may initially think, the
following is also more informative than a plain `IO`:

```haskell
foo :: MonadIO m => m a -> m a
```

In this case, `foo` can once again perform unbounded `IO` actions itself, e.g.:

```haskell
foo action = do
    liftIO fireTheMissiles
    res <- action
    liftIO fireTheMissilesAgainForGoodMeasure
    return res
```

However, we know that some control flows (such as exception handling) are not
being used, since they are not compatible with `MonadIO`. (Reason: `MonadIO`
requires that the `IO` be in positive, not negative, position.) This lets us
know, for example, that `foo` is safe to use in a continuation-based monad like
`ContT` or `Conduit`.

## Continue using the same abstractions

If you're familiar with the list API, you can pick up the majority of
`classy-prelude` trivially. Functions like `length`, `break`, `takeWhile`, and
`sum` are all present, work just like their list-specific cousins, and are
generalized to many additional types. Many of those types provide some nice
performance improvements, making it easy to speed up your code.

Another library which provides this kind of common interface to many datatypes
is lens. In many cases, this is done via a special typeclass (such as `At`,
`Cons`, or `Each`). However, these all require learning a different abstraction
from what you typically start Haskell with. On the other hand, the lens
approach allows for new ways of composing code that the more standard functions
make more difficult, so it certainly has advantages. I'm merely pointing out
here the much lower learning curve of picking up typeclasses based on the
functions you already know.

## Performance

Since in many cases, we can implement our typeclass-based functions in terms of
underlying efficient functions for a specific data (together with INLINE and
REWRITE rules), we will usually pay no overhead for using these abstractions.

## Easily test out alternative implementations

I've mentioned this in passing, but I'll call it out explicitly: programming to a typeclass lets you easily switch between different concrete implementations, which can be great for performance comparisons. Curious if a `Map` or a `HashMap` is faster? In the qualified import world, you'll need to:

* Change your import
* Modify the datatype name in all function signatures
* Rewrite any code using a `Data.Map` function that's not present in `Data.HashMap.Strict`

In a typeclass-based approach, you can write your functions the first time in
terms of the typeclass, and then likely get away with changing just one
explicit `Map` signature to `HashMap`.

## Common arguments against

I've given you a pretty opinionated list of reasons why I like typeclass-based
programming. That's not to say that strong arguments against this approach
don't exist. I'm going to give a (non-exhaustive) list of some such arguments
and address them.

*   Error messages are more confusing. This is definitely the case; an error of
    `Map is not a Vector` is more clear than `Map is not an instance of
    IsSequence`. My response to this is that, fairly quickly, you get used to the
    error messages GHC generates and can understand them easily. Not quite as
    easily as monomorphic error messages, but easily enough.

*   I've heard people claim things like "I don't know what the code is doing."
    The argument goes like this: if you see a `lookup`, you don't know if it's a
    `lookup` on a `Map` or a `Hashmap`. I consider this the weakest argument
    against typeclass programming, because it means you aren't following the
    paradigm at all. If you're coding to typeclasses, you don't _care_ which
    underlying implementation you're using: swapping out a `Map` for a `HashMap`
    would be just fine, and you just care about the stated semantics of the
    `lookup` function itself.

    There are times where you really do care about something that goes beyond
    the semantics of the typeclass. For example, if you need the contents of a
    set-like structure returned as a list in ascending order, `toList` on a
    `HashSet` will fail you. But in such a case: you're really not looking for the
    general `toList` function, you're looking for a specific function which
    guarantees ordering.

*   An annoyance with typeclass coding is that, sometimes, you need to write
    extra type signatures, since the compiler can't guess exactly which
    implementation you're trying to use. This is certainly true, and can be
    annoying, but it also goes hand-in-hand with being able to easily test out
    alternative data types.

*   Some people only want to use typeclasses based on well-established
    mathematical foundations. While I agree that having a mathematical basis to
    a typeclass is a great way to ensure you have a good abstraction, I disagree
    with it being a prerequisite for such a typeclass to be useful. And as my
    evidence for this, I call the venerable `Foldable` typeclass, which has no laws
    associated with it but is eminently useful. (Similar examples: `Binary`,
    `Storable`, `ToJSON`, and `FromJSON`.)

    * That said, I will readily admit that some of my original work in
      `classy-prelude` was far too loose in its usage of typeclasses for
      getting simple name overloading. The first few releases of the library wanted
      to test what was possible, but since then the typeclasses are restricted to
      what I would consider to be very sensible abstractions. You may disagree, and
      I'd be interested in hearing concrete examples of typeclasses you think are bad
      abstractions. But I think it's possible to look at code written exclusively
      against typeclasses and understand exactly what the code does.

*   There's a greater learning curve with an abstract interface versus a
    monomorphic interface. This is true, but once you need to learn two
    monomorphic interfaces, that learning curve is quickly amortized.

## Reward for the patient

Those of you patient enough to sit through to the end of this long monologue
can get a sneak preview. I've put my `Map` abstraction classes on Github
[inside the Jump
project](https://github.com/commercialhaskell/jump/blob/master/src/Jump/Map.hs).
The Jump project also happens to be the topic of conversation I had with the FP
Complete team that I mentioned in the first paragraph above. Expect more
information on this in the coming months.

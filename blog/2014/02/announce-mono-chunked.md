I'm happy to announce the release of a number of packages today, in particular:

* [mono-traversable](http://hackage.haskell.org/package/mono-traversable) 0.3
* [chunked-data](http://hackage.haskell.org/package/chunked-data) 0.1 (first release)
* [conduit-combinators](http://hackage.haskell.org/package/conduit-combinators) 0.1 (first release)
* [classy-prelude](http://hackage.haskell.org/package/classy-prelude), [classy-prelude-conduit](http://hackage.haskell.org/package/classy-prelude-conduit), and [classy-prelude-yesod](http://hackage.haskell.org/package/classy-prelude-yesod) 0.8

I want to discuss the purpose of these packages and recent changes. I'm very
excited about some of the opportunities these packages are presenting for
future growth and use.

## mono-traversable 0.3

mono-traversable's core concept is abstracting over a common pattern: data
structures that look like containers, but which may be monomorphic. The primary
example of this is `ByteString` and `Text`, though this abstraction has proven
useful for other cases as well, such as unboxed and storable `Vector`s, where
some typeclass constraint limits the allowed value type.

In my experience so far, the most powerful abstraction has been `MonoFoldable`.
Unlike `MonoFunctor` and `MonoTraversable`, you don't end up losing any
flexibility in data types, since we do not need to retain the original data
structure. As such, I've switched over to using MonoFoldable variants of
functions in place of Foldable ones in all of my new code.

In addition to this core abstraction, mono-traversable provides abstractions
for `Set`s and `Map`s, non-empty data types, and sequences. This last point can
serve as a replacement for a number of list-like abstractions which have been
implemented separately over the years.

Since the 0.2 release, Greg and I have worked on the following improvements:

* Additional instances for Either (thanks to João Cristóvão).
* An optimized `mapM_` implementation for `ByteString`.
* Greatly improved test coverage.
* Remove a number of type classes. As an example, we used to have a separate typeclass for non-empty types which had elements which could be ordered, which allowed for a total and optimized implementation of `maximum` and `minimum`. Instead, in version 0.3, `MonoFoldable` provides a `maximumEx` function, which can be optimized for an individual datatype, but may throw an exception, and then any non-empty data type may use `maximum` as a total function. We've done similar things for `head`, `tail` and others.
* Added unsafe functions like `unsafeHead` when you need more performance (thanks to John Lato for the idea).
* Greatly expanded the collection of `Map` and `Set` functions.
* A new `Data.MinLen` module (still experimental), which extends the concept of non-null. Instead of simply encoding size as a boolean, we use type-level naturals to encode the minimum known length of a value. This allows, for example, such total functions as `head $ tail $ mlappend listLen1 listLen1`. Greg and I have decided to release this initially as an experimental module and, if it works well, replace the current `Data.NonNull` with it.

The core functionality of `mono-traversable` is pretty stable, and I highly
encourage people to give it a shot.

## chunked-data

mono-traversable started as extracting some of the typeclass-based
functionality from classy-prelude in a principled, law-abiding manner. About
half of that functionality fell under the rubrik of monomorphic containers. The
other half was an abstraction of different kinds of chunked data: reading a
chunk of data from a `Handle`, textual encoding/decoding, lazy sequences,
builders, and zipping. `chunked-data` contains the remainder of this
functionality.

This package should be considered somewhat experimental. However, most of the
code is simply extracted from classy-prelude, where it's already had quite a
bit of testing. So those of you who are somewhat adventurous should definitely
jump in and play with it now. If you're more conservative, give in another
month or so before experimenting.

## conduit-combinators

This is the release I'm most excited about. There's quite a lot going on here,
and I'll probably write a separate blog post going into the details of this
package. But here's the basic idea.

Until now, the primary combinator collection for conduit was the
Data.Conduit.List module.  This module contains many commonly used functions,
like `map` and `isolate`.  However, there are two things I'm unhappy with in
the current module:

* The API only works on non-chunked data. For chunked data (like Text and
  ByteString), you need to use Data.Conduit.Text and Data.Conduit.Binary,
  respectively. John Lato and I have discussed this a bit in the past, and he
  made a very convincing argument to me that providing a unified chunked data API
  is superior.

* The naming scheme of Data.Conduit.List does not always encourage best
  practices. For example, `take` consumes all incoming values into memory. This
  was a decision inherited from enumerator, and made sense back when the library
  was first created. But common usage has changed.

This doesn't mean that there's anything broken in Data.Conduit.List, and
therefore I have no intention of breaking backwards compatibility by changing
the functions there. However, I think it's time to introduce a new, more modern
module providing even more combinators, chunked variations, and a more
consistent naming scheme.

That's where conduit-combinators comes into play. As a quick rundown:

* provides generalized versions of all (unless I missed something) functions from Data.Conduit.List, Data.Conduit.Binary, and Data.Conduit.Text
    * The package makes heavy use of mono-traversable and chunked-data to let this happen, which is what originally instigated my work on those two packages in this development cycle.
* since it's higher on the dependency chain, it can depend on packages like vector, and provide specific functions for it (like `sinkVector`)
* adds a lot of missing functions (like `takeWhile` and `mapWhile`)
* exports a module intended for qualified import: `Data.Conduit.Combinators`
* exports a new module, `Conduit`, which provides a one-stop-shop for all commonly needed conduit functionality. Combinator names are munged by appending a `C`.

I know many people out there are quite happy with qualified imports, but for
those of you like me who enjoy writing a short import list and then having all
of the functionality you need available, I think the new `Conduit` module will
be a real pleasure.

Since I've been giving stability forecasts, I'll do the same thing here. For
the basic functions like `map` and `sinkList`, it's highly unlikely that there
will *ever* be a change. However, as I get feedback on the library, I'm likely
to make some tweaks to behavior of other functions. One example: there are two
sets of behavior that the `drop` function could reasonably have:

* Drop the given number of items, and let the next monadically composed component consume the rest. In this case, `drop` would be a `Consumer`.
* Drop the given number of items, and then yield the rest of the items downstream. In this case, `drop` would be a `Conduit`.

I've chosen the former, but I'm open to discussion on the topic.

So if you're using conduit in any significant way in your codebase, it's
definitely worth checking out this new package. I wouldn't port large codebases
over to use it yet unless you're okay needing to refactor again in the next month
or so. But I anticipate this package reaching a stable point in the very near
future (less than 3 months from now), as it will be a vital component of some
work I'm doing at FP Complete.

One nice fact about this library: it is currently sitting at 100% HPC test
coverage, and I intend to maintain that level going forward.

## classy-prelude 0.8

The most exciting thing about the 0.8 release of classy-prelude is what's not
there: any significant code! After the work I've mentioned above,
classy-prelude (and -conduit and -yesod) has now become a simple re-export
module for functionality defined elsewhere. This is great: the core
generalizations have been made available for wider usage, and classy-prelude is
merely a convenience for getting at all of that functionality at once.

Since classy-prelude is based on the packages above, the same stability
guidelines apply. In particular, classy-prelude-conduit may see quite a bit of
turbulence as conduit-combinators changes. And if you used the chunked-data or
Data.MinLen re-exports from ClassyPrelude, those may be updated in the future.
But the vast majority of classy-prelude has remained stable for the past number
of versions (accounting for well over a year), and will continue to do so in
the future.

One question that I've raised and am looking for feedback on: should
ClassyPrelude export mtl data types and functions? I opened [a Github issue for
discussion](https://github.com/snoyberg/classy-prelude/issues/67), and am
interested if others have any thoughts on the matter.

Look forward to some blog posts demonstrating usage of these libraries over the
next few weeks. And if you have any recommendations for improvement, please
bring them up!

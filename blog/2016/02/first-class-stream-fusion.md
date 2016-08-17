This is a blog post about [a little thought
experiment](https://github.com/snoyberg/vegito#readme) I've been playing with
recently. The idea has been bouncing around in my head for a few years now, but
some recent discussions with coworkers at FP Complete (particularly Chris Done
and Francesco Mazzoli) made me spend a few hours on this, and I thought it
would be worth sharing for some feedback.

## Premise

The basic premise is this: we typically follow a philosophy in many common
libraries that there's the nice abstraction layer that we want to present to
users, and then the low-level approach under the surface that the user should
never know about but makes everything fast. This is generally a concept of
fusion and rewrite rules, and appears in things like build/foldr fusion in
base, and stream fusion in vector (and more recently, [in
conduit](https://www.fpcomplete.com/blog/2014/08/conduit-stream-fusion)).

Here are the ideas fueling this thought experiment:

* Making our code only fast when GHC rewrite rules fire correctly leads to
unreliable speedups. (Check the [benchmarks on the example
repo](https://github.com/snoyberg/vegito/blob/master/bench/bench.hs), which
show conduit slowing down despite its implementation of stream fusion.) This is
a very difficult situation to solve as a library user.

* By hiding the real implementation away under a nice abstraction, library
users do not necessarily have any understanding of what kinds of code will be
fast and what will be slow. This is not quite as frustrating as the previous
point, but still quite surprising.

* On the flip side, the high level abstractions generally allow for more
flexible code to be written than the lower level approach may allow.

* Is there a way to make the low-level, fast approach the primary interface
that the user sees, lose a minimal amount of functionality, and perhaps
regain that functionality by making the more featureful abstraction available
via explicit opt-in?

* Perhaps we can get better category laws out of a different formulation of a
streaming library (like pipes has), but still hold onto extra functionality
(like conduit has).

If that was too abstract, don't worry about it. Keep reading, and you'll see
where these ideas led me.

## Standard stream fusion

Duncan Coutts, Roman Leshchinskiy and Don Stewart introduced a concept called stream fusion, which powers the awesome speed and minimal memory usage of the vector package for many common cases. The idea is:

*   We have a stream abstraction which can be aggressively optimized by GHC (details unimportant for understanding this post)

*   Represent vector operations as stream operations, wrapped by functions that convert to and from vectors. For example:

    ```haskell
    mapVector f = streamToVector . mapStream f . vectorToStream
    ```

*   Use GHC rewrite rules to remove conversions back and forth between vectors and streams, e.g.:

    ```haskell
    mapVector f . mapVector g
        = streamToVector . mapStream f
                         . vectorToStream . streamToVector
                         . mapStream g . vectorToStream
          -- Apply rule: vectorToStream . streamToVector = id
        = streamToVector . mapStream f
                         . id
                         . mapStream g . vectorToStream
        = streamToVector . mapStream f . mapStream g . vectorToStream
    ```

In practice, this can allow long chains of vector operation applications to
ultimately rewrite away any trace of the vector, run in constant space, and get
compiled down to a tight inner loop to boot. Yay!

## User facing stream fusion

However, there's an underlying, unstated assumption that goes along with all of
this: users would rather look at vector functions instead of stream functions,
and therefore we should rely on GHC rewrite rules to hide the "complicated"
stream stuff. (Note: I'm simplifying a lot here, there are other reasons to
like having a Vector-oriented interface for users. We'll touch on that later.)

But let's look at this concretely with some type signatures. First, our main `Stream` datatype:

```haskell
data Stream o m r
```

This type produces a stream of `o` values, runs in the `m` monad, and
ultimately ends with a value of `r`. The `r` type parameter is in practice most
useful so that we can get `Functor`/`Applicative`/`Monad` instance of our type,
but for our purposes today we can assume it will always be `()`. And `m` allows
us more flexibility for optimizing things like `mapM`, but if you treat it as
`Identity` we have no effects going on. Said another way: `Stream o Identity
()` is more or less identical to `[o]` or `Vector o`.

How about common functions? Well, since this is just a thought experiment, I
only implemented a few. Consider:

```haskell
enumFromToS :: (Ord o, Monad m, Num o) => o -> o -> Stream o m ()

mapS :: Functor m => (i -> o) -> Stream i m r -> Stream o m r

foldlS :: (Monad m) => (r -> i -> r) -> r -> Stream i m () -> m r

-- Yes, we can build up more specific functions
sumS :: (Num i, Monad m) => Stream i m () -> m i
sumS = foldlS (+) 0
```

If you ignore the `m` and `r` type parameters, these functions look identical
to their list and vector counterparts. As opposed to lists and vectors, though,
we know for a fact that these functions will _never_ end up creating a list of
values in memory, since no such capability exists for a `Stream`. Take, for
example, the typical bad implementation of average for lists:

```haskell
average :: [Double] -> Double
average list = sum list / length list
```

This is problematic, since it traverses the entire list twice, being both CPU
inefficient _and_ possibly forcing a large list to remain resident in memory.
This mistake cannot be made naively with the stream implementation. Instead,
you're forced to write it the efficient way, avoiding confusion down the road:

```haskell
averageS :: (Fractional i, Monad m) => Stream i m () -> m i
averageS =
    fmap (\(total, count) -> total / count) . foldlS go (0, 0)
  where
    go (!total, !count) i = (total + i, count + 1)
```

Of course, this is also a downside: when you're trying to do something simple
without worrying about efficiency, being forced to deal with the lower-level
abstraction can be an annoyance. That's one major question of this thought
experiment: which world is the better one to live in?

## Capturing complex patterns

Coroutine-based streaming libraries like conduit and pipes provide for the
ability for some really complex flows of control without breaking
composability. For example, in conduit, you can use `ZipSink` to feed two
consumers of data in parallel and then use standard `Applicative` notation to
combine the result values. You can also monadically compose multiple
transformers of a data stream together and pass unconsumed data from one to the
other (leftovers). Without some significant additions to our stream layer
(which would likely harm performance), we can't do any of that.

Interestingly, all of the "cool" stuff you want to do in conduit happens before
you connect a component to its upstream or downstream neighbors. For example,
let's say I have two functions for parsing different parts of a data file:

```haskell
parseHeader :: Monad m => Sink ByteString m Header

parseBody :: Monad m => Sink ByteString m Body
```

I can compose these together monadically (or applicatively in this case) like
so:

```haskell
parseHeaderAndBody :: Monad m => Sink ByteString m (Header, Body)
parseHeaderAndBody = (,) <$> parseHeader <*> parseBody
```

So what if we had a conversion function that takes a coroutine-based
abstraction and converted it into our streaming abstraction? We don't expect to
have the same level of performance as a hand-written streaming abstraction, but
can we at least get composability? Thankfully, the answer is yes. [The
`Gotenks`
module](https://github.com/snoyberg/vegito/blob/master/src/Gotenks.hs)
implements a conduit-like library\*. This library follows all of the common
patterns: `await`, `yield`, and `leftover` functions, monadic composition, and
could be extended with other conduit features like `ZipSink`.

\* Unlike conduit, `Gotenks` does not provide finalizers. They complicate
things for a small example like this, and after a lot of thought over the
years, I think it's the one extra feature in conduit vs pipes that we could
most do without.

One thing notably missing, though, is any kind of operator like `=$=`, `$$`, or
(from pipes) `>->` or `<-<`, which allows us to connect an upstream and
downstream component together. The reason is that, instead, we have three
functions to convert to our streaming abstraction:

```haskell
toSource :: Applicative m => Gotenks () o m r -> Stream o m r
toTransform :: Applicative m => Gotenks i o m r -> Stream i m () -> Stream o m r
toSink :: Monad m => Gotenks i Void m r -> Stream i m () -> m r
```

And then, we're able to use standard function applications - just like in the
streaming layer - to stick our components together. For example, take this
snippet from the benchmark:

```haskell
[ bench' "vegito" $ \x ->
          runIdentity
        $ sumS
        $ mapS (+ 1)
        $ mapS (* 2)
        $ enumFromToS 1 x
, bench' "gotenks" $ \x ->
          runIdentity
        $ toSink sumG
        $ toTransform (mapG (+ 1))
        $ toTransform (mapG (* 2))
        $ toSource (enumFromToG 1 x)
```

The obvious benefit here is that our coroutine-based layer is fully compatible
with our stream-based layer, making for easy interop/composition. But in
addition:

* We now get to trivially prove the category laws, since we're just using
function composition! This is more important than it may at first seem. To my
knowledge, this is the first time we've ever gotten a streaming implementation
that has baked-in leftover support *and* full category laws, including left
identity. The reason this works is because we now have an explicit conversion
step where we "throw away" leftovers, which doesn't exist in conduit.
* In case you were worried: the `Gotenks` layer is implemented as a functor
combined with the codensity transform, guaranteeing trivially that we're also
obeying the monad laws. So without breaking a sweat, we've now got a great
law-abiding system. (Also, we get efficient right-association of monadic bind.)
* While the coroutine-based code will by nature be slower, the rest of our
pipeline can remain fast by sticking to streams.

## What's next?

Honestly, I have no idea what's next. I wanted to see if I could write a
streaming implementation that was guaranteed fast, provided interop with
conduit-style workflows, and would be relatively easy to teach. With the
exception of the two extra type parameters possibly causing confusion, I think
everything else is true. As far as where this goes next, I'm very much open to
feedback.

## UPDATE Benchmark results

Don Stewart asked me on Twitter to share the [benchmark results for this
repo](/assets/vegito-benchmark-2016-02-28.html). They're not particularly
enlightening, which is why I didn't include them initially. Nonetheless,
putting them here makes it clear what I'm getting at: vegito, vector, and
conduit (when stream fusion kicks in) are all the same speed. In fact, the more
interesting thing is to look at their compiled core, which is identical. The
annoyance is that, while `Data.Conduit.List` and `Data.Conduit.Combinators`
both fire their rewrite rules, the combinators provided by the `Conduit` module
do _not_ fire, leading to a significant (read: 200-fold) slowdown. This
slowdown is exacerbated by the choice of benchmark, which is intended to
demonstrate the specific power of the stream fusion optimizations.

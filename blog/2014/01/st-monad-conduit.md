I've recently been working on some higher level analysis utilities based on top
of
[conduit](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview).
A major component of this work is performing high-performance numerical
calculations on large streams of data. So I was particularly intriguied when I
saw [a StackOverflow
question](http://stackoverflow.com/questions/21132026/frequency-of-characters/21134840)
that touched on the same points. I'd like to elaborate on my answer to that
question, and demonstrate a possible addition to the conduit library.

The issue at play in that question is the desire to tally up the frequency of
each octet in a stream of data. If you look through the answers, it quickly
becomes apparent that using some kind of a mutable packed data structure
(either `Vector` or `Array`) provides drastically better performance than
immutable data structures. For our purposes, let's stick with the vector
library, though the discussion here applies equally well to array.

The actions we need to perform are very straight-forward: read in the entirety
of the data from some source (let's say standard input), and perform a mutating
action for each and every octet that we receieve. We could read the entire
stream of data using lazy I/O, but as readers of this blog are likely aware,
I'd rather avoid lazy I/O when possible. So in my answer, I used conduit to
read in the stream of data. The answer looks like this:

```haskell
import           Control.Monad.Trans.Class   (lift)
import           Data.ByteString             (ByteString)
import           Data.Conduit                (Consumer, ($$))
import qualified Data.Conduit.Binary         as CB
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           System.IO                   (stdin)

freqSink :: Consumer ByteString IO (V.Vector Int)
freqSink = do
    freq <- lift $ VM.replicate 256 0
    CB.mapM_ $ \w -> do
        let index = fromIntegral w
        oldCount <- VM.read freq index
        VM.write freq index (oldCount + 1)
    lift $ V.freeze freq

main :: IO ()
main = (CB.sourceHandle stdin $$ freqSink) >>= print
```

We now have a reusable `freqSink` component that will consume a stream of
`ByteString`s to produce a `Vector Int` of the frequencies. The `Sink` creates
a new mutable vector to hold the frequency values, maps over all the input
octets, and for each octet updates the mutable vector. Finally, it freezes the
mutable vector into an immutable one and returns it.

I like *almost* everything about this, except for two characters: `IO`. Our
`freqSink` function sets the base monad to be `IO`, implying that `freqSink`
may perform actions that have an impact on the outside world. However, we know
that this isn't the case: by analyzing the code, we see that all of the
mutating changes are contained within the little world that `freqSink` creates
for itself. In other words, this function is referentially transparent, but the
type signature is saying otherwise.

Fortunately, Haskell already has the perfect solution for this kind of a
problem: the `ST` monad. All we need to do is swap `IO` for `ST s` and
`freqSink` will be properly annotated as being referentially transparent. But
when we make this change, we get the following error message:

    Couldn't match type `ST s0' with `IO'

The problem is that, while `freqSink` is refentially transparent,
`sourceHandle` is *not*. Since the source is capable of performing arbitrary
`IO`, it has to live in the `IO` base monad, and since the two components live
in the same processing pipeline, `freqSink` must match that base monad as well.
While this all works, it's still quite disappointing.

But perhaps we can have our cake and eat it too. We want `freqSink`'s type
signature to be refentially transparent, which means it needs to live in `ST`.
What we need is some way to turn an `ST`-based `Sink` into an `IO`-based
`Sink`. And there's a function that let's us do just that:
[unsafeSTToIO](http://haddocks.fpcomplete.com/fp/7.4.2/20130829-168/base/Control-Monad-ST-Unsafe.html#v:unsafeSTToIO).
This ends up looking like:

```haskell
import           Control.Monad.Morph         (hoist)
import           Control.Monad.ST            (ST)
import           Control.Monad.ST.Unsafe     (unsafeSTToIO)
import           Control.Monad.Trans.Class   (lift)
import           Data.ByteString             (ByteString)
import           Data.Conduit                (Consumer, ($$))
import qualified Data.Conduit.Binary         as CB
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           System.IO                   (stdin)

freqSink :: Consumer ByteString (ST s) (V.Vector Int)
freqSink = do
    freq <- lift $ VM.replicate 256 0
    CB.mapM_ $ \w -> do
        let index = fromIntegral w
        oldCount <- VM.read freq index
        VM.write freq index (oldCount + 1)
    lift $ V.freeze freq

main :: IO ()
main = (CB.sourceHandle stdin $$ hoist unsafeSTToIO freqSink) >>= print
```

This once again works, and `freqSink`'s type signature now indicates that it is
referentially transparent. However, we've put two heavy burdens on users of our
`freqSink` function:

* They need to know about `hoist` and understand how to use it.
* They need to pull in an unsafe function and know in which circumstances it's safe to use it.

What we *really* want is to provide a general purpose function which is
completely safe. We want to contain the concept of "I can safely swap out the
base monad of this Conduit with some other base monad." So I've just pushed a
[new commit to
conduit](https://github.com/snoyberg/conduit/commit/90139e6c316de616e40f59918c761c52eac7e2cb)
adding the `conduitSwapBase` helper function (name up for debate). Let's start
by seeing how it solves our present problem:

```haskell
import           Control.Monad.ST            (ST)
import           Control.Monad.Trans.Class   (lift)
import           Data.ByteString             (ByteString)
import           Data.Conduit                (Consumer, ($$))
import qualified Data.Conduit.Binary         as CB
import           Data.Conduit.Util           (conduitSwapBase)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           System.IO                   (stdin)

freqSinkST :: Consumer ByteString (ST s) (V.Vector Int)
freqSinkST = do
    freq <- lift $ VM.replicate 256 0
    CB.mapM_ $ \w -> do
        let index = fromIntegral w
        oldCount <- VM.read freq index
        VM.write freq index (oldCount + 1)
    lift $ V.freeze freq

freqSink :: Monad m => Consumer ByteString m (V.Vector Int)
freqSink = conduitSwapBase freqSinkST

main :: IO ()
main = (CB.sourceHandle stdin $$ freqSink) >>= print
```

I renamed the previous function to `freqSinkST`, leaving its type exactly as it
was. In addition, we now have a new `freqSink`, which can live in *any* base
monad. The type signature makes it completely clear that this function is
referentially transparent. And all we needed to do was use `conduitSwapBase` to
perform this conversion.

Once that conversion is performed, we can easily combine `freqSink` with our
`IO`-based `sourceHandle`. Or for that matter, it could be combined with a
completely pure source, or a source living in the `Maybe` monad.

I believe this function could be used to clean up the compression/decompression
functions in
[zlib-conduit](http://hackage.haskell.org/package/zlib-conduit-1.0.0/docs/Data-Conduit-Zlib.html)
and (at least some of) the functions in
[blaze-builder-conduit](http://hackage.haskell.org/package/blaze-builder-conduit-1.0.0/docs/Data-Conduit-Blaze.html).

As it stands right now, `conduitSwapBase` will allow the following base transformations to be applied:

* `ST` can be converted to ~~any other monad~~. __EDIT__ See update below.
* `Identity` can be converted to any other monad.
* `IO` can be converted to any instance of `MonadIO`.
* For many transformers (all instances of [MonadTransControl](http://haddocks.fpcomplete.com/fp/7.4.2/20130829-168/monad-control/Control-Monad-Trans-Control.html#t:MonadTransControl) actually), if the base monad `m1` can be converted to `m2`, then the transformer `t m1` can be converted to `t m2`.

This addition allows us to keep more type safety in our codebase, while still
allowing safe interleaving of `IO` actions with pure code. I'm happy with the
addition so far, I'm curious to hear further ideas from the community.

__UPDATE__: As [pointed out on
Reddit](http://www.reddit.com/r/haskell/comments/1vcvxe/the_st_monad_and_conduit/cer0l4l),
a backtracking base monad can break refential transparency for `ST`. I've
[pushed a new
commit](https://github.com/snoyberg/conduit/commit/77417ee45da8f9ee4e239b1f7e6a21013fd5b084)
that constrains the types of monads that can be converted to. In particular, it
works for monads which are processed in a linear/non branching manner. This
includes Identity, IO and Maybe, and transformers like ReaderT and ErrorT.

I'm currently calling this concept `MonadLinear`, but I have a strong feeling that there's a better abstraction already in existence.

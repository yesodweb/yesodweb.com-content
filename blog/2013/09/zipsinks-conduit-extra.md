Introducing _conduit-extra_
===========================

Michael Snoyman recently added a new package called _conduit-extra_ into the _conduit_ framework. It's aimed on incubating new ideas around conduit, testing them and seeing if they're worth adding into the main package.

In this post I'd like to introduce one of its first additions.

`ZipSink`
=========

Motivating example
------------------

Our task will be to compute several different power sums from a sequence of numbers. We would like to solve the task by constructing a `Sink` that will read numbers on its input and compute first _n_ power sums of its input:

    powerSums :: (Monad m, Num a) => Int -> Sink a m [a]

The first obvious step is to create a sink that computes the _k_-th power sum:

```haskell
import Control.Applicative
import Control.Seq
import Data.Conduit
import Data.Conduit.Extra
import Data.Conduit.List (fold, sourceList)
import Data.Traversable (traverse)

powerSum :: (Monad m, Num a) => Int -> Sink a m a
powerSum k = fold (\s x -> s + x^k) 0
```

Now we'd like to generalize it to a list of power sums. Unfortunately, if we want to compute a list of sums at once, we'll have to keep the list of sums as the state of `fold`:

```haskell
powerSumsAttempt1 :: (Monad m, Num a) => Int -> Sink a m [a]
powerSumsAttempt1 n = fold f (repeat 0)
  where
    f ss x = zipWith (+) ss (map (x^) [1..n])
```

This involves mapping and zipping and the code is somewhat obscure, clearly much less readable than our original `powerSum`.

Moreover, while it produces the correct result, it's still problematic. We soon realize that it leaks memory for large inputs, because even though `fold` is strict, thunks accumulate inside the list. So we have to force the evaluation of the list during folding (using `Control.Seq`):

```haskell
powerSumsAttempt2 :: (Monad m, Num a) => Int -> Sink a m [a]
powerSumsAttempt2 n = fold f (repeat 0)
  where
    f ss x = withStrategy (seqList rseq) $ zipWith (+) ss (map (x^) [1..n])
```

Clearly, this solution has several major drawbacks:

- The original simple idea is completely lost within auxiliary code.
- The code isn't composable. We couldn't use `powerSum` as a building block. Instead, we had to reimplement a more complex variant from scratch.
- In this case we're dealing with a collection of parallel folds, which can be expressed as a fold over a list. But what if our building blocks were more complex sinks, not simple folds? Then there would be no way how to combine them together, even manually.
- We don't want to deal with strictness and evaluation, we want _conduit_ to handle it for us.


ZipSinks applicative functor
----------------------------

What we need is a way how to parallelize sinks, combine them so that the input is fed to all of them, and combine the results at the end.

Recall that `ConduitM` is a `Monad` and an `Applicative` and allows to compose conduits using their operations. But the semantic of these instances is different. It's sequential instead of parallel. Expression `condF <*> condX` runs `condF` first, when it finishes it continues with `condX` and then applies the result of `condF` to the result of `condX`.

Therefore _conduit-extra_ has introduced a simple newtype wrapper for `Sink`s

    newtype ZipSink i m r = ZipSink { getZipSink :: Sink i m r }

which implements `Applicative` with the parallel semantics:

- `pure` creates a sink that doesn't consume any input, just returns a given value;
- `<*>` runs two sinks in parallel until both finish, and then applies the result of the first one to the second one.

Now we can run `powerSum`s in parallel as

```haskell
powerSumZ :: (Monad m, Num a) => Int -> ZipSink a m a
powerSumZ = ZipSink . powerSum

powerSumTuple :: (Monad m, Num a) => Int -> Int -> Sink a m (a, a)
powerSumTuple k l = getZipSink $ (,) <$> powerSumZ k <*> powerSumZ l
```

Not just that, we can use all the existing functions for `Applicative`s. Here we had two sinks and created one sink that returned a tuple. Our final aim is to create a sink that returns a list, provided we can create a sink using `powerSumZ` for each member of `[0..n]`. This sounds like a job for `traverse`:

    traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)

which, specialized to our case, is typed as

    traverse :: (Int -> ZipSink a m b) -> [Int] -> ZipSink a m [b]

It solves our problem completely and very elegantly:

```haskell
powerSums :: (Monad m, Num a) => Int -> Sink a m [a]
powerSums n = getZipSink $ traverse powerSumZ [0..n]
```

Let's print some test results:

```haskell
main :: IO ()
main = (sourceList [1..100] $$ powerSums 4) >>= print
```

which produces

    [100,5050,338350,25502500,2050333330]

Notes
-----

- The above scenario is quite often, therefore _conduit-extra_ already provides a helper function for `Sink`s that uses `ZipSink`s only internally:

        broadcast :: (Monad m, Traversable t) => t (Sink i m r) -> Sink i m (t r)

    So an alternative way how to implement `powerSums` would be

        powerSums' n = broadcast $ map powerSum [0..n]

- `ZipSink`'s `<*>` is implemented using [`zipSinks`](http://hackage.haskell.org/packages/archive/conduit/1.0.7.4/doc/html/Data-Conduit-Util.html#v:zipSinks) from `Data.Conduit.Util`.

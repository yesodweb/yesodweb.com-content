I'm happy to announce a [new release of
conduit](http://hackage.haskell.org/package/conduit-1.0.11) (version 1.0.11).
This release was instigated by a [pull request from Patrick
Wheeler](https://github.com/snoyberg/conduit/pull/122), who wrote the majority
of the code that's been included.

tl;dr:
[Data.Conduit.Lift](http://hackage.haskell.org/package/conduit-1.0.11/docs/Data-Conduit-Lift.html)
allows you to run individual components of a pipeline with different monad
transformers, and
[catchC](http://hackage.haskell.org/package/conduit-1.0.11/docs/Data-Conduit.html#v:catchC)/[tryC](http://hackage.haskell.org/package/conduit-1.0.11/docs/Data-Conduit.html#v:tryC)
let you deal with exceptions inside an individual component. Read on for more
details.

Patrick's pull request adds a number of functions for working with monad
transformers in conduit. While conduit has always had support for arbitrary
base monads, and for quite a while has allowed transforming the base monad via
[hoist](http://haddocks.fpcomplete.com/fp/7.4.2/20130829-168/mmorph/Control-Monad-Morph.html#v:hoist),
there's been one missing feature: the ability to have different pieces of a
pipeline use different transformer stacks.

While `hoist` on its own would allow a `ReaderT` transformer to be removed from
a stack, the same technique wouldn't work for any of the more interesting
transformers. This problem has been known for a while, and has in fact [been
documented in the
Wiki](https://github.com/snoyberg/conduit/wiki/Dealing-with-monad-transformers)
for over two years. Patrick's approach solves the problem.

Suppose you want to write a `Sink` which will sum up all of the incoming `Int`s into a single `Int`. (And forget for the moment that this is easy to do with Data.Conduit.List.fold.) An easy way to do this would be to use a `StateT`:

```haskell
sumSink :: Monad m => Sink Int (StateT Int m) Int
sumSink = do
    awaitForever $ modify . (+)
    get
```

Calling this looks like the following:

```haskell
main :: IO ()
main = evalStateT (mapM_ yield [1..10] $$ sumSink) 0 >>= print
```

Unfortunately, this has some problems:

*   The calling site needs to deal with unwrapping the `StateT`, when in reality the `StateT` usage is merely an implementation detail of the `sumSink` function.

*   Now all of the components of our pipeline need to have a matching `StateT` transformer. This can be arranged, especially by using `hoist`, but it's an unnecessary complication.

*   What happens if you have two components in a pipeline that each want to use their wrapping `StateT` in a different way? The two components would now conflict with each other. Consider this (only slightly contrived) example of a `Conduit` which accumulates its input values into a sum and yields that sum:

    ```haskell
    import Data.Conduit
    import Control.Monad.State

    sumSink :: Monad m => Sink Int (StateT Int m) Int
    sumSink = do
        awaitForever $ modify . (+)
        get

    accumConduit :: Monad m => Conduit Int (StateT Int m) Int
    accumConduit = awaitForever $ \i -> do
        total <- get
        let total' = total + i
        yield total'
        put total'

    main :: IO ()
    main = evalStateT (mapM_ yield [1..10] $$ accumConduit =$ sumSink) 0 >>= print
    ```

    Unfortunately, this doesn't work as we'd hope, since each component overwrites the stored value of the sibling component. (And if you're curious, try swapping the ordering of the `yield` and `put` calls in `accumConduit`.)

The `Data.Conduit.Lift` module provides a simple solution to all of these problems. You can use whatever monad transformers you like when implementing a component of a pipeline, and then unwrap the transformer for that component alone. The function names look just like their `transformers` counterparts, but replace the trailing `T` with a `C`. To fix our example from above:

```haskell
import Data.Conduit
import Data.Conduit.Lift
import Control.Monad.State

sumSink :: Monad m => Sink Int m Int
sumSink = execStateC 0 $ awaitForever $ modify . (+)

accumConduit :: Monad m => Conduit Int m Int
accumConduit = evalStateC 0 $ awaitForever $ \i -> do
    total <- get
    let total' = total + i
    put total'
    yield total'

main :: IO ()
main = (mapM_ yield [1..10] $$ accumConduit =$ sumSink) >>= print
```

Additionally, if you'd like to use the strict variants of Writer, State, or RWS, replace the trailing `C` with a trailing `SC`. Hopefully this technique will prove useful, and its usage is clear enough from this example. If there are questions about how to use it, please bring them up so I can update the documentation as needed.

Once I saw the technique used for these functions, I realized it could be used to implement an oft-requested piece of functionality: exception handling. To date, there was no way to deal with exceptions in the middle of a conduit pipeline. Instead, exception handlers had to be used outside of the pipeline.

As of this release of conduit, there's another option (or three, to be precise): `catchC`, `handleC`, and `tryC`, which behave *almost* identically to their `Control.Exception` counterparts. The important thing to note is that they will only catch exceptions thrown in the current component of the pipeline, *not* a different component. As an example, consider this failed attempt at copying files:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.Binary
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Control.Exception (IOException)

main :: IO ()
main = runResourceT $ src $$ sinkFile "/does/not/exist/output.txt"

src :: Source (ResourceT IO) ByteString
src = sourceFile "/does/not/exist/input.txt" `catchC` \e ->
    yield (pack $ "Could not read input file: " ++ show (e :: IOException))
```

Both the source and sink end up throwing exceptions. While the exception handler in the source would catch its thrown exception, the exception from the sink will remain uncaught. This leads to one very important caveat. Taken straight from the API docs:

> Note: this will not catch exceptions thrown by other components! For example, if an exception is thrown in a Source feeding to a Sink, and the Sink uses catchC, the exception will not be caught.
>
> Due to this behavior (as well as lack of async exception handling), you should not try to implement combinators such as onException in terms of this primitive function.

As before, if you need full exception safety for cleaning up scarce resources, `ResourceT` will remain your best bet (though, if you're so inclined, you can [use the bracket pattern instead](https://www.fpcomplete.com/user/snoyberg/library-documentation/resourcet#resourcet-is-not-conduit)). But for the cases where you simply want to deal with exceptional circumstances, `catchC` and family should provide you with the tools you need.

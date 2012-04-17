There's more than one way to skin a cat, and certainly more than one way to
write code. The various options can sometimes be confusing. And in the case of
the `conduit` library, there are also some routes that you *shouldn't* take.
You'll see what I mean through the examples.

For the most part, using existing `Source`s, `Sink`s, and `Conduit`s is
straight-forward. The problem comes from writing them in the first place. Let's
take a simple example: we want a `Source` that will enumerate the `Int`s 1 to
1000. For testing purposes, we'll connect it to a `Sink` that sums up all of
its input. I came up with six different ways to write the `Source`, though two
of those are using functions I haven't yet released.

    import Criterion.Main
    import Data.Conduit
    import qualified Data.Conduit.List as CL
    import qualified Data.List
    import Data.Functor.Identity (runIdentity)
    
    sourceList, unfold, enumft, yielder, raw, state
        :: Monad m
        => Int -- ^ stop
        -> Source m Int
    
    sourceList stop = CL.sourceList [1..stop]
    
    unfold stop =
        CL.unfold f 1
      where
        f i
            | i > stop = Nothing
            | otherwise = Just (i, i + 1)
    
    enumft stop = CL.enumFromTo 1 stop
    
    yielder stop =
        go 1
      where
        go i
            | i > stop = return ()
            | otherwise = do
                yield i
                go $ i + 1
    
    raw stop =
        go 1
      where
        go i
            | i > stop = Done Nothing ()
            | otherwise = HaveOutput (go $ i + 1) (return ()) i
    
    state stop =
        sourceState 1 pull
      where
        pull i
            | i > stop = return StateClosed
            | otherwise = return $ StateOpen (i + 1) i
    
    main :: IO ()
    main = do
        mapM_ test sources
        defaultMain $ map bench' sources
      where
        sink :: Monad m => Sink Int m Int
        sink = CL.fold (+) 0
    
        bench' (name, source) = bench name $ whnf (\i -> runIdentity $ source i $$ sink) 1000
    
        sources =
            [ ("sourceList", sourceList)
            , ("unfold", unfold)
            , ("enumFromTo", enumft)
            , ("yield", yielder)
            , ("raw", raw)
            , ("sourceState", state)
            ]
    
        test (name, source) = do
            let res = runIdentity $ source 1000 $$ sink
            putStrLn $ name ++ ": " ++ show res

`sourceList` is probably the approach most of us- myself included- would
actually use in real life. It let's us take advantage of all of the
list-processing functions and special syntax that Haskell already provides.
`unfold` and `enumFromTo` are both new functions for 0.4.2 (in fact, I wrote
them for the purpose of this comparison). They correspond very closely to their
`Data.List` and `Prelude` counterparts.

`yield` is a new option we have starting with `conduit` 0.4. Due to the unified
datatypes, `Source` has inherited a `Monad` instance. This allows us to fairly
easily compose together different `Source`s, and the `yield` function provides
the simplest of all `Source`s. In previous versions of `conduit`, we could have
used `Source`'s `Monoid` instance instead of `do`-notation.

`raw` goes directly against the datatypes. I find it interesting that the raw
version isn't really much more complicated than `yield` or `sourceState`,
though you do have to understand some of the extra fields on the constructors.
Finally, we use `sourceState`. This is one of the oldest approaches, since this
function has been available since the first release of `conduit`. I think that
this function would compile and run perfectly on conduit 0.0.

The Criterion benchmarks are very informative. Thanks to Bryan's cool new
report, let's look at the graph:

<img src="/assets/skinning-conduits/source.png">

`unfold`, `enumFromTo`, and `raw` all perform equally well. `sourceList` comes
in behind them: the need to allocate the extra list is the culprit. Behind that
is `yield`. To see why, look at the difference between `yielder` and `raw`.
They're structure almost identically. For the `i > stop` case, we have `return
()` versus `Done Nothing ()`. But in reality, __those are the same thing__!
`return` is defined as `Done Nothing`.

The performance gap comes from the `otherwise` branch. If we fully expand the
`do`-notation, we end up with:

    yield i >>= (go $ i + 1)
    ==> HaveOutput (Done Nothing ()) (return ()) i >> (go $ i + 1)
    ==> HaveOutput (Done Nothing () >> (go $ i + 1)) (return ()) i
    ==> HaveOutput (go $ i + 1) (return ()) i

Which is precisely what `raw` says. However, without adding aggressive inlining
to `conduit`, most of this transformation will occur at runtime, not compile
time. Still, the performance gap is relatively minor, and in most real-world
applications should be dwarfed by the actual computations being performed, so I
think the `yield` approach definitely has merit.

What might be shocking is the abysmal performance of `sourceState`. It's a full
8 times slower than `raw`! There are two major contributing factors here:

* Each step goes through a monadic bind. This is necessitated by the API of `sourceState`.
* We have to unwrap the `SourceStateResult` type.

`sourceState` was great when it first came out. When `conduit`'s internals were
ugly and based on mutable variables, it provided a clean, simple approach to
creating `Source`s. However, `conduit` has moved on: the internals are pure and
easy to work with and we have alternatives like `yield` for high-level stuff.
And performance wise, the types now distinguish between pure and impure
actions. `sourceState` forces usage of an extra `PipeM` constructor at each
step of output generation, which kills GHC's ability to optimize.

So our main takeaway should be: don't use `sourceState`. It's there for API
compatibility with older versions, but is no longer the best approach to the
problem. Similarly, we can improve upon `sourceIO`, but we have to be a bit
careful here, since we have to ensure that all of our finalizers are called
correctly. Let's take a look at a simple `Char`-based file source, comparing a
`sourceIO` implementation to the raw constructors.

    import Data.Conduit
    import qualified Data.Conduit.List as CL
    import Control.Monad.Trans.Resource
    import System.IO
    import Control.Monad.IO.Class (liftIO)
    import Criterion.Main
    
    sourceFileOld :: MonadResource m => FilePath -> Source m Char
    sourceFileOld fp = sourceIO
        (openFile fp ReadMode)
        hClose
        (\h -> liftIO $ do
            eof <- hIsEOF h
            if eof
                then return IOClosed
                else fmap IOOpen $ hGetChar h)
    
    sourceFileNew :: MonadResource m => FilePath -> Source m Char
    sourceFileNew fp = PipeM
        (allocate (openFile fp ReadMode) hClose >>= go)
        (return ())
      where
        go (key, h) =
            pull
          where
            self = PipeM pull close
            pull = do
                eof <- liftIO $ hIsEOF h
                if eof
                    then do
                        release key
                        return $ Done Nothing ()
                    else do
                        c <- liftIO $ hGetChar h
                        return $ HaveOutput self close c
            close = release key
    
    main :: IO ()
    main =
        defaultMain [bench "old" $ go sourceFileOld, bench "new" $ go sourceFileNew]
      where
        go src = whnfIO $ runResourceT $ src "source-io.hs" $$ CL.sinkNull

The results are much closer here:

<img src="/assets/skinning-conduits/source-io.png">

We're no longer getting the benefit of avoiding monadic binds, since by its
very nature this function has to call `IO` actions constantly. In fact, I
believe that the performance gap here doesn't warrant avoiding `sourceIO` in
normal user code, though it's likely a good idea to look at optimizing the
`Data.Conduit.Binary` functions. Perhaps even better is if we can get some
combinators that make it easier to express this kind of control flow.

The story is much the same with `Sink`s and `Conduit`s, so I won't bore you
with too many details. Let's jump into the code first, and then explain what we
want to notice.

    import Criterion.Main
    import Data.Conduit
    import qualified Data.Conduit.List as CL
    import qualified Data.List
    import Data.Functor.Identity
    
    main :: IO ()
    main = defaultMain
        [ bench "mapOutput" $ flip whnf 2 $ \i -> runIdentity $ mapOutput (* i) source $$ sink
        , bench "map left" $ flip whnf 2 $ \i -> runIdentity $ source $= CL.map (* i) $$ sink
        , bench "map right" $ flip whnf 2 $ \i -> runIdentity $ source $$ CL.map (* i) =$ sink
        , bench "await-yield left" $ flip whnf 2 $ \i -> runIdentity $ source $= awaitYield i $$ sink
        , bench "await-yield right" $ flip whnf 2 $ \i -> runIdentity $ source $$ awaitYield i =$ sink
        ]
      where
        source :: Monad m => Source m Int
        source = CL.sourceList [1..1000]
    
        sink :: Monad m => Sink Int m Int
        sink = CL.fold (+) 0
    
        awaitYield :: Monad m => Int -> Conduit Int m Int
        awaitYield i =
            self
          where
            self = do
                mx <- await
                case mx of
                    Nothing -> return ()
                    Just x -> do
                        yield $ x * i
                        self

There are five different ways presented to multiple each number in a stream by
2. `CL.map` is likely the most obvious choice, since it's a natural analogue to
the list-based `map` function. But we have two different ways to use it: we can
either left-fuse the source to the conduit, and then connect the new source to
the sink, or right-fuse the conduit to the sink, and connect the source to the
new sink.

We also have an `awaitYield` function, which uses the `await` and `yield`
functions and leverages the `Monad` instance of `Conduit`. Like `map`, we have
both a left and a right version.

We also have a `mapOutput` function. In that case, we're not actually using a
`Conduit` at all. Instead, we're modifying the output values being produced by
the source directly, without needing to pipe through an extra component. Let's
see our benchmark results:

<img src="/assets/skinning-conduits/conduit.png">

There are three things worth noticing:

1. Like previously, the high-level approach (using `await` and `yield`) was slower than using the more highly optimized function from `Data.Conduit.List`.
2. There no clear winner between left and right fusing.
3. `mapOutput` is significantly faster than using a `Conduit`. The reason is that we're able to eliminate an entire extra `Pipe` in the pipeline.

`mapOutput` will not be an option in the general case. You're restricted in a number of ways:

* It can only be applied to a `Source`, not a `Sink`.
* You have to have transformations which produce one output for one input.
* You can perform any monadic actions.

However, if your use case matches, `mapOutput` can be a very convenient optimization.

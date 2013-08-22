John Wiegley and I are currently working- together with some members of the community- on various exception handling related code. As part of this work, we're also going to be publishing a number of tutorials on this topic. Eventually, we'll collect all of this information together into a more cohesive whole, but for now we wanted to get the content out there to the community as we produce it.

Note that this content is [available on the School of Haskell](https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/exceptions-and-monad-transformers); I recommend reading it there to be able to use the active code.

---


Love them or hate them, runtime exceptions are a reality of writing Haskell code. Consider the following fake code:

```haskell
myFunc = do
    resource <- acquireScarceResource
    useResource resource
    releaseResource resource
```

In a world without exceptions, this seems completely reasonable. However, what happens if the call to `useResource` throws a runtime exception? In such a case, `releaseResource` would never be called, and our scarce resource would never be released. In concurrent code dealing with mutexes, such a bug could lead to a deadlock.

The solution to this problem is well known: use the <hoogle results="1">bracket</hoogle> function. Then our above example reduces to `myFunc = bracket acquireScarceResource releaseResource useResource`. We're now safe from any exception thrown by `useResource`, and even from asynchronous exceptions (a topic I'll try to avoid in this post).

But let's analyze the type signature of `bracket`:

    bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

How do we use such a function in monad transformer stack, where the functions we pass in won't live in the `IO` monad itself?

Note that for the rest of this tutorial, I'm going to talk about the <hoogle results="1">finally</hoogle> function instead of `bracket`, which is really just a simplified version of the latter. The reasoning used will apply to not only `bracket`, but also to many other functions in <hoogle results="1">Control.Exception</hoogle>. Its signature is `IO a -> IO b -> IO a`, and its purpose is to ensure that the second function is called regardless of what exceptions are thrown by the first (or any asynchronous exceptions... sorry for bringing those up again).

## The simple case: the ReaderT transformer

Let's forget about any kind of grand solution. Instead, let's take one of the simplest monad transformers that exists, `ReaderT`, and see how we can write a `finally` that works with it. Since a `ReaderT` simply passes in some environment to all actions, this turns out to be really easy:

```haskell active
import Control.Monad.Reader
import Control.Exception (finally, try, ErrorCall)

finallyReaderT :: ReaderT r IO a -> ReaderT r IO b -> ReaderT r IO a
finallyReaderT a sequel = do
    r <- ask
    liftIO $ runReaderT a r `finally` runReaderT sequel r

main :: IO ()
main = do
    res <- try $ runReaderT
        (step1 `finallyReaderT` step2)
        "This is the environment"
    print (res :: Either ErrorCall ())
  where
    step1 = do
        r <- ask
        liftIO $ putStrLn $ "In step1, r == " ++ show r
        error "Erroring out, bye!"
    step2 = do
        r <- ask
        liftIO $ putStrLn $ "In step2, r == " ++ show r
```

That turns out to be pretty simple. And by reusing the existing `finally` function, we can feel comfortable knowing that our implementation is correct (or at least as correct as `finally` itself).

## First complication: mutable state

But `ReaderT` is a really simple case to support, since it's just a read-only environment. Let's up the ante a bit, and try using `StateT` instead. We'll now need to deal with threading the mutable state variable through. Before jumping into the code, let's consider abstractly how would could achieve this threading. We have two possible cases: an exception is thrown before calling the cleanup function, or an exception is not thrown. If no exception is thrown, then we can extract the state value from the result of the first function call, pass that to the cleanup function, and then return the final state value as the new mutable state.

But if an exception is thrown, there won't be a chance to extract the updated state value from the first function. In that case, we'll only have the initial state value available to pass through to the cleanup function. (The new result state is irrelevant, since the exception is going to be rethrown anyway.) As described until now, let's see what this kind of `finally` implementation may look like.

```haskell active
import Prelude hiding (catch)
import Control.Monad.State
import Control.Exception

finallyStateT :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT a sequel = do
    s1 <- get
    (result, s2) <- liftIO $ runStateT a s1 `catch` \e -> do
        _ignored <- runStateT sequel s1
        throwIO (e :: SomeException)
    (_ignored, s3) <- liftIO $ runStateT sequel s2
    put s3
    return result

main :: IO ()
main = do
    res <- try $ runStateT
        (step1 `finallyStateT` step2)
        1
    print (res :: Either ErrorCall ((), Int))
  where
    step1 = do
        s <- get
        liftIO $ putStrLn $ "In step1, s == " ++ show s
        put $ s + 1
        error "Erroring out, bye!" -- Try commenting me out
    step2 = do
        s <- get
        liftIO $ putStrLn $ "In step2, s == " ++ show s
        put $ s + 1
```

This seems to work, but there are a two problems:

* We were not able to reuse the standard `finally` function to implement this, since it wouldn't allow us to manually thread state. Practically, that means our implementation is susceptible to asynchronous exceptions (argh... those came up again).
* The behavior is pretty inconsistent. In one case, we use the original mutable state value and ignore the updated state from the cleanup function. In the other, we use the updated state value from the first function and keep the updated mutable state.

So let's instead try out a different approach. Regardless of whether an exception is thrown or not, we'll call the cleanup function using the original mutable state, and always ignore the state produced by the cleanup function.

```haskell active
import Control.Monad.State
import Control.Exception

finallyStateT :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT a sequel = do
    s1 <- get
    (result, s2) <- liftIO $ runStateT a s1 `finally`
                             runStateT sequel s1
    put s2
    return result

main :: IO ()
main = do
    res <- try $ runStateT
        (step1 `finallyStateT` step2)
        1
    print (res :: Either ErrorCall ((), Int))
  where
    step1 = do
        s <- get
        liftIO $ putStrLn $ "In step1, s == " ++ show s
        put $ s + 1
        error "Erroring out, bye!" -- Try commenting me out
    step2 = do
        s <- get
        liftIO $ putStrLn $ "In step2, s == " ++ show s
        put $ s + 1
```

This solves my two complaints from before, but unfortunately introduces a new one: the behavior is not really intuitive in the non-exception case. Nonetheless, in my opinion, this is the right way to implement things due to the consistency. We'll cover a method to get back the more intuitive semantics towards the end of this post.

## Second complication: alternative exit paths

The previous section essentially provided two approaches to writing a transformer-based `finally`: base it off of primitives like `catch`, or reuse `finally`. It turns out that both of these approaches have quite a bit of prior art. In the first case, there are the `MonadCatchIO-mtl` and `MonadCatchIO-transformers` packages, as well as the more recent `exceptions` package. These all define a typeclass for the primitive operations of `catch`ing and masking asynchronous exceptions (we just can't escape those, can we?).

Let's extract the `catch` logic from our first `StateT` example above to demonstrate the technique:

```haskell active
import Prelude hiding (catch)
import Control.Monad.State
import Control.Exception

-- show
catchStateT :: Exception e
            => StateT s IO a
            -> (e -> StateT s IO a)
            -> StateT s IO a
catchStateT a onE = do
    s1 <- get
    (result, s2) <- liftIO $ runStateT a s1 `catch` \e ->
        runStateT (onE e) s1
    put s2
    return result

finallyStateT :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT a sequel = do
    result <- a `catchStateT` \e -> do
        _ignored <- sequel
        liftIO $ throwIO (e :: SomeException)
    _ignored <- sequel
    return result
-- /show

main :: IO ()
main = do
    res <- try $ runStateT
        (step1 `finallyStateT` step2)
        1
    print (res :: Either ErrorCall ((), Int))
  where
    step1 = do
        s <- get
        liftIO $ putStrLn $ "In step1, s == " ++ show s
        put $ s + 1
        error "Erroring out, bye!" -- Try commenting me out
    step2 = do
        s <- get
        liftIO $ putStrLn $ "In step2, s == " ++ show s
        put $ s + 1
```

That's certainly a bit prettier than what we had before (though it's still not async-exception safe). And what's really nice is that this definition of `finallyStateT` doesn't actually have anything `StateT`-specific about it. So presuming we had a `catch` function for some other transformer, we could reuse the same definition. Let's try this out for `ErrorT`.

```haskell active
import Prelude hiding (catch)
import Control.Monad.Error
import Control.Exception

catchErrorT :: (Exception e, Error s)
            => ErrorT s IO a
            -> (e -> ErrorT s IO a)
            -> ErrorT s IO a
catchErrorT a onE = do
    eresult <- liftIO $ runErrorT a `catch` \e ->
        runErrorT (onE e)
    either throwError return eresult

finallyErrorT :: Error s => ErrorT s IO a -> ErrorT s IO b -> ErrorT s IO a
finallyErrorT a sequel = do
    result <- a `catchErrorT` \e -> do
        _ignored <- sequel
        liftIO $ throwIO (e :: SomeException)
    _ignored <- sequel
    return result

main :: IO ()
main = do
    res <- try $ runErrorT
        (step1 `finallyErrorT` step2)
    print (res :: Either ErrorCall (Either String ()))
  where
    step1 = do
        liftIO $ putStrLn $ "In step1"
        throwError "This should be interesting"
    step2 = liftIO $ putStrLn $ "In step2"
```

Go ahead and run that code snippet. Do you notice something not there to be noticed? That's right, the cleanup function is never called! Here we have a function called `finally` which, despite its name, doesn't actually call the cleanup function when it finally exits. What gives?

The issue is that, with an `ErrorT s IO` stack, there are __three__ ways to exit the function: normally, with a runtime exception, or with a `throwError` result. In `finallyErrorT`, the call to `catchErrorT` accounts for the runtime exception case, and the following call to `sequel` accounts for the normal exit case. But there's no way to account for the `throwError` case without adding `ErrorT`-specific logic to `finally` (we'll do that in a moment).

You can try this example out with `MonadCatchIO-mtl` or `MonadCatchIO-transformers` and reproduce this buggy behavior. This affects both the `ErrorT` and `ContT` transformers. In the `exceptions` package, this also applies to the `CatchT` type. (John Wiegley discussed this issue with Edward, who described this as a documentation bug, since `CatchT` should not be used on top of `IO`.)

Doing this correctly is certainly possible, as you can see with the following example:

```haskell active
import Control.Monad.Error
import Control.Exception

-- show
finallyErrorT :: Error s => ErrorT s IO a -> ErrorT s IO b -> ErrorT s IO a
finallyErrorT a sequel = do
    eresult <- liftIO $ runErrorT a `finally` runErrorT sequel
    either throwError return eresult
-- /show

main :: IO ()
main = do
    res <- try $ runErrorT
        (step1 `finallyErrorT` step2)
    print (res :: Either ErrorCall (Either String ()))
  where
    step1 = do
        liftIO $ putStrLn $ "In step1"
        throwError "This should be interesting"
    step2 = liftIO $ putStrLn $ "In step2"
```

But this seems to imply that we would need to write a separate implementation of `finally` for each and every transformer, which would be tedious and error-prone. There must be a better way.

## monad-control

If you look at our various implementations so far, they all follow a similar pattern: embedding the state of the monad inside the value, running the underlying monadic action, and then rebuilding the monadic state from the result. This procedure can be done with many different transformers (basically, all common transformers except `ContT`). To capture this concept, we have the `MonadTransControl` and `MonadBaseControl` typeclasses in the [`monad-control`](https://www.fpcomplete.com/haddocks/monad-control) package.

`monad-control` is frankly pretty complicated, and I'm not going to delve into all of its details here. Instead, I'll point you to the [`lifted-base`](https://www.fpcomplete.com/haddocks/lifted-base) package, which uses `monad-control` to create transformer-friendly versions of many common functions in `base`. As you'd expect based on this tutorial, exception handling functions are present, but also concurrency, timeouts, and mutable variables (which will become important in just a moment).

`monad-control` is- as far as I know- the third attempt at a library to cover these concepts. Previous solutions were `monad-peel` and my own [MonadInvertIO](http://hackage.haskell.org/packages/archive/neither/0.1.0/doc/html/Control-Monad-Invert.html), part of earlier versions of `neither`. While in my opinion these other two approaches are easier to understand, `monad-control` is highly optimized, and therefore gets my recommendation as the go-to library for handling monad transformer stacks.

## More intuitive mutable state

We're left with one annoyance with the `monad-control`-based solution: we lose any mutations performed to our mutable state either before an exception is thrown, or in our cleanup functions. Is there some way to keep the elegance of `monad-control` but get back the more intuitive behavior? The answer is yes, but you may not like it.

When dealing with either `StateT` or `WriterT` transformers, the behavior which I would consider most intuitive would be that any mutations are immediately captured. Unfortunately, this can't be achieved at all using the standard `StateT` and `WriterT` implementations, since the monadic state is thrown away as soon as an exception is thrown.

In this case, my solution is to use a mutable variable- such as an `IORef`- and keep a reference to it in a `ReaderT`. As an example, this technique is used by the <hoogle results="1">HandlerT</hoogle> transformer used in Yesod.

To demonstrate the concept, here's an implementation of the RWST monad transformer which uses `IORef`s for holding mutable state. For completeness, I'll include appropriate instances for `monad-control` and demonstrate usage of `lifted-base`.

```haskell active
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
import Control.Monad.RWS.Class
import Control.Monad.Reader
import Control.Monad.Base
import Control.Applicative (Applicative)
import Data.IORef.Lifted
import Data.Monoid (Monoid, (<>), mempty)
import Control.Exception.Lifted
import Control.Monad.Trans.Control

data Env r w s = Env
    { envReader :: !r
    , envWriter :: !(IORef w)
    , envState  :: !(IORef s)
    }

newtype RWST r w s m a = RWST (ReaderT (Env r w s) m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => MonadReader r (RWST r w s m) where
    ask = RWST $ liftM envReader ask
    local f (RWST g) =
        RWST $ local f' g
      where
        f' env = env { envReader = f $ envReader env }

instance (MonadBase IO m, Monoid w) => MonadWriter w (RWST r w s m) where
    tell w = RWST $ ask >>= flip modifyIORef (<> w) . envWriter
    listen (RWST (ReaderT f)) = RWST $ ReaderT $ \env -> do
        iwriter <- newIORef mempty
        result <- f env { envWriter = iwriter }
        w <- readIORef iwriter
        return (result, w)
    pass (RWST (ReaderT f)) = RWST $ ReaderT $ \env -> do
        (result, g) <- f env
        modifyIORef (envWriter env) g
        return result

instance MonadBase IO m => MonadState s (RWST r w s m) where
    get = RWST $ ask >>= readIORef . envState
    put s = RWST $ ask >>= flip writeIORef s . envState

instance (MonadBase IO m, Monoid w) => MonadRWS r w s (RWST r w s m)

runRWST :: (MonadBase IO m, Monoid w) => RWST r w s m a -> r -> s -> m (a, s, w)
runRWST (RWST (ReaderT f)) r s = do
    iwriter <- newIORef mempty
    istate <- newIORef s
    a <- f $ Env r iwriter istate
    w <- readIORef iwriter
    s' <- readIORef istate
    return (a, s', w)

instance MonadBase b m => MonadBase b (RWST r w s m) where
    liftBase = lift . liftBase

instance MonadTransControl (RWST r w s) where
    newtype StT (RWST r w s) a = StRWS {unStRWS :: a}
    liftWith f = RWST $ ReaderT $ \r -> f $ \(RWST t) -> liftM StRWS $ runReaderT t r
    restoreT = RWST . ReaderT . const . liftM unStRWS

instance MonadBaseControl b m => MonadBaseControl b (RWST r w s m) where
    newtype StM (RWST r w s m) a = ST { unST :: ComposeSt (RWST r w s) m a }
    liftBaseWith = defaultLiftBaseWith ST
    restoreM     = defaultRestoreM unST

main :: IO ()
main = do
    res <- try $ runRWST
        (step1 `finally` step2)
        "This is the environment"
        0
    print (res :: Either ErrorCall ((), Int, [Bool]))
  where
    step1 = do
        liftIO $ putStrLn $ "In step1"
        modify (+ 1)
        error "User exception"
    step2 = do
        s <- get
        liftIO $ putStrLn $ "In step2: " ++ show s
```

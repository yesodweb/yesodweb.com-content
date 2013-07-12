Note: This blog post is also [available on the School of
Haskell](https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions).
I'd recommend reading it there, as the active code snippets may greatly enhance
the material.

---

A commonly discussed piece of functionality is "catching all exceptions." The goal usually is to write reliable functions, which can recover from any kind of problem that exists in some library, or perhaps in some callback passed into the function, which the library author has no control over. Thanks to extensible exceptions, writing this kind of "catch any exception" is pretty trivial in Haskell:

```haskell active
import Control.Exception

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

dangerous :: IO Int
dangerous = error "Fool you!"

main :: IO ()
main = do
    result <- catchAny dangerous $ \e -> do
        putStrLn $ "Got an exception: " ++ show e
        putStrLn "Returning dummy value of -1"
        return (-1)
    print result
```

But this `catchAny` function isn't quite correct, due to asynchronous exceptions. I'd like to explain what the problem is, demonstrate a fix for it (inspired by John Lato using Simon Marlow's `async` library), and then generalize it even further using `monad-control`.

## Async exceptions

Let's consider the following theoretical workflow:

* I have a potentially exception-throwing function I want to run, called `dangerous`.
* This function should be run by a larger function, called `worker`. It should handle exceptions thrown by `dangerous` gracefully.
* I want to make sure that `worker` runs for no more than 5 milliseconds. I'll use the `timeout` function to ensure this.
* But unbeknownst to me, `dangerous` tends to take about 10 milliseconds.

Below is an implementation of the above logic, using the `catchAny` we defined earlier. Before you run this code, consider what the expected behavior here should be. In particular, should `worker` run to completion or not?

```haskell active
import Control.Exception
import System.Timeout
import Control.Concurrent

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

dangerous :: IO Int
dangerous = do
    putStrLn "Succeeds this time, but takes some time"
    threadDelay 10000
    return 5
    
worker :: IO ()
worker = do
    x <- catchAny dangerous $ \e -> do
        putStrLn $ "Caught an exception: " ++ show e
        return (-1)
    putStrLn $ "x + 10 == " ++ show (x + 10)

main :: IO ()
main = do
    res <- timeout 5000 worker
    case res of
        Nothing -> putStrLn "worker did not run to completion"
        Just () -> putStrLn "worker ran to completion"
```

In an ideal world, `worker` would be stopped before it finished, since it takes more than the 10 milliseconds provided to it to completely run. However, if you run the above code, you'll see that `worker` does in fact complete. What gives? Well, this is what *actually* happens when you run this code:

* The `timeout` function forks a new thread to run `worker` in. If `worker` does not complete within 5 ms, that new thread is thrown a timeout exception. This kind of throwing is done by the `throwTo` function, and is an *asynchronous exception*.
* Meanwhile, `worker` starts running, and wraps `dangerous` with `catchAny`.
* Since `dangerous` takes 10 ms, the timeout exception is called when the new thread is inside `dangerous`, which itself is inside `catchAny`. `dangerous` has no exception handling, so the exception propagates up to `worker`.
* `worker`'s `catchAny` catches all exceptions, and therefore treats the timeout exception as if it was thrown from `dangerous` itself. It therefore continues processing, completely ignoring the command to timeout.

This is a little tricky, so make sure you understand the situation properly before continuing.

## Non-solution: examine the types

My first inclination for solving this problem was to look at the types of the exception being caught. If it was a timeout exception, or any other kind of asynchronous exception, `catchAny` could simply ignore it. This looks something like:

```haskell active
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny m f =
    Control.Exception.catch m onExc
  where
    onExc e
        | shouldCatch e = f e
        | otherwise = throwIO e
    shouldCatch e
        | show e == "<<timeout>>" = False
        | Just (_ :: AsyncException) <- fromException e = False
        | otherwise = True
```

As you can see, this does in fact solve our problem. `catchAny` now ignores the timeout exception, so it is propagated to `worker`, terminating the computation. However, it has a few problems. I won't profess to understand all of the problems, but here's the most salient in my mind: __the types have nothing to do with whether an exception is synchronous or asynchronous__. Consider that, for some strange reason, we decided to asynchronously throw an `IOException` to a worker thread, e.g.:

```haskell active
main :: IO ()
main = do
    threadId <- forkIO worker
    eresult <- try $ readFile "does-not-exist.txt"
    case eresult of
        Left e -> throwTo threadId (e :: IOException)
        Right _ -> putStrLn "Funny, that shouldn't have worked"
    -- Give the forked thread time to finish
    threadDelay 50000
```

Since our `catchAny` knowns nothing about asynchronously thrown `IOException`s, our worker thread will continue doing work even after we try to kill the thread. This example is clearly a bit contrived, but consider if we had some kind of user quota system, where we send a custom asynchronous exception whenever a thread uses too much disk space. There's no way a generic `catchAny` could know about every kind of custom exception type a user defines. And even if we did, it's clear that any exception type could be thrown either synchronously or asynchronously.

## Real solution: separate worker thread

John Lato described a very straight-forward means of doing the right thing, leveraging Simon Marlow's excellent `async` library. In fact, that library is so excellent that it solved some of my implementation details before I even realized they existed... more on that in a moment.

The concept is simple: if you have some function you want to catch all exceptions for, fork a new thread and run the function there. Catch all exceptions thrown in that new thread, and return them to the original thread (via usage of software transactional memory). Now, if any async exceptions are thrown to the original thread, they are unaffected by the exception catching code. And the cool part that the `async` library took care of automatically: if the original thread gets an async exception, automatically propagate it down to the worker thread so that it terminates work immediately.

The amazing thing is just how simple this code is. We'll switch over to implementing `tryAny` instead of `catchAny`, since it's easier with the `async` library, and then we can build `catchAny` on top of that.

__Note__: the `async` library hasn't yet been deployed to the FP Haskell Center at time of writing, so I'll include the necessary code inline from `async`.

```haskell active
tryAny :: IO a -> IO (Either SomeException a)
tryAny action = withAsync action waitCatch

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny action onE = tryAny action >>= either onE return
```

Our solution is now very concise, built on top of quality libraries, and it resilient to any changes in the future to the exception hierarchy.

This is really our complete solution to the problem as described. The next two sections describe two optional enhancements to this solution.

## Going deeper

What we really want is to be completely isolated from any exceptions generated by a piece of code. The solution above is still vulnerable to one issue: exceptions from pure code hiding in an unevaluated thunk. Even the most brute force `catchAny` is susceptible to this problem.

```haskell active
import Control.Exception

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

dangerous :: IO Int
dangerous = return $ error "Unevaluated!"

main :: IO ()
main = do
    res <- catchAny dangerous (const $ return (-1))
    putStrLn "About to print the result"
    putStrLn $ "Result: " ++ show res
    putStrLn "Hmm... does this ever get printed?"
```

What we want to do is force evaluation of the value, and if forcing throws any exceptions, catch them. With the `deepseq` package, this is easy. Let's call these new functions `tryAnyDeep` and `catchAnyDeep`, and base them on our previously defined `tryAny`:

```haskell active
tryAnyDeep :: NFData a => IO a -> IO (Either SomeException a)
tryAnyDeep action = tryAny $ do
    res <- action
    return $!! res -- here's the magic

catchAnyDeep :: NFData a => IO a -> (SomeException -> IO a) -> IO a
catchAnyDeep action onE = tryAnyDeep action >>= either onE return

dangerous :: IO Int
dangerous = return $ error "Unevaluated!"

main :: IO ()
main = do
    res <- catchAnyDeep dangerous (const $ return (-1))
    putStrLn "About to print the result"
    putStrLn $ "Result: " ++ show res
    putStrLn "Hmm... does this ever get printed?"
```

We can now have complete\* confidence in the values returned from `catchAny`.

\* Complete confidence, assuming we trust the `NFData` instances, but that's a different problem.

## Transformers

OK, one final twist: can we catch exceptions in a monad transformer stack? Many of you may be aware that I'm a big advocate of Bas van Dijk's `monad-control` package, and the related `lifted-base` package. `monad-control` allows for a consistent manner of lifted control operations within a monad transformer stack. Can we generalize our `tryAny` and `catchAny` functions to work with arbitrary transformer stacks? Fortunately, we can:

```haskell active
tryAnyIO :: IO a -> IO (Either SomeException a)
tryAnyIO action = withAsync action waitCatch

tryAny :: MonadBaseControl IO m => m a -> m (Either SomeException a)
tryAny action =
    -- MAGIC!
    liftBaseWith (\runInIO -> tryAnyIO (runInIO action)) >>=
    either (return . Left) (liftM Right . restoreM)

catchAny :: MonadBaseControl IO m => m a -> (SomeException -> m a) -> m a
catchAny action onE = tryAny action >>= either onE return

tryAnyDeep :: (MonadBaseControl IO m, NFData a)
           => m a
           -> m (Either SomeException a)
tryAnyDeep action = tryAny $ do
    res <- action
    return $!! res -- here's the magic

catchAnyDeep :: (MonadBaseControl IO m, NFData a)
             => m a
             -> (SomeException -> m a)
             -> m a
catchAnyDeep action onE = tryAnyDeep action >>= either onE return

dangerous :: Monad m => m Int
dangerous = return $ error "Unevaluated!"

main :: IO ()
main = flip runReaderT () $ do
    res <- catchAnyDeep dangerous (const $ return (-1))
    liftIO $ putStrLn "About to print the result"
    liftIO $ putStrLn $ "Result: " ++ show res
    liftIO $ putStrLn "Hmm... does this ever get printed?"
```

That implementation of `tryAny` is a little bit hairy, but it essentially means:

* Capture the monadic state when we start running (via `liftWithBase`).
* Put the state inside the `IO` monad by modifying the internal value (via `runInIO`).
* Now that we have an `IO` action, run `tryAnyIO` on it.
* Get back the result.
    * If an exception was thrown, then return that exception.
    * If a value was returned, unwrap the new monadic state from and extract the actual return value (via `restoreM`).

## Moving forward

I've [added these functions](https://github.com/snoyberg/classy-prelude/blob/ccd19f2c62882c69d5dcdd3da5c0df1031334c5a/classy-prelude/ClassyPrelude.hs#L320) to the `classy-prelude` Github repo, and after a bit more testing will be releasing them. But I think including something like this in a more accessible place makes a lot of sense, as we should be trying to make the correct approach easier to implement.

I'd be happy to hear ideas on how to improve the code, or where the correct place to put these functions might be.

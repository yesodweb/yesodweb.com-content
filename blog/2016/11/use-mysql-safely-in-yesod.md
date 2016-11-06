With the latest version (0.1.4) of the `mysql` library, we now have the machinery needed to use it properly in a concurrent setting.  In the past, any multi-threaded use was a little risky, although in practice it seems to have been satisfactory for applications which were not too demanding.

The necessary changes have just been made to the MySQL version of the scaffolding, and are described here.  Existing Yesod sites should be updated in a similar manner.  This post should give you all you need to know, but further background can be found in the [MySQL manual](https://dev.mysql.com/doc/refman/5.7/en/c-api-threaded-clients.html) and [Roman Cheplyaka's blog](https://ro-che.info/articles/2015-04-17-safe-concurrent-mysql-haskell).


But It Worked Anyway, Didn't It?
--------------------------------

Let's start by reviewing why the `mysql` library works automatically in a *single-threaded* program, and why we might have got away with it most of the time in Yesod applications.

The underlying C library (`libmysqlclient`) requires a one-off initialisation, and then each thread in which it is called must be initialised to allocate some thread-local data.  However, these actions are carried out *automatically* when a `connect` call is made, if they have not already been done.  So nothing further is needed in a single-threaded program: a `connect` necessarily comes first, and it performs the required initialisations.

This behaviour of the `connect` call probably also explains why we have mostly got away with ignoring the problem in Yesod applications.  Warp creates lightweight, Haskell threads by default, and these run in a rather small number of OS threads.  When a new connection is opened and added to the pool, the OS thread running at the time will be initialised, as just described.  Due to the small number of these threads, there is a reasonable chance that this is the first database action in each of them, resulting in correct initialisation.  But there are no guarantees!


Correct Multi-Threaded Use
--------------------------

To be completely correct, we have to do all of the following:

* Initialise the library as a whole.
* Use [bound](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Concurrent.html#g:8) threads for those which might perform database operations.
* Initialise each thread properly.
* Finalise each thread to free the memory used by its thread-local state.

The library initialisation is not thread-safe; it needs to be called separately to ensure that subsequent `connect` calls, occurring in multiple threads, detect that it has been done and do not repeat it themselves.  This has been achieved in the scaffolding by calling `MySQL.initLibrary` from `makeFoundation`, before any database actions are carried out:


```
...
import qualified Database.MySQL.Base as MySQL
...
makeFoundation appSettings = do
    ...
    MySQL.initLibrary
```

The point about bound threads is that they provide a guarantee that related initialisation and database operations really do occur in the same OS thread.  However, using them means that OS threads are created frequently, and the argument given above no longer applies, not even as an approximation: the threads definitely need explicit initialisation.  They also need finalising to avoid a memory leak - again this is made important by the large number of threads.  (There are some situations in which the finalisation can be omitted, but check the [documentation](https://dev.mysql.com/doc/refman/5.7/en/mysql-thread-end.html) carefully before doing so.)

The settings passed to warp can be used to make it spawn bound threads, instead of Haskell threads, and to specify functions to initialise and finalise them.  This code shows how it is now done in the scaffolding, in `Application.hs`:

```
warpSettings foundation =
    ...
    $ setFork (\x -> void $ forkOSWithUnmask x)
    $ setOnOpen (const $ MySQL.initThread >> return True)
    $ setOnClose (const MySQL.endThread)
      defaultSettings
```

Warp forks a new thread to handle each connection, using the function specified by `setFork`.  The functions passed to `setOnOpen` and `setOnClose` are called right at the start of processing the connection, and right at the end, so they are valid places to initialise and finalise the thread for use by the `mysql` library.

The argument to `setFork` is a function which creates bound threads.  If you are wondering why it is written the way it is, instead of `void . forkOSWithUnmask`, it simply avoids the need for the `ImpredicativeTypes` language extension, which is considered [fragile](https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism) and is sometimes [broken by new compiler releases](https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0)!

Unfortunately, `forkOSWithUnmask` is not exported by Control.Concurrent until base-4.9 (ie GHC 8), so, when using earlier versions, we have to copy its definition into our code:

```
{-# LANGUAGE RankNTypes           #-}
...
import GHC.IO                               (unsafeUnmask)
...
forkOSWithUnmask :: ((forall a . IO a -> IO a) -> IO ()) -> IO ThreadId
forkOSWithUnmask io = forkOS (io unsafeUnmask)
```


### What About Efficiency?

OS threads are more expensive than Haskell threads, but the difference may not matter much compared to all the other processing which is going on in a real application.  It would be wise to do some benchmarks before worrying about it!

One possible optimisation is to make sure that HTTP keepalive is used, since warp creates a thread per connection, not per request.  Some reverse proxies might need explicit configuration for this.

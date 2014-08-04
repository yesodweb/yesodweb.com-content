Kazu and I are happy to announce the first release of
[auto-update](http://hackage.haskell.org/package/auto-update-0.1.0.0), a
library to run update actions on a given schedule. To make it more concrete,
let's start with a motivating example.

Suppose you're writing a web service which will return the current time. This
is simple enough with WAI and Warp, e.g.:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Time                  (formatTime, getCurrentTime)
import           Network.HTTP.Types         (status200)
import           Network.Wai                (responseLBS)
import           Network.Wai.Handler.Warp   (run)
import           System.Locale              (defaultTimeLocale)

main :: IO ()
main =
    run 3000 app
  where
    app _ respond = do
        now <- getCurrentTime
        respond $ responseLBS status200 [("Content-Type", "text/plain")]
                $ pack $ formatTime defaultTimeLocale "%c" now
```

This is all well and good, but it's a bit inefficient. Imagine if you have a
thousand requests per second (some people *really* like do know what time it
is). We will end up recalculating the string representation of the time a 999
extra times than is necessary! To work around this, we have a simple solution:
spawn a worker thread to calculate the time once per second. (Note: it will
actually calculate it slightly less than once per second due to the way
`threadDelay` works; we're assuming we have a little bit of latitude in
returning a value thats a few milliseconds off.)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever)
import           Data.ByteString.Lazy.Char8 (ByteString, pack)
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import           Data.Time                  (formatTime, getCurrentTime)
import           Network.HTTP.Types         (status200)
import           Network.Wai                (responseLBS)
import           Network.Wai.Handler.Warp   (run)
import           System.Locale              (defaultTimeLocale)

getCurrentTimeString :: IO ByteString
getCurrentTimeString = do
    now <- getCurrentTime
    return $ pack $ formatTime defaultTimeLocale "%c" now

main :: IO ()
main = do
    timeRef <- getCurrentTimeString >>= newIORef
    _ <- forkIO $ forever $ do
        threadDelay 1000000
        getCurrentTimeString >>= writeIORef timeRef
    run 3000 (app timeRef)
  where
    app timeRef _ respond = do
        time <- readIORef timeRef
        respond $ responseLBS status200 [("Content-Type", "text/plain")] time
```

Now we will calculate the current time once per second, which is far more
efficient... right? Well, it depends on server load. Previously, we talked
about a server getting a thousand requests per second. Let's instead reverse
it: a server that gets one request every thousand seconds. In that case, our
optimization turns into a pessimization.

This problem doesn't just affect getting the current time. Another example is
flushing logs. A hot web server could be crippled by flushing logs to disk on
every request, whereas flushing once a second on a less popular server simply
keeps the process running for no reason. One option is to put the power in the
hands of users of a library to decide how often to flush. But often times, we
won't know until runtime how frequently a service will be requested. Or even
more complicated: traffic will come in spikes, with both busy and idle times.

(Note that I've only given examples of running web servers, though I'm certain
there are plenty of other examples out there to draw from.)

This is the problem that auto-update comes to solve. With auto-update, you
declare an update function, a frequency with which it should run, and a
threshold at which it should "daemonize". The first few times you request a
value, it's calculated in the main thread. Once you cross the daemonize
threshold, a dedicated worker thread is spawned to recalculate the value. If
the value is not requested during an update period, the worker thread is shut
down, and we go back to the beginning.

Let's see how our running example works out with this:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.AutoUpdate         (defaultUpdateSettings,
                                             mkAutoUpdate, updateAction)
import           Data.ByteString.Lazy.Char8 (ByteString, pack)
import           Data.Time                  (formatTime, getCurrentTime)
import           Network.HTTP.Types         (status200)
import           Network.Wai                (responseLBS)
import           Network.Wai.Handler.Warp   (run)
import           System.Locale              (defaultTimeLocale)

getCurrentTimeString :: IO ByteString
getCurrentTimeString = do
    now <- getCurrentTime
    return $ pack $ formatTime defaultTimeLocale "%c" now

main :: IO ()
main = do
    getTime <- mkAutoUpdate defaultUpdateSettings
        { updateAction = getCurrentTimeString
        }
    run 3000 (app getTime)
  where
    app getTime _ respond = do
        time <- getTime
        respond $ responseLBS status200 [("Content-Type", "text/plain")] time
```

If you want to see the impact of this change, add a `putStrLn` call to
`getCurrentTimeString` and make a bunch of requests to the service. You should
see just one request per second, once you get past that initial threshold
period (default of 3).

Kazu and I have started using this library in a few places:

* fast-logger no longer requires explicit flushing; it's handled for you automatically.
* wai-logger and wai-extra's request logger, by extension, inherit this functionality.
* Warp no longer has a dedicated thread for getting the current time.
* The Yesod scaffolding was able to get rid of [an annoying bit of commentary](https://github.com/yesodweb/yesod-scaffold/commit/eb3046107fb29d1d2651d4cb83dc5bb90250a65e#diff-4ff81c1023e92f161457e96254132f46L73).

Hopefully others will enjoy and use this library as well.

## Control.Reaper

The second module in auto-update is `Control.Reaper`. This provides something
similar, but slightly different, from `Control.AutoUpdate`. The goal is to
spawn reaper/cleanup threads on demand. These threads can handle such things
as:

* Recycling resources in a resource pool.
* Closing out unused connections in a connection pool.
* Terminating threads that have overstayed a timeout.

This module is currently being used in Warp for slowloris timeouts and file
descriptor cache management, though I will likely use it in http-client in the
near future as well for its connection manager management.

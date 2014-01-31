# Faster and scalable fast-logger

As you may know, Michael and I released a set of packages for [WAI 2.0](http://hackage.haskell.org/package/wai) including [Warp 2.x](http://hackage.haskell.org/package/warp) and [fast-logger 2.x](http://hackage.haskell.org/package/fast-logger).
They are much faster than their old versions.
I will explain how I re-designed fast-logger in this article and how I improved the performance of Warp in the next one.

## The coming GHC 7.8 and Handle

One of the coolest features of the coming GHC 7.8 is [multicore IO manager](http://haskell.cs.yale.edu/wp-content/uploads/2013/08/hask035-voellmy.pdf).
The runtime system of GHC 7.6 or earlier provides parallel GC, efficient memory allocation mechanism and the IO manger.
So, a concurrent program complied by GHC 7.6 or earlier should be scaled on a multicore system if the +RTS -Nx command-line option is specfied.

Unfortunately, it appeared that this is not the case.
This is mainly because the IO manager has several bottlenecks.
Andreas Voellmy analyzed the bottlenecks and implemented the multicore IO manager (mainly for Linux).
I helped him in testing it and porting it to BSD variants. 

If you compile your concurrent program by the coming GHC 7.8, the executable scales on multicores.
No modifications are necessary.
Just specify the +RTS -Nx option (and -qa -A32m if necessary).
It is really nice, isn't it?

However, GHC 7.8 would disclose another bottleneck of your program.

I noticed this when I was trying to get Mighty prepared for GHC 7.8.
Mighty 2.x uses the pre-fork technique to scale on multicore systems.
That is, Mighty 2.x forks processes according to its configuration file to get over the bottlenecks the IO manager.
If you want to know the pre-fork technique in detail, please read [Mighttpd – a High Performance Web Server in Haskell](http://themonadreader.wordpress.com/2011/10/26/issue-19/).

I modified Mighty with the pre-fork code removed because it will be not necessary for GHC 7.8.
This change made Mighty much simpler.
This is why I spent much my time to develop the multicore IO manager.
I bumped up the version of Mighty to 3.

Mighty 3 scaled on multicore systems as I expected when logging is off. Unfortunately, Mighty 3 with logging enabled does not scales at all.
For instance, I run Mighty 3 with logging on a 12 core machine.
The performance of +RTS -N10 is *worse* than that of +RTS -N1.

Why? That is because fast-logger uses `Handle`. Since this `Handle` is shared by all user threads and `Handle` is protected with `MVar`, the `Handle` is a global giant lock!

## Re-designing fast-logger

As I wrote in [Mighttpd – a High Performance Web Server in Haskell](http://themonadreader.wordpress.com/2011/10/26/issue-19/), I tested many ideas when I implemented fast-logger at the beginning. `Handle` is the fastest among them but the performance of web servers loses 49% if fast-logger is enabled. I did not have other speed-up techniques at that time.

The experiment above reminds me this performance issue of logging. After working with Michael and Andreas, I became familiar with GHC much more in detail. Now I can design new fast-logger.

Logger consists of a buffer, its size, and a reference to a log message queue:

    data Logger = Logger (MVar Buffer) !BufSize (IORef LogStr)

A log message is defined as follows:

    data LogStr = LogStr !Int Builder

    instance Monoid LogStr where
        mempty = LogStr 0 (toBuilder BS.empty)
        LogStr s1 b1 `mappend` LogStr s2 b2 = LogStr (s1 + s2) (b1 <> b2)

That is, log messages are `Builder` with its length. Because a log message is an instance of `Monoid`, a log message itself behaves as a queue. We can append a log message to a queue with (<>) in O(1). Atomic append operation is ensured with `atomicModifyIORef'`.

Here is a simplified code to append a log message to a queue. (Note that `pushLog` is not disclosed.)

    pushLog :: FD -> Logger -> LogStr -> IO ()
    pushLog fd logger@(Logger mbuf size ref) nlogmsg@(LogStr nlen nbuilder) = do
        mmsg <- atomicModifyIORef' ref checkBuf
        case mmsg of
            Nothing  -> return ()
            Just msg -> withMVar mbuf $ \buf -> writeLogStr fd buf size msg
      where
        checkBuf ologmsg@(LogStr olen _)
          | size < olen + nlen = (nlogmsg, Just ologmsg)
          | otherwise          = (ologmsg <> nlogmsg, Nothing)


When a log message is appended to a queue, its total length is compared with the current buffer size (in `checkBuf`). If the total length is bigger than the buffer size, the queue is swapped with the log message. Then, the buffer is locked. The log messages in the old queue are copied into the buffer then the buffer is wrote into its corresponding file. Otherwise, the log message is just appended to the queue.

Why is this new approach fast? Well, the new one takes a lock only when it flushes its buffer and the locking can be obtained in almost all cases. But the old one tries to take a lock everytime when each message is copied into `Handle`'s buffer.

To my experiment, this is not good enough yet to scale on multicore systems. So, I prepared `Logger` per core.
It is really effective.
The API provides the following abstract data type:

    data LoggerSet = LoggerSet (Maybe FilePath) (IORef FD) (Array Int Logger)

You can create `LoggerSet` with the following APIs:

    newFileLoggerSet :: BufSize -> FilePath -> IO LoggerSet
    newStdoutLoggerSet :: BufSize -> IO LoggerSet
    newStderrLoggerSet :: BufSize -> IO LoggerSet

To log a message, `pushLogStr` is used:

    pushLogStr :: LoggerSet -> LogStr -> IO ()

When a user thread calls `pushLogStr`, a `Logger` is selected according to the core number on which the thread is running.

The new fast logger loses only about 10% of performance on any numbers of cores.

I would like to thank Michael Snoyman, Toralf Wittner, Tobias Florek, and Gregory Collins for their contributions to fast-logger.

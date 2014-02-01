# Improving the performance of Warp again

As you may remember, I improved the performance of Warp in 2012 and wrote [some blog articles](http://www.yesodweb.com/blog/2012/09/improving-warp) on this web site. Based on these articles, Michael and I wrote an article: ["Warp"](http://aosabook.org/en/posa/warp.html) for POSA, The Performance of Open Source Applications.

In the last year after working with Andreas, I hit upon some ideas to make Warp faster and implemented them. In this article, I will explain how I improved the performance of Warp again. If you have not read the POSA article, I recommend to give a look at it before reading this article.

Here are items to be explained:

- Better thread scheduling
- Buffer allocation to receive HTTP requests
- Buffer allocation to send HTTP response
- HTTP request parser

## witty

When I was testing [multicore IO manager](http://haskell.cs.yale.edu/wp-content/uploads/2013/08/hask035-voellmy.pdf) with Mighty, I noticed bottlenecks of network programs complied by GHC. To show such bottlenecks actually exist, I wrote a server program called [witty](https://github.com/kazu-yamamoto/witty).

"witty" provides seven command line options to switch standard methods to alternatives. My experiment with "witty" disclosed the following bottlenecks:

- Thread scheduling (the "-y" option)
- `Network.Socket.ByteString.recv` (the "-p" option)

I will explain these in order.

## Better thread scheduling

GHC's I/O functions are optimistic. Let's consider `recv`.
It first tries to read incoming data anyway.
If the data are available, `recv` succeeds.
Otherwise, `EAGAIN` is returned.
In this case, `recv` asks the IO manager to notify when the data are available.

If a network server repeats receive/send actions,
`recv` just after `send` probably fails because
there is a time lag for the next request from the client.
Thus the IO manager works frequently.

    recvfrom(13, )                -- Haskell thread A
    sendto(13, )                  -- Haskell thread A
    recvfrom(13, ) = -1 EAGAIN    -- Haskell thread A
    epoll_ctl(3, )                -- Haskell thread A (a job for IO manager)
    recvfrom(14, )                -- Haskell thread B
    sendto(14, )                  -- Haskell thread B
    recvfrom(14, ) = -1 EAGAIN    -- Haskell thread B
    epoll_ctl(3, )                -- Haskell thread B (a job for IO manager)

The idea is to call `yield` after `send` for better scheduling.
`yield` pushes its Haskell thread onto the end of thread queue. So,
another thread can work. During the work of other threads, a request
message would arrive.

    recvfrom(13, )                -- Haskell thread A
    sendto(13, )                  -- Haskell thread A
    recvfrom(14, )                -- Haskell thread B
    sendto(14, )                  -- Haskell thread B
    recvfrom(13, )                -- Haskell thread A
    sendto(13, )                  -- Haskell thread A

In other words, `yield` makes the IO manager work less frequently.
This magically improves throughput.

This means that even multicore IO manager still has significant overhread.
It uses `MVar` to notify data availability to Haskell threads.
Since `MVar` is a lock, it may be slow.
Or, allocation of `MVar` may be slow.

Unfortunately when I added `yeild` to Warp, no performance improvement is gained. It seems to me that Monad stack (`ResourceT`) handcuffs the `yield` hack. So, Michael removed `ResourceT` from WAI. This is why the type of `Application` change from:

    type Application = Request -> ResourceT IO Response

to:

    type Application = Request -> IO Response

This change itself improved the performance and enables the `yield` hack resulting in drastic performance improvement at least on small numbers of cores.


## Buffer allocation to receive HTTP requests


## Buffer allocation to send HTTP response


## HTTP request parser

    sendfileloop                    Network.Sendfile.Linux                    7.5    0.0
    parseReqeustLine                Network.Wai.Handler.Warp.RequestHeader    3.6    5.8
    sendloop                        Network.Sendfile.Linux                    3.6    0.0
    serveConnection.recvSendLoop    Network.Wai.Handler.Warp.Run              3.1    1.9
    >>=                             Data.Conduit.Internal                     2.9    3.9

After writing a low-level persor for HTTP requests:

    sendfileloop                  Network.Sendfile.Linux                    8.3    0.0
    sendResponse                  Network.Wai.Handler.Warp.Response         3.7    3.1
    sendloop                      Network.Sendfile.Linux                    3.2    0.0
    >>=                           Data.Conduit.Internal                     2.9    4.0
    serveConnection.recvSendLoop  Network.Wai.Handler.Warp.Run              2.6    2.0

## Acknowledgment

I thank Joey Hess for letting Warp work well on Windows and
Gregory Collins for discussion on performance.

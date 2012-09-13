For last one month, I have been improving the performance of Warp and
got a good result.  I would like to explain why I did it and what I
have actually done in several blog articles. In the articles, I will
only talk about GHC's threaded RTS.

In 2009, I started to implement my original Web server, Mighttpd
(called Mighty) using GHC 6. Since I also implemented a mail related
server before, I knew the limitation of GHC 6's IO manager:

1. It is implemented using the select() system call. 
  So, the IO manager cannot handle TCP connections over 1,024.
2. Since the number of IO manager is one, it cannot utilize
  multi-core.

To solve these two problems, I hit upon an idea to use the pre-fork
technique. This is, before accepting new TCP connections, some
number of child processes are created by the fork() system call.
The child processes shares its listening port and OS selects
one of the child processes for a new TCP connection.

As many know, Bryan O’Sullivan and Johan Tibell improved the IO
manager by using modern system calls, the epoll family on Linux and
the kqueue family on BSD/Mac. The new IO manager was integrated into
GHC 7.

GHC 7 solved the problem 1 but problem 2 still remains. It seems to me
that servers complied with GHC 7 may not get great performance out of
multi-core even if we specify the +RTS -Nx option. To my experience,
the prefork technique is still effective and creating N child
processes is a good workaround where N is the number of cores.

I decided to switch my HTTP engine to Warp and I re-implemented
Mighttpd as an HTTP logic on the top of Warp. It is now called
Mighttpd 2. It still uses the prefork technique for the reason above.

Since some people started to use Mighttpd 2 in the real world, I
enhanced it to support dynamic re-configuration and graceful
shutdown. To implement these features, I needed to write some
boilerplate for inter-process communication using UNIX signals.
It was really boring and I convinced that making the IO manager
parallel is very important.

When I sent messages about the parallel IO manager to GHC HQ, Simon
Marlow kindly told me that Andreas Voellmy, who is a Ph D student
under Paul Hudak, has already implemented it on Linux. So, I joined
his activity and started to test his new IO manager with Mighttpd 2.

Andreas said to me that Mighttpd 2/Warp is not fast enough to place
burden on the parallel IO manager. He pointed out that issuing
open()/close() system calls and the HTTP response composer are
bottlenecks. So, I started to improve the performance of Warp.  I will
explain what I have actually done in the coming articles.

Meanwhile, it would be nice to read
["The Architecture of the Mighttpd High-Speed Web Server"](http://www.iij.ad.jp/en/company/development/tech/mighttpd/)
to understand the benefits of network programming in Haskell.

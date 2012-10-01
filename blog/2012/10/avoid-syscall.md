This is fifth article for the series of "Improving the performance of Warp".
Readers are supposed to read the following articles:

1. [Improving the performance of Warp](../9/improving-warp)
2. [Sending header and body at once](../9/header-body)
3. [Caching file descriptors](../9/caching-fd)
4. [Composing HTTP response headers](../9/header-composer)

In this article, I will explain how to avoid the `fcntl()` system call
and the `gettimeofday()` vsyscall.

## Avoiding `fcntl()`

I sometimes compare Mighttpd and nginx with the results of `strace`
on Linux and/or `ktrace` on BSD variants.
One day, I noticed that nginx uses the `accept4()` system call, which I did not know about
at that moment.
(I used to be an expert of BSD variants but am a newbie to Linux.)

In the low level of GHC, file/socket operations are basically implemented as non-blocking.
If the `network` package is used,
a listening socket is created with the non-blocking flag set.
When a new connection is accepted from the listening socket,
it is necessary to set the corresponding socket as non-blocking, too.
The `network` package implements this by calling `fcntl()` twice:
one is to get the current flags and the other is to set
the flags with the non-blocking flag *ORed*.

On Linux, the non-block flag of a connected socket
is always unset even if its listening socket is non-blocking.
The `accept4()` system call is an extension version of `accept()` on Linux.
It can set the non-blocking flag when accepting.
So, if we use `accept4()`, we can avoid two unnecessary `fcntl()`s.
I modified the `network` package to use `accept4` on Linux
and it is included in version 2.3.1.0 or later.

On BSD variants
the non-block flag of a connected socket is inherited
from its listening socket.
So, we can also avoid two unnecessary `fcntl()`s
on BSD variants.
But I have not implemented this.

## Avoiding `gettimeofday()`

Date strings are used in various ways. An end HTTP server
should return GMT date strings in header fields such as
Date:, Last-Modified:, etc:

    Date: Mon, 01 Oct 2012 07:38:50 GMT

For logging, a local date string in the Apache style would be convenient:

    01/Oct/2012:16:42:33 +0900

It is known that `formatTime` in the `Data.Time.Format` module is too
slow for high performance servers.
Years ago, I implemented faster format packages: `http-date`
for the former and `unix-time` for the latter.
Unfortunately, they are still slow for high performance servers.
And if an HTTP server accepts more than one request per second,
the server repeats the same formatting again and again.
So, formatted date strings should be cached.

The members of the `web-devel` mailing-list discussed these issues
with the following assumption:

1. Formatting time to date string is a heavy job
2. Getting the current time by `gettimeofday()` is a light job

The discussion resulted in the following algorithm:

- When a formatted date is required, first issue `getttimeofday()`. Then compare it with a cached time.
- If they are equal, return the cached formatted date.
- Otherwise, format the new time to a new formatted date, cache them, and return the new formatted date.

It seems to me that the assumption 2 is not correct.
`gettimeofday()` was a system call in old Linux
while it is a vsyscall in new Linux.
To my experience, I cannot say that
calling `gettimeofday()` is a light job on both old and new Linux.

So, I implemented a new algorithm:

- Designated Haskell thread issues gettimeofday() every second, formats the result time to a date, and caches it.
- When a formatted date is required, the cached formatted date is simply returned.

Only end HTTP servers should return the Date: header field.
Since Warp can be used to implement proxies,
adding Date: is not warp's job.
Also, logging is not warp's job, either.
WAI applications should take care of
both adding Date: and logging.

Yesod and Mighttpd both use the `fast-logger` package for logging.
I modified it so that it provides both algorithms above.
Also, I changed Mighttpd to use the new algorithm
to generate Date:.
The packages with these modification are
already available from Hackage.



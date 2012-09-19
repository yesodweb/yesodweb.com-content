This article explains one technique to improve the performace of
the simple-sendfile package on which Warp relies.
Readers are supposed to read "[Improving the performance of Warp](improving-warp)".

Before Andreas suggested me that the open()/close() system calls are
a performance bottleneck, I found another bottleneck in the simple-sendfile
package. Before talking how to avoid open()/close(), I would like to
start with how to fix the latter bottleneck. I believe that this is
a good introduction to understanding the architecture of Warp.

Warp is an HTTP engine for WAI (Web Application Interface). It runs WAI
applications over HTTP. The type of WAI applications is as follows:

    type Application = Request -> ResourceT IO Response

That is, an application takes `Request` and returns
`Response`. As you can guess, Warp first receives an HTTP request
from a client and parses it to `Request`. Then, Warp gives the `Request`
to an application and takes a `Response` from it. Finally, Warp builds
an HTTP response based on `Response` and sends it back to the
client.

As I pointed out in "[Mighttpd – a High Performance Web Server in Haskell](http://themonadreader.files.wordpress.com/2011/10/issue19.pdf)", "system calls are evil for network programming in Haskell". So, Warp is defined to use as few system calls as possible.

In the HTTP request parser, Warp uses `Network.Socket.ByteString.recv` which issues
the recv() system call. If a client uses HTTP pipelining,
multiple HTTP requests can be received with one recv().

In the HTTP response builder, the situation is complicated because
`Response` has three constructors:

    ResponseBuilder Status ResponseHeaders Builder
    ResponseSource Status ResponseHeaders (Source (ResourceT IO) (Flush Builder))
    ResponseFile Status ResponseHeaders FilePath (Maybe FilePart)

For `ResponseBuilder` and `ResponseSource`,
`Network.Socket.ByteString.sendAll`- based on the send() system call-
is used to send both an HTTP response header and body with a fixed buffer.

For `ResponseFile`, the old Warp used
`Network.Socket.ByteString.sendMany` to send an HTTP response header
and `Network.Sendfile.sendfile` to send a file as an HTTP response body.
They are based on the writev() and sendfile() system call, respectively.
The combination of writev() and sendfile() had a problem.

When I measured the performance of Warp,
I always did it with high concurrency. That is, I
always make multiple connections at the same time. It gave me a good
result. However, when I set the number of concurrency to 1, I found
Warp is really really slow on Linux and FreeBSD.

I realized that this is because Warp uses
the combination of writev() and sendfile().
In this case, an HTTP header and body are sent in separate TCP packets.
To send them in a single TCP packet (when possible),
I implemented a new function called
`Network.Sendfile.sendfileWithHeader`,
which is available in simple-sendfile v0.2.4 or later.

On Linux, it uses the send() system call with the `MSG_MORE` flag to store
a header and the sendfile() system call to send both the stored header and
a file. On FreeBSD, it uses the sendfile() system call with the header
arguments to send both a header and a file. This trick ensures that
both a header and a body is sent in a single TCP packet (if the file is small enough).

This makes Warp at least 100 times faster when measured with `httperf`.

I will explain how to avoid the open()/close() system calls
used in `Network.Sendfile.sendfileWithHeader` in the next article.

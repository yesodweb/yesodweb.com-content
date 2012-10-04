I'm happy to announce the release of three new packages:
[http-reverse-proxy](http://hackage.haskell.org/package/http-reverse-proxy),
[unix-process-conduit](http://hackage.haskell.org/package/unix-process-conduit),
and
[network-conduit-tls](http://hackage.haskell.org/package/network-conduit-tls).
Additionally, there is a new version of
[network-conduit](http://hackage.haskell.org/package/network-conduit) which
provides an improved interface.

Believe it or not, there's a reason that I'm announcing these four different
packages at the same time: put together, they provide a lot of the
functionality necessary to create a virtual hosting system working over both
HTTP and HTTPS. This work directly benefits the
[keter](http://hackage.haskell.org/package/keter) deployment system. I'm still
testing these changes to keter, so the new version 0.3 has *not* yet been
released.

These new packages should be considered experimental. They are the basis of
some real life code at FP Complete, but it's early in the testing cycle, so I
cannot give any strong assurances that it will work perfectly. Nonetheless,
these packages provide some interesting features that could be useful to
others.

### network-conduit and network-conduit-tls

`network-conduit` has been around for a while now. It provides a simple
conduit-based interface for creating TCP servers and clients. This 0.6 release
enhances the interface a bit:

* You can register an action to be called after binding the listening socket.
  This is useful for apps which bind to a restricted port and then setuid to a
  non-privileged user.
* The `SockAddr` is provided to both server and client applications.
* By using [settings types](http://www.yesodweb.com/book/settings-types), it
  should be easy to make additions to the API going forward without breaking
  backwards compatibility.

The new addition here is `network-conduit-tls`, which allows you to create TCP
servers over an SSL connection. What's very nice is that it uses the same
interface for applications as the regular version, so you can write
applications that will run over either secure or insecure. This comes into play
with the next library.

__Note__: Currently `network-conduit-tls` doesn't provide a client interface.
There are some complications involved there, such as providing a certificate
approval mechanism. These aren't *difficult* problems necessarily
(`http-conduit` already does this, for example) but I decided not to include
the functionality right now. If people are interested in adding it, please
contact me.

### http-reverse-proxy

This library provides two different ways of reverse proxying. The more
traditional approach leverages both WAI and http-conduit: it provides a WAI
application and makes requests to some backend host. In this scenario, the
request and response are fully parsed by the reverse proxy, which provides
opportunities for making modifications, but can also introduce a performance
overhead. Additionally, this approach can get in the way of tunneling non-HTTP
data, such as websockets.

The second approach is more light-weight. Only the initial HTTP headers are
parsed, in order to determine how to route your request. Then, all further
communications are simply passed to and from the backend server without any
further parsing. This means that, after the initial HTTP headers, the content
could be completely invalid HTTP... which in the case of websockets is actually
an advantage.

Both approaches allow you to have a very dynamic routing table: you must
provide an action which will be run on each connection to determine where to
route to. You could even go so far as to modify the routing table based on
incoming HTTP requests if you wanted to.

One final feature is the ability to convert a WAI application to a raw
`network-conduit`-style application. This is done by using some low-level
functions provided by Warp to parse and render the requests and responses. This
makes it possible to provide a single server which can either locally serve
responses, or pass on to a different server, depending on the request headers.

### unix-process-conduit

The main motivation behind the creation of this library was actually *not* for
the `conduit` interface. It was mainly to be able to have better control of
termination of processes. To quote `System.Process.terminateProcess`'s
documentation:

> This function should not be used under normal circumstances - no guarantees
> are given regarding how cleanly the process is terminated.

For my use cases, I needed guarantees that a process was really going to die.
We actually ran into this problem historically with `yesod devel`, and used
some complicated techniques with flag files to work around it. If we can come
up with a Windows counterpart to `unix-process-conduit`, we'll be able to avoid
that kludge.

But once I was writing a library for processes, I decided to go ahead and model
the input and output streams via conduits. The `forkExecuteFile` function takes
a `Maybe (Source IO ByteString)` parameter for stdin. If it's `Nothing`, then
the subprocess keeps the same standard input as the parent. Otherwise, stdin is
taken from the given `Source`. If all you want to do is provide no input to the
child, you can use `Just $ return ()`.

Likewise, for stdout and stderr the parameter type is `Maybe (Sink ByteString
IO ())`. To simply ignore all output, you can use `Just $
Data.Conduit.List.sinkNull`.

* * *

As I mentioned, all of the new libraries are still to be considered
experimental. If you have ideas for API improvements or find any bugs, please
let know.

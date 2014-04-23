The Haskell Web Application Interface- or WAI- serves as a low-level interface
between web applications and servers. In order to do this in a
resource-efficient manner, it avoids lazy I/O and instead uses explicit
streaming. The story of how it does that streaming has evolved over time: it
[initially](http://hackage.haskell.org/package/wai-0.0.0) used its own
home-brewed streaming abstraction.
[Later](http://hackage.haskell.org/package/wai-0.3.0), Gregory Collins
convinced me to switch over to enumerator. In the [1.0
release](http://hackage.haskell.org/package/wai-1.0.0), it switched to conduit,
which was essentially designed for the very purpose of supporting WAI's use
cases. While I'm very happy with conduit, baking conduit into WAI makes the
barrier to using a different streaming framework (like pipes) higher.

So today, I'm proposing that we go all the way back to the beginning, and
remove dependencies on external streaming frameworks from WAI, making it a
completely universal web interface for Haskell.

I've been hesitant about doing this in the past due to two different reasons:

1. Making this change makes it more difficult to writes handlers and middleware
   for WAI.
2. It's not really possible to get rid of a streaming abstraction entirely;
   instead, we end up just having a locally baked abstraction, which is less
   thoroughly tested than existing abstractions.

On the first point, most middlewares and handlers which modify request and
response bodies are already maintained by the WAI team (and mostly by me
personally), so I'm less concerned about pushing such a burden out onto the
community. Thankfully, applications and frameworks can be completely insulated
by this change by providing a wai-conduit adapter package.

Regarding the second point, I've recently had some experience with this:
refactoring [http-client](http://hackage.haskell.org/package/http-client) out
of [http-conduit](http://hackage.haskell.org/package/http-conduit). It turned
out to be relatively painless, though I did end up having to reimplement a
number of combinators from conduit (especially in the test suite). Nonetheless,
the codebase is about the same level of complexity, given the low-level nature
of the http-client library, and there's been an overwhelmingly positive
response to the splitting up of those two packages, so I want to try it out
with WAI as well.

I've [created a no-conduit
branch](https://github.com/yesodweb/wai/tree/no-conduit) in the WAI repo.
Currently, I've converted the wai and warp repos over to be conduit-free (with
a few helper functions stubbed out). And Warp passes its full test suite, which
is rather promising.

The streaming interface is relatively simple. In order to consume a request body, you use the function:

    requestBody :: Request -> IO ByteString

This function returns the next strict `ByteString` from the request body in the
stream, or an empty `ByteString` if the body has been fully consumed. On the
response side, you write an application with the type:

    (Maybe Builder -> IO ()) -> IO ()

The argument function is used for emitting data to the user. If you provide a
`Just` value, it sends the data in, and a `Nothing` value flushes the stream.
(Currently, WAI uses the `Flush` data type from conduit for this purpose.)

The code is not yet ready to be released, but it is ready for some review and
discussion. I'm hoping to hear community feedback, both from current users of
WAI, and those who are considering using it (either directly in an application,
or as part of a framework).

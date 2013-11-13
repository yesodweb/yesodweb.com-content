Kazu and I have been [working on some
changes](https://github.com/yesodweb/wai/tree/wai2) to WAI and Warp for a 2.0
release. At this point, most of the API changes are done, and could benefit
from some input from the rest of the community. At the same time, the work
we've been doing inspired me to finally write the http-client library, which is
intended to be the new underlying engine for http-conduit in its upcoming 2.0
release. This blog post is intended to give an overview of the coming changes.

## WAI

I originally [sent an email to
web-devel](http://www.haskell.org/pipermail/web-devel/2013/002663.html) about
the planned changes to WAI. For the most part, the changes Kazu and I have
implemented coincide with the conclusions of that thread, namely:

* The Request constructor has been moved to an Internal module.
* serverName and serverPort have been removed.
* requestBody no longer lives in `ResourceT IO`.
* As an optimization to avoid extra lookups, `requestHeaderHost` is now part of `Request`. We may add other such optimization fields in the future.
* FilePart now contains the total file size, which is necessary for properly and efficiently supporting range requests.
* Instead of living in `ResourceT`, `ResponseSource` allows for bracket-like semantics to provide for exception safety. This allows for a lot of optimizations in Warp, and leads to some other simplifications which I'll get to later. The [responseSourceBracket](https://github.com/yesodweb/wai/blob/8318c3b5f8363fdd6884dc9456520b0190c9cf60/wai/Network/Wai.hs#L132) helper function is a convenient way to get exception safety.

In addition to interface changes, Kazu has made a number of optimizations to
the internals of Warp. A big thank you to Gregory Collins who provided some
recommendations on this front.

I'll hold off on making any comments about performance, as that's Kazu's domain
and he's still adding some optimizations.

## http-client

There are a few questions which I've received multiple times about http-conduit:

* Is it possible to get a slimmed-down version, without SSL support, to be used for testing purposes?
* Is it possible to use OpenSSL instead of the tls package?
* If all I need is a way to get a lazy ByteString, can I have a lower-dependency package?

It turns out that the http-conduit code base is pretty well set up for this
kind of change. The concept of a connection has already been abstracted in such
a way that it's trivial to use different backends for SSL connections. And the
internals of the package don't really use conduit very much, rather, the package just
exports a conduit interface for convenience.

So after some hacking, I present to you [the http-client
megarepo](https://github.com/snoyberg/http-client). It's broken up into five
packages currently:

* http-client provides the core functionality, without any SSL or conduit support.
* http-client-tls uses the tls and connection packages to provide SSL support.
* http-client-openssl uses OpenSSL for that purpose.
* http-client-multipart provides helper functions for generating multipart request bodies.
* http-client-conduit provides some helper functions for interacting with conduit.

On top of all this, I've [set up a branch of
http-conduit](https://github.com/snoyberg/http-conduit/tree/http-client) which
provides mostly the same API, but uses http-client for all of its
functionality. Now, some random thoughts:

* http-client does not use resourcet at all; it uses the bracket idiom instead. This is important for both (1) simplifying the library, and (2) making it easier to integrate with WAI 2.0, which also doesn't use resourcet. http-conduit continues to provide a resourcet-powered interface for ease-of-use.
*   Most functionality provided by http-conduit has been ported to http-client, including:

    * Connection pooling, which is an important optimization when you're regularly connecting to the same host (e.g., API access).
    * Cookies (browser API will continue in http-conduit-browser)
    * Multipart messages
    * Proxies
    * Socks proxies (when using http-client-tls)
    * Basic authentication

* There's a distinct lack of extensive testing in http-client, since most testing is still being done in http-conduit. I'd certainly like to increase the test coverage in http-client, possibly by simply porting http-conduit test cases over. But for now, the http-conduit test suite is a sufficient means of testing http-client.

I think http-client is at a very nice level of abstraction for people building
higher-level APIs. If you're in such a boat, I'd really like to hear feedback.
I think we're headed towards too much fragmentation in the HTTP client space
right now, and having a shared engine could be a good solution to the problem.

## Yesod/http-reverse-proxy/keter

Not only can Yesod be ported over to WAI 2.0 without any API changes, but it
can do so with just a few CPP declarations, so that Yesod will compile both
with the current version of WAI and WAI 2.0. The only real problematic change
is the removal of ResourceT from the WAI protocol. This is handled in Yesod by:

* Starting a new ResourceT block for each Yesod request.
* When returning a non-streaming response, closing the ResourceT block when passing the response value to Warp.
* When returning a streaming response, using the `bracket-`like functionality of ResponseSource to make sure the ResourceT block is closed.

http-reverse-proxy and keter both end up a bit simpler as a result of this
change. Since neither http-client nor Warp deal with resourcet any more, the
reverse proxy layer can ignore it as well. Basically, we can just peel off a
layer of monad transformers from ever being used.

## What's not changing: conduit

In this whole rewrite process, I actually wrote a conduit-free branch of WAI for testing purposes. It worked, but Kazu and I came to the conclusion that:

1. Removing conduit provided no discernible performance improvement.
2. Both the internal Warp code, and external user code, became harder to manage without conduit involved.

I also investigated some possible changes to conduit itself, based on [last
month's blog post](https://www.fpcomplete.com/user/snoyberg/blog-posts/simpler-conduit-core).
For the moment, I'm not pursuing that route further, but I don't want to
relegate that discussion to a footnote in this blog post. I'll hopefully give
that topic more attention later.

## Feedback requested!

None of the changes discussed here are set in stone yet, so now's the perfect
time to clone the repos and start experimenting. In particular, I'm looking for
feedback from the following groups:

* Users of WAI, either for writing WAI applications or WAI-based frameworks.
* Users of http-conduit, who are either happy with the current API, or would be interested in any of the changes mentioned above (e.g., switching tls for OpenSSL).
* Developers of other HTTP client libraries, who could consider collaboration on a single, shared HTTP client engine.

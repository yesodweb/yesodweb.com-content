I'm happy to announce the release of the Web Application Interface (WAI) 2.0,
as well as the first non-experimental release of
[http-client](http://hackage.haskell.org/package/http-client) (version 0.2).
The lion's share of the work on the new WAI and Warp was done by Kazu, and he's
planning on a separate write-up on some of the changes he's made.

On the http-client side, this release includes OpenSSL support both via the
[pure-Haskell tls package](http://hackage.haskell.org/package/http-client-tls)
and [the OpenSSL
library](http://hackage.haskell.org/package/http-client-openssl), as well as a
[new version of http-conduit](http://hackage.haskell.org/package/http-conduit)
(2.0) built on top of http-client. The API of http-conduit remains mostly
unchanged. A big thank you to Joseph Abrahamson for providing very helpful
feedback on the API design.

My [previous blog post](http://www.yesodweb.com/blog/2013/11/wai-2-http-client)
discussed the motivations behind these releases, so I won't go into those
details here. At a high level, here are some of the motivations for these changes:

- WAI 2.0
    - Improving performance of Warp
    - Scaling on multicore if complied with coming GHC 7.8
    - Cleaning up API of Warp and enriching documentations
- http-client
    - Shared infrastructure for multiple high-level HTTP client libraries.
    - Choice of SSL backend.
    - Lower dependency requirements for simple HTTP queries.
    - Easier to test out new high-level APIs.

The only other thing I want to touch on is the affect on the rest
of the package ecosystem triggered by these releases.

Together with this release, I'm releasing new versions of all of the WAI and
Yesod package sets, as well as
[http-reverse-proxy](http://hackage.haskell.org/package/http-reverse-proxy) and
[keter](http://hackage.haskell.org/package/keter). Kazu is releasing new
versions of fast-logger and wai-logger as well. I highly recommend either
upgrading *all* of these packages or sticking to the old versions entirely;
mixing-and-matching may work, but has not been highly tested. (As usual,
Stackage, FP Haskell Center and yesod-platform will continue to provide sets of
compatible packages.)

I've taken a lot of care in the new release of Yesod to keep backwards
compatibility, so unless you were depending specifically on WAI libraries, your
old code should continue building without issue. That said, if you run into any
upgrade issues, please bring them up on the mailing list so that, minimally,
others can learn from the experience, and possibly the issues can be addressed
in the libraries themselves.

While I'm very excited with this release, and think it pushes the library
ecosystem in the right direction, I'm even happier that Yesod is continuing to
maintain a high level of API stability.

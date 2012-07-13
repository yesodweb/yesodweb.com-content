I'm happy to announce the 1.3 release of WAI and its related packages. WAI is the Web Application Interface for Haskell, providing a generic and efficient interface between web servers and applications. This release includes WAI itself, the Warp web server, and various other utilities and applications.

The main features for this release are:

* Upgrade to conduit 0.5
* Warp flushes request body after processing response body. This allows you to process the request body while generating the response. While this can be useful, be warned that this can cause deadlocks with some HTTP clients.
* simple-sendfile now sends headers in the same system call as the sendfile itself, resulting in much better performance for static file serving.
* Simplified wai-extra's request body parsing, uses standard `conduit` types instead of special `BackEnd` type.
* Drastically cleaned up wai-app-static (both internals and the user-facing API).
* `warp-tls` automatically sniffs the request and determines whether to serve over HTTP or HTTPS.
* Split off the `mime-types` package from `wai-app-static`.

This release will serve as the basis for the upcoming Yesod 1.1 release. As such, if you are a Yesod user, you should not upgrade to this new version of WAI until the 1.1 release of Yesod. We are still maintaining a 1.2 branch, and will backport any bug fixes as necessary.

Please see the [chapter in the Yesod book](http://www.yesodweb.com/book/web-application-interface) for a general overview of WAI, or check out some of the more prominent packages on Hackage:

* [WAI](http://hackage.haskell.org/package/wai)
* [warp](http://hackage.haskell.org/package/warp)
* [wai-extra](http://hackage.haskell.org/package/wai-extra)
* [warp-tls](http://hackage.haskell.org/package/warp-tls)

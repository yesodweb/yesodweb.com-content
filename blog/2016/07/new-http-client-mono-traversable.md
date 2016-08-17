I'm happy to announce the release of a number of packages today, but mostly
this comes down to upgrades to
[http-client](https://www.stackage.org/package/http-client) and
[mono-traversable](https://www.stackage.org/package/mono-traversable). This
blog post will cover the main changes you should be aware of it you are using
one of these two packages. In addition to these two packages, the following
related packages are being released as well:
[http-client-tls](https://www.stackage.org/package/http-client-tls),
[http-client-openssl](https://www.stackage.org/package/http-client-openssl),
[http-conduit](https://www.stackage.org/package/http-conduit),
[mono-traversable-instances](https://www.stackage.org/package/mono-traversable-instances),
[minlen](https://www.stackage.org/package/minlen),
[chunked-data](https://www.stackage.org/package/chunked-data),
[conduit-combinators](https://www.stackage.org/package/conduit-combinators),
[mutable-containers](https://www.stackage.org/package/mutable-containers),
[classy-prelude](https://www.stackage.org/package/classy-prelude),
[classy-prelude-conduit](https://www.stackage.org/package/classy-prelude-conduit),
[classy-prelude-yesod](https://www.stackage.org/package/classy-prelude-yesod).

## http-client

http-client is an HTTP client library leveraged by a number of other packages,
including [http-conduit](https://www.stackage.org/package/http-conduit),
[pipes-http](https://www.stackage.org/package/pipes-http), and
[wreq](https://www.stackage.org/package/wreq). This release is mostly about
addressing [issue #193](https://github.com/snoyberg/http-client/issues/193),
about an controversial design decision to throw runtime exceptions on
non-successful HTTP response statuses. If you want a quick explanation of
what's changed and what you should do:

* If the HTTP server you're talking to gives a non-success HTTP response status
  (e.g., `404 not found`), you will no longer get a runtime exception by
  default.

    * If you want the old behavior, switch to the `parseUrlThrow` function
    * The `parseUrl` function remains with the old behavior, but is deprecated in favor of `parseRequest`
    * The `IsString` instance has also switched to the non-throwing behavior

* In an effort to make the remaining exceptions more useful, and avoid people
  accidentally relying on the old exception behavior, there's a new structure
  to the `HttpException` type. In particular, almost all exceptions are now
  contained in the `HttpExceptionContent` type, and will be wrapped up with the
  `HttpExceptionRequest` constructor, which provies information on the `Request`
  used to generate the exception. Hopefully this will make for much more useful
  error messages.

Based on the feedback I've received, this should bring the default behavior for
http-client into line with what people expect, and will hopefully have a
minimal impact of migration for existing users relying on the current behavior.

## mono-traversable

The mono-traversable package provides a typeclass hierarchy based around the
idea of monomorphic containers. This allows, for examples, a unified `foldMap`
function that works on lists, `ByteString`s, and unboxed vectors, as well as
abstractions over sequences (functions like `break` and `drop`) and containers
(`Map`s and `Set`s).

I [laid out a plan](https://github.com/snoyberg/mono-traversable/issues/95) for
a cleanup of the mono-traversable package.  Most of the changes here were
minor, and will not affect end users. Of import:

* The `mono-traversable` package itself has much reduced dependencies, by
  putting a number of instances into the new `mono-traversable-instances`
  package.
* A few typeclasses that used to live in chunked-data have moved to mono-traversable
* The `Data.NonNull` module is much simpler, and no longer based on `Data.MinLen` (which lives on in the minlen package)

### classy-prelude

Mostly, classy-prelude is just inheriting the upstream changes in
mono-traversable. The only other point I'd like to make about this
classy-prelude release is that it is switching over to the new [safe-exceptions
package](https://www.stackage.org/package/safe-exceptions), which I [recently
announced on the FP Complete
blog](https://www.fpcomplete.com/blog/2016/06/announce-safe-exceptions).

### Mega repo

To simplify maintenance, and address a common problem of people not knowing
which repo to report issues to, I've combined a few repos together into a
mono-traversable mega-repo:

* classy-prelude (and -conduit and -yesod)
* chunked-data
* mutable-containers

I've updated the old repo locations with a final commit pointing to the new
location.

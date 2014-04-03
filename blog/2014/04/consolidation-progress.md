My [last blog post](http://www.yesodweb.com/blog/2014/03/package-consolidation) detailed a number of changes I was going to be making for package consolidation. A number of those have gone through already, this blog post is just a quick summary of the changes.

## shakespeare

shakespeare is now a single package. hamlet, shakespeare-css, shakespeare-js,
shakespeare-i18n, shakespeare-text, and servius have all been merged in and
marked as deprecated. I've also uploaded new, empty versions of those
deprecated packages. This means that, in order to support both the old and new
versions of shakespeare, you just need to ensure that you have both the
shakespeare and deprecated packages listed in your cabal file. In other words,
if previously you depended on hamlet, now you should depend on hamlet and
shakespeare. When you're ready to drop backwards compatibility, simply put a
lower bound of `>= 2.0` on shakespeare and remove the deprecated packages.

(Note: this method for dealing with deprecated packages is identical for all
future deprecations, I won't detail the steps in the rest of this blog post.)

## conduit

conduit-extra now subsumes attoparsec-conduit, blaze-builder-conduit,
network-conduit, and zlib-conduit. It also includes three modules that used to
be in conduit itself: .Text, .Binary, and .Lazy. To deal with this change,
simply adding conduit-extra to your dependencies should be sufficient.

The other changes have to do with resourcet. In particular:

* Data.Conduit no longer reexports identifiers from resourcet and
  monad-control. These should be imported directly from their sources.
* Instead of defining its own `MonadThrow` typeclass, resourcet now uses the
  `MonadThrow` typeclass from the exceptions package. For backwards
  compatibility, Control.Monad.Trans.Resource provides `monadThrow` as an alias
  for the new `throwM` function.
* The [Resource monad](http://www.yesodweb.com/blog/2014/01/announcing-resource-monad) had a confusing name, in that it wasn't directly related to the ResourceT transformer. I've renamed it to Acquire, and put it in its own module (Data.Acquire).
    * I'm actually very happy with Acquire, and think it's a great alternative to hard-coding either the bracket pattern or resourcet into libraries. I'm hoping to add better support to WAI for Acquire, and blog a bit more about the usage of Acquire.
* MonadUnsafeIO has been removed entirely. All of its functionality can be replaced with MonadPrim and MonadBase (for example, see [the changes to blaze-builder-conduit](https://github.com/snoyberg/conduit/commit/827625e7c2845a9c662835ee3a4dc7ba79b374e1#diff-d88224359853aa9e76c6e9bcd89445c7R57)).
* MonadActive, which is only needed for Data.Conduit.Lazy, has been moved to that module.

## http-client

http-client-multipart has been merged into http-client. In addition, instead of using the failure package, http-client now uses the exceptions package.

http-client-conduit has been merged into http-conduit. I've also greatly expanded the Network.HTTP.Client.Conduit module to contain what I consider its next-gen API. In particular:

* No usage of ResumableSource.
* Instead of explicit ResourceT usage, it uses the Acquire monad and bracket pattern (acquireResponse, withResponse).
* Instead of explicitly passing around a Manager, it uses MonadReader and the HasHttpManager typeclass.

I'm curious how people like the new API. I have no plans on removing or changing the current Network.HTTP.Conduit module, this is merely an alternative approach.

## Updated yesod-platform

I've also released a new version of yesod-platform that uses the new versions
of the packages above. A number of packages on Hackage still depend on conduit
1.0, but I've sent quite a few pull requests in the past few days to get things
up-to-date. Thankfully, maintaining compatibility with both 1.0 and 1.1 is
pretty trivial.

We are happy to announce the release of Yesod 1.4. This includes:

* Releases of all Yesod packages to support version 1.4.
* The book content on yesodweb.com is completely updated for Yesod 1.4, with all snippets confirmed to compile and most of the text proofread from scratch for accuracy (in the next week the rest will be finished).
* A new Stackage snapshot available [for GHC 7.8.3](http://www.stackage.org/stackage/23593406b773a232ea760d2e50bf5d56d0a99c4e).

Its worth mentioning that there have been a ton of improvements to Yesod since version 1.2, they just didn't need any breaking changes.

Thanks to everyone who provided code, feedback, and testing for this release, it
should be a very solid one!

Here's a collection of links that provide various other pieces of information about this release:

* [Announcing Persistent 2](http://www.yesodweb.com/blog/2014/08/announcing-persistent-2)
* [Persistent 2.1 Release Candidate](http://www.yesodweb.com/blog/2014/09/persistent-2)
* [Planning Yesod 1.4](http://www.yesodweb.com/blog/2014/09/planning-yesod-1-4)
* [Yesod 1.4 release date](https://groups.google.com/d/msg/yesodweb/7leiDXHe1M8/oEWH83twOK0J)


## Changelog

What is most exciting to report is that this was a very minor change to Yesod, and
therefore most code should be upgradeable with minor changes. First, the
changelog of breaking changes:


### New routing system with more overlap checking control

This requires OverloadedStrings and ViewPatterns.
The generated code is faster and *much* more readable.

Yesod routes are not just type-safe, they also check for overlapping that could cause ambiguity. This is a great feature, but sometimes it gets in your way.
Overlap checking can be turned off for multipieces, entire routes, and parent routes in a hierarchy. For more information, see [the commit comment](https://github.com/yesodweb/yesod/commit/e23c78f2ce60591574a177de9f3ce5d634384e4a).


### Dropped backwards compatibility with older versions of dependencies

In particular, persistent-1 and wai-2. We will talk more about persistent 2.
wai-3 uses a CPS style that will require some middleware to have an additional CPS parameter.
Looking at the wai-extra source code can help with upgrading, but it should just be adding an extra parameter.


### yesod-auth works with your database and your JSON

There is better support for non-persistent backends in yesod-auth. See [pull request 821](https://github.com/yesodweb/yesod/pull/821) for details. For most users, you can fix this by adding `instance YesodAuthPersist App` to your `Foundation.hs`.

yesod-auth already released a breaking change to be able to accept JSON everywhere.
That bumped the version to 1.3
We like to keep the yesod-\* packages in sync, so now everything is getting bumped to 1.4 together.

In the 1.4 release, we also fixed requireAuth and and requireAuthId to return a 401 response when a JSON response is requested. See [pull request 783](https://github.com/yesodweb/yesod/pull/783).


### yesod-test sends HTTP/1.1 as the version

This may require updating tests to expect 303 instead of 302 redirects.


### Type-based caching with keys.

The Type-based caching code was moved into a [separate module](https://github.com/yesodweb/yesod/blob/yesod-1.4/yesod-core/Yesod/Core/TypeCache.hs) without Yesod dependencies and documented. If there is interest in seeing this as a separate package let us know, but it is also pretty easy to just copy the module.

To me, TypeCache is a beautiful demonstration of Haskell's advanced type system that shows how you can get the best of both worlds in a strongly typed language.

``` haskell
type TypeMap      = HashMap TypeRep Dynamic
```

Above we have the wonderful juxtaposition of Haskell's strong typing in the Key, and dynamic typing in the value. This HashMap is used to cache the result of a monadic action.

``` haskell
cached :: (Monad m, Typeable a) 
       => TypeMap
       -> m a                       -- ^ cache the result of this action
       -> m (Either (TypeMap, a) a) -- ^ Left is a cache miss, Right is a hit
```

Dynamic is used to have a HashMap of arbitrary value types.
TypeRep is used to create a unique key for the cache.
Yesod uses this to cache the authentication lookup of the database for the duration of the request.

``` haskell
newtype CachedMaybeAuth val = CachedMaybeAuth { unCachedMaybeAuth :: Maybe val }
    deriving Typeable

cachedAuth
    = fmap unCachedMaybeAuth
    . cached
    . fmap CachedMaybeAuth
    . getAuthEntity
```

`CachedMaybeAuth` is a newtype that isn't exported. `TypeRep` is specific to a module, so this pattern guarantees that your cache key will not conflict outside of your module.

This functionality was in yesod-1.2 even though the code was not separated into a new module.
The 1.4 release adds the ability to cache multiple values per type

``` haskell
type KeyedTypeMap = HashMap (TypeRep, ByteString) Dynamic

cachedBy :: (Monad m, Typeable a)
         => KeyedTypeMap
         -> ByteString                     -- ^ a cache key
         -> m a                            -- ^ cache the result of this action
         -> m (Either (KeyedTypeMap, a) a) -- ^ Left is a cache miss, Right is a hit
```

This is useful if your monadic action has inputs: if you serialize them to a ByteString you can use thm as a key.


## Upgrade guide

The most significant set of changes in the Yesod ecosystem actually landed in
Persistent 2. However, these were mostly internal changes with new features that maintain backwards compatibility, so many users will be unaffected.

To kickoff the upgrade process, you need to change update your cabal file to allow yesod version 1.4.
If you had constraints on persistent, update them to > 2.1
If you are using `cabal freeze` to peg your versions in the cabal.config file, cabal will provide you no assistance in making a smooth upgrae.
You are probably going to want to delete a whole lot of things in cabal.config (or possibley the entire file), and upgrade a lot of dependencies at once.
When you are done and things compile again, you will want to do a `cabal freeze`

As has become the custom for each major release, the upgrade
process is documented by the diff of the Haskellers code base upgrading to Yesod 1.4.
[For Haskellers it was pretty simple](https://github.com/snoyberg/haskellers/commit/e01e71371f0334b88b7cee9ce2a461e2009b415b).

In sum:

* Replace `type YesodPersistBackend App = SqlPersist` with `type YesodPersistBackend App = SqlBackend`.
* Add `instance YesodAuthPersist App` to `Foundation.hs`.
* Add the `ViewPatterns` language extension.

If you have more complex persistent code you may have more to do.
Look at [the previous post on persistent-2.1](http://www.yesodweb.com/blog/2014/09/persistent-2)

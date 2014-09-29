We are happy to announce the release of Yesod 1.4. This includes:

* Releases of all Yesod packages to support version 1.4.
* The book content on yesodweb.com is completely updated for Yesod 1.4, with all snippets confirmed to compile and most of the text proofread from scratch for accuracy (in the next week the rest will be finished).
* A new Stackage snapshot available at: FIXME.

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
wai-3 uses a CPS style that will require some middleware to have an additional CPS paramter.
Looking at the wai-extra source code can help with upgrading, but it should just be adding an extra parameter.


### yesod-auth works with your database and your JSON

There is better support for non-persistent backends in yesod-auth. See [pull request 821](https://github.com/yesodweb/yesod/pull/821) for details. For most users, you can fix this by adding `instance YesodAuthPersist App` to your `Foundation.hs`.

yesod-auth already released a breaking change to be able to accept JSON everywhere.
That bumped the version to 1.3
We like to keep the yesod-* packages in sync, so now everything is getting bumped to 1.4 together.

In the 1.4 release, we also fixed requireAuth and and requireAuthId to return a 401 response when a JSON response is requested. See [pull request 783](https://github.com/yesodweb/yesod/pull/783).


### yesod-test sends HTTP/1.1 as the version

This may require updating tests to expect 303 instead of 302 redirects.


### Type-based caching with keys.

The Type caching code was moved into a [separate module](https://github.com/yesodweb/yesod/blob/yesod-1.4/yesod-core/Yesod/Core/TypeCache.hs) without Yesod dependencies and documented. If there is interest in seeing this as a separate package let us know, but it is also pretty easy to just copy the module.

Type-based caching seems like a very Haskell was of doing things, but at the same time we are showing the naughty side of the language by using Data.Dynamic.

``` haskell
type TypeMap      = HashMap TypeRep Dynamic

cached :: (Monad m, Typeable a) 
       => TypeMap
       -> m a                       -- ^ cache the result of this action
       -> m (Either (TypeMap, a) a) -- ^ Left is a cache miss, Right is a hit
```

Dynamic is used to have a HashMap with arbitrary value types.
TypeRep is used to create a unique key for the cache.
Yesod uses this to cache the authentication lookup of the database.

``` haskell
newtype CachedMaybeAuth val = CachedMaybeAuth { unCachedMaybeAuth :: Maybe val }
    deriving Typeable

cachedAuth
    = fmap unCachedMaybeAuth
    . cached
    . fmap CachedMaybeAuth
    . getAuthEntity
```

`CachedMaybeAuth` is a newtype that isn't exported. `TypeRep` is specific to a module, so this pattern is guaranteed that your cache key will not conflict outside of your module.

This functionality was in yesod-1.2 even if it was not separated out.
 1.4 adds the ability to cache multiple values per type

``` haskell
type KeyedTypeMap = HashMap (TypeRep, ByteString) Dynamic
cachedBy :: (Monad m, Typeable a)
         => KeyedTypeMap
         -> ByteString                     -- ^ a cache key
         -> m a                            -- ^ cache the result of this action
         -> m (Either (KeyedTypeMap, a) a) -- ^ Left is a cache miss, Right is a hit
```


## Persistent 2.1

The most significant set of changes in the Yesod ecosystem actually landed in
Persistent 2. However, these were mostly internal changes with new features that maintain backwards compatibility,
so many users will be unaffected.

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


### Persistent 2.1: dealing with more complex changes

The big change to Persistent is that now that the Key type is now flexible.
So if you have functions that have `Key` in the type signature and are not specific to one PersistEntity,
You may need to constrain them to the `BackendKey` type.
An easy way to do this is using `ConstraintKinds`.

``` haskell
type DBEntity record =
    ( PersistEntityBackend record ~ MongoContext
    , PersistEntity record
    , ToBackendKey MongoContext record
    )
```

A Sql user would use `SqlBackend` instead of `MongoContext`. So you can change your type signature:

``` haskell
- PersistEntity record => Key record
+ DBEntity record => Key record
```

Haskellers has zero code existing outside of Handler and its persistent usage is pretty simple.
But Persistent 2 did change its monad stack.
So if you separated out model code so that it can be ran somewhere else besides just the Handler, you may also need to make some changes.

Here is one possible approach, again specialized to MongoDB, that requires `Rank2Types`.

``` haskell
type ControlIO m = ( MonadIO m , MonadBaseControl IO m)
type LogIO m = ( MonadLogger m , ControlIO m)

-- these are actually types, not constraints
-- with persistent-2 things work out a lot easier this way
type DB    a =  LogIO m => ReaderT MongoContext m a
type DBM m a =  LogIO m => ReaderT MongoContext m a

-- The constraint version of the above
-- This requires you to use Database.MongoDB.liftDB
type Mongo m = (LogIO m, MonadReader MongoContext m)
```

so now your basic type signature is just `DB ()`
For working with different monad stacks, you can use DBM.
If you are using conduits, you will have `MonadResource m => DBM m ()`.
Here is another example:

``` haskell
class Monad m => HasApp m where
    getApp :: m App 
instance HasApp Handler where
    getApp = getYesod
instance HasApp hasApp => HasApp (ReaderT MongoContext hasApp) where
    getApp = lift $ getApp
instance MonadIO m => HasApp (ReaderT App m) where
    getApp = ask 

-- | synonym for DB plus HasApp operations
type DBApp    a = HasApp m => DBM m a 
type DBAppM m a = HasApp m => DBM m a 
```

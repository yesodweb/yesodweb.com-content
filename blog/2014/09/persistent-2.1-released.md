Persistent 2.1, a stable release of the next generation of persistent is released to Hackage.

persistent is an ORM for Haskell that keeps everything type-safe.

persistent 2.1 features

* a flexible, yet more type-safe Key type
* a simplified monad stack

I already [announced persistent 2](http://www.yesodweb.com/blog/2014/08/announcing-persistent-2)
and the [2.1 release candidate](http://www.yesodweb.com/blog/2014/09/persistent-2).

Everyone should set their persistent dependencies to `> 2.1 && < 3`. `2.0.x` was the unstable release and is now deprecated.

I want to thank all the early persistent 2 adopters for putting up with a fast-moving, buggy code base. This was an experiment in shipping an unstable version, and what I learned from it is that it was a great process, but we need to make sure Travis CI is running properly, which it is now!


### Persistent 2.1 library support

The persistent and persistent-template libraries should support any kind of primary key type that you need. The backends are still catching up to the new features

* persistent-sqlite backend has fully implemented these features.
* persistent-postgres and persitent-mysql don't yet support changing the type of the id field
* persistent-mongoDB does not yet support composite primary keys

All of the above packages except persistent-mysql are being well maintained, but just developing new features at their own pace. persistent-mysql is in the need of a dedicated maintainer. There are some major defects in the migration code that have gone unresolved for a long time now.

* persistent-redis is in the process of being upgraded to 2.1
* [persistent-zookeeper](http://hackage.haskell.org/package/persistent-zookeeper) was just released, but it is on persistent 1.3.*
* There are other persistent packages out there that I have not had the chance to check on yet, most noteably persistent-odbc. Feel free to ask for help with upgrading.



### Persistent 2.1 upgrade guide

Simple persistent usage may not need any changes to upgrade.

The fact that the Key type is now flexible means it may need to be constrained.
So if you have functions that have `Key` in the type signature that are not specific to one PersistEntity, you may need to constrain them to the `BackendKey` type.
An easy way to do this is using `ConstraintKinds`.

``` haskell
type DBEntity record =
    ( PersistEntityBackend record ~ MongoContext
    , PersistEntity record
    , ToBackendKey MongoContext record
    )
```

A Sql user would use `SqlBackend` instead of `MongoContext`. So you can now change the type signature of your functions:

``` haskell
- PersistEntity record => Key record
+ DBEntity record => Key record
```

Depending on how you setup your monad stacks, you may need some changes.
Here is one possible approach to creating small but flexible Monad stack type signagures
that requires `Rank2Types`.
Again this is specialized to MongoDB. 

``` haskell
type ControlIO m = ( MonadIO m , MonadBaseControl IO m)
type LogIO m = ( MonadLogger m , ControlIO m)

-- these are actually types, not constraints
-- with persistent-2 things work out a lot easier this way
type DB    a =  LogIO m => ReaderT MongoContext m a
type DBM m a =  LogIO m => ReaderT MongoContext m a
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

With this pattern our return type signature is always `ReaderT MongoContext m`, and we are changing `m` as needed. A different approach is to have a return type signature of `m` and to place a `MonadReader` constraint on it.

```
type Mongo m = (LogIO m, MonadReader MongoContext m)
```

Right now this approach requires using a call to
`Database.MongoDB.liftDB` around each database call, but I am sure there are approaches to dealing with that. One approach would be to wrap every persistent "primitive" with liftDB.

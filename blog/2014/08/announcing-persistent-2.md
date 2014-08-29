We are happy to announce the release of persistent 2.0

persistent 2.0 adds a flexible key type and makes some breaking changes.

2.0 is an unstable release that we want your feedback on for the soon to follow stable 2.1 release.


## New Features

* type-safe composite primary and foreign keys
* added an upsert operation (update or insert)
* added an insertMany_ operation


## Fixes

* An `Id` suffix is no longer automatically assumed to be a Persistent type
* JSON serialization
  * MongoDB ids no longer have a prefix 'o' character.

# Breaking changes

* Use a simple ReaderT for the underlying connection
* fix postgreSQL timezone storage
* remove the type parameter from EntityDef and FieldDef


# In depth

## Composite keys

The biggest limitation of data modeling with persistent is an assumption of a simple (for current SQL backends an auto-increment) primary key.
We learned from [Groundhog](https://github.com/lykyah/groundhog) that a more flexible primary key type is possible. Persistent adds a similar flexible key type while maintaining its existing invariant that a Key is tied to a particular table.

To understand the changes to the `Key` data type, lets look at a change in the test suite for persistent 2.

``` diff
       i <- liftIO $ randomRIO (0, 10000)
-      let k = Key $ PersistInt64 $ abs i
+      let k = PersonKey $ SqlBackendKey $ abs i
```

Previously `Key` contained a `PersistValue`. This was not type safe. `PersistValue` is meant to serialize any basic Haskell type to the database, but a given table only allows specific values as the key.
Now we generate the `PersonKey` data constructor which specifies the Haskell key types.
`SqlBackendKey` is the default key type for SQL backends.

Now lets look at code from CompositeTest.hs


```
mkPersist sqlSettings [persistLowerCase|
  Parent
      name  String maxlen=20
      name2 String maxlen=20
      age Int
      Primary name name2 age
      deriving Show Eq
  Child
      name  String maxlen=20
      name2 String maxlen=20
      age Int
      Foreign Parent fkparent name name2 age
      deriving Show Eq
|]

```

Here Parent has a composite primary key made up of 3 fields.
Child uses that as a foreign key.
The primary key of Child is the default key for the backend.


``` haskell
let parent = Parent "a1" "b1" 11
let child = Child "a1" "b1" 11
kp <- insert parent
_ <- insert child
testChildFkparent child @== parent
```

# Future changes

## Short-term improvements

Before the 2.1 release I would like to look at doing some simple things to speed up model compilation a little bit.

* Speed up some of the compile-time persistent code (there is a lot of obviously naive code).
* Reduce the size of Template Haskell generation (create a reference for each EntityDef and some other things rather than potentially repeatedly inlining it)


## Medium-term improvement: better support for Haskell data types

We want to add better support for modeling ADTs, particularly for MongoDB where this is actually very to do in the database itself. Persistent already support a [top-level entity Sum Type](https://github.com/yesodweb/persistent/blob/master/persistent-test/SumTypeTest.hs#L35) and a simple field ADT that is just an enumeration.

Another pain point is serializing types not declared in the schema. The declaration syntax in groundhog is very verbose but allows for this. So one possibility would be to allow the current DRY persistent declaration style and also a groundhog declaration style.


## Long-term improvements: Projections

It would be possible to add projections now as groundhog or equeleto have done. However, the result is not as end-user friendly as we would like.
When the record namespace issue is dealt with in the GHC 7.10 release we plan on adding projections to persistent.


## Ongoing: Database specific functionality

We always look forward to see more databases adapters for persistent.
In the last year, a [Redis](http://hackage.haskell.org/package/persistent-redis) and ODBC](https://github.com/gbwey/persistent-odbc) adapter were added.

Every database is different though, and you also want to take advantage of your database-specific features.
[esqueleto](http://hackage.haskell.org/package/esqueleto) and [persistent-mongoDB](http://hackage.haskell.org/package/persistent-mongoDB) have shown how to build database specific features in a type-safe way on top of persistent.


## Organization

Although the persistent code has no dependency on Yesod, I would like to make the infrastructure a little more independent of yesod. The first steps would be

* putting it under a different organization on github.
* having a separate mail list (should stackoverflow be prioritized over e-mail?)

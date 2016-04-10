# Scaling a database

Many applications eventually reach the limits of a single database machine and work across multiple machines. Distributing state across multiple machines can introduce a great deal of complexity. Consequently, there are many approaches to dealing with multiple databases. The [CAP theorem](https://en.wikipedia.org/wiki/CAP_theorem) ensures that none of them is perfect (Is there ever a good impossibility theorem?). [Front Row Education](https://www.frontrowed.com/) chose to go with streaming, native replication from a PostgreSQL write database to a PostgreSQL read database.

# Possible solutions

What's the best way to program against a write database with a read replica?  I'm not certain, but here are a couple of approaches:

## Run opaque queries against chosen backend

In this approach, we don't change any of the `persistent` machinery. `selectList`, `insert` and `runSqlPool` stay the same:

```haskell
runSqlPool :: (MonadBaseControl IO m) => ReaderT SqlBackend m a -> Pool SqlBackend -> m a
writeBackend :: SqlBackend
readBackend :: SqlBackend

insertStudent :: ReaderT SqlBackend IO StudentId
insertStudent = insert student

selectStudents :: ReaderT SqlBackend IO [Entity Student]
selectStudents = selectList ([] :: [Filter Student]) []

insertAndSelect :: IO [Entity Student]
insertAndSelect = do
  _ <- runSqlPool (insertStudent >> insertStudent) writeBackend
  runSqlPool selectStudents readBackend
```

We choose which backend to run our queries against at execution time. That is, we pass one backend to `runSqlPool` if we want to execute the query against the read database and a different backend if we want to execute our query against the write database.

This approach does work in the most basic sense of the word. But it's manual and error-prone. Nothing stops us from accidentally running a write query against a read database and getting an error at runtime. That's not the Haskell way! We'd be much better off encoding this read and write information in the query's type.

## `update`, `delete` and `insert` write, `select` reads

In this approach we create wrappers around `SqlBackend` called `SqlReadBackend` and `SqlWriteBackend`. Then, we specify that all selects (reads) will operate against the read database and all inserts, update, or delete (writes) will operate against the write database. We can intermix queries of different types with multiple (now type safe) calls to `runSqlPool`:

```haskell
runSqlPool :: (MonadBaseControl IO m, IsSqlBackend backend) => ReaderT backend m a -> Pool backend -> m a
writeBackend :: SqlWriteBackend
readBackend :: SqlReadBackend

insertStudent :: ReaderT SqlWriteBackend IO StudentId

selectStudents :: ReaderT SqlReadBackend IO [Entity Student]

insertAndSelect :: IO [Entity Student]
insertAndSelect = do
  _ <- runSqlPool (insertStudent >> insertStudnet) writeBackend
  runSqlPool selectStudents readBackend
```

Attempting to run `insertStudent` against on the `readBackend` will result in a type error. Nice!

Unfortunately, it will also result in a type error when attempting to run `selectStudents` against the `writeBackend`. Which is why we used two calls to `runSqlPool` in the above example. This inability to mix reads and writes in a single transaction is rather restrictive.

This approach also ignores problems of eventual consistency. Even under streaming replication, there is some lag (hopefully, only a few milliseconds) between the read database and the write database. If we can't run reads in the same transaction, on the same DB as writes we have a serious problem. In the above example, we have no guarantee that our student insertions will have propagated to the read DB in time for the `select` that immediately follows the `insert`.

## `update`, `delete` and `insert` write, `select` can be used in a read or write context

We must generalize our read operations so that we can still run them against the write database when we need to.

```haskell
runSqlPool :: (MonadBaseControl IO m, IsSqlBackend backend) => ReaderT backend m a -> Pool backend -> m a
writeBackend :: SqlWriteBackend
readBackend :: SqlReadBackend
instance SqlBackendCanRead SqlWriteBackend
instance SqlBackendCanRead SqlReadBackend

insertStudent :: ReaderT SqlWriteBackend IO StudentId

selectStudents :: (SqlBackendCanRead backend) => ReaderT backend IO [Entity Student]

insertAndSelect :: IO [Entity Student]
insertAndSelect =
  runSqlPool (insertStudent >> insertStudent >> selectStudents) writeBackend
```

We now use type classes to say that write queries can only run against the write database but read queries can run against either type of database and we can defer the decision of where to run a read query until use. But in a safe way.

# Conclusion

With this major release, `persistent` now has the ability to run against read and write databases in a type safe way, as outlined in the third approach.

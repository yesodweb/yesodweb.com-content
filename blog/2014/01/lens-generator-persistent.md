I've just released version 1.3.1 of persistent-template, which adds a new
options: `mpsGenerateLenses`. When enabled, this option will cause lenses to be
generated instead of normal field accessors. An example is worth a thousand
words:

```haskell
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Lens

share
  [ mkPersist sqlSettings { mpsGenerateLenses = True }
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe" $ Just 35

    Just john <- get johnId

    liftIO $ putStrLn $ john ^. personName

    janeId <- insert $ john & personName .~ "Jane Joe"
    Just jane <- get janeId
    liftIO $ print jane
```

Not a major feature, but convenient. It can also be combined with the
relatively new `mpsPrefixFields` option if your field names are unique, e.g.:

```haskell
share
  [ mkPersist sqlSettings { mpsGenerateLenses = True, mpsPrefixFields = False }
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe" $ Just 35

    Just john <- get johnId

    liftIO $ putStrLn $ john ^. name

    janeId <- insert $ john & name .~ "Jane Joe"
    Just jane <- get janeId
    liftIO $ print jane
```

Note that this isn't [the first use of lenses in
Persistent](http://haddocks.fpcomplete.com/fp/7.4.2/20130829-168/persistent/Database-Persist-Class.html#v:fieldLens).
While this implementation is fully compatible with the lens package, it
introduces no dependency on that package, due to the beautiful way in which
lenses work.

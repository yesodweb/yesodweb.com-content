A few months ago, Petr Pudlák [kicked off the conduit-extra
package](http://www.yesodweb.com/blog/2013/09/zipsinks-conduit-extra) with the
addition of `ZipSink`. As a refresher, `ZipSink` provides an alternative
`Applicative` instance for `Sink` which allows multiple `Sink`s to consume the
same stream.

I've just release version 1.0.13 of conduit which promotes `ZipSink` from
conduit-extra into conduit itself. This abstraction has proven to be generally
useful, and I hope others enjoy it as well. If you want a more in-depth review
of it, please see the original blog post. The only change since then is
renaming `broadcast` to `sequenceSinks`.

Along with this change, version 1.0.13 adds a new, similar concept: `ZipSource`
and `sequenceSources`. The idea here is to combine together multiple streams,
instead of sequencing one stream after another.

As a simple motivating example, let's say we have some files on the filesystem,
where each file contains a list of student test scores. We want to combine
together the test scores for all students for each of the tests. The following
program does the job:

```haskell
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8     as S8
import           Data.Conduit
import qualified Data.Conduit.Binary       as CB
import qualified Data.Conduit.List         as CL
import qualified Data.Map                  as Map

type Name = String

people :: [Name]
people = words "alice bob charlie"

files :: Map.Map Name FilePath
files = Map.fromList $ map (\name -> (name, name ++ ".txt")) people

scores :: MonadResource m => FilePath -> Source m Int
scores fp
    = CB.sourceFile fp
   $= CB.lines
   $= CL.map (read . S8.unpack)

sources :: MonadResource m => Map.Map Name (Source m Int)
sources = fmap scores files

sources' :: MonadResource m => Source m (Map.Map Name Int)
sources' = sequenceSources sources

main :: IO ()
main = runResourceT $ sources' $$ CL.mapM_ (lift . print)
```

The important bit is the definition of `sources'`. We use `sequenceSources` to
combine together each of the individual test scores into a single test score
map. With some basic input files, the output looks like:

    fromList [("alice",1),("bob",3),("charlie",2)]
    fromList [("alice",2),("bob",2),("charlie",2)]
    fromList [("alice",3),("bob",1),("charlie",2)]

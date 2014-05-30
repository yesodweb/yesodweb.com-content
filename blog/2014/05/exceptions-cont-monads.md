I've been meaning to write a blog post for a few weeks now about exception
handling in Haskell, especially with regards to monad transformer stacks.
Unfortunately, this is *not* that blog post, just one small aspect of it:
exception handling in continuation-base monads.

I've seen many people at different times advocate having some kind of exception
handling in continuation-based monads. Without calling out individual
instances, I'll sum up a lot of what I've discussed as, "Why can't
conduit/enumerator/pipes have a `bracket` function?"

After noticing yet another request for such a thing this morning, I decided to
write up a quick demonstration of what happens when you create a `bracket`
function for the enumerator package. I'll be using MonadCatchIO-transformers
(which is thankfully deprecated in favor of the [exceptions
package](http://hackage.haskell.org/package/exceptions)), and snap-core's
[orphan
instance](https://github.com/snapframework/snap-core/blob/1aa6804b671a07584ace898620ab5bfa1ba20669/src/Snap/Iteratee.hs#L144).

Let's start off by noticing something interesting: enumerator provides an
`enumFile` function (for reading the contents of a file), but no `iterFile`
equivalent to write data back. Using a `bracket`, it's actually really easy to
write up such a function (including some debug output to make sure we're being
safe):

```haskell
iterFile :: (MonadCatchIO m, MonadIO m, Functor m)
         => FilePath -> Iteratee ByteString m ()
iterFile fp = bracket
    (liftIO $ do
        putStrLn $ "opening file for writing: " ++ fp
        IO.openFile fp IO.WriteMode)
    (\h -> liftIO $ do
        putStrLn $ "closing file for writing: " ++ fp
        IO.hClose h)
    iterHandle
```

There shouldn't be any surprises in this implementation: we open a file handle
in the acquire argument, close that handle in the release argument, and then
use the handle in the inner argument. All is well in the world. Now let's try
actually using this function, both with and without exceptions being thrown:

```haskell
main :: IO ()
main = do
    writeFile "exists.txt" "this file exists"
    run (enumFile "exists.txt" $$ iterFile "output1.txt") >>= print
    run (enumFile "does-not-exist.txt" $$ iterFile "output2.txt") >>= print
```

Or you can [try running the code
yourself](https://www.fpcomplete.com/user/snoyberg/random-code-snippets/exceptions-in-continuation-based-monads). Let's look at the output:

```
opening file for writing: output1.txt
closing file for writing: output1.txt
Right ()
opening file for writing: output2.txt
Left does-not-exist.txt: openBinaryFile: does not exist (No such file or directory)
```

Notice that the output2.txt Handle is *never closed*. This is inherent to
working with any continuation based monad, since there are no guarantees that
the continuation will be called at all. It's *also* impossible to know if your
continuation will be called only once. With something like `ContT`, it's
possible to have the continuation run multiple times, in which case your
cleanup actions can run multiple times, which can be [really
bad](http://www.haskell.org/pipermail/haskell-cafe/2010-June/079198.html).

The exceptions package handles this in the right way. There are two different
type classes:
[`MonadCatch`](http://hackage.haskell.org/package/exceptions-0.6.1/docs/Control-Monad-Catch.html#t:MonadCatch)
allows for *catching* exceptions (which a continuation based monad *does* allow
for), whereas
[`MonadMask`](http://hackage.haskell.org/package/exceptions-0.6.1/docs/Control-Monad-Catch.html#t:MonadMask)
gives guarantees about bracket/finally semantics, which is what a
continuation-based monad *cannot* do. Another valid approach is
[monad-control](http://hackage.haskell.org/package/monad-control), which makes
it (I believe) impossible to write invalid instances.

(I want to get into more of the details of the trade-offs between exceptions and
monad-control, but that will have to wait for another blog post. For now, I
just wanted to address immediate continuation based concern.)

If you're in a continuation based monad and you need exception safe resource
handling, there *is* a solution:
[resourcet](https://www.fpcomplete.com/user/snoyberg/library-documentation/resourcet).
resourcet hoists the exception safety outside of the realm of the continuation
based code, and maintainers finalizer functions via mutable variables. Note
that this isn't just useful for continuation based monads, but for any
situation where you don't have full control over the flow of execution of the
program. For example, you'd use the same technique for [an io-streams directory
traversal](https://gist.github.com/gregorycollins/00c51e7e33cf1f9c8cc0).

One last caveat. There *is* one case where a continuation based monad could in
theory have a valid `bracket` function, which is where you have full knowledge
of the code which will run the continuation, and can guarantee that all
continuations will always be executed. So if you hide constructors and only
expose such run functions, you might be safe. But the burden of proof is on
you.

* * *

Note: I'm purposely *not* linking to any of the conversations I've referred to
about getting a bracket function for continuation based monads, I don't feel
like calling people out here. Also, in case you don't feel like loading up the
School of Haskell page, here's the full source code for my example above:

```haskell
{-# LANGUAGE PackageImports #-}
import           "MonadCatchIO-transformers" Control.Monad.CatchIO  (MonadCatchIO,
                                                                     bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString        (ByteString, hPut)
import           Data.Enumerator        (Iteratee, run, ($$))
import           Data.Enumerator.Binary (enumFile, iterHandle)
import           Snap.Iteratee          () -- orphan instance
import qualified System.IO              as IO

iterFile :: (MonadCatchIO m, MonadIO m, Functor m)
         => FilePath -> Iteratee ByteString m ()
iterFile fp = bracket
    (liftIO $ do
        putStrLn $ "opening file for writing: " ++ fp
        IO.openFile fp IO.WriteMode)
    (\h -> liftIO $ do
        putStrLn $ "closing file for writing: " ++ fp
        IO.hClose h)
    iterHandle

main :: IO ()
main = do
    writeFile "exists.txt" "this file exists"
    run (enumFile "exists.txt" $$ iterFile "output1.txt") >>= print
    run (enumFile "does-not-exist.txt" $$ iterFile "output2.txt") >>= print
```

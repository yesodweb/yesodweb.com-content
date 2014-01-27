Back in June, Gabriel Gonzalez [wrote a blog post on the Resource
monad](http://www.haskellforall.com/2013/06/the-resource-applicative.html). At
the time, I thought it was an interesting idea, but I didn't have a very good
use case for it. However, while [working on Persistent
2.0](https://github.com/yesodweb/persistent/wiki/Persistent-2.0-goals), I
realized it was a great way to abstract the concept of acquiring a database
connection, and allow both `ResourceT` and non-`ResourceT` access to
Persistent.

So with Gabriel's permission to steal his idea, I [added the Resource monad to
the resourcet
package](http://hackage.haskell.org/package/resourcet-0.4.10/docs/Control-Monad-Trans-Resource.html#g:11).
The internal representation is slightly different than the one presented in
Gabriel's blog post. In order to provide proper async exception safety, the internal structure is:

```haskell
data Allocated a = Allocated !a !(IO ())

newtype Resource a = Resource ((forall b. IO b -> IO b) -> IO (Allocated a))

instance Monad Resource where
    return a = Resource (\_ -> return (Allocated a (return ())))
    Resource f >>= g' = Resource $ \restore -> do
        Allocated x free1 <- f restore
        let Resource g = g' x
        Allocated y free2 <- g restore `E.onException` free1
        return $! Allocated y (free2 `E.finally` free1)
```

`Allocated` provides a value and its cleanup method. `Resource` is a function
from `mask`'s `restore` function to an action returning `Allocated`. By being
set up in this way, we know that async exceptions are not thrown in the
intermediate steps of monadic bind.

Usage of the API is pretty simple. We can create a file-opening resource:

```haskell
openFileResource :: FilePath -> IOMode -> Resource Handle
openFileResource fp mode = mkResource (openFile fp mode) hClose
```

Using the `Applicative` instance, we can then build this up into a `Resource`
for allocating both a file reader and writer:

```haskell
myHandles :: Resource (Handle, Handle)
myHandles = (,)
    <$> openFileResource "input.txt" ReadMode
    <*> openFileResource "output.txt" WriteMode
```

And then we can allocate these `Handle`s with either the bracket pattern:

```haskell
bracketCopy :: IO ()
bracketCopy = with myHandles $ \(input, output) ->
    sourceHandle input $$ sinkHandle output
```

or using `ResourceT` itself:

```haskell
resourcetCopy :: IO ()
resourcetCopy = runResourceT $ do
    (releaseKey, (input, output)) <- allocateResource myHandles
    sourceHandle input $$ sinkHandle output
    release releaseKey
```

Hopefully others will find this abstraction useful as well.

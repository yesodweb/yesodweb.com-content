This blog post is also available [as a School of Haskell tutorial](https://www.fpcomplete.com/user/snoyberg/general-haskell/basics/foldable-mapm-maybe-and-recursive-functions). I recommend reading the content there, as you can use active code.

* * *

I've [run into this issue myself](https://github.com/snoyberg/conduit/commit/11877684b3adb7ca422ae5000fab1ebeb3fbe142), and seen others hit it too. Let's start off with some very simple code:

```haskell active
sayHi :: Maybe String -> IO ()
sayHi mname =
    case mname of
        Nothing -> return ()
        Just name -> putStrLn $ "Hello, " ++ name

main :: IO ()
main = sayHi $ Just "Alice"
```

There's nothing amazing about this code, it's pretty straight-forward pattern matching Haskell. And at some point, many Haskellers end up deciding that they don't like the explicit pattern matching, and instead want to use a combinator. So the code above might get turned into one of the following:

```haskell active
import qualified Data.Foldable as F
hiHelper :: String -> IO ()
hiHelper name = putStrLn $ "Hello, " ++ name

sayHi1 :: Maybe String -> IO ()
sayHi1 = maybe (return ()) hiHelper

sayHi2 :: Maybe String -> IO ()
sayHi2 = F.mapM_ hiHelper

main :: IO ()
main = do
    sayHi1 $ Just "Alice"
    sayHi2 $ Just "Bob"
    -- or often times this:
    F.forM_ (Just "Charlie") hiHelper
```

The theory is that all three approaches (`maybe`, `mapM_`, and `forM_`) will end up being identical. We can fairly conclusively state that `forM_` will be the exact same thing as `mapM_`, since [it's just `mapM_` flipped](http://haddocks.fpcomplete.com/fp/7.4.2/20130829-168/base/src/Data-Foldable.html#forM_). So the question is: will the `maybe` and `mapM_` approaches do the same thing? In this case, the answer is yes, but let's spice it up a bit more. First, the `maybe` version:


```haskell active
import qualified Data.Text.Lazy as T
import qualified Data.Foldable as F
import Control.Monad (when)

printChars :: Int -> T.Text -> IO ()
printChars idx t = maybe (return ()) (\(c, t') -> do
    when (idx `mod` 100000 == 0)
        $ putStrLn $ "Character #" ++ show idx ++ ": " ++ show c
    printChars (idx + 1) t') (T.uncons t)

main :: IO ()
main = printChars 1 $ T.replicate 5000000 $ T.singleton 'x'
```

The code above works correctly in constant space. However, the usage of `maybe` makes this a bit ugly. This is a common time to use `forM_` to syntactically clean things up. So let's give that a shot:


```haskell active
import qualified Data.Text.Lazy as T
import qualified Data.Foldable as F
import Control.Monad (when)

printChars :: Int -> T.Text -> IO ()
printChars idx t = F.forM_ (T.uncons t) $ \(c, t') -> do
    when (idx `mod` 100000 == 0)
        $ putStrLn $ "Character #" ++ show idx ++ ": " ++ show c
    printChars (idx + 1) t'

main :: IO ()
main = printChars 1 $ T.replicate 5000000 $ T.singleton 'x'
```

The code is certainly cleaner and easier to follow. However, try running it: you'll get a stack overflow. The issue is that the implementation of `mapM_` in `Data.Foldable` is not tail recursive. As a result, each recursive call ends up accumulating a bunch of "do nothing" actions to perform after completing the recursive call, which wipes out the stack.

Fortunately, solving this issue is pretty easy: write a tail-recursive version of `forM_` for `Maybe`:

```haskell active
import qualified Data.Text.Lazy as T
import qualified Data.Foldable as F
import Control.Monad (when)

forM_Maybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
forM_Maybe Nothing _ = return ()
forM_Maybe (Just x) f = f x

printChars :: Int -> T.Text -> IO ()
printChars idx t = forM_Maybe (T.uncons t) $ \(c, t') -> do
    when (idx `mod` 100000 == 0)
        $ putStrLn $ "Character #" ++ show idx ++ ": " ++ show c
    printChars (idx + 1) t'

main :: IO ()
main = printChars 1 $ T.replicate 5000000 $ T.singleton 'x'
```

There's one slight difference in the type of `forM_Maybe` and `forM_` specialized to `Maybe`. The former takes a second argument of type `a -> m ()`, while the latter takes a second argument of type `a -> m b`. This difference is unfortunately necessary; if we try to get back the original type signature, we have to add an extra action to wipe out the return value, which again reintroduces the stack overflow:

```haskell active
import qualified Data.Text.Lazy as T
import qualified Data.Foldable as F
import Control.Monad (when)

forM_Maybe :: Monad m => Maybe a -> (a -> m b) -> m ()
forM_Maybe Nothing _ = return ()
-- show
forM_Maybe (Just x) f = f x {-hi-}>> return (){-/hi-}
-- /show

printChars :: Int -> T.Text -> IO ()
printChars idx t = forM_Maybe (T.uncons t) $ \(c, t') -> do
    when (idx `mod` 100000 == 0)
        $ putStrLn $ "Character #" ++ show idx ++ ": " ++ show c
    printChars (idx + 1) t'

main :: IO ()
main = printChars 1 $ T.replicate 5000000 $ T.singleton 'x'
```

## mono-traversable

I'd like to address this issue in [mono-traversable](http://hackage.haskell.org/package/mono-traversable), but it would require changing the type of `mapM_` and `forM_`. I'm tempted to do so, but am interested if it causes breakage for anyone. If you have an opinion on this, please [comment on the Github issue](https://github.com/snoyberg/mono-traversable/issues/28). For the record, here's the same stack overflow with mono-traversable.

```haskell active
import qualified Data.Text.Lazy as T
import Data.MonoTraversable (oforM_)
import Control.Monad (when)

printChars :: Int -> T.Text -> IO ()
printChars idx t = oforM_ (T.uncons t) $ \(c, t') -> do
    when (idx `mod` 100000 == 0)
        $ putStrLn $ "Character #" ++ show idx ++ ": " ++ show c
    printChars (idx + 1) t'

main :: IO ()
main = printChars 1 $ T.replicate 5000000 $ T.singleton 'x'
```

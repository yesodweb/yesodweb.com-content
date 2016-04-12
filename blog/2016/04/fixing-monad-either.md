Over the years, much has been said about Either-like `Monad` instances. To summarize our history, we've had:

* No `Monad` instance at all for `Either`
* An orphan `Monad` instance in `transformers` that used the `Error` class
* A `Monad` instance for `Either` in base without an `Error` constraint
* The `ErrorT` transformer, which introduces the `Error` class constraints
* The `EitherT` transformer from the `eithert` package
* The newer `ExceptT` transformer, which greatly improves on `EitherT` by giving it a more intuitive name

I'm not going to dive into all of these points, I merely raise them to stress the idea of how many approaches have been attempted to get this right. I'd like to explain how our current situation is wrong, how we can do much better, and propose that – despite the large impact on all Haskell users out there to make such a change – we should finally fix our library ecosystem correctly, once and for all.

## The broken `fail` function

The real crux of the matter here is that, as everyone knows, the `Monad` instance for `Either` is exclusively used for representing some kind of error/failure/exception/etc (all those terms are, of course, synonymous). In the past, with the `Error` constraint, we had a valid implementation of the `fail` method for `Either`, which meant this vital method was properly implemented. Unfortunately now, we have `fail = error` for `Either`, preventing us from doing proper exception handling without resorting to more complex tricks.

## The problem with `Error`

If we look at the definition of the `Error` typeclass, we'll see two methods:

```haskell
class Error a where
    noMsg :: a
    strMsg :: String -> a
```

However, there's a perfectly good implementation of `noMsg` in terms of `strMsg`: `noMsg = strMsg ""`. So really, our `Error` typeclass boils down to just one method of type `String -> a`. Once we realize that, it turns out that there's a far more appropriate typeclass already present in base to represent errors:

```haskell
class IsString a where
    fromString :: String -> a
```

## The proper `Monad Either` instance

So now we can define a proper `Monad` instance for `Either`:

```haskell
instance IsString e => Monad (Either e) where
    return = ...
    (>>=) = ...

    fail = Left . fromString
```

This allows us to recover a properly behaving `fail` function, restoring sanity to our world.

## Either for arbitrary error types

The final issue we may wish to address is allowing `Either` to work for arbitrary types. We can do so by leveraging overlapping instances, one of the most powerful and ubiquitous language extensions available in GHC. Using `Typeable`, we can generate truly beautiful error messages for our usage of `Either`. Below is a proof of concept demonstrating how we can fully fix our `Either` situation in GHC.

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.String
import Data.Typeable

data Either' a b = Left' a | Right' b
    deriving (Functor, Show)

instance Applicative (Either' a) where
    pure = Right'

    Left' a <*> _ = Left' a
    Right' _ <*> Left' a = Left' a
    Right' f <*> Right' x = Right' (f x)

instance IsString a => Monad (Either' a) where
    return = pure
    (>>) = (*>)

    Left' e >>= _ = Left' e
    Right' x >>= f = f x

    fail = Left' . fromString

instance {-# OVERLAPPABLE #-} Typeable a => IsString a where
    fromString s =
        res
      where
        res = error $ concat
            [ "No specific IsString instance for "
            , show $ typeRep $ Just res
            , ", when converting string: "
            , s
            ]

main :: IO ()
main = do
    print (fail "failure 1" :: Either' String Int)
    print ((do
        Just x <- return Nothing
        return x) :: Either' String Int)
    print ((do
        Just x <- return Nothing
        return x) :: Either' Int Int)
```

## What's next

I'm sure most of you are in full agreement with me at this point that this is our only way forward. Hopefully we can have a quick discussion period on the libraries@ list, and get this into base in time for GHC 8, which is currently on release candidate 37.

Let's suppose that you're using a library at version 1. It exposes a function:

    someFunc :: Int -> String
    someFunc i = 'x' : show i

In version 2 of the library, someone realizes that this library isn't
general-purpose enough: why is the 'x' character hardcoded? So version 2
exposes a more powerful version of the function:

    someFunc :: Int -> Char -> String
    someFunc i c = c : show i

In your current codebase, you have:

    main = putStrLn $ someFunc 5

Changing that code to work with version 2 is trivial:

    main = putStrLn $ someFunc 5 'x'

But what if you want to make your code work with *both* versions? The real, proper answer, that I hope everyone actually uses, is to use Cabal CPP macros:

```haskell
#if MIN_VERSION_somelib(2, 0, 0)
main = putStrLn $ someFunc 5 'x'
#else
main = putStrLn $ someFunc 5
#endif
```

And sure, you *should* do that... but let's have some fun. I'm going to present
three evil techniques to accomplish the same conditional compilation result,
and try to point out their relative merits. I encourage others to come up with
other ridiculous ideas of their own.

If anyone's curious where I came up with the idea to do this, it was [thinking
about prerelease GHC patch level releases that break backwards
compatibility](https://github.com/yesodweb/yesod/issues/748#issuecomment-45475090).
And if you want to play around with the code, either [open it in FP Haskell
Center](https://www.fpcomplete.com/user/snoyberg/random-code-snippets/evil-conditional-compilation)
or [clone the Github
repo](https://github.com/snoyberg/evil-conditional-compilation). In all the
examples, simply switch whether V1 or V2 is imported to simulated
upgrading/downgrading dependencies.

## Typeclasses

A well known truth in Haskell is, "for every problem, there's an abuse of
typeclasses waiting to be discovered." As far as typeclass abuses go, this one
is pretty benign.

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Typeclass where

import V1
-- import V2

class Evil a where
    evil :: a -> String
instance Evil String where
    evil = id
instance Evil a => Evil (Char -> a) where
    evil f = evil (f 'x')

main :: IO ()
main = putStrLn $ evil $ someFunc 5
```

What's "nice" about this approach (if anything here is nice) is that everything
is compile-time checked, and the code is actually pretty readable. However, as
the different cases you want to support get more complicated, you'll need to
add in ever harrier language extensions.

## Typeable

This approach is for the Pythonista in you. Next time someone proudly states
that Haskell has a statically typed language, just pull this one out:

```haskell
module Typeable where

import Data.Typeable
-- import V1
import V2

evil :: Typeable a => a -> String
evil a
    | Just s <- cast a = s
    | Just f <- cast a = f 'x'
    | otherwise = error "Yay, runtime type errors!"

main :: IO ()
main = putStrLn $ evil $ someFunc 5
```

Advantage: it's so incredibly trivial to add more cases. Downsides: it's
runtime type checking, and the dispatch is performed at runtime, not compile
time.

## Template Haskell

Of course, no blog post about Haskell abuse would be complete without some
usage of Template Haskell. Due to the stage restriction, we have to split this
into two files. First, THHelper:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module THHelper where

import Language.Haskell.TH

evil :: Name -> Q Exp
evil name = do
    VarI _ typ _ _ <- reify name
    case typ of
        AppT (AppT ArrowT (ConT _)) (ConT _) ->
            return $ VarE name
        AppT (AppT ArrowT (ConT _)) (AppT (AppT ArrowT (ConT _)) (ConT _)) ->
            [|flip $(return $ VarE name) 'x'|]
        _ -> error $ "Unknown type: " ++ show typ
```

Notice how beautiful our pattern matching it. This combines the best (worst) of
both worlds from above: we get full compile time checking, and can easily
(hah!) pattern match on all possible signatures for the function at hand.

And calling this beast is equally elegant:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module TH where

import THHelper
import V1
-- import V2

main :: IO ()
main = putStrLn $ $(evil 'someFunc) 5
```

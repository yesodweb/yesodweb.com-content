There is a not-often-used part of the WAI `Request` value - the `vault` field -
which provides a means of extension, especially for WAI middlewares. Getting
all of the pieces to fit together exactly right isn't always obvious. This blog
post is intended to provide some quick examples of how to use it, and how a web
framework (such as Yesod) can interact with such values.

## What is a vault?

The `vault` field is a value from the [vault
package](http://www.stackage.org/package/vault). A `Vault` is essentially a
`Map` that can hold heterogenous data, and whose lookup keys are well typed.
We'll focus on just the `IO`-specified version of the interface, but there's
also an `ST` interface.

## Example 1: safe key generation, WAI application

The big trick with vault for our purposes is that the keys are fully opaque,
and must be generated in the `IO` monad. Our first example program will have a
middleware that accepts a `Key` as a parameter and sets a value in the `Vault`,
and an `Application` that also accepts that `Key` as a parameter and uses it.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-3.9 runghc --package warp --package random

import qualified Data.Vault.Lazy as V
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import System.Random
import qualified Data.ByteString.Lazy.Char8 as L8

middleware :: V.Key Int -> Middleware
middleware key app req respond = do
    -- Generate a random number
    value <- randomIO
    let vault' = V.insert key value (vault req)
        req' = req { vault = vault' }
    app req' respond

app :: V.Key Int -> Application
app key req respond =
    respond $ responseLBS status200 [] $ L8.pack str
  where
    str =
        case V.lookup key (vault req) of
            Nothing -> "Key not found"
            Just value -> "Random number is: " ++ show value

main :: IO ()
main = do
    key <- V.newKey
    run 3000 $ middleware key $ app key
```

## Example 2: unsafe key generation, WAI application

In practice, while generating the key like this works, it can be annoying to
have to pass it into your middlewares and applications. Instead, you can use
`unsafePerformIO` when defining your middleware to avoid the problem. This is a
safe usage, though you need to make sure to use the `NOINLINE` pragma to
ensure that the computation is only run once.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-3.9 runghc --package warp --package random

import qualified Data.Vault.Lazy as V
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import System.Random
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO.Unsafe

randomKey :: V.Key Int
randomKey = unsafePerformIO V.newKey
{-# NOINLINE randomKey #-}

middleware :: Middleware
middleware app req respond = do
    -- Generate a random number
    value <- randomIO
    let vault' = V.insert randomKey value (vault req)
        req' = req { vault = vault' }
    app req' respond

app :: Application
app req respond =
    respond $ responseLBS status200 [] $ L8.pack str
  where
    str =
        case V.lookup randomKey (vault req) of
            Nothing -> "Key not found"
            Just value -> "Random number is: " ++ show value

main :: IO ()
main = run 3000 $ middleware app
```
## Example 3: safe key generation, Yesod application

Adding this same approach to a Yesod application is fairly straight-forward, the only two tricks are:

* Put the generated key into the foundation data type
* Use the `waiRequest` function to get access to the raw request value

A simple working exactly:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-3.9 runghc --package yesod-core

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import qualified Data.Vault.Lazy as V
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import System.Random
import qualified Data.ByteString.Lazy.Char8 as L8
import Yesod.Core

middleware :: V.Key Int -> Middleware
middleware key app req respond = do
    -- Generate a random number
    value <- randomIO
    let vault' = V.insert key value (vault req)
        req' = req { vault = vault' }
    app req' respond

app :: V.Key Int -> Application
app key req respond =
    respond $ responseLBS status200 [] $ L8.pack str
  where
    str =
        case V.lookup key (vault req) of
            Nothing -> "Key not found"
            Just value -> "Random number is: " ++ show value

data App = App
    { randomKey :: V.Key Int
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    App key <- getYesod
    req <- waiRequest
    defaultLayout [whamlet|#{show $ V.lookup key (vault req)}|]

main :: IO ()
main = do
    key <- V.newKey
    app <- toWaiApp $ App key
    run 3000 $ middleware key app
```

## Exercise: unsafe key generation, Yesod application

The last example in our matrix would be using `unsafePerformIO` for generating
the key, and using it in a Yesod application. That should be a straightforward
modification of what we've already seen, and is left as an exercise to the
reader.

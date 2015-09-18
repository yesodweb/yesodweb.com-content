The Hamlet HTML templating system is, by default, parsed at compile time,
allowing it to do quite a bit of static checking. But it can also be useful to
parse these templates at runtime, such as if you wanted to allow dynamic
content to be submitted by users.

A few weeks ago, [a discussion on
Reddit](https://www.reddit.com/r/haskell/comments/3i2xfd/two_wrongs_static_generation_with_haskell/cuf8hig?context=4)
pointed out that the API for runtime Hamlet in shakespeare was pretty bad. Just
a week later, I came up with a use case for runtime Hamlet myself, and decided
to put together a new, more user friendly API.

This new API is [provided in
Text.Hamlet.Runtime](http://haddock.stackage.org/nightly-2015-09-17/shakespeare-2.0.6/Text-Hamlet-Runtime.html).
Hopefully it's much easier to follow going on here. And this time, there are
even comments on the functions! I'm including the example from the module
below to give a better feel for how this works:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Text.Hamlet.Runtime
import qualified Data.Map as Map
import Text.Blaze.Html.Renderer.String (renderHtml)

main :: IO ()
main = do
    template <- parseHamletTemplate defaultHamletSettings $ unlines
        [ "<p>Hello, #{name}"
        , "$if hungry"
        , "  <p>Available food:"
        , "  <ul>"
        , "    $forall food <- foods"
        , "      <li>#{food}"
        ]
    let hamletDataMap = Map.fromList
            [ ("name", "Michael")
            , ("hungry", toHamletData True) -- always True
            , ("foods", toHamletData
                [ "Apples"
                , "Bananas"
                , "Carrots"
                ])
            ]
    html <- renderHamletTemplate template hamletDataMap
    putStrLn $ renderHtml html
```

Thank you very much to everyone who has participated so far in the [Yesod User
Sruvey 2012](http://www.yesodweb.com/blog/2012/10/yesod-user-survey-2012), the
response was even stronger than I'd hoped for, and the feedback has been very
helpful.

Two of the questions on the survey had to do with our usage of Template Haskell
(TH) and Quasi Quotes (QQ) in Yesod. My goal was to determine whether their
usage was something that impacted people across the board, as opposed to a
specific segment of the Haskell population, and therefore whether it was worth
putting in the effort to create and maintain a non-TH package.

Much to my surprise, the feedback seemed to be spread completely evenly across
the board. A solid 30% of people across the board agreed that there was too
much TH and QQ in Yesod. So in this blog post, I'd like to announce and
describe a [new, slightly experimental package called
yesod-pure](http://hackage.haskell.org/package/yesod-pure) that allows you to
use Yesod without any code generation.

Note: I'm trying a slightly new format for displaying this information. All of
the code is in a [Github
gist](https://gist.github.com/3870834/b272e9c3a31ddc5b2ddddb0ccbf1849551e46f2b),
and each step is a separate commit. I'd appreciate feedback on whether this
format is helpful or not.

(Side note: I put together a [demonstration called
yesod-alternative](https://plus.google.com/u/0/116553865628071717889/posts/FMWuw7CtXa7)
a while ago showing how Heist and acid-state could be used with Yesod. That
effort could in theory be merged with yesod-pure to some extent if there is
interest.)

### Step 1: A normal Yesod app

Before we can just get rid of TH and QQ, we need to determine what we're
getting rid of. Yesod uses them in three different places:

* Routing
* Shakespearean templates
* Persistent

For our current purposes, I'm going to completely ignore Persistent. It's not
directly part of Yesod-the-framework, and already has its TH components in a
completely separate package. We might add some support for Persistent in the
future.

As a motivating example, let's create a Yesod application the normal way. It's
a simple app: a homepage with a type-safe link, and a separate route for
Fibonacci numbers. You can [see the
code](https://gist.github.com/3870834/b272e9c3a31ddc5b2ddddb0ccbf1849551e46f2b).

In this example, we've used `mkYesod` and `parseRoutes` to deal with the
routing, `whamlet` for generating HTML, and `lucius` for generating CSS.

### Step 2: No more Shakespeare

Let's take the low-hanging fruit: removing Shakespeare. This is actually
relatively easy. Hamlet is built on top of the
[blaze-html](http://hackage.haskell.org/package/blaze-html) library, so we can
just replace Hamlet with Blaze's combinators. For CSS, we're going to use plain
text fed into yesod-pure's `addCSS` function.

[Full source code](https://gist.github.com/3870834/7ab89b500616248c0de8b5d3918e14793afbca36)

```diff
@@ -3,7 +3,10 @@
 {-# LANGUAGE QuasiQuotes           #-}
 {-# LANGUAGE TemplateHaskell       #-}
 {-# LANGUAGE TypeFamilies          #-}
-import           Yesod
+import           Text.Blaze.Html             (toValue, (!))
+import qualified Text.Blaze.Html5            as H
+import qualified Text.Blaze.Html5.Attributes as HA
+import           Yesod.Pure
 
 data App = App
 
@@ -17,19 +20,21 @@ instance Yesod App
 getHomeR :: Handler RepHtml
 getHomeR = defaultLayout $ do
     setTitle "Hello World!"
-    toWidget [whamlet|
-<p>Hello World
-<a href=@{FibR 5}>Fifth fib
-|]
-    toWidget [lucius|p { color: red }|]
+    toWidget $ \render -> do
+        H.p "Hello World"
+        H.a ! HA.href (toValue $ render (FibR 5) []) $ "Fifth fib"
+    addCSS "p { color: red }"
 
 getFibR :: Int -> Handler RepHtml
 getFibR i = defaultLayout $ do
     setTitle "Fibs"
-    [whamlet|
-<p>Fib for #{i}: #{fib i}
-<a href=@{FibR $ i + 1}>Next fib
-|]
+    toWidget $ \render -> do
+        H.p $ do
+            "Fib for "
+            toHtml i
+            ": "
+            toHtml $ fibs !! i
+        H.a ! HA.href (toValue $ render (FibR $ i + 1) []) $ "Next fib"
 
 fib :: Int -> Int
 fib i = fibs !! i
```

One trick to notice is `toWidget $ \render -> do`. `render` is a __URL
rendering function__ provided by Yesod. We're able to use this to turn `FibR 5`
into a textual representation. This is the very heart of type-safe URLs:
instead of splicing text together, we ask the system itself to generate a URL
from a value known to be correct.

### Step 3: Rewrite the routing code

The next part is trickier. We need to write out routing code manually. Let's
spell out the different components of type-safe routing:

* A data type representing all possible routes in the app.
* A function that turns a type-safe route into a list of path segments (aka, renderer).
* A function that tries to turn a list of path segments into a type-safe route (aka, parser).
* A function that takes a type-safe route and dispatches to the appropriate handler code (aka, dispatcher).

In our previous code, all four pieces were being generated automatically by
`mkYesod` and `parseRoutes`. Now we'll write them all our separately.

[Full source code](https://gist.github.com/3870834/59342e83754459cae2b375923206449a2804941d)

```diff
@@ -1,8 +1,7 @@
 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE OverloadedStrings     #-}
-{-# LANGUAGE QuasiQuotes           #-}
-{-# LANGUAGE TemplateHaskell       #-}
 {-# LANGUAGE TypeFamilies          #-}
+import           Control.Applicative         ((<$>))
 import           Text.Blaze.Html             (toValue, (!))
 import qualified Text.Blaze.Html5            as H
 import qualified Text.Blaze.Html5.Attributes as HA
@@ -10,12 +9,28 @@ import           Yesod.Pure
 
 data App = App
 
-mkYesod "App" [parseRoutes|
-/ HomeR GET
-/fib/#Int FibR GET
-|]
+instance RenderRoute App where
+    data Route App = HomeR
+                   | FibR Int
+        deriving Eq
+    renderRoute HomeR = ([], [])
+    renderRoute (FibR i) = (["fib", toPathPiece i], [])
+
+parseRoute :: RouteParse App
+parseRoute [] = Just HomeR
+parseRoute ["fib", i] = FibR <$> fromPathPiece i
+parseRoute _ = Nothing
+
+dispatchRoute :: RouteDispatch App
+dispatchRoute "GET" HomeR = handler getHomeR
+dispatchRoute "GET" (FibR i) = handler $ getFibR i
+dispatchRoute _ _ = Nothing
+
+instance YesodDispatch App App where
+    yesodDispatch = dispatch parseRoute dispatchRoute
 
 instance Yesod App
+type Handler = GHandler App App
 
 getHomeR :: Handler RepHtml
 getHomeR = defaultLayout $ do
```

Notice that first change: we're no longer using the TemplateHaskell and
QuasiQuotes language extensions!

We've added a `RenderRoute` instance. The `Route` associated data type provides
all of the route constructors for our application, and renderRoute turns each
route into a list of path segments and a list of query string parameters. Every
Yesod app has this instance, it's just usually generated for you.

`parseRoute` is the inverse of `renderRoute`. In order to keep your
applicationn correct, you must ensure that these functions are always exact
inverses of each other. This is the biggest advantage we're losing by switching
away from TH. (The other big advantage we lose is brevity of code, but we'll
come back to that in step 4.)

Finally, we dispatch our application. We can see that we're only going to
respond to GET requests, though if you wanted to handle other requests it would
be trivial to add more clauses.

And we finally tie it all together with the `YesodDispatch` instance. This is
another instance which exists in every Yesod application, but isn't normally
visible. The `dispatch` function is provided by yesod-pure, and is just a
helper to simplify creating the `yesodDispatch` function. The latter is fairly
complicated, involving lots of parameters for dealing with subsites, 404 and
405 handlers, and so on. `dispatch` hides all those details from you.

Otherwise, our code remains unchanged. Type-safe URLs still work exactly as
before, and we run our code as previously. That should make sense: all we've
done is switched from an automated code generation to a manual process, but the
end result is almost identical. (The TH code is actually includes a few
performance enhnacements for the routing process, but that's not really
important for our purposes.)

### Step 4: Lose the type safety

The previous step let you create Yesod applications as you normally would by
manually writing some code. That approach let us keep all of our type-safe
features from Yesod, at the cost of writing a fair amount of boilerplate. But
suppose we'd rather give up some type safety in exchange for simpler code.
That's an option too.

Having a type-safe URL datatype means that we need to have all four components
listed above: a datatype, parsing, rendering, and dispatching. But if we drop
the URL datatype, we can get away with only dealing with dispatch, thus
simplifying our code significantly. That's the purpose of the
`Yesod.Pure.NoRoute` module, which provides routing combinators to automate
dispatch.

[Full source code](https://gist.github.com/3870834/78bed85b833d48691d20db6957fe9dcb8fb3ce70)

```diff
@@ -1,43 +1,24 @@
 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE OverloadedStrings     #-}
 {-# LANGUAGE TypeFamilies          #-}
-import           Control.Applicative         ((<$>))
+import           Control.Applicative         ((<|>))
 import           Text.Blaze.Html             (toValue, (!))
 import qualified Text.Blaze.Html5            as H
 import qualified Text.Blaze.Html5.Attributes as HA
-import           Yesod.Pure
-
-data App = App
-
-instance RenderRoute App where
-    data Route App = HomeR
-                   | FibR Int
-        deriving Eq
-    renderRoute HomeR = ([], [])
-    renderRoute (FibR i) = (["fib", toPathPiece i], [])
-
-parseRoute :: RouteParse App
-parseRoute [] = Just HomeR
-parseRoute ["fib", i] = FibR <$> fromPathPiece i
-parseRoute _ = Nothing
-
-dispatchRoute :: RouteDispatch App
-dispatchRoute "GET" HomeR = handler getHomeR
-dispatchRoute "GET" (FibR i) = handler $ getFibR i
-dispatchRoute _ _ = Nothing
-
-instance YesodDispatch App App where
-    yesodDispatch = dispatch parseRoute dispatchRoute
+import           Yesod.Pure.NoRoute
 
 instance Yesod App
 type Handler = GHandler App App
 
+fibR :: Int -> Route App
+fibR i = AppRoute ["fib", toPathPiece i]
+
 getHomeR :: Handler RepHtml
 getHomeR = defaultLayout $ do
     setTitle "Hello World!"
     toWidget $ \render -> do
         H.p "Hello World"
-        H.a ! HA.href (toValue $ render (FibR 5) []) $ "Fifth fib"
+        H.a ! HA.href (toValue $ render (fibR 5) []) $ "Fifth fib"
     addCSS "p { color: red }"
 
 getFibR :: Int -> Handler RepHtml
@@ -49,7 +30,7 @@ getFibR i = defaultLayout $ do
             toHtml i
             ": "
             toHtml $ fibs !! i
-        H.a ! HA.href (toValue $ render (FibR $ i + 1) []) $ "Next fib"
+        H.a ! HA.href (toValue $ render (fibR $ i + 1) []) $ "Next fib"
 
 fib :: Int -> Int
 fib i = fibs !! i
@@ -58,4 +39,6 @@ fibs :: [Int]
 fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
 
 main :: IO ()
-main = warpDebug 3000 App
+main = warpDebug 3000 $ App $
+    method "GET" (serve getHomeR)
+    <|> static "fib" (dynamic $ \i -> method "GET" $ serve $ getFibR i)
```

We've replaced most of our initial code with some dispatch code we've placed in
`main`. I've used the `Alternative` interface to these combinators (there's
also a `Monoid` interface, and an more experimental `Monad` interface). They
should be mostly self-explanatory, but let me explain:

* `method` ensures that the request had the given method.
* `serve` uses the given `Handler` to respond, if there are no path segments left to be processed. So our first line will only respond to requests to the root of our app.
* `static` ensures that the next path segment is the given piece of text.
* `dynamic` will attempt to read the following path segment using `fromPathPiece`.
* `multi` (not featured here) will read *all* the remaining path segments with `fromPathMultiPiece`. This could be combined with `serve` to overcome the restriction of only serving when there are no remaining path segments.

This route definition should be identical to the ones we've used previously,
look through it carefully to be sure you have the feel. This is very much a
first stab at creating combinators for routing, if anyone has some
recommendations, please let me know (or sent a pull request!).

The final thing to point out is `fibR`. I lied a bit when I said we were
getting rid of type-safe URLs. There's still a datatype for all of our routes,
but now it's just a list of texts. `fibR` attempts to recapture some of the
safety of having dedicated constructors like `FibR`. If your app uses such
wrapper functions exclusively, then you can minimize the potential for invalid
URLs to just those wrapper functions.

## Conclusion

This package is just an initial release, and should still be considered experimental. I'm interested in hearing feedback on how it works. With it, we now have three ways of creating Yesod applications:

* The standard TH/QQ combination. We get brevity of code, guarantees that rendering and parsing are inverse of each other, and full type safety.
* The `Yesod.Pure` approach: code is longer and we lose guarantees that the parsing and rendering is correct, but we retain full type safety.
* `Yesod.Pure.NoRoute`: give up on some type safety in exchange for shorter code.

All three approaches allow you to access the full power of the Yesod ecosystem,
produce and consume widgets, deal with JSON data automatically, etc.

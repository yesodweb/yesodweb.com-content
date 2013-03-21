In my
[previous](http://www.yesodweb.com/blog/2013/03/yesod-1-2-cleaner-internals)
[two](http://www.yesodweb.com/blog/2013/03/big-subsite-rewrite) posts, I
discussed some significant changes coming to Yesod in the 1.2 release. Both of
these posts discussed the changes to *handlers*. Now I'd like to switch gears
and talk about the other half of yesod-core: *dispatch*.

As a tl;dr: in Yesod 1.1 and prior, the dispatch system was a fairly
complicated beast that was hidden from users behind Template Haskell
generators. None of those code generators are going away, and they'll continue
working just like they have until now. But the internals have been cleaned up
to such a point where they can be a user facing component without inducing
fear. And as a result, we're now in a position to provide alternate dispatch
systems. As an example:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Yesod.Core
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text, pack)

people :: [(Text, Int)]
people = [("Alice", 25), ("Bob", 43), ("Charlie", 37)]

main = warp 3000 $
    onStatic "people" (dispatchTo getPeople) <>
    onStatic "person" (withDynamic $ dispatchTo . getPerson)

getPeople = return $ toJSON $ map fst people

getPerson name =
    case lookup name people of
        Nothing -> notFound
        Just age -> selectRep $ do
            provideRep $ return $ object ["name" .= name, "age" .= age]
            provideRep $ return $ name <> " is " <> pack (show age) <> " years old"
```

Let's get into some details of how it works, and why it's designed that way.

## YesodDispatch

The core of any Yesod application is its foundation datatype. One of the
features it provides is the ability to perform some initialization before your
app starts running. Two prime examples of this are initializing a database
connection pool and setting up an HTTP connection manager. This foundation type
is then available to all of your handler functions, which can access this
initialized state.

But there's other initialization that Yesod itself must perform. In particular,
we need to load up the clientsession encryption key and create a `Logger`
value. There might also be other activities to perform in the future. But for
now, our application needs to have access to these three pieces of data
(foundation, session backend, and logger) in order to process requests. Let's
represent that with
[YesodRunnerEnv](https://github.com/yesodweb/yesod/blob/2a719941ca7733a9a2ff17cf093c245bb3e5c8d3/yesod-core/Yesod/Core/Types.hs#L187).

So now we've got our environment; what do we do with it? Presumably we could
just return a handler. But that would actually be a bit inefficient for some
use cases. In particular, we have full support in Yesod for having subsites
that were not written in Yesod. yesod-static is a prime example of this: it's a
thin wrapper around the wai-app-static package. In fact, any WAI app can be
used as a Yesod subsite. Theoretically, this applies to
[Scotty](http://hackage.haskell.org/package/scotty) and even
[Happstack](https://github.com/aslatter/happstack-wai).

Alright, back to the point at hand: having our dispatch return a handler would
mean that WAI subsites would have to go through a bunch of unnecessary
processing for loading session variables, converting GET parameters to Text,
etc. So dispatching should return the lowest common denominator: a WAI
application.

Putting these two pieces of information together, we know what a dispatch
function needs to look like. We stick the whole thing in [a
typeclass](https://github.com/yesodweb/yesod/blob/2a719941ca7733a9a2ff17cf093c245bb3e5c8d3/yesod-core/Yesod/Core/Class/Dispatch.hs)
and get:

    class Yesod site => YesodDispatch site where
        yesodDispatch :: YesodRunnerEnv site -> W.Application

We'll come back to this typeclass in a little bit.

## toWaiApp(Plain)

Let's move up the stack a bit. Assuming we have some type that implements
`YesodDispatch`, how do we run it? Yesod is built on WAI, so what we really
need is a WAI Application. That seems easy enough: just provide a
`YesodRunnerEnv`. And in fact, that's basically all we do. You can see the
implementation in
[toWaiAppPlain](https://github.com/yesodweb/yesod/blob/2a719941ca7733a9a2ff17cf093c245bb3e5c8d3/yesod-core/Yesod/Core/Dispatch.hs#L69).
This function will create a `YesodRunnerEnv` from your foundation type. It uses
methods from the Yesod typeclass to determine how to create this environment.
And then it applies [a small middleware
wrapper](https://github.com/yesodweb/yesod/blob/2a719941ca7733a9a2ff17cf093c245bb3e5c8d3/yesod-core/Yesod/Core/Dispatch.hs#L81)
to clean up requested paths

Once you have a WAI Application, you can apply more middlewares if you want.
toWaiApp applies some commonly used middlewares to get a more featureful
application. Finally, you can pass the Application to any WAI handler. In
production, this will usually be Warp. But you can also use wai-test to perform
some local, non-network testing of your application. In fact, the Yesod
testsuite does this extensively, and the yesod-test package leverages this
functionality as well. Some basic yesod-test testing is included with the
scaffolding.

## Creating a YesodDispatch

We've now covered what YesodDispatch does and how it's used. How do you
actually write an instance? Most users will never have to: the Template Haskell
provided by Yesod will generate it all for you based on the high-level route
syntax. I personally think this is the best approach to take for most
applications, but it doesn't satisfy everyone's needs. So one of the major
goals of the 1.2 rewrite is to open up the system to allow alternate
dispatching.

As both a proof-of-concept and a useful tool, I've included one such alternate
dispatch system in yesod-core. That's what powered the example given at the
beginning of this post. You can see [the full
implementation](https://github.com/yesodweb/yesod/blob/8c45b2709f3b34c71650f74be22163c885a12dd2/yesod-core/Yesod/Core/Internal/LiteApp.hs).

Every site must have an associated route datatype. This is how type-safe URLs are implemented in general. However, in this light-weight dispatch system, we have no desire to create a meaningful route datatype. So instead, we have a simple wrapper around a list of path segments:

    data Route LiteApp = LiteAppRoute [Text]

And we provide instances for the RenderRoute and ParseRoute typeclasses based
on this. We also need to have an instance of the Yesod typeclass. For now, we
simply use default values for all methods, but in theory could override some,
or provide the user with a means of overriding specific settings. But for now,
we've just taken the simplest approach.

So with that overhead out of the way, we can focus on the important point: the
dispatch itself. LiteApp is defined as:

    newtype LiteApp = LiteApp
        { unLiteApp :: Method -> [Text] -> Maybe (HandlerT LiteApp IO TypedContent)
        }

The datatype is nothing more than a dispatch function itself! It takes a
request method and a list of path segments, and either returns Nothing (page
not found), or a handler to be used. We have a Monoid instance to combine these
together, and a number of primitive combinators for building up these values.
(See the source for more details.)

So the final piece is the YesodDispatch instance itself:

    instance YesodDispatch LiteApp where
        yesodDispatch yre req =
            yesodRunner
                (fromMaybe notFound $ f (requestMethod req) (pathInfo req))
                yre
                (Just $ LiteAppRoute $ pathInfo req)
                req
          where
            LiteApp f = yreSite yre

The last line gets the LiteApp value itself from the YesodRunnerEnv and unwraps the newtype wrapper, giving us a core dispatch function. The code:

    f (requestMethod req) (pathInfo req)

applies that function to the actual requested method and path. If `Nothing` is
returned, then we replace it with the `notFound` handler. Once we have a
handler function, we use the
[yesodRunner](https://github.com/yesodweb/yesod/blob/15bbd54e12f1c9514e85e604fca97ab4962ee987/yesod-core/Yesod/Core/Internal/Run.hs#L210)
function to convert it into a WAI application. (The details of how that works
goes back into the realm of handlers, so I'll stop discussion there.)

And just like that, we have an alternate dispatch system for Yesod. You're able
to still leverage the Yesod infrastructure for things like form parsing,
short-circuit responses, etc. And as described above, our dispatch system isn't
really tied to Yesod handlers at all: you can use any WAI applications. So
routing and dispatch are really two orthogonal components in the Yesod world.

## Subsites

Subsites are a bit different than normal apps. They need to know how to promote
their routes to the parent site's routes. As described in the previous post,
subsite handlers are just wrappers around the parent handlers. The subsite
knows how to unwrap that wrapping, but it also needs to know how to turn a
parent handler into a WAI Application. And we need to have all the same
environment as a standard dispatch.

So we're going to follow the same pattern from before. We have a [YesodSubRunnerEnv](https://github.com/yesodweb/yesod/blob/0546d566c3286e72d63def141d24123171506c18/yesod-core/Yesod/Core/Types.hs#L193) and a [YesodSubRunnerDispatch](https://github.com/yesodweb/yesod/blob/0546d566c3286e72d63def141d24123171506c18/yesod-core/Yesod/Core/Class/Dispatch.hs#L23) typeclass:

    class YesodSubDispatch sub m where
        yesodSubDispatch :: YesodSubRunnerEnv sub (HandlerSite m) m
                         -> W.Application

Probably the simplest implementation is WaiSubsite, which just wraps an existing WAI Application:

    instance YesodSubDispatch WaiSubsite master where
        yesodSubDispatch YesodSubRunnerEnv {..} req =
            app req
          where
            WaiSubsite app = ysreGetSub $ yreSite $ ysreParentEnv

Like normal sites, subsites can be created with Template Haskell as well,
though there's no requirement to use it, as demonstrated with WaiSubsite. You
can see a [small subsite
demo](https://github.com/yesodweb/yesod/tree/0546d566c3286e72d63def141d24123171506c18/demo/subsite)
in the repo.

## A peek at the TH

I don't want to dwell on the Template Haskell too much: I've discussed it in
the past, and frankly I don't think there's a lot of user benefit to seeing
what it's doing. At a very high level, the Template Haskell code will:

* Create your associated route datatype.
* Create RenderRoute and ParseRoute instances.
* Create a YesodDispatch instance that calls out to your handler functions, dispatching with the efficient yesod-routes package.

The basics chapter of the book has a [section on
routing](http://www.yesodweb.com/book/basics#routing-19) which gives some
demonstration generated code. With Yesod 1.2 the code will be a bit simpler,
but in reality will look quite different to be able to leverage the efficient
data structures used by yesod-routes. In other words, that section can provide
you some insight, but isn't the full story. If you're really curious to see
what code gets generated, you can compile with `-ddump-splices`.

## Upcoming dispatch features

So now I've laid out two dispatch systems: the TH-based system and LiteApp.
These will cover a large percentage of real world use cases. But I think
there's another use case to be handled better as well: RESTful web services.
Yesod works admirably for this already, but there are some improvements to be
made. We're currently discussing this at FP Complete, and will likely be using
it to power some of our future offerings. I don't have many details to share at
the moment, but will keep you updated on progress when something is available.

## Next time

I have one more feature that I want to describe for Yesod 1.2: better streaming
data support. Yesod has always been built around a streaming response
mechanism, but has required some clunky code to get it to work. Yesod 1.2
introduces a few helper functions that make the approach much more elegant.
Look for another post on this next week!

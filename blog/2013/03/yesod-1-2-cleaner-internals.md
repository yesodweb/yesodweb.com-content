The past 48 hours have been very exciting for me: I've done [possibly the
largest refactoring](https://gist.github.com/snoyberg/5133281) of the Yesod
codebase to date. Most of it was simply restructuring the internal code and
non-user-facing APIs. But there are some user visible changes going on right
now, and I think now's a good time to document them. I also have some ideas for
even more radical changes, which I'll touch on at the end of this post.

## Representations

The most important change I've just implemented was an overhaul of the
representation system. This may not even be a concept most Yesod users are
familiar with, but it happens to be one of the oldest features of Yesod. The
idea is to allow a single function to return a different representation of the
data (HTML, JSON, etc) depending on the client's Accept header.

Let's start by defining two datatypes which we'll use below.  A `ContentType`
is simply a `ByteString` holding the raw mimetype. A `Content` is a bit more
complicated: it's a sum type which can be a blaze-builder Builder, a conduit
Source, or a few other things which can be easily converted into a response
body. There's a `ToContent` typeclass which provides a function `toContent`
which converts many common datatypes into a `Content`.

I don't want to dwell on the old approach to much, but it boiled down to every
handler function returning something that looked like this:

    [ContentType] -> (ContentType, Content)

The input [ContentType] is a list of mime types that the client as requested,
ordered by preference. You can then write a function that will determine the
appropriate content based on what the user accepts. For example:

```haskell
myResponse userContentTypes =
    loop userContentsTypes
  where
    loop ("text/html":_) = ("text/html", htmlContent)
    loop ("application/json":_) = ("application/json", jsonContent)
    loop (_:rest) = loop rest
    loop [] = ("text/html", htmlContent)
```

This works out fairly nicely, except for one detail: it requires you to perform
all I/O actions (such as database queries) before you know which representation
of the data you need to provide to the user. This unfortunately doesn't work
out too well in practice, and therefore representations have not been well
utilized in Yesod.

In Yesod 1.2, we're going to have a completely different approach. A handler
function will instead need to return an instance of the ToTypedContent
typeclass, which looks like this:

    data TypedContent = TypedContent ContentType Content
    class ToTypedContent a where
        toTypedContent :: a -> TypedContent

We define some sensible instances in yesod-core itself:

```haskell
instance ToTypedContent Html where
    toTypedContent html = TypedContent "text/html" (toContent html)
instance ToTypedContent Data.Aeson.Value where
    toTypedContent value = TypedContent "application/json" (toContent value)
instance ToTypedContent Text where
    toTypedContent text = TypedContent "text/plain" (toContent text)
```

With this change, it's now perfectly acceptable to create a JSON response with
something as simple as `return $ toJSON myValue`. But how do we deal with
multiple representations? For that, we have a pair of helper functions, which
are easiest to understand with a simple example.

```haskell
getPersonR personid = do
    person <- runDB $ get404 personid
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Some Person"
            extraInfo <- getExtraInfo person
            $(widgetFile "person")
        provideRep $ toJSON person
        provideRep $ (personName person :: Text)
```

`selectRep` will get the parsed contents of the HTTP Accept header and
determine which representation should be used. `provideRep` provides an
additional representation for selection. Notice how you don't even need to
state the mime type: it's all inferred automatically through the type system.
(If you need something more dynamic, `provideRepType` is available as well.)

In our example, we start off with an HTML representation. This representation
requires some extra data to be looked up. We can safely perform the
`getExtraInfo` call inside the `provideRep` call, and the overhead of that
extra call will not affect the JSON and plain text representations.

You can also [see the code behind selectRep and provideRep itself](https://github.com/yesodweb/yesod/blob/1d0cac6e03a75d48d043bdd2227e32b9042ee7a9/yesod-core/Yesod/Handler.hs#L811).

I've already used this new API to clean up a few nagging issues (error messages
and authentication responses are now representation-aware). I'm hopeful that
this change makes it much more convenient for developers to create sites
catering to both a plain-HTML and rich client view.

## Caching

Continuing on the trend of inferring information from the types, we also have a
new request-local caching mechanism. A prime use case for this is
authentication checking. In a typical Yesod application, you'll need to check
if a user is logged in in a number of different places: the authorization code,
the `defaultLayout` function, database functions, and the handler itself.
Having to do a database round trip for the same data multiple times is
inefficient; we should be able to cache that data somewhere.

Yesod has had request local storage for a while now, but it required generation
of a unique key. Possible approaches to this are using Template Haskell to
generate one, or Data.Unique and some unsafePerformIO. But both of those
approaches are just inconvenient enough that this feature went unused.

[Luite, Felipe, Greg and I all discussed
this](https://github.com/yesodweb/yesod/issues/268) a while ago, and Felipe
mentioned using `TypeRep` (i.e., the Typeable typeclass) as a unique key. This
requires you to create a newtype wrapper for each piece of data you want
cached, but otherwise is unobtrusive. With that approach in hand, the entire
caching API becomes a single function:

    cached :: Typeable a => Handler a -> Handler a

To implemented our cached `maybeAuth` function, we could do something like:

```haskell
newtype CachedUser = CachedUser { unCachedUser :: Maybe User }
    deriving Typeable

cachedMaybeAuth = fmap unCachedUser $ cached $ fmap CachedUser maybeAuth
```

A variant of this has already been applied to the real `maybeAuth` in
`Yesod.Auth`, so no changes are required to your code, except ensuring that
your `User` type is an instance of `Typeable`.

The [implementation
itself](https://github.com/yesodweb/yesod/commit/9559c2a3454f4f69e0ad22471bc1b497aefe8ace)
is actually pretty simple.

## Handler typeclasses

The Yesod.Handler module (which will probably become Yesod.Core.Handler soon)
has a number of functions that can be used when writing Handler code. This
includes looking up GET parameters, sending redirects, and modifying the
session. And through Yesod 1.1, all of these functions have lived in the
GHandler monad.

There are a few reasons why this is suboptimal:

1. It doesn't allow us to lift into monad transformers automatically. A common
   workflow is using `defaultLayout` or `runDB` to deal with widgets or
   Persistent, and then needing to `lift` some operations from the GHandler monad.
   It would be nice to have automatic lifting.

2. GHandler, like IO, is a bit of a "sin bin." There's absolutely no control
   over what a user may do there.  By moving operations into typeclasses
   instead, we can isolate non-mutating effects, mutating effects, short-circuit
   effects, and IO actions, giving us more knowledge about what our code is doing
   from the types themselves. I don't know how many people will be interested in
   using the typeclasses in this way, but I have heard people express interest in
   this previously.

The move to typeclasses also opens up the possibility for some more radical
changes in the future without massive disruption for users. I'll touch on some
of these thoughts at the end of this post.

## YesodRequest/YesodResponse

This was actually the most invasive change to the codebase, but as it only
affects the internals I've left it toward the end of this post. In the WAI
world, we have a very simple model for an application: it takes a `Request`,
and returns a `Response`. The Yesod world seems to drastically complicate that
simple approach.

But in reality, Yesod *also* has the same simplistic approach available, it's
just always been buried under piles of code and strangely named functions/data
types. My refactoring makes this much clearer.

* The `Request` datatype from WAI does a very minimal amount of parsing. However, in Yesod we require a bit more processing of the incoming request to be performed. We store this extra information in the [YesodRequest](https://github.com/yesodweb/yesod/blob/4d6c114b125676b80508112824c3d513fcd69536/yesod-core/Yesod/Core/Types.hs#L92) datatype. This adds information like cookies and the user session.
* Similarly, The `Response` datatype from WAI is very low-level. A Yesod app may want to return a much more high-level response in terms of status code, content type, content, and an updated session. To allow for this, the [YesodResponse](https://github.com/yesodweb/yesod/blob/4d6c114b125676b80508112824c3d513fcd69536/yesod-core/Yesod/Core/Types.hs#L113) data type allows you to return either this higher-level response, or a low-level WAI response instead.
* As a parallel to the `Application` type in WAI, Yesod provides the [YesodApp](https://github.com/yesodweb/yesod/blob/4d6c114b125676b80508112824c3d513fcd69536/yesod-core/Yesod/Core/Types.hs#L213) synonym, which simply takes a `YesodRequest` and returns a `YesodResponse`.
* We need to store some kind of environment information which is not request-specific, so that our handler functions have access to it. This includes logging functions, error handlers, and the foundation datatype. All of this goes into [RunHandlerEnv](https://github.com/yesodweb/yesod/blob/4d6c114b125676b80508112824c3d513fcd69536/yesod-core/Yesod/Core/Types.hs#L167).
* There's also mutable data for each request, like the session and the cache. This goes in [GHState](https://github.com/yesodweb/yesod/blob/4d6c114b125676b80508112824c3d513fcd69536/yesod-core/Yesod/Core/Types.hs#L202).
* We finally tie up the per-request immutable data, environment data, and mutable data into a single datatype: [HandlerData](https://github.com/yesodweb/yesod/blob/4d6c114b125676b80508112824c3d513fcd69536/yesod-core/Yesod/Core/Types.hs#L181). Note that we wrap up the mutable data in an IORef instead of using a State monad since we need to maintain a consistent state even in the presence of exceptions.
* With all of that out of the way, our [GHandler monad](https://github.com/yesodweb/yesod/blob/4d6c114b125676b80508112824c3d513fcd69536/yesod-core/Yesod/Core/Types.hs#L198) is much less mysterious: it's just a Reader providing access to the HandlerData.
* [runHandler](https://github.com/yesodweb/yesod/blob/4d6c114b125676b80508112824c3d513fcd69536/yesod-core/Yesod/Core/Run.hs#L94) will take GHandler and convert it into a YesodApp by feeding it a HandlerData and converting the output into a YesodResponse. [defaultYesodRunner](https://github.com/yesodweb/yesod/blob/4d6c114b125676b80508112824c3d513fcd69536/yesod-core/Yesod/Core/Run.hs#L273) takes this a step further and creates a plain WAI Application.

If there are things that are unclear in the explanation above, please let me
know. I intend to include this text, or some version of it, in the code as a
high-level architectural view of yesod-core.

## Dispatch is a different beast

I like to classify Yesod as a Model-View-Controller (MVC) framework. In this
approach, the model is handled by Persistent (or whatever replacement you use
for data storage) and the view is handled by Shakesepare (again, this can be
replaced). The last piece of the puzzle, Controller, is what Yesod itself- and
the yesod-core package in particular- deal with.

Within controller, we can also make a few smaller pieces. The HTTP layer itself
is handled by WAI, for example. The final two pieces handled by yesod-core are
*dispatch* and *handlers*. Other frameworks will combine these two concepts
together; I think there's huge value to be gained by keeping them as separate
components.

So far, I've been focused entirely on the handler aspect of the puzzle.
Dispatch has already somewhat moved out of yesod-core itself into the
yesod-routes package which, despite its name, is actually not Yesod-specific,
as [can](http://hackage.haskell.org/package/wai-routes)
[be](http://hackage.haskell.org/package/wai-dispatch)
[seen](http://hackage.haskell.org/package/route-generator) by some of its
dependencies. My goal is to separate out even more of the dispatch
functionality from yesod-core itself, to open the door for others to use even
more of the functionality outside of Yesod, and to hopefully make a better
product for Yesod as well.

I haven't given this part of the process too much thought yet, but I'm
definitely playing around with the thought of merging in the ideas from
[yesod-pure](http://hackage.haskell.org/package/yesod-pure). I still believe
that code generation is the best bet for a robust dispatch system, but having
that code generation built on top of a more powerful, user accessible library
will make the generated code more transparent.

If anyone has thoughts on this part of the refactoring, please let me know!

## Better subsite approach for the future?

I had thought I would discuss some radical ideas for ways to clean up the
subsite system in Yesod, but this post is already long enough. I'll come out
with a separate post in a few days. I'll give this leader though: this change
is significantly more breaking in nature than anything I've implemented so far,
so I'm hesitant to move ahead with it. I might hold it off for the 1.2 release,
and then have a Yesod 2.0 release in the not-too-distant future with a higher
level of breakage.

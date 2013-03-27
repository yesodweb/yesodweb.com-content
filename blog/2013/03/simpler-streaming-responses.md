Yesod is built on top of WAI, which has always provided a means of creating
efficient, streaming responses. Throughout Yesod's development, this
functionality has always been present in one form or another. In Yesod 1.2, the
goal is to make it as simple as possible to leverage this functionality.

Let's kick off with a simple example, and then drill into the details:

```haskell
{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
import Yesod.Core
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Control.Concurrent.Lifted (threadDelay)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Control.Monad (forM_)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

getHomeR :: Handler TypedContent
getHomeR = do
    value <- lookupGetParam "x"
    case value of
        Just "file" -> respondSource typePlain $ do
            sendChunkText "Going to read a file\n\n"
            CB.sourceFile "streaming.hs" $= awaitForever sendChunkBS
            sendChunkText "Finished reading the file\n"
        Just "fibs" -> respondSource typePlain $ do
            forM_ fibs $ \fib -> do
                $logError $ "Got fib: " <> T.pack (show fib)
                sendChunkText $ "Next fib is: " <> T.pack (show fib) <> "\n"
                yield Flush
                sendFlush
                threadDelay 1000000
        _ -> fmap toTypedContent $ defaultLayout $ do
            setTitle "Streaming"
            [whamlet|
                <p>Notice how in the code above we perform selection before starting the stream.
                <p>Anyway, choose one of the options below.
                <ul>
                    <li>
                        <a href=?x=file>Read a file
                    <li>
                        <a href=?x=fibs>See the fibs
            |]

main = warp 3000 App
```

## Start simple: a standard response

Consider the following handler:

    getHomeR :: Handler Text
    getHomeR = return "Hello World!"

What exactly does Yesod do to make this into a response the client can see? The
important bit is the `ToTypedContent` typeclass. Every handler function has
`toTypedContent` applied to its result. So let's look at the relevant classes
and types.

```
type ContentType = ByteString
data Content = ContentBuilder !Blaze.Builder !(Maybe Int)
               -- ^ The content and optional content length.
             | ContentSource !(Source (ResourceT IO) (Flush Blaze.Builder))
             | ContentFile !FilePath !(Maybe FilePart)
             | ContentDontEvaluate !Content

data TypedContent = TypedContent !ContentType !Content
class ToTypedContent a where
    toTypedContent :: a -> TypedContent

-- Relevant instance
instance ToTypedContent Text where
    toTypedContent t = TypedContent
        "text/plain; charset=utf-8"
        (\t -> ContentBuilder (Blaze.fromText t) Nothing)
```

So every response has to be convertible to a `TypedContent`, which is two
pieces of information: the value for the `Content-Type` response header, and
the body fo the response. In our case, we use the `ContentBuilder` constructor,
which lets us leverage `blaze-builder`.

## Use the Source

`ContentBuilder` isn't our only option. We could serve a file with
`ContentFile`. `ContentDontEvaluate` is a modifier to deal with exceptions;
we'll discuss that a bit later. But for our streaming discussion, the most
interesting constructor is `ContentSource`. This uses a `conduit` `Source` for
creating streaming data. Let's try out a minimal example:

```
getHomeR :: Handler TypedContent
getHomeR = return $ TypedContent "text/plain" $ ContentSource $ do
    yield $ Chunk $ Blaze.fromText "Hello World!"
```

We can use the `TypedContent` and `ContentSource` constructors directly. The
result isn't really anything more impressive than what we had previously. Let's
improve that, by streaming two files consecutively:

```
getHomeR :: Handler TypedContent
getHomeR = return $ TypedContent "text/plain" $ ContentSource $ do
    mapOutput (Chunk . Blaze.fromByteString) $ sourceFile "file1.txt"
    mapOutput (Chunk . Blaze.fromByteString) $ sourceFile "file2.txt"
```

We're guaranteed that our response will live in constant memory and will
properly free resources. We have to play with `mapOutput`, `Chunk` and
`fromByteString` to convert a stream of `ByteString`s to a stream of flushable
`Builder`s.

## Make it prettier

Having to muck around with those lower-level details isn't fun. Let's bump it
up a level:

```haskell
getHomeR :: Handler TypedContent
getHomeR = respondSource "text/html" $ do
    sendChunk ("Some Text" :: Text)
    sendChunk ("Hello & Goodbye" :: Html)
```

`respondSource` wraps up the tedium of dealing with the constructors directly.
`sendChunk` will send a chunk of content to the user, and can take as an
argument most common textual types (String, strict/lazy Text, strict/lazy
ByteString, and Html). But this doesn't play very nicely with overloaded
strings, since you need to provide explicit annotations. So we also have simple
type-specified wrappers as well:

```
getHomeR :: Handler TypedContent
getHomeR = respondSource "text/html" $ do
    sendChunkText "Some Text"
    sendChunkHtml "Hello & Goodbye"
```

We can also use `sendFlush` to flush the buffer to the client immediately. And
we have the ability to use all common `conduit` concepts to build up our
`Source`.

And one final but important point: the base monad for the `Source` is
`Handler`, so you can perform arbitrary `Handler` operations inside your
`Source`, such as looking up query string parameters.

## Exceptions

Let's go back to non-streaming responses. Consider the following:

```haskell
getHomeR :: Handler Html
getHomeR =
    return $ "Hello " <> name <> "!"
  where
    name = error "Oops, forgot to set the name"
```

We have an exception being sent from pure code. Let's see what Yesod does with this:

![Pure exception](/assets/streaming/pure-exceptions.png)

This is the result we want: the user receives a 500 status code to indicate
that there was an error on the server. But how does this work? The pure
exception should only be discovered __after__ we already send our 200 status
code and response headers, right?

In fact, Yesod does some fancy footwork behind the scenes, and fully evaluates
pure response bodies before sending any data to the user, specifically to
ensure that the user gets proper response codes. And this is also the purpose
of the above-mentioned `ContentDontEvaluate` constructor: to give the user a
chance to override this behavior (e.g., for efficiency). For example, we can
modify our above code to read:

```haskell
getHomeR :: Handler (DontFullyEvaluate Html)
getHomeR =
    return $ DontFullyEvaluate $ "Hello " <> name <> "!"
  where
    name = error "Oops, forgot to set the name"
```

When run like this, the client receives an empty response from the server
instead.

"All very interesting," you might be saying, "but what does this have to do
with streaming responses?" Quite a bit, actually, as the same reasoning
applies. When using streaming responses, there's no way for Yesod to fully
evaluate your response body before sending them to the client. So if you throw
an exception in your `Source`, the client will get a corrupted response. This
isn't to say you shouldn't use streaming responses, but you have to be careful.

## Logic before streaming

Exceptions aren't the only issue. You can't modify the status code or response
headers at all once you're inside the `Source`. That means you can't perform
redirects, can't modify the session, or can't switch from a 200 OK response to
a 403 Forbidden response. The important point here is to perform your logic
__before__ streaming.

```
getHomeR :: Handler TypedContent
getHomeR = do
    maybeFoo <- lookupGetParam "foo"
    case maybeFoo of
        Just "yesod" -> redirect ("http://www.yesodweb.com" :: Text)
        _ -> return ()
    respondSource "text/plain" $ do
        sendChunkText "You didn't go to yesodweb.com"
```

We check our query string parameter and perform the redirect before calling
`respondSource`. Once we know that we're returning a normal response, we then
use `respondSource` to create the body.

## Database

Making it easy to create streaming database responses was probably my original
motivation here. I was never happy with the [current recommended
approach](http://www.yesodweb.com/book/case-study-sphinx), so I'm happy to
offer something simpler. Basically, we follow the exact same approach as with
normal streaming responses, but use the `respondSourceDB` function instead of
`respondSource`. Take the following example, which just returns a list of
people from a database.

```
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Control.Monad.Logger    (runNoLoggingT)
import           Data.Conduit            (awaitForever, runResourceT, ($=))
import           Data.Text               (Text)
import           Database.Persist.Sqlite (ConnectionPool, SqlPersist,
                                          SqliteConf (..), runMigration,
                                          runSqlPool)
import           Database.Persist.Store  (createPoolConfig)
import           Yesod.Core
import           Yesod.Persist

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    name Text
|]

data App = App
    { appConfig :: SqliteConf
    , appPool   :: ConnectionPool
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB = defaultRunDB appConfig appPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appPool

getHomeR :: Handler TypedContent
getHomeR =
    respondSourceDB typePlain $ selectSource [] [Asc PersonName] $= awaitForever toBuilder
  where
    toBuilder (Entity _ (Person name)) = do
        sendChunkText name
        sendChunkText "\n"
        sendFlush

main :: IO ()
main = do
    let config = SqliteConf ":memory:" 1
    pool <- createPoolConfig config
    runNoLoggingT $ runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        deleteWhere ([] :: [Filter Person])
        insert_ $ Person "Charlie"
        insert_ $ Person "Alice"
        insert_ $ Person "Bob"
    warp 3000 App
        { appConfig = config
        , appPool = pool
        }
```

Obviously for our specific case, loading up the three names into memory would
be acceptable. But for more complicated responses, some form of streaming is
essential. This approach works very well in concert with the new streaming API
for yesod-sitemap, allowing us to create a streaming XML response body from a
database. The following is some real-life code from the [School of
Haskell](http://www.fpcomplete.com/):

```
getSitemapR :: Handler TypedContent
getSitemapR = do
    AppContent {..} <- getYesod >>= readIORef . appContent
    sitemap $ runDBSource $ do
        yield $ SitemapUrl HomeR Nothing (Just Daily) (Just 1.0)
        mapM_ (yield . goPage) $ unpack acPageMap
        mapM_ (yield . goPost) acPosts
        yield $ SitemapUrl UsersR Nothing (Just Daily) (Just 0.6)
        yield $ SitemapUrl RecentContentR Nothing (Just Daily) (Just 0.6)
        selectSource [] [] $= CL.mapMaybeM (\(Entity _ Profile {..}) -> do
            mus <- getBy $ UniqueUserSummary profileHandle
            case mus of
                Just (Entity _ us) | userSummaryTutcount us > 0 -> return $ Just $
                    SitemapUrl (UserR profileHandle) Nothing (Just Weekly) (Just 0.5)
                _ -> return Nothing
                )
        selectKeys [] [] $= CL.mapMaybeM (fmap (fmap goTutorial) . getCanonicalRoute)
  where
    goPage (pn, PageInfo {..}) = SitemapUrl (PageR pn) Nothing (Just Monthly) (Just 0.8)
    goPost Post {..} =
        SitemapUrl (BlogPostR y m s) (Just postDate) (Just Never) (Just 0.7)
      where
        PostKey y m s = postKey
    goTutorial route = SitemapUrl route Nothing (Just Monthly) (Just 0.6)
```

As the number of users and tutorials grows considerably, we want to avoid
loading all of that information into memory. The above code runs in constant
space, dealing with each individual user, and then each individual tutorial.
Under the surface, each `SitemapUrl` value is converted into a stream of
`xml-types`
[Event](http://haddocks.fpcomplete.com/fp/7.4.2/20130313-1/xml-types/Data-XML-Types.html#t:Event)s,
and `xml-conduit` converts that stream into a stream of `ByteString`s.

We have to be careful that our database queries are guaranteed to succeed. If
we use functions like `get404` inappropriately, we could generate incorrect
response bodies.

And yes, that means that the School of Haskell is currently running on Yesod 1.2.

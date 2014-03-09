I'm happy to announce a new 2.1 release of
[WAI](http://hackage.haskell.org/package/wai) and
[Warp](http://hackage.haskell.org/package/warp), version 0.3.1 of
[http-reverse-proxy](http://hackage.haskell.org/package/http-reverse-proxy),
and the release of a brand new package:
[yesod-websockets](http://hackage.haskell.org/package/yesod-websockets).  These
releases are all connected, so let's start off by describing the major addition
to WAI 2.1.

## Primary change: responseRaw

HTTP is built around a request/response pair, and WAI has until now followed
that strictly. The three response types allowed (builder, file, and source) all
worked around the idea of first consuming a request, then returning a response.
For normal HTTP requests, this is adequate. But there are two areas (that I'm
aware of at least) where this falls short:

* Implementing proxy servers, where the `CONNECT` request method requires
  upgrading to full-duplex communication.
* Using WebSockets, which again use full duplex communication.

To work around this limitation, Warp provides a `settingsIntercept`, which
allows a special handler to intercept some requests and take control away from
the normal WAI request/response pairs. I took this approach initially because
many WAI handlers have no means of properly supporting full duplex
communications (e.g., CGI). However, this split between normal and non-normal
handling makes it very awkward to actually use WebSockets.

So starting with WAI 2.1, we have a fourth response type: `responseRaw`. Its type is:

    responseRaw
        :: (C.Source IO B.ByteString -> C.Sink B.ByteString IO () -> IO ())
        -> Response
        -> Response

The first parameter is a "raw application:" it takes a `Source` of all data
coming from the client, a `Sink` for sending data to the client, and performs
an action with them. The second parameter is a backup response for WAI handlers
which do not support `responseRaw`; usually this will just be a message like
"Your server doesn't support WebSockets."

With this change the old `settingsIntercept` from Warp is no longer necessary,
and the behavior can be achieved from inside normal WAI applications. This has
resulted in a number of changes.

### wai-websockets 2.1

The wai-websockets package has been available for over two years now, thanks to
Jasper Van der Jeugt's websockets library. However, until now, it's provided a
slightly strange API to work with `settingsIntercept`:

    intercept :: ConnectionOptions -> ServerApp -> Request -> Maybe (Source IO ByteString -> Connection -> IO ())

`ConnectionOptions` and `ServerApp` are both part of the websockets library
itself. `Request` is a WAI request, and it returns a `Just` value if the
request represents a proper WebSockets request. Starting with version 2.1,
there's a much simpler API:

    websocketsApp :: ConnectionOptions -> ServerApp -> Request -> Maybe Response

Now, instead of getting back something to tie into Warp's intercept handler, we
get back a `Response`. If there was no WebSockets request, we get `Nothing`,
which allows us to write normal, non-WebSockets responses.

### yesod-websockets

This is also the first release of the yesod-websockets package. Previously,
there was no good story for how to tie WebSockets into an existing Yesod
application. Now, integration is trivial. Let's say we want to write a very
basic chat server. We can describe this as a WebSockets application with the
following:

```haskell
chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    App writeChan <- getYesod
    readChan <- atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan
    race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg ->
            atomically $ writeTChan writeChan $ name <> ": " <> msg))
```

`sendTextData` and `receiveData` are the core functions. There's also a
conduit-based API using `sourceWS` and `sinkWSText`, as well as some
convenience asynchronous helpers (`race` and `concurrently`). But more
interesting is the integration with the existing handler infrastructure:

```haskell
getHomeR :: Handler Html
getHomeR = do
    webSockets chatApp
    defaultLayout ...
```

The `webSockets` function takes a WebSockets application and tries to run it.
If the client sent a WebSockets request, then the app will be run, and no
further `Handler` actions will be taken. Otherwise, normal operations will
continue. (You can [see the full chat
example](https://github.com/yesodweb/yesod/blob/master/yesod-websockets/chat.hs)
in the Github repo.)

As this is a first release, things are still evolving, so it's not a good idea
to base your entire app off of this yet. But if you've been interested in
playing with WebSockets, now's a good time to get started. If you end up
working on anything, please let me know!

### http-reverse-proxy

This is actually the project that kicked off my endeavors into `responseRaw` in
the first place. I wanted to add support for reverse proxying WebSockets
requests, and initially [did so without WAI
support](https://github.com/fpco/http-reverse-proxy/commit/821537e98bf5eb0a8ba0e1b5ca684bb070a62de0)
using `settingsIntercept`. But the entire approach felt like a hack. Now with
`responseRaw` support, all users of `http-reverse-proxy` automatically get
WebSockets support. (This includes `yesod devel` and `keter`, by the way.)

I also put together an experimental forward proxy server a few weeks ago, and
[`responseRaw` made that nicer
too](https://gist.github.com/snoyberg/8779671/revisions).

## Additional change: settings

Kazu and I changed a few settings since Warp 2.0, which required a major
version bump. We didn't like that the 2.0 settings infrastructure required a
major version bump for minor tweaks to settings, so we've introduced a new
system for modifying Warp settings. You still start with the `defaultSettings`
value, but to modify, for example, the port, you use the new setter function:

    setPort 8080 defaultSettings

The old record-based accessors have been deprecated. In a future release, we'll
be moving them entirely to an internal module.

The concrete settings change was [providing the socket address to the `onOpen`
and `onClose` settings](https://github.com/yesodweb/wai/issues/220). This
allows you to write logging functions when connections are opened and closed,
and indicate where the connections come from. In addition, you can inspect the
`SockAddr` in `onOpen` and reject a connection immediately. Previously, this
needed to be done from the application itself, which meant that more processing
was performed before terminating the connection.
onOpen/onClose.

I'm happy to announce release 0.5 of [conduit](http://hackage.haskell.org/package/conduit). `conduit` is a package for dealing with streaming data, making it easy to compose different forms of data production, transformation, and consumption. It fits the same solution space as the enumerator/iteratee paradigm, though it takes a different approach, intended to be easier to understand and use. Unlike lazy I/O, it guarantees prompt resource finalization and does not introduce exceptions into pure code.

This release is notable since it provides a simple, efficient, high-level interface for creating Sources, Sinks, and Conduits. Direct usage of the constructors should almost never be necessary. There are also some more powerful features available just under the hood, such as upstream results and explicit leftover discarding. These changes also allow for more Category-like behavior.

The new package includes a fairly thorough tutorial in [the Haddocks themselves](http://www.snoyman.com/haddocks/conduit-0.5.0-final/Data-Conduit.html). I recommend going through that to start off with. Even if you've been using conduit for a few versions already, this tutorial makes explicit a few details that have never (to my knowledge) been completely clarified previously, such as exactly how finalization works.

One final note: this release only includes conduit and some accompanying packages like attoparsec-conduit and zlib-conduit. It does *not* include wai, persistent, yesod, and a few other conduit-based packages. Those will be released in time, when they are fully tested and ready for release. __Please do not file issues on Github asking for release of those packages__; we will do so when the Yesod team decides that they are ready for release.

For the remainder of this post, I'd like to step through some slightly more sophisticated examples of conduit usage, based on [a mailing list question](http://www.haskell.org/pipermail/haskell-cafe/2012-June/102008.html) about creating a network server.

---------------------------

The [network-conduit](http://hackage.haskell.org/package/network-conduit) provides some high level helpers for creating network servers and clients. We'll start off with the same basic shell, and provide a different application for each example. So we'll start with:

    import Data.Conduit
    import Data.Conduit.Network
    
    main :: IO ()
    main = runTCPServer (ServerSettings 4000 HostAny) app
    
    app :: Application IO
    app = ...

So what exactly is an Application? It's just a function that takes a source and a sink, and runs them. So let's start off with a really simple server: echo.

    app src sink = src $$ sink

All we need to do is connect our Source to our Sink, and conduit handles the rest. Any data received is automatically routed right back to the Sink. Notice that there's no explicit looping, no need to directly handle the intermediate data, and no need to explicitly deal with termination.

But that's boring. Let's say we want to automatically upper-case all of the input before echoing it back. Let's think about what steps need to happen to make that work. Firstly, we need to decode the data from binary to textual data. (We could cheat and just use some functions from `Data.ByteString.Char8`, but that's neither correct nor nearly as fun.) Then we need to upper case each bit of text, re-encode the data, and send it back out. With a few more imports, that's a piece of cake:

    import qualified Data.Conduit.List as CL
    import Data.Conduit.Text
    import Data.Text (toUpper)
    
    app src sink = src
                $$ decode utf8
                =$ CL.map toUpper
                =$ encode utf8
                =$ sink

Notice how declarative this approach is: each step we outlined becomes another component of our pipeline. Let's try something else: for each chunk we receive, we'll print out the size (in bytes) of the chunk.

    import qualified Data.ByteString.Char8 as S8
    app src sink = src
                $$ CL.map (\bs -> S8.pack $ show (S8.length bs) ++ "\n")
                =$ sink

At this point, we see a pattern developing: we seem to be keeping the src and sink on the outside and just playing around with the inside of the pipeline. Let's go ahead and abstract out that pattern:

    app src sink = src $$ conduit =$ sink

    conduit :: Conduit ByteString IO ByteString
    conduit = CL.map (\bs -> S8.pack $ show (S8.length bs) ++ "\n")

Or for our first example:

    conduit = decode utf8 =$= CL.map toUpper =$= encode utf8

So that brings up a question: why does `network-conduit` provide you with a Source and Sink? Can't an `Application` just be a `Conduit`? The answer is that providing a Source and Sink separately is strictly more powerful than just using a `Conduit`. As we'll see later, this can allow us to do some neat tricks with more advanced features like connect-and-resume.

## Control flow

So far, our examples have just been infinite pipelines: they keep processing in the same way until the client closes the connection. Let's introduce some control flow: a program that echos everything until it receives the word "quit" as the first four letters in a chunk.

    conduit = do
        mbs <- await
        case mbs of
            Nothing -> return ()
            Just bs
                | "quit" `S8.isPrefixOf` bs -> return ()
                | otherwise -> do
                    yield bs
                    conduit -- loop

Instead of an infinite loop, we now explicitly call out to `await` and `yield` to read and write data, respectively.

## Interleaving other I/O

Let's create a simple file server: you'll send it a filename, and it sends you back the entire contents of the file. To do this, we'll need to slightly modify our program: instead of living in the `IO` monad, it needs to live in the `ResourceT IO` monad, to allow for exception safe file access.

    import qualified Data.Conduit.Binary as CB
    
    main :: IO ()
    main = runResourceT $ runTCPServer (ServerSettings 4000 HostAny) app
    
    app :: Application (ResourceT IO)
    app src sink = src $$ conduit =$ sink
    
    conduit :: Conduit ByteString (ResourceT IO) ByteString
    conduit = CB.lines =$=
              awaitForever (CB.sourceFile . S8.unpack . S8.takeWhile (/= '\r'))

I purposely punted here on the issue of filename character encoding; normally I would use `system-filepath` and the `filepath-conduit` package, but for simplicity I'm just using the Char8 unpack function. Also, the `Data.Conduit.Binary.lines` function only strips the newline character (\n), not the carriage return (\r). Since most telnet clients will send both (CRLF), we should manually strip it out.

`awaitForever` is a nice convenience function that will call the inner function as long as there is more input available. Of course, we can combine our quit approach from above and have manual looping control:

    conduit =
        CB.lines =$= loop
      where
        loop = do
            mbs <- await
            case mbs of
                Nothing -> return ()
                Just bs
                    | "quit" `S8.isPrefixOf` bs -> return ()
                    | otherwise -> do
                        CB.sourceFile $ S8.unpack $ S8.takeWhile (/= '\r') bs
                        loop

## Client side

`network-conduit` provides a very similar interface for producing network clients. Let's see a simple example:

    {-# LANGUAGE OverloadedStrings #-}
    import Data.Conduit
    import qualified Data.Conduit.List as CL
    import Data.Conduit.Network
    import Data.ByteString.Char8 ()
    
    main :: IO ()
    main = runTCPClient (ClientSettings 4000 "localhost") client
    
    client :: Application IO
    client src sink =
        src $$ conduit =$ sink
      where
        conduit = do
            yield "hello"
            await >>= liftIO . print
    
            yield "world"
            await >>= liftIO . print
    
            yield "goodbye"
            await >>= liftIO . print

Nothing too surprising going on here. The main purpose of this section is to set the stage for the final example.

## Proxy server

One of the motivating use cases for conduit in the first place was creating HTTP proxy servers. Previously, with `enumerator`, most people (myself included) found it too difficult to combine different pieces together to get a working proxy server. (It can be done, using multiple levels of nested `Iteratee`s, but it's a pain.)

So let's go ahead and put together a simple network proxy server. It will work as follows:

* Client connects.
* Client sends server port number on a single line.
* Client sends server hostname on a single line.
* Proxy connects to server.
* Proxy sends a "Successful connection" response to client.
* Forever:<ul><li>Client sends chunk to proxy.</li><li>Proxy sends same chunk to server.</li><li>Server sends chunk to proxy.</li><li>Proxy sends that chunk to client.</li></ul>

Using standard socket-based (or `Handle`-based) functions, this isn't too difficult: you would just have a bunch of `send` and `recv` calls going against two different sockets. The point is that, since your application controls the flow of execution, you can easily interleave different sources. Conduit (and enumerator) introduce a certain inversion of control which makes such interleaving difficult.

So conduit provides an "escape route" to give control flow back to your application. This is called connect-and-resume. While this may sound a bit scary, it's actually not so bad: you connect a source to a sink until the sink is done. Then, instead of just getting back a result, you get a result *and* a new __resumable source__. You can then connect-and-resume that resumable source again... and so on.

First, let's look at our main function. We start by listening for a client connection:

    main = forkIO $ runTCPServer (ServerSettings 5000 HostAny) proxy

Within `proxy`, we need to get the port and hostname, and make a connection to the given server. Let's define some helper functions to get a single line, and to get the port/hostname pair:

    takeLine = do
        let linefeed = 10
        bss <- CB.takeWhile (/= linefeed) =$ CL.consume
        CB.drop 1 -- drop the newline
        return $ S8.takeWhile (/= '\r') $ S8.concat bss
    
    getPortHost = do
        portBS <- takeLine
        hostBS <- takeLine
        return $ ClientSettings (read $ S8.unpack portBS) (S8.unpack hostBS)

Next we'll define our `proxy` function using connect-and-resume (the $$+ operator). We'll connect our source to the `getPortHost` sink, and then get back the client settings and a new ResumableSource. We'll pass on that ResumableSource for our read loop (`proxyLoop`):

    proxy :: Application IO
    proxy fromClient0 toClient = do
        (fromClient, clientSettings) <- fromClient0 $$+ getPortHost
        runTCPClient clientSettings (proxyLoop fromClient toClient)

From `proxyLoop`, we need to send the successful connection message to the client, get a ResumableSource for reading from the server, and start looping:

    proxyLoop fromClient toClient fromServer0 toServer = do
        yield "Connected to server" $$ toClient
        -- convert fromServer0 from a normal Source to a ResumableSource
        (fromServer, ()) <- fromServer0 $$+ return ()
        loop fromClient fromServer
      where

The inner loop itself is pretty straight-forward: it follows the four steps from above directly:

        loop fromClient fromServer = do
            (fromClient', mbs) <- fromClient $$++ await
            case mbs of
                Nothing -> close fromClient' fromServer
                Just bs -> do
                    yield bs $$ toServer
                    (fromServer', mbs) <- fromServer $$++ await
                    case mbs of
                        Nothing -> do
                            yield "Server closed connection" $$ toClient
                            close fromClient' fromServer'
                        Just bs -> do
                            yield bs $$ toClient
                            loop fromClient' fromServer'

There are two tricks here. The first is the `$$++` operator. It's the same as the `$$+` connect-and-resume operator, but it works on an existing ResumableSource instead. You can think of it as "continue resuming." The second is those calls to `close`. When you use normal conduit connecting, the Source and Sink are both closed for you automatically. However, with ResumableSources, we need to leave the Source open to be used later. That means that when we're done with them, we need to explicitly close them. Doing so is easy: just use the connect-and-close (`$$+-`) operator:

        close x y = do
            x $$+- return ()
            y $$+- return ()

Connect-and-resume isn't something you'll often need in the world of conduits, but it's incredibly useful for the corner cases when you want it.

## Full source

Below is the full source for the server, proxy, and client, in <a href="https://gist.github.com/3010975">a single Gist for easy fork-ability</a>. I hope this tutorial helped demonstrate the power of conduit, and give a guide on how to use it. If there are any questions, or recommendations for how to clarify any points, please let me know!

<script src="https://gist.github.com/3010975.js?file=proxy.hs"></script>

By the way, [Felipe pointed out](https://gist.github.com/3010975#gistcomment-360243) that it would be nice to see `proxyLoop` implemented with threads to avoid deadlocks. I purposely chose the implementation of `proxyLoop` here to better demonstrate connect-and-resume, but for the curious, here's a threaded implementation:

    proxyLoop fromClient0 toClient fromServer0 toServer = do
        yield "Connected to server" $$ toClient
        m <- M.newEmptyMVar
        tid1 <- forkIO $ do
            fromServer0 $$ toClient
            M.putMVar m True
        tid2 <- forkIO $ do
            fromClient0 $$+- toServer
            M.putMVar m False
        x <- M.takeMVar m
        if x
            then killThread tid2
            else killThread tid1

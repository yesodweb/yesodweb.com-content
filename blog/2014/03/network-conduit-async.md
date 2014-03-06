I got a request to write up some examples of using
[network-conduit](http://hackage.haskell.org/package/network-conduit) for
server and client apps. I'm going to try to cover a few of the examples
requested. I'm also going to be demonstrating some usage of the new
[conduit-combinators](http://hackage.haskell.org/package/conduit-combinators)
library, as well as Simon Marlow's
[async](http://hackage.haskell.org/package/async) package. If you want to test
this out locally, start off by running:

    cabal install conduit-combinators network-conduit async word8

network-conduit provides some functions for writing network servers and clients
using conduit. There's support for TCP, UDP, and Unix sockets, and with
network-conduit-tls there's support for SSL/TLS connections. We'll just use
plain TCP in this post.

## Server 1: ALL CAPS

The first thing we'll do is write a server that listens on port 4000, and echos
back to the client whatever it transmitted, but upper-cased. The code is
incredibly short, let's just jump into it and then hit the explanations:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Data.Conduit.Network
import           Data.Word8           (toUpper)

main :: IO ()
main = runTCPServer (serverSettings 4000 "*") $ \appData ->
    appSource appData $$ omapCE toUpper =$ appSink appData
```

`runTCPServer` takes two parameters. The first is the server settings, which
indicates how to listen for incoming connections. Our two parameters say to
listen on port 4000 and that the server should answer on all network
interfaces. The second parameter is an `Application`, which takes some
`AppData` and runs some action. Importantly, our app data provides a `Source`
to read data from the client, and a `Sink` to write data to the client.
(There's also information available such as the `SockAddr` of the client.)

The next line is a very trivial conduit pipeline: we take all data from the
source, pump it through `omapCE toUpper`, and send it back to the client.
`omapCE` is our first taste of conduit-combinators: `omap` means we're doing a
monomorphic map (on a `ByteString`), and `C` means conduit, and `E` means "do
it to each element in the container."

Go ahead and run that code and telnet to port 4000. Everything you type in
should COME BACK LOOKING ANGRY.

## Server 2: doubled characters

Let's try another simple server (mostly because we need two servers to try out
a later example). Again, let's just jump in.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Data.ByteString      (pack)
import           Data.Conduit.Network

main :: IO ()
main = runTCPServer (serverSettings 4001 "*") $ \appData ->
    appSource appData
        $$ concatMapCE (\w -> pack [w, w])
        =$ appSink appData
```

The only changes here are (1) listen on port 4001 instead of 4000, and (2)
instead of upper casing, we want to duplicate each incoming byte. Again, we're
following the same naming scheme of `CE` to indicate "conduit, element-wise."

## Client: telnet

Now let's write a client. We want our client to perform the same job as our
telnet client: connect to the server we just wrote, pipe all input from stdin
to the server, and send all output to stdout:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Control.Concurrent.Async (concurrently)
import           Control.Monad            (void)
import           Data.Conduit.Network

main :: IO ()
main =
    runTCPClient (clientSettings 4000 "localhost") $ \server ->
        void $ concurrently
            (stdinC $$ appSink server)
            (appSource server $$ stdoutC)
```

Instead of `runTCPServer` and `serverSettings`, we're now using `runTCPClient`
and `clientSettings`. But we're still dealing with the same `Application` type,
and therefore the same ability to get access to our source and sink separately.

To get our standard input, we'll use `stdinC`, which is equivalent to
`sourceHandle stdin`. We want to connect that to `appSink server`. Similarly,
we need to connect `appSource server` to `stdoutC`. But the important bit is
doing this in two separate threads. Remember that it's entirely possible that a
server could generate output without corresponding input from our application.

To handle these semantics correctly, we pull in the async package, and in
particular, the `concurrently` function. This function forks each child action
into a separate thread, and blocks until both actions complete. If either
thread throws an exception, then the other is terminated and the exception is
rethrown. This provides exactly the behavior we need.

## Client: data pipeline

Now let's get a bit more complicated. We still want to get input from the user,
but now send the output from the first server to a second server, and then send
output from *that* server to standard output. This is actually not much worse
than what we had previously:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit.Network

main :: IO ()
main =
    runTCPClient (clientSettings 4000 "localhost") $ \server1 ->
    runTCPClient (clientSettings 4001 "localhost") $ \server2 ->
        runConcurrently $
            Concurrently (stdinC $$ appSink server1) *>
            Concurrently (appSource server1 $$ appSink server2) *>
            Concurrently (appSource server2 $$ stdoutC)
```

We now call `runTCPClient` twice, once for each server. And we need three
threads instead of two, since there are three different connections going on.
We *could* just make two calls to `concurrently`, but async provides a very
convenient newtype wrapper- `Concurrently`- which lets us use its applicative
instance to run all three of our threads at the same time.

## Server and client: proxy

Now our final example. We'd like to run a proxy server. It will accept incoming
connections, connect to a server, transmit all input from the client to the
server, and all output from the server to the client. (Bonus points: try to
implement this without looking at the example below.)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Control.Concurrent.Async (concurrently)
import           Control.Monad            (void)
import           Data.Conduit.Network

main :: IO ()
main =
    runTCPServer (serverSettings 4002 "*") $ \client ->
    runTCPClient (clientSettings 4000 "localhost") $ \server -> void $ concurrently
        (appSource server $$ appSink client)
        (appSource client $$ appSink server)
```

One important thing here is the order of the calls to `runTCPServer` and
`runTCPClient`. The way we've set it up, we first start listening for incoming
connections. Then, for each new incoming connection, we create a new connection
to the server. In other words, there will be as many connections to the server
as incoming client connections. This is the right way to implement a proxy,
though there are possibly other use cases where you'd want to share server
connections across multiple incoming clients.

Other than that bit, everything else should be familiar. You should be able to
connect to 4002 and talk to OUR ANGRY MAKING SERVER.

## Proxy with authentication

All of the examples so far have involved a single operation on our streams of
data: sending *all* of it to a server, upper casing every byte, etc. Let's look
at something a bit more complicated: an authenticating proxy. In this case, the
proxy will challenge the user for a username and password, the user will enter
them, and if they are valid, the proxy session will begin.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Control.Concurrent.Async (concurrently)
import           Control.Monad            (void)
import           Data.ByteString          (ByteString)
import           Data.Conduit.Network
import           Data.Word8               (_cr)

creds :: [(ByteString, ByteString)]
creds =
    [ ("spaceballs", "12345")
    ]

checkAuth :: Conduit ByteString IO ByteString
checkAuth = do
    yield "Username: "
    username <- lineAsciiC $ takeCE 80 =$= filterCE (/= _cr) =$= foldC
    yield "Password: "
    password <- lineAsciiC $ takeCE 80 =$= filterCE (/= _cr) =$= foldC
    if ((username, password) `elem` creds)
        then do
            yield "Successfully authenticated.\n"
        else do
            yield "Invalid username/password.\n"
            error "Invalid authentication, please log somewhere..."

main :: IO ()
main =
    runTCPServer (serverSettings 4003 "*") $ \client -> do
        (fromClient, ()) <- appSource client $$+ checkAuth =$ appSink client
        runTCPClient (clientSettings 4000 "localhost") $ \server ->
            void $ concurrently
                (appSource server $$ appSink client)
                (fromClient $$+- appSink server)
```

`creds` is just a simple collection of valid username/password combos.
`checkAuth` is where most of our magic happens. First notice its type
signature: it's a `Conduit` from `ByteString`s (client input) to `ByteString`s
(output to client). We could alternatively pass around the client `Sink`
explicitly, but this is more convenient. To say something to the client, we
simply `yield`.

We want to get a line of input data from the user. Let's look at the code more
closely:

    username <- lineAsciiC $ takeCE 80 =$= filterCE (/= _cr) =$= foldC

There are a few things we're doing here:

* The `lineAsciiC` combinator streams an entire line to the provided consumer. It ensures that, even if the consumer doesn't take all of the bytes, they will be flushed.
* To prevent a memory exhaustion attack, we only keep up to 80 bytes of input.
* `lineAsciiC` automatically strips out trailing line feeds, but does not strip out carriage returns. (Note: I might change that in a future release of conduit-combinators.) So we use `filterCE` to drop it.
* `foldC` consumes the incoming stream of `ByteString`s and folds them together into a single `ByteString`.

We use the same logic for getting the password, and then test if the
username/password is in `creds`. If it is, we give a successful message.
Otherwise, we give the user an error message and throw an exception to close
the connection.

The important change to `main` is the usage of connect-and-resume (the `$$+`
operator):

    (fromClient, ()) <- appSource client $$+ checkAuth =$ appSink client

This allows us to consume some of the input from the client, and then resume
the consumption later with a totally different consumer. This is highly
convenient: instead of needing to put our authentication logic into the same
consumer as the proxy, we can keep things nicely seperated. In order to resume
consumption, we need to use the `$$+-` operator:

    (fromClient $$+- appSink server)

* * *

That's all I've got for the moment. If there are points that are unclear,
please let me know. I'd like to make this blog post the official tutorial for
network-conduit.

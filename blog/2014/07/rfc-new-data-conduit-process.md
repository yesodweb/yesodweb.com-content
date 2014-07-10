I've been working on a new iteration of the Data.Conduit.Process API over the
past few days. The current API (provided by the process-conduit package), has
[some](https://github.com/tanakh/process-conduit/issues/14)
[issues](https://github.com/snoyberg/conduit/commit/bc41f1c86c429a624ffa6353bc329008f55550ea#commitcomment-6955336).
So I'm starting over with a new API, and will be including this in
conduit-extra's next release.

Before releasing, I'd like to get some feedback on the new API. I've put
together [a School of Haskell
tutorial](https://www.fpcomplete.com/user/snoyberg/library-documentation/data-conduit-process),
which will ultimately become the real documentation for this module. It
describes usage of the library, as well as why the library looks the way it
does. You can [view the
source](https://github.com/snoyberg/conduit/blob/process/conduit-extra/Data/Conduit/Process.hs)
on the process branch of conduit.

In case anyone's wondering, the "well known bug" in `waitForProcess` may
actually not be very well known *yet*. There's a race condition documented in
the source code, but it's not nearly as narrow a window as implied there. For
example, the following code will reliably throw an exception:

```haskell
import System.Process
import Control.Concurrent.Async

main :: IO ()
main = do
    (_, _, _, ph) <- createProcess $ shell "sleep 1"
    let helper i = do
            ec <- waitForProcess ph
            print (i :: Int, ec)
    ((), ()) <- concurrently (helper 1) (helper 2)
    return ()
```

Thus the motivation for fixing the problem in Data.Conduit.Process. Thanks go
to Michael Sloan for discovering the severity of this race condition. In fact,
the bug he ran into, combined with a separate process-conduit bug I ran up
against, were the impetus for me getting this library written now.

For the lazy, here's a copy of the content from School of Haskell:

* * *

__NOTE__: This tutorial documents a not-yet-released version of conduit-extra's Data.Conduit.Process module. Currently, that module name is provided by process-conduit, which provides a completely different API. This tutorial is present now for early feedback. If you'd like to experiment, this code is available [on the process branch of the conduit repo](https://github.com/snoyberg/conduit/tree/process).

## Introduction

Whenever you run an external process, there are four ways to interact with it post-creation:

* Write to its standard input
* Read from its standard output
* Read from its standard error
* Check its exit code

The standard `System.Process` module provides means for all of these interactions. However, there are some downsides with using them:

* Many of the function in System.Process rely on lazy I/O.
* There is a subtle race condition when checking for exit codes.
* Dealing with `Handle`s directly is relatively low-level.

Data.Conduit.Process provides a higher-level interface for these four interactions, based on conduit. It additionally leverages type classes to provide more static type safety than dealing directly with System.Process, as will be described below. The library is also designed to work with the wonderful async library, providing for easy, high-quality concurrency.

Note that providing general parameters for creating a process, such as its working directory or modified environment variables, are not addressed by this module; you should instead use the standard facilities from System.Process.

## Synopsis

```haskell active
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (await, yield, ($$), (=$))
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..), conduitProcess,
                                           proc, waitForConduitProcess)
import           System.IO                (stdin)

main :: IO ()
main = do
    putStrLn "Enter lines of data. I'll base64-encode it."
    putStrLn "Enter \"quit\" to exit."

    ((toProcess, close), fromProcess, ClosedStream, cph) <-
        conduitProcess (proc "base64" [])

    let input = CB.sourceHandle stdin
             $$ CB.lines
             =$ inputLoop
             =$ toProcess

        inputLoop = do
            mbs <- await
            case mbs of
                Nothing -> close
                Just "quit" -> close
                Just bs -> do
                    yield bs
                    inputLoop

        output = fromProcess $$ CL.mapM_
            (\bs -> putStrLn $ "from process: " ++ show bs)

    ec <- runConcurrently $
        Concurrently input *>
        Concurrently output *>
        Concurrently (waitForConduitProcess cph)

    putStrLn $ "Process exit code: " ++ show ec
```

## Exit codes

There's a well documented corner case in `waitForProcess` whereby multiple calls can end up in a race condition, and therefore a deadlock. Data.Conduit.Process works around this issue by not providing direct access to a `ProcessHandle`. Instead, it wraps this with a `ConduitProcessHandle`, which uses an STM TMVar under the surface. This allows you to either poll to check if a process has exited, or block and wait for the process to exit. As a minimal example (ignore the streaming bits for now, they'll be explained shortly).

```haskell active
import Data.Conduit.Process

main :: IO ()
main = do
    (Inherited, Inherited, Inherited, cph) <-
        conduitProcess (shell "sleep 2")

    -- non-blocking
    getConduitProcessExitCode cph >>= print

    -- blocking
    waitForConduitProcess cph >>= print
```

If you need direct access to the ProcessHandle (e.g., to terminate a process), you can use `conduitProcessHandleRaw`.

## Streaming

Now to the main event: streaming data. There are multiple ways you can interact with streams with an external process:

* Let the child process inherit the stream from the parent process
* Provide a pre-existing `Handle`.
* Create a new `Handle` to allow more control of the interaction.

One downside of the System.Process API is that there is no static type safety to ensure that the `std_out` parameter in fact matches up with the value produced by `createProcess` for the standard output handle. To overcome this, Data.Conduit.Process makes use of type classes to represent the different ways to create a stream. This isn't entirely intuitive from the Haddocks, but once you see the concept used, it's easy to use yourself.

### Inherited and ClosedStream

Let's start with an example of using the simplest instances of our typeclasses. `Inherited` says to inherit the `Handle` from the parent process, while `ClosedStream` says to close the stream to the child process. For example, the next snippet will inherit stdin and stdout from the parent process and close standard error.

```haskell active
import Data.Conduit.Process

main :: IO ()
main = do
    putStrLn "Just wrapping cat. Use Ctrl-D to exit."

    (Inherited, Inherited, ClosedStream, cph) <-
        conduitProcess (shell "cat")

    waitForConduitProcess cph >>= print
```

Note that there's no way to send an EOF in School of Haskell, so the above active code will never terminate.

### Conduit

It would be pretty strange to have a library in conduit-extra that didn't provide some conduit capabilities. You can additionally get a `Sink` to be used to feed data into the process via standard input, and `Source`s for consuming standard output and error.

This next example reads standard input from the console, process standard output with a conduit, and closes standard error.

```haskell
import           Data.Conduit         (($$))
import qualified Data.Conduit.List    as CL
import           Data.Conduit.Process

main :: IO ()
main = do
    putStrLn "Just wrapping cat. Use Ctrl-D to exit."

    (Inherited, src, ClosedStream, cph) <-
        conduitProcess (shell "cat")

    src $$ CL.mapM_ print

    waitForConduitProcess cph >>= print
```

Note that these `Source`s and `Sink`s will *never* close their `Handle`s. This is done on purpose, to allow them to be used multiple times without accidentally closing their streams. In many cases, you'll need to close the streams manually, which brings us to our next section.

### Conduit + close

Let's say we'd like to close our input stream whenever the user types in "quit". To do that, we need to get an action to close the standard input `Handle`. This is simple: instead of just returning a `Source` or `Sink`, we ask for a tuple of a `Source`/`Sink` together with an `IO ()` action to close the handle.

```haskell active
{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString      (ByteString)
import           Data.Conduit         (Source, await, yield, ($$), ($=))
import qualified Data.Conduit.Binary  as CB
import           Data.Conduit.Process
import           System.IO            (stdin)

userInput :: Source IO ByteString
userInput =
       CB.sourceHandle stdin
    $= CB.lines
    $= loop
  where
    loop = do
        mbs <- await
        case mbs of
            Nothing -> return ()
            Just "quit" -> return ()
            Just bs -> do
                yield bs
                yield "\n"
                loop

main :: IO ()
main = do
    putStrLn "Just wrapping cat. Type \"quit\" to exit."

    ((sink, close), Inherited, ClosedStream, cph) <-
        conduitProcess (shell "cat")

    userInput $$ sink
    close

    waitForConduitProcess cph >>= print
```

### UseProvidedHandle

Let's take a quick detour from our running example to talk about the last special type: `UseProvidedHandle`. This says to `conduitProcess`: use the example value of `UseHandle` provided in `std_in`/`std_out`/`std_err`. We can use this to redirect output directly to a file:

```haskell active
import Data.Conduit.Process
import System.IO (withFile, IOMode (..))

main :: IO ()
main = do
    let fp = "date.txt"
    withFile fp WriteMode $ \h -> do
        (ClosedStream, UseProvidedHandle, ClosedStream, cph) <-
            conduitProcess (shell "date")
                { std_out = UseHandle h
                }
        waitForConduitProcess cph >>= print
    readFile fp >>= print
```

## Use with async

In our examples above, we only ever used a single `Source` or `Sink` at a time. There's a good reason for this: we can easily run into deadlocks if we don't properly handle concurrency. There are multiple ways to do this, but I'm going to strongly recommend using the async package, which handles many corner cases automatically. In particular, the `Concurrently` data type and its `Applicative` instance make it easy and safe to handle multiple streams at once.

Instead of duplicating it here, I'll ask the reader to please refer back to the synopsis example, which ties this all together with two threads for handling streams, and another thread which blocks waiting for the process to exit. That style of concurrency is very idiomatic usage of this library.

I'm happy to announce the 0.3.0 release of
[conduit](http://hackage.haskell.org/package/conduit). As many readers are
aware, conduit is a library to address the issue of streaming data in constant
space. This release does not come alone; a number of other packages have been
released to support this updated version. See the end of this post for a full
list.

There have been a number of improvements to the library. Quoting the changelog:

> ResourceT has been greatly simplified, specialized for IO, and moved into a
> separate package. Instead of hard-coding ResourceT into the conduit
> datatypes, they can now live around any monad. The Conduit datatype has been
> enhanced to better allow generation of streaming output. The SourceResult,
> SinkResult, and ConduitResult datatypes have been removed entirely.

For users of the high-level API, nothing has changed. In other words, the
following line is still completely valid:

    runResourceT $ sourceFile input $$ sinkFile output

Mid-level API users (conduitState, sourceIO, etc) should also avoid any changes
to their code. Only users dealing directly with the low-level API should need
to change their code. We'll cover the major changes, and some of their
motivation, in the next few sections.

__Note__: The [chapter in the Yesod book](http://www.yesodweb.com/book/conduits) on conduits still covers version
0.2. Eventually, I will bring the content up-to-date. The concepts have stayed
completely the same through all versions, and therefore the chapter should
still be mostly relevant. If you're just starting with conduit, I recommend
reading the chapter and then coming back here. Eventually I'll merge the two
together.

## Simplified, separated ResourceT

As we [discussed previously](http://www.yesodweb.com/blog/2012/02/simplifying-resourcet),
`ResourceT` has been simplified, targeting just the `IO` monad. It has also
been released as a separate package, `resourcet`.

There have been a few minor changes: `with` and `withIO` are now replaced by
`allocate`. There are now a number of typeclasses available.

* `MonadResource` is any monad stack with a `ResourceT` in it.
* `MonadUnsafeIO` is a stack with either `IO` or `ST` as a base.
* `MonadThrow` is a monad that can throw `Exception`s.
* `MonadActive` is specifically added for `ResourceT` usage. It tracks whether or not the state of current monad is still active. This is vital for properly implementing lazy I/O for conduits. For non-`ResourceT` monad stacks, `MonadActive` indicates that the monad is always active.

## Less reliant on ResourceT

A `ResourceT` is used for safely allocating resources. But if all I'm doing is
printing the numbers 1 to 10, e.g.:

    sourceList [1..10] $$ Data.Conduit.List.mapM_ print

who needs it? As a result, the `ResourceT` transformer is no longer baked into
the `Source`, `Conduit`, and `Sink` types. Instead, functions that need to
allocate resources (e.g., `sinkFile`) should place a `MonadResource` constraint
on their inner monad.

## Improved Conduit type

Previously, the `Conduit` type could return a list of return values every time
it was pushed to. This, however, is inadequate. If you have a `Conduit` That
can produce large amounts of output for a single input (e.g., a decompressor),
you have to allocate it all in memory.

A `Conduit` has been improved in two ways:

* After being pushed new input, it can return multiple outputs separated by monadic actions, instead of returning a single list.
* When a `Conduit` is closed, it returns a `Source`. If you want to consume the rest of its output, you can do so. And if you don't care, and just want to ignore it, you can close the `Source` and not spend any more cycles on it.

You'll see below that there is a new, updated version of `zlib-bindings`
available as well. This release does away with the previous callback-based API,
and makes it possible to implement a decompressor in `zlib-conduit` in a fully
constant-memory manner.

## No more result types

Originally, `conduit` had three types for sinks: `Sink`, `PreparedSink`, and
`SinkResult`. We did away with the `Sink`/`PreparedSink` distinction in conduit
0.2, and in the process greatly simplified the library and improved
performance. Now we're unifying `Sink` and `SinkResult`, with the exact same
benefits. (And yes, the same applies to `Source` and `Conduit`.)

In this process, I've come up with a guiding principle of sorts for the design
of conduit. It comes down to: only ever do one thing at a time. As a concrete
example, consider pushing to a `Sink` in conduit 0.2. We have the type (greatly
simplified):

    data Sink input m output = Sink (input -> m (SinkResult input m output))

Seems fairly straight-forward, right? But imagine that we have a pure sink,
which never performs any monadic actions (e.g., `fold`). We've now tied
together the concept of pushing new data, and that of performing a monadic
action. While this may seem benign, it has two important ramifications:

* It can drastically slow down code. Consider [417us versus 88us](http://www.yesodweb.com/blog/2012/03/more-pure-conduit).
* Taking the opposite approach (having an explicit constructor for monadic actions) allows us to unify the datatypes.

For the second point, consider `Source` and `sourceFile`. `sourceFile` cannot
return any data until it has performed an `IO` action. But the `SourceResult`
type in conduit 0.2 requires that either data is available immediately (the
`Open` constructor), or that the `Source` indicate that it is closed
(`Closed`). That's why we needed an extra type `Source`, which had a record
`m SinkResult` for pulling from the `Source`.

However, if we add a third constructor for performing monadic actions to our `SourceResult` type, we don't actually need the `Source` type any more. The result looks like:

    data Source m a =
        Open (Source m a) (m ()) a
      | Closed
      | SourceM (m (Source m a)) (m ())

`Open` provides more data, tells you the next `Source` in the stream, and
provides an action to close the `Source` early. `Closed` is pretty boring.
`SourceM` now allows you to perform an action to get the next `Source`, or
perform another action to close early.

Here's a slightly long-winded example which should hopefully demonstrate the
point. In real life code, we would just use `sourceIO`, but hopefully this
makes it clear how to pass control back and forth between the `Open` and
`SourceM` constructors.

    import Data.Conduit
    import qualified Data.Conduit.List as CL
    import System.IO
    import Control.Monad.Trans.Resource
    import Control.Monad.IO.Class (liftIO)
    
    sourceFile :: MonadResource m => FilePath -> Source m Char
    sourceFile fp =
        -- Need to start off with a monadic action
        SourceM initPull initClose
      where
        initClose = return () -- haven't opened anything, nothing to close
    
        initPull :: MonadResource m => m (Source m Char)
        initPull = do
            -- Open the file handle, and register a release action
            (releaseKey, handle) <- allocate (openFile fp ReadMode) hClose
            -- pass off to the pull function, that does the real work
            pull handle releaseKey
    
        pull :: MonadResource m => Handle -> ReleaseKey -> m (Source m Char)
        pull handle releaseKey = do
            eof <- liftIO $ hIsEOF handle
            if eof
                then do
                    -- file exhausted, close the handle
                    release releaseKey
                    return Closed
                else do
                    -- more data, get a character
                    c <- liftIO $ hGetChar handle
                    return $ Open
                        -- The next Source to use, which needs to perform another
                        -- monadic action
                        (sourceM handle releaseKey)
                        -- Early close
                        (release releaseKey)
                        -- The newly pulled data
                        c
    
        sourceM :: MonadResource m => Handle -> ReleaseKey -> Source m Char
        sourceM handle releaseKey = SourceM
            (pull handle releaseKey)
            (release releaseKey)
    
    main :: IO ()
    main = do
        str <- runResourceT $ sourceFile "test.hs" $$ CL.consume
        putStrLn str

Overall, this change probably complicates the writing of low-level code a bit.
However, the simplicity of implementation for the connect and fuse operators,
plus the overall efficiency improvements, reinforce my belief that this was the
right change to make.

## Updated packages

You'll notice that, missing from this list, are any of the WAI, Persistent, or
Yesod packages. We are purposely holding off on releasing WAI and Persistent
code- even though it's ready- to help avoid confusion for Yesod users. The
upcoming Yesod 1.0 release will depend on conduit 0.3, and will hopefully be
out in the next few weeks. Distribution maintainers: please do *not* begin the
upgrade cycle on conduit 0.3 until Yesod 1.0 is released.

* attoparsec-conduit-0.3.0
* authenticate-1.1.0
* blaze-builder-conduit-0.3.0
* conduit-0.3.0
* crypto-conduit-0.2.0
* filesystem-conduit-0.3.0
* http-conduit-1.3.0
* imagesize-conduit-0.3.0
* network-conduit-0.3.0
* resourcet-0.3.0
* uri-conduit-0.3.0
* xml2html-0.1.1
* xml-catalog-0.6.0
* xml-conduit-0.6.0
* yaml-0.6.0
* zlib-bindings-0.1.0
* zlib-conduit-0.3.0

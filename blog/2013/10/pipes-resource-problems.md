I promised a blog post elaborating on my concerns with the pipes 4.0 release.
What you're reading now is not that blog post; this is an introduction to it.
Right now, I'm trying to motivate the fact that there's a serious problem. I've
been having a private conversation with Gabriel about this specific concern,
and I don't believe my concerns have made much of an impact. So I'm raising
them here. A quick summary is:

* pipes has known cases in which it will not clean up resources promptly.
* Worse yet, the cleanup behavior is in many cases unreliable, in non-obvious ways.
* The problem can easily lead to programs crashing.

## The crashing program

Let me jump right in to a concrete example of that third point, which is IMO
the most troubling. Let's create a directory with lots of files:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad             (forM_)
import           Filesystem                (createTree)
import           Filesystem.Path.CurrentOS (decodeString, directory,
                                            encodeString, (</>))

main = do
    let files = do
            let pieces = map (decodeString . show) [1..20 :: Int]
            p1 <- pieces
            p2 <- pieces
            p3 <- pieces
            return $ "input" </> p1 </> p2 </> p3
    forM_ (zip files [1 :: Int ..]) $ \(fp, num) -> do
        createTree $ directory fp
        writeFile (encodeString fp) (show num)
```

I'd like to write a program to create a clone of this "input" directory, into a
directory called "output". Using filesystem-conduit (built on the wonderful
system-filepath and system-fileio packages), this is pretty easy:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class
import           Data.Conduit
import           Data.Conduit.Filesystem
import           Filesystem                (createTree)
import           Filesystem.Path.CurrentOS

main = runResourceT $ traverse False "input" $$ awaitForever (\infile -> do
    Just suffix <- return $ stripPrefix "input/" infile
    let outfile = "output" </> suffix
    liftIO $ createTree $ directory outfile
    sourceFile infile =$ sinkFile outfile
    )
```

`traverse` creates a stream of all of the files found in the input path,
traversing subdirectories. For each file, we create the output filename, create
the directing for the output file, and then connect `sourceFile` to `sinkFile`
to perform the actual copy. Each of these functions guarantees prompt cleanup
of the file handles they hold, and the enclosing `runResourceT` ensures that
resources will be cleaned up, even in the event of an exception.

Let's translate this code to use pipes-safe, which claims to support prompt
finalization of resources. (Note that I've included an implementation of
`traverse` here, based on the conduit implementation.)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Control.Monad.IO.Class
import           Filesystem                (createTree, isDirectory, isFile,
                                            listDirectory)
import           Filesystem.Path.CurrentOS
import           Pipes
import qualified Pipes.Prelude             as P
import           Pipes.Safe
import           Pipes.Safe.Prelude
import           Prelude                   hiding (FilePath, readFile,
                                            writeFile)

main = runSafeT $ runEffect $ traverse "input" >-> forever (await >>= \infile -> do
    Just suffix <- return $ stripPrefix "input/" infile
    let outfile = "output" </> suffix
    liftIO $ createTree $ directory outfile
    readFile (encodeString infile) >-> writeFile (encodeString outfile)
    )

traverse :: MonadIO m => FilePath -> Producer FilePath m ()
traverse root =
    liftIO (listDirectory root) >>= pull
  where
    pull [] = return ()
    pull (p:ps) = do
        isFile' <- liftIO $ isFile p
        if isFile'
            then yield p >> pull ps
            else do
                follow' <- liftIO $ isDirectory p
                if follow'
                    then do
                        ps' <- liftIO $ listDirectory p
                        pull ps
                        pull ps'
                    else pull ps
```

Go ahead and run that program. You should get output that looks something like:

> copy-pipes.hs: input/13/1/12: openFile: resource exhausted (Too many open
> files)

The exact file is crashes on may be different, and if you've increased your
ulimits, the program may succeed. But the core problem is that pipes provides
no means of guaranteeing that a resource is cleaned up.

## Why this is a problem

For many of us, the whole point of a streaming data library is to provide for
deterministic resource handling. While not the only issue with lazy I/O, its
non-determinism was high on the list. The issue is that, based on various
environmental factors, cleanup of resources could be delayed until the next
garbage collection, whose timing cannot be guaranteed.

There are three different aspects to resource handling in a streaming library:

* On demand acquisition (a.k.a., laziness).
* Prompt finalization.
* Exception safety.

One of the beauties of the iteratee pattern is that it allows for all three of
these to be addressed on the data producer side. However, exception safety
cannot be guaranteed on the data consumer side. When I started work on conduit,
I wanted to ensure that both the producer and consumer could reliably allocate
resources on demand in an exception safe manner. This pattern is allowed via
the resourcet package. conduit itself then provides on demand acquisition and
prompt finalization.

pipes-safe includes a SafeT transformer which is almost identical to ResourceT.
And this transformer guarantees that, no matter what (*for some definitions of
no matter what*), a cleanup action is called. However, just like ResourceT, it
can give no guarantees about promptness. I'll get into the details of *why* in
my next blog post, but pipes is unable to guarantee that it will run code at a
specific point.

Let's look at one of the examples from the pipes-safe docs:

    runSafeT $ runEffect $ readFile "readFile.hs" >-> P.take 4 >-> P.stdoutLn

Running this will in fact promptly close readFile.hs. But that's just due to
the small nature of this example. What actually happens is that `readFile`
opens the file, after reading four lines, the pipeline terminates, and then
`runSafeT` closes the file. This reliance on `SafeT` to do normal resource
finalization is the problem. The first example I gave demonstrates that in
simple real-world examples, there may be many operations between resource
acquisition and exiting `SafeT`.

It's true that the example I started off with could be rewritten to embed the
`SafeT` block inside, and run two separate pipelines instead of one. That's
certainly true, but that's only because of the simplicity of the example. A
more difficult example to work around would be one where the data source needs
to be shared amongst the consumers, and therefore you can't get away with
running multiple pipelines. The following example in conduit opens an input
file and splits it into 50 byte chunks:

```haskell
import Data.Conduit
import Data.Conduit.List (peek)
import Data.Conduit.Binary

main =
    runResourceT $ sourceFile "input.dat" $$ loop 0
  where
    loop i = do
        mx <- peek
        case mx of
            Nothing -> return ()
            Just _ -> do
                let fp = "out-conduit/" ++ show i
                isolate 50 =$ sinkFile fp
                loop $ i + 1
```

Given a large enough input file (I used /usr/share/dict/words), the following
pipes version will crash:

```haskell
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Safe
import qualified Pipes.Safe.Prelude as P

main =
    runSafeT $ runEffect $ P.readFile "input.dat" >-> loop 0
  where
    loop i = do
        let fp = "out-pipes/" ++ show i
        P.take 50 >-> P.writeFile fp
        loop $ i + 1
```

Note that, due to differences in `sourceFile` in conduit and `readFile` in
pipes-safe, these programs are doing slightly different things: conduit deals
with 50 byte chunks, while pipes is dealing with 50 *line* chunks. This
distinction is irrelevant for the current discussion, I'm just trying to keep
the examples concise by using functions built into the libraries.

## It's unreliable

One last point is that this behavior is unreliable. Consider this example again:

    runSafeT $ runEffect $ readFile "readFile.hs" >-> P.take 4 >-> P.stdoutLn

Above, I claimed that `readFile` would not end up closing the file handle. This
wasn't strictly accurate. *If* the file contains less than four lines,
`readFile` will close the file handle. This isn't quite non-deterministic
behavior, since we can clearly state how the program will behave on different
inputs. However, it's pretty close: depending on the size of a file, the
program will either do the right or the wrong thing, and we have no way to
restructure our program to change this.

Given the fact that, for many of us, the entire attraction of getting away from
lazy I/O is to take back deterministic, prompt resource management, this
behavior is unsettling. What concerns me even more is that the pipes libraries
claim to support proper behavior, but on basic analysis clearly don't.

In our conversations, Gabriel told me that pipes is about much more than just a
lazy I/O replacement. I have no objection to that, and pipes can and should
continue to research those directions. But in its current form, pipes is not
performing the bare minimum functionality to be considered a replacement for
iteratees or conduit.

Again, my point in this blog post is simply to establish the fact that there's
a problem. I'll get into more details about the cause in the following blog
post, and a solution to that problem in the one after that.

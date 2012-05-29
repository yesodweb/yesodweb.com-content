In my previous blog post, I mentioned that I had more conduit changes planned. This blog post is intended to discuss those changes. You can see the progress in the [conduit05 branch on Github](https://github.com/snoyberg/conduit/tree/conduit05).

This thought process got kicked off by a [question from Hiromi Ishii](http://www.haskell.org/pipermail/haskell-cafe/2012-May/101334.html) about usage of `yield` and `await`. But as long as we're playing around with the library, I think it's a good time to get out a few other ideas.

As a tl;dr: these changes should make it easier to write conduit code without touching library internals and statically guarantee a few minor invariants. Very little code in the wild should break with these changes, and fixing that code should be very mechanical. And since conduit 0.4 is already stable and working, I see no need to rush this change out, so it would be great to have a longer discussion period.

## Step 1: Housecleaning

There are a few simple changes I've wanted to make for a bit, and have been discussing offline with some people (Mathias Svensson in particular).

* We discussed the idea of replacing usage of `Void` with `forall`s, which would in theory make functions with type `Sink` automatically generalize to any `Pipe`. In turn, this would make it much easier to combine `Sink`s into `Conduit`s. After playing with this a bit, I think that the type signatures and language extensions necessary are too difficult to work with. Instead, we'll generalize the types of the functions exported by `conduit`, so that- for example- <code>mapM_</code> is now `Pipe a o m ()` instead of `Sink a m ()`.
* The second field in the `NeedInput` constructor is intended as the "no more input" field. Initially, its input type was `Void`, which prevented users from providing it input after finalization began. For simplicity of usage, this was changed just before the 0.4 release to `i`, which weakens static guarantees. In retrospect, that was a mistake, and we'll move back towards `Void`.
* This isn't an API change, but Mathias and I discussed ways to make the codebase easier to follow. This comes down to some basic refactorings and standardized namings. That will probably be part of this upgrade as well (some of it has already started).

## Step 2: Leftovers

Just before the 0.4 release, I [raised a question in a blog post](http://www.yesodweb.com/blog/2012/03/summarizing-conduit-questions) about having a separate `Leftover` constructor. To quote myself:

> There's a bit of an inconsistency, in that the Done constructor performs two actions: it returns a result, and gives back any leftover input. Also confusing is that we can only have 0 or 1 leftover values.

I felt more comfortable at the time sticking the leftover values in the `Done` constructor, as that's pretty close to how `enumerator` works, and after all, `enumerator` is a great library and does lots of things really well. But the question's been nagging at me for a while since then. So I think it's worth taking the plunge and moving over to a separate `Leftover` constructor. This comes down to two simple changes:

* Add a constructor `Leftover (Pipe i o m r) i`
* Change the `Done` constructor from `Done (Maybe i) r` to `Done r`.

This actually simplifies a lot of the code, but I already knew that from previous experiments. There are really two things pushing me over the edge this time:

* I've had time to actually analyze this in depth, and I believe that this does not lead to any data loss issues.
* It lets us get rid of one of our invariants (to some extent): you can now return leftovers without consuming input. (Yes, that solves Gabriel's complaint, I even used his code as the test case.) I still strongly recommend *not* doing this: it's been well known in the enumerator world for a while now that doing so is a bad idea.

I'm very much interested in opinions on this, but it seems like the right move to me.

## Step 3: Better `await`/`yield`

I apologize for making this the end of the post, after I teased you all with promises of a solution to Hiromi's problem. Let me start by framing the issue. The following code, though it seems intuitively correct, enters an endless loop:

    sourceTChanYield :: MonadIO m => TChan a -> Source m a
    sourceTChanYield ch = forever $ do
        ans <- liftIO . atomically $ readTChan ch
        yield ans

The reason for this is that you've provided no escape route for the code. Actually, due to the usage of `forever` here, some people might be saying, "Well, *obviously* it loops forever, what would you expect?" The answer is that the `await`/`yield` approach comes with a loaded implication from the `pipes` world, which has a subtle yet very important distinction from `conduit`:

__Just because one part of the pipeline goes down, does not mean the rest of the pipeline goes down.__

I'm beginning to think this is the core distinction between these two packages, and actually has led to a very deep difference in understanding when discussing this topic. I don't want to get into that now, since it's not really our focus, but would like to continue that discussion another time. As you'll see in the rest of this post, I think both approaches have there place. (And I think `pipes` 2.0 has come to that same conclusion with its `Frame` concept.)

So let's start with the question: why *shouldn't* taking down one piece of the pipeline take down everything else? A simple- and incomplete- answer is that it wouldn't give us a chance to perform resource finalization. More generally, it won't allow us to perform *any* kind of operations after one piece of the pipeline completes. Imagine trying to implement `consume` where, if you check if there's anything left in the stream, your code stops running.

But this behavior is exactly what we're looking for in `sourceTChanYield` above: I want to automatically stop running as soon as no one wants any more output from me. I thought about how to implement this over the weekend, and came up with two changes to the `Pipe` datatype:

* Since we can't perform any resource finalization, remove all finalization concepts from the constructors.
* Instead of providing a second field for "no more input" in the `NeedInput` constructor, have the pipeline shut down if no input is available. A side effect of this is that `await` would now have the type `Pipe i o m i` instead of `Pipe i o m (Maybe i)`.

I thought that these changes looked very familiar. Once I got back home this week, I went ahead and had a look at [Pipe type from pipes 1.0](http://hackage.haskell.org/packages/archive/pipes/1.0.2/doc/html/Control-Pipe-Common.html#t:Pipe), and sure enough, it's [isomorphic to what I came up with](https://github.com/snoyberg/conduit/blob/conduit05/conduit/Data/Conduit/Internal.hs#L433). Now we obviously can't just drop `conduit`'s type for `pipes` 1.0's solution: the latter won't support many of our needs, including resource finalization and connect-and-resume. However, it *is* a very convenient approach for implementing a number of functions, including `sourceTChanYield`.

So here's the idea: we'll provide `SPipe` as a simplified version of `Pipe`, which shuts down automatically and doesn't deal with resource allocation. `yield` and `await` will work in `SPipe` instead of `Pipe`, and the function `toPipe` will convert an `SPipe` to a `Pipe`. With all that in place, `sourceTChanYield` can now be implemented as:

    sourceTChanYield :: MonadIO m => TChan a -> Source m a
    sourceTChanYield ch = toPipe $ forever $ do
        ans <- liftIO . atomically $ readTChan ch
        yield ans

And we'll provide `await'` and `yield'` as well, which will be the same as the current non-apostrophed functions. This should mean a simple upgrade process, but with the compiler letting you know that you can switch to a simpler abstraction if you want.

## Step 3a: Adding in finalization

That's all well and good, and will hopefully solve Hiromi's issue. But it's still not a completely general solution, meaning we'll still need to go back to the original constructors and write everything out the long way in order to implement something like `sourceFile`. Or do we?

We can really break down `sourceFile` into three steps:

* Open a file handle, and register a cleanup function to close the handle in case of exceptions. (This is where `resourcet` comes into play.)
* Loop over the contents of the file.
* Close the file handle explicitly as soon as the "inner loop" completes.

This actually sounds a lot like `bracket`, doesn't it? So I present to you the horribly named (bikeshedding welcome) `bracketSPipe`:

    bracketSPipe :: MonadResource m
                 => IO a                   -- ^ allocation
                 -> (a -> IO ())           -- ^ release
                 -> (a -> SPipe i o m ())  -- ^ inner loop
                 -> Pipe i o m ()

Using this, we can easily implement our (horribly inefficient) `Char`-based file pipes:

    sourceFile :: MonadResource m => FilePath -> Source m Char
    sourceFile fp =
        bracketSPipe
            (putStrLn "opening source" >> openFile fp ReadMode)
            (\h -> putStrLn "closing source" >> hClose h)
            loop
      where
        loop handle = do
            eof <- liftIO $ hIsEOF handle
            unless eof $ do
                c <- liftIO $ hGetChar handle
                liftIO $ putStrLn $ "Read from source: " ++ show c
                yield c
                loop handle

    sinkFile :: MonadResource m => FilePath -> Sink Char m ()
    sinkFile fp =
        bracketSPipe
            (putStrLn "opening sink" >> openFile fp WriteMode)
            (\h -> putStrLn "closing sink" >> hClose h)
            (forever . go)
      where
        go handle = do
            c <- await
            liftIO $ putStrLn $ "Writing to sink: " ++ show c
            liftIO $ hPutChar handle c

    conduitFile :: MonadResource m => FilePath -> Conduit Char m Char
    conduitFile fp =
        bracketSPipe
            (putStrLn "opening conduit" >> openFile fp WriteMode)
            (\h -> putStrLn "closing conduit" >> hClose h)
            (forever . go)
      where
        go handle = do
            c <- await
            liftIO $ putStrLn $ "Writing to conduit: " ++ show c
            liftIO $ hPutChar handle c
            yield c

And just to make sure everything's working correctly, I've left in some debug output. If we run this test program:

    src1 = sourceFile "sfiletest.hs" $= CL.isolate 3
    src2 = CL.sourceList "world"

    main = runResourceT $
        ((src1 $= conduitFile "conduit") >> src2)
        $$ sinkFile "sink"

We get the output:

    opening sink
    opening conduit
    opening source
    Read from source: 'i'
    Writing to conduit: 'i'
    Writing to sink: 'i'
    Read from source: 'm'
    Writing to conduit: 'm'
    Writing to sink: 'm'
    Read from source: 'p'
    Writing to conduit: 'p'
    Writing to sink: 'p'
    closing source
    closing conduit
    Writing to sink: 'w'
    Writing to sink: 'o'
    Writing to sink: 'r'
    Writing to sink: 'l'
    Writing to sink: 'd'
    closing sink

As we would hope, the source and conduit close as early as possible, and the sink closes after its extra input is consumed.

## Let the arguments begin!

When we were working on conduit 0.4, we were also trying to get Yesod 1.0 out the door, and so we had a limited discussion period. There is no such pressure this time around. Now's a great time to bring up questions, ideas, and concerns about conduit (as has been happening quite a bit this week).

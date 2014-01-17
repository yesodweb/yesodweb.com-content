I wanted to post an update on [yesterday's blog
post](http://www.yesodweb.com/blog/2014/01/st-monad-conduit) about using the ST
monad in conduit. On Reddit, gereeter [pointed
out](http://www.reddit.com/r/haskell/comments/1vcvxe/the_st_monad_and_conduit/cer0l4l)
some serious problems with the approach I laid out. I think the problems come
down to two categories:

*   Users can (ab)use the `conduitSwapBase` abstraction to perform arbitrary `ST` actions anywhere in their code, which is clearly wrong:

        unsafePerform = runPipe . conduitSwapBase . lift

*   If a Conduit gets run multiple times from some point in the middle of its computation, the abstraction breaks. This can occur in at least two circumstances:

    * Using a backtracking base monad such as list.
    * Using connect-and-resume and then connecting the same intermediate ResumableSource multiple times.

So it's quite clear at this point that the `ST` instance used by `conduitSwapBase` is wrong and should be removed. For now, I think I'm going to remove the entire `conduitSwapBase` machinery since, while it may be somewhat useful for lifting `IO` to `MonadIO` and other such things, such functionality is already present in conduit.

All that said... what about my original problem? Are we really going to give up on the dream of having a referentially transparent signature for `freqSink`? Actually, I realized I was being __incredibly__ dense yesterday. Let's look again at the initial `freqSink` I provided:

```haskell
freqSink :: Consumer ByteString IO (V.Vector Int)
freqSink = do
    freq <- lift $ VM.replicate 256 0
    CB.mapM_ $ \w -> do
        let index = fromIntegral w
        oldCount <- VM.read freq index
        VM.write freq index (oldCount + 1)
    lift $ V.freeze freq
```

My complaint about this was that the type signature says `IO`, when in reality it works for both `IO` and `ST`. However, I'd simply forgotten what the actual type signatures in the vector package look like, e.g.:

    new :: (PrimMonad m, Unbox a) => Int -> m (MVector (PrimState m) a)

In other words, none of the code I wrote above __ever implied the `IO` constraint__. So I can trivially generalize the type signature of `freqSink` above to:

    freqSink :: PrimMonad m => Consumer ByteString m (V.Vector Int)

This now gives exactly the right implication about the code: it runs in any prim monad (which happens to be IO or ST), and while it is externally referentially transparent, it makes use of some state transformer operations under the surface. Looking at the critiques of the `conduitSwapBase` approach above, let's see if they're resolved:

* There's definitely no way to implement `unsafePerform`, since we haven't added any ST-specific code to conduit!
* Putting a backtracking monad as the base monad is now impossible, since it wouldn't be an instance of `PrimMonad`.
* Connect-and-resume will maintain the intermediate state for each time the ResumableSource is reused. However, that's *always* the case when using `IO` or `ST` as a base monad.

So overall, this seems like a much saner approach to the problem.

Thanks again to gereeter for the feedback on the previous blog post and catching my mistakes!

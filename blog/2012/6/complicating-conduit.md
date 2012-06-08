The main target of this blog post is the existing base of `conduit` users. As many of you probably know already, there have been lots of discussions about theoretical approaches to this problem domain being discussed (including the new [streaming-haskell](http://groups.google.com/group/streaming-haskell) mailing list). I think we've come up with a lot of very cool ideas. My main question is: are these ideas an improvement to `conduit` from a user perspective, or just unneeded complication?

I've put up the [most recent Haddocks](http://www.snoyman.com/haddocks/conduit-0.5.0a/Data-Conduit.html), which include a pretty thorough tutorial on `conduit`. I'll try to do a decent job explaining the changes here for those already familiar with `conduit`.

__Note__: Since `conduit` 0.4, `Source`, `Sink`, and `Conduit` are all unified into a single type, `Pipe`. If I mention `Pipe`, I'm referring to that underlying unifying type, not a type from the `pipes` package.

## The Good

I think some of the changes are "obviously" good, for some value of obvious. This mostly focuses in on the high-level interface exposed to the user. We introduced the `await`/`yield` combination in `conduit` 0.4, which in theory makes it much easier to create new `Pipe`s. However, as [mentioned previously](http://www.yesodweb.com/blog/2012/05/next-conduit-changes), it's not quite as nice as it could be, since `conduit` 0.4 does not have auto-termination.

Simply stated, auto-termination means that the following would actually work:

    mapM_ yield [1..]

In `conduit` 0.4, this will loop forever, since we've provided no escape route. Earlier, I'd mentioned the idea of creating a separate "terminating pipe" which would allow the above code to work. Felipe pointed out, however, that having two sets of `yield` functions with different semantics would be confusing, and I agree.

Since then, I played around with the finalization semantics of `conduit`, inspired by a few ideas from Gabriel. I've put together a new approach (and [documented it in the Haddocks](http://www.snoyman.com/haddocks/conduit-0.5.0a/Data-Conduit.html#g:2)), which now allows us get auto-termination. This means that, when you call `yield`, your `Pipe` will terminate immediately if the downstream `Pipe` completes.

Additionally, I've added some high-level functions for handling finalization (e.g., [bracketP](http://www.snoyman.com/haddocks/conduit-0.5.0a/Data-Conduit.html#v:bracketP)). So at this point, I believe a high-level interface consisting of `await`, `yield`, `leftover`, and `bracketP` should cover creation of the vast majority of `Pipe`s. As a result, we can:

* Move the `Pipe` constructors to a separate module (`Data.Conduit.Internal`), and recommend people avoid using them directly.
* Deprecate usage of the (now antiquated, and quite inefficient) `sourceIO`, `conduitState`, etc. functions. They were already [going out of fashion](http://www.yesodweb.com/blog/2012/04/skinning-conduits), but having such a simple alternative really does them in.

I think these changes are pretty non-controversial. I fully intend to continue exporting the constructors, as I believe there are corner cases that will still need them. But I've rewritten about 10 conduit-based libraries for testing (including warp and http-conduit), and except for one case, the high-level interface described here was sufficient.

## The questionable

Here's where I need some input. Over the past few months, `pipes`, `pipes-core`, and `conduit` have been converging on similar designs. `conduit` adopted the single datatype approach, and `pipes` and `pipes-core` both added finalization support. However, there are two major differences still remaining in their current versions: the `pipes` family allows upstream return results, while `conduit` does not. I've [expressed elsewhere](http://www.reddit.com/r/haskell/comments/uav9d/pipes_20_vs_pipescore/c4u2n1x) that I don't think that `pipes`'s current approach to upstream results is a good idea, but it was chosen to allow for a `Category` instance. Personally, I prefer intuitive behavior to adhering to a set of laws. The reason this decision is necessary is because of use cases like:

    return 5 >+> idP -- `return 5 =$ idP` in conduit

In the `conduit` 0.4 world, there's no way to get this to result in a value of 5. In fact, the types themselves won't allow it: the code above would simply not compile, as upstream `Pipe`s must have a return value of `()`. However, to satisfy the right identity of `Category`, it must be allowed to work.

The second distinction is leftover support. This is the idea that one `Pipe` can consume some input, and then give some of it back. A prime example of this would be in `ByteString`s processing: you may want to take a `ByteString`, consume a few bytes from it, and give the rest over to the next `Pipe`s in the monadic chain. For example:

    res <- CL.sourceList ["foo", "bar", "baz"] $$ do
        x <- CB.take 4
        y <- CB.take 4
        z <- CB.take 4
        let toStrict = S.concat . L.toChunks
        return $ map toStrict [x, y, z]
    print res
    -- output: ["foob","arba","z"]

This is currently not supported by `pipes` or `pipes-core`. Again, the `Category` instance comes into play: imagine the following code (from Paolo Capriotti):

    CL.map id =$= s                   ==   s 
    CL.map id =$= (leftover x >> s)   /=   leftover x >> s 

In other words, left identity is lost, because the leftovers from the right-hand side of the fusion operators are discarded. This isn't some unknown failing of `conduit`: it has been documented for a long time in the Haddocks. The approach in `conduit` has been that, while discarding leftovers may violate the `Category` laws, the fact is that leftover support is vital for any practical streaming data library, and so we've included the feature, together with warnings of how it can cause problems.

I believe we now have solutions for both of these differences that let us keep the power of `conduit` while fulfilling the `Category` laws. Let me explain them, and their downsides.

### Upstream results

This idea [came from Chris Smith](http://www.reddit.com/r/haskell/comments/uav9d/pipes_20_vs_pipescore/c4u4uz5). The idea is that an upstream `Pipe` can return a result value which is different than downstream, and then downstream can receive it. Then the identity `Pipe` would simply return the same result provided by upstream.

In practice, this works by adding a fifth type parameter to `Pipe`: the upstream result. This actually gives a great parallelism: each `Pipe` gives an output and result type, which represent the stream of data it will produce, and the final result to indicate that the stream is done. On the flip side, we have the input and upstream result types, which indicates the stream of data it will receive, and the final result that will indicate that the incoming stream is done. (The final type parameter is the underlying monad.)

This means that right identity now works, e.g.:

    return 5 >+> idP === return 5

You can actually use that code in the devel branch for conduit 0.5, and it works. It also means that we don't have to play any games with result types as is necessary with `pipes` and `pipes-core` currently. So the following would work just fine:

    x <- runPipe $ sourceList [1..10] >+> consume
    -- equivalent to: sourceList [1..10] $$ consume
    print $ x == [1..10] -- True

Besides right identity, another advantage to this is the ability to have upstream results. You can see an [argument from Paolo](http://www.reddit.com/r/haskell/comments/uav9d/pipes_20_vs_pipescore/c4u3gol) for why this is useful. I'm not convinced by that argument (as you can see in the discussion), but it does add an extra feature.

What's really interesting about all of this is that it's actually incredibly close to how `conduit` works right now. If you restrict the upstream result type to be `()`, it's the same as `conduit` 0.4. This is something important to keep in mind for later.

One other addition to the library would be adding in an `awaitE` function:

    awaitE :: Pipe i o u m (Either u i)

This would allow you to get either the next piece of input from upstream, or the upstream result value. We can still provide `await` for those (common) cases where you don't care about the upstream result:

    await :: Pipe i o u m (Maybe i)

So, to break it down simply: the advantages are that we get a right identity and upstream result types are allowed. The downside is that there's an extra type parameter floating around.

### Leftovers

Let's look at the simplest `Pipe` that needs leftovers: `peek`. It's implemented as:

    peek :: Pipe i o u m (Maybe i)
    peek = await >>= maybe
        (return Nothing)
        (\i -> leftover i >> return (Just i))

The problem is that if `peek` is to the right of a fusion operator, that leftover value will be implicitly dropped. For example:

    peek >> consume           -- no data loss
    (idP =$= peek) >> consume -- lost the first element

Notice my use of the term "implicit": I don't think anyone is arguing that the data loss itself is a problem (I've discussed the inherent problems of data loss in streaming data many times before). The problem is that there's no indication that it's happening.

One possibility for solving the leftovers issue is to layer it on top of a `Pipe` type that has no leftover support. However, to my knowledge, no one has come up with a working solution to that yet. More importantly to me: it would be terribly inconvenient to use. We'd need to be constantly converting from our normal `Pipe` to this `LeftoverPipe` (or `PutbackPipe` in `pipes-core` terminology), and I think it would kill usability.

So instead, I came up with a different solution, which introduces (tada!) another type parameter for leftovers. Here's the idea: we have a type parameter saying which kinds of leftovers are being given back by a certain `Pipe`. In the case of `peek` above, it would be the same as the input parameter, e.g.:

    data Pipe l i o u m r
    peek :: Pipe i i o u m (Maybe i)

But a `Pipe` like `consume` which never calls `leftover` wouldn't need to constrain the `l` parameter to be equal to `i`. Instead, it would look like:

    consume :: Pipe l i o u m [i]

And now the trick: the composition operators would only function on `Pipe`s which have a `Void` leftover type, i.e.:

    (>+>) :: Monad m => Pipe Void a b r0 m r1 -> Pipe Void b c r1 m r2 -> Pipe l a c r0 m r2

This means that it's impossible to implicitly lose leftovers through composition, as we're guaranteed by the types to have no leftovers here. We would then have one more function:

    injectLeftovers :: Monad m => Pipe i i o u m r -> Pipe l i o u m r

This allows us to explicitly "inject" leftovers back into the `Pipe` until the `Pipe` is done consuming input, and if there are any leftovers remaining, they are discarded. So this means we can keep all of our current functionality, but actually get indications from the type system when we're discarding data.

Note that if we constrain the leftover parameter to be identical to input, we get the same behavior as `conduit` 0.4.

The disadvantages are the fact that we have (another) extra type parameter, and the inconvenience of explicitly injecting the leftovers.

## Keeping the old interface

Before you decide if you like this change or not, let me add in one more piece of information. For both added type parameters, I noted that we could get the current behavior of `conduit` by constraining the type parameters in some way. It turns out that we can keep our old interface almost entirely intact.

    type Source    m o = Pipe () () o    () m ()
    type Sink    i m r = Pipe i  i  Void () m r
    type Conduit i m o = Pipe i  i  o    () m ()
    
    ($$)  :: Monad m => Source m a    -> Sink a m b    -> m b
    ($=)  :: Monad m => Source m a    -> Conduit a m b -> Source m b
    (=$)  :: Monad m => Conduit a m b -> Sink b m c    -> Sink a m c
    (=$=) :: Monad m => Conduit a m b -> Conduit b m c -> Conduit a m c

Yes, you're reading that correctly: all of our main interaction points with the `conduit` library can remain the same. We have the exact same type parameters to `Source`, `Sink`, and `Conduit`, and the connect and fuse operators do the same thing. Under the surface, these operators are making a call to `injectLeftovers`, so they retain the implicit leftover discarding of the previous versions.

You might be thinking, "If we have all this extra power under the hood, but we still drive it the same way, isn't this a no-brainer?" Well, there are still two aspects of this change that can affect users:

1. Error messages. GHC will spit out all six type parameters, in all their glory. This can be a bit confusing.
2. Writing general code.

That second point is already a bit of an issue, so let me expand. Consider the `peek` function we described earlier. If we had to express it in one of the above three types, we would need to choose `Sink`, as it is the only one that allows a non-`()` result type. So the signature would be:

    peek :: Sink i m (Maybe i)

Under the hood, in conduit 0.4, this expands to:

    peek :: Pipe i Void m (Maybe i)

Now suppose we're trying to construct a `Conduit`, and we want to leverage existing functions. So we do something like:

    myConduit :: Conduit Foo m Bar
    myConduit = do
        ...
        x <- peek
        ...

It doesn't compile, because the output type for `myConduit` is `Bar`, while `Sink` constrains the output type of `peek` to `Void`. To get around this issue, `conduit` 0.4 provides the `sinkToPipe` function. But the better solution is to define library functions to use the most general type available whenever possible, *not* the `Source`, `Sink`, or `Conduit` version.

The problem is exacerbated a bit with these two extra parameters. While it can be annoying to have to work in this general way, I think we have two approaches to mitigating the problem:

1. Let GHC be your friend. Write your code with the simpler type first, then remove the type signature and ask GHC what its type is.
2. Providing similar `sinkToPipe` functions for automatically generalizing types.

To explain the latter:

    sourceToPipe  :: Monad m => Source m o    -> Pipe l i o u m ()
    sinkToPipe    :: Monad m => Sink i m r    -> Pipe l i o u m r
    conduitToPipe :: Monad m => Conduit i m o -> Pipe l i o u m ()

So now that you've got the facts, the question is: are these changes worth it?

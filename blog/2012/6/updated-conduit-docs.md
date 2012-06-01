Over the past few versions, various portions of `conduit` documentation has
ended up on this blog, in the Yesod book, in Mezzo Haskell, or the Haddocks
themselves. It's become a bit of a mess.

So following on the excellent example of `pipes`, I've decided to consolidate a lot of this information into the Haddocks. I've generated the Haddocks and [made them available online](http://www.snoyman.com/haddocks/conduit-0.5.0/Data-Conduit.html) if anyone wants to have a look.

Note that this documentation reflects the upcoming `conduit` 0.5. This code has not yet been released to Hackage, as it needs more testing and more feedback. I'm hoping this post will help encourage the latter :).

Garbiel and I have been discussing some possible changes to `conduit` offline, and the [discussion on Reddit on Paolo's blog post](http://www.reddit.com/r/haskell/comments/uav9d/pipes_20_vs_pipescore/) has been very informative.

In addition to the changes I [mentioned last time](http://www.yesodweb.com/blog/2012/05/next-conduit-changes), I've put in a few more changes to the `conduit` codebase (mostly based on Gabriel's advice):

* The main `Data.Conduit` module will no longer expose the internals of the `Pipe` datatype. Instead, a few primitives are exposed, and (hopefully) everything else can be implemented in terms of those. So far, I've been able to rewrite all of the .List, .Text, and .Binary functions using that interface, which is very encouraging.
* Instead of manually dealing with resource finalization via the constructors, a few combinators (`addCleanup` and `bracketP`) should provide all the capabilities necessary.
* I've relocated most of the "utility" functions (e.g., `conduitState`, `sourceIO`) to a separate `Data.Conduit.Util` module. These functions are less efficient and more clumsy than the primary interface. I'm not (yet) removing these functions, but am considering deprecating them.

## tryYield

To deal with the fact that `Pipe`s don't automatically terminate, I've modified
the `Pipe` yield function (now named `tryYield`). It now has type:

    o -> Pipe i o m r -> Pipe i o m r

The distinction is that, instead of specifying the next
`Pipe` to run via monadic composition, it is now specified via the second
argument. In other words, we can rewrite:

    infinite = yield () >> infinite

as

    infinite = tryYield () infinite

The difference is that the first one really won't ever terminate: monadic binding indicates that the following `Pipe` must *always* run. With `tryYield`, it will only be run if the downstream `Pipe` is still accepting input. I believe this should remove a landmine from the `conduit` API.

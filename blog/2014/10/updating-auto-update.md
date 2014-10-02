A few weeks back, Kazu and I received an email from Martin Bailey, who was
interesting in working with us to further optimize Warp. The subject at the
time was reduced buffer copying and allocations. That subject is very
interesting itself, and one finalized, will get its own blog post as well. But
that's *not* the subject of this blog post.

After working on some ideas, Kazu benchmarked Warp, and found a massive
performance degredation which had already slipped onto Hackage. The problem
only appears under highly concurrent requests (which is exactly what Kazu's
benchmark was testing). That bug has now been resolved, and users are
recommended to upgrade to the newest versions of auto-update and warp. (We also
included some other performance boosts here, care of Ryan Newton's
atomic-primops package.) This blog post covers the problem, and its solution.

## It's about time

One of the required response headers according to the HTTP spec is the `Date`
header. As you might guess, this consists of the date, as well as the current
time on the server. This has a very specific format specified in the spec. A
naive approach to filling in this header would be, for each request, to run:

```haskell
now <- getCurrentTime
let value = formatTimeHTTP now
    headers' = ("Date", value) : headers
```

However, when you're writing a server that handles hundreds of thousands of
requests per second, having to get and format the current time on each request
is incredibly expensive. Instead, quite some time ago, Kazu introduced a date
formatting worker thread, which essentially looks like this:

```haskell
let getNowFormatted = formatTimeHTTP <$> getCurrentTime
nowFormatted <- getNowFormatted >>= newIORef
forkIO $ forever $ do
    threadDelay 1000000
    getNowFormatted >>= writeIORef nowFormatted
return $ readIORef nowFormatted
```

We fork a single thread that recalculates the formatted time every one second,
and updates a mutable reference.  This means that, regardless of how many
clients connect, we will always run this computation at most once per second.
And reading the current time requires no system calls, formatting, or
allocation: it's just a memory read.

## Watt's the matter

The problem with this approach is that, even if there are zero requests, we're
still going to run this computation once a second. This doesn't hurt our
performance too much (it's a relatively cheap operation). However, it does mean
that our process never stops working, which is bad for power conservation. So
we needed a way to let that worker thread turn off when it wasn't needed
anymore, and start up again on demand.

If this sounds familiar, it's because [we blogged about it just two months
ago](http://www.yesodweb.com/blog/2014/08/announcing-auto-update). But there's
an implementation detail I want to point out about auto-update. When I started
writing it, I aimed to have the worker thread completely shut down when not
needed, and then for a *new* worker thread to be spawned on demand. This
resulted in some strange corner cases. In particular, it was possible for two
different callers to spawn two different worker threads at the same time. We
used `atomicModifyIORef` to ensure that only one of those threads became the
"official" worker, and the other one would shut down right away. There was also
a lot of tricky business around getting async exceptions right.

The code wasn't too difficult to follow, and was still pretty short. You can
[view it on
Github](https://github.com/yesodweb/wai/blob/auto-update/0.1.1.3/auto-update/Control/AutoUpdate.hs).

## The core of the problem

Unfortunately, before release, we didn't do enough performance testing of
auto-update in highly concurrent settings. When we threw enough cores at the
problem, we quickly ran into a situation where multiple Haskell threads would
end up making concurrent requests for the auto-update value, and *each of those
threads* would fork a separate worker thread. Only one of those would survive,
but the forking itself was killing our performance! (I had one benchmark
demonstrating that, for one million requests across one thousand threads on
four cores, over ten thousands worker threads were forked.)

There was another problem as well. We made heavy use of `atomicModifyIORef` to
get this working correctly. Compare this to our original dedicated thread
solution: each data access was a simple, cheap `readIORef`. By introducing
`atomicModifyIORef` at each request site, we introduced memory barrier issues
immediately.

## The solution

After iterating a few times, Kazu, Martin and I came up with a fairly elegant
solution to the problem. As noted, the original dedicated thread was in many
ways ideal. A lot of our problems were coming from trying to fork threads on
demand. So we came up with a compromise: why not have a single, dedicated
worker thread, but allow it to go to sleep/block when it's no longer needed?

Blocking semantics meant we needed to move beyond `IORef`s over to either
`MVar`s or STM (we elected for the former, but it would still be interesting to
compare performance with the latter). But we still want to be able to have
incredibly cheap lookup in the common case of a value being precomputed. So we
ended up with three mutable variables:

1. An `IORef` containing a `Maybe a`. If a precomputed value is available, it's
   present in there as `Just a`. Otherwise, there's a `Nothing` value. This
   mean that, in the normal case, a requester only needs to (a) do a memory read,
   and (b) case analyze the value.

2. An `MVar` baton to tell the worker thread that someone is waiting for a
   value. In the case that the `IORef` contains `Nothing`, the requester fills
   this `MVar`. Before it starts working, the worker thread always tries to
   `takeMVar` this baton.

3. An `MVar` for passing back result values. This may seem redundant with the
   `IORef`, but is necessary for the case of `Nothing`. A requester `putMVar`s
   into the baton, and then blocks in `readMVar` on this reference until the value
   is available. The worker thread will update both the `IORef` and this value at
   the same time, and then go to sleep until it needs to compute again, at which
   point the process will loop.

Since the requesters never try to fork threads, there's no possibility of
running into the high contention problem of multiple forks. The worst case
scenario is that many requesters see a `Nothing` value at the same time, and
all end up blocking on the same `MVar`. While less efficient than just reading
an `IORef`, this isn't nearly as expensive as what we had previously.

There's another big advantage: the normal case no longer uses any
`atomicModifyIORef` (or any other synchronized memory access), meaning we've
avoided memory barriers during periods of high concurrency.

And finally: [the code is drastically
simpler](https://github.com/yesodweb/wai/blob/auto-update/0.1.1.4/auto-update/Control/AutoUpdate.hs).

And now that we've fixed our regression, and gotten back high performance
together with better power consumption, we can finally return to Martin's
original idea of better buffer management. More on that soon hopefully :).

Following up on [last month's blog
post](http://www.yesodweb.com/blog/2014/04/disemboweling-wai), I'm happy to
announce that a version of WAI without any conduit dependency is available for
testing. If you'd like to test this the easy way, please add the following line
to your Cabal config file (on a Linux system: $HOME/.cabal/config):

    remote-repo: wai3:http://www.stackage.org/stackage/79eaf0520967e1e63e61b39e15f914a8971052c5

Then, run `cabal update` and you should be able to install wai-3.0. This
snapshot also includes updated versions of all the yesod packages, which are
compatible with wai 2.0 and 3.0 (and, I believe, 1.4).

I think this version of the code is pretty stable, and just about ready for
release. If you're a user of WAI, please take some time to check out the new
version and get in your feedback before the 3.0 release.

## Decisions

There were two issues left unresolved after the last public discussion: the
streaming interface, and how we'd provide exception safety. After quite a bit
of offline discussion, we came to the following decisions.

The streaming interface looks like the following:

    type StreamingBody = (Builder -> IO ()) -> IO () -> IO ()
    data Response
        = ...
        | ResponseStream H.Status H.ResponseHeaders StreamingBody
        | ...

`StreamingBody` is a function which takes two arguments: a function for sending
another chunk of data to the client, and a function to flush data to the
client. This is more efficient than the previous `Maybe Builder -> IO ()`
approach as it avoids the need to construct/destruct a `Maybe` value.

Regarding exception safety, we ended up with:

    type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

where the constructor for `ResponseReceived` is only exposed from an internal
module. The idea is to follow the `bracket` pattern, but also avoid any
RankNTypes/ImpredicativeTypes, as their presence made it much harder to write
middlewares. There is a known flaw with this approach: a poorly written
application can run the supplied callback multiple times. This is *not* stopped
by the type system, but is a violation of the WAI interface.

I'd recommend that anyone building higher-level frameworks on top of WAI use
the type system to ensure that such an abuse can't occur. However, as is
becoming the mantra of WAI, we want to keep the interface low-level and simple,
and therefore we're willing to expose a few rough edges like this.

## WebSockets

The entire wai codebase is now devoid of streaming data libraries... except for
wai-websockets, which depends on the websockets library, which in turn depends
on io-streams. As I think most of my readers know, I'm very much in favor of
sharing libraries across the Haskell ecosystem wherever possible, and
therefore- until now- have had no problem with adding a dependency on a
different streaming data abstraction. However, given that WAI is attempting to
be completely devoid of a streaming data framework, this dependency no longer
makes sense.

There seem to be a few approaches forward here. One would be to simply keep the
status quo, and depend on websockets/io-streams. Another would be to try and
remove the io-streams dependency from websockets. I'm not sure if that's
something Jasper's interested in or not. Another would be to have a
WAI-specific implementation of websockets, which [initially doesn't look too
difficult](https://gist.github.com/snoyberg/2265832d9d4d8f0e7df5).

Note that this issue came to a head earlier this week when [a major performance
problem was
uncovered](https://groups.google.com/d/msg/yesodweb/rvR4uLUMi3k/mEiCovtZzu0J)
in yesod-websockets. We haven't yet done enough investigation to determine the
real culprit, but it appears to be io-streams based on the profiling output.

In any event, it's very likely the WAI 3.0 will ship with wai-websockets still
depending on websockets as-is, and we can figure out any changes to be made for
a later wai-websockets-3.1 release.

## Reflections

Overall, the move away from conduit was not too difficult. Let me address three
separate components:

* Writing applications is not any more difficult, simply because it's trivial to wrap up the new streaming interface inside a conduit interface. I avoided doing so in wai itself, but did exactly that when working on Yesod.
* Writing handlers (e.g. Warp) turned out to not be too bad. With all of the performance optimizations we've put into Warp over the years, it frankly was no longer taking real advantage of the beauty of a streaming data framework, so cutting out conduit didn't really make the codebase any worse (and, in some places, made it a bit cleaner, especially where we were already doing ugly `IORef` tricks).
* The tough one was writing middlewares. conduit simply provides a really easy way to say "take this stream of `Builder`s, convert it to a stream of `ByteString`s, apply a simple transformation on the HTML, and GZIP compress the whole thing." Doing this with lower-level streaming primitives is far more clumsy. For a good example, [see wai-handler-launch](https://github.com/yesodweb/wai/blob/059a145df13d5624f647b7508440d6f2a8717362/wai-handler-launch/Network/Wai/Handler/Launch.hs).

My feeling now is the same as a month ago: if we were in a world where lots of
people were writing WAI middlewares regularly, this change would be a bad idea.
But it's become quite clear that most people are writing applications, where
having a low-level streaming interface is not a big problem.

## Timing

If I hear no feedback on the changes in WAI by June 3, I'll assume everyone's
OK with the changes, and will start planning an official release. So if you
have some feedback you'd like to get in, please do so in the next week.

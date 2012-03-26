Note: the post below was written from a train and a bus, without internet access. Keep that in mind. Since I got home, I've uploaded the relevant Haddocks for [conduit 0.4.0](http://www.snoyman.com/haddocks/conduit-0.4.0/index.html).

I don't think people will accuse me of being shy with my opinions. I've been
pretty outspoken that- while pipes certainly seems very elegant- I'm concerned
that it hasn't been designed from the ground up to deal with the same issues
that `conduit` has always been targeted at. As a quick review:

* Resource handling. There's a claim that resource management is completely orthogonal to pipes, and can be achieved by using `ResourceT`. I disagree: while this may ensure that resources are *eventually* released, there's no guarantee that resources will be freed as early as possible.
* Nonstandard control flow is not supported. This is a huge pain point we had with `enumerator`: you have to make sure that the entirety of your code lives in the `Iteratee` monad to have access to the data stream. `conduit` introduced the concept of `BufferedSource`, which allowed us to greatly simplify many packages (WAI, http-conduit, persistent...).
* I could be wrong here, but I don't think pipes has any support for lazy I/O. This isn't really a big deal, but it *is* a nice feature in `conduit` that I'd like to keep. (I'm referring to the Data.Conduit.Lazy module, which has the `lazyConsume` function.)

Many of you probably saw a blog post a few days ago (sorry, I'm writing
offline, no links) analyzing the types in the `conduit` 0.3 release, and
pointing out that it would be possible to unify all three main datatypes
(`Source`, `Sink`, and `Conduit`) into a single type.

What followed was a very thorough discussion/debate on Reddit. My side of the
discussion basically came down to:

* As seemingly obvious as this translation may appear, I won't believe it works properly until I see the code. I'm stubborn like that.
* The blog post completely ignored `BufferedSource`, which is central to `conduit`.
* Even if everything went perfectly for the first two points, we can't simply declare it more elegant because we did away with two types. Elegance is far too subjective, and particularly when comparing a concrete implementation to an abstract recommendation, it's a meaningless concept.

In other words: this idea seems interesting, but it's far too early to claim
victory. It needs more research. (I was a bit disappointed that this sentiment
seemed ungrounded to others on Reddit, but I digress.)

As it turned out, I was sitting on trains and busses for about 6 hours today,
and was sufficiently curious about this question to spend some time
researching. I implemented Twan's type (with one minor tweak, if I remember
correctly, to get the `Monad` instance to work) and got to work. Here are my
findings:

* It is certainly possible to reimplement `conduit` using this modified type, and the resource semantics all seem to work correctly (i.e., all tests pass).
* There is the possibility for a unified connect/fuse function that composes `Pipe`s together. All of the original connect/fuse operators (excluding `BufferedSource`) can be implemented on top of that.
* For the most part, implementation is greatly simplified by this approach. The only exceptions I noticed were the need to call `absurd` for the `HaveOutput` constructor of `Sink`s, and the need to deal with a meaningless `NeedInput` constructor for `Source`.

## BufferedSource

OK... but what about `BufferedSource`? It's actually possible to leave it in
the package precisely as it appears in `conduit` 0.3. However, while working on
this, I came up with a different approach with I'm actually quite taken with.
To start, let's review the problem.

Imagine you're writing a webserver. You want it to work something like:

    let src = sourceSocket socket
    headers <- src $$ getHeaders
    let req = makeRequest headers src
    app req

What's the problem? The `makeRequest` function needs to provide a way for the
application to read the request body. However, on line 2, when we connected the
source to `getHeaders`, some of the request body may have been included in the
data chunk read for the headers. As written, our code will discard that data,
and our application will not get a valid request body! Instead, with
`BufferedSource`, we would write:

    bsrc <- bufferSource $ sourceSocket socket
    headers <- bsrc $$ getHeaders
    let req = makeRequest headers bsrc
    app req

A `BufferedSource` is able to keep track of its state, and keep track of any
leftover chunks from previous calls. This means that our previously discarded
bytes will be kept in a mutable variable inside `bsrc`, and provided for
`makeRequest`.

Instead, I'd like to introduce a new approach:

    let src1 = sourceSocket socket
    (src2, headers) <- src $$& getHeaders
    let req = makeRequest headers src2
    app req

The difference is `$$&`, what I call the connect-and-resume operator (names
open for bikeshedding). The idea is to connect the source to the sink until the
sink terminates, then capture the final state of the source, combine it with
any leftovers provided by the sink, and return it, together with the return
value of the sink.

I haven't yet tested this in Warp and http-conduit, but I'm fairly optimistic
that it can completely supplant `BufferedSource` in both.

## Elegance is in the eye of the beholder

So we're done, right? We got rid of two extra datatypes, plus got rid of
`BufferedSource`, and everything works. Time to release and go home.

Not quite. I still have one concern: is this actually a better solution? Do
people look at this new single type and say, "Hey, it's just a single type, I
can understand that!" or get confused as to why we need to use `Void` in a
`Sink`. Or how about the fact that `Source` and `Conduit` are now both
`Monad`s, but they contain the result type instead of the output type?

In other words, I want input. I think I prefer the new formulation, though I
think having separate types has distinct advantages. One possibility that came
up today with Yitz Gale was using `newtype` wrappers for Source, Sink, and
Conduit. This would let us define only meaningful instances for all three, and
make sure that error messages stay simple. But will it result in too much
wrapping and unwrapping?

## Thank you

I think I gave off the wrong impression in the discussion on Reddit. Some
people believed I wasn't interested in hearing their opinions. I hope this post
shows that this was *not* the case: I value input very much, and clearly what
you were saying has a lot of merit. So thank you.

Don't be surprised or offended when I respond to ideas with skepticism. I
believe that the issues at play here are more complicated than most people
appreciate. When I see an idea that has not yet been fully fleshed out, my
initial reaction is to challenge it, to see if it stands up to the test.

It's been quite a while since I've written a blog post about conduit. I
recently saw (and participated in) some [discussion on
Reddit](http://www.reddit.com/r/haskell/comments/1msinm/perfect_streaming_using_pipesbytestring/)
that made me realize some people may benefit from a status report on where
conduit is today.

Quick intro: for those of you unfamiliar with conduit, it's a Haskell library
for handling streaming data. It is designed to provide easy composability,
proper chunked data support, and guaranteed prompt resource finalization.  This
blog post assumes that you've been following the history of streaming data in
Haskell. If you haven't been, but are interested in conduit, I'd recommend
[reading the tutorial
instead](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview).

## Stability of conduit

Version 1.0.0 of conduit was released in February of 2013, or seven months ago.
It introduced a minor breaking change to the 0.5 series, which was first
introduced in June of 2012. In other words, the conduit API has reached a point
of high stability. New higher-level helper functions are occassionally added to
the package, but the core data types and primitive operations have all remained
stable. (Yay!)

Speaking of the core API: it consists of three primitives (`await`, `yield`,
and `leftover`), composition (`=$=`, with the type-specified versions `$=` and
`=$` for clarity), and the connect operator (`$$`). There are also a few other
functions for dealing with finalizers (`yieldOr`, `bracketP`) and the
connect-and-resume operators. My point is: conduit is a *simple* library as
well.

Given the combination of stable and simple, plus the powerful capabilities of
conduit (which we'll get to later in the post), I think it's well worth
everyone's time to have a look at [the conduit
tutorial](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview)
to get comfortable with the library, and try it out on a few of your projects.
I think the main obstacle to conduit usage has been some negative assertions
against it, which leads us to our next section.

## Correctness of conduit

There's a belief that goes around in the community quite a bit that I'd like to
immediately dispel. Contrary to claims to the contrary, __conduit is correct__.
I've seen many people be scared of using conduit because of some horrible
lurking bugs, or some huge violation of various typeclass laws. Let me address
the concrete claims I'm aware of:

* *It's built on a bunch of impure, ugly stuff.* Initial proof-of-concept
  implementations of conduit did in fact use a lot of mutable references. For
  *many* reasons, this was replaced by a CPS approach a number of years ago.

* *An invalid Monad instance.* Also a claim a few years old. This was true,
  when conduit based its leftover implementation on the enumerator approach,
  which allowed for only a single leftover value at a time. This was actually
  spelled out in the documentation, but in any event the situation was resolved
  with the 0.5 release, which allowed for multiple leftovers.

* *Invalid MonadTrans instance.* If you play around with the internal
  constructors, you can observe a difference between `lift . return` and
  `return`. This is approach is a commonly used optimization, and is unobservable
  without reaching into the internals of the library. I think most people agree
  that this is an acceptable approach.

* *Violates the category laws.* Now this is the interesting one. It's true:
  conduit does not provide a valid Category instance. However, there is nothing
  incorrect about this: *conduit never claimed to provide a Category instance*.
  I'm going to elaborate on the motivations behind this below. However, let me
  point out the two ways in which conduit does not follow Category laws:

    * *Ordering of resource finalizers.* conduit prefers to guarantee that finalizers will be called as soon as possible. As a result, there are some times when changing the associativity of conduit composition can alter the order in which finalizers are called. Dan Burton [pointed this out to me](https://github.com/snoyberg/conduit/pull/57#issuecomment-7474555). Gabriel Gonzalez [wrote a blog post](http://www.haskellforall.com/2013/01/pipes-safe-10-resource-management-and.html) around that time giving a more solid explanation for why this dichotomy exists.

    * *Identity does not preserve leftovers.* In other words, `idConduit =$= leftover x /= leftover x`. Internal to the conduit library, there's actually a set of data types that allows this lack of leftover promotion to be codified in the type system. IMO, this is the only true rough edge presented by conduit right now.

I'm personally not aware of any other claims against conduit. Or, for that
matter, of any outstanding bugs in the implementation itself. I've given two
places where conduit deviates from Category laws. I want to step into the
philosophy behind conduit a bit and explain why I did things that way, given
that there are Category-based solutions for both problems.

## The conduit guarantees

conduit has always been designed as a pragmatic framework. The defining
principle here is that there are real-world problems we're trying to solve, and
a solution that doesn't address those isn't a complete solution. Some of those
problems are:

* Deterministic resource handling.
* Exception safety.
* Proper handling of chunked data.
* Allowing for complex control structures (a.k.a., the HTTP proxy problem).
* Easy composability.
* Simple interface.
* Support for monad transformers.
* Support for pure code.

Let me make something clear. There are many problems that conduit *could*
solve, but chooses not to because that's not the task it set out to address.
That doesn't mean that conduit is flawed, just that it may not be the right
abstraction for what you're doing. An example of this is catching an exception from a
Source inside a Sink.

## The alternatives

Having stated what conduit wants to do, and claimed that it's a stable, simple
approach for doing this, the question remains: why should I choose conduit over
the alternatives? The simplest way to answer that is to compare the conduit
feature set with those of the alternatives.

### enumerator

The iteratee pattern, seemingly most publicized in the enumerator package, is
IMO still a great approach, and I based most of my work in conduit on it. There
are, however, a few limitations which conduit tries to overcome, such as
complex control structures, easy composability, and a simple interface. I won't
dwell on this too much, you can [read much
more](https://github.com/snoyberg/conduit/blob/master/README.md#enumerator) in
the (somewhat dated) conduit README.

### io-streams

io-streams is a relatively new contender. My main critique is that they threw out the baby with the bathwater.

* By excluding support for any monads but `IO`, io-streams is drastically
  limited in what problem domains it can easily address, since it eliminates
  both pure code and monad transformer code.

* There is composability, but it's not as general as conduit: you must always
  modify either an InputStream or an OutputStream; you can't have a general
  transformer for both sides.

*   By forcing resource handling to be addressed exclusively via the `bracket`
    idiom, io-streams can end up with less efficient resource usages than
    conduit/resourcet. See [the resourcet
    tutorial](https://www.fpcomplete.com/user/snoyberg/library-documentation/resourcet)
    for a further explanation. The [io-streams tutorial
    itself](http://hackage.haskell.org/packages/archive/io-streams/1.1.2.0/doc/html/System-IO-Streams-Tutorial.html#g:4)
    in fact encourages this more inefficient resource usage, by opening up four
    file descriptors simultaneously when two is sufficient. The standard solution
    in conduit is both more resource efficient and, IMO, significantly cleaner due
    to its declarative nature:

    ```haskell
    runResourceT
         $ mconcat (map sourceFile ["in1.txt", "in2.txt", "in3.txt"])
        $$ sinkFile "out.txt"
    ```

    compared to:

    ```haskell
    main = do
       withFileAsOutput "out.txt" WriteMode $ \outStream ->
       withFileAsInput  "in1.txt" $ \inStream1 ->
       withFileAsInput  "in2.txt" $ \inStream2 ->
       withFileAsInput  "in3.txt" $ \inStream3 ->
       supply  inStream1 outStream
       supply  inStream2 outStream
       connect inStream3 outStream
    ```

All of that said, io-streams has chosen a specific niche to target, and has
done a good job at it. If what you need is specifically to deal with I/O data
sources, it could be a good abstraction. Even in this solution space, I'm not
convinced io-streams has any edge over conduit. But certainly, once you move on
to more general streaming data issues, I think conduit has a clear advantage.

### pipes

The fairest comparison between two libraries really comes down to conduit and
pipes. This isn't surprising: both libraries are based around the same core
principles, and have been influenced by each other many times in the past. When
two things are so similar, the differences stand in stark contrast.

There are some practical differences to point out. In conduit, `await` is
explicit about end-of-stream, whereas in pipes it's not. Finalizers are baked
into the core datatype in conduit, whereas they are not in pipes. But those are
more surface-level issues. I'd like to get to the heart of the issue.

I believe the core distinction is really philosophy. I've explained above my
philosophy with conduit: choose a problem domain, and try to solve that. I
think Gabriel would agree that pipes takes a different approach: choose a set
of core principles (i.e., laws) to adhere to, and then implement as much as we
can on top of that.

My previous blog post touched on a very practical outcome of this distinction:
pipes is not in a position to adopt the most straightforward and intuitive
solution to the problems at hand, because they violate the category laws.

I have a theory about this approach: we've simply shuffled complexity from one
place to another. But that thought is a large enough one that I'll save it for
my next blog post.

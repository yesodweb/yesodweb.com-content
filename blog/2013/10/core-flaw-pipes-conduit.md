This blog post has actually been through many iterations as I've investigated
the problems more thoroughly. After looking at the various examples I'll be
bringing below quite a bit, I've come to a conclusion: there is just one single
design decision in pipes which leads to the problems I'll describe. And
conduit has inherited half of this issue, leading to it getting some of these
issues as well.

In this blog post, I'm hoping to motivate the fact that there is actually a
problem. I've been working on some experimental code in conduit which changes
this design, thereby simplifying the internal structure, keeping all of its
current features, and solving the two ways in which conduit currently does not
follow the category laws. I'll describe all of these issues in this post, and
save the new design for a later blog post.

Note that, while this blog post was actually written first, it can be considered a [continuation of my previous blog post](http://www.yesodweb.com/blog/2013/10/pipes-resource-problems), which gives some very concrete examples of the resource issues I raise below.

## The flaw: automatic termination

If you look at the core concepts of pipes (e.g., the [Pipe datatype in pipes
1.0](http://hackage.haskell.org/package/pipes-1.0/docs/src/Control-Pipe-Common.html#Pipe)),
things are very simple. A Pipe can yield a value downstream, await for a value
from upstream, perform a monadic action, and complete processing. This core is
simpler than conduit's core, which includes leftovers and finalizers, as well
as failure when awaiting from upstream. 

However, this simplicity includes a heavy cost: there's no way to detect
termination of a stream. As soon as one component of a pipeline terminates, the
rest of the pipeline terminates also. This behavior can be convenient in many
ways; the identity pipe, for example, is expressed simply as `forever $ await >>= yield`.
However, this decision ends up pushing complexity into many other parts of the
ecosystem, and in some cases makes proper behavior impossible to achieve.

conduit is not immune to this issue. conduit does not have automatic
termination on the consuming side, but does have it on the producing side.

I've held off on commenting on these limitations in pipes previosly, since
until recently pipes has not provided any form of solution for many of the problems
I'm going to raise. With the advent of pipes-bytestring, pipes-parse, and
pipes-safe, there's enough of a solution available to make a meaningful
analysis. After looking at these solutions, my conclusion is:

* pipes has removed complexity from its core. However, this hasn't in fact
  solved complexity, it's merely pushed the complexity to other helper
  libraries. In my opinion, the overall solution is far more complex than a
  single consistent solution would be.

* In trying to solve some of these problems outside of the core, pipes has lost
  many of its touted principles, such as easy composition.

* And in some cases, the layered pipes solution does not actually provide
  the guarantees we'd expect. Said another way, pipes is buggy.

The remainder of this post will be examples of limitations in pipes and conduit
that result from this functionality. Note that, even though most of the issues
I raise have workarounds, I will not be discussing those in general. My goal is
to point out that the core abstraction is deficient, not address possible
workarounds.

## pipes: How do I fold?

conduit provides a single abstraction which addresses all of the data
processing functionality it supports. You get prompt resource handling, chunked
data support, and the ability to fold over an input stream. In pipes, these are
all handled by a separate abstraction. To clarify what I mean, compare the type
signatures for a summing sink (receives all input values and adds them up) and
a printing sink (i.e., prints all input to stdout) in conduit:

    sum :: (Monad m, Num a) => Consumer a m a
    print :: (MonadIO m, Show a) => Consumer a m ()

Notice how both of these are conceptually the same: they are both consumers,
which can be composed with other conduits in the normal way (i.e. the `=$=`
operator and monadic bind). Now compare the types in pipes:

    sum :: (Monad m, Num a) => Producer a m () -> m a
    print :: MonadIO m => Show a => Consumer' a m r

These two things are fundamentally different. The first is a function that
takes a data producer and processes it. It does not get to take advantage of
normal composition (though that can be achieved by instead composing on the
producer). pipes has these two separate approaches for processing a stream of
data, and each must be used at different points.

To understand why this is the case, let's look at a simplistic implementation
of `sum` in conduit:

    sum =
        loop 0
      where
        loop x = await >>= maybe (return x) (\y -> loop $! x + y)

The `await` function returns a `Maybe` value. If upstream has no more output,
then the `sum` function is notified with a `Nothing` value, and can return the
sum it has computed. In pipes, however, if upstream closes, `await` will simply
never return.

### pipes: Dummy return values

You *could* implement a limited `sum` function in pipes, such as "add up the
first 10 elements." This would look something like this (I'm specializing to
`Integer` to help the explanation later):

    sum :: Monad m
        => Int -- ^ total values to add
        -> Consumer' Integer m Integer
    sum count0 =
        loop count0 0
      where
        loop 0 total = return total
        loop count total = await >>= \i -> loop (count - 1) (total + i)

That's simple enough. In fact, it's even simpler than the conduit version,
since it doesn't need to pay attention to whether upstream terminated. (Put a
bookmark on that comment, I'll get back to it momentarily.)

So let's go ahead and try to use this. A naive caller function may look like this:

    main = do
        x <- runEffect $ mapM_ yield [1..20] >-> sum 10
        print x

However, this generates a compiler error:

    Couldn't match type `Integer' with `()'
    
The issue is that the producer has a return type of `()`, whereas we want to
return an `Integer` from `sum`. pipes requires that all components of the
pipeline have the same return value, since any one of them can terminate
computation. In order to work around this, we need to use some kind of a return
value from the producer. A `Maybe` value works well for this:

    main = do
        x <- runEffect $
            (mapM_ yield [1..20] >> return Nothing)
            >-> fmap Just (sum 10)
        print x

Now our return value is of type `Maybe Integer`, not `Integer`. But if we think
about it, this is perfectly logical, since the `sum` function can't return a
value unless there are at least 10 values in the stream.

This comes back to the fact that the conduit version of the above `sum`
function is more complicated. That's because it will explicitly deal with
termination of the upstream. This grants it the ability to return the current
total, or if so desired, emulate the pipes behavior above and return a
`Nothing` to indicate not enough input was provided.

### conduit: lack of upstream return values

There's a bit of a mismatch in the conduit abstraction: the most downstream
component (the Sink) can provide a return value, but the rest of the upstream
components cannot. This was an explicit design decision, and in my experience
it's what users actually need the vast majority of the time. (I only needed an
upstream return value once in all of my conduit usage, and was able to work
around the problem using some low-level tricks.) However, this does present
some more abstract problems. For one, there's no meaningful right identity in
conduit.

Remember that in conduit, upstream has automatic termination, while downstream
does not. This explains why only downstream can provide a return value.
However, if we turn off automatic termination on both sides, we can get values
returned from both upstream and downstream. (Yes, this claim is pretty vague
right now, I'll elaborate fully in my next blog post.)

## pipes: Prompt resource finalization

Consider the following simplistic file reading function in conduit:

```haskell
readFile :: FilePath -> Source (ResourceT IO) String
readFile file = bracketP
    (do h <- IO.openFile file IO.ReadMode
        putStrLn $ "{" ++ file ++ " open}"
        return h )
    (\h -> do
        IO.hClose h
        putStrLn $ "{" ++ file ++ " closed}" )
    fromHandle
  where
    fromHandle h = forever $ liftIO (IO.hGetLine h) >>= yield

main :: IO ()
main = runResourceT $ producer $$ CL.mapM_ (liftIO . putStrLn)

producer = do
    readFile "input.txt" $= CL.isolate 4
    liftIO $ putStrLn "Some long running computation"
```

It uses the `bracketP` combinator, which uses `ResourceT` to ensure exception
safety, while using conduit's built-in deterministic resource handling to
ensure prompt finalization. Our data producer streams four lines of data from
the file, and then runs some (theoretically) long-running computation to
generate some more output to be placed in the same output stream. Running this
program gives fairly expected results:

```
{input.txt open}
line 1
line 2
line 3
line 4
{input.txt closed}
Some long running computation
```

As we would hope, the input file is opened and closed before the long running
computation even starts. Now let's look at the same code in pipes. This example
is taken from the pipes-safe documentation, modified slightly to include this
long running computation concept:

```haskell
readFile :: FilePath -> Producer' String (SafeT IO) ()
readFile file = bracket
    (do h <- IO.openFile file IO.ReadMode
        putStrLn $ "{" ++ file ++ " open}"
        return h )
    (\h -> do
        IO.hClose h
        putStrLn $ "{" ++ file ++ " closed}" )
    P.fromHandle

main :: IO ()
main = do
    runSafeT $ runEffect $ producer >-> P.stdoutLn

producer = do
    readFile "input.txt" >-> P.take 4
    liftIO $ putStrLn "Some long running computation"
```

pipes-safe provides a `bracket` function, very similar to conduit's `bracketP`.
It also provides `SafeT`, which is strikingly similar to `ResourceT`. Besides
minor differences in operators and functions names, this code is basically
identical. So running it should produce the same output, right?

```
{input.txt open}
line 1
line 2
line 3
line 4
Some long running computation
{input.txt closed}
```

That's a bit worrisome. The input file is kept open during the entire long
running computation!  This problem is identified in the [pipes-safe release
announcement](http://www.haskellforall.com/2013/01/pipes-safe-10-resource-management-and.html)
from January.

The reason pipes is not able to guarantee prompt finalization is that the data
producer is never given a chance to perform its own cleanup. In the line:

    readFile "input.txt" >-> P.take 4

Assuming input.txt has more than four characters, the call to `P.fromHandle` in
`readFile` will never exit. Instead, processing will halt as soon as `take 4`
returns. I consider this behavior to actually be a bug: the `bracket` function
has distinctly different semantics than `Control.Exception.bracket`, and scarce
resources will be kept open for an indefinitely long time (until the `SafeT`
block is exited).

### conduit: lack of assocativity

conduit doesn't get away free here either. conduit also doesn't allow the
upstream to continue processing after downstream completes. Instead, it adds a
new concept: a finalizer function can be yielded with each value. However,
this implementation approach doesn't allow for deterministic ordering of
finalizers. This bug was originally [identified by Dan
Burton](https://github.com/snoyberg/conduit/pull/57#issuecomment-7474555).
However, by getting rid of early termination in producers, we can solve this
problem and take back full associativity.

## pipes: Chunking and leftovers

The other major feature that conduit bakes into the core which pipes does not
is leftovers. Leftover support is necessary for a few different things, but the
need is most apparent when dealing with chunked data structures like
`ByteString` and `Text`. Consider a program which will write the first 20 bytes
from the file "input.txt" to the file "output1.txt", and the second 20 bytes to
"output2.txt". This is trivial in conduit:

```haskell
main :: IO ()
main = runResourceT $ sourceFile "input.txt" $$ do
    isolate 20 =$ sinkFile "output1.txt"
    isolate 20 =$ sinkFile "output2.txt"
```

With an input.txt of:

    hellohellohellohelloworldworldworldworldbyebyebyebye

output1.txt ends up with:

    hellohellohellohello

and output2.txt is:

    worldworldworldworld

(You can inflate the number 20 to something much larger to make the need for
streaming data more realistic.)

Let's imagine that the first chunk of data that is read from input.txt is 60
bytes large. The first call to `isolate` will read that chunk, split it into 20
and 40 byte chunks, send the 20 byte piece off to output1.txt, and return the
remaining 40 bytes as leftovers. The second call to `isolate` can then read
that chunk in and repeat the process.

It's hard for me to imagine this being much more declarative. We're able to
leverage conduit's two forms of composition. Monadic composition allows us to
string together the two consumers to consume successive data from the producer.
And we're able to use fusion to combine the `isolate` calls with the `sinkFile`
calls, and to connect the source with the combined sink.

This kind of dual composition has been the hallmark of pipes since its first
release, so certainly building up something similar should be trivial. With the
newly released pipes-bytestring, let's try to naively copy our conduit code
over.

```haskell
main :: IO ()
main =
    withFile "input.txt" ReadMode $ \input ->
    withFile "output1.txt" WriteMode $ \output1 ->
    withFile "output2.txt" WriteMode $ \output2 ->
        runEffect $ fromHandle input >-> do
            take 20 >-> toHandle output1
            take 20 >-> toHandle output2
```

When I run this code, *output2.txt is empty*! To understand why, let's consider
how `isolate` works in conduit. We get an initial chunk of (say) 60 bytes,
split off the first 20, and return the remaining 40 as leftovers. But pipes has
no leftover support, so take simply drops the data on the floor! Not only is
this unintuitive behavior, and completely undocumented, but is
non-deterministic: if the first chunk was instead 30 bytes, only 10 bytes of
data would be lost. If it was 100 bytes, 80 bytes would be lost. I'd consider
the very presence of this function to be an inherent flaw in this library that
needs to be rectified immediately.

(By the way, `drop` is even worse than `take`. I'll leave it to reader comments
to discover why.)

In order to get the right behavior, you have to use the `splitAt` function instead:

```haskell
main :: IO ()
main =
    withFile "input.txt" ReadMode $ \input ->
    withFile "output1.txt" WriteMode $ \output1 ->
    withFile "output2.txt" WriteMode $ \output2 -> do
        input' <- runEffect $ splitAt 20 (fromHandle input) >-> toHandle output1
        void $ runEffect $ splitAt 20 input' >-> toHandle output2
```

There are a number of points that need to be elucidated here:

* The `do`-block is no longer performing any kind of composition of Pipes, but
  rather just IO composition. In fact, we've had to completely give up on
  "vertical composition" of pipes to make this work.

* We have to explicitly pass around the producer. I'm familiar with this style
  of coding, since early versions of conduit encouraged it, and it's not an
  experience I'd want to repeat. It's easy to accidentally pass around the old
  producer instead of the new one, for example. And this is exactly the kind of
  drudgery that streaming libraries should be able to liberate us from!

This approach to leftovers just inverts the whole concept of a producer to a
pull-based model. This is a valid approach, but it sacrifices so much of the
elegance and simplisity we have in a streaming library, and pushes it to a user
problem. The API is now seemingly doubled, between "Pipes" and "Splitters" as
the API documentation calls them. (This is similar to the issues I raise above
regarding folds.)

While these limitations [can be worked
around](http://www.reddit.com/r/haskell/comments/1n0i29/folding_lines_in_conduit/ccep9ek),
I believe the workarounds defeat so much of the elegance of the declarative
approach pipes claims. conduit keeps that elegance by baking leftovers directly
into the core abstraction.

### pipes: Simple parsing

As a further illustration of the problems of lack of proper chunked data
support, consider the following trivial conduit snippet:

```haskell
parseA :: Monad m => Sink Text m A
parseC :: Monad m => Sink Text m C

myParse :: Monad m => Sink Text m (A, C)
myParse = (,) <$> parseA <*> parseC
```

Since monadic composition works naturally for `Sink`s- even chunked `Sink`s-
composing two different `Sink` can be achieved by using standard Applicative
operators. Such easy composition is not possible (AFAICT) with pipes-parse or
pipes-bytestring.

So my claim is: pipes has simplified its core by leaving out leftover support,
resulting in some really complicated user-facing APIs. conduit includes the
complexity in one place, the core, and the rest of the codebase reaps the
benefits.

### conduit: Lack of identity in presence of leftovers

conduit solves leftovers by baking it into the core abstraction as a separate
concept. This has been criticized by Gabriel and others (rightfully so) in that
it makes the core harder to reason about. The manner in which this issue
manifests is that identity does not preserve leftovers. In other words,
`idConduit =$= leftover x /= leftover x`.

At this point, you're probably wondering: I get the problems with leftovers,
how does this indict automatic termination as the cause? I'll have to be a bit
vague until my next post, but the basic idea is that there's an incredibly easy
way to implement leftovers: each time a component completes, it returns both
its return value and its leftovers. When this component is monadically composed
with another component, the leftovers are supplied as input to that new
component. And when composed via fusion (a.k.a., vertical composition), the
leftovers are provided as part of the result.

## pipes and conduit: isolate

I don't think the iteratee approach gets nearly enough credit; in some cases,
we're still not completely caught up. Take for example [the `isolate`
function](http://haddocks.fpcomplete.com/fp/7.4.2/20130922-179/enumerator/Data-Enumerator-List.html#v:isolate),
which has the following description:

> isolate n reads at most n elements from the stream, and passes them to its iteratee. If the iteratee finishes early, elements continue to be consumed from the outer stream until n have been consumed.

This kind of function could be incredibly useful for something like consuming
an HTTP request body. A web server will determine the length of the request
body from the `content-length` header, and then stream that body to the
application. If the application doesn't consume the entire body, `isolate` can
ensure that the rest of the input is flushed, so that the next request is
available for the webserver to continue processing.

A simpler example of this would be a function to consume lines. Consider the
following approach in conduit:

    line :: Monad m => Conduit Char m Char
    line = do
        mc <- await
        case mc of
            Nothing -> return ()
            Just '\n' -> return ()
            Just c -> yield c >> line

The algorithm is simple: get a character. If there is no character, or it's a
newline, we're done processing. Otherwise, yield the character downstream, and
continue. Let's try to use this function to get the second line of input:

    main = do
        secondLine <- mapM_ yield "Hello\nWorld\n" $$ do
            line =$ return ()
            line =$ CL.consume
        putStrLn secondLine

We'd expect the output to be `World`, but unfortunately it's not. The actual
output is `Hello`. The reason is that the Sink attached to the first call to
`line` does not consume any of the input provided by `line`. As a result, it
terminates immediately, and therefore `line` also terminates immediately, since
producers automatically terminate. In fact, `line` is never called here at all!

One workaround is to provide a modified `line` that takes a `Sink` as its first
argument, e.g.:

    -- This is the same as the previous line
    lineHelper :: Monad m => Conduit Char m Char

    line :: Monad m
         => Sink Char m a
         -> Sink Char m a
    line sink = lineHelper =$ do
        result <- sink
        CL.sinkNull -- discard the rest of the line
        return result

Then, instead of using fusion to combine `line` with our sinks, we just pass
them as arguments, e.g.:

    main = do
        secondLine <- mapM_ yield "Hello\nWorld\n" $$ do
            line $ return () -- note: replaced =$ with $
            line $ CL.consume
        putStrLn secondLine

While this works, it's not ideal. Like the pipes solutions to folding and
leftovers, we're left with two different and conflicting approaches which don't
compose with each other.

### Sneek preview

To give a bit of a sneak peek for the next post, let's consider what an ideal
version of `line` may look like. It would need to be able to continue consuming
input after calling `yield`. We may even call that something like `tryYield`,
and allow `yield` to maintain its current auto-termination behavior. This would
look like:

    line :: Monad m => Conduit Char m Char
    line = do
        mc <- await
        case mc of
            Nothing -> return ()
            Just '\n' -> return ()
            Just c -> tryYield c >> line

We're still left with a question. The current behavior of conduit would mean
that no input is consumed if downstream is already closed. With the
hypothetical `line` function I just wrote, one character will be consumed
before `tryYield` is ever called. Is there any way to perfectly model the
previous behavior and ensure no actions are performed if downstream is closed?
I'll let you know in the next blog post.

## Conclusion

I want to be clear: the pipes design is very elegant. Some of the issues I've
listed above can be worked around. If you're OK with having to use a separate
set of functions for writing folds, for example, the approach will work.
However, there are other cases- like prompt resource finalization- for which
there does not appear to be a readily available workaround. If you don't have
need of prompt resource finalization, then this limitation may not bother you.
For other cases, it could be a deal-breaker.

On the conduit side, we're looking at three identified flaws: associativity
affecting finalizers ordering, guaranteed emptying when using a Conduit, and
identity regarding leftovers. These would all be nice to fix, but at the same
time none of them are major issues. So the question is: would making this kind
of a change be worth it?

Before making any decisions, I think it's worth analyzing the new design. I
think at the very least it will give us new insights into our existing
approaches, and maximally may let us drastically improve our streaming
libraries.

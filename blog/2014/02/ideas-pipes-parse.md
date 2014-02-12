I haven't mentioned them before, but many times in the past, I've played around
with from-scratch implementations of conduit to test out new theories. I've
tried having versions without automatic termination of any sort, different
orderings of finalizers, various different ways of handling leftovers, and
strict adherence to the category laws. Each one of them has taught me
something, but since the 1.0 release, none of them were enough of an
improvement on what exists already to warrant the pain in switching. And
frankly, most of them were far worse than what we already have.

A few days ago, Gabriel Gonzalez [wrote a blog
post](http://www.haskellforall.com/2014/02/pipes-parse-30-lens-based-parsing.html)
about the newest release of pipes-parse. Given that we've had many
conversations over the years- both publicly and privately- about the directions
of our libraries, I wasn't surprised that the core idea behind this library is
something we'd discussed once before, and in fact was an idea I'd even tried
out for conduit once. Based on that experience, I'd like to share some ideas on
how Gabriel (or others) could approach the problem a bit differently, and
possibly avoid some user-facing issues.

## Proliferation of APIs

Before discussing the details of leftovers themselves, let mention what I
consider the primary issue. It seems that each new feature added to pipes is
introducing a completely separate API. Consider, for example, a simple
question: how do you get the next value in a stream? In the conduit world, the
answer is `await`. There are also some convenience functions built on
top of `await`: `awaitForever`, `peek`, `mapM_`, etc. But there is just one
primitive for awaiting values, and it has the same type everywhere:

    await :: Monad m => Consumer i m (Maybe i)

In the pipes world, there are now (to my knowledge) three different "get the
next value" primitives:

```haskell
await :: Monad m => Consumer' a m a
next :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))
draw :: Monad m => Parser a m (Maybe a)
```

This in turn means that utility functions need to be written multiple times, in
different ways, depending on the context in which they're needed. For example,
`takeWhile` in `Pipes.Prelude` works on a "normal" `Pipe`, but will silently
discard one extra value from the stream since a normal `Pipe` does not support
leftovers. `span` from `Pipes.Parse` performs essentially the same
functionality, but works in the `Parser`/leftover aware realm.

One of the biggest changes to come to the conduit library was the unification
of the `Source`, `Sink`, and `Conduit` datatypes into a single datatype, called
`Pipe`. And the reason it's called `Pipe` is, as you might guess, because it
was inspired from the `pipes` world (via [Twan van
Laarhoven](http://twanvl.nl/blog/haskell/conduits-vs-pipes)). Though I was
skeptical at first about the confusion in error messages and type signatures
which may have ensued, the net result was, in my mind, undeniably positive.

It seems to me like pipes is now at that same kind of crossroads. There are a
large number of different data types and type synonyms, different ways of
composing things together, and different functionality depending on what type
you're using. I'd recommend standardizing on one as the canonical entry point
to pipes, and make the standard libraries all use that API.

It seems like the `Parser` API is the best suited for this task. Unless I'm
mistaken, all of the folding functions in `Pipes.Prelude` (e.g., `toList`) can
be implemented in terms of `Parser`, and it adds the capability of leftovers.
If this change happened, then functions like `takeWhile` would no longer have
to silently discard data from the data stream either.

Side note: you might think that `conduit` has lots of different kinds of
composition due to the three different fusion operators, `$=`, `=$`, and `=$=`.
In reality, they're all type-restricted aliases for the same function. The
question of whether they should be type restricted at all is a good debate to
have, but that's not my topic here.

## Overview of approaches

With that more general comment out of the way, let's get into the details of
`Parser`.  Leftovers may seem like a really complicated concept, and (at least
to me) lens-based parsing sounds pretty intimidating. However, the core
concepts here are pretty trivial, and I think most people faced with the same
constraints would end up with similar solutions to what the major streaming
data libraries have. To motivate this, let's consider the simplest of all
streaming data: pure lists.

In our case, we'll store our "data producer" (i.e., a list) in a `State` monad.
Getting another value from the stream (a.k.a., awaiting, drawing) means getting
the list, popping an element off the top, putting back the smaller list, and
returning the popped element. Putting a value back in the stream (a.k.a.,
undrawing, leftover) means getting the list, sticking an element at the
beginning, and putting it back. This can all be embodied in very little
Haskell code:

```haskell
import Control.Monad.Trans.State.Strict

type Parser a r = State [a] r

-- In conduit, this is await
draw :: Parser a (Maybe a)
draw = do
    list <- get
    case list of
        [] -> return Nothing
        x:xs -> do
            put xs
            return (Just x)

-- In conduit, this is leftover
unDraw :: a -> Parser a ()
unDraw a = do
    list <- get
    put $ a:list
```

At its core, this is what pipes-parse is doing. Instead of a pure list, it's
using a `Producer`, which is really just a list transformer. But there's
another, slightly less obvious approach that we could take instead. Right now,
we're sticking leftovers right back on the same stack, making it impossible to
distinguish between values taken from the original stream, and values leftover
from the `Parser`. Instead, we could store a tuple in the `State` monad: the
original list, and the leftovers. This is also pretty easy to code:

```haskell
import Control.Monad.Trans.State.Strict

type Parser a r = State ([a], [a]) r

-- In conduit, this is await
draw :: Parser a (Maybe a)
draw = do
    (list, leftovers) <- get
    case leftovers of
        x:leftovers' -> do
            put (list, leftovers')
            return (Just x)
        [] ->
            case list of
                [] -> return Nothing
                x:list' -> do
                    put (list', leftovers)
                    return (Just x)

-- In conduit, this is leftover
unDraw :: a -> Parser a ()
unDraw a = do
    (list, leftovers) <- get
    put (list, a:leftovers)
```

While this certainly works, it seems a little overkill: what possible benefit
is there in having this separation? Well, it would allow us to distinguish
between "completely unparsed values" and "parsed and leftovered" values. In our
discussion so far, and in the documentation for pipes-parse, I see absolutely
no reason why this feature would be relevant. However, let me introduce a
non-trivial parsing example to motivate things a bit further.

## An archive file format

Let's say for some (probably bad) reason we've decided that the tar file format
is unsuitable for our purposes. Instead, we've come up with a new file format
that works as follows:

* Each file consists of a textual filename and binary contents.
* The filename will be UTF8 encoded.
* We will encode lengths using a variation on netstrings: the decimal representation of the length followed by a colon.
* Each file will be encoded as the length of the textual filename, its UTF-8 representation, the length of its binary contents, and the contents.

Yes, this example is ridiculous, but I wanted to find something that would
demonstrate pipes-parse's ability to handle leftover preserving. To make the
above description a bit easier to understand, here's the Haskell code to encode
a list of these files:

```haskell
data File = File
    { fileName     :: !Text
    , fileContents :: !ByteString
    }
    deriving Show

encodeFile :: File -> Builder
encodeFile (File name contents) =
    tellLen (T.length name) <>
    fromByteString (TEE.encodeUtf8 name) <>
    tellLen (S.length contents) <>
    fromByteString contents
  where
    tellLen i = fromByteString $ TEE.encodeUtf8 $ T.pack $ shows i ":"

encodeFiles :: [File] -> Builder
encodeFiles = mconcat . map encodeFile
```

What's important for the parsing is that we will need to switch back and forth
between a binary and a textual stream of data in order to handle this
correctly. (Note: in reality, if for some terrible reason you decide to
actually implement this format, you should encode the filename length using the
__byte count__, not the character count. I specifically used the character
count to force this awkward kind of stream switching.)

I've [implemented the parser in conduit](https://gist.github.com/snoyberg/8888288) if anyone's interested in checking it out. The magic leftover-preserving occurs in the `withUtf8` function:

```haskell
withUtf8 :: MonadThrow m
         => ConduitM Text o m r
         -> ConduitM ByteString o m r
withUtf8 =
    fuseLeftovers toBS (CT.decode CT.utf8)
  where
    toBS = L.toChunks . TLE.encodeUtf8 . TL.fromChunks
```

We're saying to convert the stream to text assuming a UTF8 encoding. We'll
generate chunks of text on demand (i.e., lazily), and will stop once we hit an
invalid UTF8 sequence (that's the behavior of `Data.Conduit.Text`). Then, after
downstream is done, collect all of the leftovers that it generated, and convert
them back to their binary representation.

Obviously, in order to do this, we need to be able to distinguish between the
consumed upstream and the leftovers from downstream. If we cannot make that
distinction, we'd be forced to encode the *entire* stream into text, take out
the text that we need, and then convert the rest of the stream *back* to
binary. Moreover, we'd have to perform the conversion back and forth for each
time we call `withUtf8`. And even worse than the performance hit is the fact
that it may not work: if the byte stream contains invalid UTF8 character
sequences, this may break, depending on how your parser handles such invalid
sequences.

I'm fairly certain that pipes-parse falls prey to this issue. (If I've
misunderstood the library, please correct me, and I'll include the correction
here.) `conduit` handles the issue differently: the "parser" (a.k.a., `Sink`)
has an explicit command for reporting leftovers, and it's up to the composition
operator to decide how to handle the leftovers. The standard operators- `$=`,
`=$` and `=$=`- use a similar trick to pipes-parse, and stick the leftovers
onto the upstream `Source`. And that's precisely why they have the behavior of
discarding downstream leftovers. However, this is just a sensible default, not
a requirement of conduit. It took me under five minutes to write [an
alternative composition
function](http://hackage.haskell.org/package/conduit-extra-0.1.4/docs/Data-Conduit-Extra.html#v:fuseReturnLeftovers)
that had leftover preserving behavior instead.

### Simpler example

I tried to make the above example somewhat realistic, but the details may be a
bit overwhelming. Instead, let's consider something much simpler: an
isomorphism between two data types. We want to convert the stream from type `A`
to type `B`, perform some peeking, and then deal with the rest of the stream.
An example of doing this with conduit would be:

```haskell
import           Control.Applicative ((<$>), (<*>))
import           Data.Conduit        (yield, ($$), (=$=))
import           Data.Conduit.Extra  (fuseLeftovers)
import qualified Data.Conduit.List   as CL
import           Debug.Trace         (traceShow)

newtype A = A Int
    deriving Show
newtype B = B Int
    deriving Show

atob (A i) = traceShow ("atob", i) (B i)
btoa (B i) = traceShow ("btoa", i) (A i)

main :: IO ()
main = do
    let src = mapM_ (yield . A) [1..10]
    res <- src $$ (,,,)
        <$> fuseLeftovers (map btoa) (CL.map atob) CL.peek
        <*> CL.take 3
        <*> (CL.map atob =$= CL.take 3)
        <*> CL.consume
    print res
```

We have the numbers 1 through 10 as type `A`. In our `Sink`, we first convert
the stream to type `B`, peek, and then return the leftovers upstream. Then we
take three `A`s, again convert to `B` and take three more elements, and finally
consume the remainder of the stream. I've added trace statements to demonstrate
exactly how many conversions are performed:

```
("atob",1)
("btoa",1)
("atob",4)
("atob",5)
("atob",6)
(Just (B 1),[A 1,A 2,A 3],[B 4,B 5,B 6],[A 7,A 8,A 9,A 10])
```

The conversions that occur are the absolute bare minimum than could occur: the
first element has to be converted to `B` in order to be `peek`ed at, and then
converted back to `A` to return to the original stream. Then, when we later
take three more elements of type `B`, they obviously need to be converted.

Let's look at the equivalent in pipes-parse, [courtesy of Joseph Abrahamson](http://stackoverflow.com/questions/21649920/using-pipes-parse-to-preserve-leftovers-with-a-map/21650705):

```haskell
{-# LANGUAGE RankNTypes #-}
import           Control.Applicative
import           Control.Lens               (Iso', from, iso, view, zoom)
import           Control.Monad.State.Strict (evalState)
import           Debug.Trace
import           Pipes
import           Pipes.Core                 as Pc
import qualified Pipes.Parse                as Pp
import qualified Pipes.Prelude              as P

newtype A = A Int
    deriving Show
newtype B = B Int
    deriving Show

atob (A i) = traceShow ("atob", i) (B i)
btoa (B i) = traceShow ("btoa", i) (A i)

ab :: Iso' A B
ab = iso atob btoa

piso :: Monad m => Iso' a b -> Iso' (Producer a m r) (Producer b m r)
piso i = iso (P.map (view i) <-<) (>-> P.map (view $ from i))

main :: IO ()
main = do
  let src = P.map A <-< each [1..10]
  let parser = (,,,) <$> zoom (piso ab) Pp.peek
                     <*> zoom (Pp.splitAt 3) Pp.drawAll
                     <*> zoom (Pp.splitAt 3 . piso ab) Pp.drawAll
                     <*> Pp.drawAll
  let res = evalState parser src
  print res
```

The result is the same, but look at the traces:

```
("atob",1)
("btoa",1)
("atob",2)
("btoa",2)
("atob",3)
("btoa",3)
("atob",4)
("btoa",4)
("atob",4)
("atob",5)
("btoa",5)
("atob",5)
("atob",6)
("btoa",6)
("atob",6)
("atob",7)
("btoa",7)
("atob",8)
("btoa",8)
("atob",9)
("btoa",9)
("atob",10)
("btoa",10)
(Just (B 1),[A 1,A 2,A 3],[B 4,B 5,B 6],[A 7,A 8,A 9,A 10])
```

As described above, in pipes-parse you need to convert the entire stream. In
our example, the conversion is trivial, and therefore not too worrying. But in
the case of either an expensive conversion, or a possibly-failing conversion,
this behavior would be incredibly problematic.

__UPDATE__: Davorak on Reddit helped me [come up with a better
example](http://www.reddit.com/r/haskell/comments/1xmmtn/some_ideas_for_pipesparse/cfcr1jq)
which demonstrates not just doubled encoding, but a program not completing
correctly. The conduit/pipes comparison code is [available as a
Gist](https://gist.github.com/snoyberg/8943391).

So my second recommendation would be to tweak `Parser` to have a stack of
leftovers in addition to a `Producer`, which would allow for more powerful
leftover preserving 

## Drop lenses

If you take the two recommendations above, you'll end up with a large
collection of pipes-parse-ready functions, like `take` and `takeWhile`. And
each of these functions will maintain a stack of leftovers, either to be used
by the next monadically-composed `Parser`, or to perhaps propagate upstream. At
this point, the lens-based approach to leftovers is overkill.

The problem with the lens approach is twofold. Firstly, it's difficult for
people to understand. The core concept is not complex, but the machinery around
it obscures its simplicity. But more importantly, it does not seem to compose
well. I don't mean that in the sense of lens or function composition: clearly
those work as expected. What I mean is that you won't be able to take the
existing utility functions I mentioned above and automatically use them in a
leftover-propagating manner.

I'd recommend borrowing from the conduit solution here directly: just have a
separate composition function or operator that returns the downstream
leftovers. It's a simple concept, it's easy to show in a type signature, and
it's easy to build more complex solutions on top of it.

I'll even argue that Gabriel's announcement post is in line with this
recommendation: as pointed out, the lens laws are being bent by pipes-parse. If
there's a separate solution that requires no law bending, why not use that?

Let me be clear: I'm not at all implying that lenses themselves are the problem
here. I think the problem is treating leftover propagation as an isomorphism
between two producers. I think lenses can actually play very nicely with a
streaming data package. I've been working on some conduit data analysis
libraries that make heavy usage of lens to let users write much simpler code
(things like `filterField stockDate (< today)`).

So to reiterate my third recommendation: provide a composition operator for
dealing with leftovers explicitly, let users reuse your existing library of
functions, and don't force them to try to write isomorphisms on entire streams
where a simple predicate function will suffice.

I hope these ideas come across the right way: ideas that I think would improve
the pipes ecosystem. These ideas should be taken with a large grain of salt.
They are strongly inspired by my experience with conduit, and with the kinds of
work I tend to see conduit applied to.

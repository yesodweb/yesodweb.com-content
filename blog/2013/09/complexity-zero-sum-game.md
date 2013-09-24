Consider the following truly ridiculous code:

```haskell
import Safe

main = do
    case readMay "1" of
        Nothing -> putStrLn "Invalid integer"
        Just i ->
            case readMay "2" of
                Nothing -> putStrLn "Invalid integer"
                Just j -> print $ i + j
```

This is a long, convoluted, error-prone way of adding the numbers 1 and 2. No
programmer should ever write code like this. I feel quite comfortable in saying
that the following code is absolutely better in every way than the previous
code:

```haskell
main = print $ 1 + 2
```

Going from the first to the second example is an exercise in reducing
complexity. A huge part of our job as programmers is trying to figure out ways
to reduce complexity in our codebases. Better abstractions, powerful libraries,
and great languages are all techniques we try and use to reduce the complexity.

Very rarely will we see an example as clear-cut as the one above. Often times
there will be some kind of a trade-off between two approaches. Other times, the
complexity will truly be unwarranted, but simplifying will just be a hard
problem to solve.

However, in some cases, none of this will be true. Some solutions may be
complex, but are complex because the problem itself really does have that level
of complexity inherent to it. After we've implemented every simplification we
can find, we'll still be left with a complex solution. And this is when we
enter the complexity [zero sum
game](http://en.wikipedia.org/wiki/Zero-sum_game). At this point, all we can do
is shuffle complexity from one part of the system to another. We can make good
arguments for why the complexity should be placed at one point or another, but
ultimately the complexity is there.

Almost two years ago, pipes was released, and touted as a simpler alternative
to enumerator and conduit. I claimed at the time that it was a baseless
comparison, since pipes didn't have most of the functionality of those two
libraries. Nonetheless, the comparisons were made. I've since then asked many
times to see the claims backed up. Finally, with the release of pipes-parse and
pipes-bytestring, there's enough of a basis for comparison to be made. And I'm
going to claim that, while the core of pipes may be simpler than the core of
conduit, the complexity has merely been shuffled, and shuffled in a very
pessimistic manner.

I'll step through a few of the features which conduit has elected to bake into
its core, which have to some extent complicated the core. And then I'd like to
compare those with the pipes approach of adding them on afterwards.

I'd like to make one thing clear before getting started. I have huge respect
for pipes, and the work Gabriel has done on it. pipes itself has had a huge
influence on conduit's development, and without pipes I doubt conduit would
be nearly as elegant as it is today. The purpose of this blog post is to
point out some important differences in the two libraries, and places where
I believe the pipes solution is lacking.

## Deterministic resource handling

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
from January. By keeping finalizers in the core datatype, conduit is able to
ensure prompt resource finalization, at the expense of losing strict
associativity. pipes has elected instead for a simpler core and strict
associativity, with the result of some fairly surprising behavior.

So assuming that prompt finalization is actually important to you, how do you
achieve this behavior with pipes? You could restructure your program a bit and
run two different pipelines. That's fine in this case, of a simple pipeline
writing to stdout. But as the program becomes more complicated, rewriting it in
such a way as to ensure the finalizer is called immediately may become
prohibitively difficult.

Point being: there's certainly complexity involved in prompt finalization. But
punting on this as pipes has done doesn't solve the complexity, it merely
pushes it off as a concern for the user. In conduit, the library itself handles
the complexity, so it doesn't become a user concern, with the downside that
composition is not associative with regards to ordering of finalizers.

## Chunking and leftovers

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
the API documentation calls them.

Gabriel claims this can be encapsulated nicely via a `StateT` as is done in
pipes-parse. This may be true, and it's certainly true that everything can be
implemented with this abstraction. But now users are required to hop between
two or three different ecosystems of functions in order to implement anything
involving chunked data.

### Simple parsing

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

## Folding

Let's go back to basics, and implement a very simplistic left fold on lists:

```haskell
foldl _ a [] = a
foldl f a (b:bs) = foldl f (f a b) bs
```

It has to handle to cases: if the list is empty, return the accumulator,
otherwise get a new accumulator and recurse. Great. Can we do the same thing
with conduit?

```haskell
foldl :: Monad m => (a -> b -> a) -> a -> Sink b m a
foldl f a = do
    mb <- await
    case mb of
        Nothing -> return a
        Just b -> foldl f (f a b)
```

It's a bit wordier, since we need to perform a monadic action to get the next
value instead of using simple pattern matching, but the algorithm is the same.
Using this let's us leverage all of our standard monadic composition and
fusion. For example, to skip the first 5 numbers in the stream and sum up the
following 5, you would write:

```haskell
res <- mapM_ yield [1..20] $$ do
    drop 5
    isolate 5 =$ foldl (+) 0
print res
```

Let's compare this to the (slightly simplified) type of `fold` in pipes:

```haskell
fold :: Monad m => (b -> a -> b) -> b -> Producer a m () -> m b
```

That type is decidedly different. Instead of our conduit `fold`, which provided
a `Sink` which could use normal composition, `fold` from pipes is using the
same trick we saw previously of taking an explicit `Producer` and producing a
single value. This defeats all of our standard composition abilities. I think
the canonical way to reimplement my above code would look something like:

```
FIXME i have no idea
```

~It gets even scarier when you look at the implementation of `fold`:~. Never mind, the implementation of `fold` uses explicit constructors just for an optimization. It seems like the right way to implement this `fold` is via:

```haskell
fold f a p = do
    eb <- next p
    case eb of
        Left _ -> return a
        Right (b, p') -> fold f (f a b) p'
```

That now looks pretty similar to the conduit version, the difference being `next`. The issue is that `await` in pipes cannot indicate the termination of the stream. I know there are reasons for this choice, but it seems to me like the complexity knob, once again, was set to the wrong calibration.

* fold can't be implemented in the higher-level API
* No need for ~> composition, we've just got monads

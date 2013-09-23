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

Uh-oh. The input file is kept open during the entire long running computation!
This problem is identified in the [pipes-safe release
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

(You can inflate the number 20 to something much larger to make the need for
streaming data more realistic.)

Let's imagine that the first chunk of data that is read from input.txt is 60
bytes large. The first call to `isolate` will read that chunk, split it into 20
and 40 byte chunks, send the 20 byte piece off to output1.txt, and return the
remaining 40 bytes as leftovers. The second call to `isolate` can then read
that chunk in and repeat the process.

It's hard for me to imagine 

* Use takeWhile

## await can fail

* fold can't be implemented in the higher-level API
* No need for ~> composition, we've just got monads

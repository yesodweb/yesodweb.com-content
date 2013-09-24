This blog post is in response to a post by Gabriel Gonzalez on [perfect
streaming](http://www.haskellforall.com/2013/09/perfect-streaming-using-pipes-bytestring.html).
The issue he raises is a good one: how to do efficient processing of chunks of
data, where each chunk may contain a large amount of data that should be
streamed.

While I appreciate the stated problem, I dislike the given solution. Gabriel's
approach is to invert the entirety of the stream processing, forcing the user
to have to explicitly pull data out of a producer instead of the more usual,
declarative semantics most of the streaming data libraries (enumerator,
conduit, and pipes) adhere to. (In fact, this critique is not limited to the
specific technique employed here, but to the general approach to chunked data
employed by pipes-bytestring and pipes-parse. I'm writing up a more detailed
blog post on that subject separately.)

Nonetheless, Gabriel's criticism of the simplistic approach the `lines`
function takes in `conduit` is valid. While the function is convenient for many
common use cases, it is not robust enough for data with large lines. But
instead of inverting the entirety of the streaming library to fix the problem,
I'd rather just reuse a more common idiom in Haskell: the fold.

If you want to just jump in, the full code is [available on School of
Haskell](https://www.fpcomplete.com/user/snoyberg/random-code-snippets/folding-lines-in-conduit).
There are some auxilary functions in there that should really be in
Data.Conduit.Text itself. But let's focus on the `foldLines` function:

```haskell
foldLines :: Monad m
          => (a -> ConduitM Text o m a)
          -> a
          -> ConduitM Text o m a
foldLines f =
    start
  where
    start a = CL.peek >>= maybe (return a) (const $ loop $ f a)

    loop consumer = do
        a <- takeWhileText (/= '\n') =$= do
            a <- consumer
            CL.sinkNull
            return a
        dropText 1
        start a
```

The logic here is pretty straight-forward, let's just step through it. The user
provides an initial value for the accumulator, which is initially passed to the
`start` helper function. `start` peeks at the input stream; if there's no data
in the stream, we immediately return the accumulator.  If there is data in the
stream, `peek` ensures that the data will be put back on the stream. We use
`const` to ignore that initial chunk (we'll consume it in a moment), and pass
the accumulator to the user supplied function to get the user's consuming
function.

`loop` is able to use all the glory of standard `conduit` composition. We use
the `takeWhileText` function to stream everything up to the first newline
character to the user's consumer. `sinkNull` is employed to ensure the entire
input is consumed, regardless of the behavior of the user function. Notice also
that the user is unable to accidentally consume more content than the current
line.  After all of that, we return the new accumulator value, use `dropText`
to drop the newline character on the stream, and then we start again with the
new accumulator.

To demonstrate usage of this function, I've included a small function for
getting the line number and character count for all the lines in a file.

```haskell
type LineNumber = Int
type CharCount = Int
data LineStat = LineStat !LineNumber !CharCount

myFunc :: Monad m => LineNumber -> ConduitM Text LineStat m LineNumber
myFunc number' = do
    count <- CL.fold (\count t -> count + T.length t) 0
    yield $ LineStat number count
    return number
  where
    number = number' + 1

showLineStat :: LineStat -> Text
showLineStat (LineStat number count) = T.concat
    [ "Line number "
    , T.pack $ show number
    , " has character count of: "
    , T.pack $ show count
    ]

main :: IO ()
main = runResourceT
     $ CB.sourceFile "input.txt"
    $$ CT.decode CT.utf8
    =$ void (foldLines myFunc 0)
    =$ CL.map showLineStat
    =$ unlinesText
    =$ CT.encode CT.utf8
    =$ CB.sinkHandle stdout
```

`myFunc` is able to use all the standard conduit machinery to do its work. It
uses a standard `fold` to sum up the length of each line, and then `yield`s a
`LineStat` for each line. For this kind of usage, it would probably make sense
to include some kind of a `scan`-like function, but folding works fine.

`main` itself is a bit verbose, since it has to deal with character
encoding/decoding issues. (Tangentially, I've considered adding file read/write
support to Data.Conduit.Text, but don't like adding implicit character
encoding/decoding. I'd be interested on hearing people's thoughts on this.)

Bonus points: if you feel like optimizing this, try using
`blaze-builder-conduit` and turn `showLineStat` into a function of type
`LineStat -> Builder`.

I think this style of solution is far preferable to having to reinvent the
wheel to deal with chunked data. Unfortunately, this style of coding seems to
be out of reach for pipes. To understand why, let's look at the implementation
of `takeWhileText`, the powerhouse underneath `foldLines`:

```haskell
takeWhileText :: Monad m
              => (Char -> Bool)
              -> Conduit Text m Text
takeWhileText p =
    loop
  where
    loop = await >>= maybe (return ()) go
    go t =
        case T.span p t of
            (x, y)
                | T.null y -> yield x >> loop
                | otherwise -> yield x >> leftover y
```

The logic is pretty simple. Loop over the input. If there is no input, we're
done. If there is an input chunk available, split it based on the provided
predicate. If the second part (`y`) is null, it means that the entire chunk
matched the predicate, so we should yield the chunk and continue looping.

If `y` is not null, then we simply yield the piece that *did* match (`x`) and
then return the rest as leftovers, to be consumed later. To me, this is the
most natural way to express the algorithm. However, pipes does not provide
leftover support, and therefore has to invert streaming data to instead be
explicit pulling from a producer. As I mentioned before, I'll go into more
details in a later blog post on the problems with this approach.

Gabriel's `FreeT` trick is interesting, and certainly clever. Given the
constraints of pipes, it seems like a good solution to the problem. However,
let's not confuse a constrained good solution with the ideal solution. In my
opinion, being able to reuse commonly used abstractions is far preferable to
needing to invent lots of clever hacks.

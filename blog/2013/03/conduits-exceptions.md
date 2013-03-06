Congratulations to the Snap team on the recent [first release of
io-streams](http://snapframework.com/blog/2013/03/05/announcing-io-streams).
I've [briefly touched on](/blog/2013/02/upcoming-conduit-1-0#conduit-niche) my
thoughts on io-streams, but that's not my purpose here. I've seen that one
statement in the io-streams release post has worried some people with its
implication, so I wanted to address it here.  In reference to conduit (and
iteratee/pipes), the blog post states:

> their use of continuation-passing style interacts badly with asynchronous exceptions

I realize this is just a FAQ section in a blog post, but I think it's only
right that if you make claims that another library is failing to handle some
use case, you demonstrate this. I would like to make it clear to all conduit
users out there: conduit was specifically designed to be fully exception safe,
and to my knowledge there has not yet been any hole found in conduit's
exception safety, neither for synchronous nor asynchronous exceptions.

My guess as to what Gregory means in this blog post is the fact that conduit
relies on resourcet for exception safety, which Gregory has previously stated
is inferior to the bracket pattern. I would like to demonstrate that, in fact,
neither of those clauses is factually accurate. If the blog post means
something else with bad interaction with async exceptions, I'd be happy to hear
an explanation.

## Exception safety without resourcet

Let's first get this out of the way: you can write fully exception safe code in
`conduit` using the standard bracket pattern and never touching resourcet:

```haskell
import           Data.Conduit        (($$))
import           Data.Conduit.Binary (sinkHandle, sourceHandle)
import           System.IO           (IOMode (ReadMode, WriteMode), withFile)

main = withFile "input.txt" ReadMode $ \input ->
       withFile "output.txt" WriteMode $ \output ->
       sourceHandle input $$ sinkHandle output
```

I don't think this point deserves any more illustration. So for everyone's
future reference: there's no point claiming that conduit requires resourcet.

## resourcet is more flexible than bracket

My next point is that resourcet allows some use cases which are impossible with
the bracket pattern alone. In particular, bracket requires properly nested
resource allocations. This doesn't allow for interleaving of resource
allocation and freeing.

Let's take a simple case: you have a number of uncompressed files sitting in an
input folder. You want to gzip-compress the contents of all of these files,
split them into chunks of equal size, and write them to separate files in an
output folder. For efficiency, you should never have more than two files open
at a single time (one for reading, one for writing). And the program should be
fully exception safe: no matter what happens, the handles should all be closed
when the function exits.

This kind of thing is exactly what ResourceT was designed to solve. The problem
is trivial to address with conduit + ResourceT:

```haskell
-- Monad instance of Source allows us to simply mapM_ to create a single Source
-- for reading all of the files sequentially.
source = mapM_ sourceFileTrace infiles

-- The Sink is a bit more complicated: we keep reading 30kb chunks of data into
-- new files. We then use peek to check if there is any data left in the
-- stream. If there is, we continue the process.
sink =
    loop 1
  where
    loop i = do
        isolate (30 * 1024) =$ sinkFileTrace (outfile i)
        mx <- peek
        case mx of
            Nothing -> return ()
            Just _ -> loop (i + 1)

-- Putting it all together is trivial. ResourceT guarantees we have exception
-- safety.
transform = runResourceT $ source $$ gzip =$ sink
```

If you'd like to play with that a bit more, you can [view it on School of
Haskell](https://www.fpcomplete.com/user/snoyberg/random-code-snippets/interleaved-resourcet).

I hope this demonstrates clearly that, instead of being some ugly hack,
ResourceT is a powerful tool which allows us to safely write code that does not
fit in the bracket pattern.

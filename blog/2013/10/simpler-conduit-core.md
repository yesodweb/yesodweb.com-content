In my last blog post, I made the case that automatic termination is the cause
of a lot of problems in both pipes and conduit. In this blog post, I'd like to:

1. Derive a simple core datatype that does not have automatic termination.

2. Discuss the desired behavior of this datatype.

3. Assess what convenience functionality we lost along the way, and see if we
   can get it back with an added layer on top of the core.

## Deriving the core datatype

I want to start as simple as possible. Let's begin with the core datatype from
pipes 1.0. This leaves out any extra complexity we don't want to deal with in
this process, like leftovers, finalizers, or bidirectionality.

```haskell
data Pipe i o m r
    = Pure r
    | M (m (Pipe i o m r))
    | Yield (Pipe i o m r) o
    | Await (i -> Pipe i o m r)
```

Quick recap: `M` is for performing monadic actions. `Yield` passes a value
downstream, and gives up control of the flow of execution to downstream. If
downstream meanwhile returns `Pure`, the current `Pipe` will never get control
of execution again. Similarly, `Await` asks for a value from upstream, and will
similarly terminate of upstream returns `Pure`.

Let's implement the identity pipe in terms of the raw constructors:

```haskell
idP :: Monad m => Pipe i i m r
idP = Await (Yield idP)
```

We'll also need some kind of function to compose two pipes together. I'm not
going to include all of that code inline, but you can [view a Gist with a
simple
example](https://gist.github.com/snoyberg/6806404/034c1a25466d5bf162df225a5d2864f4e0992504).
I do want to point out its type signature:

```haskell
fuse :: Monad m
     => Pipe a b m r
     -> Pipe b c m r
     -> Pipe a c m r
```

In this blog post, I want to modify the core datatype to allow for
non-termination. We'll start with the `Await` side of the equation, and allow
the identity pipe guide our design for the rest of the other constructors.
(Note: we're going to target simplicity here, not efficiency. Efficiency can be
addressed another time.)

The simplest way to allow for non-termination is to modify `Await` to include a
`Maybe` wrapper:

    Await (Maybe i -> Pipe i o m r)

This means that, when awaiting for a response from upstream, we can be informed
via `Nothing` that no values are available. However, our `idP` no longer
compiles. That's because we're getting a `Maybe i`, but `Yield` expects an `i`.
Let's fix this by modifying our `Yield` constructor also:

    Yield (Pipe i o m r) (Maybe o)

Now our original `idP` continues to compile. You can see an [updated
Gist](https://gist.github.com/snoyberg/6806404/da2656a1a4f6e56716014068698a5488910540e3)
if you're curious. This change now allows us to write a fold, e.g.:

```haskell
fold :: Monad m => (r -> i -> r) -> r -> Pipe i o m r
fold f =
    loop
  where
    loop r = Await $ \mi ->
        case mi of
            Just i -> loop $! f r i
            Nothing -> Pure r
```

However, we still haven't actually *fixed* the non-termination problem. As soon as one `Pipe` terminates, the whole `Pipe` terminates. The practical issue is that prompt termination is still not achieved. Consider the following producer:

```haskell
yieldMany :: [o] -> Pipe i o IO r
yieldMany [] = M (putStrLn "Finalization" >> return (Yield (yieldMany []) Nothing))
yieldMany (o:os) = Yield (yieldMany os) (Just o)
```

If the input stream it fully consumed, then `Finalization` is printed.
Otherwise, it's not. You can [see the example
yourself](https://gist.github.com/snoyberg/6806404/ed5fc84ecba65b850599fd6b9f27746d8ee1fbf0).



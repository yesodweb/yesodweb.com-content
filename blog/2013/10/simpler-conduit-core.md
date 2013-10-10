__NOTE__ I strongly recommend [reading this post on School of
Haskell](https://www.fpcomplete.com/user/snoyberg/blog-posts/simpler-conduit-core).
It makes extensive usage of active code.

In [my last blog post](http://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit), I made the case that automatic termination is the cause
of a lot of problems in both pipes and conduit. In this blog post, I'd like to:

1. Derive a simple core datatype that does not have automatic termination.

2. Discuss the desired behavior of this datatype.

3. Assess what convenience functionality we lost along the way, and see if we
   can get it back with an added layer on top of the core.

Note that this blog post is meant to demonstrate an idea; it's not to be taken as a working implementation. There are many details that still need to be worked out. I'm hoping that presenting my thoughts in this manner will give enough of a basis for an informed discussion.

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

We'll also need some kind of function to compose two pipes together. The type for this function is:

```haskell
fuse :: Monad m
     => Pipe a b m r
     -> Pipe b c m r
     -> Pipe a c m r
```

You can experiment with some sample code here:

```haskell active
import Control.Monad

data Pipe i o m r
    = Pure r
    | M (m (Pipe i o m r))
    | Yield (Pipe i o m r) o
    | Await (i -> Pipe i o m r)

fuse :: Monad m
     => Pipe a b m r
     -> Pipe b c m r
     -> Pipe a c m r
fuse _ (Pure r) = Pure r
fuse up (M m) = M (liftM (fuse up) m)
fuse up (Yield down o) = Yield (fuse up down) o
fuse up0 (Await down) =
    go up0
  where
    go (Pure r) = Pure r
    go (M m) = M (liftM go m)
    go (Yield up b) = fuse up (down b)
    go (Await up) = Await (go . up)

(>->) = fuse

idP :: Monad m => Pipe i i m r
idP = Await (Yield idP)

consume :: Monad m => Int -> Pipe i o m [i]
consume =
    go id
  where
    go front 0 = Pure (front [])
    go front count = Await $ \i -> go (front . (i:)) (count - 1)

yieldMany :: Monad m => [o] -> Pipe i o m r
yieldMany [] = error "FIXME"
yieldMany (o:os) = Yield (yieldMany os) o

runPipe :: Monad m => Pipe () () m r -> m r
runPipe (Pure r) = return r
runPipe (M m) = m >>= runPipe
runPipe (Await f) = runPipe (f ())
runPipe (Yield f ()) = runPipe f

main = runPipe (yieldMany [1..] >-> idP >-> consume 10) >>= print
```

In this blog post, I want to modify the core datatype to allow for
non-termination. We'll start with the `Await` side of the equation, and allow
the identity pipe guide our design for the rest of the other constructors.
(Note: we're going to target simplicity here, not efficiency. Efficiency can be
addressed another time.)

## Await and Yield: add Maybe

The simplest way to allow for non-termination is to modify `Await` to include a
`Maybe` wrapper:

    Await (Maybe i -> Pipe i o m r)

This means that, when awaiting for a response from upstream, we can be informed
via `Nothing` that no values are available. However, our `idP` no longer
compiles. That's because we're getting a `Maybe i`, but `Yield` expects an `i`.
Let's fix this by modifying our `Yield` constructor also:

    Yield (Pipe i o m r) (Maybe o)

Now our original `idP` continues to compile. This change now allows us to write a fold, e.g.:

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

## Finalization

However, we still haven't actually *fixed* the non-termination problem. As soon as one `Pipe` terminates, the whole `Pipe` terminates. The practical issue is that prompt termination is still not achieved. Try out the following example:

```haskell active
import Control.Monad

data Pipe i o m r
    = Pure r
    | M (m (Pipe i o m r))
    | Yield (Pipe i o m r) (Maybe o)
    | Await (Maybe i -> Pipe i o m r)

fuse :: Monad m
     => Pipe a b m r
     -> Pipe b c m r
     -> Pipe a c m r
fuse _ (Pure r) = Pure r
fuse up (M m) = M (liftM (fuse up) m)
fuse up (Yield down o) = Yield (fuse up down) o
fuse up0 (Await down) =
    go up0
  where
    go (Pure r) = Pure r
    go (M m) = M (liftM go m)
    go (Yield up b) = fuse up (down b)
    go (Await up) = Await (go . up)

(>->) :: Monad m
     => Pipe a b m r
     -> Pipe b c m r
     -> Pipe a c m r
(>->) = fuse

idP :: Monad m => Pipe i i m r
idP = Await (Yield idP)

consume :: Monad m => Int -> Pipe i o m [i]
consume =
    go id
  where
    go front 0 = Pure (front [])
    go front count = Await $ \mi ->
        case mi of
            Just i -> go (front . (i:)) (count - 1)
            Nothing -> Pure (front [])
-- show Data producer with finalization
yieldMany :: [o] -> Pipe i o IO r
yieldMany [] = M (putStrLn "Finalization" >> return (Yield (yieldMany []) Nothing))
yieldMany (o:os) = Yield (yieldMany os) (Just o)
-- /show

fold :: Monad m => (r -> i -> r) -> r -> Pipe i o m r
fold f =
    loop
  where
    loop r = Await $ \mi ->
        case mi of
            Just i -> loop $! f r i
            Nothing -> Pure r

runPipe :: Monad m => Pipe () () m r -> m r
runPipe (Pure r) = return r
runPipe (M m) = m >>= runPipe
runPipe (Await f) = runPipe (f Nothing)
runPipe (Yield f _) = runPipe f

-- show When does the finalizer get called?
main = do
    runPipe (yieldMany [1..10] >-> idP >-> fold (+) 0) >>= print
    runPipe (yieldMany [1..10] >-> idP >-> consume 5) >>= print
-- /show
```

If the input stream it fully consumed, then `Finalization` is printed.
Otherwise, it's not. We need to change our semantics so that we don't exit until *all* `Pipe`s exit.

Downstream is already notified when upstream is done producing data, via `Nothing` getting passed with `Yield`. We need a similar mechanism on the upstream side. In this case, we want it to be a `Maybe` result value. This value needs to be present as soon as the `Pipe` begins execution. To allow for this, we're going to refactor our type a bit as follows:

```haskell
data Step i o m r
    = Pure r
    | M (m (Step i o m r))
    | Yield (Pipe i o m r) (Maybe o)
    | Await (Maybe i -> Step i o m r)

type Pipe i o m r = Maybe r -> Step i o m r
```

Now a `Pipe` is notified if downstream has already completed execution. I'd like to focus on one important distinction in the `Step` type's constructors: whereas `M` and `Await` represent the next thing to be done via a `Step` value, `Yield` represents it with a `Pipe` value. The reason for this distinction is that, when you call `Yield`, downstream has a chance to continue processing, and may provide a return value if it hadn't provided one previously. In the `M` and `Await` cases, downstream never gets a chance to provide a new result value.

Let's think about the identity pipe. Its semantics should be that, if downstream is done processing, it's also done processing. If downstream is not done processing, it should await for a new value from upstream and yield it downstream. This turns out to be pretty easy to implement:

```haskell
idP :: Monad m => Pipe i i m r
idP Nothing = Await (Yield idP)
idP (Just r) = Pure r
```

Below is our full running example.

```haskell active
import Control.Monad

data Step i o m r
    = Pure r
    | M (m (Step i o m r))
    | Yield (Pipe i o m r) (Maybe o)
    | Await (Maybe i -> Step i o m r)

type Pipe i o m r = Maybe r -> Step i o m r

-- show Fusion is a bit more complicated now
fuse :: Monad m
     => Pipe a b m r
     -> Pipe b c m r
     -> Pipe a c m r
fuse up down mr = fuse' up (down mr)

fuse' :: Monad m
      => (Maybe r -> Step a b m r)
      -> Step b c m r
      -> Step a c m r
fuse' up0 (Pure r) =
    go $ up0 $ Just r
  where
    go (Pure r') = Pure r'
    go (M m) = M (liftM go m)
    go (Yield up _) = go $ up $ Just r
    go (Await up) = Await $ \ma -> go $ up ma
fuse' up (M m) = M (liftM (fuse' up) m)
fuse' up (Yield down o) = Yield (fuse up down) o
fuse' up0 (Await down) =
    go $ up0 Nothing
  where
    -- It's easy to get this next clause wrong.
    -- We need to make sure that we give downstream
    -- a chance to finish processing, not just terminate
    -- immediately.
    go (Pure r) = fuse' (\_ -> Pure r) (down Nothing)
    go (M m) = M (liftM go m)
    go (Yield up b) = fuse' up (down b)
    go (Await up) = Await (go . up)
-- /show

(>->) :: Monad m
     => Pipe a b m r
     -> Pipe b c m r
     -> Pipe a c m r
(>->) = fuse

idP :: Monad m => Pipe i i m r
idP Nothing = Await (Yield idP)
idP (Just r) = Pure r

-- show Consumers can just ignore downstream results
consume :: Monad m => Int -> Pipe i o m [i]
consume count0 _ =
    go id count0
  where
    go front 0 = Pure (front [])
    go front count = Await $ \mi ->
        case mi of
            Just i -> go (front . (i:)) (count - 1)
            Nothing -> Pure (front [])
-- /show

-- show Finalization
-- Note that we only finalize once downstream completes.
-- We could instead finalize as soon as our input is empty,
-- which would allow for more promptness. Try implementing that
-- change. Make sure that the finalizer only gets called once!
yieldMany :: [o] -> Pipe i o IO r
yieldMany _ (Just r) = M (putStrLn "Finalization" >> return (Pure r))
yieldMany [] Nothing = Yield (yieldMany []) Nothing
yieldMany (o:os) Nothing = Yield (yieldMany os) (Just o)
-- /show

fold :: Monad m => (r -> i -> r) -> r -> Pipe i o m r
fold f r0 _ =
    loop r0
  where
    loop r = Await $ \mi ->
        case mi of
            Just i -> loop $! f r i
            Nothing -> Pure r

runPipe :: Monad m => Pipe () () m r -> m r
runPipe f = runStep (f Nothing)

runStep :: Monad m => Step () () m r -> m r
runStep (Pure r) = return r
runStep (M m) = m >>= runStep
runStep (Await f) = runStep (f Nothing)
runStep (Yield f _) = runPipe f

main = do
    runPipe (yieldMany [1..10] >-> idP >-> fold (+) 0) >>= print
    runPipe (yieldMany [1..10] >-> idP >-> consume 5) >>= print
```

That's it, we've moved from automatic termination to manual termination. We can now fold, get prompt finalization, and even modify result values from downstream. Let's play with a few more changes.

## Draining upstream

Consider writing a `Pipe` that takes precisely 5 values from upstream and passes them downstream. If downstream finishes early, it still takes those 5 values. This is not currently possible in pipes or conduit. Let's try it out in our new framework:

```haskell active
import Control.Monad

data Step i o m r
    = Pure r
    | M (m (Step i o m r))
    | Yield (Pipe i o m r) (Maybe o)
    | Await (Maybe i -> Step i o m r)

type Pipe i o m r = Maybe r -> Step i o m r

fuse :: Monad m
     => Pipe a b m r
     -> Pipe b c m r
     -> Pipe a c m r
fuse up down mr = fuse' up (down mr)

fuse' :: Monad m
      => (Maybe r -> Step a b m r)
      -> Step b c m r
      -> Step a c m r
fuse' up0 (Pure r) =
    go $ up0 $ Just r
  where
    go (Pure r') = Pure r'
    go (M m) = M (liftM go m)
    go (Yield up _) = go $ up $ Just r
    go (Await up) = Await $ \ma -> go $ up ma
fuse' up (M m) = M (liftM (fuse' up) m)
fuse' up (Yield down o) = Yield (fuse up down) o
fuse' up0 (Await down) =
    go $ up0 Nothing
  where
    go (Pure r) = fuse' (\_ -> Pure r) (down Nothing)
    go (M m) = M (liftM go m)
    go (Yield up b) = fuse' up (down b)
    go (Await up) = Await (go . up)

(>->) :: Monad m
     => Pipe a b m r
     -> Pipe b c m r
     -> Pipe a c m r
(>->) = fuse

idP :: Monad m => Pipe i i m r
idP Nothing = Await (Yield idP)
idP (Just r) = Pure r

consume :: Monad m => Int -> Pipe i o m [i]
consume count0 _ =
    go id count0
  where
    go front 0 = Pure (front [])
    go front count = Await $ \mi ->
        case mi of
            Just i -> go (front . (i:)) (count - 1)
            Nothing -> Pure (front [])

fold :: Monad m => (r -> i -> r) -> r -> Pipe i o m r
fold f r0 _ =
    loop r0
  where
    loop r = Await $ \mi ->
        case mi of
            Just i -> loop $! f r i
            Nothing -> Pure r

runPipe :: Monad m => Pipe () () m r -> m r
runPipe f = runStep (f Nothing)

-- show
takeExactly :: Monad m => Int -> Pipe i i m r
takeExactly 0 (Just r) = Pure r
takeExactly 0 Nothing = Yield (takeExactly 0) Nothing
takeExactly count _ = Await $ \mi -> Yield (takeExactly (count - 1)) mi
-- We can optimize the above a bit by skipping
-- extra Awaits, give it a shot.

yieldMany :: Show o => [o] -> Pipe i o IO r
yieldMany rest (Just r) = M $ do
    putStrLn $ "Finalization: " ++ show rest
    return (Pure r)
yieldMany [] Nothing = Yield (yieldMany []) Nothing
yieldMany (o:os) Nothing = Yield (yieldMany os) (Just o)

main = runPipe (yieldMany [1..10] >-> takeExactly 5 >-> fold (+) 0) >>= print
-- /show

runStep :: Monad m => Step () () m r -> m r
runStep (Pure r) = return r
runStep (M m) = m >>= runStep
runStep (Await f) = runStep (f Nothing)
runStep (Yield f _) = runPipe f

```

## Downstream result type

There's a nice generalization we can provide here. There's no reason that the upstream and downstream result types need to be the same. In fact, if we just add an extra type parameter (`d` for __d__ownstream result), the rest of our code can remain the same. Here's an example:

```haskell active
import Control.Monad

-- show
data Step i o d m r
    = Pure r
    | M (m (Step i o d m r))
    | Yield (Pipe i o d m r) (Maybe o)
    | Await (Maybe i -> Step i o d m r)

type Pipe i o d m r = Maybe d -> Step i o d m r

-- Notice the way that a->b->c flows downstream.
-- On the other hand, the result values x->y->z
-- "bubble up" from downstream.
fuse :: Monad m
     => Pipe a b y m z
     -> Pipe b c x m y
     -> Pipe a c x m z
-- /show
fuse up down mr = fuse' up (down mr)

fuse' :: Monad m
      => (Maybe y -> Step a b y m z)
      -> Step b c x m y
      -> Step a c x m z
fuse' up0 (Pure r) =
    go $ up0 $ Just r
  where
    go (Pure r') = Pure r'
    go (M m) = M (liftM go m)
    go (Yield up _) = go $ up $ Just r
    go (Await up) = Await $ \ma -> go $ up ma
fuse' up (M m) = M (liftM (fuse' up) m)
fuse' up (Yield down o) = Yield (fuse up down) o
fuse' up0 (Await down) =
    go $ up0 Nothing
  where
    go (Pure r) = fuse' (\_ -> Pure r) (down Nothing)
    go (M m) = M (liftM go m)
    go (Yield up b) = fuse' up (down b)
    go (Await up) = Await (go . up)

(>->) :: Monad m
     => Pipe a b y m z
     -> Pipe b c x m y
     -> Pipe a c x m z
(>->) = fuse

-- show
-- Identity keeps the same stream value (i) and result
-- value (r) for both upstream and downstream.
idP :: Monad m => Pipe i i r m r
idP Nothing = Await (Yield idP)
idP (Just r) = Pure r
-- /show

-- show
-- Consumers can just ignore the downstream result.
consume :: Monad m => Int -> Pipe i o d m [i]
consume count0 _ =
    go id count0
  where
    go front 0 = Pure (front [])
    go front count = Await $ \mi ->
        case mi of
            Just i -> go (front . (i:)) (count - 1)
            Nothing -> Pure (front [])
-- /show

fold :: Monad m => (r -> i -> r) -> r -> Pipe i o d m r
fold f r0 _ =
    loop r0
  where
    loop r = Await $ \mi ->
        case mi of
            Just i -> loop $! f r i
            Nothing -> Pure r

runPipe :: Monad m => Pipe () () d m r -> m r
runPipe f = runStep (f Nothing)

runStep :: Monad m => Step () () d m r -> m r
runStep (Pure r) = return r
runStep (M m) = m >>= runStep
runStep (Await f) = runStep (f Nothing)
runStep (Yield f _) = runPipe f

-- show
-- Producers can simply return the downstream result
-- as its own result value.
takeExactly :: Monad m => Int -> Pipe i i r m r
takeExactly 0 (Just r) = Pure r
takeExactly 0 Nothing = Yield (takeExactly 0) Nothing
takeExactly count _ = Await $ \mi -> Yield (takeExactly (count - 1)) mi

yieldMany :: Show o => [o] -> Pipe i o r IO r
yieldMany rest (Just r) = M $ do
    putStrLn $ "Finalization: " ++ show rest
    return (Pure r)
yieldMany [] Nothing = Yield (yieldMany []) Nothing
yieldMany (o:os) Nothing = Yield (yieldMany os) (Just o)
-- /show

main = runPipe (yieldMany [1..10] >-> takeExactly 5 >-> fold (+) 0) >>= print
```

However, we can also use this functionality to modify the result type. For example, we may want to return any unused values from our input list in `yieldMany`. This would look like:

```haskell active
import Control.Monad

data Step i o d m r
    = Pure r
    | M (m (Step i o d m r))
    | Yield (Pipe i o d m r) (Maybe o)
    | Await (Maybe i -> Step i o d m r)

type Pipe i o d m r = Maybe d -> Step i o d m r

fuse :: Monad m
     => Pipe a b y m z
     -> Pipe b c x m y
     -> Pipe a c x m z
fuse up down mr = fuse' up (down mr)

fuse' :: Monad m
      => (Maybe y -> Step a b y m z)
      -> Step b c x m y
      -> Step a c x m z
fuse' up0 (Pure r) =
    go $ up0 $ Just r
  where
    go (Pure r') = Pure r'
    go (M m) = M (liftM go m)
    go (Yield up _) = go $ up $ Just r
    go (Await up) = Await $ \ma -> go $ up ma
fuse' up (M m) = M (liftM (fuse' up) m)
fuse' up (Yield down o) = Yield (fuse up down) o
fuse' up0 (Await down) =
    go $ up0 Nothing
  where
    go (Pure r) = fuse' (\_ -> Pure r) (down Nothing)
    go (M m) = M (liftM go m)
    go (Yield up b) = fuse' up (down b)
    go (Await up) = Await (go . up)

(>->) :: Monad m
     => Pipe a b y m z
     -> Pipe b c x m y
     -> Pipe a c x m z
(>->) = fuse

idP :: Monad m => Pipe i i r m r
idP Nothing = Await (Yield idP)
idP (Just r) = Pure r

consume :: Monad m => Int -> Pipe i o d m [i]
consume count0 _ =
    go id count0
  where
    go front 0 = Pure (front [])
    go front count = Await $ \mi ->
        case mi of
            Just i -> go (front . (i:)) (count - 1)
            Nothing -> Pure (front [])

fold :: Monad m => (r -> i -> r) -> r -> Pipe i o d m r
fold f r0 _ =
    loop r0
  where
    loop r = Await $ \mi ->
        case mi of
            Just i -> loop $! f r i
            Nothing -> Pure r

runPipe :: Monad m => Pipe () () d m r -> m r
runPipe f = runStep (f Nothing)

runStep :: Monad m => Step () () d m r -> m r
runStep (Pure r) = return r
runStep (M m) = m >>= runStep
runStep (Await f) = runStep (f Nothing)
runStep (Yield f _) = runPipe f

takeExactly :: Monad m => Int -> Pipe i i r m r
takeExactly 0 (Just r) = Pure r
takeExactly 0 Nothing = Yield (takeExactly 0) Nothing
takeExactly count _ = Await $ \mi -> Yield (takeExactly (count - 1)) mi

-- show
yieldMany :: Monad m => [o] -> Pipe i o r m ([o], r)
yieldMany rest (Just r) = Pure (rest, r)
yieldMany [] Nothing = Yield (yieldMany []) Nothing
yieldMany (o:os) Nothing = Yield (yieldMany os) (Just o)

main = runPipe (yieldMany [1..10] >-> takeExactly 5 >-> fold (+) 0) >>= print
-- /show
-- IGNORED
```

## Leftovers/chunked data

So I promised a solution to leftovers as well. Actually, we can base a solution on that preceding example. When we return a result, we should also return any unconsumed input with it. When we monadically compose two `Pipe`s, we want the leftovers from the first to be injected into the second to be used any time the second awaits. We haven't actually shown a `Monad` instance yet, and without creating a `newtype` in place of our type synonym, we can't. But let's play around with some `pipeReturn` and `pipeBind` functions:

```haskell active
import Control.Monad

-- show
data Step i o d m r
    = Pure [i] r
    | M (m (Step i o d m r))
    | Yield (Pipe i o d m r) (Maybe o)
    | Await (Maybe i -> Step i o d m r)

type Pipe i o d m r = Maybe ([o], d) -> Step i o d m r

pipeReturn :: Monad m => r -> Pipe i o d m r
pipeReturn r _downRes = Pure [] r

pipeBind :: Monad m
         => Pipe i o d m a
         -> (a -> Pipe i o d m b)
         -> Pipe i o d m b
pipeBind f g mr =
    go $ f mr
  where
    go (Pure is a) = inject is (g a) mr
    go (M m) = M (liftM go m)
    go (Yield p o) = Yield (pipeBind p g) o
    go (Await f) = Await (go . f)

inject :: Monad m => [i] -> Pipe i o d m r -> Pipe i o d m r
inject [] p = p
inject is0 p0 =
    go is0 . p0
  where
    go [] p = p
    go is (Pure is' r) = Pure (is' ++ is) r
    go is (M m) = M (liftM (go is) m)
    go is (Yield p o) = Yield (go is . p) o
    go (i:is) (Await p) = go is (p $ Just i)

peek :: Monad m => Pipe i o d m (Maybe i)
peek _ = Await $ \mi ->
    case mi of
        Nothing -> Pure [] Nothing
        Just i -> Pure [i] (Just i)

peekFold :: Monad m => (r -> i -> r) -> r -> Pipe i o d m (Maybe i, r)
peekFold f r = peek `pipeBind` \a -> fold f r `pipeBind` \b -> pipeReturn (a, b)

main = do
    runPipe (yieldMany [1..10] >-> peek) >>= print
    runPipe (yieldMany [1..10] >-> peekFold (+) 0) >>= print
-- /show

fuse :: Monad m
     => Pipe a b y m z
     -> Pipe b c x m y
     -> Pipe a c x m z
fuse up down mr = fuse' up (down mr)

fuse' :: Monad m
      => (Maybe ([b], y) -> Step a b y m z)
      -> Step b c x m y
      -> Step a c x m z
fuse' up0 (Pure cs z) =
    go $ up0 $ Just (cs, z)
  where
    go (Pure bs y) = Pure bs y
    go (M m) = M (liftM go m)
    go (Yield up _) = go $ up $ Just ([], z)
    go (Await up) = Await $ \ma -> go $ up ma
fuse' up (M m) = M (liftM (fuse' up) m)
fuse' up (Yield down o) = Yield (fuse up down) o
fuse' up0 (Await down) =
    go $ up0 Nothing
  where
    go (Pure bs y) = fuse' (\_ -> Pure bs y) (down Nothing)
    go (M m) = M (liftM go m)
    go (Yield up b) = fuse' up (down b)
    go (Await up) = Await (go . up)

(>->) :: Monad m
     => Pipe a b y m z
     -> Pipe b c x m y
     -> Pipe a c x m z
(>->) = fuse

idP :: Monad m => Pipe i i r m r
idP Nothing = Await (Yield idP)
idP (Just (is, r)) = Pure is r

consume :: Monad m => Int -> Pipe i o d m [i]
consume count0 _ =
    go id count0
  where
    go front 0 = Pure [] (front [])
    go front count = Await $ \mi ->
        case mi of
            Just i -> go (front . (i:)) (count - 1)
            Nothing -> Pure [] (front [])

fold :: Monad m => (r -> i -> r) -> r -> Pipe i o d m r
fold f r0 _ =
    loop r0
  where
    loop r = Await $ \mi ->
        case mi of
            Just i -> loop $! f r i
            Nothing -> Pure [] r

runPipe :: Monad m => Pipe () () d m r -> m r
runPipe f = runStep (f Nothing)

runStep :: Monad m => Step () () d m r -> m r
runStep (Pure _ r) = return r
runStep (M m) = m >>= runStep
runStep (Await f) = runStep (f Nothing)
runStep (Yield f _) = runPipe f

takeExactly :: Monad m => Int -> Pipe i i r m r
-- We specifically ignore leftovers, since we want to ensure
-- that we consumed exactly the given number of elements
-- from the stream.
takeExactly 0 (Just (_, r)) = Pure [] r
takeExactly 0 Nothing = Yield (takeExactly 0) Nothing
takeExactly count _ = Await $ \mi -> Yield (takeExactly (count - 1)) mi

yieldMany :: Monad m => [o] -> Pipe i o r m ([o], [o], r)
yieldMany rest (Just (os, r)) = Pure [] (rest, os, r)
yieldMany [] Nothing = Yield (yieldMany []) Nothing
yieldMany (o:os) Nothing = Yield (yieldMany os) (Just o)
```

## Early termination

So we've done all of this work to get rid of early termination. But in practice, it's often very convenient to have early termination. For example, a common pipes and conduit idiom to create a producer from an infinite list is:

    mapM_ yield [1..]

Without some kind of early termination, this will never exit. Fortunately, having early exit from a `Monad` is a solved problem. We could either layer something like `MaybeT` in our transformer stack, or add a new constructor to our `Step` datatype. There are a few complications introduced by this, notably that we have an extra type parameter (`t` for __t__ermination) which must be unified with our result type, but the approach works.

```haskell active
import Control.Monad
-- show
data Step i o d t m r
    = Pure [i] r
    | M (m (Step i o d t m r))
    | Yield (Pipe i o d t m r) (Maybe o)
    | Await (Maybe i -> Step i o d t m r)
    | Stop [i] t

type Pipe i o d t m r = Maybe ([o], d) -> Step i o d t m r

yield :: Monad m => o -> Pipe i o d d m ()
yield _ (Just (_, d)) = Stop [] d
yield o Nothing = Yield (pipeReturn ()) (Just o)

stop :: Monad m => Pipe i o d d m r
stop (Just (_, r)) = Stop [] r
stop Nothing = Yield stop Nothing

pipeMapM_ :: Monad m
          => (a -> Pipe i o d t m ())
          -> [a]
          -> Pipe i o d t m ()
pipeMapM_ _ [] = pipeReturn ()
pipeMapM_ f (x:xs) = f x `pipeBind` (const (pipeMapM_ f xs))

main :: IO ()
main = do
    let producer =
            pipeMapM_ yield [1..] `pipeBind`
            const stop
    runPipe (producer >-> takeExactly 10 >-> fold (+) 0)
        >>= print
-- /show
pipeReturn :: Monad m => r -> Pipe i o d t m r
pipeReturn r _downRes = Pure [] r

pipeBind :: Monad m
         => Pipe i o d t m a
         -> (a -> Pipe i o d t m b)
         -> Pipe i o d t m b
pipeBind f g mr =
    go $ f mr
  where
    go (Pure is a) = inject is (g a) mr
    go (M m) = M (liftM go m)
    go (Yield p o) = Yield (pipeBind p g) o
    go (Await p) = Await (go . p)
    go (Stop is t) = Stop is t

inject :: Monad m => [i] -> Pipe i o d t m r -> Pipe i o d t m r
inject [] p = p
inject is0 p0 =
    go is0 . p0
  where
    go [] p = p
    go is (Pure is' r) = Pure (is' ++ is) r
    go is (M m) = M (liftM (go is) m)
    go is (Yield p o) = Yield (go is . p) o
    go (i:is) (Await p) = go is (p $ Just i)
    go is (Stop is' r) = Stop (is' ++ is) r

peek :: Monad m => Pipe i o d t m (Maybe i)
peek _ = Await $ \mi ->
    case mi of
        Nothing -> Pure [] Nothing
        Just i -> Pure [i] (Just i)

peekFold :: Monad m => (r -> i -> r) -> r -> Pipe i o d t m (Maybe i, r)
peekFold f r = peek `pipeBind` \a -> fold f r `pipeBind` \b -> pipeReturn (a, b)

fuse :: Monad m
     => Pipe a b y z m z
     -> Pipe b c x y m y
     -> Pipe a c x z m z
fuse up down mr = fuse' up (down mr)

fuse' :: Monad m
      => (Maybe ([b], y) -> Step a b y z m z)
      -> Step b c x y m y
      -> Step a c x z m z
fuse' up0 (Pure cs z) =
    go $ up0 $ Just (cs, z)
  where
    go (Pure bs y) = Pure bs y
    go (M m) = M (liftM go m)
    go (Yield up _) = go $ up $ Just ([], z)
    go (Await up) = Await $ \ma -> go $ up ma
    go (Stop bs y) = Stop bs y
fuse' up (M m) = M (liftM (fuse' up) m)
fuse' up (Yield down o) = Yield (fuse up down) o
fuse' up0 (Await down) =
    go $ up0 Nothing
  where
    go (Pure bs y) = fuse' (\_ -> Pure bs y) (down Nothing)
    go (M m) = M (liftM go m)
    go (Yield up b) = fuse' up (down b)
    go (Await up) = Await (go . up)
    go (Stop bs y) = fuse' (\_ -> Stop bs y) (down Nothing)
fuse' up0 (Stop cs z) = fuse' up0 (Pure cs z)

(>->) :: Monad m
     => Pipe a b y z m z
     -> Pipe b c x y m y
     -> Pipe a c x z m z
(>->) = fuse

idP :: Monad m => Pipe i i r t m r
idP Nothing = Await (Yield idP)
idP (Just (is, r)) = Pure is r

consume :: Monad m => Int -> Pipe i o d t m [i]
consume count0 _ =
    go id count0
  where
    go front 0 = Pure [] (front [])
    go front count = Await $ \mi ->
        case mi of
            Just i -> go (front . (i:)) (count - 1)
            Nothing -> Pure [] (front [])

fold :: Monad m => (r -> i -> r) -> r -> Pipe i o d t m r
fold f r0 _ =
    loop r0
  where
    loop r = Await $ \mi ->
        case mi of
            Just i -> loop $! f r i
            Nothing -> Pure [] r

runPipe :: Monad m => Pipe () () d r m r -> m r
runPipe f = runStep (f Nothing)

runStep :: Monad m => Step () () d r m r -> m r
runStep (Pure _ r) = return r
runStep (M m) = m >>= runStep
runStep (Await f) = runStep (f Nothing)
runStep (Yield f _) = runPipe f
runStep (Stop _ r) = return r

takeExactly :: Monad m => Int -> Pipe i i r t m r
-- We specifically ignore leftovers, since we want to ensure
-- that we consumed exactly the given number of elements
-- from the stream.
takeExactly 0 (Just (_, r)) = Pure [] r
takeExactly 0 Nothing = Yield (takeExactly 0) Nothing
takeExactly count _ = Await $ \mi -> Yield (takeExactly (count - 1)) mi

yieldMany :: Monad m => [o] -> Pipe i o r t m ([o], [o], r)
yieldMany rest (Just (os, r)) = Pure [] (rest, os, r)
yieldMany [] Nothing = Yield (yieldMany []) Nothing
yieldMany (o:os) Nothing = Yield (yieldMany os) (Just o)
```

## Conclusion

The approach I've shown here is not optimized. A faster way of implementing this is to collapse `Pipe` and `Step` back into a single data type, and have a dedicated constructor to asking for downstream results. It's also more efficient to skip all of the `Maybe` wrappers. Instead of `Await` taking a `Maybe i`, `Await` can have two fields: one for when a value is available from upstream, and the other for when no such value is available.

Though it's currently highly experimental, I've implemented these ideas in an [experimental branch of conduit](https://github.com/snoyberg/conduit/blob/1d6e35e3956666a62ed65538e40474b4891be9f9/conduit/Data/Conduit/Internal.hs#L68). A quick translation from our approach is:

* Stop is called Terminate.
* Await has been deconstructed into two fields.
* Instead of `Yield` providing a `Maybe o`, it provides an `o`. The functionality provided by yielding `Nothing` is now handled by the `Empty` constructor.
* `Check` is used for checking for downstream results.
* `Yield` and `Empty` are also able to check for downstream results. In the case of `Empty`, we are guaranteed that downstream must provide a result. In the case of `Yield`, we include the extra field as an optimization, since composing `Yield` and `Check` is otherwise so common.

I've been able to replicate the current conduit API on top of this. The full test suite passes, including some tests for identity and associativity that failed previously. And the benchmarks show this approach as comparable to previous versions.

This approach seems very promising to me, but I also think we need to take time to analyze it properly. I look forward to hearing what the rest of the community thinks.

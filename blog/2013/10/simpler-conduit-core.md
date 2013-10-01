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

Quick recap: 

Automatic termination shows up
in two ways: calling `await` and calling `yield`. In this blog post, what I'd like to do i


I recently had an email thread with Greg Weber and Max Cantor about
`classy-prelude`. We ended up focusing on `classy-prelude`'s implementation of
`map`. Currently, `classy-prelude` defines a `CanMap` typeclass as follows:

```haskell
class CanMap ci co i o | ci -> i, co -> o, ci o -> co, co i -> ci where
    map :: (i -> o) -> ci -> co
```

Our conversation revolved mostly around how this leads to difficult type
signatures and error messages, which is certainly a known problem with the
class-based approach. However, that's not what I want to focus on now. (If
anyone's interested in *that* conversation, let me know, I think it's an
interesting one to discuss more broadly as well.)

The issue we're trying to solve with this typeclass is dealing with many
different shapes of containers, from polymorphic without constraints (e.g.,
list, vector), to constrained polymorphic (e.g., Set) to completely monomorphic
(e.g., ByteString, Text). I wanted to investigate prior art in this area, so I
decided to look at Edward's ever-popular `lens` package, something I'm always
itching to learn more about.

I'm by no means an expert on this package, but it seems like the relevant type
class for the `map` function would be `Each`, defined as:

```haskell
class (Functor f, Index s ~ Index t) => Each f s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: IndexedLensLike (Index s) f s t a b
```

I was shocked at just how similar these two typeclasses were to each other.
Using `each`, it's possible to define a `map` function:

```haskell
map :: Each.Each Lens.Mutator s t a b => (a -> b) -> s -> t
map = Lens.over Each.each
```

I tried replacing this in `classy-prelude` (using [FP Haskell
Center](https://www.fpcomplete.com/) to do my coding, of course), and [the
result](https://github.com/snoyberg/classy-prelude/commit/024cf34a3690088238a6c28e5f0694162a1111a7)
was nearly perfect. As expected, I had to modify some type signatures in my
test suite to use `Each` instead of `CanMap`, and there appear to be no
instances of `Each` for `Set` and `HashSet` (I'm sure there's a reason for
that, I'm just not familiar with the issue).

To me, this is a very interesting direction to consider heading in.
`classy-prelude` has always focused on pragmatism, and retaining the
programming style most users are accustomed to. `lens`, on the other hand,
takes a much more principled approach to its type classes, but has a steeper
learning curve. Perhaps by building `classy-prelude` on top of `lens` in this
manner, we can get an easy-to-learn library which gradually exposes users to
powerful and well-designed abstractions. This would also allow the community to
focus on making instances for one set of typeclasses, and then users could
essentially layer whatever high-level interface on top of it that they want.

I haven't looked into the other typeclasses in `classy-prelude` yet, but I have
a strong feeling that many of them can be implemented on top of `lens` classes.

Is this something others find interesting and worth pursuing?

* * *

Coming back to that original thread from Greg and Max, I *am* a bit concerned
that such a change would only make the error message and type signature issues
even harder to deal with. But `classy-prelude` has never been a
beginner-friendly project, and at least if both `classy-prelude` and `lens`
users got the *same* terrifying error messages, it might be easier to build up
some common documentation on how to address them.

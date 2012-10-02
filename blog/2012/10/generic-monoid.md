I recently was working on a project which included a very large datatype for
holding configuration data. The configuration data was parsed from a file. One
trick was that each config file could reference another "parent" config file.
The desired semantics are that the settings in the "child" override those in
the parent. To make things a bit more concrete, consider something like:

```haskell
data Config = Config
    { userLanguage :: Text -- only one allowed
    , translationFolder :: [FilePath] -- can be many
    }
```

Obviously, I was dealing with many more fields. The trick for dealing with the
parent folders was simple; my algorithm looked like:

```haskell
parseConfig :: FilePath -> IO Config
parseConfig fp = do
    doc <- readFile fp -- it's stored as XML, but that's irrelevant
    let parentFPs = getParents doc
    parents <- mapM parseConfig parentFPs
    let config = getConfig doc
    return $ mconcat $ config : parents
```

In other words, we just use a `Monoid` instance to put together the different
config files. To simplify the task of creating this `Monoid` instance, I made
sure to add appropriate `Monoid` wrappers to each field as necessary. For the
example above, I would add `First` to `userLanguage`, since we only wanted to
get the first one. For `translationFolder`, since we want to grab the folders
from the parents in addition to the child, we'd leave it as a list. Then, the
`Monoid` instance is just some boilerplate:

```haskell
instance Monoid Config where
    mempty = Config mempty mempty
    Config a b `mappend` Config x y = Config (a `mappend` x) (b `mappend` y)
```

Of course, writing such an instance by hand quickly becomes tedious. What I
wanted was some way to generate that boilerplate automatically. And the
solution I found was GHC 7.4's new Generics implementation. The code I wrote is
heavily based on the [GHC
documentation](http://www.haskell.org/ghc/docs/7.4.2/html/users_guide/generic-programming.html),
which happens to be a great coverage of the topic. The docs give the example of
serialization, which uses a unary function. Implementing `Monoid` involves a
nullary and a binary function, which makes it a good follow-up to the
serialization example.

(Note: [full code available as a Github gist](https://gist.github.com/3769191).)

The first step is to create a generic version of our `Monoid` typeclass:

```haskell
class GMonoid f where
    gmempty :: f a
    gmappend :: f a -> f a -> f a
```

This looks very similar to our standard `Monoid` typeclass. One tweak is the
fact that the instance of the typeclass now takes an argument (a.k.a., it's of
kind `* -> *` instead of `*`). The reason is that the Generics datatypes all
have a phantom type variable. My understanding is that this type variable is
currently unused.

Once we have this typeclass, we need to create instances for the different
Generic datatypes. There are five datatypes available: U1, K1, M1, :+:, and :*:
(please see the linked documentation for an explanation). Thankfully, most of
these instances are incredibly straight-forward.

Our first instance is for `U1`, which represents a nullary constructor. In
non-generic world, it's easy to deal with this case. `mempty` would just be the
constructor, and `mappend`ing two identical nullary constructors should result
in the same constructor. The generic version is just as simple:

```haskell
instance GMonoid U1 where
    gmempty = U1
    gmappend U1 U1 = U1
```

Next, let's consider product types, e.g. `data Foo = Foo Bar Baz`. `mempty`
would want to take advantage of the `mempty` provided for `Bar` and `Baz`.
`mappend` would like to `mappend` the fields in the left and right `Foo`. We
can express this almost identically in the generic version:

```haskell
instance (GMonoid a, GMonoid b) => GMonoid (a :*: b) where
    gmempty = gmempty :*: gmempty
    gmappend (a :*: x) (b :*: y) = gmappend a b :*: gmappend x y
```

Sum types are a bit trickier. It's not immediately clear what the right thing
to do is. Consider the datatype `data Foo = Foo1 Bar | Foo2 Baz`. Should
`mempty` use the first or second constructor? As for `mappend`, if both input
values use the same constructor, the solution is relatively simple. But what
happens if we have something like `mappend (Foo1 x) (Foo2 y)`? There's no
obvious solution.

So I decided to just leave off the sum type instance. What's wonderful about
the generics implementation is that this means, at compile time, trying to use
the generics code will fail on any sum type.

Nonetheless, for completeness sake, I did put together an instance. Its
semantics are to arbitrarily choose the first constructor for `mempty`, and the
first argument to `mappend` if there's a constructor conflict. This looks like:

```haskell
instance (GMonoid a, GMonoid b) => GMonoid (a :+: b) where
    gmempty = L1 gmempty
    gmappend (L1 x) (L1 y) = L1 (gmappend x y)
    gmappend (R1 x) (R1 y) = R1 (gmappend x y)
    gmappend x _ = x
```

Ultimately, we'll end up hitting non-generic values (the actual values
contained by our datatype). At that point, we want to switch over to standard
`Monoid` functions. Again, the generics implementation will prevent us from
using datatypes which are not instances of `Monoid`.

```haskell
instance Monoid a => GMonoid (K1 i a) where
    gmempty = K1 mempty
    gmappend (K1 x) (K1 y) = K1 $ mappend x y
```

And finally, we need to deal with the `M1` datatype, which is just a metadata
container:

```haskell
instance GMonoid a => GMonoid (M1 i c a) where
    gmempty = M1 gmempty
    gmappend (M1 x) (M1 y) = M1 $ gmappend x y
```

Now that we've implemented all of our instances, how do we use this? The
`Generic` typeclass provides two methods: `to` and `from`, to convert a generic
representation to a value and vice-versa. So we just take advantage of those,
together with our generic `gmempty` and `gmappend`, to come up with default
`mempty` and `mappend` functions:

```haskell
def_mempty :: (Generic a, GMonoid (Rep a)) => a
def_mempty = to gmempty

def_mappend :: (Generic a, GMonoid (Rep a)) => a -> a -> a
def_mappend x y = to $ from x `gmappend` from y
```

If we had control of the `Monoid` typeclass ourselves, we could also use the
DefaultSignatures extension right now to bake this directly into the `Monoid`
typeclass. Then, any time we wrote `instance Monoid Foo`, it would use
`def_mempty` and `def_mappend`. However, in our case, we have to do it
manually:

```haskell
instance Monoid Config where
    mempty = def_mempty
    mappend = def_mappend
```

Still much cleaner than having to write it all out manually.

If you're looking for a more sophisticated example of generics usage, check out
the `ToJSON` and `FromJSON` typeclasses in the [aeson
package](http://hackage.haskell.org/packages/archive/aeson/0.6.0.2/doc/html/Data-Aeson.html#t:ToJSON).

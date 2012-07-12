tl;dr: Announcing the beginning of [classy-prelude](https://github.com/snoyberg/classy-prelude), an alternate prelude which makes it easier to use conflicting names via typeclasses, and thereby encourages usage of more powerful libraries like `text` and `unordered-containers`. I'll release the code to Hackage, as soon as Hackage comes back up.

A few months ago, Max Cantor braved the treacherous journey into the barren wastelands of northern Israel to pay me a visit /melodrama. We discussed a number of different ideas then, one of which being an alternate prelude.

There have been a number of different alternate preludes attempted before, trying to do such things as fix the numeric type classes or automatically generate Javascript code. Our goal was entirely different.

For me, it's based around a very simple idea: programmers are lazy. Let me give a practical motivating case. Suppose you're on line 247 of the file you're currently editing, and you need to modify some value wrapped in a `Functor`. You'd ideally like to write `f <$> x`. But have you imported `Control.Applicative` yet? If you want to write that "correct" code, you'll need to:

1. Scroll up to your import list.
2. Look for `Control.Applicative`.
3. If it's not there, type in `import Control.Applicative ((<$>))`.
4. Try to find where you were in the code.
5. Try to remember what you were doing.

I'm sure I'm not the only Haskeller who has simply written `fmap f x` in this situation, even if we'd prefer the former. The solution to this is pretty simple: just export more common functions and operators. (While we're at it, we'll export common datatypes like `ByteString`.) But this is a relatively mundane problem. Let's look at some other very common cases:

* Do you represent textual data with `String` or `Text`? The former lets you use `++`, `concat`, and a bunch of other functions in the `Prelude`. The latter forces you to `import qualified Data.Text as T`, and then use `T.append` and `T.concat`.
* You need to build up a lookup table. Do you use `Data.Map` or a simple associated list? This has important ramifications on program correctness, as the associated list approach doesn't promise you the same invariants as `Data.Map`.
* If you want to start using `conduit` (or `enumerator`), you'll likely end up importing about 3 modules: the core module, one providing list-like primitives, another providing binary functions... and so on.

## Solution: type classes

To sum up the problem more succinctly: we have too many name collissions. We deal with this via qualified imports, but this introduces a large number of import statements and tedious usage of module prefixes.

So Max and I came up with a simple solution: just create typeclasses for these common, shared functions, and export them from a modified prelude. This means that your standard 20-odd line set of imports turns into:

    {-# LANGUAGE NoImplicitPrelude #-}
    import ClassyPrelude

Wherever possible, we reuse existing typeclasses. For example, `++` is just an alias for `Data.Monoid.mappend`, and `concat` for `Data.Monoid.mconcat`. Since most of our common types (`String`, `Text`, `ByteString`, `Map`, ...) provide `Monoid` instances, we immediately get a much more useful operator.

In other cases, such as the `length` function, no such typeclass exists. For those cases, we define a new typeclass. The implementation is pretty simple in this case:

    class CanLength c i | c -> i where
        length :: c -> i
    instance CanLength [a] Prelude.Int where
        length = Prelude.length
    instance CanLength ByteString Prelude.Int where
        length = S.length
    instance CanLength LByteString Int64 where
        length = L.length
    instance CanLength Text Prelude.Int where
        length = T.length
    instance CanLength (Map k v) Prelude.Int where
        length = Map.size
    instance CanLength (Set x) Prelude.Int where
        length = Set.size

Notice the use of functional dependencies to state the datatype used to represent the length. I considered using type families, but believe that for our purposes here, the brevity of fundeps really pays off. This really shows in giving us relatively short error messages.

## Downsides

There are of course some downsides to this approach.

* As just mentioned: error messages are less helpful.
* In some cases, explicit type signatures are necessary. For example, `length . pack . "hello"` is ambiguous, since `length` and `pack` could be working on either strict or lazy text, or a string, or possibly something else entirely.
* Some seemingly simple functions (like `map`) need a bit more type machinery around them, as depending on the data structure in question, `map` can be more or less polymorphic.

As a result of these, we think that classy-prelude is a good choice for experienced Haskellers looking to speed up their development time, not for beginners. It's best to get accustomed to the types available monomorphically first.

## A harder example

To give an idea of some of the more complicated aspects here, let's take a complicated example: `filter`. We want to support `filter` for lists, `ByteString`s, `Map`s, and even conduits. To see the problem, we need to look at the different type signatures:

    filter :: (a -> Bool) -> [a] -> [a]
    filter :: (Word8 -> Bool) -> ByteString -> ByteString
    filter :: (a -> Bool) -> Map k a -> Map k a
    filter :: Monad m => (a -> Bool) -> Conduit a m a

To sum up:

* For lists, `Map`s, and `Conduit`s, the type of the predicate is polymorphic. For `ByteString`, it is always `Word8`.
* For lists and `Conduit`s, the input type and predicate type are the same. For `Map`, we have an extra type variable (`k`).
* For lists, `ByteString`s, and `Map`s, there are two arguments, while for `Conduit`s, there is only one. Said another way, the first three return a function, while the `Conduit` case returns a `Conduit` value.

So how do we represent these four different functions with a single typeclass? Firstly, we break it down into the part which is similar: each function takes a predicate `a -> Bool` and returns some value. So let's represent that with a typeclass:

    class CanFilter f a where
        filter :: (a -> Bool) -> f

We don't want to get into undecidable or overloaded instances, so in order to handle lists, `ByteString`s and `Map`s, we'll create another helper class that represents the case when `filter` returns a function:

    class CanFilterFunc c i | c -> i where
        filterFunc :: (i -> Bool) -> c -> c

Our functional dependency is essentially say "we have some container `c` which holds `i`, and only `i`." In theory we might want to get rid of that fundep, and then we could express things like "you can filter a `ByteString` like a collection of `Word8`s __or__ a collection of `Char`s." However, I've opted to avoid such expressions for now.

Next we need to provide an instance of `CanFilter` for instances of `CanFilterFunc`:

    instance (b ~ c, CanFilterFunc b a) => CanFilter (b -> c) a where
        filter = filterFunc

This might be a bit surprising: why do we use `b -> c` instead of `c -> c`, and what's the purpose of `b ~ c`? The answer is that we're trying to help GHC with type inference. If we used `c -> c`, it would mean "here's an instance for when you want a function that takes `c` and returns `c`. What we're defining instead is an instance for *any* unary function, and then with the `b ~ c` equality constraint we're saying, "oh, and we only allow this to work if the input matches the output." This means that if GHC is only able to identify the type of either the input or the output, type inference still succeeds.

Instances of `CanFilterFunc` are pretty simple; here are the three mentioned above.

    instance CanFilterFunc [a] a where
        filterFunc = Prelude.filter
    instance CanFilterFunc ByteString Word8 where
        filterFunc = S.filter
    instance Ord k => CanFilterFunc (Map k v) (k, v) where
        filterFunc = Map.filterWithKey . Prelude.curry

Finally, our `conduit` instance:

    instance (Monad m, r ~ r') => CanFilter (Pipe l i i r m r') i where
        filter = CL.filter

Again, we use the equality constraint trick to force the upstream and downstream result types to match. (If you haven't been following the recent conduit changes, don't worry about it.)

## Future changes

I wrote the library so far by porting a project at work over to it, and adding in functionality as needed. I think this is the best approach to building up a useful prelude, and I'd like to ask others to help out in this as well. For now, it might be best to just copy in the ClassyPrelude module to your project and add things as necessary, and afterwards send a pull request.

For immediate upcoming features, I'm planning on adding support for `vector` and `unordered-containers` datatypes, as well as `aeson` for JSON parsing. (__Edit__: I already added `vector` and `unordered-containers` support.) Max and I have also been discussing reversing the conduit/classy-prelude dependency, and providing instances in `conduit` itself. (__Edit__: For now, we have a separate classy-prelude-conduit package.) I'd like to hold off on that till classy-prelude stabilizes.

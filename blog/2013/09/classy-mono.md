We are pleased to announce the first release of the [mono-traversable](hackage.Haskell.org/package/mono-traversable) package and version 0.6 of the [classy-prelude](hackage.Haskell.org/package/classy-prelude). A special thanks goes to [FPComplete](www.fpcomplete.com) and [DocMunch](www.docmunch.com) for sponsoring its development and to Edward Kmett for advising the development of mono-traversable.

## Goals

This release takes the implementation of classy-prelude in an entirely different direction that we are very excited about.
First lets review why classy-prelude was created:

classy-prelude tackles a few problems with the existing Prelude that most experienced Haskell programers are doing already.

  * removing all partial functions
  * modernizing the prelude
    * generally use Text instead of String (basic-prelude also does this)
    * encourage the use of appropriate data structures such as Vectors or HashMaps instead of always using lists and associated lists

But classy-prelude was also created to confront what I believe to be Haskell's most obvious wart: name-spacing and the need to qualify functions.
Haskell has only offered up one solution to avoid using module qualifications or mangled function names: make greater use of generic type-classes.


## Change of implementation: from ad-hoc to lawful mono-traversable typeclasses

Part of the problem of the existing Prelude is that it tends to just operate on lists. My understanding is that the core Haskell libraries are moving in the direction of using polymorphic type-classes (for example, more usage of Foldable and Traversable).

However, there is still a problem with this direction: it does not work with monomorphic containers such as Text and ByteString. A Functor operates over a container, so we can `fmap` over both a `Vector a` and a list `[a]`. But Text and ByteString are monomorphic containers: their type does not tell us what they contain, so there is no way we can fmap over them. But it is frustrating: Text is obviously a container of characters and ByteString is obviously a container of bytes. classy-prelude solved this in an ad-hoc way, by creating a new type-class for each function to operate on multiple containers and adding on more type parameters as needed. For example, map was implemented through a CanMap type class:

```
class CanMap ci co i o | ci -> i, co -> o, ci o -> co, co i -> ci where
    map ∷ (i → o) → ci → co
```

The previous approach of classy prelude met its design goals, but it had drawbacks. The main problem was that APIs with lots of type variables and functional dependencies give hard to decipher error messages. I also came across a case of using a complex data structure where GHC could not resolve the types and switching to the Prelude version immediately resolved it. But switching to the standard Prelude version of a function (when operating over lists) was already one of my techniques to help understand the error messages. I was much happier with the error messages from using fmap (over polymorphic containers) than using the map from classy-prelude.

The original classy-prelude approach missed the chance to solve the underlying monomorphic vs polymorphic problem in a well-grounded way. This is the motivation for the mono-traversable package: we just use a type family to declare the type that the mono-morphic container contains.

Note that all typeclasses have been prefixed with `Mono`, and functions have
been prefixed with `o` The mnemonic for `o` is `only one` or alternatively
mono, but `m` is overused in Haskell, so take the second letter instead.
We are open to suggestions for better naming schemes.

```
type family Element mofu
type instance Element T.Text = Char                                                      
type instance Element [a] = a  

class MonoFunctor mofu where
    omap ∷ (Element mofu → Element mofu) → mofu → mofu
    default omap ∷ (Functor f, Element (f a) ~ a, f a ~ mofu) ⇒ (a → a) → f a → f a 
    omap = fmap

instance MonoFunctor T.Text where
    omap = T.map

instance MonoFunctor [a]
```

All of the laws for the polymorphic typeclasses apply to their monomorphic
cousins. Thus, even though a `MonoFunctor` instance for `Set` could
be defined, it is omitted since it could violate the functor
law of `omap f . omap g = omap (f . g)` because a Set must compress duplicate elements created by `f`.

Note that omap does not change type like fmap, its type is the equivalent of `(a -> a) -> f a -> f a`, there is no `b`.  Users of the classy-prelude no longer have a single map function. `map` is now aliased to `fmap`, and they have to choose between `fmap` and `omap` (and a different map for Set).

The MonoTraversable module of mono-traversable defined MonoFunctor, MonoFoldable, and MonoTraversable.

Any Functor or Foldable can be made into a mono by using a default instance that will use Functor or Foldable.

```
instance MonoFunctor  (Maybe a)
instance MonoFoldable (Maybe a)
```

The MonoFoldable typeclass provides a lot of functionality, and is strictly more inclusive than the normal Foldable functionality. So the classy-prelude exports just the MonoFoldable definitions:

```
concatMap ∷ (Monoid m, MonoFoldable c) ⇒ (Element c → m) → c → m 
concatMap = ofoldMap
```

Previously that would have had 4 type variable with functional dependencies between them, but now we have just 2 type variables constrainted by 2 type-classes and a type family. The constraints all help communicate what the types express in a straightforward way, whereas functional dependencies requires more contemplation to figure out what is actually going on. For an implementer this is much nicer. From an end user perspective the function is fairly intuitive and they already understood how it is behaved, but now the error messages should be easier to decipher when something goes wrong. Of course, deciphering error messages is intimately related to understanding the types. The lawful typeclass approach is also an approach that can create simpler and easier to understand types and thus better error messages for end users.

mono-traversable also exports some typeclasses that we consider more experimental (IsSet, IsMap, and IsSequence), but that we hope can provide the common functionality that is needed for a Prelude. We look forward to your feedback and help to improve these.

Some of the functionality discussed here is similar to the ListLike package, but we are hoping mono-traversable is a refinement of the underlying type-classes. Perhaps one of the problems of ListLike is that you are encouraged to import it qualified: hopefully the combination of mono-traversable name mangling and classy-prelude is a big improvement so that users can use mono-traversable unqualified or choose to entirely replace the Prelude and use non-mangled names.


## Partial functions and non-empty data structures

Another important aspect of classy-prelude's modernization is providing better alternatives to parital functions. In the 0.6 release we try to make it easier to use [semigroups](hackage.haskell.org/package/semigroups), including `Data.List.NonEmpty`. Some of Haskell's partial functions come from attempting to operate on a list that must be non-empty. We have a powerful type sytem, so we should be putting it to use! Rather than partial functions, or even functions from [safe](hackage.Haskell.org/package/safe), we can define data structures have at least one element with Data.List.NonEmpty

```
head (x:|[]) == x
```

mono-traversable includes a very preliminary NonNull type-class that attempts to extend the functionality of Data.List.NonEmpty from just lists to any type of sequential data.


## Performance

Besides encouraging appropriate data structures, mono-traversable is also focused on allowing for high performance definitions. Some other packages such as [monoid-subclasses](http://hackage.haskell.org/package/monoid-subclasses) tackle some of the functionality, but only dealing with an entire Monoid is not as performant as operating on individual elements. Similarly, the experimental NonNull typeclass is much less featureful than the [non-empty](http://hackage.haskell.org/package/non-empty) package, but is designed to use data structures in a performant way.


## Summary

  * classy-prelude is now based on lawful typeclasses, particularly the addition of mono-traversable
  * there is a lot of experimentation going on here, so we look forward to your feedback!

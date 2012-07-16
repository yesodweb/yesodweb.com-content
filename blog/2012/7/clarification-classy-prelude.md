Firstly, I wanted to mention that the [markdown engine powering this
blog](http://hackage.haskell.org/package/markdown) is now available on Hackage.
I made a [small release
announcement](https://groups.google.com/d/topic/yesodweb/peqyF3cUABM/discussion).
Basically: it's experimental, and I'd love feedback on how it works for others,
but it should be quite usable as-is.

Anyway, onto the main event.

* * *

When I [announced classy
prelude](/blog/2012/07/classy-prelude), there was quite a
mixed reaction from the community. There was quite a bit of positive feedback,
and lots of people seemed interested. And on the flip side, a number of people
very unequivocally declared it a horrible, horrible idea.

I'm not going to mince words: I think a couple of the detractors are making a
number of [baseless
assertions](/blog/2012/07/announcing-baseless-assertion),
attacking strawmen, and engaging in hyperbole. Declaring war on a new package
and swearing to never use it or any of its dependencies to bring about its
eventually demise does not really fit into a normal, healthy discussion. Making
claims about "brittle typeclass extensions" without any clarification is not
helpful. When discussions devolve to such a level, I don't think there's any
point in engaging.

Part of the fault in all of this is that I did not clarify the purpose of the
library well enough in the initial post, instead focusing more on how it works.
I will try to rectify that in this post, and in doing so hope to answer some of
the more well-stated arguments against classy prelude.

I strongly encourage discussion about classy prelude, and certainly want to
hear critiques. But please try to make them based on actual facts with well
reasoned arguments, not just asserting that typeclasses are horrible or that
this library will break all of Haskell's type safety.

## Let's start over: what is classy prelude?

*   It is a library which leverages type classes to provide name overloading,
    thereby reducing the number of import statements made and decreasing the
    syntactic overhead of qualified imports.

    Said another way: it's nothing more than syntactic sugar. There may
    certainly be better theoretical approaches to such syntactic sugar (a new
    namespace language extension, or a better module system, or maybe something
    like TDNR), but none of those actually exist today. In today's Haskell, the
    only approach possible to achieve this goal is typeclasses. (If someone knows
    something I don't, please say so.)

*   The motivation here is a simple hypothesis: programmers are lazy. Writing:

    ~~~haskell
    "foo" ++ "bar"
    ~~~

    is far easier than:

    ~~~haskell
    import qualified Data.Text as T

    T.pack "foo" `T.append` T.pack "bar"
    ~~~

    Therefore, people will tend towards the easier path, all else being equal.
    The goal is to lower that resistance to the right way to do things, and
    therefore encourage better code overall.

*   The purpose of classy prelude is *not* to encourage writing polymorphic
    code based on the typeclasses provided. Though it's certainly possible to
    write code such as:

    ~~~haskell
    {-# LANGUAGE NoImplicitPrelude #-}
    import ClassyPrelude
    import ClassyPrelude.Classes

    foo :: (CanLookup t k v, CanInsertVal t k v)
        => k
        -> v
        -> t
        -> Maybe v
    foo x y z = lookup x $ insert x y z
    ~~~

    That's not my intention. Instead, the idea would be to actually nail this down to concrete types, e.g.:

    ~~~haskell
    {-# LANGUAGE NoImplicitPrelude #-}
    import ClassyPrelude

    foo :: (Eq k, Hashable k) => k -> v -> LHashMap k v -> Maybe v
    foo x y z = lookup x $ insert x y z
    ~~~

    Compare that to the equivalent without classy-prelude:

    ~~~haskell
    import Data.HashMap.Lazy (HashMap)
    import qualified Data.HashMap.Lazy as HashMap
    import Data.Hashable

    foo :: (Eq k, Hashable k) => k -> v -> HashMap k v -> Maybe v
    foo x y z = HashMap.lookup x $ HashMap.insert x y z
    ~~~

    When used this way, the only difference from classy-prelude is syntactic.

*   That said, if people *want* to try to write polymorphic code with
    classy-prelude, I see no problem with trying it out. It's true that there
    are no typeclass laws defined for the classes provided, and therefore such
    generic code *may* not work correctly, but it's certainly worth trying it out.
    Which brings me to the most important point...

*   classy-prelude above all else is an __experiment__. It is in no way
    intended to be a replacement for the actual prelude (and probably never
    will be). I've used it in a few smaller projects at work to remove redundant
    code... and that's it. If someone sent me a pull request on one of the Yesod
    libraries today which used classy-prelude, I would reject it, because __the
    library is not ready for prime time__.

    Are there questionable choices made right now? Absolutely. Some people have
    pointed out that trying to unify a `ByteString` `map` and `conduit`'s `map`
    into a single class may be overkill. I completely agree: it __may__ be
    overkill. However, I vehemently disagree with anyone claiming that they know
    it's wrong. How can you know it's wrong, bad, evil, and kills kittens until
    you've actually tried it?

So far, my experience has been that error messages do not get significantly
more confusing, that code tends to just work the way you'd expect it to (since
it's all just syntactic sugar for existing, well-tested and thoroughly
type-safe libraries), and that the code becomes more legible since we've
removed a bunch of unnecessary line noise (i.e., qualified usage of functions).

Are there downsides to this library? Certainly. Is it going to become the
de-facto library used for all Haskell development? (Almost) certainly not. Has
it already proven itself useful in some actual, real world code? Yes. And that
last point is the important one: even if you personally can't see a need for
this, others (myself included) do. Even if you believe that it violates every
principle of Haskell you hold dear, your belief isn't enough to win an
argument. And even if you try to declare a holy crusade against this thing, you
won't kill it. If people find it useful, they'll use it. If not, then it will
die without your help.

What I'm really saying is this: let's bump up the level of interaction here. If
you see a flaw, demonstrate the flaw. If you believe something is wrong,
explain that you __think__ it's wrong, but don't start claiming to have
absolute truth on your side. I'm happy to engage in a healthy conversation
about this library, but I have better things to do with my time then engage in
pointless flame wars.

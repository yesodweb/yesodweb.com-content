I'm happy to announce new versions of [basic-prelude](http://hackage.haskell.org/package/basic-prelude), [classy-prelude](http://hackage.haskell.org/package/classy-prelude), and
[classy-prelude-conduit](http://hackage.haskell.org/package/classy-prelude-conduit), as well as the addition of the new package
[classy-prelude-yesod](http://hackage.haskell.org/package/classy-prelude-yesod).
This is the first release of these packages which I consider stable enough for
general use, and I encourage people to have a look. You can also check out the
[basic-prelude](http://github.com/snoyberg/basic-prelude) and
[classy-prelude](http://github.com/snoyberg/classy-prelude) repos on Github.

Since it's been a while since I discussed classy-prelude, let's start with a
quick recap, beginning with the motivation: I think that the standard `Prelude`
is lacking in a few ways:

1. It encourages some bad practices (e.g., the partial `head` function).
2. It promotes certain datatypes (e.g., `String`) over better alternatives (e.g., `Text`).
3. Some commonly used functions are not exported (e.g., `mapMaybe`).
4. Since it sticks to concrete types in many cases (usually lists), it uses up the common namespace for function names (e.g., `length`), thereby requiring programmers to use qualified imports on a regular basis.

I think the first point stands on its own: there's a basic question we need to
ask ourselves about what we consider idiomatic Haskell code, and in my opinion,
partial functions is not a part of it. While that's an important point to
discuss, it's relatively straight-forward, so I won't be dwelling on it any
further.

The other three points rest around a central philosophy I have: programmers are
inherently lazy (in a good way), and will often choose the path of least
resistance. To demonstrate, consider the difference between using `String` and
`Text` for some simple concatenation:

```haskell
-- String version
name = firstName ++ " " ++ lastName

-- Text version
import qualified Data.Text as T
name = firstName `T.append` " " `T.append` lastName
```

(Without `OverloadedStrings`, this would be even longer.) It's not that the
second version is really that much worse than the first, it's just slightly
less convenient. And due to that extra bit of work, `Text` gets used less
often. You can see the same thing with `Map` versus associated lists, `Vector`
and lists, and so on.

__Note__: As Herbert pointed out to me, with GHC 7.4 and up, you could just use
the `<>` operator provided by `Data.Monoid` instance of <code>\`T.append\`</code>. So
consider the case where you need to use some `Prelude` functions that require a
`String`.

```haskell
-- String version
main = putStrLn $ "Invalid name: " ++ name

-- Text version
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid ((<>))
main = TIO.putStrLn $ "Invalid name: " <> name
```

By comparison, in `classy-prelude` this becomes:

```haskell
import ClassyPrelude
main = putStrLn $ "Invalid name: " ++ name
```

If you think that my assessment so far doesn't warrant any changes to our
tooling, turn back now, this blog post isn't for you. If you *are* interested
in some kind of a solution to this issue, I have two options for you.

## basic-prelude

Points 1-3 above can actually be solved pretty easily: just create a new
prelude module that has better defaults. `BasicPrelude`, provided by the
[basic-prelude](http://hackage.haskell.org/package/basic-prelude) package, does
just this. It exports more helper functions, avoids partial functions, and
exports a bunch of the common datatypes, like `ByteString` and `HashMap`.

Another important premise of `BasicPrelude` is that it doesn't replace any
existing types or type classes. It reuses the exact same `Monad` typeclass that
is exported by `Prelude`. That means that code using `BasicPrelude` is
completely compatible with "normal" Haskell code. Another way to put it is that
`BasicPrelude` is a non-revolutionary approach to improving the Haskell
programming experience.

`basic-prelude` is actually split up into two modules: `BasicPrelude` and
`CorePrelude`. `BasicPrelude` simply re-exports everything provided by
`CorePrelude`, and then adds in some missing components. `CorePrelude` is
intended to be a foundation for the creation of other preludes. It was
originally part of `classy-prelude`, but was then separated out by Dan Burton,
who now maintains `basic-prelude` with me. `CorePrelude` tries to export
components that would be usable by *all* `Prelude` replacements. For now, our
simple barometer of this is "would both BasicPrelude and ClassyPrelude use
this?"

`BasicPrelude` sticks to monomorphic functions for the most part, with a strong
bias towards lists (just like standard `Prelude`). It doesn't really do much
that's controversial, and should be a great approach to try out for people
experimenting with an alternate prelude. And if you don't like something about
it, you can either file an issue, or just create your own fork. Due to the
design of `basic-prelude`, forking does *not* create incompatible code, so it's
not a high-impact move.

## classy-prelude

`classy-prelude` is the more radical prelude. As mentioned, it builds on top of
`CorePrelude`, just like `BasicPrelude` does. The distinction, however, is that
instead of providing monomorphic, list-biased functions, it creates a slew of
typeclasses and provides polymorphic functions. Unlike many common typeclasses,
these typeclasses are *not* intended to be used in a polymorphic context
themselves, but rather to avoid the need to use qualified imports to
disambiguate names. In other words, we're using typeclasses for namespacing
purposes only.

(Despite this, we actually have a fairly thorough test suite covering the
behavioral laws of these type classes. So you *could* theoretically write
polymorphic code with `classy-prelude`, it's just not what I originally
intended.)

This namespacing approach was fairly uncommon (perhaps `classy-prelude` was the
first usage of it?) when I first started `classy-prelude`, and as a result I
was unsure how well it would turn out in practice. At this point, I've been
using `classy-prelude` for a number of my projects (both personal and [at
work](http://fpcomplete.com/)), and the approach is certainly viable. I
personally greatly prefer it to the non-classy approach, and will almost
certainly be using it for the foreseeable future- likely until we get a better
namespacing solution in GHC itself.

There are of course some downsides, some of which can be worked around:

*   Error messages become more confusing. I have no good solution to that right
    now. I don't think the messages are too daunting for experienced Haskellers,
    but I would not recommend `classy-prelude` to beginners.

*   In some cases it is impossible for the compiler to figure out which type
    you mean. For example, the following monomorphic code is completely
    unambiguous:

    ```haskell
    import qualified Data.Map as Map

    foo :: Text -> Int
    foo name =
        Map.lookup name people
      where
        people = Map.singleton "Michael" 28
    ```

    However, the equivalent `classy` code is problematic:

    ```haskell
    foo :: Text -> Int
    foo name =
        lookup name people
      where
        people = singleton "Michael" 28
    ```

    The problem is that both `singleton` and `lookup` are polymorphic, and are not
    used in the result, so there's no way to know which container to use.
    Fortunately, there's an easy workaround: the as* functions. In our case, we
    just replace the last line with:

    ```haskell
        people = asMap $ singleton "Michael" 28
    ```

    Overall, the code is still shorter. As an added bonus, you can now simply
    switch `asMap` to `asHashMap` to swap which container structure you use.

*   In some cases, keeping the same name can go beyond the capabilities of the
    type system. For example, when working on `classy-prelude-yesod`,
    overloading `insert` for both its usage in Persistent and containers like `Map`
    proved to be a bit problematic, specifically when not using the return value.
    For example, the following code doesn't compile:

    ```haskell
    _ <- runDB $ insert $ Person "Michael" 28
    ```

    The options in this case are either to not use the overloaded name and
    instead use a separately named function (in this case, `insertDB`), or to use a
    disambiguating helper function (`voidKey`) that fixes the types- similar to the `asMap`
    function described above. Those two solutions look like:

    ```haskell
    voidKey $ runDB $ insert $ Person "Michael" 28
    runDB $ insertDB $ Person "Michael" 28
    ```

    I'm not sure yet how I feel about these two approaches, but it definitely
    stresses the point that we're using the typeclass system in an extreme manner.

## classy-prelude-conduit and classy-prelude-yesod

These two packages build on top of `classy-prelude` to provide even more common
functionality. The former provides `conduit` and `xml-conduit` functions, while
the latter adds on top of that `yesod`, `persistent`, `http-conduit`, and a few
other things. `classy-prelude-yesod` has *not* been as thoroughly exercised as
the other packages discussed here, so you're more likely to run into issues
with it.

## Conclusion

These alternate preludes are not for everyone, but I think they offer a lot to
certain audiences. If you want to try out a smaller move, I'd recommend
`BasicPrelude`. If you want to be a bit more experimental, go for
`classy-prelude`. If you decide to drop either one at any point, it should not
be overly onerous to switch back to normal, monomorphic, qualified imports.

I'm definitely interested to hear people's experience with these packages.
There are still lots of improvements to be made, more common functionality to
be added, and documentation to be written. Now's a great time to get involved!

When I've answered the same question more than three times, it's usually time
to write up a blog post explaining the situation in more detail, and just link
people to that in the future. This is such a blog post.

Many people working with Haskell end up with one of these two classes of
inexplicable GHC errors:

1. Cannot match `ByteString` with `ByteString`, with a whole bunch of package
   name and version junk as well.

2. `SomeTransformerT` is not an instance of `MonadTrans`, when the
   documentation clearly indicates that `SomeTransformerT` **does** define such
   an instance.

How can a `ByteString` not be a `ByteString`? Well, there are two ways I can
think of. The first is that you're accidentally trying to mix up a strict
`ByteString` with a lazy `ByteString`, whose types clearly don't unify. While
that problem *does* pop up, in my experience most people figure that one out
pretty quickly. By the time someone's asking a question on Stack
Overflow/Reddit/haskell-cafe, it's usually much more insidious: there are two
copies of `bytestring` on their system.

Imagine this situation: you install GHC 7.6, which ships with
bytestring-0.10.0.2. You then install text via `cabal install text`. A few days
later, someone mentions that bytestring-0.10.4.0 is out, and it's all new and
shiny, so you go ahead and install it with `cabal install bytestring`.
Everything works wonderfully, and life is good. Then you decide to experiment
with text a bit, so you write the following program:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as S8

main :: IO ()
main = S8.putStrLn $ encodeUtf8 "Hello World!"
```

Woe unto you! GHC rejects your program with:

```
foo.hs:6:22:
    Couldn't match expected type `S8.ByteString'
                with actual type `bytestring-0.10.0.2:Data.ByteString.Internal.ByteString'
    In the return type of a call of `encodeUtf8'
    In the second argument of `($)', namely `encodeUtf8 "Hello World!"'
    In the expression: S8.putStrLn $ encodeUtf8 "Hello World!"
```

When is a `ByteString` not a `ByteString`? Here, apprently. Now it turns out
the GHC is actually giving you quite of bit of useful information, you just
need to know what to look for. It's expecting the type `S8.ByteString`, which
expands to `Data.ByteString.Char8.ByteString`, which in reality is just a type
synonym for `Data.ByteString.Internal.ByteString`. So what GHC really means is
that it can't unify the following two types:

    expected:                     Data.ByteString.Internal.ByteString
    actual:   bytestring-0.10.0.2:Data.ByteString.Internal.ByteString

Well now the difference just jumps out at you: the *actual* type comes from the
bytestring-0.10.0.2 package, whereas the first comes from... well, somewhere
else. As I'm sure you're guessing right now, that "somewhere else" is
bytestring-0.10.4.0, but GHC doesn't bother telling us that, since including
that level of information in every error messages would be overwhelming. To
step through why this came up exactly:

* `text` is installed against bytestring-0.10.0.2 (it was the only version of bytestring available at the time you installed text).
* Therefore, `encodeUtf8` will generate a `ByteString` value from version 0.10.0.2.
* Your program imports `Data.ByteString.Char8`, which is provided by both bytestring-0.10.0.2 and bytestring-0.10.4.0.
* GHC's default dependency resolution is: take the latest version of each package, in this case 0.10.4.0.
* Now we have a `S8.putStrLn` function expected a 0.10.4.0 `ByteString`, but an `encodeUtf8` function returning a 0.10.0.2 `ByteString`.

So how do we work around this problem? I can think of three ways:

1. Explicitly tell GHC which version of the bytestring package you want to use to force consistency, e.g. `runghc -package=bytestring-0.10.0.2 foo.hs`.
2. Never use GHCi, runghc, or ghc directly from the command line. Instead, always create a cabal file first. cabal's default dependency resolution will force consistent package loading.
3. Don't wind up in the situation in the first place, by ensuring you only have one version of each package installed.

That last point is what I strongly recommend to all users. And this is exactly
the design goal around [Stackage](http://www.stackage.org/), so it will
hopefully not come as a surprise that that's exactly what I recommend most
Haskell users use to get their packages installed.

Let's demonstrate that second case of `MonadTrans`. This time, let's try it
with GHC 7.8.3 (for reasons I'll make clear in a moment). GHC ships with
transformers-0.3.0.0. Next, we'll install the `either` package with `cabal
install either`. Once again, someone comes along and tells us about a shiny new
package, transformers-0.4.1.0. Dutifully, we upgrade with `cabal install
transformers-0.4.1.0`. And then we try to run the following simple program:

```haskell
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

main :: IO ()
main = do
    x <- runEitherT $ lift $ putStrLn "hey there!"
    print (x :: Either () ())
```

GHC mocks you with:

```
foo.hs:6:23:
    No instance for (MonadTrans (EitherT ()))
      arising from a use of ‘lift’
    In the expression: lift
    In the second argument of ‘($)’, namely
      ‘lift $ putStrLn "hey there!"’
    In a stmt of a 'do' block:
      x <- runEitherT $ lift $ putStrLn "hey there!"
```

"But `EitherT` *is* an instance of `MonadTrans`!" you insist. That may be true, but it's an instance of the *wrong `MonadTrans`*. The `either` package is built against transformers-0.3.0.0, whereas you've imported `lift` from transformers-0.4.1.0. This can be worked around as above, with `runghc -package=transformers-0.3.0.0 foo.hs`. And yet again, my strong recommendation is: use Stackage.

There's one more particularly painful thing I need to point out. Some packages
are bundle with GHC, and are depended on by the `ghc` package. The special
thing about the `ghc` package is that it can be reinstalled without installing
a new version of GHC itself. Any packages depended on by the `ghc` package
cannot be unregistered without breaking `ghc`, which would in turn break
libraries like `doctest` and `hint`. If you follow these points to conclusion,
this means that you should never upgrade GHC-bundled libraries. I [wrote a blog
post on this
topic](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds), and the
takeaway is: please, always support older versions of packages like bytestring,
transformers, and- of course- base.

There's one final case I want to mention. Try running `cabal install data-default-0.5.1 http-client`, and then run the following program:

```haskell
import Data.Default
import Network.HTTP.Client

main :: IO ()
main = withManager defaultManagerSettings $ \man -> do
    res <- httpLbs def man
    print res
```

You'll get the irritating error message:

```
foo.hs:6:20:
    No instance for (Default Request) arising from a use of ‘def’
    In the first argument of ‘httpLbs’, namely ‘def’
    In a stmt of a 'do' block: res <- httpLbs def man
    In the expression:
      do { res <- httpLbs def man;
           print res }
```

But if you look at http-client, `Request` *is* in fact an instance of
`Default`. "Alright, I know what's going on here" you say. Certainly there are
two versions of data-default installed, right? Actually, no, that's not the
case. Have a look at the following:

```
$ ghc-pkg list | grep data-default
    data-default-0.5.1
    data-default-class-0.0.1
```

There's just a single version of each of these packages available. So why are
we getting our mysterious error message? Once again, it's because we have two
versions of the `Default` class. After `data-default` version 0.5.1,
`data-default` split into a number of packages, and the `Default` class
migrated into `data-default-class`. http-client defines an instance for
`Default` from `data-default-class`. And if you use `data-default` version
0.5.2 or higher, it will simply re-export that same class, and everything will
work.

However, our `cabal install` command forced the installation of the older
data-default (0.5.1) which defines its own `Default` typeclass. Therefore, we
end up with two separate `Default` classes that don't unify. This is a problem
that exists whenever packages are split or joined, which is why you should
embark on such refactorings with great care.

As it happens, this is yet another problem that is solved by using Stackage,
since it forces a consistent set of versions for data-default and
data-default-class.

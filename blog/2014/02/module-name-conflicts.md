For some upcoming improvements to FP Haskell Center, I recently added a new
feature to Stackage: the ability to detect module name conflicts. This is where
two different packages both export a module of the same name.

You can see [the full module name conflict
list](https://gist.github.com/snoyberg/9246423) for my most recent build. The
file format is fairly dumb: one line lists all of the packages using a common
module name, and the following line contains all of the module names shared.
(JSON, YAML, or CSV would have been better file formats for this, but one of
the goals of the Stackage codebase is to avoid extra package dependencies
wherever possible.)

Most of these conflicts don't seem problematic at all. The fact that base,
haskell98, haskell2010, and base-compat share a lot of the same module names,
for example, should be expected, and users really do need to choose just one of
those packages to depend on.

Some other cases, on the other hand, might cause issues. For example, both
hashmap and unordered-containers export the Data.HashSet module. This can
negatively affect users of GHCi who have both packages installed and try to
import Data.HashSet. Also, if for some reason a cabal package depended on both,
you'd need to use package imports to disambiguate. There can also be an issue
of confusion: if I see `Data.HashSet` at the top of a module, it would be nice
to know which package it comes from without having to check a cabal file or
running ghc-pkg.

I'm mostly writing this blog post as I think it's the first time we've had any
kind of collection of this information, and I don't think we've had a community
discussion about conflicting module names. I don't know if the problem is
significant enough to even warrant further analysis, or how have thoughts on
how to proceed if we *do* want to try and disambiguate module names.

Here are some of the conflicting module names, and the packages using them:

* System.FilePath.Glob
    * Glob
    * filemanip
* Test.Framework
    * HTF
    * test-framework
* Control.Monad.Trans.List
    * List
    * transformers
* Control.Monad.CatchIO
    * MonadCatchIO-mtl
    * MonadCatchIO-transformers
* Test.QuickCheck.Instances
    * checkers
    * quickcheck-instances
* Crypto.Random
    * crypto-api
    * crypto-random
* Crypto.Random.API
    * crypto-random
    * crypto-random-api
* Data.HashSet
    * hashmap
    * unordered-containers
* Language.Haskell.Lexer
    * haskell-lexer
    * haskell-src
* Test.Hspec
    * hspec
    * nanospec
* Data.String.UTF8
    * hxt-unicode
    * utf8-string

Which of the following snippets of code is better?

```haskell
#if MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#else
import Control.Applicative ((<*), pure)
#endif
```

Versus:

```haskell
import Control.Applicative ((<*), pure)
```

If you are working on a project that supports multiple GHC versions, enable
extra warnings via `-Wall`, and actually like to get your code to compile
without any warnings, you'll probably say that the former is better. I'm going
to claim, however, that any sane human being knows intuitively that the latter
is the better version of the code, for multiple reasons:

* It doesn't require a language extension to be enabled
* It's much shorter without losing any useful information to the reader
* It's more robust to future changes: if you need to add an import, you don't
  have to remember to update two places

However, if you look through my code bases, and the code bases of many other
open source Haskell authors, you'll find the former examples regularly. I'm
beginning to come to the conclusion that we've been attacking this problem the
wrong way, and what we _should_ be doing is:

* Turning on `-Wall` in our code
* Either modify `-Wall` in GHC to not warn about unused imports, or explicitly
  disable unused import warnings via `-fno-warn-unused-imports`
* As many of us already do, religiously [use Travis
  CI](http://docs.haskellstack.org/en/stable/GUIDE/#travis-with-caching) to
  check multiple GHC versions to avoid accidental regressions
* In our Travis builds, start turning on `-Werror`

Maintaining complex CPP in our imports is sometimes a necessary evil, such as
when APIs change. But when we are simply doing it to work around changes in
what `Prelude` or other modules export, it's an unnecessary evil. This is
similar to the change to GHC a few years back which allowed `hiding
(isNotExported)` to not generate a warning: it made it much easier to deal with
the now-no-longer-present `Prelude.catch` function.

While it's true that removing unused imports is a nice thing to do to our
codebases from time to time, their presence does not actually indicate any
potential issues with our code. My concern with the presence of these warnings
is that they will lead to one of two situations:

* We simply accept that our libraries generate warnings when compiled, which
  ends up hiding actionable warnings via a terrible signal-to-noise ratio
* In an effort to clean up all warnings, we end up creating hideous messes like
  those above, or breaking backwards compatibility with old versions of
  dependencies

I haven't actually started making these modifications to my libraries, as I'm
not yet fully convinced that this is a good idea. There are also other points
in this design space, like explicitly marking some imports as redundant, though
that would require some deeper changes to GHC and wouldn't be usable until we
drop support for all current GHC versions.

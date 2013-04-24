I just released version 1.0.4 of shakespeare-css (the package providing the
Lucius CSS template system). This release contains one newly added feature:
mixins. This allows you to define reusable CSS components. A common use case
for this is to address vendor prefixes. So as a motivating example:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Text.Lucius
import qualified Data.Text.Lazy.IO as TLIO

transition :: String -> Mixin
transition val =
    [luciusMixin|
        -webkit-transition: #{val};
        -moz-transition: #{val};
        -ms-transition: #{val};
        -o-transition: #{val};
        transition: #{val};
    |]

main :: IO ()
main = TLIO.putStrLn $ renderCssUrl undefined
        [lucius|
            .some-class {
                ^{transition "all 4s ease"}
            }
        |]
```

This produces the following CSS (whitespace added for readability):

```css
.some-class {
    -webkit-transition: all 4s ease;
    -moz-transition:    all 4s ease;
    -ms-transition:     all 4s ease;
    -o-transition:      all 4s ease;
    transition:         all 4s ease;
}
```

There's not much more to it than that. You can begin using this new feature
immediately with your existing projects, simply bump the lower bound on
shakespeare-css.

Thanks to Blake Rain for [motivating me to actually get this
written](https://groups.google.com/forum/#!msg/yesodweb/oQaFiDfDaic/0hLqy6T7mbMJ).

With the close of this issue, there are officially no more open issues in [the
Yesod 1.2 milestone on
Github](https://github.com/yesodweb/yesod/issues?milestone=5&state=open). In
other words, we're getting very close to release. Greg, Felipe and I are tying
up some last few tweaks to Persistent 1.2, and then we should be ready to go.
This is officially your last chance to provide any input on the 1.2 release.

If you're looking for a refresher on what changes are coming, please check out
the [high-level changelog](https://github.com/yesodweb/yesod/wiki/Changelog)
and the [detailed change
list](https://github.com/yesodweb/yesod/wiki/Detailed-change-list).

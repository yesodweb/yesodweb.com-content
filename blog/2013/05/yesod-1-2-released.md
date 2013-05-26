The Yesod team is pleased to announce the release of Yesod 1.2. You can get it with:

    cabal install yesod-platform yesod-bin

The yesod binary is now a separate package which helps manage dependencies, but it does mean you need to remember to install 2 separate packages.

Yesod [1.1 was released](http://www.yesodweb.com/blog/2012/08/announcing-yesod-1-1) in August. Shortly after, Michael [started working for FP Complete](http://www.yesodweb.com/blog/2012/08/joining-forces-advance-haskell). A lot has happened since then!


## Yesod ecosystem

* [Esqueleto](http://blog.felipe.lessa.nom.br/?p=68) was released shortly after Yesod 1.1. This gives SQL users the full power of raw SQL but fully type-checked queries.
* [yesod-pure](http://www.yesodweb.com/blog/2012/10/yesod-pure) was released for those who want less Template Haskell and type-safety.
* [Conduit 1.0 and Wai 1.4 were released](http://www.yesodweb.com/blog/2013/02/announce-conduit-1-0-wai-1-4)
* Javascript templates were [revised to insert JSON](http://www.yesodweb.com/blog/2012/11/updates-julius-interpolation) instead of text.
* [TypeScript template support was added](http://www.yesodweb.com/blog/2013/04/shakespeare-typescript)
* CSS templates [support mixins](http://www.yesodweb.com/blog/2013/04/mixin-support-in-lucius)
* [Persistent 1.2 was released](http://www.yesodweb.com/blog/2013/04/persistent-1-2-out)
* The Warp web server has been continually [sped up](http://www.yesodweb.com/blog/2012/10/measuring-warp). These kinds of efforts make Yesod look pretty good in a recently created [benchmark](http://www.techempower.com/benchmarks/) suite. There is now a WAI entry in addition to the Yesod entries.


## Yesod 1.2

### Representation system

Previously discussed in the post: [a better representation system, cleaner internals, and the request local cache](http://www.yesodweb.com/blog/2013/03/yesod-1-2-cleaner-internals). Providing different representation types (JSON or HTML) used to be cumbersome at times, but now it is simple using selectRep.

    getResource :: Handler TypedContent
    getResource = do
      selectRep $ do
          provideRep $ [hamlet|<div>|]
          provideRep $ object ["result" .= "ok"]


### Request local type-based caching

See the [previous mentioned blog post](http://www.yesodweb.com/blog/2013/03/yesod-1-2-cleaner-internals), but you just need to create a newtype wrapper around some data and then you can cache it with the `cached` function.


### Subsite overhaul

[Subsites are now just a transformer over a master site](http://www.yesodweb.com/blog/2013/03/big-subsite-rewrite)


### Flexible routing

[routing dispatch is more flexible](http://www.yesodweb.com/blog/2013/03/yesod-dispatch-version-1-2)


### Better streaming API

[streaming has been simplified](http://www.yesodweb.com/blog/2013/03/simpler-streaming-responses)


### Asset pipeline

Yesod has always made it easy to combine dynamic css and javascript since before Rails started using the term asset pipeline. What was missing was the same ease for static assets. Combining static assets is very important for optimal performance by reducing the total number of network requests. You can now easily combine CSS and Javascript with the combineScripts and combineStylesheets helpers. [Here is the scaffolding change](https://github.com/yesodweb/yesod-scaffold/commit/fe2e2a0eed1f0cb2cc4b09b144df0a08f66e294a) and you can also look at the [haddock documentation](http://hackage.haskell.org/packages/archive/yesod-static/1.2.0/doc/html/Yesod-Static.html#g:3).


### Better testing

yesod-test was completely overhauled, making it easier to use and providing cleaner integration with hspec.. It is easy in Haskell to just lean against the type system for most things and skip testing, particularly if it is something that is hard to test with QuickCheck. But yesod-test (and wai-test) are there to prevent bugs that the type system cannot.


### Even more

* More efficient session handling.
* yesod-auth email plugin now supports logging in via username in addition to email address.
* probably more stuff we forgot to mention



## Conclusion & more info

FPComplete's development of the [School of Haskell](https://www.fpcomplete.com/school/how-to-use-the-school-of-haskell) has been great for the Haskell community to keep spreading knowledge. It has also been running with the changes for 1.2 for quite a while which should contribute to making 1.2 a more stable release.

[The high-level changelog](https://github.com/yesodweb/yesod/wiki/Changelog#yesod-12-not-yet-released) has been discussed in high-level here. [Detailed changes are here](https://github.com/yesodweb/yesod/wiki/Detailed-change-list#not-yet-released-yesod-12)

The book documentation for [1.2](http://www.yesodweb.com/book-1.2) has been started, but still needs more work to get fully up to date.

Most of the changes to upgrade your site to 1.2 should be fairly mechanical. I started a [wiki page for the upgrade](https://github.com/yesodweb/yesod/wiki/1.2-upgrade). If you have any issues, please note them there or on the mail list.

We hope you enjoy using Yesod 1.2

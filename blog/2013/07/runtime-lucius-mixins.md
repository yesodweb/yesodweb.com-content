About two months ago, [I announced that Lucius now had mixin
support](http://www.yesodweb.com/blog/2013/04/mixin-support-in-lucius).
Unfortunately, it was missing something important: support in runtime Lucius.
Many of you have probably never used runtime Lucius, but it's the component
underlying Lucius's ability to do live code reloading during development. So
without this feature, it's impossible to use mixins when using `yesod devel`.

As of `shakespeare-css` 1.0.6.1, this is no longer a problem: mixins should now
work perfectly with `yesod devel`. In order to take advantage of this, just add
a minimum bound on your shakespeare-css constraint in your cabal file. (The
next release of yesod-platform will include this change.)

If anyone finds any problems, let me know.

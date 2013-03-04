Heads-up everyone: I just release a new version of the yesod-platform to
Hackage. Notable updates in this release include:

* Luite has made a number of improvements to `yesod devel`, such as using `cabal-install`'s improved dependency solver.
* A number of added API functions for yesod-test, thanks to Paul Rouse and Shane Kilkelly.
* Update to conduit 1.0.0.
* Some scaffolding improvements for a better logging experience.
* The scaffolding tool now includes a PostgreSQL + Fay scaffolding option. This option is still considered experimental, but I encourage people to give it a shot. (I've been using Fay quite a bit for some [work projects](http://haskell.fpcomplete.com), and it's working out very well. Thanks Chris!)
* As usual, a bunch of improvements to our underlying library set, too numerous to be listed.

As a reminder: yesod-platform provides a stable, tested set of packages which
are known to compile together and interact well. If you're looking for
stability and don't mind using libraries that are one to two months old, it's
highly recommended. If you want to be using more recent libraries, then just
install the `yesod` package directly instead.

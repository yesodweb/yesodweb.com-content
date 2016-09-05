After having completely forgotten to do this for a long time, I
finally set aside some time last night to fix up the Travis CI for the
[Yesod scaffoldings](https://github.com/yesodweb/yesod-scaffold). We
have a number of different flavors of scaffoldings (e.g., PostgreSQL,
MySQL, simple, and minimal), and keep all of the different flavors as
branches on a single repo so that improvements to the base scaffolding
can easily be merged into all of the others. The goal of my changes was to:

* Have Travis check against the different snapshots likely to be
  selected by the `stack new` command
* Automate testing against live databases, so that I can be lazy and
  not set up those databases for local testing

Overall, this went pretty smoothly, and also serves as a nice example
of a short [Stack](https://haskellstack.org)-based Travis
configuration. You can
[see the latest PostgreSQL Travis configuration](https://github.com/yesodweb/yesod-scaffold/blob/postgres/.travis.yml).

Beyond my desire to be lazy in the scaffolding release process, the
other obvious benefit is much more confidence when review PRs against
the scaffolding that things will actually work.

## Interesting discoveries

I discovered two things in this work that I hadn't realized
previously:

* Due to a dependency on a relatively recent yesod-auth version, only
  LTS Haskell 6 and up and Stackage Nightly are supported by the
  scaffolding. Therefore, for the build matrix, I haven't included LTS
  5 and lower.
* I was unaware of
  [a bug in GHC 8.0.1](https://ghc.haskell.org/trac/ghc/ticket/12130)
  which prevents the scaffolding from compiling. This bug has already
  been resolved upstream and the fix will be included in GHC 8.0.2. In
  the meanwhile, I've
  [blocked GHC 8.0.1 from usage in the scaffolding](https://github.com/yesodweb/yesod-scaffold/commit/527b2efbc15b7b9deda978f472004fe6cc5d17bb).

Upon reflection, this is probably a good thing: we are all but
guaranteed that a new user will start off with LTS 6, which is a well
tested set of packages and a very stable GHC version, leading to
hopefully little user friction when getting started.

A user _could_ in theory do something like `stack new foo yesod-simple
--resolver lts-5 --solver`, but I think it's fair to assume that
someone passing in such specific flags knows what he/she is doing and
we should just stay out of his/her way.

## Contributing

Now that CI is in better shape, this is a good time to remind everyone of how
to contribute to the Yesod scaffoldings. The PostgreSQL scaffolding is
considered the base, with the other flavors merging in changes from there. If
you want to make a change, please submit a PR to the `postgres` branch of the
[yesod-scaffold
repo](https://github.com/yesodweb/yesod-scaffold/tree/postgres). If you have a
patch which is specific to one of the scaffoldings instead (like changing MySQL
config settings), please submit it to that branch.

Note that it is _not_ supported to send pull requests against the `.hsfiles`
files in the [stack-templates
repo](https://github.com/commercialhaskell/stack-templates), as such changes
can't be properly tested, and will be overwritten the next time a change from
the yesod-scaffold repo is merged in.

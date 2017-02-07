I've made some changes in the past few days to the CI setup for the
[yesodweb/yesod repo](https://github.com/yesodweb/yesod) that I
thought contributors may be interested in knowing about.

* We were regularly running into build timeouts on OS X on Travis. To
  work around this, we no longer build benchmarks and Haddocks on OS
  X, and compile with
  `-O0`. [Relevant Travis changes](https://github.com/yesodweb/yesod/commit/07147f42c2cbb47fddc4c67587e629a74c22fdd6)
* I've (finally) bit the bullet and made the repo compile with `-Wall
  -Werror` enabled, at least for recent versions of GHC. From now on,
  PRs will need to maintain
  warning-cleanliness. [Relevant Travis changes](https://github.com/yesodweb/yesod/commit/52f67fb04bebd050aa27b4a13fe2ca73aedf25f2#diff-354f30a63fb0907d4ad57269548329e3R178)
* There's now an AppVeyor configuration, so PRs can be checked against
  Windows (in addition to the existing Linux and OS X coverage
  provided by Travis). This did, in fact, reveal
  [two](https://github.com/yesodweb/yesod/commit/954f813569278ea80df61942537c91c1ab79d78e)
  [issues](https://github.com/yesodweb/yesod/commit/1bc1ef5a35e0fae15ee990fa1876cdc91a2fc4e3)
  around line
  endings. [Relevant AppVeyor addition](https://github.com/yesodweb/yesod/commit/e90b31bb4a57d468c5d8428f7d84fe0ec31bea41).

Nothing major, just a few changes that contributors should be aware
of. Hopefully that green checkmark on a PR will now have a few less of
both false positives and false negatives.

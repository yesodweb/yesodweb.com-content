I just released a simple tool to Hackage called
[stackage-update](https://hackage.haskell.org/package/stackage-update). Instead
of repeating myself, below is a copy-paste of the README.md from [the Github
repository](https://github.com/fpco/stackage-update).

* * *

This package provides an executable, `stackage-update`, which provides the same
functionality as `cabal update` (it updates your local package index). However,
instead of downloading the entire package index as a compressed tarball over
insecure HTTP, it uses `git` to incrementally update your package list, and
downloads over secure HTTPS.

It has minimal Haskell library dependencies (all dependencies are shipped with
GHC itself) and only requires that the `git` executable be available on the
PATH. It builds on top of the
[all-cabal-files](https://github.com/commercialhaskell/all-cabal-files)
repository.

### Advantages

Versus standard `cabal update`, using `stackage-update` gives the following advantages:

* Only downloads the deltas from the last time you updated your index, threby requiring significantly less bandwidth
* Downloads over a secure HTTPS connection instead of an insecure HTTP connection
    * Note that the `all-cabal-files` repo is also updated from Hackage over a secure HTTPS connection

### Usage

Install from Hackage as usual with:

```
cabal update
cabal install stackage-update
```

From then on, simply run `stackage-update` instead of `cabal update`.

### Why stackage?

You may be wondering why this tool is called `stackage-update`, when in fact
the functionality is useful outside of [the Stackage
project](https://www.stackage.org/) itself. The reason is that the naming
allows it to play nicely with the other Stackage command line tooling.
Concretely, that means that if you have stackage-cli installed, stackage-update
works as a plugin. However, you can certainly use `stackage-update` on its own
without any other tooling or dependencies on the Stackage project.

### Future enhancements

* If desired, add support for GPG signature checking when cloning/pulling from the `all-caba-files` repo

I'm guessing almost every Yesod user has - at some point - used the
venerable `yesod devel` command, which launches a server which
auto-recompiles your source code on any file changes. This has been a
core part of the Yesod ecosystem for many years. Unfortunately, it's
had to be far more complicated than I'd have liked:

* Since it predates `addDependentFile` (good work Greg Weber on
  getting that in!), it has some pretty complex logic around guessing
  which external files (like Hamlet files) should force a recompile.
  (Adding support for `addDependentFile` to the current `yesod devel`
  is possible, but it's a non-trivial undertaking.)
* In order to ensure a consistent set of dependencies, it does some
  real fancy footwork around intercepting arguments passed to `ghc`
  and linker executables.
* In order to parse various files, it links against the `ghc` library,
  tying it to a specific compiler version. This makes things difficult
  for users (don't accidentally use `yesod` from GHC 7.10.3 with GHC
  8.0.1!), and sometimes
  [really](https://github.com/yesodweb/yesod/issues/1304)
  [painful](https://github.com/yesodweb/yesod/issues/1284) for
  maintainers.

For a few months now, I've been meaning to greatly simplify `yesod
devel`, but the maintenance burden finally gave me the excuse I needed
to bite the bullet and do it. The result is a
[dramatic simplification of the code base](https://github.com/yesodweb/yesod/pull/1305). First
I'd like to ask for user feedback, and then I'll discuss some of the
details of implementation.

## Please try it out!

Since this is such a big change, I'd really appreciate if others could
give this a shot before I release it. There are two ways you can do
this:

Method 1:

* `git clone https://github.com/yesodweb/yesod --branch 1304-stack-based-devel yesod-new-devel`
* `cd yesod-new-devel`
* `stack install yesod-bin`
* From your project directory, run `yesod devel`. NOTE: do _not_ use
  `stack exec -- yesod devel`, you want to use the newly globally
  installed executable, not the one from your snapshot!

Method 2:

* Add the following to your `stack.yaml` file's `packages` list:

  ```yaml
  - location:
      git: https://github.com/yesodweb/yesod
      commit: f3fc735a25eb3d5c051c761b59070eb9a0e4e156
    subdirs:
    - yesod-bin
    extra-dep: true
  ```
* Likely: add the following to your `stack.yaml` file's `extra-deps` list:

  ```yaml
  - say-0.1.0.0
  - typed-process-0.1.0.0
  ```

* `stack build yesod-bin`
* `stack exec -- yesod devel`

Use whichever method you feel most comfortable with. Please let me
know both successes and failures, and then I'll try to get this rolled
out. Comments would be great
[on the Github pull request](https://github.com/yesodweb/yesod/pull/1305).
So far, in my limited testing, I've found that the new `yesod devel`
runs faster than the current one, but that could very much be
confirmation bias speaking.

Note: there are a few removed features in this update, please see [the
changelog](https://github.com/yesodweb/yesod/blob/1304-stack-based-devel/yesod-bin/ChangeLog.md).

## How it works

The big change - as the branch name implies - was depending entirely
on Stack for all of the heavy lifting. Stack already provides a
`--file-watch` command to automatically recompile, and uses GHC's own
`addDependentFile` information to track external file
dependencies. This cuts out the vast majority of the
complexity. There's no longer any need to depend on the `ghc` library,
there's less `Cabal` library code involved (making cross-version
support much simpler), and almost everything is handled by shelling
out to external executables.

I also got to redo the concurrency aspects of this using my absolute
favorite package in the world:
[async](https://haskell-lang.org/library/async). The result is, in my
opinion, very straightforward. I also leveraged some of the newer
libraries I've worked on, like
[safe-exceptions](https://haskell-lang.org/library/safe-exceptions),
[typed-process](https://haskell-lang.org/library/typed-process), and
[say](https://haskell-lang.org/library/say).

The code is (finally) well commented, so you can
[jump in and look yourself](https://github.com/yesodweb/yesod/blob/master/yesod-bin/Devel.hs). I've
also added
[a decent README](https://github.com/yesodweb/yesod/tree/master/yesod-bin#readme),
and
[an example of using `yesod devel` with a non-Yesod project](https://github.com/yesodweb/yesod/tree/master/yesod-bin/devel-example#readme).

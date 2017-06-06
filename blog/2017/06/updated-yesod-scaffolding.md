A few days ago I released an
[update to the Yesod scaffolding](https://github.com/yesodweb/yesod-scaffold/pull/147). It's
nothing major, but it has some new niceness I thought people would be
interested in:

1. I've (finally) moved the Haskell source files into a `src`
   directory. I rejected some moves in the past. But since then, this
   style has become the dominant style in the Haskell world, and it
   makes sense to embrace it.
2. Instead of putting language extensions in the `default-extensions`
   field of the cabal file, they are now in `LANGUAGE` pragmas in each
   source file. This was not an obvious decision to make, and there
   are still people (myself included) who are conflicted on it. You
   can see some of the discussion of this on Twitter:

   <blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Where should the Yesod scaffolding put language extensions? <a href="https://t.co/6qZZm8gVhE">https://t.co/6qZZm8gVhE</a></p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/868863178205089792">May 28, 2017</a></blockquote><script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

3. We've moved from a cabal file to an hpack `package.yaml` file. I
   only started using hpack a few months back, but it's completely won
   me over already. For those not familiar, check out
   [the hpack repo](https://github.com/sol/hpack#readme). Note that
   hpack _generates_ a cabal file, so there is full compatibility with
   cabal-the-build-system. We just get some niceties, like
   [leaving off `exposed-modules`](https://twitter.com/snoyberg/status/868180822272200704).

Next time you create a scaffolded Yesod project (by running,
e.g. `stack new mysite yesod-postgres`), you'll automatically get this
updated scaffolding.

After sufficient complaining [ensued on Reddit after my previous blog
post](http://www.reddit.com/r/haskell/comments/2pwiq9/use_stackage_for_docs/),
and enough claims of "this is so trivial to implement," I decided to bite the
bullet and just implement it. After debugging a [lens bash
script](http://www.reddit.com/r/haskell/comments/2pwiq9/use_stackage_for_docs/cn179k7),
I [reimplemented it in pure
Haskell](https://github.com/yesodweb/cabal-src/blob/master/hackage-docs.hs) and
added it to [my mega repo tool
chain](http://hackage.haskell.org/package/cabal-src) to be part of my normal
release process.

So for everyone who thinks that Hackage is the right place to have
documentation: it's there again. I'm still quite unsatisfied with the whole
situation, and have wasted yet another hour of my life on what in my mind is
meaningless busywork. But so be it, it's frankly easier to solve a technical
problem than put up with the complaints. (I've certainly spent more than an
hour in the past four years explaining to people multiple times why docs on
Hackage don't exist for my packages.)

Now I'll put out a request: will someone *please* [implement READMEs on
Hackage](https://github.com/haskell/hackage-server/issues/262)? Yes, I could do
this, but I've never touched the code base, and have lots of other things to
do. The current situation of having to describe our packages at least three
times (synopsis, description, and README) is annoying, and the fact that some
forms of documentation cannot be expressed at all in description fields is very
problematic (see linked mailing list thread in that issue).

I hope I haven't just set a precedent that complaining at me gets me to do work
I don't like...

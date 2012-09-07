It's been a while since my last blog post, so I just wanted to give a small
community update touching on various things. I personally have been busy with
[some changes in my personal
life](http://photos.snoyman.com/?entry=20120826New_Baby_), and therefore I
haven't had as much time to work on code as I would like. But the community has
been incredibly active during my hiatus.

*   Perhaps most exciting for many Yesoders: Felipe has [released
    esqueleto](http://blog.felipe.lessa.nom.br/?p=68), an ESDL for generating
    SQL queries. It sits on top of Persistent, and makes it easy to create
    type-safe, complex SQL queries. I haven't used it for anything in production
    yet (it *was*, after all, just released), but the API is a work of art, and the
    examples are incredibly compelling.

*   Kazu has been putting through a bunch of refactorings and performance
    enhancements on Warp. I won't steal his thunder on that front, I think
    he'll be sharing some of that work himself.

*   Felipe has optimized our session handling code even more than he has
    previously. Thanks to Lorenzo Bolla and Greg for reporting. You can see the
    discussion [in the Github
    issue](https://github.com/yesodweb/yesod/issues/415).

*   Luite and Falco Hirschenberger have been moving the new GHC API-based
    `yesod devel` forward. Combined with the file watching code from Mark's
    GSoC project, this will hopefully give us a much more responsive dev
    environment.

Apologies for being less responsive than usual over the past few weeks, and
thanks to the community for picking up the slack and providing such quality
support on both the mailing list and Stack Overflow. I'm hoping to be back to
normal by the middle of next week... with a fair amount of stuff to catch up on
as well.

### GHC 7.6

For those who haven't heard, [GHC 7.6 has been
released](http://www.haskell.org/ghc/docs/7.6.1/html/users_guide/release-7-6-1.html).
A big congratulations to the whole GHC team. For Yesod users on Mac in
particular, this is a very important release: Luite's patch for [issue
#7040](http://hackage.haskell.org/trac/ghc/ticket/7040) has been included,
meaning that `yesod devel` no longer segfaults. (If you're using 7.4, the
recommended workaround is to use 32-bit GHC.)

I'd like to remind everyone right now of our GHC support policy. We only
*officially* support Yesod on the most recent version of the Haskell Platform,
meaning that for the moment GHC 7.6 is *not* a supported target. I know for a
fact (based on emails I've received) that many of our support libraries do not
currently compile on 7.6.

Over the next few weeks, we'll hopefully be able to get all of this worked out.
Most of these problems are very trivial: changing import lists, adding language
pragmas, etc. This is a great opportunity for people looking to make some
initial commits to the project to dig their teeth in. As always, pull requests
are very much welcome.

### FP Complete

I couldn't write a blog post without mentioning a bit of what's going on with
my new position at FP Complete. Most of what I want to talk about on that front
will deserve its own blog post at a later date, but there's one topic in
particular that I'd like to bring up with the Yesod community, at least to get
the seed planted.

One component that is currently lacking from the Yesod ecosystem, and the
Haskell ecosystem in general, is commercial add-ons. There is not currently any
standard, straightforward approach for writing, selling, purchasing, and using
commercial components, which hinders both the adoption of Haskell/Yesod as an
enterprise platform, and discourages some people from releasing high-quality
components for others to use.

One aspect of our offering at FP Complete will be to address this. I think this
will be a great opportunity for the existing Yesod community to spend more of
their time doing what they love: writing Haskell code. This project is still at
early planning stages, but we're very interested in getting community feedback.
If you would like to either write or use commercial components in some way,
please be in touch.

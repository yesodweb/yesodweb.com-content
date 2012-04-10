It's no secret that there are different approaches to web development in
Haskell. Arguably, the two most discussed frameworks in recent times have been
Snap and Yesod (with no offense intended to Happstack). I can't speak for the
Snap team, but from the Yesod team, our approach- initiated by myself- has been
to mostly ignore what's going on with Snap. We collaborate together where we
can (which is actually fairly often), but for the most part we are two
frameworks, heading in two directions, and there's no point at constantly
rehashing arguments about the "better" approach.

While in theory I think this tactic of letting each framework develop
separately is correct, I was sorely mistaken. As evidenced by the questions
raised by newcomers on the forums discussing the Yesod 1.0 release, it's
obvious that a major question on people's minds is: what makes these two
frameworks different?

I can understand that for some Yesoders, this was a bit disheartening. We have
no intention of comparing ourselves to Snap. We don't even see the two
frameworks as competing (more on that later). But the response of some of us in
both recent and past discussions has not always been of the calibre that it
should. I myself am guilty of the same, and I'd like to offer an apology for
offense given to developers and users of other systems.

Besides the obvious issues of decorum, there are two main reasons why asserting
Yesod's superiority is not an appropriate way to respond to questions:

1. Aggressively defending Yesod doesn't make us look any better.
2. Yesod is *not* superior to Snap.

Yes, read that second point again. I'll repeat it: __Yesod is not superior to Snap.__
They are different. Do I prefer Yesod? Absolutely. If not, I wouldn't
be working on it. Are there people out there who prefer Snap? Yes. But
certainly they simply haven't yet been englightened as to the superiority of
Yesod, right? No. See point 2 again.

Yesod provides a high level interface for web development based on simple DSLs
for routing and persistence, an indentation-sensitive templating system, and
pervasive type safety. I'm less of an expert on Snap, so this could be wrong,
but Snap provides a powerful combinator approach to routing, a logic-free
system for templating (Heist), and (to my knowledge) doesn't get in the
business of persistence. They are superficially similar, but fundamentally
different.

I happen to have a lot of thoughts- which I haven't really shared with anyone
yet- on bridging the gap between the two approaches. Heist is in no way baked
into Snap, and could easily be used with Yesod. Similarly, Persistent (or
Happstack's acid-state) could be paired up with Snap. Yesod provides its own
combinator based form solution, but I've been curious to see what we could do
with digestive-functors, which (at least to my eyes) is the de facto forms
approach for Snap. Going even more low-level, Snap's combinator based
approach to routing could be used with Yesod. (To clarify: we can make Yesod
applications without any Template Haskell or Quasi-Quotations. I've done proofs
of concept before, and think this could be something interesting to discuss
later.)

But ideally, I could see integration run even deeper. There is absolutely no
technical reason we need two separate web servers. I've been pushing for
adoption of WAI by other frameworks for a long time. It's currently being used
by other frameworks like Webwire and Scotty, and by standalone applications
like Hoogle, but I would like Snap and Happstack to come to the table to
discuss the possibility of a truly universal interface. Can we achieve it
tomorrow? Probably not. But we should identify the things which are preventing
it.

These are all wonderful long-term goals. But in the short term, there's a lot
we can do to work together on a non-technical level. I think we all agree that
Haskell is a wonderful language, and would like to see its more mainstream
adoption (you know, fail to avoid success at all costs). Any bickering we have
(for which I take responsibility) only weakens Haskell's appeal. Which is
really a complete shame: the community is one of Haskell's strongest assets.
I've never worked with such an intelligent, cooperative, helpful, and friendly
group of people before.

Instead of ignoring what the other groups are doing, it's time to coordinate.
Newcomers are confused about which framework to start with? Instead of touting
our own virtues on Reddit and Stack Overflow, we should have a single Wiki page
that gives fair, unbiased, and comprehensive descriptions of the distinctions
amongst the frameworks. Someone loves Hamlet but wants to user acid-state,
while using Snap's routing? They should be able to ask us about it on
web-devel, and we'll talk it out and come up with a solid example of how it's
done.

It might seem like Snap and Yesod are sworn enemies who are devoted to each
other's destruction. But that's not the case. I've personally had very friendly
interactions with each and every main developer of Snap, and I both like and
respect them. I hope I've been able to inspire the same. (The same goes for
Happstack by the way, that's just not my focus right now.) There are no deep
rifts, blood feuds, or grudges amongst us, just some surface level fighting.

I say it's time to present a unified front. We need to continue the arguments
to help improve all the frameworks, but keep them professional. We're not here
to tear each other down, but to build all of us up. I hope I'll be able to help
repair any damage that I've done in the past, and hope all teams will join with
me now.

Recently the Haskell community has been engaged in a very intense discussion
around potential changes to the `Prelude` (aka "burning bridges" or "FTP").
[Here's the most recent incarnation of the discussion for
context.](http://www.reddit.com/r/haskell/comments/2vfczx/ghc_710_prelude_we_need_your_opinion/)
The changes under discussion are non-trivial, and many people are putting in a
huge amount of energy to try and make Haskell the best it can be. And to be
clear: I'm talking about people arguing on both sides of this discussion, and
people trying to moderate it. As someone who's mostly been sitting on the
sidelines in this one, I want to start by expressing a big thank you to
everyone working on this.

(If anyone's wondering *why* I'm mostly sitting this one out, it's because I
don't feel very strongly about it either way. I think there are great arguments
going both ways, and over the past week I've fluctuated between being -0.2 on
the proposal and being +0.2.)

When a big discussion like this happens, it's easy for people to misinterpret
it as something unhealthy. I'm here to remind everyone that, in fact, the
opposite is true: what we're seeing now is the sign of an incredibly healthy
community, based on an amazing language, that is undertaking extraordinary
things. And there are of course some warts being revealed too, but those are
relatively minor, and I believe we will be able to overcome them.

So to begin my cheerleading:

__The fact that we can even consider doing this is amazing.__ I don't think
very many languages could sustain a significant rewrite of their most core
library. Let's just keep things in perspective here: even the worst case
scenario damage from this change involves updating some documentation and
modifying a relatively small amount of code in such a way that will be
backwards compatible with old versions of the library. This is a true testament
not only to the power of the Haskell language, but to the thoughtfulness with
which this proposal was made.

__The discussion has been incredibly civil.__ This topic had all the makings
for an internet flame war: strongly held opinions, good arguments on both
sides, lots of time and effort involved, and Reddit. I am happy to say that I
have not seen a single personal attack in the entire discussion. Almost every
piece of discourse has been beyond reproach, and the few times where things
have gotten close to crossing the line, people on *both sides* have politely
expressed that sentiment, leading to the original content being removed.

To some extent, I think we're all a bit spoiled by how good the civility in the
Haskell world is, and we should take a moment to appreciate it. That's not to
say we should ever expect any less, but we should feel comfortable patting
ourselves on the back a bit.

__We're dynamically coming up with new, better processes.__ When opinions are
so strongly divided, it's difficult to make any kind of progress. As a
community, we're adapting quickly and learning how to overcome that. As you can
see in the thread I linked above, we now have a clear path forward: a feedback
form that will be processed by Simon PJ and Simon Marlow, who will make the
ultimate decision. This process is clear, and we couldn't be more fortunate to
have such great and well respected leaders in our community.

__Nothing else has stopped.__ If you look at issue trackers, commit logs, and
mailing list discussions, you can see that while many members of the community
are actively participating in this discussion, nothing else has ground to a
halt. We're a dynamic community with many things going on, so the ability to
digest a major issue while still moving forward elsewhere is vital.

* * *

That said, I think there are still some areas for improvement. The biggest one
lies with the core libraries committee, of which I'm a member. We need to learn
how to be better at communicating with the community about these kinds of large
scale changes. I'm taking responsibility for that problem, so if you don't see
improvements on that front in the next few weeks, you can blame me.

More generally, I think there are some process and communications improvements
that can be made at various places in the community. I know that's an
incredibly vague statement, but that's all I have for the moment. I intend to
follow up in the coming weeks with more concrete points and advice on how to
improve things.

In sum: Haskell's an amazing language, which has attracted an amazing
community. This discussion doesn't detract from that statement, but rather
emphasizes it. Like any group, we can still learn to do a few things better,
but we've demonstrated time and again (including right now!) that we have the
strength to learn and improve, and I'm certain we'll do so again.

I'm proud to be part of this community, and everyone else should be as well.

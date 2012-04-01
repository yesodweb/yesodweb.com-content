There's been a lot of discussion recently about ways to improve cabal.
While we can continue to work to add incremental improvements, such as
the upcoming and much-improved dependency solver, I think there's a
deeper philosophical issue at play, and I would like to explore that.

Let's start by analyzing the central philosophy underpinning cabal,
and its standard workflow. You connect to a central source (Hackage)
of code, download it to your system, build it, and use it. This seems
very straight-forward, and seemingly is the correct approach.

But now I want to sidetrack into some of Yesod's underlying
philosophy, and demonstrate its incompatibility with cabal. Many
people know that Yesod is Hebrew for "Foundation." What you may not
realize is that it's also a term from Jewish mysticism. Jewish
mysticism, also known as Kaballah, includes the concepts of receiving
energy from a source.

While this seems similar on a superficial level to what cabal does,
it's actually quite different. In cabal, you *download* code, telling
the source precisely what to send and when to send it. In Kaballah,
you would receive. Also, the Kaballistic approach would be that the
energy you receive is pure and perfect. Cabal instead has the audacity
to try and *build* the code it receives, perverting it into something
else!

So, to rectify this mismatch, the Yesod team is announcing the start
of a new build management tool: cabala.

## cabala workflow

Following Kaballistic patterns, we need to open ourselves to receiving
new code. To do so, we will run the command:

   cabala receive

This command will block until the cabala server sends a new package.
Of course, cabala is not so arrogant as cabal. You do not get to
choose which package will be downloaded, the source will determine
this automatically for you.

As hinted at above, cabala will not be so impudent as to try to
"improve" the code by building it. That is, of course, blasphemy:
Haskell is the One True Code that created even the angels:

> יצרם בדעת בבינה ובהשכל (He create them with knowledge, understanding, and Haskell)

Instead, the code is kept in its original, pure form. Ultimately, we
hope to build a machine capable of running unmodified Haskell. Until
then, we will follow another Kaballistic principle: random pieces of
the code you have received will be displayed to your screen, allowing
you to meditate on their meaning.

## Outcome

We can guarantee with 100% certainty that there will be an immediate
drop in cases of dependency hell to 0. Since cabala will perform no
dependency analysis, it's simply not possible. Additionally, we expect
instances of build failures to go down to 0 as well.

Another interesting improvement will be performance. We guarantee that
all programs will run in under 1 second with the new system.

There is, of course, a downside. It may take a significant amount of
time to prepare yourself to properly receive cabala. However, this is
a one-time cost, and the extreme long-term benefits more than amortize
it away. As Haskellers, we should already be used to high learning
curves.

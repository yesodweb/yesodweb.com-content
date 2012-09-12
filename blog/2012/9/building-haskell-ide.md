Our overarching directive at FP Complete is to promote the commercial adoption
of Haskell. We're approaching this by looking at what commercial needs are not
being met currently, and trying to fill in the gaps. One of these areas is an
IDE.

Some of you out there are probably thinking, "What gap? I don't even want an
IDE." I usually fit into that category myself. As everyone knows, Vim is the
only text editing software anyone ever needs, though of course Emacs is a
pretty good OS.&lt;/flame-war&gt;

The Vim/Emacs mentality works great for many Haskellers. And it may even be
that the majority of *current* Haskellers don't use an IDE. But that's almost
certainly [selection bias](http://en.wikipedia.org/wiki/Selection_bias). Since
there aren't yet any fully-featured Haskell IDEs<sup>\*</sup> that are easy to get started
with, Haskell attracts people who are comfortable without an IDE. Or new
Haskellers quickly learn to make do without one.

That may have seemed like a very long way to say "we're building an IDE," but
there's an important point I'm trying to stress. We're building an IDE for a
specific demographic: newcomers to Haskell who are more comfortable in an IDE
than in a text editor. This is a very important point to keep in mind. Some of
the features that newcomers will want may not be appealing at all to an
experienced Haskeller. And on the flip side, the advanced features some
Haskellers may desire would likely be overkill for newcomers. (That doesn't
mean we can't implement them, but it does change priorities.)

With all that said, let me finally state the purpose of this blog post: I want
to share with the community our initial design goals, and get community
feedback on what you think should be added or altered.

\*Note: I am fully aware that there are other projects attempting to create
Haskell IDEs, such as [EclipseFP](http://eclipsefp.github.com/) and
[Leksah](http://leksah.org/). I'm not trying to downplay those projects, but I
believe as more details come out about what we're offering, the
apparent overlap between the projects will decrease.

## Basic functionality

To start off with, we'll need the basics: create projects, edit files, and
build. We'll have some basic project templates (a Yesod application for one,
but there will be others). There will also be some built-in commands for common
activities (such as the `yesod add-handler` command).

Text editing itself needs to have all of the usual suspects: common
keybindings, syntax highlighting, search/replace.

## Debugging/help

Like most IDEs, we'll be providing error messages in a separate pane. We're
also hoping to provide some "humanization" of error messages to make them less
intimidating (with the full error message still available for those who want to
see it). From each error message, we'll be providing some options, in
particular jumping to the relevant code and a "more information" link which
will go to an FP Complete Wiki page.

The UI isn't ironed out, but we'll provide *something* along the lines of
hovering over an identifier to get its type, along with links to where the type
is defined or online documentation. This very likely will go beyond simply
linking to the Haddocks: we'll likely have links to cookbooks explaining common
ways to use different types and functions.

## Project management

A prime goal is to avoid the need for users to be editing cabal files manually.
(Actually, we have an even more general goal of completely removing cabal
dependency hell from users, but that will have to wait till another blog post.)
Most of what goes on in a cabal file is tedious, which is exactly the kind of
stuff we want to avoid. So the IDE will automatically handle:

* Keeping track of the list of modules to build.
* Package dependencies.
* Various GHC flags (e.g., optimization level, depending on release vs debug builds).
* Language pragmas.

That last one might be a bit more controversial, so let me explain. I think one
of the truly terrifying experiences for new users is to open up a file, and
have the first 15 lines be LANGUAGE pragmas. (I think the *next* terrifying
experience is when the following 40 lines are import statements.) Having to
remember to put the LANGUAGE pragmas in each file is always a pain. For my
current projects, I still take that approach, as it means the code is GHCi
friendly. But in our new IDE world, we'll be able to handle that kind of stuff
automatically.

As many of you know, the Yesod scaffolded site already encourages you to put
your language pragmas into the cabal file instead of the individual source
files. This works nicely because it's very unlikely that you'd want to use GHCi
directly (that's what `yesod devel` is for). In our new IDE, I think we'll have
the same situation.

## Building

There should never be a need to drop down to the command line to build. Whether
you're debugging some code, profiling it, or building for production, it can
all be done through the IDE. Longer term, we're hoping to bundle quite a few
debugging and profiling tools to make the experience straight-forward.

This also ties in to deployment. We'll want to have easy deployment for web
applications. This will likely mean some integration with a build server as
well for production of binaries that are properly configured for your
deployment server. (In other words, something that builds appropriate `keter`
bundles.)

## Source control

We'll need to have built-in source control management, likely backing with a
popular providing such as Github. Initially, support for committing, pushing,
pulling, and merging will be sufficient, though as time goes on we'll want to
improve what we're offering.

## More advanced tools

There are lots of additional tools that we’ll want to be adding. Prime candidates include refactoring, debugging, and profiling. We’ll try to leverage existing tools (such as GHC’s built-in memory profiles) whenever possible, but a fair amount of this will be new endeavors.

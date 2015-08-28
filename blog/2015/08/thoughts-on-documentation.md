I write and maintain a lot documentation, both open source and commercially.
Quite a bit of the documentation I maintain is intended to be collaborative
documentation. Over the past few years, through my own observations and
[insights from
others](https://groups.google.com/d/msg/commercialhaskell/qEEJT2LDTMU/wU9O32lKWT8J)
(yes, this blog post is basically a rip-off of a smaller comment by Greg), I've
come up with a theory on collaborative documentation, and I'm interested in
feedback.

__tl;dr__: people don't seem to trust Wiki content, nor explore it. They're
also more nervous about editing Wiki content. Files imply: this is officially
part of the project, and people feel comfortable sending a PR

When talking about documentation, there are three groups to consider: the
maintainers, the contributors, and the readers. The most obvious medium for
collaborative documentation is a Wiki. Let's see how each group sees a Wiki:

*   Maintainers believe they're saying "feel free to make any changes you want,
    the Wiki is owned by the community." By doing that, they are generally hoping
    to greatly increase collaboration.

*   Contributors, however, seem to be *intimidated* by a Wiki. Most contributors
    do not feel completely confident in their ability to add correct content,
    adhere to standards, fit into the right outline, etc. So paradoxically, by
    making the medium as open as possible, the Wiki *discourages* contribution.\*

*   Readers of documentation greatly appreciate well structured content, and
    want to be able to trust the accuracy of the content they're reading. Wikis
    do not inspire this confidence. Despite my previous comments about
    contributors, readers are (logically) concerned that Wiki content may have been
    written by someone uninformed, or may have fallen out of date.

By contrast, let's take a different model for documentation: Markdown files in
a Github repository:

*   Maintainers have it easy: they maintain documentation together with their
    code. The documentation can be forked and merged just like the code itself.

*   Contributors- at least in my experience- seem to love this. I've gotten
    dozens (maybe even hundreds) of people sending minor to major pull requests
    to documentation I maintain on open source projects this way. Examples range
    from the simplest (inline API documentation) to the most theoretically most
    complex (the content for the Yesod book). Since our target audience is
    developers, and developers already know how to send pull requests, this just
    feels natural.

*   Readers trust content of the repository itself much more. It's more
    *official*, because it means someone with commit access to the project
    agreed that this should belong here.

This discussion came up for me again when I started thinking about writing [a
guide for the Haskell tool
stack](https://github.com/commercialhaskell/stack/blob/master/GUIDE.md). I got
halfway through writing this blog post two weeks ago, and decided to finish it
when discussing with other stack maintainers why I decided to make this a file
instead of another Wiki page. Their responses were good confirmation to this
theory:

Emanuel Borsboom:

> Ok, that makes a lot of sense to me.  We might want to consider moving
> reference material to files (for example, the stack.yaml documentation).
> Another nice thing about that is that it means the docs follow the versions
> (so no more confusion about whether the stack.yaml page is for current master
> vs. latest release).

Jason Boyer:

> I can confirm this sentiment is buried in me somewhere, I've definitely felt
> this way (as a user/developer contributing little bits). On a more technical
> note, the workflow with editing the wiki doesn't offer up space for review -
> it is a done deal, there is no PR.

While Wikis still have their place (at the very least when collaborating with
non-technical people), I'm quite happy with the
file-as-a-collaborative-document workflow (that- I again admit- Greg introduced
me to). My intended behavior moving forward is:

* Keep documentation in the same repo as the project
* Be liberal about who has commit access to repos

\* I've seen a similar behavior with code itself: while many people (myself
   included in the past) are scared to give too many people commit access to a
   repository, my experience (following some advice from Edward Kmett) with giving
   access more often rather than less has *never* led to bad maintainer decisions.
   Very few people are actually malicious, and most will be cautious about
   breaking a project they love. (Thought experiment: how would you act if you
   were suddenly given commit access to a major open source project
   (GHC/Linux/etc)? I'm guessing you wouldn't go through making serious
   modifications without asking for your work to be reviewed.)

The Yesod team is happy to announce the 1.1 release of Yesod, and the 1.0
release of Persistent. Yesod is a web application framework for Haskell, which
allows you to leverage type safety to help mitigate entire classes of bugs,
while allowing you to write your code quickly and concisely. Persistent is
Yesod's most commonly used data storage layer, featuring full compile time
checking of database interactions.

These releases maintain a large degree of backwards compatibility, and as such,
upgrade should be fairly simple. As users begin the upgrade process, we'll put
together a collection of notes for handling upgrades and post them here.

## Changelog

### Yesod

* Upgrade to conduit 0.5
* Hierarchical routing.
* Control of sitewide Hamlet settings in the scaffolding.
* Easily add additional template languages in `Settings.hs`.
* `yesod add-handler` automates the process of adding a new route, creating the handler module, creating stub handler functions, and updating `Application.hs` and the cabal file.
* `yesod keter` builds a keter bundle. With a config setting, it will also upload it for you.
* By default, response bodies are now fully evaluated before sending to avoid empty responses when pure code throws an exception. `DontFullyEvaluate` is provided to override this default.
* Better control of uploaded file handling, defaulting to temporary file system storage.

### Persistent

* Upgrade to conduit 0.5
* Sum types
* Support for `sqltype=...` attribute.

## A new initiative: add-ons

Yesod has a very strong set of core packages. We provide many features out of
the box with a standard platform install. And our development approach is very
open, allowing many people to submit features back into these core packages.

Overall, this has been a great approach. Users have a well known set of
libraries they can rely on, and those libraries are very featureful. However,
this centralized development approach has a few downsides:

*   It doesn't give much room for experimentation. Either the code makes it into
    the core packages, or it's not in at all.

*   There are only so many hours in the day, and only so many people on the
    Yesod core team. If every feature anyone ever dreams of has to go through
    us, we'll become a bottleneck.

We're not actually dealing with any form of technical problem. Yesod is already
designed to be highly modular. Most of the "core components" of Yesod, like
forms and authentication, are provided as separate packages anyway. There's
actually very little in the `yesod-core` package itself.

Greg and I have discussed this some, and we think it's time to start a new
initiative in the community: writing add-ons. An add-on approach has a number
of advantages:

*   A new contributor doesn't have to deal with any of the complexity of the
    main Yesod libraries, instead getting to focus on much smaller pieces of
    code.

*   There's no overview process necessary: you can write your code, put it on
    Github/BitBucket/wherever, upload to Hackage, and email the mailing list,
    without anyone needing to review your code. (You can of course still ask us
    to review code if you want.)

*   If you have some crazy idea you'd like to experiment with, you can do so.
    Since it's not part of the core packages, you can play around as much as you want.

So how do you create an add-on to Yesod? I'd say it breaks down into one of three
categories:

### A collection of helper functions

A great example of this is
[yesod-goodies](http://hackage.haskell.org/package/yesod-goodies). The idea is
to just take some commonly used code, or some code to interface with an
external system like Gravatar, and package it up. Often times, this code won't
even have anything to do with Yesod specifically: the Gravatar code in
yesod-goodies, for example, is just interested in generating the appropriate
URLs.

There are some things to keep in mind when writing this kind of code, such as
keeping your type variables more generic. For example, in a typical Yesod site,
you could write something like:

    someFunction :: Int -> Handler RepHtml

But in generic code, you can't use the `Handler` type alias. Instead, you'll
need to use `GHandler` (generic handler), and leave type variables empty for
the sub and master sites, e.g.:

    someFunction :: Int -> GHandler sub master RepHtml

### Pre-built Widgets

Remember that Yesod was built with reusable components in mind. One of the
strongest examples is `Widget`s, allowing you to bundle together HTML, CSS, and
Javascript. One possibility is to take some external API (like Google Maps) and
package up some front end to it (for example,
[Haskellers](http://www.haskellers.com/) has some location selecting code in
it).

Like `Handler`, you'll need to change your type signature a bit, e.g.:

    mapSelector :: GWidget sub master ()

And you can also use type classes to specify some additional requirements. For
example, if you're going to be using jQuery, you could write something like:

    usesJquery :: YesodJquery master => GWidget sub master ()
    usesJquery = do
        master <- lift getYesod
        addScriptEither $ urlJqueryJs master
        ...

### Subsites

The other major reusable component in Yesod is subsites. This allows you to
package up entire sets of route handlers and reuse them across multiple sites.
There's a very solid example of [a chat
subsite](http://www.yesodweb.com/book/wiki-chat-example) in the Yesod book.
Other existing subsites are for authentication and static files. And another
subsite others have been working on is an admin subsite (similar to Django's
admin system).

## Conclusion

I think this initiative will allow more users to become involved in Yesod
development, and let more interesting bits of code come out of the woodwork.
None of this is to say that the Yesod core team is going to start turning away
pull requests. Quite the contrary, we'll still happily be accepting added
features that are solid and stable. (And of course bug fixes and performance
enhancements will continue as well.) This also doesn't mean you're on your own:
if you have an idea and want some help bringing it to fruition, bring it up on
the mailing list, there are lots of great people there who can give amazing
feedback.

As this add-on process progresses, I'm also hoping users start writing more
blog posts about their experiences in writing these add-ons, and eventually we
can get a community-driven set of guides for new add-on writers. I also believe
that in the near future, there will be even more advantages available to add-on
authors, but that idea will have to wait for a later blog post.

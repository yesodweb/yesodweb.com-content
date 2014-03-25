A few weeks ago, there was a [pretty heated debate about the PVP on the
libraries mailing
list](http://www.haskell.org/pipermail/libraries/2014-February/022114.html).
I've seen a few different outcomes from that discussion. One is to reduce
dependency footprints to try and avoid some of the dependency problems.
(Another one is concrete proposals for changes to the PVP; I intend to post a
proposal as well in the coming days, but wanted to get the easier stuff out of
the way first.)

This blog post is about some plans I have for consolidating multiple packages
together, hopefully to result in simpler dependency trees without causing users
to have unneeded package dependencies- at least not too often. The reason I'm
writing up this blog post is to let the community know about these changes in
advance, and let me know if any of these changes will cause them problems.
Also, if I list a package as deprecated, and you'd like to take over
maintainership, please let me know.

One of the guiding principles I've used in setting this up is that I belive
some dependencies should be considered incredibly cheap. Depending on
`process`, for example, is a cheap dependency, since it comes with GHC. (Unless
you place restrictive bounds on process, which can instead *cause* dependency
problems.) For more information on these principles, please [read my
description](https://github.com/fpco/streaming-commons#dependencies).

I'll start at the bottom of the dependency tree and build up.

## streaming-commons, zlib-bindings, text-stream-decode, conduit and pipes

Currently, there are about six core conduit packages: conduit, zlib-conduit,
attoparsec-conduit, etc. In addition, for some of these packages, I've split
off helper packages providing functionality to be used by other streaming data
libraries as well, such as zlib-bindings and text-stream-decode.

I want to collapse that into just three packages. All of the streaming helpers
will end up in a new package,
[streaming-commons](https://github.com/fpco/streaming-commons). I've talked
with Gabriel Gonzalez about this, and we'll be collaborating together on
creating a high-quality, low-dependency library. This library will also include
some features currently baked into conduit itself, like lock-free Windows file
reading and directory traversals.

All of the conduit core packages on top of that would then be merged into a new
package, conduit-extra. So we'd end up with conduit, conduit-extra, and
streaming-commons. The only downside is that, if you only needed zlib support,
you'll now get a few extra packages as well. However, following the principle I
listed above, these extra dependencies should all be coming from the "basically
free" dependency category.

### Crazier ideas for streaming-commons

This may be taking the idea too far, but we could include some even more
advanced tooling in streaming-commons. This could include not only the data
types from xml-types and json-types- which provide both streaming and tree
based data structures for those data formats- but also attoparsec parsers and
blaze-builder renderers. This could allow quite a bit of the xml-conduit
codebase to be shared by the pipes world, for example.

I'm curious if people think this is a cool idea, or too radical (or both!).

## Deprecate failure, attempt, and safe-failure

This one's been on my list for a while, pending some details being worked out
with Edward Kmett. The goal is to completely deprecate failure and attempt in
favor of the exceptions package, and within exceptions split out `MonadThrow`
from `MonadCatch`.

This will also mean removing some redundant functionality from resourcet. It
will be nice to be rid of the custom `MonadThrow` and `MonadUnsafeIO` defined
there.

## http-client-\*

A few simple moves: merge http-client-multipart into http-client, and merge
http-client-conduit into http-conduit. The latter change will mean that it's a
bit more difficult to use http-client in conduit without depending on tls, but
that's a use case anyone has expressed interest to me in.

Another change I'm planning to do at the same time is to add a new module to
http-conduit, with an alternate API. There are a few places where I'm
dissatisfied with the current API, and this module will work as an experimental
next-gen http-conduit. I'm planning on keeping both versions of the API around
for quite a while for backwards compatibility, however. The changes I'm looking
at are:

* Avoiding ResumableSource
* Using `with`-semantics (like http-client) to avoid accidentally keeping connections open.
* Don't bake in the need for ResourceT
* Possibly use lenses for field modifiers on the Request data type.

## shakespeare

This change is pretty simple: collapse shakespeare, shakespeare-css,
shakespeare-js, shakespeare-text, shakespeare-i18n, and hamlet into a single
package. It made sense to keep these separate when APIs were changing rapidly,
but things are basically stable now.

## wai

Since they don't add any extra dependencies, I'd like to merge wai-test and
wai-eventsource into wai-extra. Once again, since we're dealing with stable
APIs, this shouldn't cause too much trouble.

I'm also considering deprecating wai-handler-devel, since it's no longer used
at all by `yesod devel`.

## Deprecate pool-conduit

pool-conduit used to be a resource pool implementation based on code in conduit. Since then:

* The actual resource pool implementation is provided by resource-pool.
* The code I used from conduit has moved to resourcet.

At this point, pool-conduit doesn't really have much in it. If there's code
that people are using from it, I'd like to get it merged into resource-pool
itself.

## yesod

The next iteration of Yesod will have a [significantly simpler dispatch
system](https://groups.google.com/d/msg/yesodweb/F4Lh9NDg_9w/NzjVODJgQxQJ).
This new code doesn't really make sense as a general-purpose routing tool, so
I'm planning on moving that code into yesod-core itself and deprecate
yesod-routes. I know there are other users of yesod-routes; I think it makes
sense to rename yesod-routes to do something like merging yesod-routes into
wai-routes, as yesod-routes has no Yesod-specifics in it.

Another minor change: merge yesod-eventsource into yesod-core. No extra dep,
and a stable API.

Finally, the big (and somewhat controversial) one: merge most of the yesod-\*
core packages into the yesod package itself. A number of year ago, we did
precisely the opposite. However, given API stability, I think the time has come
to simplify our dependency list again here. This will have the added benefit
that when a user reports "I'm using Yesod version 1.2.3", it will give us more
information.

## xml

I'm planning on deprecating dtd, uri-conduit, and xml-catalog, as I don't use
them any more and have no time to maintain them.

Another idea I'm playing with is merging html-conduit into xml-conduit. This
would add a tagstream-conduit dependency. Alternatively, perhaps
tagstream-conduit could be merged into xml-conduit as well.

## classy-prelude

Merge classy-prelude-conduit in with classy-prelude. Downside: classy-prelude
will depend on conduit-combinators, but that doesn't actually add more than 3
extra packages.

## Next step: trimming dependencies in general

I'm not planning on rushing any of these changes. The goal is to take them
slowly and methodically, and release changes incrementally. For example, after
the conduit changes are done, I'd release new versions of wai, yesod, etc, that
are compatible with both the new and old versions. Hopefully the user facing
changes will simply be tweaking import lists and cabal files, but users will be
able to take their time on this.

Ultimately, I'm planning on releasing new version of persistent (2.0) and Yesod
(1.4). You can see the [Persistent 2.0
goals](https://github.com/yesodweb/persistent/wiki/Persistent-2.0-goals). The
Yesod 1.4 release doesn't actually have any breaking changes planned, aside
from upgrading its dependencies.

One other thing I'm going to be doing in this process is a more general
trimming down of dependencies. I'll be going through yesod-platform to find
packages we don't need. I also want to avoid redundant packages if possible
(e.g., we only need one cryptographic hash packages). In many cases, as a
community, we have multiple package options available. I think a great move for
the community would be to start consolidating those options as well where it
makes sense. If I have concrete ideas on that, I'll certainly share them.

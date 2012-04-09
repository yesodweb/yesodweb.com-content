The Yesod team is very pleased to announce the release of Yesod 1.0. As
expected, this was a minor incremental upgrade over version 0.10. You can see
[a full changelog](https://github.com/yesodweb/yesod/wiki/Changelog) on the
Wiki. The only possibly confusing change I'm aware of is the meaning of
`approot` versus `host` in the config file; please see the changelog for more
details.

We're very proud of this release. With 1.0, we're signaling that we've achieved
feature maturity with an API we're happy with. We aren't stopping of course; we
already have plans for the next wave of features, mostly focusing on better
client side integration. But we consider 1.0 a strong foundation to build on
top of.

The rest of this post is intended as an introduction to Yesod for new users.

------------------------------------------------------

## Introductory Screencast

<iframe src="http://player.vimeo.com/video/39646807?portrait=0" width="640" height="356" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe><p><a href="http://vimeo.com/39646807">Yesod 1.0 Introductory Screencast</a> from <a href="http://vimeo.com/user1975429">Michael Snoyman</a> on <a href="http://vimeo.com">Vimeo</a>.</p>

## Why another web framework?

The main goal of Yesod is to provide robustness. We want a web framework that
helps you build secure sites and minimize production bugs. In this endeavor, we follow the
mantra that the compiler is your ally, *not* your enemy. To make this happen,
we use Haskell's strong typing to eliminate entire classes of bugs and security
holes. This applies to everything from creating valid links
([type-safe URLs](http://www.yesodweb.com/book/routing-and-handlers)), avoiding Cross
Site Scriping (XSS) attacks, and automatic data marshaling.

Those familiar with more commonly used statically typed languages, like
Java or C++, may have already decided that the extra safety is not worth it.
Two complaints from traditionaly statically typed languages are:

* They're just so verbose!
* I haven't written a single bug that would have been caught by a compiler.

We believe Haskell solves both of these complaints. Due to type inference, you
rarely have to give a type signature in your Haskell code. Many of us choose to
anyway, and consider it a form of documentation which is enforced by the
compiler. But in most cases it's optional.

As for the second point: it's true that the type systems in Java and C++ make
it difficult to express program invariants well. In Haskell, however, the
expressive type system let's us do much more. If you've ever had an XSS
vulnerability, generated an invalid link, treated a stream of bytes as text, or
just made a typo, the compiler *can* help you. This means that in Haskell, you
don't have to waste time writing unit tests for the boring, mundane stuff. Let
the compiler handle it for you automatically, and you can worry about the more
important issues.

The result: Yesod is a web framework with a level of productivity rivaling
Rails or Django, but with greater security and much easier code maintenance.

### The other advantages

While robustness was our main goal in creating Yesod, we've also achieved other
major benefits as well:

* __Asynchronous made easy__. Everyone keeps talking about how important asynchronous programming is for creating servers. In other systems, you have to restructure your code to work with callbacks. Haskell's multithreaded runtime does all the heavy lifting for you. You write simple code that asks for data and sends it, and Haskell's compiler (GHC) will restructure it to use the appropriate event library for your OS. This allowed us to write a [powerful and fast webserver in about 500 lines of code](http://www.yesodweb.com/blog/2011/03/preliminary-warp-cross-language-benchmarks).
* __Scalable and performant__. Yesod lets you write simple, high-level code, and gives you good performance. But when you need more, you can tune your compiled code for something even faster. Many of Yesod’s base libraries work exactly this way: providing a nice, safe interface for users while getting near-C performance with direct memory access. The GHC compiler ensures we get fast machine code at the end of the day.
* __Light-weight syntax__. A lot of web development is boilerplate. Setting up routing tables, creating database schemas, and dealing with forms can all be long, repetitive code. Yesod’s has simple DSLs for templating, persistence, routing, and much more. But more importantly the DSLs are correct: they are all compile-time checked to get rid of the runtime bugs.

## Learn more

We'll be coming out with a few more introductory blog posts over the next few
weeks, and hopefully a few more screencasts too. If you're ready to jump in,
the [getting started guide](http://www.yesodweb.com/page/quickstart) gives you
all the information you need to get up-and-running with Yesod, as well as a
number of handy links.

And later this month, O'Reilly will be publishing our
[first book on Yesod](http://shop.oreilly.com/product/0636920023142.do). You can already
[read it online](/book).

<img src="http://akamaicovers.oreilly.com/images/0636920023142/cat.gif">

We're very happy with Yesod, and think it can be a valuable tool for many web
developers. We hope you do too!

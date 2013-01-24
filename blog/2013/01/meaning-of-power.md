As [I announced
recently](http://www.yesodweb.com/blog/2013/01/new-goal-yesod-1-2), the Yesod
team has started work on the upcoming 1.2 branch. I'm going to be following up
with a few blog posts to discuss some design decisions we have to make. To try
and frame this discussion better, I want to describe a little bit of the design
philosophy I had in initially creating Yesod, and see if that can help us make
our decisions.

## Definition of Power

Programmers often use a term in two very different- and often times
contradictory- ways. Consider the folowing two sentences:

* This library lets me create anything I want, it has so much power.

* I was able to use that library and create exactly what I needed in just a few
  lines of code, it's such a powerful library.

Both statements refer to a positive aspect of some library, but the positive
aspects are completely different. The first refers to the *flexibility* of a
library. The second is saying that the library is optimized for your use case
(for brevity, let's call this featurefulness).  And to a great extent, these
two concepts are at odds.

Let's take a simple example. Suppose you need to generate some HTML content.
One approach would be that my library provides you with the means to generate a
stream of bytes. This approach is incredibly flexible. You can generate any
character encoding you want. You can optimize your HTML by leaving off some
closing tags if you wanted. You can use single or double quotes for attributes
values. You can use decimal or hexadecimal numerical entities. And for that
matter, you could forget about HTML entirely, and just generate PNG or PDF
files if you wanted.

At the other end of the spectrum, I could provide you with an HTML templating
solution. In a few lines of code, you could have a fully valid HTML document.
You could specify at a high level the key-value attribute pairs for each
element, the contents of the element in textual form, and the library would
generate your HTML for you.

Which approach is more powerful? That's a meaningless question. Each approach
has an advantage over the other. They are each powerful in their own way,
whether flexibility or featurefulness. The question you should be asking is not
which is more powerful, but which one you should use.

## Layers of Abstraction

Another way to frame this discussion is to recognize that we're just talking
about layers of abstraction. Under the surface, my theoretical HTML templating
library is just generating the bytes for you. So we need some way to determine
what our best abstraction level is. And to do that, we need to analyze our
goals.

### Educating yourself

One goal which I've seen touted occasionally in the broader web framework
discussion is one of educating yourself. The argument goes that if you use a system that
does a lot of the work for you, then you don't learn anything. I frankly think
this is a meaningless argument, and not a helpful goal. All you've done in this
case is stated that you want to learn how to reimplement existing
functionality. If your goal is to understand how HTML is rendered, then forcing
yourself to use a raw-byte-generating library is a good idea. If your goal is
to create an HTML page, forcing yourself to learn the details of how rendering
is done is a needless distraction.

### Flexible enough for the problem at hand

So what other goals are available? Clearly, we need to be able to address the
actual problem at hand. For example, if our task is to generate a PNG file,
then the HTML library is not suitable, and we'd have to use the lower level of
abstraction of dealing with bytes. (This is of course ignoring the possibility
that there might be some other libraries in existance that could help with
PNGs.) Said another way, we need to make sure that we haven't sacrificed too
much flexibility given our goals.

### Simplicity

Another goal is to make the coding process as simple as possible. Simplicity
spells out in multiple ways: the code is shorter, easier to read, easier to
write, less buggy, more robust. I think it's an easy argument to make that an
HTML templating library will allow simpler code to be written than directly
producing a stream of bytes.

### Learnability

I can probably express my entire byte-generating library as a function `emit ::
ByteString -> IO ()`. That's incredibly simple to learn. By contrast, an HTML
library might have types to represent different kinds of nodes, attributes,
etc. This would imply that it is easier to learn the lower-level library.

While this may be true, it actually hides a lot. With the bytes library, you'll
need to learn the rules of HTML escaping, attribute quoting, nesting, etc.
So the breadth of the API does *not* always tell you how difficult it will be
to learn how to use something. If a task at hand requires a feature that a
lower level abstraction doesn't provide, you'll also need to learn how to
implement that yourself!

### Performance

Often times (but not always), high levels of abstraction can introduce a
performance cost. Weighing this against the benefits of the higher level of
abstraction is really a personal decision. In some cases (say a desktop app
used by one person that will be run once a month), the benefit of additional
performance is probably negligible. In software that will be calculating large
scientific calculations and will run for months on end on a cluster of
supercomputers, eeking out a 2% performance increase could equal a massive cost
savings.

So the *value* of extra performance is up for debate, but in all cases
performance is a good thing to have.

## Optimizing for the problem at hand

The main point I hope came across in the previous section is that __we need to
design solutions for the problem at hand__. Let's consider the question,
"What's better, C or Haskell?" I'm sure many readers would immediately say
Haskell, but that's not really a fair answer. Suppose that the problem at hand
is to add a tiny piece of functionality to a large C project. C is probably the
better tool in that case, as introducing a Haskell piece of code into this
codebase just wouldn't make sense. Similar reasons would be targeting a
platform without a Haskell compiler, or a memory constrained environment where
we cannot reasonably use garbage collection.

Bringing all of this back home: in the Yesod world, I've tried to identify a
number of different levels which users may want to have. Consider this
progression of levels of abstraction for creating a web application, starting
with the lowest:

* Direct I/O on a socket.
* network-conduit, allowing an abstraction over the streaming nature of the data.
* WAI, abstracting over the HTTP protocol itself.
* yesod-core, providing routing and common handler functions.
* yesod, adding in Persistent and Shakespeare for data access and templating.
* The Yesod scaffolding, tying it all together with a set of best practice folder conventions and helper functions.

The line can sometimes get a little bit blurred, but for the most part we have
a distinction between all of the different sets of goals a user might have.
Need to create a non-HTTP network application? network-conduit can provide what
you want. Need a basis for a full blown web app using a standard database and
templating solution? Use the scaffolding. And so on.

Once we've established what our goals are, we can then proceed to try to
optimize our solution based on the four points from the previous section:

*   We need to make sure that we have the *flexibility* to solve the entire
    problem. If `network-conduit` only allowed textual data, this would not be
    flexible enough, since many network protocols require binary data. On the flip
    side, we don't need to provide full flexibility higher up. The scaffolded site
    includes Persistent, it doesn't need to provide five other data storage
    mechanisms as well. If you want to use a different data model, go down a layer
    of abstraction. (Or in reality, use the scaffolding that doesn't include
    Persistent, but you get the idea.)

    But there's another point to make as well. If we can still provide the
    extra flexibility without hindering the user experience, we should. A prime
    example of that is the fact that Yesod will allow you to deal directly with the
    WAI protocol via `waiRequest` and `sendWaiResponse`. So really, our objectives
    are:

    * We *must* provide enough flexibility for the problem at hand.
    * We *should* make it possible to get extra flexibility if it doesn't destroy our abstraction.
    * We *could* make this extra flexibility easy to get at as well.

*   Once we've identified what features we're going to have, we should make it as
    easy as possible to access them. Let's take routing as an example. At the level
    of Yesod, we've created a system where routes are non-overlapping, represented
    as individual pieces of a requested path, and are fully decoded to text. We
    don't take the hostname or the query string into consideration for routing by
    default. And our routing syntax is possibly the simplest expression of these
    constraints.

    What if you need to be able to route based on hostname? There are approaches
    available, but they are not as simple. At a certain point, trying to get all of
    this extra flexibility into the system doesn't make sense, at which point it's
    more logical to drop a layer of abstraction to WAI. But this brings up another
    issue: if I need more flexibility in one area (e.g., routing), should I really
    have to give up simplicity in another one (e.g., templating)?

    To address that, Yesod is designed to be modular. The Shakespearean template
    system can be used with non-Yesod application as well, so you can have an
    incredibly flexible routing system together with a simple HTML templating
    solution.

    Could we augment Yesod's routing system to- by default- route based on the
    hostname? Certainly. But it would complicate our solution. We'd be getting
    extra functionality which we don't need for our stated problem domain, and
    giving up on simplicity. And that's not a good tradeoff.

*   Learnability comes down to two points. One is that there should be a subset of
    the API which addresses the most common needs. This overlaps very much with the
    goals of simplicity. You can create a lot of Yesod applications by just
    sticking to `defaultLayout` and `widgetFile`.

    The second aspect is documentation. Functionality needs to be well documented
    in how it works. But that's not sufficient. It needs to be easy for people to
    find what they're looking for. One aspect is highlighting the most common API
    subset in the documentation. Another is giving lots of examples, such as our
    cookbook. In many cases, people don't need to know all the details and every
    way to achieve a goal, but seeing a single best practice example is enough.

*   There's not much to say about performance. Make your code as efficient as
    possible without losing the necessary flexibility and simplicity.

## Concrete examples

To take this out of the abstract, I want to give some more concrete examples of
how these design goals have played themselves out in some of the libraries in
the Yesod ecosystem.

### conduit

The first question is what our use cases are. I was lucky in this case, since
we already had a large body of streaming code to use as a basis for determining
what features we needed. Note an important point here: when creating a library,
it's IMO completely meaningless to simply design a solution. You have to have
real cases in mind to guide the design, or you end up with libraries that seem
simple and elegant but don't solve the actual problems.

Based on what I'd implemented previously, I knew that we'd need the ability to
create data producers, consumers, and transformers. We'd want to be able to
escape the Inversion of Control that most streaming packages had. Buffering
(a.k.a., leftovers) was a requirement for the majority of use cases. As a
result, all of these features are first-class citizens of conduit.

There are other features that were left out because they were less commonly
used. `enumerator`'s `isolate` function, for example, is more powerful than
`conduit`'s. But getting that extra flexibility would greatly harm `conduit`'s
simplicity, and since that flexibility wasn't necessary, it was left out. On
the other hand, we absolutely needed to have guaranteed and prompt resource
finalization. Even though that complicated `conduit`, it had to be included.

Is `conduit` as efficient as directly reading and writing to file handles? No,
it introduces some overhead. Is it as easy to learn as a basic I/O API would
be? No, it has a larger API and more concepts to learn. However, it makes
resulting code simpler to read and write, and less bug prone. So the tradeoff
is (IMO) worth it.

### authenticate/yesod-auth

Let's say we want to implement OpenID login on a website. At a high level, I'd
simply like a library where I say "allow OpenID access" and a button magically
appears on the login page. But unfortunately there's a lot more to it than
that:

* I need to provide such a login page and insert the correct HTML.
* I need to create an OpenID completion route.
* I'll have to store user credentials in a database (or equivalent) somehow.

There are lots of different approaches to each of these. So in this case, I
decided to attack the problem by starting with the most general solution and
building more and more specialized solutions. The authenticate library itself
knows nothing about any web frameworks, and must be passed in GET and POST
parameters manually. It handles communications with the OpenID providers, and
introduces a type-safe interaction layer.

On top of that is yesod-auth, which includes an OpenID plugin. This *does*
fulfill the goal of "just say OpenID and it appears on the login page": the
YesodAuth typeclass has a member for declaring all of the plugins, and plugins
can provide HTML to be injected into the login page. However, there's still
quite a bit of machinery to getting yesod-auth itself running: integration with
the database, providing routes to access authentication, etc.

Finally, we have the Yesod scaffolding, which provides sensible defaults for
all of these. When you run the scaffolding, you have a fully functional
authentication system, and can quite trivially add in new authentication
options like OpenID.

What we have here is a very clear delineation of simplicity versus flexibility.
There are many use cases for which the scaffolding's authentication system
would not be appropriate, but for which you'd still want to have OpenID logins.
If you're using Yesod, you can stick with yesod-auth and still get a level of
simplicity. And if you're not using Yesod at all, authenticate is still
available to facilitate your OpenID logins, though you'll have to do a bit more
work (or rely on a different higher-level wrapper if available).

Designing libraries in this fashion can take a bit more effort, but in my
opinion providing reusable tools for the broader community is worth it. Beyond
any advantages for the community in general, this also makes code easier to
test, and a wider group of users can help push bug fixes and improved
featuresets even faster.

## Summary

* Figure out the problem you're actually trying to solve.
* Determine the flexibility necessary to solve that problem.
* Based on those required features, design as simple a programming experience as possible.
* If it's possible to add extra flexibility without comprimising simplicity, do it.
* Optimize as much as possible based on these constraints.
* Document, making it clear what the recommended approaches are.

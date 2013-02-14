I've been contemplating, coding, discussing, and writing (unpublished) blog
posts about the next version of conduit- off and on- for a few months now. I'm
going to try to get all of the important points for discussion in this blog post, starting with the most important (and hopefully interesting) for
users and digressing further into background information.

## Bad error message examples

I was planning on writing this blog post some time next week, but I realized
that [this Google+
post](https://plus.google.com/u/0/116553865628071717889/posts/1CykbXfUMYS) was
really something of a tease. But let me reiterate the same message I stated
over there:

> I'm working with +Felipe Lessa and +Dan Burton on a new version of conduit (more details to follow soon hopefully). The main goal is a simplified user interface. One of the most difficult parts of that is making error messages more helpful.

> Does anyone have examples of some conduit code that generates really unreadable error messages? We'd like to make sure the new code does better.

## Motivation

The most important question we need to ask ourselves for this release is: why
are we doing it? That response is the guiding principle for all the work we're
doing, and will be the barometer for whether we're doing a good job or not.

The motivation for this release is simple: simplicity. There are a few pain
points in the library right now that have irked me (and others) since conduit
0.5 was released, and this is our chance to clean those up. Before explaining
those pain points, let me emphasize something: I did *not* say that we're
hoping to add some cool new features, increase performance, or cure cancer.
Those are all great things, but outside the scope of this release.

So the pain points in the interface that I'm aware of are:

* conduit 0.5 introduced two interesting new features: optional leftovers and
  upstream terminators. That balooned our core datatype (`Pipe`) to having six
  type variables. Though users shouldn't have to deal with that type directly,
  it's sometimes inevitable, especially with error messages. This is the biggest
  pain point in the library that I'm aware of.

* We have two competing interfaces for conduit: the original
  Source/Sink/Conduit with the `$$` family of operators, and the newer
  pipes-inspired interface using `runPipe` and `>+>`. This duplication (though
  warranted in the current setup due to optional leftovers and upstream
  terminators) is confusing.

*   To try and mitigate this confusion, I created even more confusion. On top of
    the core Source/Sink/Conduit datatypes, we also have general versions of
    them. But we have to deal with a general version which has leftovers, and
    one which doesn't have leftovers. And one which returns the upstream terminator, and one
    that doesn't. This results in a total of __12 type synonyms__. Said another
    way:

    ![The number of type variables and type synonyms is too damn
    high](http://i.qkme.me/3szl66.jpg).

* The result is that it's difficult for users to know what the type signatures
  of their code should be. "Is this a GLConduit, or a GInfConduit, or just a
  Conduit?"

To put this [in terms of
power](http://www.yesodweb.com/blog/2013/01/meaning-of-power), we currently
have too much flexibility in our library. We need to trade that in for some
simplicity.

An important constraint we have in this move is to avoid a major upheaval.
conduit has been a stable library for over half a year, and I don't want to
destroy that stability. So- as much as possible- this change should be
backwards compatible. This has played out very well in practice in my testing,
as I'll describe later.

## The actual change

__Note:__ You can see the current codebase [on the producer branch of
Github](https://github.com/snoyberg/conduit/tree/producer).

I have to thank Felipe and Dan. I'd been playing around with around 6 different
ideas on how to solve this problem, and together we were finally able to sort
the wheat from the chaff. I believe the approach I'm going to describe is our
best option, and results in a very elegant library. We haven't completely
settled on this approach, but we're pretty close.

As you may have noticed from the problems listed above, optional leftovers and
upstream terminators cause a lot of complication. They happen to be great
features for reasoning about the internal workings of the library, but in
practice don't really help users out very much (I think I've only taken
advantage of upstream terminators in one really obscure piece of code). So the
first step is to get rid of those two features from the user-facing API.

We'll retain the Pipe datatype internally with all six type variables. However,
we'll introduce a new wrapper around it, ConduitM, which has only four type
variables: input from upstream, output to downstream, the underlying monad, and
the return value. Now the worst case scenario is that the user will see four
type variables in some error messages.  While not ideal, it's definitely
manageable. We also considered a typeclass based approach (see below) which
theoretically would have given even better error messages, but the overall
comparison made this approach seem to be the winner.

It's now easy to define our three core synonyms in terms of this ConduitM datatype:

```haskell
type Source m a = ConduitM () a m () -- no input values, no return value
type Sink a m b = ConduitM a Void m b -- no output values
type Conduit a m b = ConduitM a b m () -- no return value
```

This is great, but leaves us with the problem of creating general functions.
For example, suppose I want to write a `Conduit` such as the following:

```haskell
sourceList :: [a] -> Source m a

tripleOutput :: Conduit Int m Int
tripleOutput = awaitForever $ \x -> sourceList [x, x, x]
```

This example won't type-check: since `sourceList` is a `Source`, its input type
is forced to be `()`. But `tripleOutput` has an input of type `Int`, so it
won't work. What we really want is to state that sourceList can work with
*any* kind of input, and then it can be used as either a `Source` or a
`Conduit`. On the consumption side, we have a similar dilemna: we want to state that
something consumes a stream of input and produces a return value, but can
output *any* value it wants.

This is a perfect use case for RankNTypes. We can state that something produces a stream of data without specifying its input with:

```haskell
type Producer m a = forall i. ConduitM i a m ()
```

And similarly for consumption:

```haskell
type Consumer a m b = forall o. ConduitM a o m b
```

Now we can give a type signature `sourceList :: [a] -> Producer m a`, and our
example type checks. And thus we have a full API based on one concrete type
(ConduitM) and five type synonyms: Source, Conduit, Sink, Producer and
Consumer. From a user perspective, you would almost always use Source, Conduit,
and Sink, unless you're creating functions which will be used in both a Source
and Conduit (or Sink and Conduit). The core conduit libraries would be set up
with the generic types when possible.

We're still actually debating the names for these last two synonyms. The other
option is to stick with the current nomenclature and call them GSource and
GSink. I'd be interested in the community's thoughts on this, I'm very much on
the fence right now.

Notice that we only need to create generalized versions of Sources and Sinks.
Conduits are already as generalized as they need to be, and thus we're not
discussing any form of GConduit type synonym.

__Note__: We have alternate approaches to the Producer/Consumer approach
spelled out below, specifically: explicit generalizing functions, typeclasses,
and using the ConduitM type directly. You can see their downsides listed below.

### Measuring against our goals

So does this solution actually solve our stated goals? Let's see.

* The user will never have to interact with the 6-variable Pipe datatype,
  unless he/she wants to dig into the Internal module. Check.

* The pipes interface needed to be present to allow users to deal with optional
  leftovers and upstream terminators. Since those are no longer in the
  user-facing API, we can relegate the pipes interface to the .Internal module as
  well. Check.

* We've replaced 12 type synonyms with just 5. Three of them are integral to
  the library, so we've only added in 2 more, both of which have pretty clear
  meanings. Mostly check.

* We now have very clear guidelines on how user code should be written: use
  Source/Sink/Conduit unless you really need something else, and then use
  Producer/Consumer. Check.

### Updating user code

I've made clear many times in the past that I have a strong bias towards
real-world code to back up any claims. For each and every approach mentioned in
this blog post, I've tried migrating the entire Yesod ecosystem over. Most of
the other approaches resulted in some kind of complication. In the case of
this approach, there were virtually no complications, except for code dealing
directly with the .Internal module. For everything else, the upgrade guide
basically goes:

* If you're using the G\*Conduit types, replace them with Conduit.
* If you're using the G\*Sink types, replace them with Consumer.
* If you're using G\*Source, replace it with Producer.

And yes, it really should be that simple. Since most user code should only be
dealing with Source/Conduit/Sink, no updating may be necessary. In fact, for
the majority of the Yesod ecosystem, I was able to get the libraries to
simultaneously compile with both conduit 0.5 and 1.0. So this should be the
lowest-impact conduit update to date!

One last point: I'm going to take this upgrade as an opportunity to finally
remove the long-deprecated Data.Conduit.Util.\* modules, containing some long
outdated, harder-to-use, and less efficient helper functions. If you're still
using those modules, it's time to upgrade.

## What didn't work

To help explain why we arrived at the solution above given some of its
limitations, I wanted to describe (in brief) some of the other approaches we
tried.

### Just three types

In theory, we could just use the RankNTypes versions for Source and Sink as
well. Unfortunately, this results in quite a few unpleasant surprises:

* One example is that [normal function
  composition](https://github.com/yesodweb/wai/commit/eaf3aabe2389e90cc71392e77b2b81ce657e31a7#L6L70)
  can fail due to the fact that type inference is brittle with RankNTypes. In one
  place I had to replace `id` with `(\x -> x)`.
* Similarly, I needed to add a lot of type signatures all over the place,
  something we shouldn't be forcing on users.
* Error messages started to become pretty unreadable, referencing type
  variables that the user never wrote.

To spell it out a bit further: having the RankNTypes values be returned as a
result of a function never caused any issues, but having a RankNTypes value as
an *argument* to the function caused lots of pain. So as much as I'd love to be
able to just stick to just three types for simplicity, it's a *false*
simplicity: the complexity has merely been moved elsewhere. I think having the
two additional type synonyms for use in special cases where they do not
cause problems is the appropriate trade-off.

### Another failed attempt: typeclass

This is the approach I probably spent the most time on. It's pretty attractive:
error messages now mention things such as "Upstream m is not the same as Int",
which seems like a wonderful step forward. We can also have our conduit
functions work with arbitrary monad transformers on top of our
Source/Sink/Conduit types. And finally, Source/Sink/Conduit can all be newtype
wrappers, guaranteeing that error messages are always as concise as possible.

Unfortunately, the system fell apart:

*   We still run into the issue of generalizing code, so we must either resort
    to [ugly type
    synonyms](https://github.com/snoyberg/conduit/blob/conduit-1.0/conduit/Data/Conduit/Internal.hs#L842)
    or ugly type signature, e.g.:

        map :: MonadStream m
            => (Upstream m -> Downstream m)
            -> m ()

*   Once we generalize, the error messages are no longer pretty.

*   We can't use standard `lift` and `liftIO`, since we could be lifting
    through an arbitrary number of layers. Instead, we ended up with
    specialized `liftStreamMonad` and `liftStreamIO`.

### Explicit generalizing calls

Most of the complication we run up against comes from generalizing functions.
Another possibility would be to just make generalizing functions to convert
Sources and Sinks into Conduits. Then Source/Sink/Conduit could be unique types
and error messages and type signatures are clear. However, this was a burden
that doesn't seem to make sense to put on users. It would be a major step
backwards in conduit usability from where we are now.

So the trade-off we have instead is two extra type synonyms, and error messages
may occasionally display the four-variable data type.

### Just a single type

Forget the synonyms! We just have a single data type:

    newtype Conduit input output m result

Then we'd have:

    sourceFile :: FilePath -> Conduit i ByteString m ()
    map :: (a -> b) -> Conduit a b m ()

This is certainly workable, but subjectively we decided this was inferior to
the solution we ultimately came up with. I still think this is a good
contender, however, and can actually be achieved fully by just ignoring the
five type synonyms. I'd be interested what people think of this approach.

## conduit's niche

I actually wrote most of the ideas for this section in [its own blog
post](https://github.com/yesodweb/yesodweb.com-content/blob/master/blog/2013/02/upcoming-conduit-1-0-part-1.md),
but ultimately decided to just include a smaller section here at the end of
this post.

I get questions on a fairly regular basis about switching conduit for pipes or
io-streams (and in the past quite a few about comparing to enumerator). Those
packages are all in a similar solution space to conduit, but do not fully meet
its feature set. In some places, they provide functionality which we don't
require, and in others omit vital functionality.

The main purpose of this section is to spell out the design goals of conduit,
in particular through comparison with the other packages. Some particular
points about conduit:

*   conduit was not created in a vacuum. There was a large body of existing code
    and features we wished to add to it, and based on those requirements we created
    conduit. The pipes package in particular took a much more abstract approach in
    design. I have no objections to that approach, but it *does* result in quite a
    different set of trade-offs.

    For example, conduit has never claimed to follow Category laws, and in fact it
    does not. pipes is quite strict in its adherence. One difference that came to
    light recently was prompt finalization: in some cases, following the Category
    laws results in delayed finalization. For conduit and its pragmatic approach,
    this is unacceptable. For pipes, deviating from Category laws is unacceptable.
    Both approaches are valid, but also mutually exclusive, and conduit is
    unapologetic about its choices.

*   As is hopefully obvious from this blog post, conduit is focused on creating the
    most user-friendly API possible for its feature set. To achieve that, we'll
    bundle in the functionality that we support in a single set of operations.
    Leftovers and finalizers are bundled into the core datatype, so that users do
    not need to combine multiple concepts to get a working whole.

    Note that there have been some claims about other libraries being simpler than
    conduit. I agree that the type variable situation was overly complicated, but
    given that we're addressing that problem now, I believe conduit is the simplest
    library to use for its problem domain. Like io-streams, it is based on three
    primitives (await, yield, and leftover, which perfectly mirror their read,
    write, and unRead). In addition, conduit provides a robust library of helper functions to deal
    with common use cases.

*   Composability is a requirement. I disagree with the assertion that composable
    code == Category instance: composable means code can easily be reused in a
    logical way. enumerator provides this with its concept of Enumeratees, which
    can be combined with both Enumerators and Iteratees, for example. pipes and
    conduit make composability a first-class citizen.

    However, I think io-streams is *not* providing this adequately. When you create
    a transformer in io-streams, it must be focused on either an InputStream or an
    OutputStream, but cannot work on both.

*   connect-and-resume is absolutely vital in a number of complicated use cases,
    such as combining a web server and client to create an HTTP proxy. It's a major
    feature of conduit, and was actually the motivating case for creating conduit
    in the first place. I believe io-streams could provide this same functionality,
    but enumerator and pipes certainly don't.

*   We need to have exception safety. Whether you love them or hate them,
    exceptions *are* a reality of programming, and we need to deal with them. I
    consider ResourceT to be a very good solution to the problem. But contrary to
    some claims, ResourceT is *not* a prerequisite of exception safety in conduit.
    You can, for example, write:

    ```haskell
    import Data.Conduit (($$))
    import Data.Conduit.Binary (sourceHandle, sinkHandle)
    import System.IO (withBinaryFile, IOMode (..))

    main =
        withBinaryFile "input.txt" ReadMode $ \input ->
        withBinaryFile "output.txt" WriteMode $ \output ->
        sourceHandle input $$ sinkHandle output
    ```

    However, ResourceT provides quite a bit of flexibility that the standard
    bracket pattern does not allow, such as interleaved resource cleanup. That's
    why instead of considering ResourceT a hack, I consider it a great solution to
    the problem.

*   While conduit is designed primarily for I/O, it should support pure code as
    well. This is great for testing, and for creating libraries like xml-conduit
    which can- in a memory-efficient and resource-friendly manner- parse both
    in-memory and I/O-based documents. pipes and enumerator allow for this, but
    io-streams has a distinct I/O bias. (There's nothing wrong with targeting a
    specific use case, but it *does* exclude others.)

*   We want support for transformer stacks. Monad transformers are a great way to
    structure code and deal with complexity. I am absolutely of the opinion that by
    getting rid of support for monad transformers, we would simply be moving
    complexity from our library to the user. This is another case where only
    io-streams lacks support. And frankly, I don't believe that supporting a
    transformer stack adds any significant complexity to a library, so it seems to
    me as a bad trade-off.

On the other hand, there are some features which other libraries have which we
don't. Some interesting things from other approaches:

*   enumerator is the only approach which- without some external control structure
    like bracket or ResourceT- gives exception-safe resource handling for the data
    producer. It's in fact a major part of the design philosophy, and I don't think
    people give that enough credit. I think overall the complexity trade-off needed
    to achieve this is too high, but there's no doubt that it's a feature that
    others don't have.

*   pipes has been adding new features, like bidirectionality. I haven't seen solid
    real-world use cases that would benefit greatly from it, but I am interested in
    seeing how it progresses. Similarly, pipes recently added some support for
    handling exceptions inside a pipeline. In my experience, it has always made
    more sense to handle the exceptions outside the pipeline, but I'm interested to
    see how this progresses. I think the dual nature of pragmaticism in conduit and
    research and experimentation in pipes has already reaped great benefits, and
    will continue to do so.

*   io-streams has introduced a replacement for the Handle system for lower-level
    I/O. This is certainly something we can consider switching to in the future.
    For the most part, conduit just uses the standard Haskell I/O infrastructure
    (with exceptions to deal with file locking on Windows). I wouldn't be surprised
    to see an io-streams-conduit package in the future.

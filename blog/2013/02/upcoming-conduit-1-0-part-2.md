OK, I've written [two](http://www.yesodweb.com/blog/2013/01/meaning-of-power)
[posts](upcoming-conduit-1-0-part-1) in the abstract. Let's get down to the
code! The ideas you'll see here have not yet been decided, but I feel pretty
confident about them. You can see the work-in-progress code on the
[conduit-1.0](https://github.com/snoyberg/conduit/tree/conduit-1.0) branch.

## Problem Assessment

Let's talk a little bit of history. conduit lived its first few revisions with
three separate data types: Source, Conduit, and Sink. These types were very
straight-forward: a Source could only produce data, a Sink could consume a
stream of data and produce a single final result, and a Conduit could consume a
stream of data and produce a stream of data. Conduit and Sink could both return
leftovers to be consumed later, and Source and Conduit could never produce any
kind of terminating value.

I still really like that API. By having separate types, the type signatures and
error messages are incredibly straight-forward. There's no need to wonder about
whether a Sink will ever produce leftovers, or if a Conduit will infinitely
consume data.

The downside was a lack of composability. Consider a simple task: write a
Conduit which consumes a stream of FilePaths, and produces a stream of
ByteStrings containing the contents of all of the files. In modern-day conduit,
this is trivial:

```haskell
fileConduit :: MonadResource m => Conduit FilePath m ByteString
fileConduit = awaitForever sourceFile
```

Or [see the School of Haskell
version](https://haskell.fpcomplete.com/user/snoyberg/random-code-snippets/conduit-from-filepath-to-bytestrings)
if you don't believe me.

The problem is that in earlier versions of conduit, there was no unification
between Sources, Conduits, and Sinks, so sourceFile could not function as a
Conduit. Twan van Laarhoven [pointed
out](http://twanvl.nl/blog/haskell/conduits-vs-pipes) that these three types
could, in fact, be unified and thereby arrive at something very close to
pipes's Pipe datatype. And starting from conduit 0.4, that's the approach we've
taken. To make it all work, Source, Conduit, and Sink are now type synonyms
around Pipe.

In place of our 2 Source type variables (monad and downstream), 3 Conduit
variables (monad, upstream, and downstream), and 3 Sink variables (monad,
upstream, and final result), we now had 4 type variables representing the union
of all three.

In addition, we added in two interesting features in conduit 0.5: optional
leftovers, and upstream terminators. Each of these required a new type
variable. So we now have the infamous 6 type variables.

__Note__: If the next paragraph confuses you, don't worry, it should. I'm
stressing the lunacy of the current system.

And to make matters worse: if we want to write truly generic code, we can't use
the type synonyms! If we state `sourceFile :: FilePath -> Source m ByteString`,
we're stating that a `sourceFile` can never have upstream data coming in, so it
can't unify with a Conduit. One approach is to require users to write their
type signatures using the six-variable `Pipe` datatype... which is incredibly
difficult to understand. To "help" with this, I created the generalized type
synonyms. But *these* had to vary based on whether we had leftovers and whether
we were infinitely consuming.

So what we wanted was to be able to unify functions across different types.
What we got was a massively complex unified type providing far more power than
most people need, and obscuring what was actually going on. (Which is clearer:
`sourceFile :: FilePath -> Source m ByteString` or `sourceFile :: FilePath ->
Pipe l i ByteString u m ()`?) Error messages are confusing, and there are in
fact two interfaces available to users from the main module of the package!

## Back to Basics

I think our Pipe datatype is very nice, and provides a lot of power. I've
personally taken advantage of all of that power in one way or another. But the
vast majority of the time, it's overkill. So let's do something simple: we'll
hide it. The Pipe datatype should now be considered exclusively as internal
plumbing, and the average user just needs to deal with Source, Conduit, and
Sink.

But if thoses three types are type synonyms, we aren't actually hiding
anything: error messages will still bring up the Pipe type. So let's change
them to newtype wrappers instead. With this change, we'll have recaptured the
elegance of conduit 0.3, but internally unified things.

Internal unification is great (smaller codebase, more provably correct code),
but it doesn't solve the user facing issues, such as the sourceFile example
above. One approach would be to have some explicit conversion functions, e.g.:
`sourceToConduit :: Source m b -> Conduit a m b`. However, I think we can do
better, and get a few other benefits in the process.

### Minor issue: Monad instances

Actually, let's detour for just a moment and point out one other advantage we
have in the conduit 0.4 world: Sources, Sinks, and Conduits all form Monads,
which makes it significantly easier to program with them. I want to keep this
functionality, but the types of Source and Conduit won't allow it. The problem
is that the return type for Source and Conduit is always `()`, but we need to
let it be a variable.

Fortunately, there's
[prior](http://hackage.haskell.org/packages/archive/binary/0.6.4.0/doc/html/Data-Binary-Put.html#t:PutM)
[art](http://hackage.haskell.org/packages/archive/blaze-markup/0.5.1.4/doc/html/Text-Blaze.html#t:Markup)
for this issue, so we have a relatively easy solution:

```haskell
newtype SourceM o m r
type Source m o = SourceM o m ()
```

This will complicate error messages a bit, but the power of the Monad instances
is just too great to give up.

## MonadStream typeclass

OK, back to our dilemna. We have some operations that we would like to work
over multiple, distinct datatypes. When stated that way, the solution is
obvious: we need a typeclass. And now in turns out that I'm eight month late
catching up to Chris Smith, who [proposed the MonadStream
typeclass](https://github.com/cdsmith/my-pipes/blob/master/Pipes.hs#L92) back
then.

We can throw all of our primitives (await, leftover, yield, and a few others)
into this typeclass, and then create instances for Source, Conduit, and Sink.
Then using `yield 5` could either be a Source or a Conduit. `await` could work
for both a Conduit and a Sink. And to make our streaming abstraction work like
other monad transformers, we can create MonadStream instances of any arbitrary
transformers sitting on top of a Source, Conduit, or Sink.

So now our `sourceFile` type signature looks like:

```haskell
sourceFile :: (MonadStream m, MonadResource (StreamMonad m), Downstream m ~ ByteString)
           => FilePath
           -> m ()
```

That's a lot more complicated than our non-generic version. It seems we've
(again) lost the simplicity of conduit 0.3. However, we can throw in three type
synonyms to help us regain our sanity. MonadSource, MonadConduit, and MonadSink
make the input, output, underlying monad, and intended use of a function much
more obvious. With them, the above becomes:

```haskell
sourceFile :: MonadResource m
           => FilePath
           -> MonadSource m ByteString
```

The only change in type signature is swapping `MonadSource` for `Source`.

As far as end users go, there's no need to ever deal directly with the Monad*
variants of the types, they are only necessary for creating general-purpose
libraries. If you want to just use conduit for your own application, you can
get away with just the simple Source, Conduit, and Sink types.

## Backwards compatibility

A major concern in this move is minimizing breakage. Fortunately, it looks like
the majority of code out there will continue working. Here are the exceptions:

* I (finally) removed the long-deprecated Data.Conduit.Util.* modules. If
  you've been clinging to those, it's time to move on.
* Anyone dealing directly with the Pipe constructors will need to deal with the
  newtype wrappers.
* There are some really rare cases of MonomorphismRestriction causing code to
  not compile.
* The Pipe operators (>+> and <+<) are no longer exported by Data.Conduit, and
  will not work on the new Source, Sink, and Conduit types. Similarly, if you
  were taking advantage of upstream terminators or optional leftovers, you might
  have to change your code.

I believe that Yesod 1.1 will be able to support both conduit 0.5 and 1.0
simultaneously, which should help the migration process.

## Conclusion

I'm very excited about this upcoming release. I've missed the simple interfaces
we've had in the past, and hope bringing them back will make it even easier for
users to take advantage of conduit.

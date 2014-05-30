Duncan Coutts kicked off a discussion on the core libraries mailing list in
April about exception handling in monad transformers. We made a lot of headway
in that discussion, and agreed to bring up the topic again on the libraries
mailing list, but unfortunately none of us ever got around to that. So I'm
posting a summary of that initial discussion (plus some of my own added
thoughts) to hopefully get a broader discussion going.

The initial thread kicked off with a link to ghc's
[ExceptionMonad](http://www.haskell.org/ghc/docs/latest/html/libraries/ghc-7.8.2/Exception.html#t:ExceptionMonad)
typeclass, which encapsulates the ability to catch exceptions, mask async
exceptions, and guarantee cleanup actions (a.k.a. bracket/finally). The
question is: is there a canonical way to do the same thing, without depending
on the ghc library?

As is usually the case in the Haskell ecosystem, the answer is that there are
about five different packages providing this functionality. I'd break them down
into two categories:

* Packages which define a typeclass (or set of typeclasses) specifically for
  exception handling. Such examples include [MonadCatchIO-mtl](http://hackage.haskell.org/package/MonadCatchIO-mtl),
  [MonadCatchIO-transformers](http://hackage.haskell.org/package/MonadCatchIO-transformers), and [exceptions](http://hackage.haskell.org/package/exceptions).
* Packages which define a generic way to embed a monad transformer inside the value, and thereby perform *any* control operation in the base monad. Examples are [monad-peel](http://hackage.haskell.org/package/monad-peel) and [monad-control](http://hackage.haskell.org/package/monad-control) (or if you want to go really far in time, [neither](http://hackage.haskell.org/package/neither-0.1.0/docs/Control-Monad-Invert.html)).

Fortunately, most of those options have been deprecated in favor of
alternatives. Today, there are really two choices: exceptions and
monad-control. I'd like to describe these in a bit more detail, and contrast
some of the pluses and minuses of both approaches. My overall goals are
twofold:

* Get more knowledge out there about the advantages of the two libraries.
* Work towards a community consensus on when each library should be used.

I'm interested in the latter point, since having a consistent usage of the
`MonadBaseControl` vs `MonadMask` typeclasses in various packages makes it
easier to reuse code.

Note: I don't mean to take credit for the ideas that are expressed in this blog
post. As I said, it's a combination of summary of a previous discussion (mostly
amongst Duncan, Edward, and myself) and a few new thoughts from me.

## exceptions

The exceptions package exposes three typeclasses, all specifically geared at
exception handling, and each one incrementally more powerful than the previous
one. `MonadThrow` is for any monad which can throw exceptions. Some examples of
instances are:

* `IO`, where the exception becomes a runtime exception.
* `Either`, where the exception becomes the `Left` value.
* `Maybe`, where an exception results in `Nothing`.
* Any monad transformer built on top of one of those. (Note also that there's a special `CatchT` transformer, which keeps the exception in the transformer itself.)

In addition to just throwing exceptions, you often want to be able to catch
exceptions as well. For that, the `MonadCatch` typeclass is available. However,
some monads (in particular, `Maybe`) cannot be `MonadCatch` instances, since
there's no way to recover the thrown exception from a `Nothing`.

The final typeclass is `MonadMask`, which allows you to guarantee that certain actions are run, even in the presence of exceptions (both synchronous and asynchronous). In order to provide that guarantee, the monad stack must be able to control its flow of execution. In particular, this excludes instances for two categories of monads:

* Continuation based monads, since the flow of execution make ignore a callback entirely, or call it multiple times. (For more information, see [my previous blog post](http://www.yesodweb.com/blog/2014/05/exceptions-cont-monads).)
* Monads with multiple exit points, such as ErrorT over IO.

And this is the big advantage of the exceptions package vs MonadCatchIO: by
making this distinction between catching and masking, we end up with instances
that are well behaved, and `finally` functions that guarantee cleanup happens
once, and only once.

One design tradeoff is that all exceptions are implicitly converted to `SomeException`. An alternate approach [is possible](http://hackage.haskell.org/package/failure-0.2.0.2/docs/Control-Failure.html), but ends up causing many more problems.

## monad-control

monad-control takes a completely different approach. Let's consider the `StateT
Int IO` monad stack, and consider a function

    foo :: StateT String IO Int

Now suppose that I'd like to catch any exceptions thrown by `foo`, using the
standard `try` function (specialized to `IOException` for simplicity):

    tryIO :: IO a -> IO (Either IOException a)

Obviously these two functions don't mix together directly. But can we coerce
them into working together somehow? The answer lies in exposing the fact that,
under the surface, `StateT` just becomes a function in `IO` returning a tuple.
Working that way, we can write a `tryState` function:

```haskell
tryState :: StateT String IO a -> StateT String IO (Either IOException a)
tryState (StateT f) = StateT $ \s0 -> do
    eres <- tryIO $ f s0
    return $ case eres of
        Left e -> (Left e, s0)
        Right (x, s1) -> (Right x, s1)
```

(Full example [on School of Haskell](https://www.fpcomplete.com/user/snoyberg/random-code-snippets/trystate).) The technique here is to:

1. Capture the initial state.
2. Use `tryIO` on the raw `IO` function.
3. Case analyze the result, either getting an exception or a successful result and new state. Either way, we need to reconstitute the internal state of the transformer, in the former by using the initial state, in the latter, using the newly generated state.

It turns out that this embedding technique can be generalized in two different ways:

* It applies to just about any `IO` function, not just exception functions.
* It applies to many different transformers, and to arbitrarily deep layerings of these transformers.

For examples of the power this provides, check out [the lifted-base
package](http://hackage.haskell.org/package/lifted-base), which includes such
things as thread forking, timeouts, FFI marshaling.

This embedding technique does *not* work for all transformers. As you've
probably guessed, it does not work for continuation-based monads.

## Compare/contrast

Even though these libraries are both being proposed for solving the same
problem (exception handling in transformer stacks), that's actually just a
narrow overlap between the two. Let's look at some of the things each library
handles that the other does not:

* exceptions allows throwing exceptions in many more monads than monad-control works with. In particular, monad-control can *only* handle throwing exceptions in `IO`-based stacks. (Note that [this actually has nothing to do with monad-control](http://hackage.haskell.org/package/lifted-base-0.2.2.2/docs/Control-Exception-Lifted.html#v:throwIO).)
* exceptions allows *catching* exceptions in many more monads as well, in particular continuation based monads.
* monad-control allows us to do far more than exception handling.

So the overlap squarely falls into: throwing, catching, and cleaning up from
exceptions in a monad transformer stack which can be an instance of
MonadMask/MonadBaseControl, which is virtually any stack without
short-circuiting or continuations, and is based on `IO`.

Given that the overlap is relatively narrow, the next question is- if you have
a situation that could use either approach- which one should you use? I think
this is something that as a community, we should really try to standardize on.
It would be beneficial from a library user standpoint if it was well accepted
that "oh, I'm going to need a bracket here, so I should use MonadXXX as the
constraint," since it will make library building more easily composable.

To help kick off that discussion, let me throw in my more subjective opinions
on the topic:

* exceptions is an easier library for people to understand. I've been using
  monad-control for a while, and frankly I still find it intimidating.
* If you're writing a function that might fail, using `MonadThrow` as the constraint can lead to much better code composability and more informative error messages. (I'm referring back to [8 ways to report errors in Haskell](http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/).)
* exceptions allows for more fine-grained constraints via MonadThrow/MonadCatch/MonadMask.
* monad-control makes it virtually impossible to write an incorrect instance. It's fairly easy to end up writing an invalid `MonadMask` instance, however, which could easily lead to broken code. This will hopefully be addressed this documentation and education, but it's still a concern of mine.
* monad-control requires more language extensions.
* While there are things that exceptions does that monad-control does not,
  those are relatively less common.

Overall, I'm leaning in the direction that we should recommend exceptions as
the standard, and reserve monad-control as a library to be used for the cases
that exceptions doesn't handle at all (like arbitrary control functions). This
is despite the fact that, until now, all of my libraries have used
monad-control. If the community ends up deciding on exceptions, I agree to
(over time) move my libraries in that direction.

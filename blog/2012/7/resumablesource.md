One of the more advanced features of `conduit` is connect-and-resume. I've
discussed the motivation for this elsewhere, but it essentially comes down to
simplifying interleaving of input streams. One very practical ramification of
this is that the `http-conduit` API can be drastically simpler than the
previous `http-enumerator` API.

For the most part, connect-and-resume is a technique which hides under the
surface, and you don't have to really think about it too much. Starting with
`conduit` 0.5 (and `http-conduit` 1.5), its semantics have changed a bit and it
is now a bit more user-facing, which has led to a bit of confusion. I want to
explain why it changed, and how to adapt your code.

## Core issue: finalization

At the core of this change sits finalization. Let's take a relatively simple
example: reading data from a file. And let's simplify further and ignore
exceptions. In the normal `conduit` workflow (i.e., not using
connect-and-resume), the basic workflow is:

1. Wait till the downstream `Sink` asks for input.
2. Open the file handle.
3. Pull a chunk of data and provide it downstream.
4. Repeat (3) as long as downstream needs input.
5.   When either downstream is done, or no data is left in the file, close the
     handle.

Let's ask a simple question: what happens if the `Sink` never asks for input?
We *could* in theory open the file and immediately close it, but that's
inefficient. Instead, we would like to do nothing regarding the file reading in
this case. Keep that in mind.

Now, let's take into account connect-and-resume. The basic idea is that, when
downstream is done, we don't immediately close the file. Instead, we return a
new __something__ (to be defined later) which lets you continue reading the
file from where you left off.

## something == Source

So we used some kind of "something" above to be the frozen state of the file
reading. Let's say for a moment that we wanted this something to be a plain old
Source. That works out really nicely: we can reuse all of our existing tools
for dealing with Sources for this frozen things.

The problem comes in when this frozen Source is never actually used again.
Remember in our first normal connection example, we said that- if the file was
never actually read from- no finalization needs to take place. But in our case,
the file has already been read from, and therefore we *do* need to perform
finalization, even if the Source isn't actually used.

This was precisely the situation in `conduit` 0.4. The solution was to *always*
call a Source's finalizer. This means that in some cases, finalizers needed to
be defined where they logically did not apply (e.g., how to finalize a
`sourceFile` when you've never read from it). In more technical terms, it means
that the `PipeM` constructor needed to have a finalizer defined as well, which
led to lots of redundant code.

## conduit 0.5: something == ResumableSource

Starting in conduit 0.5, we've refactored this situation a bit. Finalizers are
now associated only when yielding a value downstream. This means that if a
Source is never pulled from, no finalizer is registered. This automatically
addresses our issue of the unused `sourceFile`.

The problem comes in with connect-and-resume. In this case, we *need* to perform some finalization in all cases. To accomodate this, we have a new datatype:

    data ResumableSource m o = ResumableSource (Source m o) (m ())

A `ResumableSource` is just a `Source` and its associated finalizer. There are three basic operators for interacting with a `ResumableSource`:

* `$$+` connects a normal `Source` to a `Sink`, and returns a `ResumableSource`.
* `$$++` connects a `ResumableSource` to a `Sink`, and returns a new `ResumableSource`.
* `$$+-` connects a `ResumableSource` to a `Sink`, and then closes it.

## The changes in http-conduit

Hopefully this puts us in a good position to understand what's going on with
`http-conduit`. Simplifying things just a bit, the function to make an HTTP
request in http-conduit 1.4 was:

    http :: Request -> IO (Source IO ByteString)

In http-conduit 1.5, this changed to:

    http :: Request -> IO (ResumableSource IO ByteString)

The reason is that we have a finalizer associated with our `Source`: we need to
shut down our socket, even if we never pull any data from it. In `conduit` 0.4
and `http-conduit` 1.4, we could rely on the fact finalizers are always run,
even if the Source isn't used. But in 1.5, in order to ensure that the socket
is closed, we need to use ResumableSource.

This actually works out to a fairly minor change in usage. In many cases, you
can simply replace `$$` with `$$+-`, and continue using the library exactly as
you did before.

## Getting a Source back

In some circumstances, you may really want to get a `Source` out of your
`ResumableSource`. (For example, you may have a pre-existing API like `wai`
that you're trying to conform to.) In order to allow this, `conduit` provides
the `unwrapResumable` function, which gives you both a `Source` and a
finalizer.

But the finalizer and `Source` it gives you are both specially set up to avoid
double-finalization. If the `Source` is used, then the finalizer doesn't
perform any action. On the flip side, if the `Source` is never used, then the
finalizer will perform the originally registered finalization. This way, for
instance, your socket will be closed once, and precisely once.

## Conclusion

I hope this post clarifies the API changes in `http-conduit` 1.5. If not,
please let me know, and I can try to elaborate a bit more.

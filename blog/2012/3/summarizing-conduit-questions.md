My [last blog post](http://www.yesodweb.com/blog/2012/03/pipes-like-conduit) on
the  prosposed changes to conduit got [quite a discussion on Reddit](http://www.reddit.com/r/haskell/comments/reft1/pipeslike_conduit/). I
really appreciate everyone's input, thank you! In the process of the
discussion, a number of questions came up, and I'd like to summarize them here.

By the way, if anyone thinks I'm spending an inordinate amount of time on this
stuff... it's because I am :). This is the last remaining blocking issue for
the Yesod 1.0 release, so I'm trying to make a decision on this stuff quickly.
I don't want to make a bad decision under pressure of course, but if we can
come to some conclusions in the next week, it would be very nice to be able to
use the shiny new conduit 0.4 in Yesod 1.0.

Most importantly: is this a good change?
----------------------------------------

The first question is the most important. Is the move towards a single datatype
overall a good thing or a bad thing? Obviously the advantages are strong: a
single set of instances to maintain, a single fusion operator, less
constructors to deal with. However, we need to accept that there are also
downsides: error messages are more confusing and code needs to deal with
meaningless constructors (e.g., `HaveOutput` for `Sink`).

After playing around with this quite a bit, I'm strongly leaning towards saying
that the benefits outweigh the costs. The clincher for me was when I was able
to reimplement `sequence` and `sequenceSink`, and the two functions basically
disappeared. Compare the
[old](http://hackage.haskell.org/packages/archive/conduit/0.3.0/doc/html/src/Data-Conduit-Util-Conduit.html#sequenceSink)
and
[new](https://github.com/snoyberg/conduit/blob/5b83899ea840aedda9b12385f0b1be3f3ee5a50d/conduit/Data/Conduit/Util/Conduit.hs#L149)
versions.

I'm still hoping to hear from some `conduit` users on this to make sure the
changes won't be off-putting, but I think it's almost certain that the code
well be merged into master.

### Side point: newtypes?

The idea came up of using `newtype`s for the `Source`, `Sink` and `Conduit`
types to try and have a best of both worlds. I still think it would be
beneficial (better error messages, and a nicer `Functor` instance for `Source`
and `Conduit`), but at least for now, I think it's too much overhead to have to
wrap/unwrap everywhere. There's also an argument to be made that a `newtype`
would hide away the "true nature" of our types, though I'm still on the fence
as to whether users should be confronted with the fact that the three types are
unified.

Type for second record in `NeedInput`
-------------------------------------

The `NeedInput` constructor has two records. The first takes some input and
returns a new `Pipe`. The second is for indicating that no input is available.
Unlike the early termination records for `HaveOutput` and `PipeM`, this record
gives a `Pipe`, since it's feasible that we may want to output more values
after we've run out of input (the typical example here is a decompression
`Conduit`).

Said another way: the early termination for `HaveOutput` and `PipeM` can only
ever be called when the upstream `Pipe` closes, not when the downstream `Pipe`
closes.

Anyway, the idea of this record is that it can't receive any input, since once
it's been called, we know that the upstream pipe has closed and won't be
providing any more input. There are two ways we could model this: set the `i`
parameter to `()`, or set it to `Void`.

However, there's also a third approach: keeping `i` as it was before. Since
we'll never be providing any more input to this `Pipe`, it's completely
irrelevant what the `i` parameter is. If the `Pipe` ever requests more input,
we'll just call the early termination `Pipe` again anyway. The advantage to
this third approach is that it simplifies some of the internal code, since we
don't need to juggle different parameters.

I'm leaning towards the third approach, but all three seem equally feasible.

Separate `Leftover` constructor?
--------------------------------

There's a bit of an inconsistency, in that the `Done` constructor performs two
actions: it returns a result, and gives back any leftover input. Also confusing
is that we can only have 0 or 1 leftover values.

We could address the second issue by changing the `Maybe` to a list.
Alternatively, we could solve both issues by introducing a new `Leftover`
constructor, and modifying the `Done` constructor, like so:

    | Done r
    | Leftover (Pipe i o m r) i

I've put this in a [branch on Github](https://github.com/snoyberg/conduit/tree/pipe-leftover),
and it certainly works. However, I think I'm most comfortable leaving code as-is:

* We don't have the concept of chunking (i.e., dealing with more than one value at a time) anywhere else in the type, so why should the leftovers be different?
* Right now, we have a nice invariant expressed in the types that you can only return leftovers when computation is complete. I think I like that setup.

Source: to `Void` or not to `Void`?
-----------------------------------

In order to ensure that a `Sink` never yields output, we set the `o` parameter
to `Void`. Initially, we set the `i` parameter on a `Source` to `()`, so that
`runPipe` can just provide an infinite stream of unit values.

However, we can just as well set the `i` parameter to `Void`, and then call the
no-more-input record of `NeedInput`. I'm not going to try and summarize the
arguments back and forth on this one, because there are a lot of them. I *will*
say that I'm leaning towards `Void`, just because it gives a very nice parallel
between `Source` and `Sink`.

Fuse operators: unify?
----------------------

There's now a fusion function (`pipe`) which can fuse together `Source`s,
`Sink`s and `Conduit`s. All three fusion operators (`$=`, `=$`, and `=$=`) are
simply type-constrained wrappers around it. (`$$` also utilizies `pipe`, but it
also calls `runPipe` on the generated `Pipe`.) The question is: do we need all
three operators, or should we have just one?

The advantage of separate operators is clearer error messages, and more
explicit code. However, it hides the fact that all three types are really one
and the same. (Again, I'm ambivalent as to whether that hiding is a feature or
a bug.) It also means that people have to learn more names.

So should we have a unified fusion operator? And what would it be called?

Note: either way, this next release will still contain the other three,
type-constrained fusion operators, if only to ease migration. If we add a
unified operator, it would be in addition to those three for now, and likely
after a few point releases we would deprecate the three operators.

Bikeshedding: rename the $$& operator
-------------------------------------

This is likely the easiest. I call `$$&` the connect-and-resume operator, and
it connects a `Source` to a `Sink`, gets a result, and also gives back the most
recent state of the `Source`. This allows us to continue computation.

Frankly, I chose a pretty bad name for the operator. (In my defense, I did that
on purpose to make sure I didn't become attached to it.) Some other ideas that
have been floated are `$$-` and `$$+`. I feel no particular drive one way or
another here.

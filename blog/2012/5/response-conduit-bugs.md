I was actually planning on writing an entirely different blog post today, which
was going to show how some ideas from version 1.0 of `pipes` could help
simplify some conduit-based code. That post will have to wait a few days, as I
think this one is more important.

Many people in the Haskell community have probably seen Gabriel Gonzalez's [conduit bugs](http://www.haskellforall.com/2012/05/conduit-bugs.html) blog post. Normally, I would restrict my response to such a post to comments on Reddit, and allow interested readers to find my response there. However, given that the blog post makes such categorical (excuse the pun) claims about the incorrectness of conduit, I feel an obligation to give a more official response.

Firstly, the [Reddit discussion](http://www.reddit.com/r/haskell/comments/u7vyx/conduit_bugs/) is very informative. Of the four general claims made in the post, two of them are completely refuted: the `MonadTrans` laws are only violated if you hold to a belief that equality means exactly the same constructors, as opposed to moral equivalence. The claim of doubled `release` calls is not true at all, and I hope Gabriel will update his post to clarify this mistake.

The claim about violation of the category laws is a weak claim. Even the blog post acknowledges that `conduit` makes no claims to actually follow these laws. Further, it quotes parts of the `conduit` documentation that states explicitly that the code used to demonstrate the "bug" is invalid `conduit` code. I'd like to highlight that point, and use it to address the only remaining point from the blog post.

## It's not a bug

If the original blog post were renamed "conduit corner cases" or "conduit gotchas" or "unintuitive behavior in conduit," I would not be writing this response. This response is triggered by the claim that conduit is exhibiting buggy behavior. Gabriel is using a very specific- and in my opinion incorrect- definition of the term bug: that the types allow you to write a buggy program.

It is entirely possible to create a buggy program with conduit. I do not deny it. Instead of attempting to encode all invariants in the type system, conduit takes the approach of expressing most invariants in the type system, and relegating others to documentation. The reason for this is twofold: simplicity and performance.

Let's take one of the examples from the original blog post:

    residue x = Done (Just x) ()

If you look in the [documentation](http://hackage.haskell.org/packages/archive/conduit/0.3.0/doc/html/Data-Conduit.html#t:Sink), one of the stated invariants of `conduit` is:

> It is a violation of the Sink invariants to return leftover data when no input has been consumed.

(Note: as I mention in my response on Reddit, there is in fact a *documentation* bug, since the invariants of the package were accidently removed from the documentation in the move from 0.3 to 0.4. I will correct that in the coming days.)

Could this invariant be encoded in the type system? Yes. In fact, version 0.2 of `conduit` did so, by having separate `Sink` and `SinkResult` types. I debated whether or not it was a good idea to trade static safety for simplicity in this case, and decided that simplicity won out. You can argue with my decision here, but to call this behavior buggy is incorrect.

For performance, let's look at the end of the blog post:

> However, this is quite easy for conduits to fix. All you do is remove the finalizer field from the PipeM constructor and have pipeClose ignore PipeM actions completely, only associating finalizers with HaveOutput.

This is actually incorrect even with regards to correctness: removing the finalizer field from `PipeM` would mean that, for example, `sourceFile` could not guarantee that the file handle is closed immediately. (If Gabriel would like to show concrete code demonstrate that I'm incorrect, I'd be intersted to see it. I'm always happy to simplify a library if possible.) But having that second field is vital for minimizing work done. If the first field needed to be run even when no more output is requested, `sourceFile` would always end up reading an extra chunk of data. I would consider *this* to be buggy behavior.

Putting it all together, I think there's a simple and fundamental difference between `pipes` and `conduit`, which leads to the mistaken claims of bugs:

<b><code>conduit</code> exposes internals of the package to the extent that you can actually produce invalid code. <code>pipes</code> believes in preventing this kind of usage statically.</b>

You can argue that the `pipes` approach is better, and I'll argue that `conduit`'s approach is better. But let's call a spade a spade, and admit that `conduit`'s approach isn't buggy.

## The remaining claim: invalid `Monad` instance

This is by far the most important claim in the original blog post. It breaks down into two claims. The first claim is easily refuted, since in order to demonstrate the problem, you need to return leftovers without consuming input. As stated above, this is a violation of the invariants of `conduit`.

(As a side note, there is an off-hand comment in the post about two cases of data loss. The other case is documented behavior, and explained clearly in the [appendix on conduits](http://www.yesodweb.com/book/conduits). The problem is inherent to the problem domain, and `pipes` has only avoided the issue by dropping a very important feature (leftover handling) from the library. It's obviously possible to "solve" problems by removing features, but that's an unacceptable solution.)

The problem here is again one of documentation: an undocumented invariant. This invariant has been discussed on the mailing list, but never actually made it into the Haddocks. Simply stated: `conduit` finalizers can only clean up resources which they allocated. This is demonstrated in the `conduit` codebase itself: `sourceHandle` and `sinkHandle` do not close their `Handle`s, since they did not allocate them. `sourceFile` and `sinkFile`, on the other hand, did allocate their own `Handle`s, and thus must clean them up.

So again, it is *possible* to construct code with `conduit` that is incorrect, but you have to violate invariants to do so.

## The future of `conduit`

One of Garbiel's claims is that we could solve these problems by hiding the constructors. You might be surprised to hear me agree with this 100%. I do not believe it is ideal that `conduit` exposes users to its internals (and in fact, as Garbiel rightly points out, forces usage of these internals to get correct code). At the same time, I believe it was the correct decision for now to expose these internals, as we don't have a clear idea yet of what a higher-level interface would look like. Hiromi Ishii and I recently had [a discussion on haskell-cafe](http://www.haskell.org/pipermail/haskell-cafe/2012-May/101334.html) discussing some helper functions, and I believe that the future of `conduit` will in fact see a number of such functions replacing direct usage of the constructors. (I still plan to export the constructors from the `.Internal` module, however.)

In other words, `conduit` is a young package. It has rough edges. If you're not careful, you can cut yourself. But the package (to the best of my knowledge) works correctly, as documented. There are ways that we can make the package nicer, and I'm hoping to do so incrementally, with input from the community.

## The conduit invariants

This list will soon make it into the `conduit` package itself. If anyone can think of invariants that I have omitted, please let me know.

* A value of type `Pipe` cannot be reused. Some `Pipe`s (e.g., `sourceHandle`) have implicit state associated with them due to the very nature of the underlying I/O actions.
* No leftover input may be returned via `Done` without first consuming input.
* A finalizer may only finalize a resource previously allocated by the same `Pipe`. If this invariant is respected, then (barring exceptions) a finalizer will run if and only if the resource was allocated.
* Not quite an invariant, but: when transforming streams via a `Conduit`, it is possible to have data loss. Please see the appendix for more information.

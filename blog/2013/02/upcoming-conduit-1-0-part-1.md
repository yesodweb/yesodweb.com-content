As part of the upcoming Yesod 1.2 change, I've been thinking about breaking
changes we might want to make to other base libraries as well. I've actually
been considering some changes to conduit for quite a while, and after a lot of
thought I think I have a solid proposal. But first, let's talk about what
conduit does relative to other packages and what its goals are. My next
blog post will discuss the proposed upcoming changes to conduit.

Note: I'll be referring to some of the ideas I outlined in my [meaning of
power](http://www.yesodweb.com/blog/2013/01/meaning-of-power) blog post. It's
certainly not required reading, but might clarify some of my terminology.

## enumerator, pipes, and io-streams

I still get questions on a fairly regular basis asking how conduit stacks up
against the three libraries listed above. (Less so recently with enumerator,
since that discussion has happened quite a bit.) Let me address each one in
turn to hopefully paint a picture of what conduit hopes to be- in my mind at
least.

This isn't a question about superiority of one library over another; instead, I
believe each of the four libraries has a distinct purpose, and this purpose
directs its development. By analyzing the differences between conduit and the
other package, we can help to better clarify what conduit's purpose is, and
therefore create a better end product.

### enumerator

conduit came out in a direct response to issues we were facing with enumerator.
I don't want to elaborate too much on those, as they're [well
documented](https://github.com/snoyberg/conduit#enumerator). What I *do* want
to point out is what we gave up in this change:

* One of the defining features of the enumerator approach is that it allows a
  data producer to guarantee resource finalization. This happens because the
  producer maintains the flow of execution at all times. conduit has given up on
  this feature, receiving in its place a much simplified (IMO) execution model.
  We use ResourceT to allow guaranteed resources, and in exchange can guarantee
  that behavior in not only producers, but transformers and consumers as well.
* Our Conduits are (I believe) strictly less powerful that enumerator's
  Enumeratees. My prime example of this is that `isolate` in enumerator can
  force flushing, whereas in conduit it cannot, since termination of the
  downstream will automatically terminate upstream. Again, I consider this an
  acceptable tradeoff.

There may very well be other differences as well. My point here isn't to list
every one. Instead, I want to point out that conduit has, in fact, given up on
some flexibility in exchange for simplicity.

### pipes

pipes and conduit are two incredibly similar packages. I think it's fair to say
that the core distinction is a strong philosophical difference: conduit is
designed first and foremost to solve problems in the real world, whereas pipes
is designed to find the most elegant solution to the problem. I believe each
approach has its place, but this leads to some important differences, some of
which will be very relevant for the 1.0 discussion.

Let's take the issue of guaranteed resource finalization. Gabriel [recently
explained](http://www.haskellforall.com/2013/01/pipes-safe-10-resource-management-and.html)
that you can't have both prompt resource finalization *and* guaranteed ordering
of finalization functions. (At least, that's my reading, and what I've
discovered myself in practice.) In pipes-safe, the solution seems to be to
consider guaranteed ordering a requirement, and therefore to not always have
prompt finalization. In conduit, however, we're much more worried about real
world use cases. In all of the code I've written or seen written, ordering of
finalizers is irrelevant to correct code, but prompt finalization is a hard
requirement. Therefore, conduit chooses instead to guarantee promptness and not
guarantee ordering.

Another important distinction is understanding what's baked into the core
abstraction versus what's considered an add-on. In conduit, we're aiming for a
set of features which solves the vast majority of use cases. It may include
functionality which is not needed for some users, and for more extreme cases
may not provide enough. But the simplicity and performance benefits of this
bundled approach is too valuable to pass up. pipes takes the approach of
allowing extra features to be added separately. This is certainly a valid
approach and seems to work well, but I believe is not the right direction for
conduit.

Let me bring special attention to the issue of handling exceptions within a
pipeline. I noticed that pipes-safe provides this feature. In conduit, we've
decided against getting involved in that business, based simply on a lot of
experience with failed attempts. I'm actively interested in seeing how this
approach works out for pipes. But for now it's not a feature I think we should
adopt, as I think it will prove to be more complicated than it initially looks.

Gabriel pointed out to me another very good example: bidirectionality. It's a
very interesting feature in pipes, and could be used for a lot of purposes.
However, it also adds in extra complexity, and is not a feature necessary to
implement our current conduit use cases. It might be that it will turn out to
be such an incredibly useful feature in the future that conduit may wish to
adopt it, but for now I think it makes more sense to leave it out.

### io-streams

_Note_: I'll admit from the start that I am no expert on io-streams. I had to go
to Gregory for a few clarifications, and it's possible I'm still incorrect on
some of the details. My point here is *not* to create an exhaustive comparison
between the libraries, but rather at a high level point out the differences in
approach.

Probably the biggest distinction between these packages is that- while conduit
was designed primarily to solve I/O issues- it aims to work in an entirely pure
context as well.  io-streams, on the other hand, has a distinct I/O bias. This
is not a criticism; picking a goal to target and focusing on that is a great
technique. But it does clarify one of conduit's goals: work for more than just
I/O, even if I/O is our main motivating case.

So practically speaking, it seems like io-streams would not be a library that
would address the needs of something like xml-conduit, which requires both pure
and impure interfaces. (I could be mistaken here, but I have a hard time seeing
it happen.)

In the same vein, conduit was designed from the ground up to work with
arbitrary monad transformer stacks. Again, this is an intentional design
decision on both the parts of conduit and io-streams; Gregory explained to me
that they purposely avoided supporting transformer stacks. While I understand
his motivations, it's simply not an option for supporting the use cases we want
to support it conduit. Handling streaming database responses in persistent, for
example, would not be possible with such an approach (or without significantly
rewriting the persistent API).

Additionally, a fairly substantial part of the io-streams codebase is a
replacement for `Handle`s, not streaming data abstractions. In that sense, I
don't see conduit and io-streams in conflict. conduit currently uses `Handle`s
for low-level I/O primitives, but could certainly switch to io-streams instead,
or use the package as an optional replacement. So in my opinion, the window
for comparison between the two is actually fairly narrow.

Simply because they came up in the context of io-streams, I wanted to discuss
two criticisms leveled against conduit.

__conduit requires you to use ResourceT instead of the bracket idiom__. This
isn't actually true. ResourceT is a convenience allowing you to allocate
resources within a pipeline, but you are absolutely able to use bracket
patterns external to your pipeline and then avoid ResourceT. For the file copy
example, consider the following code snippet ([also available from School of
Haskell](https://haskell.fpcomplete.com/user/snoyberg/random-code-snippets/conduit-without-resourcet):

```haskell
import Data.Conduit (($$))
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import System.IO (withBinaryFile, IOMode (..))

main =
    withBinaryFile "input.txt" ReadMode $ \input ->
    withBinaryFile "output.txt" WriteMode $ \output ->
    sourceHandle input $$ sinkHandle output
```

However, this isn't simply a convenience; it also works to allow for much more
timely resource freeing. By forcing a bracket pattern, we must wait for all of
the inner code to complete before freeing a resource. With ResourceT, usage and
freeing can be interleaved safely. For example, considering concatenating 20
files and breaking them up into equal chunks of a certain size, each to be
written to a separate file. ResourceT will ensure that before each succeeding
source file is open, the previous one is closed, and the same for the
destination files.

__conduit is too complicated, it even has six type parameters__. I'll
half-agree with this statement. Idiomatic usage of conduit doesn't require you
to talk about six parameters. We have three types (Source, Sink, and Conduit),
which have respectively 2, 3, and 3 parameters. And I'll argue that the
presence of each of those parameters is not only necessary, but fully intuitive
to users.

So why did I half agree? Writing general code stil requires dealing explicitly
with the Pipe datatype, or at least one of the wrappers. And error messages
still mention this type. This is probably the main shortcoming I've found with
conduit 0.5, and the prime goal I have to fix in 0.6. However, I'll hold off on
further explanation for my next blog post, where we can dig into the meat of
conduit itself.

## Conclusion

conduit has a well defined set of use cases it's trying to solve, and is
unapologetic about not fitting other use cases. It fits a certain level of
abstraction, and doesn't attempt to shoe-horn itself into a different level as
well. In my opinion, this produces a high quality, user friendly, and
performant library. It is targeted exclusively at real world problems, and as a
result will trade in elegance in some cases if it will better solve real world
examples.

In my next post, I'll dive into some proposed changes for conduit. As a sneak
preview: the core concepts of conduit will stay completely unchanged, and
likely the vast majority of user code will continue to work. The goal is to cut
out confusion from the API and simplify type signatures and error messages.

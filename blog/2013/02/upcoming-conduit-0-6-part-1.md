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
which will be very relevant for the 0.6 discussion.

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

### io-streams

I'll admit, this one baffled me a bit. When I started getting asked how conduit
and io-streams compares, I couldn't really understand why someone would be
comparing a streaming data abstraction to a I/O abstraction. I believe the
question came about because Gregory Collins announced that Snap would be using
io-streams in place of a streaming data abstraction. So let me try and clarify.

conduit is at an entirely different level of abstraction from io-streams.
io-streams is much more set as a replacement to `Handle` than to conduit. It's
certainly possible for a library to elect to use io-streams instead of conduit,
but what that's really doing is choosing to swap out I/O systems and
*simultaenously* choose an entirely different level of abstraction.

Personally, I think streaming data abstractions are a good thing. I don't want
to go back to explicit reads and writes. I believe something like io-streams as
the user-facing abstraction falls into the trap of pretending that a simple API
means simple usage. It's true that learning the conduit API will take longer
than learning the io-streams API, but once you've learned it, code becomes much
easier to understand. For example, copying a file in conduit is `runResourceT $
sourceFile src $$ sinkFile dst`. From what I understand, the io-streams result
will be much longer and less declarative.

There are also entire use cases which don't seem addressed by io-streams. It
has chosen not to abstract over the monad for actions, meaning it can neither
be used in pure contexts or with monad transformer stacks. It also seems
ill-suited for cases such as xml-conduit. That's not to say that the library is
badly designed- on the contrary. It seems to be designed perfectly for the use
case its targeting. My point is that that use case is *different* than the
conduit use case.

conduit currently builds its I/O system on top of `Handle`s. Depending on how
things progress with io-streams, I could certainly picture either replacing
that with io-streams, or making io-streams an optional replacement.

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

conduit has a while defined set of use cases it's trying to solve, and is
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

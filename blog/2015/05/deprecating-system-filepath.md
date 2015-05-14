I [posted this information on
Google+](https://plus.google.com/+MichaelSnoyman/posts/Ft5hnPqpgEx), but it's
worth advertising this a bit wider. The tl;dr is: system-filepath and
system-fileio are deprecated, please migrate to filepath and directory,
respectively.

The backstory here is that system-filepath came into existence at a time when
there were bugs in GHC's handling of character encodings in file paths.
system-filepath fixed those bugs, and also provided some nice type safety to
prevent accidentally treating a path as a `String`. However, the internal
representation needed to make that work was pretty complicated, and resulted in
some weird corner case bugs.

Since GHC 7.4 and up, the original character encoding issues have been
resolved. That left a few options: continue to maintain system-filepath for
additional type safety, or deprecate. John Millikin, the author of the package,
[decided on the latter back in
December](https://plus.google.com/+JohnMillikin/posts/j7NCSdRHGvN). Since we
were using it extensively at FP Complete via other libraries, we decided to
take over maintenance. However, this week we decided that, in fact, John was
right in the first place.

I've already migrated most of my libraries away from system-filepath (though
doing so quickly [was a
mistake](https://github.com/snoyberg/classy-prelude/issues/102), sorry
everyone). One nice benefit of all this is there's no longer a need to convert
between different `FilePath` representations all over the place. I still
believe overall that `type FilePath = String` is a mistake and a distinct
datatype would be better, but there's much to be said for consistency.

Some quick pointers for those looking to convert:

* You can drop basically all usages of `encodeString` and `decodeString`
* If you're using basic-prelude or classy-prelude, you should get some deprecation warnings around functions like `fpToString`
* Most functions have a direct translation, e.g. `createTree` becomes `createDirectoryIfMissing True` (yes, the system-filepath and system-fileio names often times feel nicer...)

And for those looking for more type safety: all is not lost. Chris Done has
been [working on a new
package](https://www.stackage.org/haddock/nightly-2015-05-14/path-0.2.0/Path.html)
which is aimed at providing additional type safety around absolute/relative and
files/directories. It's not yet complete, but is already seeing some
interesting work and preventing bugs at some projects we've been working on
(and which will be announced soon).

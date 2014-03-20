I've just released [a new version of
conduit-combinators](http://hackage.haskell.org/package/conduit-combinators)
which adds two new functions: `sourceDirectory` and `sourceDirectoryDeep`. The
former lists all of the children of a given directory, and the latter traverses
deeply into a directory tree and lists all of the files present.

To see how this is used, consider the following simple example, which prints
out the total number of files in the current directory tree. (Note: the `False`
parameter means not to traverse symlinks to directories.)

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Conduit

main :: IO ()
main = runResourceT (sourceDirectoryDeep False "." $$ lengthC) >>= print
```

Note that this is equivalent to running `find . -type f | wc -l`.

This new function supersedes `Data.Conduit.Filesystem.traverse`, which uses
more memory. To give you an idea of the difference, for a directory structure
with 3361 files, `traverse` uses a maximum residency of 957KB, whereas
`sourceDirectoryDeep` uses a maximum of 48KB.

In the implementation of `traverse`, the entire contents of a directory are
read into memory as a list, and then each entry is analyzed one at a time. If
the entry is a file, it is yielded, and then can be garbage collected. But if
the entry is a directory, that directory must then be traversed, at which point
both the remaining contents from the parent directory, *and* the contents of
the new directory, are in memory simultaneously. By contrast, in
`sourceDirectoryDeep`, only a single file path is read into memory at a given
time.

Even if you're just doing shallow traversals, you can get a memory improvement
by using `sourceDirectory` instead of `getDirectoryContents`.

## Deprecating filesystem-conduit

At this point, I'm also deprecating filesystem-conduit, as all of its
functionality is represented in conduit-combinators. I'm actually hoping to
consolidate a few other packages over the coming weeks in an effort to simplify
dependency trees a bit. I'll post on the subject when I have some more concrete
plans.

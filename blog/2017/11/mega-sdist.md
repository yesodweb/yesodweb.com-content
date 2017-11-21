Many years ago, I wrote a utility called `mega-sdist` to help me with managing
mega repos (more on that below). I've been using it myself ever since, making
some minor improvements over the years. But I realized recently that I never
really announced it to others, and especially not to the people whom it would
help the most: other Yesod contributors and maintainers. Consider this the
(massively belated) announcement.

You can find the most up-to-date information in [the project README.md on
Github](https://github.com/snoyberg/mega-sdist#readme). Below is the current
content of that file, to help save you a click.

* * *

This is a utility written to address the specific needs in maintaining
Haskell "mega-repos," or Git repositories containing multiple Cabal
projects. It is intended to ease the process of deciding which
packages need to be released and tagging those releases appropriately.

It provides the following functionality:

* Detect when local code has changed from what's on Hackage
    * Note that, due to Hackage revisions, sometimes this logic isn't
      perfect
* Detect when a version number needs to be updated
* Dump the difference between the Hackage version of your package and
  the local version

To install it... well, listen. This tool is intended for people
authoring Haskell packages. Odds are, you already know how to do
this. And if you don't know, this probably isn't a tool that will help
you. Anyway, in order to install it, first
[install Stack](https://haskell-lang.org/get-started) and then run
`stack install mega-sdist`, or just `stack install` inside this
repository.

## Opinionated tool

This utility is highly opinionated in some ways, e.g.:

* It only supports one style of Git tag name:
  `packagename/version`. This may look weird in non-mega-repos, where
  `v1.2.3` looks better than `foo/1.2.3`, but for mega-repos the
  former doesn't make sense.
* It depends on Stack for both discovering all of your local packages,
  and for uploading to Hackage.

If you're OK with these opinions, keep reading for usage.

## Have I changed anything?

Let's say I'm working on the
[monad-unlift megarepo](https://github.com/fpco/monad-unlift) (chosen
as an example of a relatively small repo). I've merged some PRs
recently, or at least think I have. But I don't remember which of the
individual packages within the repo this affected. Instead of looking
at the commit history like some caveman, I'll typically do:

```
$ git pull # make sure I have all latest changes
$ mega-sdist
```

The `mega-sdist` command will:

* Build tarballs for all local packages
* Check what the latest versions of my packages on Hackage are
* Do a full `diff` on these two things and see if anything's changed

At the time of writing, here's the output from this repo:

```
The following packages from Hackage have not changed:
monad-unlift-0.2.0

The following packages require a version bump:
monad-unlift-ref-0.2.1
```

What this means is:

* The `monad-unlift` package I have locally is at version `0.2.0`. And
  it perfectly matches that version on Hackage. No actions necessary.
* The `monad-unlift-ref` package I have locally is at version
  `0.2.1`. And it doesn't match the code on Hackage. Therefore, if I
  wanted to run `stack upload monad-unlift-ref` successfully, I'd need
  to bump the version number.

## What did I change?

Well, again, if I wanted to see what changed, I could run (again, like
a caveman):

```
$ git diff monad-unlift-ref/0.2.1 -- monad-unlift-ref
```

But that's long! `mega-sidst`'s got your back. Just run:

```
$ mega-sdist monad-unlift-ref --get-diffs
```

This will print out the difference between the tarball uploaded to
Hackage and what you have locally. Besides my tongue-in-cheek comment
above, this is also useful if, for some reason, you either don't have
or don't trust the tags in your Git repo.

One other thing: this diff is currently based on the pristine tarball
from Hackage, ignoring cabal file revisions. So the difference may be
slightly different from what you'd get from `stack unpack
monad-unlift-ref-0.2.1`. But `¯\_(ツ)_/¯` that's revisions for you.

The default behavior of `mega-sdist` is to look at all packages
specified in your `stack.yaml`. Targets can be any directory. And
`mega-sdist` will automatically look at packages in any
_subdirectory_, so that `mega-sdist .` is the same as `mega-sdist` at
the root of your repo\*.

\* Assuming all of your packages are actually _in_ your repo, but only
crazy people would do otherwise.

## Preparing a new release

OK, now I continue working on my project, and I've:

* Made some changes to `monad-unlift`
* Updated the cabal file's version number
    * And of course I also updated the `ChangeLog.md`, I'm not some
      monster

From the root of my repo, I run:

```
$ mega-sdist monad-unlift
```

Or, equivalently, from inside the `monad-unlift` subdirectory I run:

```
$ mega-sdist .
```

Either way, I get:

```
The following new packages exist locally:
monad-unlift-0.2.1

No version bumps required, good to go!
```

This tells me that my package has local changes, _and_ the version
number has been updated, so that `stack upload monad-unlift` will
work. Neato! Now, you _could_ just run `stack upload ...`, but here's
what I usually do. First, I'll review the changes I'm about to upload
and make sure there are no surprises:

```
$ mega-sdist --get-diffs .

The following new packages exist locally:
monad-unlift-0.2.1
diff -r old/monad-unlift-0.2.0/ChangeLog.md new/monad-unlift-0.2.1/ChangeLog.md
0a1,4
> ## 0.2.1
>
> * Silly changes
>
diff -r old/monad-unlift-0.2.0/Control/Monad/Trans/Unlift.hs new/monad-unlift-0.2.1/Control/Monad/Trans/Unlift.hs
51a52,54
>
> -- I just need some space
>
diff -r old/monad-unlift-0.2.0/monad-unlift.cabal new/monad-unlift-0.2.1/monad-unlift.cabal
2c2
< version:             0.2.0
---
> version:             0.2.1

No version bumps required, good to go!
```

OK, that's what I wanted. Time to release. Next, I'm going to use
`mega-sdist` to tag the release:

```
$ mega-sdist --gittag .
```

From the root of my repo, this would notice that `monad-unlift-ref`
still requires a version bump, and refuse to proceed. But inside the
`monad-unlift` directory, it notices that all necessary version bumps
are done, and happily tags:

```
$ mega-sdist --gittag .
The following new packages exist locally:
monad-unlift-0.2.1

No version bumps required, good to go!
Raw command: git tag monad-unlift/0.2.1
```

And suddenly I notice something new:

```
$ ls tarballs/
monad-unlift-0.2.1.tar.gz
```

Neat, `mega-sdist` left behind tarballs I can upload! To do so, I run:

```
$ stack upload tarballs/*
```

Note that this will work whether I'm trying to upload just one
package, or all of the updated packages in my repo. Finally, I need to
push the new tags to Github (or wherever):

```
$ git push --tags
```

And in fact, this upload sequence is so common that I have a shell
alias set up:

```
$ alias upload
alias upload='mega-sdist --gittag . && stack upload tarballs/* && git push --tags'
```

So there you have it: convenient little utility to help manage repos
with lots of packages in them.

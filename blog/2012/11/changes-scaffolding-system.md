I've just pushed some significant changes to Hackage for the `yesod`
executable. These changes fall into two categories: an improved `yesod devel`
experience and better scaffolding. The former is by far the cooler feature,
and will hopefully be covered in another blog post soon. This blog post covers
the second feature.

I have purposely not yet updated `yesod-platform` to include this new
executable, as I still consider it somewhat experimental. It would be great if
everyone could install this new version and test it out. Doing so should be as
simple as:

    cabal update
    cabal install yesod-1.1.3

If you run into any problems, please report them.

## Scaffolding

In the past few days, I've pushed a major overhaul to how the Yesod scaffolding
system works. The scaffolded site itself remains mostly unchanged, but the
manner in which the code is stored and generated is completely different.

Under the new system, there is a [dedicated scaffolding repo on
Github](https://github.com/yesodweb/yesod-scaffold). There is a separate branch
for each scaffolding option, e.g.
[PostgreSQL](https://github.com/yesodweb/yesod-scaffold/tree/postgres) versus
[MongoDB](https://github.com/yesodweb/yesod-scaffold/tree/mongo). Throughout
the scaffolding source, the word PROJECTNAME (whether in the file name or file
contents) can be treated as a variable; it will automatically be replaced by
the actual project name specified by the user.

Since each version is its own repo, synchronizing changes is as simple as a
`git merge`. I consider the PostgreSQL version the master branch, and
merging changes from there to all other versions. Testing is also greatly
simplified: you can just build the code like a normal project, and when you're
done, commit your changes. There's no need to go through an extra "run the
scaffolder" step. Due to this simplification, it's now possible to reinstate a
feature we used to have: [a scaffolded site without a
database](https://github.com/yesodweb/yesod-scaffold/tree/simple).

Once we have a set of scaffoldings, we run a special tool based on
`project-template` (discussed below) to generate a single file containing our
entire scaffolded site. We get one of these files for each version of the
scaffolding (PostgreSQL, MongoDB, etc) and then compile them into the `yesod`
executable. The `yesod` executable then uses `project-template` itself to
unpack those files.

To me, the main advantages of this move are:

1. It's much easier to test out new ideas: just fork the scaffolding repo.
2. Likewise, making modifications to the scaffolding is just a pull request away.
3. It's much easier to maintain drastically different scaffoldings.
4. We've removed a bunch of hairy code from the `yesod` executable.
5. Users can now specify URLs containing scaffolded sites, lowering the barrier to entry for experimentation.

To prove that last point, I've created a test branch for including Fay support
in the scaffolding, and placed the scaffolding file at

[https://raw.github.com/gist/4030486/dfe10c7c109c842f9eddd6a2811bfee4f305debe/postgres-fay.hsfiles](https://raw.github.com/gist/4030486/dfe10c7c109c842f9eddd6a2811bfee4f305debe/postgres-fay.hsfiles).

If you run `yesod init`, take the `url` option, and provide that address, you
can start to play with `yesod-fay` right now.

## Project Template

As a first step towards the collaborative Haskell IDE project, [I
stated](http://www.yesodweb.com/blog/2012/09/project-templates) I was going to
be working on a `project-template` library for providing a single format for
different IDEs and scaffolding tools to represent project templates. [The
initial version is now ready](http://hackage.haskell.org/package/project-template), and is being used in our codebase at FP Complete.
I'm hoping other IDE projects are able to take advantage of it to.

I initially had lots of grandiose plans for conditional text, conditional
files, etc. As you may have guessed based on this blog post, after careful
consideration, I've decided that such a system is overkill. Instead, I've
elected to go for a very simple file format (see, for example,
[postgres-fay.hsfiles](https://raw.github.com/gist/4030486/dfe10c7c109c842f9eddd6a2811bfee4f305debe/postgres-fay.hsfiles)
linked above).

This format is just a simple piece of text, with special `START_FILE` pragmas
to specify where a new file starts. Binary files can be included via
base64-encoding. And that's really it. What's cool here is that it opens the
door for uses beyond creating scaffoldings. For example, I could imagine a
`runghc` wrapper (or perhaps a feature added to GHC itself) that would
automatically unpack a file into multiple logical subfiles and then run them.

The library is [available on
Hackage](http://hackage.haskell.org/package/project-template). Feedback
welcome!

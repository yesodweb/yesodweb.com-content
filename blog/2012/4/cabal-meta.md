# using cabal-meta to transced to dependency heaven

## dependency hell

Dependency hell is arguably Haskell's biggest problems.
Collectively it hurts everyone's productivity and makes people think twice before adding another dependency to a project or installing a new project. Yesod is about to do our 1.0 release. This is a statement that we have a mature, if imperfect framework. Cabal is now actually our weakest link. It is the biggest stumbling block for new and experienced Haskell users and a large barrier to contributions.

The latest version of Cabal has a few changes to help the situation out:

* a new constraint solver, which should make dependency Hell less frequent.
* a warning before before breaking an existing installation.

Neither of these truly fix an install problem, they just make the problems less likely to occur.


## Yesod solutions

Besides making April Fools jokes about Cabal, we:

* support cabal-dev (an isolation tool) in our tools, such as the development mode server. We also encourage the usage of virthualenv/hsenv which requires no special support on our side
* released yackage, a local hackage server that helps make the right dependencies visible
* released cabal-src, a tool for making cabal remember locally installed packages. Note this functionality is already in cabal-dev.
* released yesod-platform, a meta-package that specifies exact versions of Yesod dependencies. Note that initially we had a tool called cabal-nirvana with a similar approach, but there were some issues with it that will require changes to cabal

Even with all these tools, one day I found myself completely incapable of installng Yesod from source.
The first thing I did was create meta packages like yesod-platform. These do in fact solve the problem.
However, they are difficult to maintain because they are just duplicates of existing information.
I realized that cabal can already do this for me: all I have to do is install everything at once.


## The cabal-meta approach

If you run this command, you can easily get a failure:

    cabal install foo && cabal install bar

Whereas if you run this command it should almost always work:

    cabal install foo bar

So I changed the Yesod installer scripts to use this approach.
However, even though I could install yesod from source, I found it did not guarantee that I could build my Yesod application.
I needed to installi both yesod source and my application at the same time.

Today I am officially releasing cabal-meta, a tool which facilitates installing all dependencies at the same time.

My experience and that of the few others that have already used cabal-meta is that if an installation is possible, it will succeed.
Most of the usage has been on top of hsenv. I am sure there are cases where installs will not succeed and certainly this can break existing installs.

However, the successful combination of hsenv and cabal-meta has changed my attitude about Haskell installs: I am completely fearless. I now have one less source of incidental complexisty weighing me down every time I use Haskell.


### sources.txt

When invoked, cabal-meta looks for a file `sources.txt`.
Each line of `sources.txt` is either a hackage package, a directory, or a github repo (which is cloned as a directory).
A directory is either a local cabal package or contains another `sources.txt` to recurse into.
Here is my sourcex.txt for my Yesod application:

    ./
    sphinx -fone-one-beta
    path/to/yesod/sources

`./` refers to the current directory and thus your current project.
`sphinx` is a hackage package. This is just showing off a somewhat unrelated feature of being able to specify build flags. This lets me stop worrying about build flags and instead just write them down.

To build a Yesod application using the latest code, create a sources.txt in the project directory with:

    ./
    https://github.com/yesodweb/yesod
    https://github.com/yesodweb/shakespeare
    https://github.com/yesodweb/persistent
    https://github.com/yesodweb/wai

This is showing off another feature of cabal-meta: installing from github. Code is placed in a vendor/ directory and updated on every install. We intend to use cabal-meta for future Yesod beta releases.


Please see the full [cabal-meta documentation](http://github.com/yesodweb/cabal-meta)


## Oh no, another Cabal wrapper!

Someone might also rightfully complain that this functionality should be merged into Cabal. I discused this tool a little with Andres, who has already done a great job improving Cabal, and he had some different ideas on the subject. I could spend hours trying to convince everyone that this *idea* is correct, or I could spend that time writing some code and then we can have a conversation about how this concrete tool actually works, based on real user experience. Also, it is much quicker to write a small wrapper: I can use my [shelly](https://github.com/yesodweb/Shelly.hs) library and forget about a whole bunch of concerns the official tool would have. So I believe the wrapper approach is better while still exploring the idea. Once the functionatliy of the wrapper is deemed successfull, its functionality should be merged in to Cabal.


## The bigger picture

This tool is partly inspired from the Ruby Bundler tool, in particular the ability to source local directories and remote repostitories. In addition to these features, Bundler has solved dependency hell in Ruby. Bundler has an easier time isolating installs because a dynamic language does not need to recompile an existing library if the same version is required but with different dependencies.

Someone might rightfully complain that these solutions are band-aids over existing issues. To me the bigger issue is that every Haskeller user needs to spend at least an hour learning how to use hsenv/cabal-dev and cabal-meta to properly install their code. They still must continually think about how to properly use their sandboxes. A new Haskeller is going to read articles and see documentation that just invokes cabal and they are still going to end up in dependency hell. Their only sure way out is to reach out to the community, but that may not happen, and they may decide the hassle of Haskell is not worth the effort.

There are 2 Google Summer of Code proposals already that solve some of the root problems of Cabal dependency hell. I am hoping at least 1 will be accepted.

For now though, give cabal-meta a try (along with hsenv/cabal-dev) and be fearless with your Haskell installs!


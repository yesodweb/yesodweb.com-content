Here are some ideas for GSoC proposals that I think are good matches for the constraints and goals of Google Summer of Code and Haskell.org

Sorry for making this post late, I just realized we were 5 days from the GSoc deadline!


# Make Template Haskell visible

There are critics of Tempalte Haskell, but it is often difficult to find constructive critiques from them.
In a conversation with a new team member we were able to figure out that a big problem with 
Template Haskell for him is that it invisibly generates code.
If the template Haskell generates identifiers then you can no longer just grep for those identifiers.
You can turn on -ddump-splices, but viewing code generated that way is generally a horrible process.
I started a [ghc proposal](https://ghc.haskell.org/trac/ghc/ticket/8624#comment:11) to simply instead output the code to a file.

So if you have Foo.hs it would generate a file Foo.th.hs

Now when you grep for identifiers, they will show up, and you can see all the generated code.

My estimation may be wildly inaccurate, but I think the just above will not take that long.
But that is a good thing for GSoC, to have an easy to achieve first goal that will immediately benefit the community.
I think there are enough directions to take this project to keep the summer busy.

  * There are other sources of code generation such as deriving: these could also be added.
  * Making sure IDEs, and text editors can find the generated code and match it to their source location
  * Shipping the generated code and using it is an interesting idea to explore. This could help with cross-compiling and marking packages as Safe.


# Improve the Persistent (Database serialization) library

Persistent is relied on by a lot of application developers, and it could definitely be improved.
We already started work on Persistent 2.0, which will finally have flexible key support.
There are so many things that could be improved with persistent, please talk with us to nail down a more concrete proposal.


# Make a Haskell development mode

This is a continuation of a successful GSoC project from 2 years ago which created the fsnotify library now used by the community.
The idea has always been to take things further and provide the ability to automatically recompile Haskell projects as files are edited,
and figuring out how to do this as quickly possible through plugins or the GHC API.
We already have yesod devel for yesod, but we want to improve this process and generalize it so it can be used effectively on any Haskell project.

I also want to give a shout out to an existing proposal for [faster Cabal/GHC parallel builds](http://blog.johantibell.com/)
A development mode helps manage the development compilation cycle to keep that process efficient, but ultimately the compilcation process needs to be as fast as possible.


# Notes on applying to GSoC

The best thing you can do is find your project mentors ahead of time.
This requires starting your application process today rather than the night before the hard deadline.
Right now there are 3 possible proposals here, but only 2 mentors available.
I will help look for more mentors in the community to match interest from students.
A single student can make multiple proposals.

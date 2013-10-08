[A previous post](http://www.yesodweb.com/blog/2013/09/classy-mono) announced a new library [mono-traversable](http://hackage.haskell.org/package/mono-traversable) and an overhaul of a prelude replacement [classy-prelude](http://hackage.haskell.org/package/classy-prelude).

Comments on Reddit made me realize that since many Haskellers do not use Prelude replacements many of them are confused about how Prelude replacements are meant to be used.


# Library authors vs. Application Developers

This is a concept that I cannot harp on enough.

Application Developers are free to do whatever they want if they aren't sharing code.
This should be apparent that if you are building a binary it does not effect anyone else.
There are open source applications that are trying to share their code, and the more important this is for the application, the more it will become similar to a library. However, most Haskell application developers are mostly attempting to execute or distribute a working program, not share code.

Library authors are the ones with the constraints. Anything that is part of the API or dependency set will affect users of the library. Even code that is not a direct part of the API can affect a user's ability to read and understand the library code.

Lets keep this in mind as we discuss Prelude replacements.


# Prelude replacements are for application developers 

There is no need to view prelude replacements as some sort of a threat to the Prelude or as a source of fragmentation. In fact the opposite is the case: prelude replacements are an important tool to help the community standardize on continuing to use the Haskell Prelude.

Rather than requiring full community buy-in or fragmentation, prelude replacements work best with partial buy-in. It is a false assumption that prelude replacements cause fragmentation. Prelude replacements are best for application developers that are not distributing libraries. Most non-trivial application developers already end up creating their own modified Prelude, they just are not sharing it as a library with everyone else.

It is an entirely different story for library authors. The goal of a library author is to take as much pain as possible away from their users and put it into the library. They should be very cautious about using prelude replacements (or any other non-standard form of convenience). It adds a dependency, can make their code harder for users to read, and may leak non-standard functions or types. If a prelude replacement is required, consider something with as few changes as possible, such as basic-prelude, or consider building your own from core-prelude. Another possibility is taking just the desired parts from prelude replacements. For example, if someone wants the power of the mono-traversable library, they should just use that library directly rather than classy-prelude.


# Prelude replacements can greatly help the standard Prelude

So I hope we have established that when used appropriately (judiciously in libraries), prelude replacements cause no harm. Their main goal is to make application developers more productive, but they have the side effect of giving real world information to discussions about improving the Prelude.

classy-prelude will give valuable real-world feedback on potential improvements to the Prelude such as standardising semigroups. I was going to open a libraries discussion issue about adding an Eq constraint to groupBy, but I would have had to spend a lot of time discussing the issue on a mail list. Instead we were able to very quickly make the change to classy-prelude (actually we as calling the function groupOn), and we can gather experience over the coming months from actually using it.

A few of the improvements in classy-prelude will be adopted in the Prelude, and most will be rejected. This process helps keep the Prelude informed of potential options and stay up to date.


# How library authors should use new libraries

mono-traversable is a new library. It is very useful for writing code that can operate over different monomorphic containers, but the library itself may make drastic changes or someone may come up with a better library. It has not been vetted by community usage yet.

mono-traversable introduces some new typeclasses. MonoFoldable fully generalize Foldable, so it is possible to just export Mono* versions of functions. However, for library authors, if you want users to use your functions on polymorphic structures, I would advise also exporting the normal polymorphic Foldable version of your function in addition to the MonoFoldable version and letting users decide which to use them. Do feel free to create Mono* instances of the data types you define though.

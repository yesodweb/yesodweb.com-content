I'm writing this blog post to address a personal annoyance of mine as the
maintainer of a large number of Haskell packages.
[Very](https://github.com/yesodweb/persistent/issues/338)
[often](https://github.com/yesodweb/yesod/issues/887), I get bug reports about
lack of documentation on Hackage. This has occurred for years. Most people who
file these issues are not aware of the fact that lack of documentation error is
more often than not a problem with Hackage. Some people are aware of this, and
are asking me to start running a separate tool every time I upload a package to
generate the documentation locally.

I have another annoyance with documentation on Hackage: I'm forced to write my
package's description in a very strange Haddock-inside-cabal format in the
cabal file itself. I need to write a description in any event in a README for
users on Github, so this is purely wasted efforted.

To address both of these issues at the same time, I've [started modifying the
description
field](https://github.com/snoyberg/http-client/commit/3be0c6485a307d448e786116c02ce2b4a62d7a0b#diff-27b3b3f5035b08bb6f576aa9ec75a4e3L4)
in my package's to give a link to their [Stackage](http://www.stackage.org)
address. I'm doing this out of laziness on my part: I can now feel confident
that documentation will be available at pages where people will be pointed to,
I will hopefully get less needless issues opened about "Hackage documentation
is broken," and I don't need to keep two meaningful descriptions of all of my
packages written (one in the weird cabal/Haddock format, one in much nicer
Markdown).

Others are clearly welcome to do this as well, but my main motivation here is
explaining my reasoning for these changes, so I don't get a flood of new
inquiries as to "why do you have such a strange description field in all your
packages?"

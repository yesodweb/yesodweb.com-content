A while ago, I wrote a small package called
[servius](http://www.stackage.org/package/servius), a simple executable that
serves static files with Warp, and will additionally render Hamlet and Lucius
templates. In some earlier package consolidation, the tool became part of
shakespeare, and eventually was commented out (due to concerns around the
dependency list on Hackage looking too big).

Today, I just resurrected this package, and added support for rendering
Markdown files as well. I often times end up working on Markdown files (such as
for this blog, the [Haskell Documentation
project](https://github.com/commercialhaskell/haskelldocumentation), and the FP
Complete blog), and being able to easily view the files in a browser is useful.

As it stands, the three specially-handled file types of Hamlet (.hamlet),
Lucius (.lucius), and Markdown (.markdown and .md). If others wish to add more
templating or markup languages to this list, I'm more than happy to [access
pull requests](https://github.com/snoyberg/servius).

Final note: this package is currently uploaded using the [pvp-bounds feature of
Stack](https://www.fpcomplete.com/blog/2015/09/stack-pvp), so don't be
surprised when the version bounds on Hackage are more restrictive than those in
the repo itself.

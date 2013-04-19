In preparation of the 1.2 release of Yesod, I've made a long-overdue change to
this site: it now supports the display of
[multiple](http://www.yesodweb.com/book-1.2)
[versions](http://www.yesodweb.com/book-1.1) of the book. This should make it
easier for users still sticking with a previous version of the framework. The
plain /book URLs will always contain the most recent versions of the content,
while /book-1.1, /book-1.2, etc, will be used for version-specific content. The
two versions are currently identical, but I'll be making some changes over
the coming weeks to the 1.2 version of the book.

For those of you who have poked around at the [book content
repo](https://github.com/yesodweb/yesodweb.com-content), I'm making one other
important change right now: migrating the book content from DITA to DocBook. My
motivations are twofold: there are more DocBook tools available than DITA in
the open source community, and O'Reilly uses DocBook in their publishing
process.

If anyone notices problems in the online book, please give me a heads-up.

* * *

Just because I can't get away without touting a Yesod 1.2 feature: multiple
versions of the book is a perfect motivating case for using a subsite, as the
exact same routing behavior can be reused for each version of the book. In this
case, I just had each separate subsite point to a different Git branch. If
you're curious to see this in practice, have a look at the [commit introducing
the
subsites](https://github.com/yesodweb/yesodweb.com/commit/3789dc43d114e5b3229b423c89c0fcdd46d4133e),
and in particular the [subsite routing
file](https://github.com/yesodweb/yesodweb.com/blob/3789dc43d114e5b3229b423c89c0fcdd46d4133e/Book/Routes.hs).

Since the Yesod 1.0 release, we've been very careful to try and avoid breaking
changes wherever possible. But occasionally, we still have a few things we want
to improve, whether it be for a more consistent API or better performance. So
I'm announcing the start of a Yesod 1.2 development goal to open another window
of opportunity for such breaking changes.

The goal here is still to maintain backwards compatibility most of the time.
Hopefully the vast majority of code out there will continue working without any
change, or at the most a few minor and clearly documented cases. So don't
consider this an opportunity to make massive and fundamental changes to Yesod:
now is not the time for it.

To keep track of these issues, I've [created a new milestone in our Github
issue tracker](https://github.com/yesodweb/yesod/issues?milestone=5), as well
as a [new yesod1.2 branch](https://github.com/yesodweb/yesod/tree/yesod1.2).
Some of the issues open are fairly major (like refactoring the module structure
of `yesod-core`), while others (like adding an `IsContent` instance for
`ResumableSource`) are minor.

Obviously tackling one of those major issues will be quite involved, but I'd
like to encourage people who have never worked on the Yesod codebase before to
take this opportunity to get involved. Some of the issues available should be
fairly easy to implement, there's no pressing timetable on getting them
implemented, and you can ask for assistance in creating an implementation. If
you see an issue you'd like to tackle, please add a comment indicating so on
the issue.

This is also another opportunity for users to throw out ideas for what they'd
like to see improved. Please go ahead and open up new issues, or discuss these
things on the mailing list.

As for current users: Yesod 1.1 will continue to be supported in terms of bug
fixes for at least a few months after Yesod 1.2 is released, and if there is
demand, that support can continue. The overriding goal in this endeavor is the
smoothest transition possible for the Yesod community.

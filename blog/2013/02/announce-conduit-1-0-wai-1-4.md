Anyone who was on #haskell today probably noticed an inordinately large
number of uploads from me today. Besides some dependencies getting major
version bumps, the motivators for this were two new releases:

* conduit 1.0.0
* wai 1.4.0

The former has been discussed quite a bit. For those unaware: this is a
mostly backwards-compatible update meant to make the library a bit more
accessible. If you're on the FP Complete School of Haskell, you can [read the
online
introduction](https://haskell.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview).

The second release was also very minor: it was the addition of a single field
to the `Request` datatype to track the size of the request body. You can [see
the discussion on
web-devel](http://www.haskell.org/pipermail/web-devel/2013/002633.html). The
only point of contention was whether to use `Maybe` or a custom datatype. I
ultimately decided for the custom data type, for no particular strong reason.

What's really nice about these two updates is their lack of disruption: since
the API breakage is so small, most packages can work with either the old or new
version, and therefore upgrading is a simple matter.

Coming back to the upcoming Yesod 1.2: both of these releases are a good start
towards 1.2. I have some thoughts on improvements to some of the core
components of Yesod, and I'll hopefully be sharing those in the next few weeks.
In the meanwhile, we're still tracking some known feature requests [on the
Github issue
tracker](https://github.com/yesodweb/yesod/issues?milestone=5&page=1&state=open),
so if you want to get involved, pick an issue and implement it!

Now that [persistent 2.0 is out the
door](http://www.yesodweb.com/blog/2014/08/announcing-persistent-2), it's time
to start talking about Yesod version 1.4. First question you might ask: what
happened to Yesod 1.3? Answer: a few of the Yesod libraries (e.g., yesod-auth)
are already on version 1.3, so to avoid confusion, we're jumping straight to
1.4.

Second question: what are we planning on breaking this time? Answer: hopefully
nothing! The main purpose of this release is actually to just remove some
backwards-compatibility hacks in the Yesod codebase for older versions of
dependencies, like shakespeare pre-2.0, conduit pre-1.2, WAI pre-3.0, and
persistent pre-2.0.

There are few exceptions to this, which should hopefully have minimal impact on
users. You can see these in [the detailed change
list](https://github.com/yesodweb/yesod/wiki/Detailed-change-list#yesod-14).
One change I'd like to call out is the updated routing system. This is a
fundamental change to how yesod-routes works. The generated code is
*drastically* simpler as a result. Instead of constructing a data structure
that allows for efficient pattern matching of the request path and then
attempting to parse the resulting pieces, the new code simply generates a
series of clauses, one for each route, and ensures proper parsing using view
patterns. In my initial benchmarking, this made routing twice as fast as Yesod
1.2. I would release this as part of 1.2, but it introduces a new requirement
on the `ViewPatterns` language extension. So instead, I held it off for the 1.4
release.

If there are other breaking changes that people would like to propose, now's
the time to do it. But be aware that I'll likely push back hard on any
breakage. If there's a very good reason for it, we can do it. But I'd rather
keep stability wherever possible.

There's one exception to that rule, which is the purpose of the rest of this
blog post: the scaffolded site. Making changes to the scaffolded site never
breaks existing application, and therefore we can be much more liberal about
changing things there. There *is* a downside in terms of education: all
existing tutorials on the scaffolding would need to be updated. But one of my
points below addresses that.

So here are my proposed scaffolding changes:

* Let's move away from config files towards [environment variables](http://12factor.net/config) for configuration. A config file is still a convenient way to record configuration, but injecting that configuration through environment variables means configuration can also be stored in a database or elsewhere and injected through environment variables the same way.
* Along the same lines, we would no longer need a command line argument to indicate which environment we're in (devel vs production, etc). All such settings would be controlled via environment variables.
* To allow for easy development, we would have a single `YESOD_DEVEL` environment variables which would indicate if we're currently in development. If so, it would apply a number of default environment variable values to avoid the need to set these in your shell manually.
* Finally, and I expect this to be controversial: let's use classy-prelude-yesod in the `Import` module, instead of just taking `Prelude` with a few objectionable functions filtered out.

This is just a first pass at a scaffolding cleanup, I'm sure there are other improvements that can be made as well.

I don't have a specific date on a Yesod 1.4 release, but I'm not expecting it
to be a long development process. The vast majority of the work is already done
(on the yesod-1.4 branch), and that codebase is already being used extensively
in [a rather large Yesod application](https://www.fpcomplete.com), so I'm not
too worried about regressions having slipped in.

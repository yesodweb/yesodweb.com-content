As I'm sure many readers are aware, Google+ is shutting down. This
[affects Yesod's authentication
system](https://github.com/yesodweb/yesod/issues/1579). In particular,
any users of the `Yesod.Auth.GoogleEmail2` module will need to
migrate.

Fortunately for all of us, Patrick Brisbin has written both [the
code](https://www.stackage.org/haddock/lts-13.7/yesod-auth-oauth2-0.6.1.0/Yesod-Auth-OAuth2-Google.html)
for using Google Sign-in, but has also put together [a great migration
blog post](https://pbrisbin.com/posts/googleemail2_deprecation/). If
you're affected, please check that out for a relatively painless
migration.

Earlier today, I migrated Haskellers.com in two commits: the [first
did all of the
work](https://github.com/snoyberg/haskellers/commit/f77bba90d9684afb532639c68e64449523992535),
and the [second made things more
future-proof](https://github.com/snoyberg/haskellers/commit/68198eb05389a96fe3250d2f3e179364531faca5).

As you may be able to tell from the `GoogleEmail2` module, this isn't
the first time we've had to migrate between Google authentication
APIs. Hopefully this one will stick.

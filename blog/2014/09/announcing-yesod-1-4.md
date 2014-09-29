The Yesod team is very happy to announce the release of Yesod 1.4. This includes:

* Releases of all Yesod packages to support version 1.4.
* The book content on yesodweb.com is completely updated for Yesod 1.4, with all snippets confirmed to compile and most of the text proofread from scratch for accuracy (I'll finish the rest in the next week).
* A new Stackage snapshot available at: FIXME.

What I'm
most excited to report is that this was a very minor change to Yesod, and
therefore almost all code should be upgradeable with minor changes. First, the
changelog:

* New routing system, which requires OverloadedStrings and ViewPatterns. Generated code is faster and *much* more readable.
* Dropped backwards compatibility with older versions of dependencies.
* yesod-test now sends HTTP/1.1 as the version. This may require updating tests to expect 303 instead of 302 redirects.
* Overlap checking can be turned off for multipieces, entire routes, and parent routes in a hierarchy. For more information, see [the commit comment](https://github.com/yesodweb/yesod/commit/e23c78f2ce60591574a177de9f3ce5d634384e4a).
* requireAuth and and requireAuthId return a 401 response when a JSON response is requested. See [pull request 783](https://github.com/yesodweb/yesod/pull/783).
* Better support for non-persistent backends in yesod-auth. See [pull request 821](https://github.com/yesodweb/yesod/pull/821) for details. For most users, you can fix this by adding `instance YesodAuthPersist App` to your `Foundation.hs`.

The most significant set of changes in the Yesod ecosystem actually landed in
Persistent 2. However, these were similarly mostly internal changes, so most
end users will be unaffected, barring the need to add some language pragmas.

As has become quite the custom for each major release, I'd like to document the
process of upgrading Haskellers to Yesod 1.4. In this case, you can probably
get by with [just looking at the
diff](https://github.com/snoyberg/haskellers/commit/e01e71371f0334b88b7cee9ce2a461e2009b415b).
In sum:

* As usual, update your cabal file to allow version 1.4.
* Replace `type YesodPersistBackend App = SqlPersist` with `type YesodPersistBackend App = SqlBackend`.
* Add `instance YesodAuthPersist App` to `Foundation.hs`.
* Add the `ViewPatterns` language extension.

Thanks to everyone who provided code, feedback, and testing for this release, I
think it's a very solid one!

Here's a collection of links that provide various other pieces of information about this release:

* [Announcing Persistent 2](http://www.yesodweb.com/blog/2014/08/announcing-persistent-2)
* [Persistent 2.1 Release Candidate](http://www.yesodweb.com/blog/2014/09/persistent-2)
* [Planning Yesod 1.4](http://www.yesodweb.com/blog/2014/09/planning-yesod-1-4)
* [Yesod 1.4 release date](https://groups.google.com/d/msg/yesodweb/7leiDXHe1M8/oEWH83twOK0J)

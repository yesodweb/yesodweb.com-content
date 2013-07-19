As I mentioned on Google+, I've just changed the default book link on the site
to point to the new version 1.2. The book is not yet fully migrated over to
version 1.2 of Yesod, but the entire basics section (i.e., the first 11
chapters) are. In addition, as of today, I've just converted four more
chapters:

* [RESTful Content](http://www.yesodweb.com/book/restful-content)
* [Yesod’s Monads](http://www.yesodweb.com/book/yesods-monads)
* [Blog: i18n, authentication, authorization, and database](http://www.yesodweb.com/book/blog-example-advanced)
* [JSON Web Service](http://www.yesodweb.com/book/json-web-service)

Some comments about notable changes:

* RESTful content had a fairly substantial overhaul to reflect the new
  `TypedContent` approach we have. I personally think the new approach (and
  therefore the new documentation) is much easier to follow.

* Similarly, due to the major change in how the monad transformers work,
  Yesod's monads is quite different. There are also some explanations for
  design decisions towards the end. If you want to understand some of the deeper
  parts of Yesod, I'd recommend reading this chapter again.

* The blog example (finally) has proper formatting, instead of the terrible
  "let's copy some Literate Haskell" hack that I used previously.

* JSON web service is mostly unchanged, and is pretty boring. I'll be adding a
  fancier JSON example after all the chapters are converted.

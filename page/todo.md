Title: TODO List
This is a public Yesod TODO list of tasks that are not yet underway unless noted, largely for those who are interested in contributing to Yesod. Please don't add anything onto here without discussing with Yesod contributors.

## Wiki

* don't redirect from page to topic on update
* better link assigning
* show diffs using the Diff or wordsetdiff package.



## testing infrastructure

* Try out doctest
* wai-test improvements
* develop a high-level haskell html/web application testing library or an interface to an existing one from a different language (Ruby's Capybara). Currently planning on using the shpider library.
* Yesod scaffolding generate testing files

## community infrastructure

* Write LambdaEngine, a set of scripts and Haskell programs to completely set up a vanilla Ubuntu install (either on EC2 or elsewhere) for hosting Yesod webapps. The ideal version would be something like Heroku for haskell.


## hamlet/templates

* a version of the hamlet template package that allows arbitrary haskell code in the templates. (might be easy)
* a simple pass-through html template (easy)

## Yesod-core changes

* Easier support for faster, non-blocking Javascript loading via something akin to require.js.

## Persistent

* All lower case (with underscores) model and column names so you don't have to quote everything when using the sql shell.
* sql statement logging, easy access to a logger in the persistent monad

Make it easier to put logic in the models

* model validations/callbacks
* tie that neatly into forms

MongoDB backend

* support for embedded objects in the MongoDB persistent backend.

## Development tools

* yesod console- load a ghci session with simple access to your Persistent models, printed out in a convenient (tabular?) format.

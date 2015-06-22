# Modularizing Yesod for JSON API servers

Yesod was created with a focus on making rendering HTML pages easy.
Serving up JSON is a simple task that doesn't require much documentation.
And the Yesod book has always focused on the HTML use case and lacked in examples of serving up JSON.
But it has certainly always been up to the task of API server, particularly after adding support for returning multiple content types from the same route.

I have always used Yesod for applications that serve JSON data to an API client or a single-page browser application.
The fact that Yesod can intelligently manage web page dependencies has been a nice bonus for using it to render a homepage, a landing page, and the initial page of a single-page web application.

However, API servers often have no need to render any HTML whatsoever.
For this use case, Yesod can feel like it offers too many things that are not relevant.

The release of the stack tool addresses much of the practical downside of having unnecessary dependencies (sandbox rebuilds, solver issues). However, the downsides do remain even if smaller form, and modularity should be a guiding feature of a web framework.

To address this, the next evolution of Yesod will be about modularization. API servers should only get about what they need by depending on the yesod-core package and should have a slimmed down library footprint.


## Removing the shakespeare templating language dependency

yesod-core currently depends on the shakespeare templating language.
This is for several reasons, none of which prevent us from removing the shakespeare templating language.
So in the next major version release of yesod, there will be a separate yesod-shakespeare package.

### Dog-fooding, or laziness

Since Yesod has always recommended using shakespeare for templating, dog-fooding it for HTML snippets in yesod-core has been a natural way to understand the templating language.
Or it could just be that we were too lazy to write template with blaze-html which in comparison feels much lower level.

### Orphan instances

yesod-shakespeare now contains orphan instances. Haskell's global typeclass system is wonderful for many reasons, but it is also obviously anti-modular in its current form.
But we aren't expecting the orphan instances to cause a problem.

### Breakages

Breakages from the yesod-shakepeare split off should be minimal


## Future changes: removal of blaze-html and aeson dependencies from the core

The stability of Yesod is one of its main selling points.
So we are going to look closely at how to accomplish future changes while minimizing breakage.

### yesod-webpage

a yesod-webpage package would split out Yesod's Widget system (a system for managing webpage dependencies) and probably be where Yesod's usage of html lives.

### Breaking up the Yesod typeclass

The Yesod typeclass will need to be broken up into smaller chunks. Much as YesodAuth and YesodPersist are configuration typeclasses present in other packages, the Yesod typeclass will need to be broken up into smaller chunks.

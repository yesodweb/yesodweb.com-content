_This is a cookbook recipe. Yesod has a strong and growing collection of
cookbook recipes [available on our
Wiki](https://github.com/yesodweb/yesod-cookbook). If you see any recipes
you think should be highlighted here, or would like to request some additions,
please bring it up [on the mailing list](groups.google.com/group/yesodweb) or
[the Google+
community](https://plus.google.com/communities/115485592130002322136)._

Let's take a very simple problem: including some CSS or Javascript on a page in
your Yesod site. There are actually many different ways to do this.
Fortunately, there are just a few recommended options, and the choice really
depends on what kind of use case you're optimizing for. Let's step through
these recommended approaches:

## widgetFile

Probably the simplest thing to do is to use the `widgetFile` helper function
from the scaffolding. This typically looks something like:

```haskell
getHomeR = defaultLayout $ do
    setTitle "My Awesome Site"
    $(widgetFile "home")
```

`widgetFile` will then look in your `templates` folder and find `home.hamlet`,
`home.lucius`, `home.cassius`, and `home.julius`. If it finds any of those, it
will include them in your page appropriately. When you're using `yesod devel`,
it will automatically reload any changes to the Lucius, Cassius and Julius
files.

Some important points:

* For per-page styling, this works out very well. The scaffolding will place
  the generated CSS and JS into an automatically generated external file, so
  users won't have to redownload the contents. But it will also automatically
  concatenate these contents with other CSS and JS snippets, minimizing the
  number of connections.

* Since `defaultLayout` also uses `widgetFile`, you can likewise edit
  `default-layout.lucius`, `default-layout.cassius` and
  `default-layout.julius`.

* If you have a very large amount of CSS or JS that will be used on many pages,
  this is not the right approach to use, since it will inflate the size of each
  page's CSS and JS download. Instead, the contents should be placed in an
  external file.

## addScript(Remote)/addStylesheet(Remote)

If you have large files, such as jQuery or bootstrap.css, you don't want Yesod
to automatically concatenate their contents for each page. Instead, you want to
have a single static file that all pages can reference, and caching will ensure
that the file isn't downloaded multiple times.

The simpler variant is just referring to this file via URL. This works very
well for CDN-hosted files. So for example, to include jQuery, you can use:

```haskell
addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
```

If you want to serve the files yourself, the process is only a bit more
complicated. First, you'll need to create the file inside the `static` folder,
e.g. `static/css/bootstrap.css`. Then you'll need to reference its *type-safe
URL*. To save you from typos, Yesod automatically generates identifiers for all
files in your static folder, based on a simple renaming scheme. So to include
Bootstrap, you would use something like:

```haskell
addStylesheet $ StaticR css_bootstrap_css
```

### addScriptEither/addStylesheetEither

There's one other tweak to throw in. For some specific libraries (including
jQuery), Yesod provides a typeclass where you can specify the location of the
file to be used. This way, all jQuery-using widgets can share a single jQuery
instance. To keep things as general as possible, these libraries are specified
as an `Either` type, giving either a textual URL or a type-safe URL. In order
to add something like that, you need to use `addScriptEither`, e.g.:

```haskell
master <- lift getYesod
addScriptEither $ urlJqueryJs master
```

## toWidget

If you don't want to have to create an external template file, you can use the
Shakesperean quasi-quoters. You just need to promote them to Widgets, e.g.:

```haskell
toWidget [lucius| foo { bar: baz } |]
toWidget [julius| alert("Hello World!"); |]
```

In general, however, I recommend using the external file approach.

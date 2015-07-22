# Yesod-devel

A new development server is upon us. It's name is yesod-devel.  
This post is about yesod-devel which is my Google Summer of Code project and not the current yesod-devel that is part of the yesod framework. It's not yet available and is still under development, meaning a lot of things in this post may change.

yesod-devel is a development server for haskell web applications that are [WAI] compliant.

# What we expect from the application.

This is my opinion of what **I** expect from the web application and this may therefore change depending on what the community thinks. I think this design is good and losely coupled and leaves a lot of freedom to the web application.

At the heart of your application (the root of your web application) we expect you to have an `Application.hs` file which holds the Appliaction module. This is the file pointed to by your `main-is` section of the .cabal file.:

This Application.hs file holds the `main` function which fires up a [`warp`] server at a an address and port specified in an environment variable.
Yesod devel will read everything it needs from the web application from environment variables and not from a config file.  
It is the responsibility of the web application to set environment variables(setEnv). This way yesod-devel is very losely coupled to the web application. That is, we(yesod-devel) will not have to specify the names, paths of your config files or which serialization format it will use.

The environment variables we currently need are:

    * haskellWebAddress="<ip_address>/localhost"
    * haskellWebPort="<port_number>"

# What you should expect from yesod-devel.

#### Automatic source and data file discovery.

You shouldn't expect to tell yesod-devel where your source files or data files (hamelet files and so forth) are as long as your web application knows where everything is. All you need to do is call the yesod-devel binary inside your app's root.

#### Building and running your code.

yesod-devel when run in your web application's working directory will run build and run your application on localhost:3000.

#### Automatic code reloading.

Yesod-devel supports automatic code reloading for any file modified in the current working directory. This is more proof of just how losely coupled yesod-devel will be from your application.

Newly added files don't trigger a recompile and neither do deleted files. However, file modifications do trigger a recompile.
This is a deliberate design choice. Text editors as well as other programs keep adding and removing files from the file system and if we listened for any randomly created file or deleted file to trigger a recompile we would end up triggering useless recompiles.

This however means there's a trade-off. For being so losely coupled we have to manually restart yesod-devel everytime we add or delete files.

#### Reverse proxying.

Yesod-devel will start listening on the address and port specified in your environment variables `haskellWebAddress` and `haskellWebPort` respectively and reverse proxy it to your **localhost:3000**.

#### Report error messages to the browser.

Yesod-devel will report error messages from ghc to the web browser on localhost:3000.

#### Command line arguments.

Currently yesod-devel takes no command line arguments.

However, in the plans are the following.

  * \--configure-with <config flags to cabal configure>
  * \--no-reverse-proxying
  * \--show-iface fileName.hs

You should be fine without passing any of these arguments unless you have a special reason to.

Currently yesod-devel will configure your web application with the following flags to cabal.
  
  * \-flibrary-only
  * \--disable-tests
  * \--disable-benchmarks
  * \-fdevel
  * \--disable-library-profiling
  * \--with-ld=yesod-ld-wrapper
  * \--with-ghc=yesod-ghc-wrapper
  * \--with-ar=yesod-ar-wrapper
  * \--with-hc-pkg=ghc-pkg

I assume that these arguments are self explanatory.

[WAI]: https://www.yesodweb.com/book/web-application-interface
[`warp`]: http://hackage.haskell.org/package/warp

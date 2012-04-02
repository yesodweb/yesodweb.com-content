Title: Yesod quick start guide

## Getting Haskell

You'll need two main tools: the Glasgow Haskell Compiler (GHC) and
cabal-install, the package installer. The best way to get them is with the
[Haskell Platform](http://hackage.haskell.org/platform/).

If you're on Windows or Mac, download the installers from the Haskell Platform
site. For Linux users, most distributions already include the platform in the
repositories. On most Debian-based systems, for example, you can just run

    sudo apt-get install haskell-platform
    
Make sure to add `$HOME/.cabal/bin` to your `PATH`. Once you're set up, run

    cabal update
    
to download a list of available packages. For more information on Haskell tools, see the [tools chapter of the Mezzo Haskell book](https://github.com/mezzohaskell/mezzohaskell/blob/master/chapters/tools.md).

## Install Yesod

Building Yesod and all of its dependencies is a simple procedure. Just run:

    cabal install yesod-platform

Note that this will be installing a large number of packages, and may take a
while. (15 minutes on modern systems, up to 40 minutes on older systems.) This
is a one time setup, and will have no impact on normal development or
runtime performance.

## Start a new site

Now I'm sure you want to test this out! Installing Yesod also provided you with
a `yesod` executable, which has two important commands.

    yesod init
    
will ask
you a few questions, and then generate a scaffolded site for you. Once you
change into that directory, type

    yesod devel
    
to start the development server.
You can now access your site at
[http://localhost:3000/](http://localhost:3000/).

## Learn more

Now it's time to start coding! You can play around with the code right now, or
if you want to learn more, check out these resources:

* [Yesod tutorial](http://yannesposito.com/Scratch/en/blog/Yesod-tutorial-for-newbies/)
* [Yesod book](/book)
* [Screencasts](/page/screencasts)
* [The Wiki](/wiki)
* [Community](/page/community)

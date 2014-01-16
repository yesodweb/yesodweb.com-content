Title: Yesod quick start guide

## FP Haskell Center

The easiest way to get started with Yesod is by using [FP Haskell
Center](https://www.fpcomplete.com/business/haskell-center/overview/). FP
Haskell Center provides a web based development environment with all of the
Haskell toolchain and Yesod libraries preinstalled. You can get started right
away by [cloning an existing Yesod
project](https://www.fpcomplete.com/school/project-templates/file-server).

The rest of this page provides instructions for installing on your local
system.

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
    
to download a list of available packages. For more information on Haskell
tools, see the
[tools chapter of the Mezzo Haskell book](https://github.com/mezzohaskell/mezzohaskell/blob/master/chapters/tools.md).
Mac users may also want to see
[this page](http://www.haskell.org/haskellwiki/Mac_OS_X_Common_Installation_Paths)
for help with their `PATH` setting.

__Note__: There is a bug in the 64-bit GHC 7.4 for Mac which prevents `yesod devel` from working correctly. This bug has been corrected in 7.6. However, as 7.4 is still the recommended platform for development, it is advisable to install the 32-bit version of GHC.

## Install Yesod

Building Yesod and all of its dependencies is a simple procedure. Just run:

    cabal update
    cabal install yesod-platform yesod-bin --max-backjumps=-1 --reorder-goals

Note that this will be installing a large number of packages, and may take a
while. (15 minutes on modern systems, up to 40 minutes on older systems.) This
is a one time setup, and will have no impact on normal development or
runtime performance.

## Start a new site

Now I'm sure you want to test this out! The `yesod` executable has two important commands.

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

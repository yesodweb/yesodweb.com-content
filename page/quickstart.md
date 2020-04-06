Title: Yesod quick start guide

1. Follow the [FP Complete get started guide](https://haskell.fpcomplete.com/get-started) to get the Stack build tool.
    * On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Create a new scaffolded site: `stack new my-project yesod-sqlite && cd my-project`
    * NOTE: You can see [other template options on Github](https://github.com/yesodweb/stack-templates).
3. Install the yesod command line tool: `stack install yesod-bin --install-ghc`
4. Build libraries: `stack build`
5. Launch devel server: `stack exec -- yesod devel`
6. View your Yesod site at [http://localhost:3000/](http://localhost:3000/)

NOTE: If you get an error message about `GHC_PACKAGE_PATH` at step (5), you
need to install a newer version of yesod-bin. Try running `stack build
yesod-bin-1.4.11` and rerunning `stack exec -- yesod devel`. Also, if you choose
the "mini" scaffolding, `yesod devel` will not work.

### System libraries

Note that you will need the dev version of some system libraries to be
available for the above steps to work. For example, on Ubuntu, you may need to
run something like:

    sudo apt-get install -y build-essential zlib1g-dev

If you're using a database, you'll likely need to install the system libraries
to talk to it. Some Ubuntu examples are:

    sudo apt-get install -y libmysqlclient-dev
    sudo apt-get install -y libpq-dev

## Learn more

Now it's time to start coding! You can play around with the code right now, or
if you want to learn more, check out these resources:

* [Yesod book](/book)
* [Community](/page/community)
* [The Wiki](/wiki)
* [Haskell Documentation](https://haskell-lang.org/documentation)
* [Screencasts](/page/screencasts)
* [Cloning FluxBB](https://siskam.link/2018-04-14-cloning-fluxbb.html) (blog post about writing a forum in Yesod with Esqueleto)
* [Yesod tutorial](http://yannesposito.com/Scratch/en/blog/Yesod-tutorial-for-newbies/) (slightly outdated, book provides more up-to-date content)

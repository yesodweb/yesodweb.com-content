Title: Yesod quick start guide

The Yesod team strongly recommends using [the stack build tool](https://github.com/commercialhaskell/stack#readme) for developing with stack. There are other build tools available in the Haskell world which will likely work with Yesod, but stack provides the easiest experience. To get started:

1. Follow the [installation instructions for stack](https://github.com/commercialhaskell/stack/wiki/Downloads) to get stack.
2. Install the yesod command line tool: `stack install yesod-bin cabal-install --install-ghc`
3. Create a new scaffolded site: `stack exec -- yesod init --bare && stack init`
4. Build libraries: `stack build`
5. Launch devel server: `stack exec -- yesod devel`
6. View your Yesod site at [http://localhost:3000/](http://localhost:3000/)

NOTE: If you get an error message about `GHC_PACKAGE_PATH` at step (5), you
need to install a newer version of yesod-bin. Try running `stack install
yesod-bin-1.4.11` and rerunning `stack exec --yesod devel`. Also, if you choose
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
* [Yesod tutorial](http://yannesposito.com/Scratch/en/blog/Yesod-tutorial-for-newbies/)
* [The Wiki](/wiki)
* [Screencasts](/page/screencasts)

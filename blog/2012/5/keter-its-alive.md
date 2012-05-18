Take a good look at this site. Do you see something different?  Well, to quote
Captain Jack Sparrow, it's something not there to be noticed.

This site is now being deployed via [Keter](https://github.com/snoyberg/keter).
In other words, __it's alive__!!! Keter is monitoring the yesodweb.com
application process, handling logs and log rotation, configuring Nginx as
necessary, and automatically deploying code updates. (If yesodweb.com used a
database, Keter would handle that too.)

I've always believed heavily in dogfooding, which is why I'm rolling out Keter
now. However, this should still be considered alpha quality code. I'll be
keeping a close watch on the server to make sure nothing funny goes on.
However, I think the code base is at the point where it makes sense to start
some more serious testing.

I'm going to avoid going into the implementation details in this blog post, and
instead focus on the end user. If people are interested in a post on the
technical aspects of Keter itself, let me know and I'll try to write one.

## Prepare the Server

You'll need to install two things on the server: Nginx and PostgreSQL. To do so
on a Debian based system, just run:

    sudo apt-get install nginx postgresql

## Build the Executable

Since Keter is just another [package on Hackage](http://hackage.haskell.org/package/keter), you can build it with a
simple `cabal install keter`. However, doing so on your server would entail installing an
entire Haskell toolchain, which is not something we generally
encourage. So for now, the recommended approach would be to do the `cabal install`
on a local system with the same architecture and distribution as your
web server, and then copy the executable to the server. If you happen to be
running Ubuntu 12.04 64-bit, I've made a
[copy of my executable](http://www.yesodweb.com/static/keter.bz2) available online.

Now, a few caveats:

* This code will only work on Linux for now, since it uses inotify for checking for file system changes. I'm hoping that once our [Google Summer of Code project](http://www.google-melange.com/gsoc/project/google/gsoc2012/mdittmer/18001) gets underway, I'll be able to leverage the cross platform file watching code Luite has been working on and which Mark will hopefully be releasing.
* There are a few assumptions built into the executable, like where to write nginx config files (`/etc/nginx/sites-enabled/keter`) and the name of the user for running PostgreSQL admin activities (`postgres`). This seems to work perfectly on Debian-based systems, but may not translate well elsewhere. If people are using other distributions on their servers and want to add support to Keter, please be in touch.

Once you have the executable, place it in `/usr/bin` on your server. (Of
course, you can choose a different location, just keep in mind your changes as
you continue.)

## Run on Startup

Next you'll want to make sure that keter is run on startup. This will depend
again on your distribution. I'm going to assume using Ubuntu and Upstart. The
following script works for me:

    # /etc/init/keter.conf
    start on (net-device-up and local-filesystems and runlevel [2345])
    stop on runlevel [016]
    respawn
    
    console none
    
    exec /usr/bin/keter /opt/keter

Notice that `keter` takes a single argument: the folder to use as its base of
operations. You can use whatever folder you want, but `/opt/keter` seems like a
good choice to me.

You can start running Keter immediately with `sudo start keter`.

## Bundle Your App

An application is sent to Keter as a keter bundle. This is a single GZIPed
tarball with a .keter filename extension, and it contains your executable,
resources, and your keter config file. This last file __must__ be placed at
`config/keter.yaml`. Let's set up a simple example. Suppose you have the
following inanely boring web application:

    -- hello.hs
    {-# LANGUAGE OverloadedStrings #-}
    import System.Environment (getEnv)
    import Network.Wai
    import Network.Wai.Handler.Warp
    import Network.HTTP.Types
    import Control.Monad.IO.Class
    import System.IO (hFlush, stdout)
    
    main :: IO ()
    main = do
        putStrLn "Application is starting"
        liftIO $ hFlush stdout
        portS <- getEnv "PORT"
        let port = read portS
        run port $ \req -> do
            liftIO $ putStrLn $ "Received a request at: " ++ show (pathInfo req)
            liftIO $ hFlush stdout
            return $ responseLBS status200 [("content-type", "text/plain")] "Hello World!"

There's one Keter-specific aspect to this code to mention: reading the `PORT`
environment variable. This is how an app is told where to listen for requests.
Nginx will work as a front proxy and receive all requests from clients, and
then pass them off to the appropriate web app based on hostname. This is the
*only* change necessary to get an app to work with Keter. All Yesod scaffolded
sites support this functionality already, and it should be trivial to add to
Snap and Happstack applications too.

OK, now that we have our app, let's look at our keter.yaml file:

    # config/keter.yaml
    exec: ../hello
    host: www.example.com

This is pretty simple. `exec` says where to find the executable. This will be
relative to the `keter.yaml` file, which is inside the `config` folder, so we
use `../hello`. The `host` setting says which hostname to listen on. As a side note, the
hostname will also be provided to your app via the `APPROOT` environment
variable, which is how Yesod apps generate absolute URLs. This can be safely
ignored: Yesod handles this automatically, and the data likely isn't necessary
for other frameworks.

There are two other options you can specify in your keter.yaml file:

* `args` is a list of command line arguments to pass to your application.
* `postgres` is a boolean indicating whether a PostgreSQL database should be created for this app. If `true`, then the database parameters will be provided via environment variables. (Persistent can parse these automatically.)

So a basic config file for a Yesod site called `myawesomeapp` running on `www.myawesomeapp.com` and using PostgreSQL would be:

    exec: ../dist/build/myawesomeapp/myawesomeapp
    args:
        - production
    host: www.myawesomeapp.com
    postgres: true

Coming back to our simple hello world app, here's a shell script that can
bundle up your executable and config file:

    #!/bin/bash -ex
    ghc --make -threaded hello.hs
    strip hello
    rm -f hello.keter
    tar czfv hello.keter hello config/keter.yaml

This will produce a hello.keter bundle which is all ready to be deployed.

## Deploying

This is the (relatively) easy part. Copy your bundle to `/opt/keter/incoming`.
That should be all there is to it. Now the caveats:

* You may be missing libraries or other resources on the server, which can cause your app not to run. If after copying the bundle to the server your app doesn't start, try running the executable file itself manually.
* If it takes your app more than 60 seconds to start answering HTTP requests, Keter will determine that it's unresponsive and shut it down.
* The folder provided for your app should *not* be considered a good place for permanent storage, as Keter will wipe it out every time you redeploy your app. If you need permanent storage, than either (1) store it elsewhere in the filesystem, or (2) put it in a database/S3/whatever. The second option is definitely recommended in general, as it will make it much easier to migrate to a different server later.

Oh, and obviously you'll need to set up your DNS to point to your server. Maybe
we'll provide some ability to automate this via [Amazon Route 53](http://aws.amazon.com/route53/) in the future.

## Logs

Keter keeps fairly detailed logs of its own activities, plus logs all stdout and stderr output from each application. Logs each go into their own folder, and are automatically rotated. To see what's been happening with Keter, look in `/opt/keter/log/keter/current.log`. For example, the log on yesodweb.com contains:

    2012-05-17 11:16:35.08: Unpacking bundle '/opt/keter/incoming/yesodweb.keter' into folder: /opt/keter/temp/yesodweb-0
    2012-05-17 11:16:35.29: Created process: config/../dist/build/yesodweb/yesodweb

Each app log gets placed in `/opt/keter/log/app-<appname>`, with error output
going in the `err` subfolder. Keter does nothing to modify the contents of
these files, it simply pipes directly from the app to the file.

## What's Next?

The main thing now is testing: making sure it's working correctly under all
circumstances. Keter will give fairly detailed diagnositcs about itself, and
logs every single exception that gets thrown. This should make it easier to
track down any problems. I've run a fair amount of testing myself, and
everything seems to be in order, but obviously we won't know that for certain
until more reports come in.

There are also some features I'd like to add:

* We want to automate deployment of this as much as possible, most likely by getting distribution packages. In Ubuntu, for example, I want to set up a PPA. Unfortunately, I have no experience with PPAs or Debian packages at all, and would really appreciate help on this.
* Similarly, for even easier deployment, I'd like to provide an AMI (Amazon Machine Image) so that setting up a new host is basically a one-click process.
* I originally said that I wanted Keter to provide a web interface for viewing status and log files. I've changed my mind on this a bit: this shouldn't be built into Keter, but instead be provided as a separate app that can be added to Keter. This will also allow people to customize things easily, and for us to have different options (maybe someone wants a Happstack-based app?). If anyone's interested in writing this app, let me know, it shouldn't be too difficult.
* Integration with some kind of build server. Goal: push to Github, code is built and deployed automatically. I think I was unclear on what I wanted last time I stated this: the idea is __not__ to replace Cabal. Cabal is a wonderful piece of architecture, and is the de-facto standard for building Haskell code (for good reason). My goal is to have something that sits on top of Cabal.
* Improvements to Yesod for log files. This isn't actually a change in Keter at all, but one which Keter has made me aware of. When we set up the new logging system in Yesod, we turned off buffer flushing for performance reasons. I was a bit uneasy about it at the time, but I think at this point it's pretty obviously the wrong approach, as it prevents the log files from being written in any meaningful way (especially when deploying a new app). I've worked around this in yesodweb.com by [flushing the log every second](https://github.com/yesodweb/yesodweb.com/commit/b2325c31bad8934ae53b5f5d4dc24c580699ec31#L0R49), but that's just a temporary workaround; we need to get a better solution in the libraries themselves. I also think we need to improve the log format itself, likely by moving to [moving to Apache style logging](https://github.com/yesodweb/yesod/issues/269).

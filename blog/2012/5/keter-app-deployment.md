tl;dr: There's an [experimental deployment system available for testing](https://github.com/snoyberg/keter).

I
[mentioned](https://groups.google.com/forum/?fromgroups#!topic/yesodweb/v0XEaSJ14rk)
on the Yesod and web-devel mailing lists last week that I was unsatisfied with
the existing Yesod deployment scripts. I worked on the problem a bit this week,
and now have some code worth testing. For now, this system is called *Keter*,
though I expect that to change once someone gives me a better idea.

If you're wondering, Keter is Hebrew for "crown." (You should all be very proud
of me; it took a lot of self restraint not to call this post "Yesod's crowning
achievement.") Those familiar with
[kabalah](http://www.yesodweb.com/blog/2012/04/replacing-cabal) might notice a
connection between Yesod and Keter. But I digress.

## Advantages

This system already provides quite a bit of convenience for users:

* Allows updating your app with zero downtime.
* Checks that new versions of your app actually work before switching to them.
* Automatically configures PostgreSQL databases as necessary.
* Monitors processes and restarts them if they crash.

__Note__: As [pointed out on Reddit](http://www.reddit.com/r/haskell/comments/thygx/keter_web_app_deployment/c4msbpa),
this tool by itself is *not* sufficient to guarantee you'll never have downtime
with deployments. One issue is database migrations, which can still cause
downtime. My point is that with this approach, as opposed to the previous
deployment scripts, it *is* possibility to have zero downtime between versions.

## Usage

Keter is a single executable. The program takes a single argument: a folder
from which to run. This folder will have an `incoming` subfolder, where you'll
copy in your keter bundles (more on this in a moment).

Keter relies on a preinstalled Nginx to act as a reverse proxy. It will write
out an nginx config file (for now, hardcoded to
`/etc/nginx/sites-enabled/keter`) and then tell Nginx to reload (via
`/etc/init.d/nginx reload`) on any changes.

It also requires a running PostgreSQL server. It will create new accounts and
databases as necessary via `sudo -u postgres psql`. Again, the exact approach
used is currently hard-coded, but will be made more extensible going forward.

### Keter Bundles

In order to have Keter manage an app, you must package it up in a keter bundle.
This is simply a GZIPed tar file with a .keter extension. There is only one
required file in this bundle: config/keter.yaml. Let's look at a sample
keter.yaml file for a scaffolded Yesod site:

    exec: ../dist/build/pgtest/pgtest
    args:
        - production
    host: pgtest
    postgres: true

`exec` gives the location of the executable, relative to the config file
itself. `args` is the list of command line arguments. `host` gives the hostname
that should be bound to, and `postgres` indicates whether or not a PostgreSQL
database should be created. If `postgres` is `true`, then a randomly named
database will be created, and the information will be passed to your app via
environment variables. Persistent is already configured to accept these
variables, so no additional work is necessary on your part.

You might be concerned about that host value. Doesn't it violate DRY to have to
specify the host in `keter.yaml`, and then specify the approot in
`settings.yml`? The answer is yes. That's why updating `approot` is not
required. Keter will set an `APPROOT` environment variable automatically, based
on the `host` value, and Yesod will automatically use that to override the
value in `settings.yml`.

In other words: that 5-line Yaml file is just about all you need to do to
configure your app for deployment. Making a bundle is equally easy, e.g.:

    rm -f pgtest.keter
    tar zcfv pgtest.keter dist/build/pgtest/pgtest static config

Once Keter is more fully developed and ready to be used, we'll set up the
scaffolding to include the keter.yaml file, and add a command like `yesod
keter` to create these bundles automatically.

## How it Works

The projects [README file](https://github.com/snoyberg/keter/blob/master/README.md) actually gives a
pretty thorough breakdown of the components involved in the code. (Note: as
mentioned below, logging has not yet been implemented.) One thing to note is
that old versions of apps will be automatically terminated (via SIGTERM) and
their folders deleted when a new version is available. This has two important
ramifications:

1. If you want to handle long-running connections, you need to catch SIGTERM and let your app continue to run.
2. You can't put anything important in your local folder. If you need to persist data, either write it elsewhere in the filesystem, or put it in your database.

As for the code itself, it makes heavy use of message passing. Each component
(Nginx manager, process watcher, Postgres configuration) is handled by a
separate thread, and messages are passed over `Chan`s. I think this actually
makes the code fairly easy to work with, though input from others is greatly
appreciated. (This is really my first time making such heavy use of this
style.) There isn't much in the way of code comments yet, but that will be
fixed going forward as well.

## TODO

I still intend to add a comprehensive logging framework. However, I consider
myself to be quite a n00b at proper log file maintenance. If anyone wants to
brainstorm on the best approach here, I'd appreciate it. Once the log framework
is in place, I intend to stick a web frontend on the whole thing to give you
access to app logs, in addition to status of apps and possibly more statistics.
Input here is welcome as well.

There are also likely plenty of ways to clean up the code. If anyone has ideas,
please let me know and/or send a pull request.

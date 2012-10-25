It's been a while since I wrote a post about the status of Keter, and there
have been a number of important changes and feature additions in the past few
months. This post should fill in some gaps.

For those of you not familiar with it, Keter is a web application deployment
manager. You package up your executable, static resources, and a config file,
and Keter will:

* Run and monitor your server process.
* Assign a port number to your app.
* Handle virtual hosting to route requests to the right app.
* Start running new versions of your app as they are providing, and atomically
  switch which app is proxied to.

The [Keter README
file](https://github.com/snoyberg/keter/blob/master/README.md) has full
instructions on getting up-and-running with Keter, and the code itself can be
installed from Hackage. To get up-and-running quickly on Ubuntu-based systems,
you can run:

    wget -O - https://raw.github.com/snoyberg/keter/master/setup-keter.sh | bash -ex

Keter is designed to work with any web application, the only requirement is
that the app answer HTTP requests on the PORT environment variable provided to
it. It is completely agnostic to your choice of development language and
toolset.

As far as notable features added in the past few months:

*   Removed Nginx dependency. Keter itself provides its own reverse proxying
    system via
    [http-reverse-proxy](http://hackage.haskell.org/package/http-reverse-proxy). It
    uses that package's raw proxy system, which is able to get by with a
    substantially reduced overhead versus a full-blown proxy server.

    Moving away from Nginx has simplified the deployment process, and according
    to user reports improved response times. (Note: I have *not* performed any
    significant benchmarking to back that up, the reports are anecdotal.)

*   Using [Vincent Hanquez's tls
    package](http://hackage.haskell.org/package/tls) and the
    [network-conduit-tls](http://hackage.haskell.org/package/network-conduit-tls)
    wrapper, Keter now fully supports secure connections. In practice, you can
    really only have one secure domain per site (due to the normal restriction of
    needing a separate IP address per SSL host). The README file shows how to
    configure this setup.

*   An application can listen on multiple hosts at the same time. It will still
    maintain a single canonical host for setting the APPROOT environment
    variable.

*   Static file hosting is built-in. Each application can define zero or more
    host-folder mappings. Under the surface, this uses Warp and wai-app-static,
    providing efficient file serving and caching features out of the box.

*   Hostname redirects are built in. The main use case for this is
    automatically redirecting from `mysite.com` to `www.mysite.com` (or the
    reverse, if you prefer that).

*   setuid support built-in. If you set a setuid value in the config file, all
    processes will be run as that user and project files owned by that user.

Keter is being used in the wild in a number of places, including this site. The
Keter instance is actually hosting four different applications, all on an
Amazon EC2 micro instance, so the overhead is not very high. If you're looking
for a tool to simplify deployment of your web applications, try Keter.

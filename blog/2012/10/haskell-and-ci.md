Continuous integration is about keeping your code base in shape and to
never let it degenerate. This is done through setting up a CI service
that will instantly build your project and run the tests once the code
base changes. All projects, in particular those with more than one
developer, will benefit from continuous integration.  But is it worth
the trouble in setting it up and maintaining it? If you answered no,
this post might change your mind.

## Software alternatives

[Jenkins CI] and [Travis CI] are the two continuous integration servers
I've been playing around with. They both have their pros and cons and
hopefully after reading this blog post you'll be able to pick the one
that best fits your project.

I'll be keeping my advice general to Haskell projects using the typical
cabal setup. But since this is the Yesod blog I'll include a section
related to that too.

### Travis CI

Travis is a free CI service for open source projects hosted on GitHub
and it integrates well with GitHub. Travis launched in 2011 but Haskell
support were not added until March 2012.

The build agents seem to be quite fast but the maximum build time is
limited. This can be an issue for Haskell packages that have heavy
dependencies like the yesod-platform. Furthermore, Travis is still quite
new and
[outages](http://about.travis-ci.org/blog/2012-09-24-post-mortem-pull-request-unavailability/)
can happen or Travis might just decide to decrease the build times, the
current alloted build times seem to between 8 to 20 minutes depending on
which build step is running. Currently Travis only supports public
repositories hosted at GitHub.

Here is a list of great features of Travis:

  * Extremely easy [setup for Haskell](http://about.travis-ci.org/docs/user/languages/haskell/)
  * Supports all your public repos and your organization's public repos!
  * When somebody does a pull request, it will build those patches and
    it integrates well into the GitHub web interface.  [Read
    more](https://github.com/blog/1227-commit-status-api)
  * Want build results to be announced in your favorite irc channel?
    Travis has many cool notification setups. Check out all the ways you
    can
    [configure](http://about.travis-ci.org/docs/user/build-configuration/)
    Travis!


### Jenkins CI

Many Haskellers already use Jenkins for their CI needs.  To get Jenkins
running you have to install it on your own server. Jenkins is a large
Java project. There is an [*incredible*](https://github.com/jenkinsci)
amount of open source plugins for it, including both Git and darcs
Integration. Setting up Haskell projects is quite easy. I've maintained
the build server for Yesod for over a year now and I've seldom needed to update
the [Jenkins
configuration](https://github.com/Tarrasch/Jenkins-home-dir/) of that
server.

Here is a list of great features of Jenkins:

  * You don't need to use GitHub nor Git and you don't need to clutter
    your repo with a `.travis.yml` file
  * There are plugins for most things
  * Flexibility, you can add workers if you have many projects and you
    feel that Travis is congested
  * Jenkins can configure to your needs. In the Yesod world we've set up
    the `yesod` project to be a *post build* of `wai`, `persistence` and
    `shakepeare`. That enables continuous integration across dependent
    projects
  * Jobs can start in any environment. Travis always run from clean
    environments. This isn't always desired. In general you only want to
    build your project and not it's dependencies. *Fast builds* is a
    good CI practice!
  * Jenkins is [community
    driven](http://jenkins-ci.org/content/about-jenkins-ci)


### Some aftermath

So for a long time there have not been any popular continuous
integration hosting sites like Travis, so people had to host the CI
service themselves. As for source control hosting, Git became widely
[recognized](http://www.youtube.com/watch?v=4XpnKHJAok8) around 2007 and
the GitHub source hosting site launched a year after.  Continuous
integration is a software engineering term coined in 1999.  Two popular
CI services, Hudson and Teamcity, seem to have been released roughly
around 2006 and now Travis have come along to simplify continuous
integration by hosting the actual build workers. The reason that hosted
CI for open source projects took longer to appear could be that more
expensive computer resources are required (computation instead of storage), also
the sandboxing technology and such might have made it more complicated.
In either case, Travis is free for us but it does cost money, therefor a
[donating](http://love.travis-ci.org/)
option is available.

## Setting up the build steps

Luckily, there is almost nothing you need to know when setting up your
Jenkins jobs or your Travis configuration. For Jenkins, just have a
"Execute shell" step where you enter the commands you use to build your
package, typically something like `cabal configure && cabal install`.
For Travis the default steps work for most projects, that is, you only
need to specify that you use Haskell.

### CI for Yesod sites

A Yesod site package can use
the general Haskell setup. If you happen to use the
[`yesod-platform`](http://www.yesodweb.com/blog/2012/03/announcing-yesod-platform)
meta package, you will need [this
trick](https://github.com/dtekcth/DtekPortalen/blob/167df56/.travis.yml#L4)
in your build step to ensure that the meta package gets built before the
other dependencies.

## Conclusion -- which CI software should I use?

If your project is open on GitHub and have few dependencies and get
a lot of pull requests, then Travis is ideal for you. However, for a
closed project not using Git in a company that already has a Jenkins
cluster, Jenkins fits better.

There are of course other CI services as well. Some with Haskell
support! For instance, somebody have created a [cabal
plugin](https://github.com/fushunpoon/cabal-teamcity-plugin) for
TeamCity.

## What's next?

The only thing left now is for the Haskell community to slowly start to
adopt continuous integration in open source projects wherever possible.
I've [started slow](https://github.com/Tarrasch/yesod-text-markdown)
myself.  If you use GitHub, pick any of your Haskell projects, register
it for CI at [Travis](http://travis-ci.org/), add your
[`.travis.yml`](http://about.travis-ci.org/docs/user/build-configuration/)
file and add the [build status
image](http://about.travis-ci.org/docs/user/status-images/) at the top
of your README. It will be appreciated by the community!

[Jenkins CI]: http://jenkins-ci.org/
[Travis CI]: http://about.travis-ci.org/

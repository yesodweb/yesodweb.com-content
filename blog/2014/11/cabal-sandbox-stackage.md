Installing Haskell packages is still a pain. But I believe the community has some good enough workarounds that puts Haskell on par with a lot of other programming languages.
The problem is mostly that the tools and techniques are newer, do not always integrate easily, and are still lacking some automation.

My strategy for successful installation:

* Install through Stackage
* Use a sandbox when you start having complexities
* freeze (application) dependencies

Simple definitions:

* Stackage is Stable Hackage: a curated list of packages that are guaranteed to work together
* A sandbox is a project-local package installation
* Freezing is specifying exact dependency versions.

I really hope that Stackage (and sandboxes to a certain extent) are temporary workarounds before we have an amazing installation system such as [backpack](http://plv.mpi-sws.org/backpack/).
But right now, I think this is the best general-purpose solution we have. There are other tools that you can use if you are not on Windows:

* hsenv (instead of sandboxes)
* nix (instead of Stackage and sandboxes)

hsenv has been a great tool that I have used in the past, but I personally don't think that sandboxing at the shell level with hsenv is the best choice architecturally.
I don't want to have a sandbox name on my command line to remind me that it is working correctly, I just want cabal to handle sandboxes automatically.


## Using Stackage

See the [Stackage documentation](https://github.com/fpco/stackage/wiki/Preparing-your-system-to-use-Stackage). You just need to change the remote-repo setting in your ~/.cabal/config file.

Stackage is a curated list of packages that are guaranteed to work together.
Stackage solves dependency hell with exclusive and inclusive package snapshots, but it cannot be used on every project.

Stackage offers 2 package lists: exclusive, and inclusive.
Exclusive includes only packages vetted by Stackage.
Exclusive will always work, even for global installations.
This has the nice effect of speeding up installation and keeping your disk usage low, whereas if you default to using sandboxes and you are making minor fixes to libraries you can end up with huge disk usage.
However, you may eventually need packages not on Stackage, at which point you will need to use the inclusive snapshot.
At some point you will be dealing with conflicts between projects, and then you definitely need to start using sandboxes. 
The biggest problem with Stackage is that you may need a newer version of a package than what is on the exclusive list.
At that point you definitely need to stop using Stackage and start using a sandbox.

If you think a project has complex dependencies, which probably includes most applications in a team work setting, you will probably want to start with a sandbox.



## Sandboxes

    cabal sandbox init

A sandbox is a project-local package installation.
It solves the problem of installation conflicts with other projects (either actively over-writing each-other or passively sabotaging install problems).
However, the biggest problem with sandboxes is that unlike Stackage exclusive, you still have no guarantee that cabal will be able to figure out how to install your dependencies.

sandboxes are mostly orthogonal to Stackage.
If you can use Stackage exclusive, you should, and if you never did a `cabal update`, you would have no need for a sandbox with Stackage exclusive.
When I am making minor library patches, I try to just use my global package database with Stackage to avoid bloating disk usage from redundant installs.

So even with Stackage we are going to end up wanting to create sandboxes.
But we would still like to use Stackage in our sandbox: this will give us the highest probability of a successful install.
Unfortunately, Stackage (remote-repo) integration does not work for a sandbox.

The good news is that there is a patch for Cabal that has already been merged (but not yet released).
Even better news is that [you can use Stackage with a sandbox today](https://www.fpcomplete.com/blog/2014/10/new-stackage-features#using-stackage-without-changing-your-repo)!
Cabal recognizes a cabal.config file which specifies a list of constraints that must be met, and we can set that to use Stackage.


    cabal sandbox init
    curl http://www.stackage.org/stackage/7.8.3-latest/cabal.config > cabal.config
    cabal install --only-dep



## Freezing

There is a problem with our wonderful setup: what happens when our package is installed on another location?
If we are developing a library, we need to figure out how to make it work everywhere, so this is not as much of an issue.

Application builders on the other hand need to produce reliable, re-producible builds to guarantee correct application behavior.
Haskellers have attempted to do this in the .cabal file by pegging versions.
But .cabal file versioning is meant for library authors to specify maximum version ranges that a library author hopes will work with their package.
Pegging packages to specific versions in a .cabal file will eventually fail because there are dependencies of dependencies that are not listed in the .cabal file and thus not pegged.
The previous section's usage of a cabal.config has a similar issue since only packages from Stackage are pegged, but Hackage packages are not.

The solution to this is to freeze your dependencies:

    cabal freeze

This writes out a new cabal.config (overwriting any existing cabal.config).
Checking in this cabal.config file guarantees that everyone on your team will be able to reproduce the exact same build of Haskell dependencies.
That gets us into upgrade issues that will be discussed.

It is also worth noting that there is still a rare situation in which [freezing won't work properly because packages can be edited on Hackage](http://www.reddit.com/r/haskell/comments/2ma5gw/package_versioning_hackage_or_cabal_issue/).



## Installation workflow


Lets go over an installation workflow:

    cabal sandbox init
    curl http://www.stackage.org/stackage/7.8.3-latest/cabal.config > cabal.config
    cabal install --only-dep

An application developer will then want to freeze their dependencies.

    cabal freeze
    git add cabal.config
    git commit cabal.config



## Upgrading packages


cabal-install should provide us with a `cabal upgrade [PACKAGE-VERSION]` command.
That would perform an upgrade of the package to the version specified, but also perform a conservative upgrade of any transitive dependencies of that package.
Unfortunately, we have to do upgrades manually.

One option for upgrading is to just wipe out your cabal.config and do a fresh re-install.

    rm cabal.config
    rm -r .cabal-sandbox
    cabal sandbox init
    curl http://www.stackage.org/stackage/7.8.3-latest/cabal.config > cabal.config
    cabal update
    cabal install --only-dep
    cabal freeze

With this approach all your dependencies can change so you need to re-test your entire application.
So to make this more efficient you are probably going to want to think about upgrading more dependencies than what you originally had in mind to avoid doing this process again a week from now.

The other extreme is to become the solver. Manually tinker with the cabal.config until you figure out the upgrade plan that `cabal install --only-dep` will accept.
In between, you can attempt to leverage the fact that cabal already tries to perform conservative upgrades once you have packages installed.

    rm cabal.config
    curl http://www.stackage.org/stackage/7.8.3-latest/cabal.config > cabal.config
    cabal update
    cabal install --only-dep --force-reinstalls
    cabal freeze

You can make a first attempt without the `--force-reinstalls` flag, but the flag is likely to be necessary.

If you can no longer use Stackage because you need newer versions of the exclusive packages, then your workflow will be the same as above without the curl step.
But you will have a greater desire to manually tinker with the cabal.config file.
This process usually consists mostly of deleting constraints or changing them to be a lower bound.


## Conclusion

Upgrading packages is still a horrible experience.

However, for a fresh install, using Stackage, sandboxes, and freezing works amazingly well.
Of course, once you are unable to use Stackage because you need different exclusive versions you will encounter installation troubles.
But if you originally started based off of Stackage and try to perform conservative upgrades, you may still find your situation easier to navigate because you have already greatly
reduced the search space for cabal.
And if you are freezing versions and checking in the cabal.config, the great thing is that you can experiment with installing new dependencies but can always revert back to the last known working dependencies.

Using these techniques I am able to get cabal to reliably install complex dependency trees with very few issues and to get consistent application builds.

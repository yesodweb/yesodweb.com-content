Installing Haskell packages is still a pain. But I believe the community has some good enough workarounds that puts Haskell on par with a lot of other programming languages.
The problem is mostly that the tools and techniques are newer, do not always integrate easily, and are still lacking some automation.

My strategy for successful installation:

* Install through Stackage
* Use a sandbox when you start having complexities
* freeze (application) depenendencies

Simple definitions:

* Stackage is Stable Hackage: a curated list of packages that are guaranteed to work together
* A sandbox is a project-local package installation
* Freezing is specifying exact dependency versions.

I really hope that Stackage (and sandboxes to a certain extent) are temporary workarounds before we have an amazing installation system such as [backpack]().
But right now, I think this is the best general-purpose solution we have. There are other tools that you can use if you are not on Windows:

* hsenv (instead of sandboxes)
* nix (instead of Stackage and sandboxes)

hsenv has been a great tool that I have used in the past, but I personally don't think that sandboxing at the shell level with hsenv is the best choice architecturally.
I don't want to have a sandbox name on my command line to remind me that it is working correctly, I just want cabal to handle sandboxes automatically.


## Using Stackage

See the [Stackage documentation](). You just need to change the remote-repo setting in your ~/.cabal/config file.

Stackage is a curated list of packages that are guaranteed to work together.
Stackage solves dependency hell with exclusive and inclusive package snapshots, but it cannot be used on every project.

Stackage offers 2 package lists: exclusive, and inclusive.
Exclusive includes only packages vetted by Stackage.
Exclusive will always work, even for global installations.
This has the nice effect of speeding up installation and keeping your disk usage low, whereas if you default to using sandboxes and you are making minor fixes to libraries you can end up with huge disk usage.
However, but you may eventually need packages not on Stackage, at which point you will need to use the inclusive snapshot.
The inclusive snapshot risks conflicts between projects. At the point you are dealing with that, you need to start using sandboxes. 
The biggest problem with Stackage is that you may need a newer version of a package then what is on the exclusive list.
At this point you will need to stop using Stackage and start using a sandbox.

If you think a project has complex dependencies, which probbably includes most applications in a team work setting, you will probably want to start with a sandbox.



## Sandboxing

    cabal sandbox init

A sandbox is a project-local package installation.
It solves the problem of installation conflicts with other projects (either actively over-writing each-other or passively sabotaging install problems).
However, the biggest problem with sandboxes is that unlike Stackage exclusive, you still have no guarantee that cabal will be able to figure out how to install your dependencies.

sandboxes are completely orthogonal to Stackage. What we want to do is use Stackage inclusive with a sandbox.
This will give us the highest probability of a successful install.
Unfortunately, Stackage (remote-repo) integration does not work for a sandbox.

The good news is that there is a patch for Cabal in the works already.
Even better news is that [you can use stackage with a sandbox today](https://www.fpcomplete.com/blog/2014/10/new-stackage-features#using-stackage-without-changing-your-repo)!
The cabal.config file specifies a list of constratins that must be me://www.fpcomplete.com/blog/2014/10/new-stackage-features#using-stackage-without-changing-your-repo, and we can simply set that to use Stackage.


    cabal sandbox init
    curl http://www.stackage.org/stackage/7.8.3-latest/cabal.config > cabal.config
    cabal install --only-dep



## Freezing

There is a problem with our wonderful setup: what happens when our package is installed on another location?

If we are developing a library, we need to figure out how to make it work everywhere, so this is not as much of an issue.

Application builders on the other hand need to produce reliable, re-producible builds to guarantee application behavior.
Haskellers have attempted to do this with a Cabal file.
But .cabal file versioning is meant for library authors to specify maximum version ranges that a library author hopes will work with their package. Pegging packages to specific versions in a .cabal file will eventually fail because there are dependencies of dependencies that are not pegged. The previous section's usage of a cabal.config has the same issue, since only packages from Stackage are pegged, but Hackage packages are not.

The solution is freezing your dependencies:

    cabal freeze

Note that this will overwrite any existing cabal.config, that gets us into upgrade issues


## Installation workflow

Lets go over an installation workflow:

    cabal sandbox init
    curl http://www.stackage.org/stackage/7.8.3-latest/cabal.config > cabal.config
    cabal install --only-dep

An application developer will then want to freeze their dependencies.

    cabal freeze
    git add cabal.config
    git commit cabal.config


There are still some rare gotchas that you could run into even when using a fresh install

* It is possible that using Stackage for a cabal.config will not work if there is a manually patched package on Stackage.
* Because packages can be edited on Hackage, it is possible that [freezing will not always work](http://www.reddit.com/r/haskell/comments/2ma5gw/package_versioning_hackage_or_cabal_issue/).


## Upgrading packages

cabal-install should provide us with a `cabal upgrade [PACKAGE-VERSION]` command.
That would perform an upgrade of the package to the versino specified, but also performa a conservative upgrade of any transitive dependencies of that package.

One option here is to just wipe out your cabal.config and do a fresh re-install.

    rm cabal.config
    rm -r .cabal-sandbox
    cabal sandbox init
    curl http://www.stackage.org/stackage/7.8.3-latest/cabal.config > cabal.config
    cabal install --only-dep

With this approach you need to re-test your entire application.
So to make this more efficient you are probably going to want to think about upgrading more dependencies than what you originally had in mind to avoid doing this process a month from now.

The other extreme is to become the solver. Manually tinker with the cabal.config until you figure out the upgrade plan.
In between, you can attempt to leverage the fact that cabal already tries to perform conservative upgrades once you have packages installed.

    rm cabal.config
    curl http://www.stackage.org/stackage/7.8.3-latest/cabal.config > cabal.config
    cabal install --only-dep --force-reinstalls
    cabal freeze

You can try not using the --force-reinstalls flag, but it is likely to be necessary.


## Conclusion

Upgrading packages is still a horrible experience.

However, for a fresh install, using Stackage with sandboxes and freezing works amazingly well.
I am able to get cabal to reliably install complex dependency trees with few issues and get consistent application builds.

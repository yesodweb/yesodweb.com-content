After lots of discussion and testing, I'm happy to announce a significant
update to the Yesod scaffolded site. You can see some mailing list discussions
on this at:

* [Yesod 1.4 reminder](https://groups.google.com/d/msg/yesodweb/BhUq8wLrhgQ/0_rwAqvEGxEJ)
* [Proposed overhaul to the scaffolding](https://groups.google.com/d/msg/yesodweb/rpuXOKtUj_o/LrRNQJns6noJ)

The newest version of yesod-bin (and all versions going forward) will ship with
this newly minted scaffolding. There is no migration guide for existing sites;
scaffoldings aren't the kind of things to be easily migrated, and all existing
sites will continue to function due to lack of breaking API changes.
Nonetheless, if people are excited enough about these changes that they'd like
to integrate them into their existing sites, please start up a discussion on
the mailing list.

The primary motivation in this change is an overhaul to the settings system.
The new system is much more modular regarding environment variables, config
files, and hard-coded settings. It's also much simpler to determine where a
setting value comes from, and to configure things differently in different
environments. Since I've already described these changes in quite some detail,
I will instead point you to [the second mailing list thread linked
above](https://groups.google.com/d/msg/yesodweb/rpuXOKtUj_o/LrRNQJns6noJ).

To give an example of how this works, take a look at the following lines from
the new `settings.yml` file:

```
host:           "_env:HOST:*4" # any IPv4 host
port:           "_env:PORT:3000"
approot:        "_env:APPROOT:http://localhost:3000"
```

In the old scaffolding, both `PORT` and `APPROOT` were recognized environment
variable names, that would override whatever was defined in `settings.yml`.
However, that overriding occurred in a library (Yesod.Default.Config) and was
not obvious to users, which led to many bug reports/support questions.
Additionally, there was no way to override the host/interface binding via
environment variables. In the new system:

* It's obvious from reading the config file which settings can be overridden via environment variables, and what the names of those variables are.
* It's trivial to change the names, or disable environment variable overriding (e.g., replace the `approot` line with `approot: http://localhost:3000`).
* New arbitrary settings can be added at will, and easily use environment variables to override their values.

The other change is that we've replaced the ad-hoc `Import` prelude replacement
previously found in the scaffolding with ClassyPrelude.Yesod. For users not
interested in classy-prelude, it should be trivial to replace `import
ClassyPrelude.Yesod` with `import Prelude`, `import Yesod`, and whatever else
you want. Greg has also spent quite some time in the past week improving the
documentation for both classy-prelude and mono-traversable, which will help
things out considerably.

Besides that, I've done a bunch of minor maintenance on the scaffolding,
cleaning up import lists, adding some missing documentation, and so on.

Like any new release, it's certainly possible that there are some issues, so if
you notice any, please feel free to bring them up. Also, documentation fixes
are great as well. As always, please send any pull requests against [the
postgres branch of the yesod-scaffold
repo](https://github.com/yesodweb/yesod-scaffold/tree/postgres), from which we
automatically merge changes to all other versions of the scaffolding.

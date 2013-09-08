I'm happy to announce the release of yesod-platform 1.2.4. The Yesod Platform
is a collection of Yesod together with all of its upstream dependencies, pegged
at specific versions which are known to compile and work together. By using the
Yesod Platform, you can often times avoid dependency headaches. Installing is
usually a matter of running:

    cabal update && cabal install yesod-platform --force-reinstalls

This release is mostly just a collection of upstream version bumps, though
there are a number of incremental improvements to Yesod itself as well. I
haven't really kept up-to-date with release announcements since Yesod 1.2 came
out, so here's a short list of changes since that release:

* Improved JSON responses for yesod-auth and yesod-core (Tero Laitinen and Greg Weber).
* Protocol-independent links for CDN-hosted resources (Iku Iwasa).
* A nicer yesod devel refresh page (Chris Done).
* Ability to switch logging level dynamically via `shouldLogIO`.
* Improved email authentication password security.
* Better request body support in yesod-test (Konstantine Rybinov).
* bootstrap and normalize updates in scaffolding (Iku Iwasa).
* Update many crypto libraries to latest dependencies (Alexey Kotlyarov and others).
* No double-compression in gzip middleware (John Lenz).
* Many dependency updates and documentation improvements (Lubomír Sedlář, Fredrik Carlen and others).

Yesod itself has kept a stable API since 1.2, though there has been some
upstream API breakage. Most users should be able to upgrade to the newest
yesod-platform without incident. If you notice any problems, please bring them
up on the mailing list so that others know what to look out for.

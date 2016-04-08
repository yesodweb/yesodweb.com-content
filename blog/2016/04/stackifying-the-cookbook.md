Not too long ago, Sibi did a great job of cleaning up the Yesod cookbook and
[moving it to its own Github
repository](https://github.com/yesodweb/yesod-cookbook). The cookbook has been
a collaborative project from the community, and has accumulated lots of useful
examples over the years.

I'd now like to encourage others to help further improve it. As a recent Reddit
post brought to my attention, some of the cookbook examples do not compile with
recent versions of Yesod. I'd like to encourage the following:

* Look through the cookbook and update examples to compile with the latest version of Yesod
* Add Stack script interpreter lines to the beginning of each example

If you don't know about that second point, you can [read the docs from the
Stack
website](http://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter). But
you may find it easier to just [look at the commit I made
yesterday](https://github.com/yesodweb/yesod-cookbook/commit/09e67ee9299bbc0d7fb457b078e94fac09b678c3#diff-73178b26ac4da87e5f7cde8dea92dfe3R4),
or even just this part of it:

```
#!/usr/bin/env stack
{- stack
     --resolver lts-5.10
     --install-ghc
     runghc
     --package yesod
     --package yesod-static
     --package persistent-sqlite
 -}
```

With this addition, you can save the example into any `.hs` file and run it
with `stack foo.hs` (or `chmod +x foo.hs && ./foo.hs`).  This gives three
really important advantages over an unadorned example:

* It clearly documents which version of GHC and all dependencies are being used (via the `resolver` line)
* It states which packages need to be available. Note that I'm not providing an exhaustive list of packages, since depending on yesod will automatically include all of its dependencies (e.g., yesod-form, warp)
* For someone trying to get started quickly, it takes care of many details automatically (installing GHC, building packages). All they need to do is install Stack, and the `#!/usr/bin/env stack` is a good hint in that direction

I'd recommend communicating intentions of working on cookbook examples on the
mailing list, so that work isn't duplicated, and so others can review changes.
And if there are questions about how to make updates to some examples,
definitely ask!

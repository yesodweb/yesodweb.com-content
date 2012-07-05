There have been essentially three separate efforts to going on to get logging added throughout Yesod:

* Greg spearheaded the `Yesod.Logger` module and the `file-location` package, which allow you to add log messages that contain a log level and the source file location where the message was sent.
* Kazu wrote the `wai-logger` (and later `fast-logger`) package to allow for very efficient concurrent output to file handles. This was originally written for use in his [mighttpd2](http://hackage.haskell.org/package/mighttpd2) web server (based on Warp). Greg additionally wrote the request logger middleware, based to some degree on `wai-logger`.
* We had a fair amount of existing logging code in place already which didn't use either of the approaches above. For example, the `warpDebug` function basically had its own implementation of the request logger mentioned above.

When I started working on `keter` and was reviewing log output, I decided one of the major goals for the 1.1 release would be to rework the logging system top to bottom to fit the following goals:

* Use Kazu's efficient output code wherever possible.
* Simplify the user-facing aspect of the implementation.
* Keep the user-friendly output Greg provided for development.
* By default, have output *always* auto-flush so that Keter log files don't lose data.

I finally got a solid block of time to work on this, and I'm very happy with the results. You can look at some of the [discussions about this](https://github.com/yesodweb/yesod/issues/360); I just wanted to give a brief overview of the approach we've ended up with.

## Refactoring fast-logger

There are a few important aspects to Kazu's packages for our purposes:

* The efficient Handle code.
* A DateRef value that makes current time rendering cheap.
* A log formatter that uses Apache's log format.

The third piece there depends on `wai`, but the other two are completely general-purpose. Previously, the DateRef code was in wai-logger, which introduced an unnecessary dependency on wai. The first minor tweak I made was to move DateRef to fast-logger.

The second is more important. Greg introduced a type called `Logger` for encapsulating the concept of a `Handle` that can be written to efficiently. I moved this code to `fast-logger` as well, with one addition: `Logger` now encapsulates both a `Handle` and a `DateRef`. In other words, it has everything you need to start producing timestamped logs.

Finally, I set up a function called `mkLogger` to create a `Logger` from a `Handle`. To deal with auto-flushing, it takes an additional parameter to determine whether flushing should be performed after each write.

## RequestLogger

Now that the `Logger` type is provided by `fast-logger` itself, we can let `wai-extra`'s RequestLogger middleware depend on it directly. This definitely cleaned up the code quite a bit.

The next step was to create a settings type to handle the different possible configurations for request logger, basically: where to log to, which format to use (Apache or Greg's developer mode), and details like whether to use colors.

In the end, we now have a single `mkRequestLogger` function, the settings type, and two convenience pre-built middlewares: `logStdout` (which uses the Apache format) and `logStdoutDev` (which uses the colored, developer output).

## MonadLogger

Felipe came up with a very nice addition: we should create a general typeclass for monads which allow you to log messages. Then, the functions from Yesod like `logDebug` could be run from a number of different monads, not just a `Handler`.

The result is `MonadLogger`. This typeclass has a single function for logging messages, including the log level (warn, error, debug, etc) and the location in the source code where it occurred. The module provides instances for all of the common monad transformers (including ResourceT).

As for the common base monads (IO, ST, and Identity): they all have instances as well, but *they don't do anything*. The idea here is that you can have code that will log statements, but you can control the logging behavior using the base monad.

In the specific case of Yesod, the `Handler` and `Widget` monads have both been given instances of `MonadLogger` which actually print results, using methods you define in the `Yesod` typeclass. In other words: you still have full control of logging in these monads.

## Persistent

Finally, we get to add a feature we've wanted for a while: logging of Persistent SQL queries. The approach is simple: add a `MonadLogger` superclass to the SQL code, and inject a log statement whenever performing a database query or action. If the base monad is `IO`, then no logging will occur. But if you're running this from within yesod, the messages will be included with the rest of your logs. (The SQL statements all have a log level of "SQL", so if you want to keep SQL out of your logs, you'd just modify your `messageLogger` method to ignore those messages.)

If you're using Persistent from a non-Yesod context, and you want to have SQL statement logging, you'd need to create a separate base monad that knows how to log messages. Most likely I'll add such a monad transformer to `monad-logger` in the future. It should basically be a `newtype`d `ReaderT` holding onto the function to writing log messages.

## Check it out

I've sent [a pull request](https://github.com/kazu-yamamoto/logger/pull/3) with the changes for wai-logger, fast-logger, and adding the new monad-logger package. The beta branches of wai, persistent, and yesod are all up-to-date with these changes. If you have specific ideas for the logging system or would like to see other improvements, please bring them up!

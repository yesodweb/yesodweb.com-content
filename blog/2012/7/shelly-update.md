# What Shelly is not

* A better way to write untyped shell scripts

The only fundamental advantage that non-shell languages have over shell is platform consistency.
Shelly should work fine on Windows if you aren't using it to invoke unix commands.

Of course there are also fundamental downsides to non-shell languages. Distributing them is not easy.
Shelly is just a Haskell library: it cannot solve distribution issues.
I use Shelly for my work, personal, and Haskell community scripts and don't have any great difficulties distributing my scripts.


# The purpose of Shelly

* A convenient way for Haskell code (that may not be primarily concerned with scripting) to invoke other programs
* A way to bring more type-safety (and other Haskell features) into your shell scripts
* Faster scripts than some other non-shell alternatives

Shelly is not an attempt to re-invent shell scripting like PowerShell.
It is still executing strings and collects the returned string.
You can start building up command specific interfaces that increase type-safety, but Shelly doesn't add much help here, you are really just using Haskell.
Building up command specific interfaces takes additional effort, so you are probably going to do it for the minority of commands being executed.

The benefit of type safety comes largely in the code that processes the results of the commands you need to run or whenever you build up an abstraction in your script (which includes just defining a function).

Even trivial Ruby scripts can be noticeably slow to startup and parse options. Scala needs to wait for the JVM to startup. Haskell hits a sweet spot of good type system and fast startup execution.


# Whats new?

Since I originally published [an introductory article](http://www.linux-magazin.de/Online-Artikel/Shell-scripting-with-type-safety-using-Haskell/) I have been making a lot of refinements.


## A list-oriented interface

[Anton K](https://github.com/anton-k) contributed a [List-oriented interface](https://github.com/yesodweb/Shelly.hs/blob/master/examples/Pipe/Pictures.hs) which will automatically batch file operations.
He named it Shelly.Pipe, referrring to shell piping, but I am wondering if the name is overloaded by Haskell's streaming Pipe concept and if we should rename it to Shelly.List


## Good error messages

Shelly defines a monad, previously `ShIO`. On request from a user I also changed the `ShIO` monad name to just `Sh`. I switched from using `type` to `newtype`. I now have to do some extra type class deriving, but the error message is a much friendlier `Sh` rather than listing out the detail of the Monad transformer.

Shelly already has verbosity settings that can print out executed commands and their results.
But after starting to use shelly at work, I had some errors that were not that easy to track down, and I began to question my usage of Haskell.

I can't recommend that anyone without existing expertise in Haskell use it for impure code since it doesn't have stack traces.
When something goes wrong, stack traces are the minimum amount of information that should be at your disposal.
And yet I contribute to a web framework and a scripting library! The nice thing about a web application is that requests are generally isolated from eachother, run a small amount of code, and log requests, so tracking down failures becomes easier.
The fact that Haskell does not have stack traces is a testament to how type-safe it is, but also to the fact that it has not seen a great deal of real world usage.

At yap.TV where I work, we like FP and use Scala (it has stack traces!) to serve up APIs, but we don't want to pay the JVM startup cost for scripts. So this is where Haskell should be able to shine, but I had to do something about the error messages.

Shelly now gives you something different than stack traces: detailed logging.
In most cases this should be more useful than a stack trace.
Shelly keeps a log of API usage and writes it out to a file in a .shelly folder when an exception is thrown.
Previously I dumped it to the screen, which was easy, but sometimes made for just too much information. The current .shelly folder implementation probably still needs some more iterations.
This is all in addition to the `verbosely` settings that will print out commands and their output as the program is running.
Shelly's own error messages strive to give necessary detail and in some cases it will catch Haskell exceptions and re-throw them with better messages.

There are some downsides to the logging approach to errors.

* You could have an exception from code that isn't logging its usage. Note that shelly will still dump its log on failure. But if you do some other IO that can fail, you should use liftIO to bring it into `Sh` and use `trace` or `tag` to log what they are doing.
* The logging as implemented is essentially a memory leak. I don't use shelly in long-running processes. A long-running program should re-enter the shelly monad to clear the previous logged data. In the future shelly may alwasy write to the log file and automatically delete it when there are no errors.


## A variadic command runner

I didn't know writing variadic functions was so easy in Haskell -- that is if you return the same type every time.
I had a lot of issues teaching the Haskell type system how to return either `Sh` a or `Sh` ().

    shelly $
      listing <- run "ls" ["-a", toTextIgnore "foo"]
      listing <- cmd "ls" "-a" "foo"

If all this did was remove brackets and commas, I would not have bothered. The real point is that shelly can now take as an argument any value that can be turned into a FilePath. Thanks to John Milikin for suggesting this approach. In practice it means you can now use a FilePath as an argument without extra boilerplate conversion. Shelly's filepath combinators `</>` and `<.>` work the opposite way, automatically converting a Text to FilePath. This means you can write large shelly scripts that very rarely need to convert between Text and FilePath. I now execute almost all shell commands with `cmd`. However, cmd loses compositional capabilities, so when I build up abstractions over commands I use `run`.

`cmd` also creates a greater need for using Haskell's little known (and documented), but very useful defaulting feature.
This is at the top of my scripts now:

    {-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
    {-# OPTIONS_GHC -fno-warn-type-defaults #-}
    import Shelly
    import Data.Text.Lazy (Text)
    default (Int, Text)


## Relative paths

A normal Haskell program that uses relative paths or changes the working directory is not thread-safe because another thread could change the working directory, which is essentially a mutable global variable. Shelly fixes this by keeping track of the shelly working directory in its state, which is always accessed in a thread-safe manner. `cd` just changes Shelly's state. The first thing all Shelly functions do with a relative FilePath is make it absolute using the Shelly working directory. This is normally transparent to the user, except that `ls` and `find` would always give back absolute paths. I added a `relativeTo` function, which `ls` will now use to keep directory listing relative (if it is given a relative path). `find` calls `ls` and also benefits.


## Find functions

I generalized the find function into several 'find*' functions that one can use to fold over instead of accumulating a list and also filter out directories or files. Directory filtering can make finds much more efficient. Folding also has potential to increase efficiency, but I have not done any performance analysis of the file finding code.


## Backgrounding and shelly-extra

There are two use cases for backgrounding.

* launching daemons
* concurrency/futures

My standard reply to launching daemons is: do not use shelly (or any shell script) for this, other than to launch a daemon launcher.
If you use Shelly to launch a daemon with an ad-hoc approach you are probably going to end up with some ad-hoc issues.
I use shelly to launch monit, and I plan on trying out angel, which is written in Haskell.
Your OS probably has its own daemon launching tools also.

Shelly now has a futures implementation in a separate shelly-extra package (thanks to a github contributor).
The futures require SafeSemaphore, and I want to keep shelly's dependencies to a minimum, so I plan on moving functionality that needs extra dependencies to shelly-extra package.

I like the end result of the futures implementation because:

* It doesn't require modifications to the shelly package.
* It guarantees your program will wait for all concurrent jobs to finish

Generally shell scripts contain a lot of quick commands, but when you have the occasional commands that are noticeably long and independent of other commands, you can easily run them concurrently.



## escaping False

By default, shelly will escape shell characters. You can set this off or on with `escaping`.



## SSH

Shelly now has a crude way to run commands over SSH that should be improved in the future.

    sshPairs_ "server" [
        ("cd", ["adir"]),
        ("ls", ["*"])
      ]

It takes the list of command pairs and combines them with '&&' and runs them over ssh unescaped.
Haskell does not have a client side SSH library, but I am beginning to wonder if it is truly a pre-requisite for simple SSH needs, or if we can get by on the existing system `ssh` command.


## The future

* Improve SSH interaction
* Fix how the PATH env variable is handled
* Move from lazy Text to strict Text

I have been making a lot of bug fixes and small API changes and bumping the package versions very quickly. I am getting pretty happy with the API now though, and nearing a stable API 1.0 release.

It is great to [hear](http://www.reddit.com/r/haskell/comments/w86gu/my_current_job_task_is_boring_so_i_wrote_a_simple/) from those [successfully automating tasks](http://www.scholarslab.org/dh-developer/shell-programming-in-haskell-converting-s5-slides-to-pdf/) with Shelly. Its fun to try to discover new concepts with Haskell, but you can also just enjoy using it to automate some boring tasks.

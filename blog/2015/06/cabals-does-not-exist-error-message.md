I've seen many people confused by this error message, and just ran into it
myself, so decided a blog post to explain was in order.

As I was hacking on [stack](https://github.com/commercialhaskell/stack) this
morning, I pushed and got [a build failure from
Travis](https://travis-ci.org/commercialhaskell/stack/jobs/66331799#L671) with
the following content at the end:

```
Failed to install asn1-encoding-0.9.0
Last 10 lines of the build log ( /home/travis/.cabal/logs/asn1-encoding-0.9.0.log ):
cabal: /home/travis/.cabal/logs/asn1-encoding-0.9.0.log: does not exist
```

What the error message means is: I tried to load up the log file containing the
output of the build, but the file does not exist. It's possible that there are
multiple reasons for this, but I know of only one: when cabal is unable to
download the necessary package from Hackage, usually because Hackage is having
a temporary blip.

As a user: if you see this message, just try running your command again. If
it's a temporary Hackage outage, the second attempt may succeed. Another option
(which I'd strongly recommend) is to switch to a more reliable package index;
see the blog post on [FP Complete's S3
mirror](https://www.fpcomplete.com/blog/2015/03/hackage-mirror) for more
details. I've been pushing for a while to make this the default remote-repo
used by cabal-install, but unfortunately that hasn't happened. (For the record:
stack does use the S3 mirror by default.)

For cabal: I see three steps worth taking:

1. Fix this confusing error message to say something like "Download of http://... failed". I thought there was a cabal issue opened about this already, which is why I didn't open up a new one right now. If there *isn't* one, it's worth opening.
2. Automatically retry a failing download. I'm not convinced this is a good thing to do, but it's a possible mitigation. (We don't do that right now in stack, though I've been toying with the idea. Feedback appreciated.)
3. Switch to a more reliable mirror for the default remote-repo.

Fixing this instability at the Hackage level would of course also be a good
move.

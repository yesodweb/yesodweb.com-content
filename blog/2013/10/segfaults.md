This is just a word of warning for Yesod users. There appears to be a serious
bug in GHC 7.4.2 which can be triggered when using Persistent. We've
experienced this quite a few times at FP Complete, but have never been able to
get a minimal reproducing test case. Recently [an issue was filed with the same
behavior](https://github.com/snoyberg/keter/issues/28), and John Wiegley
recommended I write a blog post to warn users.

The problem comes up when using the `get404` function. It seems that under some
circumstances, this function will return an invalid value if the given key does
not exist. If this value is then used, it results in a segfault of the process.
If this situation arises, you can work around the problem by using `get` and
`case` statements. Please see [my comments on the Github
issue](https://github.com/snoyberg/keter/issues/28#issuecomment-25543946) for
more details.

This bug does not seem to exist on the GHC 7.6 series, so upgrading GHC
versions may be the simplest way to prevent this problem.

If anyone has minimal examples of this occurring, please email me, I'd love to
include a test case for this in either the Persistent or Yesod test suite to
ensure we're protected against such a regression in GHC.

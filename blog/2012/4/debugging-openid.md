I got an email today reporting a bug on Haskellers.com. Someone's OpenID (to
protect the innocent, we'll call it `https://example.com`) was not working.
Here's the story of debugging it.

I started off (of course) by trying to reproduce it. I went to Haskellers and
tried logging in with the OpenID, and sure enough got this cryptic message:

    data: end of file

This immediately caught my attention, because I didn't remember ever creating
such an error message. So I thought about the library stack at play here:

1. haskellers.com
2. yesod-auth
3. authenticate
4. http-conduit
5. tls

I figured that the error message must have come from `tls`, because it's the
only library in that stack that I didn't write. So I put together a quick test
case to use `http-conduit` to connect to the server. And it worked perfectly. So
the bug seemed to not be from either `http-conduit` or `tls`.

OK, let's start over from the top of the stack. I took my `yesod-auth` test
program and tried to connect to log in to example.com. Same error message. So
it's not caused by Haskellers. I tried using my authenticate test program.
Again, same error message. But now I know it must be coming from somewhere in
authenticate, right?

Now's the time to use the golden hammer of debugging: print statements. Using
this powerful and sophisticated technique, I traced the problem to
[getForwardUrl](https://github.com/yesodweb/authenticate/blob/master/authenticate/Web/Authenticate/OpenId.hs#L34),
then to
[discover](https://github.com/yesodweb/authenticate/blob/master/authenticate/OpenId2/Discovery.hs#L51),
then to
[discoverYADIS](https://github.com/yesodweb/authenticate/blob/master/authenticate/OpenId2/Discovery.hs#L66).

At this point, there were two things bothering me:

* I still had no idea where that error message was coming from.
* discoverYADIS was being called twice. It succeeded the first time, and failed the second.

It's not surprising that the discoverYADIS function is called twice, it's the very
nature of OpenID. Usually, we connect to the OpenID specified by the user, only
to find an HTML tag or HTTP header telling us to look elsewhere for the rest of
the login information. But why was it failing the second time around?

I got the two URLs that were being requested, and went back to my http-conduit test program. I ran something along the lines of:

    simpleHttp "https://example.com/"
    simpleHttp "https://example.com/?xrds"

No problem at all. So both URLs seemed to work. Then I got an idea: maybe it's
a problem caused by connection sharing. So I modified the test program:

    withManager $ \m -> do
        req <- parseurl "https://example.com/"
        httpLbs req m
        httpLbs req m

Boom! I got the exact same `data: end of file` error message. Hurrah! Now
debugging could focus on just `http-conduit` and `tls`. Somehow, the request
was failing when we were reusing a connection.

But I [already had code for that case](https://github.com/snoyberg/http-conduit/blob/c1da8cfb65bb69e802d1e17b7872902c2bd115f2/Network/HTTP/Conduit.hs#L198). tl;dr: If any exceptions occur when sending the request, and we're reusing an old connection, then start over with a fresh connection.

But what we *weren't* handling was the case when the exception was thrown when
reading the response headers. Our example.com server was behaving as follows:

* Accept the first request
* Send the first response
* Accept the second request
* Close the connection

This doesn't seem very logical to me, and perhaps there was something else involved that forced the server to close the connection when it did. Either way, the fix is simple: [catch exceptions when reading the response headers](https://github.com/snoyberg/http-conduit/commit/77e8c6438af599a4f819172204ece5a0c4d6a16e).

So there you have it: start to finish debugging of an OpenID bug.

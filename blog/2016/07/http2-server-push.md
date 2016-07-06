## What is HTTP/2 server push?

Server push is a hot topic of [HTTP/2](https://tools.ietf.org/html/rfc7540).
If we used server push effectively,
user experience cloud be improved.
For instance, let's think a scenario to
download "index.html" which requires "style.css". 

If a browser uses HTTP/1.1, it gets the "index.html" first,
parses it,
then obtains "style.css".
So, two round-trip-times (RTTs) are necessary
before the browser starts rendering.

On the other hand, if a browser uses HTTP/2 and
the corresponding server supports server push,
only one RTT is necessary.
That is, when the browser send a GET request for "index.html",
the server pushes "style.css" before
it transfers "index.html".
When the browser parses "index.html",
it finds "style.css" in its local cache.
Thus, it can start rendering at this timing.

## APIs for server push

So, how can we implement HTTP/2 server push in [Warp](http://www.aosabook.org/en/posa/warp.html)?
Clearly, it is necessary for an application to tell Warp
what files should be pushed.
One way is to extend the `Response` type to store information on server push.
But this breaks compatibility.
We need to modify all existing applications.

To keep all existing types as is,
I decided to use [vault](http://www.yesodweb.com/blog/2015/10/using-wais-vault) in the `Request` type.
Vault is heterogeneous infinite map which can store any kinds of types.
Now, Warp creates a new `IORef` and store its getter and setter to the vault:

```haskell
getHTTP2Data :: Request -> IO (Maybe HTTP2Data)
setHTTP2Data :: Request -> Maybe HTTP2Data -> IO ()
```

`HTTP2Data` is the data type to store a list of files to be pushed.
An application can set files to be pushed by `setHTTP2Data`.
Warp retrieves them by `getHTTP2Data` and
pushes them before sending the corresponding `Response`.

Note that the vault is a part of Web Application Interface ([WAI](http://hackage.haskell.org/package/wai)) but
these two functions are not.
They are APIs provided only by Warp.

## Middleware for server push

The next question is how an application know files to be pushed for a given URL.
One way is [manifest files](https://github.com/GoogleChrome/http2-push-manifest/).

Another way is learning based on `Referer:`.
Typically, there is the `Referer:` whose value
is, say, https://example.com/index.html", 
in a GET request to "style.css".
So, analyzing requests for a while,
we can learn that "style.css" should be pushed
when "index.html" is requested.

@davean suggested me to implement this mechanism as a middleware.
So, I created a middleware for HTTP/2 server push based on `Referer:`.
It's default behavior for a given `Request` and `Response` is as follows:

- If files to be pushed is found for a given path in a learning dictionary, set them by `setHTTP2Data`.
- Otherwise, register the file of `Response` to the learning dictionary only when the following conditions are met:
1. The `Request` stores a valid `Referer:`
2. The `Response` is a file type
3. The path of `Referer:` is "/" or ends with ".html"/".html"
4. The path ends with ".js" and ".css"

The learning dictionary is cleared every 30 seconds.

Warp v3.2.7 with the push APIs and wai-http2-extra v0.0.0 with the middleware are already
on Hackage.
If you implement a server push middleware based on manifest files,
please send a pull request on [github](https://github.com/yesodweb/wai).

## Visualization

Here are screen shots of Firefox accessing to new Warp.
Figure 1 is the first access and the middleware has not learned anything.
So, no pushes are used.
Figure 2 is the second access. 
You can see .js and .css files are pushed.


![Figure 1: Download time-line without server push](/assets/server-push/nopush.png)

![Figure 2: Download time-line with server push](/assets/server-push/push.png)

## Next Step

The next step would be implementation of
[Cache Digests for HTTP/2](https://tools.ietf.org/html/draft-kazuho-h2-cache-digest).
In this scheme, a browser can tell a list of cached file to a server.
So, the server can avoid unnecessary pushes.

## Acknowledgment

Efforts to bring HTTP/2 server push feature to Warp was originally made by Andrew Pritchard.
Without his experience and suggestions, 
this work would be impossible.
I thank him deeply.

The ideas for this blog post were thought up on Sunday, which was the holiday of [Purim](http://en.wikipedia.org/wiki/Purim). One of the central themes of the holiday is "it was reversed." I think that's somewhat relevant to what I'm writing. (And depending on what you think of this post, the fact that large quantities of alcohol are also consumed on this holiday might play in too.)

I'm guessing like many others, I've seen quite a bit of [talk about comonads recently](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html), but did not have a good grasp of what they were. This post is *not* a comonad tutorial, but simply some interesting things I found along my way.

Note: Full source code for this blog post is [available in a Github
gist](https://gist.github.com/snoyberg/5023146).

## Processing HTTP responses

As usual, I like to approach things from a very real-world perspective. So
let's consider a fairly reasonable problem. We're going to make an HTTP request
to a certain URL. The response will have three pieces of information we're
interested in:

1. A `content-type` header which will tell us the character encoding of the response.
2. An `x-language` header telling us the language the text should be translated to.
3. A response body with some English-language text, in the character encoding specified by content-type.

We'll use http-conduit for this. Getting the initial response from the server is simple:

    withManager $ \man -> do
        req <- parseUrl userSuppliedURL
        res <- httpLbs req man

The type of `res` is `Response LByteString`. The `Response` datatype has a
number of fields, such as status code and response headers, in addition to the
response body itself. It looks something like:

    data Response a = Response
        { responseStatus :: Status
        , responseHeaders :: [(ByteString, ByteString)]
        , responseBody :: a
        }

It's actually got a few more fields, and is presented as an abstract datatype,
but that's not terribly important for our purpsoes.

Coming back to our `Response LByteString`. We now need to perform two
transformations on this: decoding the binary data to textual data using
content-type, and translating English to some other language based on
x-language. Let's provide some simple functions to implement this:

    decode :: Response LByteString -> LText
    decode res =
        f $ responseBody res
      where
        f =
            case lookup "content-type" $ responseHeaders res of
                Just ct
                    | "utf-8" `isSuffixOf` ct -> TLE.decodeUtf8
                    | "utf-16" `isSuffixOf` ct -> TLE.decodeUtf16BE
                _ -> TLE.decodeUtf8

    translate :: Response LText -> Text
    translate res =
        case (responseBody res, lang) of
            ("Hello", "es") -> "Hola"
            ("Hello", "he") -> "שלום"
            _ -> "Unknown"
      where
        lang = fromMaybe "he" $ lookup "x-language" $ responseHeaders res

This isn't exactly the most complete implementation imagineable, but it will
suffice for the moment. Each function takes a full `Response` and returns a
newly modified response body. The question is: how do we compose these two
functions?

One possibility would be to rewrite the functions to have the type signatures:

    decode :: Response LByteString -> Response LText
    translate :: Response LText -> Response Text

However, this is unsatisfactory for (at least) two reasons:

1. Our original type signatures make it clear at the type level that the only piece of the response being modified is the body. The new signatures remove this safety.
2. It will slightly complicate our implementations.

So keeping our functions as they stand now, let's try to compose them to convert our `Response LByteString` into a `Response Text`.

## The Comonad approach

One approach is to manually take our resulting response bodies and replace them in the original `Response` value, e.g.:

    print $ res
        { responseBody = translate $ res { responseBody = decode res }
        }

This works, but it's not particularly pretty or scalable. However, this pattern
seems pretty repeatable, and can be pulled out into a helper function:

    extend f res = res { responseBody = f res }

Using this helper function, our code becomes a much more manageable:

    print $ (extend translate . extend decode) res

The motivation here is pretty simple. Our `Response` has some fixed, unchanging
contextual information (the status code and response headers), and some data we
want to play with (the response body). We want to write functions that use
*both* pieces of information as input to produce the new response body output.
`extend` then modifies such a function to *keep* that contextual information so
that the next function we want to run has it available.

And that right there is the core of comonads. It may not seem very revolutionary, and at this point I haven't given much generalized intuition for what's going on. We'll get to that later. Until then, let's give a full-blown `Comonad` instance for `Response`:

```
instance Comonad Response where
    extract = responseBody
    duplicate res = res { responseBody = res }
    extend f res = res { responseBody = f res }
```

Note that we only need to provide either `duplicate` or `extend`. I've provided
both for illustrative purposes only.

## The Reader approach

Alright, let's forget Comonads entirely and restate the problem a bit. We said
that there are two pieces of input being provided to our functions: some
context which does not change at all, and then some value which gets updated
through our calculation. So why not separate these into two different
parameters?

Getting the response body out of a `Response` is easy: just use the `responseBody` function to extract it. But how do we create a contextual value that contains the status code and headers without including the body? One approach would be to use a tuple, or create a new datatype. However, we can do something much simpler: replace the `responseBody` field in our `Response` value with unit `()`.

This new `Response ()` value can then be passed to all functions without being modified. Then we need to create wrappers around `decode` and `translate` which take the context and body as separate parameters, combine them back together, and pass them to `decode` and `translate`, respectively. This is actually pretty trivial:

```
decode2 :: Response () -> LByteString -> LText
decode2 res body = decode res { responseBody = body }

translate2 :: Response () -> LText -> Text
translate2 res body = translate res { responseBody = body }
```

Once we curry these two functions, their types become `LByteString -> LText` and `LText -> Text`, which can be composed with simple function composition. To use these to get the new response body, we can use:

```
let res' = res { responseBody = () }
print $ (translate2 res' . decode2 res') (responseBody res)
```

We could make this process more pleasant in two ways for larger applications:
provide a helper function which automates the creation of the wrapper
functions, and use a Reader monad to pass the context around. However, we're
not going to do pursue that course now.

## Comonads and Monads, oh my!



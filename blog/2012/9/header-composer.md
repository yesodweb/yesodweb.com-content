This is 4th article for the series of "Improving the performance of Warp".
Readers are supposed to read the following articles:

1. [Improving the performance of Warp](improving-warp)
2. [Sending header and body at once](header-body)
3. [Caching file descriptors](caching-fd)

Note that new versions of Warp have already been released,
which contain the cache mechanism of file descriptors
described in the third article.

## The old composer for HTTP response header

Andreas pointed out that the code to compose HTTP response header
may be another performance bottleneck.
So, I tackled this problem.
Let me explain how the old composer for HTTP response header
worked. Recall that the `Response` data type has
three constructors:

    ResponseBuilder Status ResponseHeaders Builder
    ResponseSource Status ResponseHeaders (Source (ResourceT IO) (Flush Builder))
    ResponseFile Status ResponseHeaders FilePath (Maybe FilePart)

As you can see, each constructor includes both `Status` and `ResponseHeaders`, which are defined in the `http-types` package. The former is defined as follows:

    data Status = Status {
        statusCode :: Int
      , statusMessage :: ByteString
      }

The following are the definitions of the latter:

    type ResponseHeaders = [Header]
    type Header = (HeaderName, ByteString)
    type HeaderName = CI ByteString

That is, `ResponseHeaders` is a list of pairs of case-insensitive `ByteString` (for field keys) and `ByteString` (for field values).

The old composer for HTTP response header creates a `Builder` of the blaze-builder package by appending the `Bytestring`s in the `Status` and the `ResponseHeaders`. Each append operation runs in O(1).
To simpify, I will only talk about the `ResponseFile` case on Linux. The `Builder` is converted to a list of *packed* `ByteString`s and sent with the `writev()` system call. And then a file (HTTP response body) is sent with the `sendfile()` systam call.

As I described in [Sending header and body at once](header-body), this code was changed. The list of *packed* `ByteString`s is stored to the socket buffer by using `send()` with the `MSG_MORE` flag and then a file is sent with `sendfile()`. At this point, I received Andreas's comments.

## The new composer for HTTP response header

In many cases, the performance of the blaze builder is sufficient.
But I suspected that it is not fast enough for
high performance servers.
So, I tried two methods.

The first method: create a list of `ByteString`s without packing
and send the list with `writev()`. Since the header and its body
should be squeezed into a single TCP package (if possible),
I set the `TCP_CORK` socket option before calling `writev()`
and reset it after calling `sendfile()`.
But this method is no better than the old composer.
I guess that copying many small chunks from the user space to the kernel
space does not work efficiently.

The second method: calculate the necessary length of the HTTP header
from `Response` and allocate a buffer for a new `ByteString`
according to the length.
Then copy `Status` and `ResponseHeaders` with low level
functions. Here is the most fundamental copy function:

    copy :: Ptr Word8 -> ByteString -> IO (Ptr Word8)
    copy !ptr !(PS fp o l) = withForeignPtr fp $ \p -> do
        memcpy ptr (p `plusPtr` o) (fromIntegral l)
        return $! ptr `plusPtr` l

This copies a sequence of `Word8`s from the `ByteString` in the second argument
to the `Ptr Word8` (extracted from `ByteString`) in the first argument.
This new *packed* `ByteString` is sent by using `send()` with the `MSG_MORE` flag and a file is sent with `sendfile()`. This improved the performance. So, I adopted this method.

Readers may notice that calculating length of an HTTP header is redundant.
If we design better WAI having APIs to manipulate header fields,
we can omit the calculation.
I will discuss the possible design change for WAI in a future article.

## Division

The current code to copy `Status` is as follows:

    copyStatus :: Ptr Word8 -> H.HttpVersion -> H.Status -> IO (Ptr Word8)
    copyStatus !ptr !httpversion !status = do
        ptr1 <- copy ptr httpVer
        writeWord8OffPtr ptr1 0 (zero + fromIntegral r2)
        writeWord8OffPtr ptr1 1 (zero + fromIntegral r1)
        writeWord8OffPtr ptr1 2 (zero + fromIntegral r0)
        writeWord8OffPtr ptr1 3 spc
        ptr2 <- copy (ptr1 `plusPtr` 4) (H.statusMessage status)
        copyCRLF ptr2
      where
        httpVer
          | httpversion == H.HttpVersion 1 1 = httpVer11
          | otherwise = httpVer10
        (q0,r0) = H.statusCode status `divMod` 10
        (q1,r1) = q0 `divMod` 10
        r2 = q1 `mod` 10

Since `statusCode` is `Int`, we need to convert it to three `Word8`s.
As you can see, the convention involves
two `divMod` operations and one `mod` operation.
Unfortunately, due to bugs of GHC, such division operations are slow.
Unnecessary guard is reported in [ticket 5161](http://hackage.haskell.org/trac/ghc/ticket/5161). This has been fixed in GHC 7.4.1.
[Ticket 5598](http://hackage.haskell.org/trac/ghc/ticket/5598) says
that `divMod` is complied
into unnecessary two divisions.
This has been fixed in GHC 7.6.1.

Even if you are using GHC 7.6.1 or later, this kind of conversion is slow and
unnecessary. We can prepare `ByteString`s corresponding to values of `Int` beforehand.
There are two ways to implement this.

Method A: prepare an array of `ByteString` whose index is status code.
This does not change the definition of `Status`
but it appeared slower than Method B.

Method B: change the definition of `Status` to have one more
`ByteString` (e.g. Status 200 "200" "OK"). This is faster than method A
but breaks backword compatibilities.
To my experience,
http-conduit, wai-app-file-cgi and (of course) warp have to be fixed.
Aristid Breitkreuz, the author of http-types, agreed
to adopt method B but has not yet released a new version.

That's all about what I have done to the composer for HTTP response header.

This article explores the design space of [HTTP/2](https://tools.ietf.org/html/rfc7540) *priority*.

- For Haskellers: HTTP/2 uses a single TCP connection per site and multiplex contents in it. If a large image file occupies the connection, CSS files or JavaScript files are not downloaded quickly, resulting that the browser cannot start rendering. To prevent this undesired behavior, HTTP/2 provide priority. So, browsers can specify that  CSS files or JavaScript files get preference over image files.
- For non Haskellers: Haskell is a standardized programming language and its flagship implementation is GHC (Glasgow Haskell Compiler). I don't distinguish Haskell and GHC in this document. Haskell provides lightweight threads in addition to OS (native) threads. In other words, Haskell implements the N:M model - N lightweight threads can run on M cores. The key for this highly concurrent system is *immutable* data which is essentially thread safe. Combining STM (Software Transactional Memory), immutable data can be treated as *mutable* data but they are dead-lock free.

## Weight

Each HTTP/2 stream has its own HTTP/2 priority whose priroity value consist of

- flag
- stream dependency
- weight (from 1 to 256).

Using stream dependencies, HTTP/2 priority can be *nested* as trees. But this article concentrate on *flat* one because the nested one can be implemented on the flat one. To learn HTTP/2 priority deeply, please read ["Understanding HTTP/2 prioritization"](https://speakerdeck.com/summerwind/2-prioritization) written by Moto Ishizawa.

HTTP/2 streams should use *resource* according to their *weight*. Larger, more preferred. The first question would be what is resource? For [h2o](https://github.com/h2o/h2o/releases) written by Kazuho Oku and [Warp](https://github.com/yesodweb/wai/tree/master/warp) whose http/2 part is written me, resource is numbers of sending times. Both implementations prepare one 16KiB sending buffer per HTTP/2 connection. Roughly speaking, they divide contents into 16KiB frames and interleave the frames. [nghttp2](https://nghttp2.org/), written by Tatsuhiro Tsujikawa, also reflects the byte size of sent frames.

Suppose a browser specify weight 10 for A.css, weight 5 for B.js, and weight 1 for C.jpg. A server divide them into 16 KiB fragments and should send in the following order:

- A.css, A.css, B.js, A.css, A.css, B.js, A.css, A.css, B.js, A.css, A.css, B.js, A.css, A.css, B.js, C.jpg,...

So, the second question is how to implement this? If you use weight as *priority* of *max heap* and repeat dequeueing and enqueueing with weight reduced one, the result sequence would be:

- A.css(10), A.css(9), A.css(8), A.css(7), A.css(6), A.css(5), B.js(5), A.css(4), B.js(4), A.css(3), B.js(3), A.css(2), B.js(2), A.css(1), B.js(1), C.jpg(1),...

This is not what we want. In other words, this is not *fair*.

## Random skew heap

## Weighted fair queueing

## O(1) algorithm

## Summary

## Benchmark

## Conclusion

## History

First h2o implemented Array of Queue IO (external) then
nghttp2 implemented Binary Heap IO (external).
Subsequently Warp implemented Random Skew Heap but
switched to Priority Search Queue (interanl) after this study.

## Acknowledgment

I would like to thank Moto Ishizawa for analyzing HTTP/2 priority implementations and thank Kazuho Oku and Tatsuhiro Tsujikawa for discussing this topic deeply.

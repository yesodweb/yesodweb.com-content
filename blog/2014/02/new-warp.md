# Improving the performance of Warp again

As you may remember, I improved the performance of Warp in 2012 and wrote [some blog articles](http://www.yesodweb.com/blog/2012/09/improving-warp) on this web site. Based on these articles, Michael and I wrote an article: ["Warp"](http://aosabook.org/en/posa/warp.html) for POSA, The Performance of Open Source Applications.

In the last year after working with Andreas, I hit upon some ideas to make Warp faster and implemented them. In this article, I will explain how I improved the performance of Warp again. If you have not read the POSA article, I recommend to give a look at it before reading this article.

Here are items to be explained:

- Better scheduling
- Buffer allocation to receive HTTP requests
- Buffer allocation to send HTTP response
- HTTP request parser

## Better scheduling

## Buffer allocation to receive HTTP requests

## Buffer allocation to send HTTP response


## HTTP request parser

    sendfileloop                    Network.Sendfile.Linux                    7.8    0.0
    sendloop                        Network.Sendfile.Linux                    3.8    0.0
    serveConnection.recvSendLoop    Network.Wai.Handler.Warp.Run              3.1    2.0
    >>=                             Data.Conduit.Internal                     2.6    4.0
    parseRequestLine                Network.Wai.Handler.Warp.RequestHeader    2.4    1.6


    sendfileloop                  Network.Sendfile.Linux                    8.3    0.0
    sendResponse                  Network.Wai.Handler.Warp.Response         3.7    3.1
    sendloop                      Network.Sendfile.Linux                    3.2    0.0
    >>=                           Data.Conduit.Internal                     2.9    4.0
    serveConnection.recvSendLoop  Network.Wai.Handler.Warp.Run              2.6    2.0

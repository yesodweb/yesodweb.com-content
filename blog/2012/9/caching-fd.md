This is the third article for the series of "[Improving the performance of Warp](improving-warp)".
As I wrote in "[Sending header and body at once](header-body)", I will explain how to avoid the open()/close() system
calls in this article.

## simple-sendfile basis

I designed the simple-sendfile to use few system calls as possible
since "system calls are evil for network programming in Haskell".
To make a story simple, I will only talk about the sendfile system call
of Linux.

The type of the `sendfile` function in `Network.Sendfile` is as follows:

    sendfile :: Socket -> FilePath -> FileRange -> IO () -> IO ()

`FileRange` has two constructors: `EntireFile` and `PartOfFile`.

Let's consider the case where we send the entire of a file
by specifying `EntireFile` to the `sendfile` function.
Unfortunately, the function has to call the stat() system call
to know the size of the file because the sendfile() system call on Linux
requires to specify how many bytes to be sent
(sendfile() on BSD has magic number '0' which indicates
the end of file).

If WAI applications know file size, they can specify
PartOfFile to avoid the stat() system call.
It is easy for WAI applications to cache file information
such as size and modification time.
If cache timeout is fast enough (say 10 seconds),
the risk of cache inconsistency problem is not serious.
And because we can safely clean up the cache,
we don't have to worry about leakage.

## open() and close()

If `PartOfFile` is specified,
the `sendfile` function calls open(), sendfile() repeatedly if necessary, and close().
When I implemented the simple-sendfile package,
I noticed that open() and close() should also be eliminated.
For this, we should cache file descriptors.

Caching file descriptors should work as follows:
If a client requests to send a file, a file descriptor
is opened. And if another client requests the same file shortly,
the file descriptor is reused.
At a later time, the file descriptor is closed
if no Haskell thread uses it.

Sounds easy? Unfortunately I had no idea on how to safely cache file descriptors.
It seems to me that it is difficult to ensure that
no Haskell thread uses a file descriptor when closing it.

Typical tactics for this case is reference counter.
But I was not sure that I could implement robust mechanism
for reference counter. What happens if a Haskell thread is
killed for unexpected reasons?
If we fails to decrement its reference counter,
the file descriptor leaks.

Andreas motivated me to consider this issue again
by pointing out that the performance bottleneck of Warp is
open()/close(). I have thought this over one month and
all necessary pieces got together suddenly.

## The timeout manager of Warp

I implemented a cache mechanism for file descriptor based on
Warp's timeout.
So, let me explain Warp's timeout first.
For security reasons, Warp kills a Haskell thread,
which communicates with a client,
if the client keeps quiet for a while (30 seconds by default).
I think that the hearts of Warp's timeout are two:

- Double `IORef`s
- Safe swap and merge algorithm

Suppose that status of connections is described as `Active` and `Inactive`.
To clean up inactive connections,
a dedicated Haskell thread, say timeout manager, repeatedly looks into status of each connection.
If status is `Active`, the timeout manager turns it to `Inactive`.
If `Inactive`, the timeout manager kills its associated Haskell thread.

Each status is refereed by an `IORef`.
To update status through this `IORef`,
atomicity is not necessary because status is just overwritten.
In addition to the timeout manager,
each Haskell thread repeatedly turns its status to `Active` through its own `IORef` if its connection actively continues.

To check all status easily,
the timeout manager uses a list of the `IORef` to status.
A Haskell thread spawn for a new connection
tries to 'cons' its new `IORef` for an `Active` status to the list.
So, the list is critical section and we needs atomicity to keep
the list consistent.

A standard way to keep consistency in Haskell is `MVar`.
But as Michael Snoyman pointed out in "[Warp: A Haskell Web Server](http://steve.vinoski.net/pdf/IC-Warp_a_Haskell_Web_Server.pdf)", `MVar` (in threaded RTS) is slow.
This is because each `MVar` is protected with a home-brewed spin lock.
So, he used another `IORef` to refer the list and `atomicModifyIORef`
to manipulate it.
`atomicModifyIORef` is implemented on CAS(Compare-and-Swap),
which is much faster than spin locks.

The following is the outline of the safe swap and merge algorithm:

    do xs <- atomicModifyIORef ref (\ys -> ([], ys)) -- swap with []
       xs' <- manipulates_status xs
       atomicModifyIORef ref (\ys -> (merge xs' ys, ()))

The timeout manager atomically swaps the list with an empty list.
Then it manipulates the list by turning status and/or removing
unnecessary status for killed Haskell threads.
During this process, new connections may be created and
their status are inserted with `atomicModifyIORef` by
corresponding Haskell threads.
Then, the timeout manager atomically merges
the pruned list and the new list.

## The algorithm to cache file descriptors

Warp's timeout is safe to implement a cache mechanism for
file descriptors because it does not use reference counters.
However, we cannot simply reuse Warp's timeout for some reasons:

Each Haskell thread has its own status. So, status is not shared.
But we would like to cache file descriptors to avoid open() and
close() by sharing.
So, we need to search a file descriptor for a requested file from
cached ones. Since this look-up should be fast, we should not use a list.
You may think `Data.Map` can be used.
Yes, its look-up is O(log N) but there are two reasons why we cannot use it:

1. `Data.Map` is a finite map which cannot contains multiple values
   for a single key.
2. `Data.Map` does not provide a fast pruning method.

Problem 1: because requests come concurrently,
two or more file descriptors for the same file may be opened.
So, we need to store multiple file descriptors for a single file name.
We can solve this by re-implementing `Data.Map` to
hold a non-empty list.
This is technically called "multimap".

Problem 2: `Data.Map` is based on a binary search tree called "weight
balanced tree". To my best knowledge, there is no way to prune the tree
directly. You may also think that we can convert the tree to a list (`toList`),
then prone it, and converts the list back to a new tree (`fromList`).
The cost of the first two operations is O(N) but
that of the last one is O(N log N) unfortunately.

One day, I remembered Exercise 3.9 of "Purely Functional Data Structure" -
to implement `fromOrdList` which constructs
a red-black tree from an ordered list in O(N).
My friends and I have a study meeting on this book every month.
To solve this problem, one guy found a paper by Ralf Hinze,
"Constructing Red-Black Trees".
If you want to know its concrete algorithm,
please read this paper.

Since red-black trees are binary search trees,
we can implement multimap by combining it and non-empty lists.
Fortunately, the list created with `toList` is sorted.
So, we can use `fromOrdList` to convert the sorted list to a new
red-black tree.
Now we have a multimap whose look-up is O(log N) and
pruning is O(N).

The cache mechanism has been already merged to the master branch of
Warp waiting for releasing.

## New functions in simple-sendfile

I explained the `sendfile` function and
the sendfileWithHeader function in
this article and the previous one, respectively:

    sendfile :: Socket -> FilePath -> FileRange -> IO () -> IO ()
    sendfileWithHeader :: Socket -> FilePath -> FileRange -> IO () -> [ByteString] -> IO ()

To avoid the open()/close() system call, I added two more functions
to the simple-sendfile package:

    sendfileFd :: Socket -> Fd -> FileRange -> IO () -> IO ()
    sendfileFdWithHeader :: Socket -> Fd -> FileRange -> IO () -> [ByteString] -> IO ()

Of course, the master branch of Warp uses the last one.

That's all. Thank you for reading this long article.

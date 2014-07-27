I was stumped by this one myself for a bit today, so I thought writing it up in
a blog post would be a good way to make sure (1) I don't forget this little
fact, and (2) hopefully the next person doesn't need to puzzle over this as
long as I did. Let's say you want to read the contents of a file in the `proc`
filesystem, such as `/proc/uptime`. There are many ways to do that in Haskell.
Let's ignore any streaming data framework for the moment, and instead focus on
just the "string-like" types: `String` and strict/lazy `ByteString`/`Text`.
Here's a little program that tries all of them out:

```haskell
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.IO         as T
import qualified Data.Text.Lazy.IO    as TL

test :: Show a => String -> (FilePath -> IO a) -> IO ()
test name reader = do
    contents <- reader "/proc/uptime"
    putStrLn $ name ++ ": " ++ show contents

main :: IO ()
main = do
    test "String           " readFile
    test "strict ByteString" S.readFile
    test "lazy ByteString  " L.readFile
    test "strict Text      " T.readFile
    test "lazy Text        " TL.readFile
```

Given that the uptime file is just simple ASCII data, you'd probably assume
(like I did) that all of these will produce the same result. In fact, that's
not the case. On my system, the results are:

```
String           : "60740.70 136144.86\n"
strict ByteString: ""
lazy ByteString  : "60740.70 136144.86\n"
strict Text      : "60740.70 136144.86\n"
lazy Text        : "60740.70 136144.86\n"
```

Strict `ByteString` reading is returning an empty value! Why is this happening?
It's actually quite easy to see once you throw in two new pieces of
information. First, let's look at the implementation of
`Data.ByteString.readFile`:

```haskell
readFile :: FilePath -> IO ByteString
readFile f = bracket (openBinaryFile f ReadMode) hClose
    (\h -> hFileSize h >>= hGet h . fromIntegral)
```

Notice how we allocate a buffer exactly the right size to read in the entire
contents of the file. We don't do this with any of the file reading functions.
For the lazy variants, we don't *want* to read the entire file into memory at
once. And for strict `Text`, knowing the size of the file doesn't tell us the
size of the buffer we need to allocate, due to variable length encoding. So
this nifty optimization only applies to strict `ByteString`s.

Now piece of data number two:

```
$ ls -l /proc/uptime 
-r--r--r-- 1 root root 0 Jul 27 13:56 /proc/uptime
```

Huh, the file is empty! As is [well
documented](http://www.tldp.org/LDP/Linux-Filesystem-Hierarchy/html/proc.html),
virtually every file in the proc filesystem is listed as empty, and the
contents are generated on demand by the kernel.

So how do you read the file contents into a strict `ByteString`? There are actually plenty of approaches that work. In my case, I ended up just writing a helper function using conduit:

```haskell
    localReadFile fp =
         IO.withBinaryFile fp IO.ReadMode $ \h ->
         sourceHandle h $$ foldC
```

But probably the simplest thing to do is to just convert a lazy `ByteString`
into a strict `ByteString`, e.g. `fmap L.toStrict . L.readFile`.

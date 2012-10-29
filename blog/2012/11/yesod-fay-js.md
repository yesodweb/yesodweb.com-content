It's been a while since I discussed Yesod's approach to client-side
programming. I haven't been quiet due to a lack of interest. On the contrary,
I've been playing around with a lot of different approaches, and discussing
things with a number of people as well. Additionally, there have been some very
exciting changes in the Haskell Javascript space.

Back when [we discussed this in
April](http://www.yesodweb.com/blog/2012/04/client-side), I demonstrated a
combinator-based approach for generating Javascript from our Haskell code in a
type-safe manner. After playing around with this a bit, I decided it was too
inconvenient to be a focus of efforts. I've kept that code, together with some
other attempts I've made, in a [yesod-js repo on
Github](https://github.com/snoyberg/yesod-js). If you're interested in the
process I went through to come to the ideas I'm stating in this blog post, you
might want to look there. Otherwise, the remainder of this post will be talking
about future plans exclusively.

So the big question is: what do we want from a client-side solution? For me, it
boils down to:

1.  A better language than Javascript. Ideally, I want the same level of static
    type checking in my client-side code as my server-side code. Having a better
    syntax than Javascript is a nice bonus, but not the main point for me.

2.  Tight server/client integration. I'd like automatic serialization of data
    between server and client. Or let's take it a step further: it would be
    great if it's almost transparent to the client-side code if we're calling a
    function locally or remotely.

3.  We need to solve the big problems in client-side development as well. In
    other words: even if we could use GHC on the client side and compile any
    arbitrary piece of Haskell, we'd *still* have problems to solve: the DOM API is
    a mess, creating interactive UIs requires a lot of work, etc.

4.  We should not reinvent the wheel; we can use existing Javascript libraries
    when they're available. This might sound obvious, but I don't think it is
    (I'll give a concrete example later). There is a huge amount of effort going on
    into making client-side development better, and by simply barracading ourselves
    in our Haskell bubble, we'll be missing out on a lot of improvements out there.

    This is actually a more general Yesod philosophy: we should stick to
    tried-and-true techniques as much as possible, and simply make them better by
    adding in Haskell's strengths. An additional benefit of this is the reduced
    learning curve for developers coming from another language. (Or, conversely,
    the fact that you can reuse your Yesod skills when working with other
    languages.)

## AngularJS

We can solve all four of these issues with a two-pronged approach. Starting
with issues (3) and (4): we piggy-back on an existing, well-designed library in
the Javascript world to solve the big problems. After some research and a lot
of recommendations, I mocked up a demo using AngularJS. I have to say, I'm
quite impressed. I won't by any means claim to be an expert, and I'm certainly
not completely sold that it's the One True Path, but it does solve a lot of
problems in an elegant manner.

You can see my [sample code on
Github](https://github.com/snoyberg/yesod-js/blob/master/yesod-angular/angular.hs).
For this demo, I wrote the Javascript code as simple Javascript (e.g.,
[people.julius](https://github.com/snoyberg/yesod-js/blob/master/yesod-angular/angular/people.julius)).
One important trick I used here is providing a set of __commands__. For
example:

```haskell
cmdGetPeople <- addCommand $ \() -> do
    people' <- getYesod >>= liftIO . readIORef . ipeople
    return $ map (\(pid, Person name _) -> PersonSummary pid name) $ Map.toList people'
```

Each command takes a single JSON value as input, and returns a single JSON
value as output. This is an important simplification over the standard approach
of passing separate parameters: we can more clearly define our API, as will
become important when coming to type safety. Calling this from Javascript is
simple:

```javascript
function($scope, $http) {
    $http.post("#{cmdGetPeople}", []).success(function(data) {
        $scope.people = data;
    });
}
```

Note, however, that there is no type safety in this approach.

I would classify AngularJS as giving us some level of reactive programming. We
update variables, and the views update themselves automatically. It doesn't
have all the power of a proper FRP solution (like reactive-banana or netwires),
and it uses a bit of a hack- efficient dirty checking- to get it working. So
for a long time I was opposed to using Angular: why use a hacky, suboptimal
solution to a problem when we have a beautiful solution just waiting to be
refined?

And the answer is simple: Angular is ready to be used now. I still believe that
FRP will give us a better result in the end, and I hope the Haskell community
continues to develop FRP solutions, and bring them into the client-side realm.
But in the meanwhile, using Angular gives us a huge amount of the benefits
we're looking for, while staying with mainstream web development.

## Fay

The problem with this demo is that we're still programming everything in
horrible, ugly, unsafe Javascript. I originally thought to overcome this with a
combinator approach, but as I mentioned I think combinators will be too awkward
overall.

After playing around with some alternatives, I tried out
[Fay](http://fay-lang.org/), and I'm very impressed. It doesn't provide quite
the full Haskell experience, but for most websites I think it's offering the
right trade-off. The generated code, while not idiomatic Javascript, is still
close enough to the source to be recognizable.

One huge selling point of Fay is its Foreign Function Interface (FFI). As an
example, in Fay the `alert` function can be called via:

```haskell
alert :: String -> Fay ()
alert = ffi "window.alert(%1)"
```

This simplicity makes it trivial to interact with existing libraries, such as
jQuery. And as a result, I think Fay can be used as a drop-in replacement for
arbitrary Javascript code. To test out that theory, I started converting a
small project I've been writing to use Fay.

(For those of you wondering: this site is just a tool I built for my wife to
sort our family photos. It's never seen the light of day before.)

You can [view the commit in
question](https://github.com/snoyberg/photosorter/commit/f7527feab714a90a0b9a28c5eef81e5e43361d8b)
to see exactly what I did. The goal was to replace one Julius file with a Fay
file.

### Build Process

One problem people face is how to integrate Fay into your project's build
process. Fay files are valid Haskell, and should be compiled with GHC to
type-check them. Afterwards, they can be compiled by Fay into Javascript.
However, I've gotten spoiled by Julius, and wanted to have automatic code
reloading during development.

To address this, I created two Template Haskell functions.
[fayFileProd](https://github.com/snoyberg/photosorter/commit/f7527feab714a90a0b9a28c5eef81e5e43361d8b#L5R24)
compiles with GHC, erroring out if the compile fails. Then, if that's
successful, I use the Fay library to compile to Javascript. If that succeeds, I
return a Javascript value, the same as Julius would have produced.

The other function is
[fayFileReload](https://github.com/snoyberg/photosorter/commit/f7527feab714a90a0b9a28c5eef81e5e43361d8b#L5R44).
Instead of compiling to Javascript at compile time, it performs the compilation
at runtime, thereby automatically reloading any changes. The Fay compiler
itself runs incredibly quickly, so there's no noticeable lag. And if you have
errors in your Fay file, they show up in the browser:

![Fay error message](http://www.yesodweb.com/assets/yesod-fay-js/fay-error-message.png)

Then there's a helper function called `fayFile` which will use `fayFileProd`
during production builds and `fayFileReload` when using `yesod devel`.  This won't
catch type errors during development builds, but for me that's an acceptable
trade-off: we get very fast response times, and our production builds are
guaranteed to be type-safe. If you really want, you can manually run `ghc` to
type-check your code.

I [used
`fayFile`](https://github.com/snoyberg/photosorter/commit/f7527feab714a90a0b9a28c5eef81e5e43361d8b#L3R20)
just like I'd use `widgetFile`. No modifications to the build system necessary.

### File structure

If you looked at the code above, you might have noticed some special folders.
What I've set up is to subfolders: `fay-shared` is for modules used by both
client- and server-side code (see the next section about this). The `fay`
folder is for client-side-only code. My plan is to modify the `yesod devel`
file checker to ignore file changes in the `fay` folder, thereby avoiding code
reload when you're just playing around with client-side code.

### Commands

In the code I gave for Angular above, I mentioned the concept of commands. It
turns out that Chris came up with almost exactly the same approach for getting
Fay to interact with the server. I've adopted his approach almost verbatim in
this example.

In `fay-shared`, there's a single module which defines a [`Command`
datatype](https://github.com/snoyberg/photosorter/commit/f7527feab714a90a0b9a28c5eef81e5e43361d8b#L7R11).
This represents all of the commands that can be sent to the server, along with
the results of such calls. In the
[Handler.Command](https://github.com/snoyberg/photosorter/commit/f7527feab714a90a0b9a28c5eef81e5e43361d8b#L2R1)
module, I handle these requests on the server. The `handle` function is
general-purpose, and could be put into a `yesod-fay` package. In
`handleCommand`, I've written handler functions for each command. The type
system ensures that the response we send back is the one the client is
expecting.

[Calling a command from the
client](https://github.com/snoyberg/photosorter/commit/f7527feab714a90a0b9a28c5eef81e5e43361d8b#L9R23)
is even easier. For example:

```haskell
call (AddPost date slug) $ const reload
```

What this says is "Call the AddPost command with date and slug parameters, and
when the call returns, I want you to ignore the response value and reload the
page."

If you compare the [original
Javascript](https://github.com/snoyberg/photosorter/commit/f7527feab714a90a0b9a28c5eef81e5e43361d8b#L13L1)
to the [new Fay
code](https://github.com/snoyberg/photosorter/commit/f7527feab714a90a0b9a28c5eef81e5e43361d8b#L9R1),
they're basically the same structure. Both are using jQuery and registering
callbacks for events. Both are making AJAX calls to the server. But personally,
I much prefer the Fay code: it's easier to read (as a Haskeller), and the AJAX
calls are completely type-safe.

## Why not ghcjs?

I played around with ghcjs. It's a truly amazing project: it allows you to
compile virtually any GHC-understood code to Javascript. However, there are a
few reasons I've decided to go with Fay for the moment:

1.  There's a difficult installation process for ghcjs, involving a modified
    version of GHC. Over time, I believe that this will be simplified as patches
    get merged upstream, but for many users it would be too difficult a bar to
    overcome.

2.  One of Fay's great features is its dead-simple FFI. By contrast, ghcjs has
    the traditional FFI, which makes more difficult to interact with Javascript
    libraries.

3.  Currently, the generated code won't run on Internet Explorer. I know
    there's work in the pipeline to address this, so longterm it shouldn't be a
    problem.

So if you read between the lines here, what I'm really saying is, "ghcjs isn't
the right solution today, but it might be soon." It's a truly amazing effort,
and I'm looking forward to where it heads. I think there's a lot of room for
Fay and ghcjs to complement each other: Fay being a simpler tool for simple
tasks, and ghcjs being the "big guns" when you need more power.

## And Elm?

Another solution in this space which is developing nicely is
[Elm](http://elm-lang.org/). It provides a client-side FRP solution, and with
[elm-yesod](http://hackage.haskell.org/package/elm-yesod) there's already the
ability to integrate nicely with Yesod.

However, as much as Elm is influenced by Haskell, it's still a separate
language. This may not be a problem for many cases, and if you think Elm would
be the right fit, you should definitely give it a shot.

## Next steps

I sketched out four problems I'm hoping to solve. As it stands, I think Fay
solves the first two, and Angular solves the second two. So the next obvious
step would be to integrate these two approaches. Initially, this looks pretty
simple: using Angular required writing some Javascript code. Now we'll just
write Fay code.

Fay is still a very young project, and therefore there are still some obstacles
to overcome. In the process of writing this code, I came up with the following
observations:

* We really need to have some package management system for Fay. I'm not too
  concerned with exactly what this would look like, but it's important to be
  able to share common bindings like jQuery.

* When sharing datatypes between the server and client, I'm forced to use
  `String` instead of `Text` (there are likely a few other examples of this
  too). It would be nice if Fay could automatically treat `Text` as a synonym for
  `String`, while for the server-side code use `Data.Text.Text`.

* Chris's example code has a `getThis` function for retrieving the `this`
  object in Javascript. Unfortunately, due to how function calls work in Fay,
  this is almost meaningless. In particular, `this` does *not* point to the
  clicked DOM element when using a jQuery event handler. I worked around this by
  pulling the information out of the event object itself, but it would be nice if
  we could somehow recover `this`.

* A very minor point: `CompilerState` returns a list of modules which have been
  parsed. If it could also provide the `FilePath`s these were parsed from, then
  I could register those files as dependencies in the Template Haskell calls.

But these are all relatively minor points. The fact that, without too much
difficulty, I could convert some live Javascript code into Haskell code is very
exciting. Maybe 2013 will be the year of Haskell on the browser :).

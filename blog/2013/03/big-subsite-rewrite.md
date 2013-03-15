In my [previous blog
post](http://www.yesodweb.com/blog/2013/03/yesod-1-2-cleaner-internals), I
alluded to some more radical changes I'd been playing with for the Yesod 1.2
release. Initially, I thought I wouldn't be including them for now, but after
[some experimentation](https://github.com/yesodweb/yesod/tree/new-subsite), I
think I'm going to go ahead and include this.

For a quick overview of what's happening, you can look at the updates I've made to the following codebases:

* [yesodweb.com](https://github.com/yesodweb/yesodweb.com/commit/bef548d92c13dfd8b18e251bb258e1a2e0f08994)
* [yesod-fb](https://github.com/snoyberg/yesod-fb/commit/b8fc9849c6812445b0b04030be3ccb9fb2110b80)
* [yesod-auth-fb](https://github.com/snoyberg/yesod-auth-fb/commit/5f121c23fb8ca4bd041ad55e5e09bb00526fee37)
* [haskellers.com](https://github.com/snoyberg/haskellers/commit/3f149f8cf4ce7ffdb3512bcaa13fd0558e01baaa)

I've also been keeping the [detailed
changelog](https://github.com/yesodweb/yesod/wiki/Detailed-change-list)
up-to-date with the coming changes. And if you just want to jump ahead and see
what a subsite looks like, go the [the end of the blog
post](#putting-it-together).

## Irritations

If it ain't broke, don't fix it. So let's ask the question: what's wrong with
Yesod 1.1, and in particular the subsite integration? The following are some
annoyances I've had with it for quite a while:

* GHandler and GWidget both take type parameters for both the subsite and the master site, *even if you aren't writing a subsite*.
    * This added complication appears everywhere.
    * It's unclear whether users should write code that says `GHandler sub App ()` or `GHandler App App ()` (the latter having the convenient synonym `Handler ()`).
    * If you use the latter, your code is more readable, but may cause problems if you later decide to use a subsite.
    * Even if you use the prettier `Handler` wrapper, error messages will still mention `GHandler App App`.
* Speaking of error messages, we had really ugly onces back in the old days of the GHandlerT monad transformers (anyone remember `GHandlerT sub App (Control.Monad.Trans.Resource.ResourceT IO)`?). To work around this, `GHandler` and `GWidget` are no longer monad transformers.
    * But we still want to be able to *pretend* that they're transformers, so we have a [custom `lift` function](http://haddocks.fpcomplete.com/fp/7.4.2/20130313-1/yesod-core/Yesod-Handler.html#v:lift).
    * We can't do useful things like restrict the actions a handler can do by changing its underlying monad.
* We have this weird arbitrary encoding of master and subsite. What if a subsite wants to have a subsite? Well, it's possible, but it's quite confusing how that fits in with datatypes that only mention two site parameters.
* Many functions (like `getCurrentRoute`) refer to the subsite instead of the master site. As a result, you need to play around with things like `getRouteToMaster` even if you're not writing a subsite.

And then of course the main issue everyone probably has with subsites: they're
too confusing to create! Actually, I think this is *mostly* a documentation
issue, with a little bit of Template Haskell woes thrown in. We'll get to that
later, but for now I want to talk about how we're going to change the core of
Yesod itself to allow for better subsites.

## Back to transformers

Let's dive in with the new and improved approach:

    newtype HandlerT site m a
    newtype WidgetT  site m a

That's basically the whole change I'm talking about. Instead of a subsite and
master site parameter, we have a single site parameter. And instead of
hard-coding a specific underlying monad, the monad is a parameter again. This
immediately solves all the custom `lift` nonsense. And clearly, if you're not
writing subsite code, you won't even have to think about subsites, because
*there is no subsite*!

So now, if I'm writing the application `App`, my handler functions look like:

    myAppHandler :: HandlerT App IO RepHtml

And just like now, we'll have the convenient `Handler` type synonym, now defined as:

    type Handler = HandlerT App IO

So for the majority of user code just using the `Handler` synonym, nothing has
to change.

But that just begs the question: how *do* you write code that lives in a
subsite? Easy: you stack the transformers. Suppose you're working in the `Auth`
subsite; a handler function would look something like this:

    myAuthHandler :: HandlerT Auth (HandlerT App IO) RepHtml

Or more generally, you could allow `myAuthHandler` to work with *any* master
site:

    myAuthHandler :: HandlerT Auth (HandlerT master IO) RepHtml

Or you could require that the master site implement some kind of interface via
typeclasses:

    myAuthHandler :: YesodAuth master => HandlerT Auth (HandlerT master IO) RepHtml

There's no longer any confusion about "which site" a function lives in, since
`HandlerT` only knows about a single site. So `getCurrentRoute` just returns a
route, not a "subsite route" or a "master site route". `getYesod` returns the
site foundation type, not sub or master site.

But if you're writing a subsite, you might need to access information from the
parent. But that's now trivial: just use `lift`. For example:

    myAuthHandler :: HandlerT Auth (HandlerT App IO) RepHtml
    myAuthHandler = do
        auth <-      getYesod -- returns an Auth
        app  <- lift getYesod -- returns an App

This change requires some code rewriting for upgrading, but after working on it
a while I believe that the trade-off is worth it.

## Where's ResourceT?

If you're paying close attention, you might be wondering where ResourceT went.
The answer is that it's actually embedded inside of `HandlerT`. In fact, I just
[added some code to
resourcet](https://github.com/snoyberg/conduit/blob/master/resourcet/Control/Monad/Trans/Resource.hs#L692)
to make this kind of usage more efficient. The reasons I went for this
embedding are:

* Avoiding an extra transformer layer can mean increased performance.
* Type signatures and error messages will stay simpler.

This means that, unlike Yesod 1.1, using `lift` inside a `Handler` will *not*
let you run a `ResourceT IO` action. Instead, you'll want to use
`liftResourceT`.

## And what about WidgetT

I've followed almost exactly the same formulation for `WidgetT`. And if you're
not writing subsites, there's really only one other thing you need to know.
Instead of `lift`ing handler actions into your `WidgetT`, most handler actions
live in typeclasses now, so they can be automatically used in a Widget. If you
do have a `Handler` function that you want to lift, you'll need to use
`handlerToWidget`. (That might get changed to `liftHandlerT` instead, but
that's a different discussion...)

For subsites, the situation is a little bit more tricky. To understand why,
consider the new type signature for the `defaultLayout` method:

    defaultLayout :: WidgetT site IO () -> HandlerT site IO RepHtml

Now consider that you're writing a widget in a subsite that refers to a route
in the subsite. This might look something like:

    [whamlet|<a href=@{LoginR}>Please login|]

Since `LogingR` is a route in the Auth subsite, the type of that widget is
`WidgetT Auth IO ()`. But if we're working on the `App` site, `defaultLayout`
expects a `WidgetT App IO ()`. How do we reconcile the two? I've come up with two approaches.

1.  Never created subsite widgets. Instead, whenever you want to embed a
    subsite URL, you convert it to a master site URL. Then, you can apply
    `defaultLayout` and `lift` its result: For example:

        toParent <- getRouteToParent
        lift $ defaultLayout [whamlet|<a href=@{toParent LoginR}>Please login|]

2.  Provide a helper function to lift up a widget into the parent site. This looks something like:

        liftWidget :: WidgetT child IO a
                   -> HandlerT child (HandlerT parent m) (WidgetT parent m a)

    Then our example would be:

        widget <- liftWidget [whamlet|<a href=@{LoginR}>Please login|]
        lift $ defaultLayout widget

So far, I favor the first, as it's pretty close to what we do already. This
still doesn't feel as elegant as I'd like it to be. However, in its favor, this
approach doesn't really perform any magic: it's fairly obvious what each step
is doing. And for the most part, this will be boilerplate stuff that all
subsites use, so it should be something that can be learnt once and reused.

<h2 id="putting-it-together">Putting it together</h2>

So how do you actually create a subsite? It's much like creating a normal site, with a few differences:

1. Use mkYesodSubData to create your route datatype and rendering function.
2. Create a YesodSubDispatch instance, using the mkYesodSubDispatch TH function to generate the dispatch function itself.
3. Instead of a plain Handler, you'll have a Handler stack, something like: HandlerT SubSite (HandlerT master IO).

I've put together a [subsite
demo](https://github.com/yesodweb/yesod/tree/new-subsite/demo) which
demonstrates how to create a subsite. Here are some important excerpts from
that demo.

Creating routes is very similar to how you do so for a master site. Instead of
mkYesod, you use mkYesodSubData:

    mkYesodSubData "Wiki" [parseRoutes|
    / WikiHomeR GET
    /read/*Texts WikiReadR GET
    /edit/*Texts WikiEditR GET POST
    |]

To set up dispatch, you must create an instance of YesodSubDispatch, using the
mkYesodSubDispatch TH helper function to generate the actual code. You can put
whatever restrictions you want on the master site by changing the typeclass
constraints.

    instance YesodWiki master => YesodSubDispatch Wiki (HandlerT master IO) where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesWiki)

As described above, you have to remember to lift some functions and convert
subsite routes to the parent site, e.g.:

    toParent <- getRouteToParent
    lift $ defaultLayout
        [whamlet|
            <p>
                <a href=@{toParent $ WikiReadR page}>Read page
        |]

---

There is definitely still some room for improvement here. We can probably find
some common patterns to be abstracted out, like "apply defaultLayout and
convert all the routes". But overall, this approach feels much cleaner to me
than what we have currently. And most importantly, it takes the complexity out
of the majority of apps entirely.

In my opinion, the only downside with this change is its breaking nature, but
hopefully most breakage can be mechanically fixed (e.g., replace GHandler Foo
Foo with HandlerT Foo IO). If people see other problems, or think I'm
understating the effect of the breakage, please bring it up.

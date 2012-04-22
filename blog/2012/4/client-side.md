We've said for a while that our big feature in the post-1.0 world of Yesod
would be better client side integration. Well, we had the 1.0 release about two
weeks ago. That's enough time to relax, time to keep plowing ahead!

There are many different approaches we could consider taking for client side
integration. I'm going to describe one here that I've started working on. But
that doesn't mean any decisions have been made. The purpose of this post is to
get the discussion started on the direction we want to take, and hopefully
start to flesh out more clearly what our goals are.

## Motivating case: name concatenator

As usual for something as ambitious as a completely new paradigm for client
side development, let's start with a use case that doesn't reflect any real
world use at all. We want to create a page where you can type in your first and
last names, and as you type them, a span on the page will automatically be
updated with your full name. (Yes, I know this isn't very i18n-friendly...)

Let's think about what we'd have to do if we want to write this manually:

* Set up some HTML with the input fields and the span.
* Generate some kind of unique identifiers for each of those three.
* On page load, bind event handlers to the keyup event of the input fields which will somehow adjust the span.

Not too bad. Yesod will already help us with the unique identifiers with
`newIdent`. We can use hamlet to put together the HTML without any trouble, and
then just write some Javascript for the event handling. So why isn't this an
acceptable solution to the problem?

* We have to write in two completely different languages: Haskell and Javascript.
* There's no way for the compiler to help us with ensuring the Javascript code is correct.
* We're going to have almost identical code for the two text fields, and there's no immediately obvious way to avoid code duplication.
* The solution is not composable, and doesn't scale well for large projects.

## An approach: FRP-inspired monadic EDSL

I've put together a [proof of concept](https://github.com/snoyberg/yesod-js)
library. Let's look at an example that implements the behavior I described
above.

    getHomeR :: Handler RepHtml
    getHomeR = defaultLayout $ runJS $ do

        (firstWidget, firstVal) <- textInput
        (lastWidget, lastVal) <- textInput

        let fullVal = firstVal `jsPlus` " " `jsPlus` lastVal
        fullWidget <- textOutput fullVal

        lift [whamlet|
    <p>First name: ^{firstWidget}
    <p>Last name: ^{lastWidget}
    <p>Full name: ^{fullWidget}
    |]

We use the `textInput` function to generate two values: a widget containing the
input tag, and some special value. We then use the `jsPlus` function to
concatenate the first value and the last value, with a space in the middle. We
plug this value into the `textOutput` function to get a widget containing a
span tag that will display the full name. And finally, we stick all of those
widgets into a final widget and display it.

Personally, this kind of feels like an ideal solution to this specific problem.
I can't guarantee that this approach will scale, or that it will work well for
all kinds of client side code, but it feels like a nice fit right now. I'd love
to get feedback.

## The devil's in the details

Let's get into the details a bit. The easiest thing to start with is looking at
the type signatures involved:

    data JSValue jstype
    data JSTypeString
    type JSString = JSValue JSTypeString
    type JS sub master = WriterT JSData (GWidget sub master)
    
    runJS :: YesodJquery master => JS sub master a -> GWidget sub master a
    textInput :: JS sub master (GWidget sub master (), JSString)
    textOutput :: JSString -> JS sub master (GWidget sub master ())
    
    class JSPlus a
    instance JSPlus JSTypeString
    jsPlus :: JSPlus jstype => JSValue jstype -> JSValue jstype -> JSValue jstype

We have a new datatype `JSValue` which includes a
[phantom type](http://www.haskell.org/haskellwiki/Phantom_type). This phantom states the
Javascript datatype of the value. We can use this to prevent us from accidently
adding a string and a number together, for example. Notice that `JSString` is
just a `JSValue` with a `JSTypeString` phantom.

We also introduce a new monad, `JS`, which our functions need to live inside.
`runJS` is our unwrapper for this monad. When we run it, all necessary
Javascript is automatically generated and added to our `Widget`.

The `textInput` function does exactly what I explained before: It gives back a
widget and a value. But now we can see that said value is just a `JSString`.
Likewise, when `textOutput` takes in a value, it's a `JSString` as well.

Finally, we have the `jsPlus` function, which will only allow you to add
together two things of the same type. I used a typeclass to restrict what could
be added; an alternate approach would be to have a separate function for each
type of "plussing", e.g. `jsPlusString`, `jsPlusInt`, etc. I have no strong
feeling on which approach should be taken.

By the way, the reason we could use `jsPlus` on " " is because of
`OverloadedStrings` and the following instance:

    instance (JSTypeString ~ jstype) => IsString (JSValue jstype) where
        ...

## The generated Javascript

To better understand the next section, let's take a sidetrack to analyze the
generated Javascript. Adding in some indentation:

    $(function(){
        var h2; // first name
        var h4; // last name
    
        var h6 = function() {
            $("#h7").text((h2||'') + " " + (h4||''))
        };
    
        $("#h3").keyup(function(){
            h2 = $(this).val();
            h6()
        });
    
        $("#h5").keyup(function(){
            h4 = $(this).val();
            h6()
        });
    });

We have two variables: `h2` and `h4`. (All the names are auto-generated via
`newIdent`.) These are used to cache the values in the first and last name
input fields, respectively. Next, we have a function `h6`, which uses the `h2`
and `h4` variables to create the full name. It places that result in the
appropriate span tag, `h7` being the auto-generated ID of the span tag.

Finally, we bind to the `keyup` events for both of the input fields. Each time,
we cache the value in the field to the appropriate local variable, and then
call the `h6` function to update the span.

Now let's see how we generate such Javascript.

## The JS monad

If you look at the definition of the `JS` monad, you'll see that it's just a
`WriterT` for `JSData`. So really, all of the magic for this code lives in that
datatype. Let's look at this thing:

    data JSData = JSData
        { jsdEvents :: Map.Map JSVar (Set.Set JSFunc -> Builder)
        , jsdVars :: Set.Set JSVar
        , jsdFuncs :: Map.Map JSFunc Builder
        , jsdDeps :: Map.Map JSVar (Set.Set JSFunc)
        }
    
    newtype JSVar = JSVar Text
    newtype JSFunc = JSFunc Text

Let's start with the simple ones, and build our way up. `jsdVars` is simply a
set of all the variables that we're going to define. Each time we use
`textInput`, it auto-generates a new variable name and adds it to this set.
That's how we got our list of variable declarations in our output Javascript.

Next we have `jsdFuncs`, which maps function names (e.g., `h6`) to their
bodies. This would be generated by `textOutput`, specifying how to update the
output field.

`jsdDeps` is a connection between the previous two fields. It specifies which
functions depend on which variables. So for our example, it would look
something like:

    Map.fromList [("h2", Set.singleton "h6"), ("h4", Set.singleton "h6")]

To understand how this works, let's look at the definition of `JSValue`:

    data JSValue jstype = JSValue
        { jsvExpr :: Builder
        , jsvDeps :: Set.Set JSVar
        }

A `JSValue` is just two pieces of information: a Javascript expression, and a
set of variables it depends on. When we pass a value to `textOutput`, it
creates a new function (in this case, `h6`) which uses the `jsvExpr` to update
the span tags, and then creates dependencies between each variable in `jsvDeps`
and that function. What we're saying is: each time one of the underlying
variables gets updated, please call this function.

Finally, we get to `jsdEvents`. This is where we specify how to update each
variable. It maps the variable name to a funny type:

    Set.Set JSFunc -> Builder

What this is saying is: Tell me which functions depend on my value. Then I'll
set up an event handler that will update the local variable and call any
dependent functions and update them as well.

## That's all for now

I'm hoping this sparks a discussion about benefits and weaknesses of the
approach I've set up here, and possible alternatives. If you want to see more
of the internals of how this is implemented,
[check out the sourcecode](https://github.com/snoyberg/yesod-js/blob/master/Yesod/Javascript.hs).

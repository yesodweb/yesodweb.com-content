I think we're overdue on having a new example Yesod application. This time, I
want to demonstrate reusable components, specifically Widgets.

I saw a [blog post about Yesod's
composability](http://www.steveseverance.com/2012/10/02/ahgth-comments-on-snapyesod/)
recently. My takeaway from the post is twofold:

1. It's not clear to some users how to create reusable components in Yesod.

2. We're lagging behind systems like ASP.NET by having a smaller ecosystem of
   reusable components readily available.

I think it's too easy to misinterpret these two points as saying the Yesod is
missing out on features to produce reusable components. As [I mentioned in a
previous post](http://www.yesodweb.com/blog/2012/08/announcing-yesod-1-1),
there are three main approaches to creating reusable components in Yesod:
creating general purpose functions (not Yesod-specific), widgets, and subsites.

Of the three, I think the widgets are currently the area we should focus on the
most. They provide the ability to create collections of HTML, CSS, Javascript,
and external dependencies that can be placed in other pages. One way in which
we take advantage of this in the core Yesod packages is to allow form fields to
register Javascript components, such as a jQuery UI datepicker. In this post,
I'd like to give another concrete example, inspired by Steve's blog post:
charts.

Note: the code used in this post is available [as a Github
gist](https://gist.github.com/3830169). There might be many differences between
the version on the blog and the version in the gist.

## `Jqplot.hs`

    {-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

We're going to provide a module that provides a pure-Haskell interface to
the jqplot library. To use it, you don't need to write any HTML, CSS, or
Javascript in your code, as we'll see in our example app.

The idea is that all of the low-level Javascript code goes in this module,
and apps just deal with the widget.

Note that, for simplicity, we're only implementing a tiny subset of jqPlot's
full functionality. A more full-fledged wrapper could be written, but would
obviously be more involved.

    module Jqplot
        ( YesodJqplot (..)
        , YesodJquery (..)
        , PlotSettings (..)
        , plot
        ) where

    import Yesod
    import Yesod.Form.Jquery (YesodJquery (..))
    import Data.Monoid ((<>))
    import Data.Text.Lazy.Builder (toLazyText)
    import Data.Aeson.Encode (fromValue)
    import Data.Aeson (ToJSON (toJSON))
    import Data.Text (Text)
    import qualified Data.Text.Lazy
    import Text.Julius (juliusFile)

This is just a minor utility function which renders values to JSON text.
jqPlot, like many Javascript libraries, allows its functions to take
arguments as JSON data. This function could in theory be provided by aeson.

    encodeText :: ToJSON a => a -> Data.Text.Lazy.Text
    encodeText = toLazyText . fromValue . toJSON

We need to be able to find the jqPlot Javascript files. By putting the
location in a typeclass, users are able to provide whichever location they
want (e.g., on a local webserver) or use the default value, which in this
case uses the jsdelivr CDN.

Note that the YesodJquery typeclass, provided by the yesod-form package, works the same way.

    class YesodJqplot master where
        jqplotRoot :: master -> Text
        jqplotRoot _ = "http://cdn.jsdelivr.net/jqplot/1.0.4/"

Now we begin our Haskell API. For a plot, we'll have three settings: the
label for the X and Y axes, and the datapoints to be plotted.

    data PlotSettings = PlotSettings
        { psXLabel :: Text
        , psYLabel :: Text
        , psData :: [(Double, Double)]
        }

This function is the meat of our module. It takes the PlotSettings and
turns it into a Widget. Note the type signature here: we're using GWidget
with arbitrary subsite and master site, so that this widget will work with
many different applications. However, since we need to know the location of
the jQuery and jqPlot libraries, we have the relevant typeclasses in the
context.

    plot :: (YesodJquery master, YesodJqplot master) => PlotSettings -> GWidget sub master ()
    plot PlotSettings {..} = do

Grab the master site...

```haskell
    master <- lift getYesod
    let root = jqplotRoot master
```

so that we can add dependencies to the widget. Note that the calling app
will automatically inherit these dependencies, and can remain completely
ignorant of what's going on inside this function. Yesod will also ensure
that each file is only included in the page once.

```haskell
    addScriptEither $ urlJqueryJs master
    addScriptRemote $ root <> "jquery.jqplot.min.js"
    addScriptRemote $ root <> "plugins/jqplot.canvasTextRenderer.min.js"
    addScriptRemote $ root <> "plugins/jqplot.canvasAxisLabelRenderer.min.js"
    addStylesheetRemote $ root <> "jquery.jqplot.min.css"
```

We need to give a unique ID to a div tag where the chart will be placed.
We ask Yesod to provide a unique identifier and then both the Julius and
Hamlet templates are able to use it. This avoids two problems: typos
between the two files, and name collisions when using the same widget
twice in a page.

```haskell
    divId <- lift newIdent
```

And as a standard best practice, we've placed the Julius and Hamlet in
separate files.

```haskell
    toWidget $(juliusFile "jqplot.julius")
    $(whamletFile "jqplot.hamlet")
```

## `jqplot.julius`

Here's the important point to make: when you're *creating* a widget, you have
to write Javascript. In many ways, you can consider this like writing an FFI
binding: it's low level and not type safe, but once you've written it properly,
you can interact fully from within Haskell-land.

In our case, here's the Julius file that produces the relevant Javascript.

```javascript
$(function(){
  $.jqplot('#{divId}', [#{encodeText psData}], {
      series:[{showMarker:false}],
      axes:{
        xaxis:{
          label:#{encodeText psXLabel}
        },
        yaxis:{
          label:#{encodeText psYLabel},
          labelRenderer: $.jqplot.CanvasAxisLabelRenderer
        }
      }
  });
});
```

## `jqplot.hamlet`

The HTML is incredibly simple: it just creates a dummy `div` tag with the correct `id`.

    <div id=#{divId}>

That can also be written as:

    <div ##{divId}>

But that version looks just a bit more confusing to me.

## `jqplot-example.hs`

Finally, let's use this library!

    {-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
    import Yesod
    import Jqplot

Let's kick off a basic Yesod app with three routes: a homepage and two
charts.

    data App = App

    mkYesod "App" [parseRoutes|
    / HomeR GET
    /chart1 Chart1R GET
    /chart2 Chart2R GET
    |]

Next we create typeclass instances. We can use the default methods for
YesodJquery and YesodJqplot and take advantage of the built-in CDNs, or
could override and use a local copy instead. For simplicity, we choose the
former.

    instance Yesod App
    instance YesodJquery App
    instance YesodJqplot App

Basic handler, nothing special.

    getHomeR :: Handler RepHtml
    getHomeR = defaultLayout $ do
        setTitle "Homepage"
        [whamlet|
    <p>Demonstration of a reusable widget.
    <p>
        <a href=@{Chart1R}>First chart
        |
        <a href=@{Chart2R}>Second chart
    |]

In this chart, we create a graph of the x² function, without writing a
single line of HTML, CSS, or Javascript. In theory, as more widgets become
available in the Yesod ecosystem, this type of development could become more
standard. But for now, the following example is more normative.

    getChart1R :: Handler RepHtml
    getChart1R = defaultLayout $ do
        setTitle "Graph of x²"

        plot PlotSettings
                { psXLabel = "x"
                , psYLabel = "x²"
                , psData = map (\x -> (x, x * x)) $ [0, 0.1..10]
                }

Very often, you'll want to take some precomposed widget and embed it
inside some other HTML. That's entirely possible with widget interpolation.

```haskell
getChart2R :: Handler RepHtml
getChart2R = defaultLayout $ do
    setTitle "Made up data"
    toWidget [lucius|
.chart {
    width: 500px;
    height: 300px;
}
|]
    [whamlet|
<p>You can just as easily embed a reusable widget inside other source.
<div .chart>^{madeUpData}
<p>And it just works.
|]
  where
    -- Yup, totally bogus data...
    madeUpData = plot PlotSettings
        { psXLabel = "Month"
        , psYLabel = "Hackage uploads"
        , psData =
            [ (1, 100)
            , (2, 105)
            , (3, 115)
            , (4, 137)
            , (5, 168)
            , (6, 188)
            , (7, 204)
            , (8, 252)
            , (9, 256)
            , (10, 236)
            , (11, 202)
            , (12, 208)
            ]
        }
```

And now we just run our app!

    main :: IO ()
    main = warpDebug 3000 App

## Conclusion

I think widgets are one of the best features in Yesod (I think I rank it at #2,
right behind type-safe URLs). I haven't seen any other framework provide this
kind of packaging of HTML, CSS, and Javascript together.

I think one of the reasons for this is that Haskell's type system makes the
implementation so straight-forward. The contents of a `Widget` form a `Monoid`,
and the `Widget` itself is essentially a `WriterT` transformer sitting on top
of the standard `Handler` monad. That means we can run arbitrary code from
inside our `Widget`, such as pulling out the last five blog entries for a
"recent activity" widget.

I'm very curious to see how the other Haskell web frameworks approach this
problem. If the approaches are similar enough, I think it would be worth
investigating the possibility of creating a more universal widget system, so
that widgets could be shared among various Haskell frameworks. If anyone's
interested in working on this with me, please be in touch.

Following up on our [previous post](http://www.yesodweb.com/blog/2012/04/client-side) outlining a possible client-side approach for Yesod, I wanted to follow up with a slightly more concrete example. It seems the rage today is to provide a todo list example, so here's mine. All of the code is available from [the yesod-js Github repo](https://github.com/snoyberg/yesod-js).

This is all still very much experimental, and no one has made any decisions yet regarding whether we'll be pursuing this approach (or any others). The discussion is still completely open, I'm just adding some more fuel to the fire.

-----------------

Let's put together a simple example app that will keep a server-side list of todo items, and allow the client to update them. First, our language extensions and imports.

    {-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, KindSignatures,
                 TypeFamilies, FlexibleContexts, GADTs, MultiParamTypeClasses,
                 FlexibleInstances, TypeSynonymInstances
      #-}
    import Yesod.Core
    import Yesod.Persist
    import Data.Text (Text)
    import Database.Persist.Sqlite
    import Network.Wai.Handler.Warp (run)
    import Yesod.Json
    import Yesod.Form.Jquery (YesodJquery)
    import Network.HTTP.Types (status201, status204)
    import Yesod.Javascript
    import Data.Aeson hiding (object)
    import Control.Applicative ((<$>), (<*>))
    import Text.Lucius (lucius)

We'll store our data in Persistent, and set up some serialization with JSON. This is all pretty boilerplate.

    share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Todo
        text Text
        done Bool
    |]
    
    instance ToJSON (Entity Todo) where
        toJSON (Entity tid (Todo text done)) = object
            [ "id" .= tid
            , "text" .= text
            , "done" .= done
            ]
    instance FromJSON Todo where
        parseJSON (Object o) = Todo
            <$> o .: "text"
            <*> o .: "done"
        parseJSON _ = fail "Invalid todo"

Now our app. We'll have a pretty standard REST interface, allowing GET, PUT and DELETE.

    data App = App ConnectionPool
    
    mkYesod "App" [parseRoutes|
    / HomeR GET
    /todo TodosR GET PUT
    /todo/#TodoId TodoR GET DELETE
    |]

And a bit more boilerplate.

    instance Yesod App
    instance YesodPersist App where
        type YesodPersistBackend App = SqlPersist
        runDB f = do
            App pool <- getYesod
            runSqlPool f pool
    instance YesodJquery App

Let's implement our RESTful interface. This is all standard Yesod stuff, nothing client-side specific. However, we're not providing any HTML representations for now. That could be easily changed so we could have a more robot-friendly site, but that's not our purpose here.

    getTodosR :: Handler RepJson
    getTodosR =
        runDB (selectList [] []) >>= jsonToRepJson . asTodoEntities
      where
        asTodoEntities :: [Entity Todo] -> [Entity Todo]
        asTodoEntities = id
    
    putTodosR :: Handler ()
    putTodosR = do
        todo <- parseJsonBody_
        tid <- runDB $ insert todo
        sendResponseCreated $ TodoR tid
    
    getTodoR :: TodoId -> Handler RepJson
    getTodoR tid = runDB (get404 tid) >>= jsonToRepJson . Entity tid
    
    deleteTodoR :: TodoId -> Handler ()
    deleteTodoR tid = do
        runDB (delete tid)
        sendResponseStatus status204 ()

Now we'll start our client side stuff. All client-side values get wrapped in `JSValue`, with a phantom type to indicate the actual datatype. Let's generate some getters to access the fields of a todo item.

    jsTodoText :: JSValue (Entity Todo) -> JSValue Text
    jsTodoText = jsGetter "text"
    
    jsTodoDone :: JSValue (Entity Todo) -> JSValue Bool
    jsTodoDone = jsGetter "done"

We'll also set up a constructor to generate a new todo item. We have to use `jsCast` to erase the phantom types of the individual values.

    jsTodo :: JSValue Text -> JSValue Bool -> JSValue Todo
    jsTodo text done = jsonObject
      [ ("text", jsCast text)
      , ("done", jsCast done)
      ]

In theory, this kind of code could automatically be generated as part of Persistent (along with the ToJSON/FromJSON instance generation, which Persistent can already handle). One thing to note is that the underlying `jsGetter` function is *not* typesafe. By giving explicit signatures here, we're adding back type safety.

Next, we'll implement our homepage. Our interface doesn't (yet) allow you to mark items as done, but we provide the CSS class anyway.

    getHomeR :: Handler RepHtml
    getHomeR = defaultLayout $ do
        toWidget [lucius|
    .done {
        color: #999;
    }
    |]
        runJS $ do

Let's pull in the todo items via Ajax. `ajaxJson` takes a type-safe route and returns two values: the data pulled from the server, and the name of the function that can be used to reload the data. We'll use that later, when we add new items.

            (todos, reload) <- ajaxJson TodosR

Now we'll format our todo items into HTML. There are a number of functions here worth mentioning:

* `htmlOutput` will take a `JSValue Html` and produce a `Widget`.
* `wrapTag` will wrap a `JSValue Html` with an extra tag. `wrapTagClass` does the same, but will also apply a `class`-attribute.
* `jsjoin` will concatenate a `JSValue [Html]` into a `JSValue Html`. This is performed client side with the Javascript `join` function.
* `jsfor` maps over a list of items. Under the surface, it uses Underscore.js's `map` function.
* `jsif` is essentially Javascript's ternary operator `?:`.
* `jsToHtml` turns text into HTML. Notice that we're getting the same XSS-protection that Yesod is known for, even at the client side.

I'm not sure how I feel about this block, but it certainly works. The end result is a simple unordered list containing all of the TODO items.

            list <- htmlOutput $ wrapTag "ul" $ jsjoin $ jsfor todos $ \todo ->
                wrapTagClass "li"
                    (jsif (jsTodoDone todo) "done" "notdone")
                    $ jsToHtml $ jsTodoText todo

We want to display how many items we have total, so we use `jslength` and `jsShowInt` to get a textual representation of the item count, and then get a widget with `textOutput`.

            countWidget <- textOutput $ jsShowInt $ jslength todos

Now we start on the interface for adding new tasks. We'll use `textInput` to get an input widget, and the `JSValue` being input.

            (taskWidget, taskValue) <- textInput

Now we'll set up a Javascript function body which will use Ajax to submit the new todo item. Notice that we don't have to explicitly pull the value from the input field; using `taskValue` automatically gets the latest value for us.

Our last parameter is the `reload` function we got earlier. This says that, each time we submit the form, we want the data to be reloaded from the server. We could theoretically do a local update here instead, but for future features it will be important to have the server-generated ID available.

            putter <- putJson TodosR (jsTodo taskValue jsFalse) reload

And now we create a submit button, which will call `putter` whenever it's clicked.

            submitWidget <- button [whamlet|Add Item|] putter

Finally, we just piece it all together:

            lift [whamlet|
    <h2>Item count: ^{countWidget}
    ^{list}
    <form>
        Enter new task: #
        ^{taskWidget}
        ^{submitWidget}
    |]

And more boilerplate...

    main :: IO ()
    main = do
        pool <- createSqlitePool "todo.db3" 5
        runSqlPool (runMigration migrateAll) pool
        toWaiApp (App pool) >>= run 3000

I'm happy to announce the first release of a new package,
[yesod-gitrepo](http://hackage.haskell.org/package/yesod-gitrepo). This package
encapsulates a pattern I've used a number of times, namely: loading and
refreshing content from a Git repository. Below is the current contents of the
`README.md` file.

This code is currently being used in production, and should be pretty stable.
That said, it has *not* received a huge amount of battle testing yet. So please
due test corner cases before shipping it in production for your site.

* * *

yesod-gitrepo provides a means of embedding content from a Git repository
inside a Yesod application. The typical workflow is:

* Use `gitRepo` to specify a repository and branch you want to work with.
* Provide a function that will perform some processing on the cloned
  repository.
* Use `grContent` in your `Handler` functions to access this parsed data.
* Embed the `GitRepo` as a subsite that can be used to force a refresh of the
  data.
* Set up a commit handler that pings that URL. On Github, this would be a
  webhook.

This is likely easiest to understand with a concrete example, so let's go meta:
here's an application that will serve this very `README.md` file. We'll start
off with language extensions and imports:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           ClassyPrelude.Yesod
import           Text.Markdown
import           Yesod.GitRepo
```

Now we're going to create our foundation datatype. We need to give it one
field: the `GitRepo` value containing our parsed content. Our content will
simply be the text inside `README.md`, wrapped up in a `Markdown` newtype for
easy rendering. This gives us:

```haskell
data App = App
    { getGitRepo :: GitRepo Markdown
    }

instance Yesod App
```

And now let's set up our routes. We need just two: a homepage, and the subsite.
Our subsite type is `GitRepo Markdown` (as given above). We replace the space
with a hyphen as an escaping mechanism inside Yesod's route syntax:

```haskell
mkYesod "App" [parseRoutes|
/ HomeR GET
/refresh RefreshR GitRepo-Markdown getGitRepo
|]
```

Next up is our home handler. We start off by getting the current content parsed
from the repository:

```haskell
getHomeR :: Handler Html
getHomeR = do
    master <- getYesod
    content <- liftIO $ grContent $ getGitRepo master
```

Then it's just some normal Hamlet code:

```haskell
    defaultLayout $ do
        setTitle "yesod-gitrepo sample"
        [whamlet|
            <p>
                <a href=@{RefreshR GitRepoRoute}>
                    Force a refresh at
                    @{RefreshR GitRepoRoute}
            <article>#{content}
        |]
```

And finally, our `main` function. We pass in the repo URL and branch name, plus
a function that, given the filepath containing the cloned repo, processes it
and generates a `Markdown` value. Finally, we provide the generated `repo`
value to our `App` constructor and run it:

```haskell
main :: IO ()
main = do
    repo <- gitRepo
        "https://github.com/snoyberg/yesod-gitrepo"
        "master"
        $ \fp -> fmap Markdown $ readFile $ fp </> "README.md"
    warp 3000 $ App repo
```

Give it a shot. You should have a webapp hosting this very README file!

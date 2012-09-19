In my [last blog
post](http://www.yesodweb.com/blog/2012/09/building-haskell-ide), I discussed
one aspect of my work with FP Complete: the goal of creating a Haskell IDE.
Since then, I've gotten lots of incredible feedback from the community, and in
particular have been in email discussion with some of the major players in the
Haskell IDE scene. I think it's safe to say that we all agree that there's
going to be a large amount of overlap in our efforts, and we will be
coordinating to try and minimize duplicated work as much as possible, while
still providing for the unique goals of each IDE project.

In response to all this, I've created a [Wiki on
Github](https://github.com/fpco/haskell-ide/wiki) to keep track of our goals,
and a [Haskell IDE Google Group](https://groups.google.com/d/forum/haskell-ide)
for discussion. I strongly recommend joining if you're interested in any more
sophisticated Haskell code editing tools.

We have a lot of topics to cover, and a single blog post won't be nearly enough
to even scratch the surface. For now, I'd like to focus on one specific
feature. I haven't chosen to start here because it's the most important
feature, but because I think it's a problem that we can solve relatively easily
and thoroughly.

## Project Templates

Most (all?) IDEs provide the concept of a project template: instead of writing
all of the code for a project from scratch, you select a template, answer a few
questions, and a bunch of files are automatically generated. We already have
this in the Haskell world: Yesod provides the project scaffolded (via the
`yesod init` command), and I believe Snap provides something like this as well.
But these are just two examples. I'm sure we could easily come up with a dozen
other possible templates: a GTK+ application, a web services client, or a
console app.

Currently, there's no standard for how this should work in the Haskell world
or, to my knowledge, in the non-Haskell world either. (If there is, please let
me know, I'd like to be able to build on existing work.) The Yesod scaffolding (and
Snap's I believe) are both generated via specialized command line tools. I'm
sure each IDE would be fully capable of building wrappers for for two tools,
but that quickly becomes an existential complexity issue. It also makes it much
more difficult for someone to start providing a new scaffolding. I know we
suffer from this already in the Yesod world, where innovation is definitely
stifled by having the One True Blessed Scaffolding.

So here are my goals for the ideal templating system:

*   A single file to represent a template. This can be some kind of archive (ZIP
    file, tarball, etc), I don't really care, but single file systems simplify
    things greatly.

*   Provide a Haskell library for both generating and consuming these
    templates. We can have a command line tool as a wrapper around the library,
    but the library should be the primary means of interacting. (You'll see this as
    a pattern as I talk more about the IDE world.)

*   Build on top of commonly used formats as much as possible. The reasoning
    here is that, even though we'll be providing a canonical Haskell library,
    not all IDEs are written in Haskell (yet). If someone is writing an IDE in
    Python and wants to provide Haskell support, we should make it as easy as
    possible.

    *   By the way, it's worth pointing out that, as described, there's nothing
        Haskell-centric about my proposal here. I've been going in the
        direction of creating language-agnostic tools and formats as much as possible
        (e.g., [`keter`](http://hackage.haskell.org/package/keter), which can host web
        apps written in *any* language).

*   I'm guessing that the most common way that people will want to actually
    provide a template is as a Git(hub)/Darcs repo. It would be great if we
    could provide a web service that takes a repo and automatically generates a
    template file. Then users of an IDE could theoretically just type in a repo URL
    to some text box and automatically get the most recent code available.

*   Similarly, we should provide a simple command-line tool that takes a folder
    and generates a template file.

## A semi-concrete proposal

As many of you know, I normally prefer to discuss actual working code/ideas
than to discuss theoretical ideas. In this case, however, I think it's worth
fleshing out the idea a bit before jumping in and implementing something. So
I'm going to lay out my proposal here, and ask for everyone's input and
recommendations before we start implementation. I recommend the discussion be
targetted at the [Haskell IDE Google
Group](https://groups.google.com/d/forum/haskell-ide) as much as possible.

For file format: let's use JSON. I'm not worried about file size: these project
template files will likely be transferred over HTTP most of the time, and
compression can be performed at that level. As for binary files, we'll
base64-encode the contents.

The JSON file needs to have three sections:

1.  Metadata describing the project template itself. This would be the name of
    the template, a description, author, homepage, and maybe a version.
    (Version could be automatically generated as the date it was created.) This is
    all pretty boring.

2.  Data that needs to be collected from the user. In the Yesod scaffolding, we
    ask for the user's name, the project name, and the database backend to use.
    The first two are (mostly) free-form text, while the third is an enumeration. I
    think we'll need to support a few basic datatypes:

    * Text, with a regex for validation.
    * Booleans
    * Enumerations

    We can also allow default values. So to model the Yesod scaffolding,
    perhaps something like this:

    ~~~json
    {"user-fields":
        [ {"name":"user-name","type":"text","validation":".+","description":"Your name"}
        , {"name":"project-name","type":"text","validation":"[\w_]+","description":"Name of your project"}
        , {"name":"database-backend","type":"enumerator","choices":
            [{"display":"MySQL","value":"mysql"},{"display":"MongoDB","value":"mongodb"}],
            "description":"Name of your project"}
        ]}
    ~~~

3.  The files that will be generated. We need to take into account some issues:

    1. Some files will be generated conditionally based on the input from the user.
    2. Some of the files will be named based on the user input (e.g., the name of the `cabal` file).
    3. The actual contents of the file will depend on the user input (e.g., the *contents* of the `cabal` file).
    4. We want to support both textual and binary files. Binary files need not have any conditional aspect to them.

    For the first issue, we'll need to have a basic expression language. I
    think equality, inequality, and, or, parantheses and variables should be
    sufficient. So to say that the file `config/postgres.yml` should only be
    generated if the database backend is postgresql, we could have something like:

    ~~~json
    {"filename":"config/postgres.yml",
     "contents":"We'll discuss this in a moment...",
     "condition":"database-backend == 'postgresql'"
    }
    ~~~

    For the conditional file naming, how about something like this:

    ~~~json
    {"filename":[{"variable":"project-name"},{"content":".cabal"}],
     "contents":"..."
    }
    ~~~

    In order to solve the third point, we'll use a combination of what we've established for points 1 and 2:

    ~~~json
    {"filename":[{"variable":"project-name"},{"content":".cabal"}],
     "contents":
        [ {"content":"name: "},
          {"variable":"project-name"},
          {"content":"...build-depends:..."},
          {"content":"\n     , postgresql-simple >= 0.3 && < 0.4","condition":"database-backend == 'postgresql'"}
        ]
    }
    ~~~

    The last one is easiest to solve: each file can have a field `encoding` which is either "text" or "base64".

    ~~~json
    {"filename":"some-image.png",
     "contents":"DEADBEEF",
     "encoding":"base64"
    }
    ~~~

Once we have the file format figured out, the library is relatively simple. Let's describe a simple consumption API:

~~~haskell
data CodeTemplate
instance FromJSON CodeTemplate

data UserInputType = UIText (Maybe Regex) | UIEnumeration [(Text, Text)] | UIBool
data UserInput = UserInput
    { uiType :: UserInputType
    , uiName :: Text
    , uiDescription :: Text
    }

userInputs :: CodeTemplate -> [UserInput]
generateFiles :: CodeTemplate -> Map Text Text -> Map FilePath LByteString
~~~

Setting up a generation API for dealing with completely static files should be
simple. It will be a bit more involved to deal with conditionals, but with
properly defined ADTs it shouldn't be too bad.

## Next steps

I think the most important next step is to determine what use cases my proposal
doesn't cover. The file creating code specifically doesn't allow many common
text generation techniques, like looping, as I simply see no use case for it,
but perhaps I'm mistaken. I'm also curious to hear what other ideas people have
for project templates.


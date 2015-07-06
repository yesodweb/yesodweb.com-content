# Announcing yesod-table

Over the last two years, I've seen the need for safe dynamic table-building
in half a dozen yesod projects I've worked on. After several design iterations, 
the result of this experience is [yesod-table](https://hackage.haskell.org/package/yesod-table), 
which saw its first stable release last week. This blog post will contain code excerpts,
but you can also look at the [documentation](https://hackage.haskell.org/package/yesod-table-1.0.0/docs/Yesod-Table.html)
the [full example app on github](https://github.com/andrewthad/yesod-table/blob/master/example/main.hs),
which can be compiled and run.

## Naive Solution

Before getting into specifics about yesod-table, I want to take a look at the naive 
table-building strategy and identify the common pitfalls. Let's say that you have a data 
types `Color` and `Person`:

    data Color = Red | Green | Blue | Purple
      deriving (Show)
    data Person = Person
      { firstName     :: Text
      , lastName      :: Text
      , age           :: Int
      , favoriteColor :: Color
      }

We have a list of `Person` (let's call it `people`), and we want to show them all in a 
table. You could write out a hamlet file like this:

    <table>
      <thead>
        <tr>
          <th>First Name</th>
          <th>Last Name</th>
          <th>Age</th>
      <tbody>
        $forall p <- people
          <tr>
            <td>#{firstName p}
            <td>#{lastName p}
            <td>#{show (age p)}

And there it is. This is the simplest solution to building a table. In fact, if you've worked
on web applications for any length of time, you've probably written code like this
before. I've implemented this pattern in PHP+html, in rails+haml, and in yesod+hamlet
projects. And every time, it is unsatisfactory.

## Problems With Naive Solution

Let's take a look at three reasons why this solution leaves us wanting something more:
* **Duplication**. After building a few tables this way, you realize that you are
  copying the HTML elements and the list iteration (`$forall`) every time.
* **Non-composability**. If I want to build a similar table, one that shows the 
  same fields but additionally has a column for `favoriteColor`, I have to copy
  the whole thing. I can't glue another piece onto the end.
* **Breakable Invariant**. If we do decide to add a `favoriteColor` column, we might 
  try simply adding `<td>#{show (favoriteColor p)}` to the end. This would cause incorrect 
  behavior at runtime, because we would have forgotten to add 
  `<th>Favorite Color` to the table header. The problem is that we have an invariant 
  not captured by the type system: `thead` and the `tbody` loop must have the same
  number of `<th>`/`<td>` elements, and the order must match.

In particular, the last issue (breakable invariant) has been a source of great pains to me before. 
On a three-column table, you are less likely to forget the `<th>` or put it in the wrong place.
As the table gets larger though (six or more columns), these mistakes become easier to make, and it's
harder to be sure that you did the right thing until you see it at runtime.

## Example with yesod-table

So let's take a look at how yesod-table addresses these issues. The module it provides 
should be imported as follows:

    import Yesod.Table (Table)
    import qualified Yesod.Table as Table

Let's build the same table we saw earlier:

    peopleBasicInfoTable :: Table site Person
    peopleBasicInfoTable = mempty
      <> Table.text   "First Name" firstName
      <> Table.text   "Last Name"  lastName
      <> Table.string "Age"        (show . age)

And then we can feed it data and render it with `buildBootstrap`:

    -- Although it's called buildBootstrap, it builds a table just fine
    -- if you aren't using bootstrap. It just adds bootstrap's table classes.
    getExamplePeopleR = defaultLayout $ Table.buildBootstrap peopleTable people

## Explanation of Internals

The key to this approach is looking at a table pattern (called a `Table` in this library)
as a collection of columns, not a collection of rows. From the yesod-table source, we have:

    newtype Table site a = Table (Seq (Column site a))
      deriving (Monoid)
    
    data Column site a = Column
      { header :: !(WidgetT site IO ())
      , cell :: !(a -> WidgetT site IO ()) 
      }

Each column is just the content that will go in the `<th>` (the value of `header`) and 
a function that, given the data for a table row, will produce the content that belongs 
in the `<td>`. A table is trivially a collection of columns and gets a `Monoid`
instance from `Seq` for free (for those unfamiliar, `Seq a` is like `[a]` but with
different performance characteristics). Consequently, any two `Table`s that are 
parameterized over the same types can be concatenated. As a final note of explanation,
the `Table.text` function that we saw above just a helper for building singleton 
tables. So, the three `Table`s below are equivalant:

    import qualified Data.Sequence as Seq
    import qualified Data.Text as Text
    -- These three generate a single-column table that displays the age.
    reallyEasyToReadTable, easyToReadTable, hardToReadTable :: Table site Person
    reallyEasyToReadTable = Table.int "Age" age
    easyToReadTable = Table.text "Age" (Text.pack . show . age)
    hardToReadTable = Table.Table $ Seq.singleton $ Table.Column 
      (toWidget $ toHtml "Age")
      (toWidget . toHtml . show . age)

As should be clear, the convenience functions for singleton `Table`s should always be
preferred.

## How Is This Better?

Now to address the most important question: Why is this better than what we had earlier?
Firstly, consider the issue of the breakable invariant. This is now a non-issue. Imagine
that we modified the earlier table to show a person's favorite color as well:

    peopleFullInfoTable1 :: Table site Person
    peopleFullInfoTable1 = mempty
      <> Table.text   "First Name"     firstName
      <> Table.text   "Last Name"      lastName
      <> Table.text   "Age"            (show . age)
      <> Table.string "Favorite Color" (show . favoriteColor)

The table is correct by construction. You cannot forget the column header because 
it's part of the `Column` data type. You're less likely to make this mistake, because
now that information is directly beside the content-extracting function, but even 
if you somehow typed this instead, you would get a compile-time error:

      <> Table.string (show . favoriteColor)

Secondly, we can look at duplication. All the
table-rendering logic is moved into `buildBootstrap` (and you can write you're own 
table renderer if that one is not satisfactory). The `Table` that we are using now
has neither the HTML elements nor the list iteration that we dealt with earlier.

Finally, we can look at composability. As an alternative way of adding the column
for a person's favorite color, we could write:

    peopleFullInfoTable2 :: Table site Person
    peopleFullInfoTable2 = mempty
      <> peopleBasicInfoTable
      <> Table.string "Favorite Color" (show . favoriteColor)

Additionally, if we need to promote this `Table` to work on something 
like `Entity Person` (if it was backed by `persistent`), we could do this:

    -- You need to use ekmett's contravariant package
    peopleEntityFullInfoTable :: Table site (Entity Person)
    peopleEntityFullInfoTable = contramap entityVal peopleFullInfoTable2

I won't go into contravariant functors here, but it's a very useful pattern
for working with `Table`s. The astute reader will notice 
that the monoidal composition pattern shown earlier means that we can only append
or prepend columns. We cannot inject them into the middle. I'll give 
yesod-table a B minus on to composability objective.

## Closing Notes and Acknowledgements

One final closing note. You may have noticed that all of the `Table`s in this 
post were parameterized over `site`. This is because they don't depend on 
a particular foundation type. Usually, the way that this can happen is that 
you use a route in one of the columns:

    -- Assume that our foundation type was named App
    peopleTableWithLink :: Table App (Entity Person)
    peopleTableWithLink = mempty
      <> peopleEntityFullInfoTable
      <> Table.linked "Profile Page" (const "View") (PersonProfileR . entityKey)

The above example must be parameterized over `App` (or whatever your foundation
type is named), not over `site`.

This monoidal approach to building tables was inspired by Gabriel Gonzalez's 
[Equational Reasoning at Scale](http://www.haskellforall.com/2014/07/equational-reasoning-at-scale.html)
and by the classic [diagrams paper](http://www.cis.upenn.edu/~byorgey/pub/monoid-pearl.pdf).
I hope that others find the library useful. I am very open to pull requests and suggestions, so if 
you have an idea for a convenience function, feel free to open up an issue on 
[the github page](https://github.com/andrewthad/yesod-table).



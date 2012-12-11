tl;dr: There's an update to Persistent's type generation which greatly
simplifies error messages at the expense of losing some not-often-needed
generality. You can turn this generality on or off with a simple boolean flag,
so if you want the full power, it's available. For most users, I recommend
turning it off as described below. As an example of error message
simplification, compare:

      Actual type: GHandler
                     sub0
                     master0
                     (Maybe
                        (Entity (UserGeneric Database.Persist.GenericSql.Raw.SqlBackend)))
                   -> (Maybe
                         (Entity (UserGeneric Database.Persist.GenericSql.Raw.SqlBackend))
                       -> GHandler
                            sub0
                            master0
                            (Maybe
                               (Key (UserGeneric Database.Persist.GenericSql.Raw.SqlBackend))))
                   -> GHandler
                        sub0
                        master0
                        (Maybe
                           (Key (UserGeneric Database.Persist.GenericSql.Raw.SqlBackend)))

Using the new simplified types, this turns into:

      Actual type: GHandler sub0 master0 (Maybe (Entity User))
                   -> (Maybe (Entity User)
                       -> GHandler sub0 master0 (Maybe (Key User)))
                   -> GHandler sub0 master0 (Maybe (Key User))

## The problem: multi-backend datatypes

(If you just want to make your life easier and don't care about the details,
feel free to skip to the next section.)

Persistent works with multiple database backends. One issue that comes into play
for this is textual serialization of database keys. In the case of our SQL
backends, we have simple numerical keys. So any piece of text that entirely
consists of digits could be a valid key. On the other hand, MongoDB has a much
more specific key format.

The solution to this was to parameterize database keys on the backend they
applied to, in addition to the data type they apply to. In other words, we
have:

    data Key backend entity

You can see more about this motivation [in an email I wrote over a year
ago](https://groups.google.com/d/topic/yesodweb/rV_EnThsyK0/discussion). Now
suppose you want to embed a reference to a different entity in a current one,
e.g.:

    Person
        name Text
    Car
        owner PersonId
        make Text
        model Text

`PersonId` gets converted to `Key backend Person`... but how do we know what
`backend` is? Up until now, Persistent's solution to this is to create a
separate `CarGeneric` datatype which is parameterized on `backend`, and to
create a helper `Car` synonym that uses the backend specified in your
`MkPersistSettings`. In other words, you get:

    data PersonGeneric backend = Person Text
    type Person = PersonGeneric SqlBackend
    data CarGeneric backend = Car (Key backend (PersonGeneric backend)) Text Text
    type Car = CarGeneric SqlBackend

This lets you keep some generic data types in case you want to store your data
in multiple backends (like PostgreSQL and MongoDB). However, it results in much
more complicated error messages. And given that this is such a corner use case,
this solution was- in retrospect- a bad tradeoff.

So now, if you specify `mpsGeneric` as `False` (or use `sqlOnlySettings`), the
generic datatypes won't be created. Instead, you'd just get:

    data Person = Person Text
    data Car = Car (Key SqlBackend Person) Text Text

If you're only using a single database backend, this should introduce no downsides.

## Start using the update

You basically need to [make two
changes](https://github.com/yesodweb/yesod-scaffold/commit/95e08a7afbbbe210432b955ef412a8e3282767de)
to your site to use this new feature:

1. Depend on `persistent-template` 1.1.1 or greater.

2.  In `Model.hs`, change `sqlSettings` to `sqlOnlySettings` (or the [equivalent
    for
    Mongo](https://github.com/yesodweb/yesod-scaffold/commit/90c3776e011eadbe88cf8a40f0b8555534457f66)).

Note that you'll need to be using `persistent` 1.1.

If you're a library author creating reusable components (e.g., an admin
interface), you should stick with the generic datatypes to avoid tying down to
a single backend. But for most standard applications, I highly recommend the
change.

## Future work

I'm hoping this is just one step of many for simplifying error messages in the
Yesod ecosystem. We've done a lot of that in the past, but [I have some new
ideas](https://plus.google.com/u/0/116553865628071717889/posts/AgCBx2SjPwD).
If you want to have a look at a concrete idea, you can look at [an experimental
typeclass-based conduit
library](https://github.com/snoyberg/classy-prelude/blob/master/classy-prelude-conduit/Data/Conduit/Classy.hs),
based around [an idea I saw from Chris Smith a while
ago](https://github.com/cdsmith/my-pipes/blob/master/Pipes.hs).

I'm hoping to write separate blog posts about thoughts on conduit and classy-prelude, but I'll save that for another time.

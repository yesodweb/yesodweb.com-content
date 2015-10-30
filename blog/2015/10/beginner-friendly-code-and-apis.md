I just flew from Tel Aviv to Los Angeles. It's a long flight, and includes
significant travel time outside of the flight itself. There are two results of
that: (1) I've slept something like 8 hours in the past 72, and (2) I've had a
lot of time to play with random code sitting on my laptop and think about it
from different perspectives. I'm going to talk about (2), and use (1) as an
excuse for anything ridiculous I say. Note that this blog post is just
exploring ideas, not giving any real statements.

The following two pieces of code are identical in functionality:

```haskell
someFunc1 key =
    case lookup key someMap of
        Nothing -> getDefaultValue
        Just value -> otherFunc value

someFunc2 key = maybe getDefaultValue otherFunc . flip lookup someMap
```

Which is better? I'll start with objective facts from my own experience:

1. When I was a beginner Haskeller, I would have been able to write something
   like `someFunc1`
2. When I was a beginner Haskeller, I would have had to try and puzzle out
   someFunc2` for a long time if I read it, and almost certainly couldn't have
   written it
3. Past the beginner phase, reading code like `someFunc2` taught me new ways to
   compose Haskell code easily
4. As an intermediate Haskeller, I definitely got the feeling that I should be
   writing my code in `someFunc2` style (point-free, avoid pattern matching,
   use function composition)
5. I seem to receive pull requests on a regular (though not frequent) basis
   rewriting code in the style of `someFunc1` to look like `someFunc2`
6. I have a gut reaction that `someFunc2` is better
7. `someFunc2` is shorter
8. And yet, to this date, I think I can still parse and understand `someFunc1`
   more easily, and also believe I will more quickly spot a bug in that style

There's quite a few "I got the feeling" statements above. Those are *objective*
statements about my *subjective* observations; I'd be really intersted in
whether other people have had the same kinds of observations over time, or if
my experience is unique. But now, the truly subjective/exploratory part:

It seems like my progression as a Haskeller results in forcing myself to write
in a harder-to-parse style to make my code shorter, to satisfy some base need
for "better" code, even though by most measurements I just made, the
longer/explicit/pattern matching code is in fact better.

There are certainly advantages to point-free style though, right? Some more
complicated combinators - like `foldr` - result in clearer code, plus code that
generalizes to more data types than just a list (thanks to `Foldable`). In some
cases (like stream fusion and other rewrite rules) point-free style may be more
efficient; I know that in conduit `await >>= maybe f g` is more efficient than
pattern matching.

To try and generalize my point beyond point-free style: we've been having some
discussions about more code sharing in the web framework space recently. One
point that's come up is a difference between Servant and Yesod regarding
explicitness of parameters. For example, in Yesod (and I think other WAI
frameworks), there's a function available inside your handler code to lookup
request headers. In Servant, a dependency on such a piece of data is explicit
at the type level. (There are similar issues around short-circuiting requests,
and explicitness of different content representations.)

My 5-year-more-Haskell-experienced self is very much intrigued by the Servant
approach. It seems more sound. And yet, I still interact regularly with less
experienced Haskellers. And just this past week, this kind of need for explicit
declaration of all data came up in practice for a customer, and resulted in
Haskell being a harder adoption for them.

I'm feeling the same tension again. The Yesod API seems to appeal to what the
beginner would expect at a straightforward level: if you want to send a
redirect, you just call the `redirect` function, and don't need to worry about
changing type signatures around. Need a request header? Ask for it. However, I
know that being explicit about these things gives great benefit (in Servant's
case, it has the ability to generate API bindings that Yesod doesn't have). But
even so, today when I write a web app in Haskell, I _still_ like the ability to
use the "beginner-friendly" API without the explicitness.

This is where I'm stuck. There are clearly benefits to each kind of approach in
these dichotomies. And I'm sure there are more dichotomies than I've listed.
The kind of questions I've been pondering are:

1. If I would design an API differently today than in the past, based on my
   larger Haskell experience, what does that mean? Is the new API better? Or
   have I lost touch with what's most useful, and am instead chasing an less
   important ideal?
2. Is there an inherent tension between beginner-friendly and expert-friendly
   APIs for some of these things, and are the two groups best served by
   separating out the APIs?
3. When do I give in to the voice that says "you could rewrite that to be
   cleaner" and when do I say "no, the simple code is better, don't complicate
   it in the name of cleanliness."

Sorry for the mostly unintelligible rambling. In the interest of sometimes
listening to the less experienced person inside of me, I'm posting this without
much review, in case I'm actually hitting on an important topic. If not, please
ignore me.

And in case anyone's curious, a lot of this came up when working on
[wai-frontend-monad](https://github.com/yesodweb/wai/tree/wai-frontend-monad/wai-frontend-monad),
which attempts to extract the `HandlerT` transformer from Yesod into something
more generally useful, and in a more modern coding style. That work (combined
with lack of sleep on a plane) seems to have opened up some existential
dilemmas ;).

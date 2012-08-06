I'm going to be giving a [webinar with
O'Reilly](http://oreillynet.com/pub/e/2351) this Thursday (10 AM Pacific)
titled "Designing Type-Safe Haskell APIs." If the topic interests you, please
sign up. I'd love to see you all
there (figuratively speaking, of course).

The talk itself will mostly focus on actual code and techniques. For timing
reasons, I'm leaving out most of the "motivation" section, explaining __why__
we should care about designing with type safety in mind. So I'm going to try
and give that part of the talk right here.

My focus for motivation is developers in the real world writing real software.
There are many other use cases where type safety has even more obvious
benefits. If you want to prove correctness of your code, for example, strong
typing is essential. But for our purposes, the audience is normal development
projects: putting together some web site, writing a tool to process data,
manipulating XML files, etc.

In other words, I'm looking at the realm where people are writing code in Java,
Python, Ruby, and a slew of other languages. The question is: why should we
bother with type safety and Haskell? What practical advantage does it have over
these other languages?

The first thing to recognize is that *type safety isn't a feature in and of
itself*. Imagine you're pitching a new product to some company, or trying to
convince your employer that a new Haskell-based tool will be a great
investment. They say, "Why? What does it provide over our existing product?" If
your response is "It's type safe," you're not going to be convincing anyone.

In other words, __type safety is a means to an end__.

So what is that end? Well, let's look at what it does. A simple explanation is
that type safety moves runtime bugs to compile time errors. (I'll cover this
concept more in the talk.) But again, such a claim won't sell your software. So
this is *also* just a means to an end. And that end is __minimizing bugs in
code__.

Minimizing bugs is a feature that you *can* sell people on. If I'm comparing
product A and product B, and I have some reason to believe product B will have
less bugs, I'm very likely to buy it. It can win out over other aspects of the
product, such as cost (do you really want to pay for bugs?), ease-of-use (it sure
was easy to generate that invalid output), or extra capabilities (hey look,
this product can do *50* things badly).

But now that we've narrowed in on our main goal (minimizing bugs), we have to
also acknowledge that neither type safety nor Haskell hold a monopoly here.
That are a large number of techniques for minimizing bugs in code: unit
testing, static analysis, quality assurance (QA), and many others. So we have a
new question: why is type safety such a great tool for minimizing bugs?

For one, type checking is run automatically. Excluding abuses of unsafe
functions, you can't compile Haskell code that doesn't type check. On the other
hand, you can easily write, run, and deploy code that fails unit tests. But
that's really more of a process problem. The real issue is if you're relying on
QA to catch all your bugs. The QA process is time-consuming and expensive. Type
checking is cheap.

But type checking has another major advantage. It covers your entire program
automatically. Any invariant enforced by the type system is guaranteed to be
correct throughout your codebase. Compare this with unit tests, where you need
to write a unit test for each and every place where something could go wrong.
And even so, for the invariants it enforces, the type system will give total
coverage, versus unit tests which will only cover the specific cases you've
included.

A common rebuttal to that last point is that the type system can't really
enforce important things. I've seen people claim things like, "I haven't had
any bugs in the past X months that could have been caught by the type system."
My response to that is, you're using the wrong type system. Trying to judge the
power of type safety based on the incredibly weak type system provided by Java
and similar languages misses out on a huge number of possibilities. I'll be
giving a bunch of examples on Thursday of invariants you can enforce in the
type system itself. A small example is using types to avoid cross-site
scripting (XSS) attacks. This is a bug that exists in a large number of
websites, and is difficult to track down. With strong typing, we can make it
almost disappear.

Coming back to our type safety sales pitch: I'll claim that in many cases, type
safe design is the best way to minimize bugs. However, we can't fool ourselves
into believing it's the only technique available. There are times when unit
tests, static analysis, and QA will give better results than type safety. (Just
look at the Yesod codebase, we have huge test suites in addition to our
extensive usage of type safety.) In some cases, trying to enforce an invariant
in the type system will make code more difficult to work with.

And that's where we need to start looking at a cost/benefit analysis. Type
safety is a great powerful tool, but it *does* have some costs. In most cases,
with proper usage and proper training, I think that type safety's benefits far
outweigh its costs. The additional setup costs for designing the type safe
system will not only drastically simplify maintenance of a project, but even
during the initial development phase, I believe that those costs amortize. In
all but the simplest of programs, I think type safety will pay for itself
before you make your first alpha release.

But that argument only applies to languages that make it easy and convenient to
write type-safe code. Haskell and a few other languages excel at this:
declaring new datatypes is concise, pattern matching is built in, the compiler
can catch things like unhandled cases, and so on. Other languages do not
provide such power in an easy-to-access fashion. And in those cases, the sell
for type safety is harder. I strongly believe that a reason we're seeing people
moving to dynamically typed languages is because the popular statically typed
languages have given static typing such a bad name.

I know that was quite wordy (which is why I won't be saying it on Thursday),
but let me try to sum up the major points:

* Type safety is a great tool for minimizing bugs.

* Type safety is one tool of many for that job.

* There are distinct advantages for type safety, e.g., always run, full coverage.

* Languages like Haskell make it cheaper to take full advantage of type safety.

* Even in Haskell, type safety is not sufficient to handle all cases.

Enough for the introduction, let's discuss the actual techniques at the webinar
on Thursday.

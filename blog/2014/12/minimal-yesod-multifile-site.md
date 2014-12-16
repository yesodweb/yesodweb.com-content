I was talking with a coworker yesterday who was uncertain of how to start on a
Yesod application without using the scaffolding. A single-file application is
easy, and is [well
documented](http://www.yesodweb.com/book/basics#basics_hello_world). The
question is: how do you make a trivial multi-file site, where the handler
functions can live in individual modules and still use type-safe URLs?

The problem in achieving this is separating out the data type creation from the
dispatch generation. This is also [discussed in the
book](http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules),
but what seems to be lacking is a simple cookbook-style example. So here it is!

<script src="https://gist.github.com/snoyberg/07d61088a1376ff3deab.js"></script>

This example serves a very minimal site (actually, it even has a JSON service),
but can easily be extended to support more complex things. This is a good basis
for people looking to implement minimal services without the need for things in
the scaffolding like authentication, configurable logging, etc.

Also, if anyone's interested, it would be possible to add this as one of the
scaffolding options from `yesod init`.

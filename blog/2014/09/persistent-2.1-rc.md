We are happy to announce a stable persistent 2 release candidate.

We previously [announced an unstable release of persistent 2](http://www.yesodweb.com/blog/2014/08/announcing-persistent-2). It was a good idea to call it unstable because some commenters pointed out that we were not exposing the full power of the new flexible Key type. This lead to a couple of breaking releases that organizied the internal types of persistent. All of these are on the unstable 2.0.x series.

persistent-2.0.3.* is on hackage now. We consider this a release candidate that will be promoted to persistent-2.1. We may wait until the haddocks build on hackage before releasing.


# Ongoing persistent maintainership

Persistent is of huge importance to the Hasekll community, playing the role of default ORM.
The project has benefited immensely from the community involvement.
We get a lot of prompt bug reports that often include fixes.
And there have been many great new features added by contributors.

However, persistent it is lacking in dedicated maintainers for each backend. We would like for Michael to keep doing a great job stewarding the project, but for others to step up and help own a SQL backend.

An extreme example of a lack of backend maitenance is when we received a pull request for a CouchDB backend. It was great to share the code as a starting point, but it was already using an older version of persistent and is now sitting in the experimental folder in a bit-rotted state.

In general a persistent backend can only be first class and in our tree with a dedicated maintainer.  
Michael and I maintain persistent and persistent-template. I maintain the persitent-mongoDB backend. The issue now is more with the SQL backends, where the maitenance and development for them is being pushed on Michael. For example, I implemented custom Key types in persistent, persistent-template, and persistent-mongoDB. Michael and myself implemented them for persistent-sqlite, but it still needs to be implement for persistent-postgressql and persistent-mysql.

Maintaining persitent and persitent-template has had a questionable cost/benefit ratio for me. But I have personally found that maintaing the persistent-mongoDB backend has paid off well for me.
I need to have a good understanding of what is happening with my code that deals with the database. Rather than treating it as a black box I make continous incremental improvements to the library that I rely on, and I can smoothly get code onto hackage rather than having local modifications.

Let us know if you are interested in helping to maintain a backend.

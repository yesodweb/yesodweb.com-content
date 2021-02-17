Three years ago, I met fellow Haskeller Brian Hurt while working with Obsidian Systems on a Reflex project. Not long after, he started telling me his ideas about how to fix social media. These ideas intrigued me, one thing led to another, and we began building ChatWisely. We thought other Haskellers might like to hear about what we’re doing and how we use Haskell to do it.

ChatWisely is a member-supported mini-blogging social network currently in open beta. We envision a place where people connect in a spirit of comradery. We see an elevated discourse and a way to show bullies the door by providing a platform to debate safely. Here’s how we’re doing it.  


<h2 align=center>Safety First</h2>
Brian and I built several mechanisms to filter out people looking for a fight or to harm others. First and foremost will be the monthly subscription fee of one dollar. We think that will discourage a large portion of toxic people. Another is a sharable mute list that we believe will help mitigate the rest. And finally, in the event of a serious Terms of Service violation, we ban payment methods rather than just accounts. 

<h2 align=center>Mini-Blogging</h2>
The big idea here is that sometimes, a short post isn’t enough. Brian and I made a way to link that short post to a longer one. So the timeline looks something like Twitter’s, but with some posts that can expand to something more detailed. These can be connected to other people’s posts to create a continuity in conversation hard to come by on other platforms. So when another member’s post inspires you to write a longer one about your experience with the ghcjs ffi (for example), you can link your post to theirs.

<h2 align=center>Ownership of Your Timeline</h2>
Members can organize their timeline and choose to what extent they follow other people’s posts. The typical mainstream social network requires that when you follow someone you must follow everything they post, or nothing. Sure, there are filtered word lists in some cases. But none of it seems to work quite right. Instead, we have groups called caboodles that members can use to decide where other people’s posts fit, and how to share their own. So say someone likes their uncle’s cookie recipes but not his political posts. They can follow one but not the other.

<h2 align=center>Geolocated Messaging</h2>
One day this pandemic will be over and we’ll be there to meet that day. At that time, when a member’s movie caboodle wants to organize local screenings of the latest blockbuster from the House of Mouse they can make posts visible to people in their proximity. Perhaps you want to target local Haskellers to organize a meetup, or leave them a message that pops up when they’ve found the meeting place. Also, I think running a scavenger hunt with geolocated clues sounds like a hoot.

<h2 align=center>How It’s Built</h2>
Brian and I rely heavily on the Haskell ecosystem to build ChatWisely. Haskell’s type system reduces errors and cognitive load. GHC is our pair programming partner that tightens the delivery cycle and lets us learn how to build ChatWisely while we’re building it. Refactoring is a breeze, and unit testing is constrained to the I/O edges of the system, which means we spend less time on that and more time building the product. Here’s the principal tools we rely on to get the job done.  
<br/> <br/>
<h4> Ghcjs </h4> The fact is we all hate javascript. The problem is, we can’t build web apps without it. Ghcjs lets us deal with the realities of building a product that uses a web browser for a client. The FFI lets us wrap our hand-written javascript in Haskell which helps to keep that part of the codebase pretty small.. We especially love what is built on top of that, Reflex-Dom.
  
<h4> Reflex-Dom </h4> Reflex helps us build a system that needs to adapt to changes in data, requirements and platform. We’re learning how to build ChatWisely as we build it, and reflex keeps up with our changing ideas on how to do that. Our first app store product will be a PWA, delivered with reflex.

<h4> Servant </h4> Servant delivers the API, and requires us to separate definition from implementation. This helps us keep the backend from turning into a big ball of mud. We can auto-generate clients, which we currently use in unit testing. They even have a way to generate a reflex client, and we’ll be adding that in the near future.

<h4>Yesod</h4> We use Yesod for marketing tasks, which largely require static pages. Currently it handles our landing page and unsubscribe mechanism for our emails. The Yesod Widget is a monoid, and therefore composable, which makes structuring html simple. 


<h2 align=center>Three Reasons Why People Should Use ChatWisely.</h2>
<h4>We are member supported</h4> We won’t have ads, which means we have no need to manipulate people’s timelines in order to serve those ads. Their timeline should be about conversations of interest.

<h4>We solve the tweet thread problem</h4>Brian and I find tweet threads hard to follow. Our mini-blog looks like twitter in the sense that you get a timeline of short posts. However if a post is the beginning of something more developed, that message can open up to access it.

<h4>Keep the RealWorld conversation going</h4>We have delayed the development of these features for obvious reasons. But one day we’ll be together again. By then we’ll have useful geo-location tools for conference attendees and speakers to continue the conversation.

What makes a weekend conference fun for me are the conversations in-between formal talks. I get all caught up in compelling conversations, and want to keep that going. We’ll have a way to do that, without having to know or remember anyone’s email address or phone number.

Conference speakers will often want to build on the momentum gathered after a successful talk. Brian and I think Twitter hashtags are the terrible but often only way to do this. We’ll have a way to use proximity and common interests to help build that momentum and keep everyone engaged.

---
We built ChatWisely as a response to the unpleasantness all too common on mainstream social networks. Depending on our membership for support creates the place we want to meet because ad revenue and data-mining motivates engagement, not conversations and connection. No one is fooled by what mainstream social networks call engagement because that looks to us like derailed conversations, confusing timelines we only have a shallow control over, and unsafe situations.

Brian and I love the daily experience of building ChatWisely, the Haskell ecosystem brings joy to the experience of running our startup. You can support us on [patreon](https://www.patreon.com/chatwisely) and should come by and test the [beta](https://chatwisely.com/i/messages). We look forward to hearing from you about any ideas or questions you may have.


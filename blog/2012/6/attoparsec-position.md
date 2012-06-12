I think for many of us, attoparsec has supplanted Parsec for most parsing. Especially with its new(-ish) `text` support, it's a great option for almost all parsing needs. Except position information.

My motivating example was `xml-conduit`: this is a library I use at work a lot, and is in fact part of tools that are shipped to many of our clients. It's quite awkward that in the case of parse errors, there's no indication of where in the file the parse error occurred.

Adding position information to attoparsec itself is a non-starter. I don't know the internals myself, but Bryan says that it would introduce too much performance overhead. So the question is: can we get this position information *without* changing attoparsec? The answer is a qualified yes.

Continuing with xml-conduit: It has an attoparsec parser which parses individual *tokens*. A token would be something like an element beginning, or a processing instruction, or raw content. Let's take the case of an element beginning. We don't care about the position information of the individual components of the token (e.g., where an attribute starts); we only want to tell the user where the token *started*, and maybe where it ended.

This drastically simplifies the problem. All I need to do is count up the lines and columns leading up to this token. And to do that, all I need to know are how many lines/columns were consumed by all the previous tokens.

To address this, I've made some modifications to the devel branch of attoparsec-conduit. There is now a function called [sinkParserPos](https://github.com/snoyberg/conduit/blob/d62cdffcddd6ad01eea2c9b0fe568489c4e77dbf/attoparsec-conduit/Data/Conduit/Attoparsec.hs#L129) which takes an initial position and a parser, and returns the updated position and the parsed result. Internally, it counts up lines and columns from each chunk of data it sends to attoparsec, with special handling for parse completion (we don't want to count unconsumed data).

On top of this, I've built a very simple [conduitParserPos](https://github.com/snoyberg/conduit/blob/d62cdffcddd6ad01eea2c9b0fe568489c4e77dbf/attoparsec-conduit/Data/Conduit/Attoparsec.hs#L116). This function will repeatedly apply a parser to an input stream and produce a stream of parsed values, until the input stream is exhausted. However, the output stream contains both the parsed value *and* the start and end position (aka [PositionRange](https://github.com/snoyberg/conduit/blob/d62cdffcddd6ad01eea2c9b0fe568489c4e77dbf/attoparsec-conduit/Data/Conduit/Attoparsec.hs#L53)) of the parsed value.

<p>In <code>xml-conduit</code>, I just made changes to leverage <code>conduitParserPos</code> and thread these position values a little bit later into the parse stream. This means we get nice position information for both parse errors (e.g., <code>&lt;foo&lt;</code>) and for non-well-formed XML (e.g., <code>&lt;a>&lt;b>&lt;/a></code>).</p>

Although I implemented the technique in attoparsec-conduit, there's nothing conduit-specific about it. Anyone can layer this technique on top of attoparsec.

It's not a panacea: we couldn't get nice position information for Hamlet with this approach, for example, since Hamlet parses the entire document as a single entity, not as a stream of tokens. But for those cases where the approach works, it lets you keep using attoparsec, but giving slightly more human-friendly error messages.

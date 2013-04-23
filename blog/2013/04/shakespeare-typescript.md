We are very excited about the upcoming Yesod 1.2 release, but we should mention other improvements in the Yesod ecosystem. There are some greate updates to shakespeare-js are available today.

* Dramatically faster template reloads
* Support for TypeScript on unix
* Roy integration supports variable interpolation

Shakespeare is a family of compile time templates that is used for HTML, CSS, Javascript, and text. Javascript templates come from the shakespeare-js template. Variables holding JSON content can be inserted into the templates. The templates can be compiled by invoking external compilers: this has allowed us to support CoffeeScript templates for a long time now.


## Faster template reloads

In production code, shakespeare compiles everything once to static strings awaiting some variable insertions. In developement mode, it is preferred to use the shakespeare functions that automatically reload their file contents such as coffeeFileReload. This means that on every web request the file will be re-parsed in case it changed. This works great, but when using an external service such as coffee-script, there is a noticeable lag to shell out to the compiler. So the reload functions now cache the file contents in memory and only reload them when the modification time of the file changes.


## TypeScript support on unix

TypeScript is now supported on unix in the same way that CoffeeScript has been. We should be able to make it work on Windows if there is interest.

TypeScript is a JavaScript superset that is easy to start using. TypeScript has the backing of Microsoft, which may actually be hindering developer uptake in the non-Microsoft communities, but if you judge TypeScript by what it is, I think they have come up with something fantastic. The TypeScript superset strictly tracks the latest JavaScript standards and compiles down to an older version of Javascript runnable by all browsers.

What TypeScript has added which is not in the Javascript specs is types. By default TypeScript is dynamically typed like JavaScript and will run most Javascript code without a few small additions like declarations of global variables. But you can add in types wherever you want and gradually make your code more type-safe.

The biggest problem with TypeScript is the lack of type inference and presence of null. Coming from Haskell you might expect the type to be inferred when it is obvious to you that it could be, but it will often end up just being a dynamic type unless you write down a type annotation for every function. However, you still may be able to achieve some strong typing with less effort if you are leveraging existing type declaration files for the frameworks you are using, like these: https://github.com/borisyankov/DefinitelyTyped

Personally I have been moving all my CoffeeScript code to TypeScript. CoffeeScript turns Javascript into a pretty good dynamic language, but I need more than that to write reliable software (without spending an enormous effort on writing tests). I definitely miss the comprehensions in CoffeeScript, but I can use lodash.js in its place. It helps that some of the CoffeeScript features are making their way into the JavaScript standard and are available in TypeScript.

Knowledge of TypeScript, like CoffeeScript, is a skill that is portable outside of the Haskell community.


## Roy variable interpretation

Roy templates now support variable interpolation.

[Roy](http://roy.brianmckenna.org/) is a melding of functional programming to the existing Javascript semantics. It has never got a strong uptake due to the project lacking polish and documentation, but that keeps improving and I know there are some happy Haskell users of Roy now.


## Javascript polyglot

Fay will compile a Haskell subset directly to Javascript, and is a popular option in the Haskell community. [yesod-fay](http://hackage.haskell.org/package/yesod-fay) uses shakespeare-js and is also being kept up to date with the 1.2 changes.

[Elm](http://elm-lang.org/) is another option that tackles both type-safety and GUI. The [elm-yesod](http://hackage.haskell.org/package/elm-yesod) package uses shakespeare-js and integrates Elm as a widget.

Of course you can use whatever you want on the client side. Those mentioned here have existing packages for easy integration with Yesod. ghcjs is continually being improved and is another option for compiling Haskell (and the only option for Haskell using more than the Fay subset), and there are examples available for usage with Yesod.

There is nothing stopping you from mixing and matching the different available approaches: TypeScript for code that is leveraging javascript frameworks, Fay for sharing server-side code, and Roy/Fay/Elm for strongly typed client-side code.

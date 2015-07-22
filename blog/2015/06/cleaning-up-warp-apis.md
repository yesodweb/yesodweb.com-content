For the last one and a half years, I have been trying to implement [HTTP/2](https://tools.ietf.org/html/rfc7540) in Warp.
Since both HTTP/2 implementations of Firefox and Chrome requires TLS for HTTP/2,
I'm also trying to improve the performance of WarpTLS.
In the process, I need to change the `Connection` data type.
I felt nervous a bit because `Connection` is exported from the top module, `Network.Wai.Handler.Warp`.
I believe there are only two users of this data type: Warp itself and WarpTLS.
Normal users do not use it.
So, `Connection` should be exported from the `Internal` module.
This motivated me to clean up the Warp APIs.

The APIs of Warp 3.0 has the following issues:

- The top module exports many internal APIs which are prone to change.
- The `Internal` module does not export enough internal APIs.

Michael and I decided to clean up the Warp APIs. 
The following changes will be made in the version 3.1:

- Already deprecated APIs (`settingsFoo`) are removed from the top module.
- In the documentation of the top module, each API is categorized into either standard or internal.
- The `Internal` module exports all internal APIs including the internal APIs in the top module.
- Stopping exporting the `Buffer` and `Timeout` module which have been exported from the top module.

The standard APIs of the top module mainly consist of high-level `run` functions, `Settings` related stuff and necessary data types. We try to maintain these stably. 

The internal APIs in the top module will be removed in Warp version 3.2. This is just documented. DEPRECATED pragmas are not added since there is no simple way to make an API deprecated in the top moudle but live in the internal module.

Warp version 3.1 is not released yet but is available from [github repository](https://github.com/yesodweb/wai). We will wait for a week or two to hear users' opinions.

I originally wrote this as an email to the web-devel and Yesod mailing
lists, but decided that this topic was important enough to be mentioned
in a blog post as well. If you have questions, please direct them to the
mailing list.

Following some discussions on the issue both recently and many months ago, I've just released some updates to how Julius interpolation works. The basic idea is that we want to make interpolation of properly escaped JSON data the default, and the interpolation of unchecked Javascript code the exception to the rule. Concretely, take the following Julius template:

    let greeting = "Say \"Hello\"" :: String
     in [julius|alert("#{greeting}")|]

In Julius 1.0, greeting is interpolated directly into your Javascript template, resulting in the output:

    alert("Say "Hello"")

Importantly, the double quotes are *not* escaped. Instead, we would like `greeting` to be treated as JSON, the double quotes within the text to be escaped, and the surrounding double quotes to be automatically added. I've just released versions 1.0.2 and 1.1 of shakespeare-js (the package providing Julius), and it is now valid to say:

    let greeting = "Say \"Hello\"" :: String
     in [julius|alert(#{toJSON greeting})|]

Resulting in:

    alert("Say \"Hello\"")

What if you really want to spit out raw Javascript code? You can do so via the `rawJS` function:

    let somecode = "alert(\"Hello\")" :: String
     in [julius|#{rawJS somecode}|]

I've set up this release system to try and maintain backwards compatibility, while simultaneously making it a compile-time error when you misuse the library. For the former, I've released shakespeare-js 1.0.2, which keeps the same interpolation semantics as 1.0.1, but adds in the rawJS function. You can safely add `rawJS` to virtually any existing #{} interpolation in Julius without any change in behavior.

The difference with version 1.1 is that the old interpolations will no longer work. Under the surface, this is because I've removed the ToJavascript instances for String and Text. You can now only interpolate JSON values (by using toJSON) or raw Javascript (by using rawJS).

Down the road, it will likely make sense to add in ToJavascript instances for common types which first convert to JSON. However, doing so in this first release would mean that old code would silently change its semantics. Hopefully you will now be able to upgrade to 1.0.2 without any breakage, and in the near future upgrade to 1.1 and get error messages where you need to insert rawJS calls.

I am not yet updating the yesod-platform to use the new versions of these packages; I'd like to ask people to test things out and report any issues they run into so that we can make a properly stable yesod-platform release soon.

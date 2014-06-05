Just a short PSA. yesod-auth has shipped with a GoogleEmail module for a while,
which used Google's OpenID system to authenticate users via their email
addresses. It had the nice property of requiring no configuration to get
started, and was therefore included with the scaffolded site as one of two
default authentication plugins (Mozilla Persona/BrowserID being the second).

However, Google has begun deprecating their OpenID services. In particular, new
sites will no longer be able to use OpenID login already, and all OpenID
services will be removed some time next year (IIRC). There are three results of
this that Yesod users should be aware of:

1. GoogleEmail will no longer be one of the plugins included with the
   scaffolded site.
2. I've released a new version of yesod-auth which includes a *new* module: GoogleEmail2. This module utilizes the new Google+ login system. The user facing interface is almost identical to what GoogleEmail provides, but you must follow more configuration steps, like getting OAuth credentials from Google. The module documentation contains a list of steps necessary.
3. If you have a site that uses GoogleEmail, I recommend you immediately begin testing GoogleEmail2, and role it out ASAP.

GoogleEmail2 has *not* been very thoroughly tested, so please QA your site
before releasing to production. I've already switched over some of my codebases
to using it, but that code is not in production yet, so I don't have detailed
real-world experience to share.

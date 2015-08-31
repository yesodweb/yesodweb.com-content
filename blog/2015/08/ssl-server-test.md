If you are using Warp TLS version 3.1.1 or earlier and the tls library version 1.3.1 or earlier, try the [SSL Server Test](https://www.ssllabs.com/ssltest/) provided by QUALYS SSL LABS. I'm sure that your server will get rating F and you will get disappointed. Here is a list of failed items:

<table border=1>
<tr style="color: orange;">
<td >Secure Renegotiation </td><td>Not supported   ACTION NEEDED</td>
</tr>
<tr style="color: orange;">
<td>Secure Client-Initiated Renegotiation </td><td> Supported   DoS DANGER</td>
</tr>
<tr style="color: red;">
<td>Insecure Client-Initiated Renegotiation </td><td> Supported   INSECURE</td>
</tr>
<tr style="color: orange;">
<td>Downgrade attack prevention </td><td> No, TLS_FALLBACK_SCSV not supported</td>
</tr>
<tr style="color: orange;">
<td>Forward Secrecy </td><td> With some browsers</td>
</tr>
<tr style="color: orange;">
<td>Session resumption (caching) </td><td> No (IDs assigned but not accepted)</td>
</tr>
</table>

Is the quality of the tls library low? The answer is NO. The code is really readable. But some small features were missing unfortunately. This article describes how Aaron Friel and I added such features to get an A rating.

If you are not interested in technical details, just upgrade Warp TLS to version 3.1.2 and the tls library to version 1.3.2. Your server automatically will get an A rating, or a T rating in the case of a self-signed certificate.

## Secure Renegotiation

<table border=1>
<tr style="color: orange;">
<td >Secure Renegotiation</td><td>Not supported   ACTION NEEDED</td>
</tr>
</table>

The original TLS 1.2 renegotiation defined in [RFC 5246](https://tools.ietf.org/html/rfc5246) is now considered insecure because it is vulnerable to
man-in-the-middle attacks.
[RFC 5746](https://tools.ietf.org/html/rfc5746)
defines the "renegotiation_info" extension to authenticate both sides.
The tls library implemented this
but the result was "Not supported".
Why?

The SSL Server Test uses `TLS_EMPTY_RENEGOTIATION_INFO_SCSV`,
an alternative method defined in RFC 5746,
to check this item.
So, I modified the tls library to be aware of this virtual cipher
suite:

<table border=1>
<tr style="color: green;">
<td >Secure Renegotiation</td><td>Supported</td>
</tr>
</table>

## Client-Initiated Renegotiation

<table border=1>
<tr style="color: orange;">
<td>Secure Client-Initiated Renegotiation </td><td> Supported   DoS DANGER</td>
</tr>
<tr style="color: red;">
<td>Insecure Client-Initiated Renegotiation </td><td> Supported   INSECURE</td>
</tr>
</table>

A typical scenario of renegotiation is as follows: a user is browsing some pages over TLS.
Then the user clicks a page which requires the client certificate in TLS.
In this case, the server sends the TLS `HelloRequest` to start
renegotiation so that the client can send the client certificate
through the renegotiation phase.

A client can also initiate renegotiation by sending the TLS `ClientHello`.
But neither secure renegotiation (RFC 5746) nor insecure renegotiation (RFC 5246)
should not be allowed from the client side [because of DOS attacks](https://community.qualys.com/blogs/securitylabs/2011/10/31/tls-renegotiation-and-denial-of-service-attacks).

I added a new parameter `supportedClientInitiatedRenegotiation` to
the `Supported` data type, whose default value is `False`.
This modification results in:

<table border=1>
<tr>
<td>Secure Client-Initiated Renegotiation </td><td> No</td>
</tr>
<tr>
<td>Insecure Client-Initiated Renegotiation </td><td> No</td>
</tr>
</table>

## Downgrade attack prevention

<table border=1>
<tr style="color: orange;">
<td>Downgrade attack prevention </td><td> No, TLS_FALLBACK_SCSV not supported</td>
</tr>
</table>

Downgrade attack is a bad technique to force a client and a server to
use a lower TLS version even if higher TLS versions are available.
Some clients fall back to a lower TLS version if the negotiation of a higher TLS version fails.
An active attacker can cause network congestion or something to make the negotiation failed.

To prevent this, [RFC 7507](https://tools.ietf.org/html/rfc7507) defines Fallback Signaling Cipher Suite Value, `TLS_FALLBACK_SCSV`.
A client includes this virtual cipher suite to the cipher suite proposal
when falling back.
If the corresponding server finds `TLS_FALLBACK_SCSV` and
higher TLS versions are supported,
the server can reject the negotiation to prevent the downgrade attack.

I implemented this feature and the evaluation results in:

<table border=1>
<tr style="color: green;">
<td>Downgrade attack prevention </td><td>Yes, TLS_FALLBACK_SCSV supported</td>
</tr>
</table>

For your information, you can test your server with the following commands:

```
% openssl s_client -connect <ipaddr>:<port> -tls1
% openssl s_client -connect <ipaddr>:<port> -tls1 -fallback_scsv
```

## Forward Secrecy

<table border=1>
<tr style="color: orange;">
<td>Forward Secrecy </td><td> With some browsers</td>
</tr>
</table>

Forward Secrecy can be achieved with *ephemeral* Diffie Hellman (DHE) or
*ephemeral* elliptic curve Diffie Hellman (ECDHE).
Warp TLS version 3.1.1 sets `supportedCiphers` to:

```
[ TLSExtra.cipher_ECDHE_RSA_AES128GCM_SHA256
, TLSExtra.cipher_DHE_RSA_AES128GCM_SHA256
, TLSExtra.cipher_DHE_RSA_AES256_SHA256
, TLSExtra.cipher_DHE_RSA_AES128_SHA256
, TLSExtra.cipher_DHE_RSA_AES256_SHA1
, TLSExtra.cipher_DHE_RSA_AES128_SHA1
, TLSExtra.cipher_DHE_DSS_AES128_SHA1
, TLSExtra.cipher_DHE_DSS_AES256_SHA1
, TLSExtra.cipher_AES128_SHA1
, TLSExtra.cipher_AES256_SHA1
]
```

This is evaluated as "With some browsers". 
[SSL Labs: Deploying Forward Secrecy](https://community.qualys.com/blogs/securitylabs/2013/06/25/ssl-labs-deploying-forward-secrecy) suggests that
`TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA` and `TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA`
are missing.
Aaron Friel added the two cipher suites to the tls library and also
added them in Warp TLS:

```
[ TLSExtra.cipher_ECDHE_RSA_AES128GCM_SHA256
, TLSExtra.cipher_ECDHE_RSA_AES128CBC_SHA256 -- here
, TLSExtra.cipher_ECDHE_RSA_AES128CBC_SHA  -- here
, TLSExtra.cipher_DHE_RSA_AES128GCM_SHA256
, TLSExtra.cipher_DHE_RSA_AES256_SHA256
, TLSExtra.cipher_DHE_RSA_AES128_SHA256
, TLSExtra.cipher_DHE_RSA_AES256_SHA1
, TLSExtra.cipher_DHE_RSA_AES128_SHA1
, TLSExtra.cipher_DHE_DSS_AES128_SHA1
, TLSExtra.cipher_DHE_DSS_AES256_SHA1
, TLSExtra.cipher_AES128_SHA1
, TLSExtra.cipher_AES256_SHA1
]
```

This configuration is evaluated as "With modern browsers".

<table border=1>
<tr>
<td>Forward Secrecy </td><td> With modern browsers</td>
</tr>
</table>

Note that the article also suggests `TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA`.
I know that adding this cipher suite results in "Yes (with most browsers)"
But we don't want to support 3DES.

## Session resumption

<table border=1>
<tr style="color: orange;">
<td>Session resumption (caching) </td><td> No (IDs assigned but not accepted)</td>
</tr>
</table>

Session resumption is a mechanism to reduce the overhead of key exchange.
An exchanged key is associated with a session ID and stored in both
the client and the server side.
The next time the client sends the TLS `ClientHello` message,
the client can specify the session ID previously used.
So, the client and the server are able to reuse the exchanged key.

The tls library supports this mechanism. That's why the result says "IDs assgined". Since Warp TLS does not make use of `SessionManager`, it also says "but not accepted". 

I'm not planning to implement this simple session resumption in Warp TLS since the server would need to have states of exchanged keys. Rather, I would like to implement the stateless TLS session resumption defined in [RFC 5077](https://tools.ietf.org/html/rfc5077).

## Acknowledgment

I would like to thank Kazuho Oku for giving useful information about the secure renegotiation.

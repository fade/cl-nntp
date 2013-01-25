
Common Lisp NNTP client library
===============================

## Description

This is a NNTP client library. The current implementation is not
complete, posting is not implemented yet.

To load the system:

    (asdf:load-system "cl-nntp")

Note that the name of the ASDF system is *``cl-nttp``*, but the lisp
package is *``com.liotev.nntp``*, with *``cl-nntp``* and *``nntp``* specified
as its nicknames.

Here is an example of using some of the functions:

    ;; Connect to an nntp server
    (cl-nntp:connect "nntp.aioe.org" 119)

    ;; Select a group
    cl-nntp:group "comp.lang.lisp")

    ;; Fetch the current article
    (cl-nntp:article)

The last server you connect to is the default server. If you connect to
port 563 or 443, a TLS(SSL) connection will be used. If you want to use
TLS with other ports you have to send the key parameter :use-tls to the
cl-nntp:connect function. (Note that STARTTLS is not implemented yet but
will be in the near future.)

If the server requires authorization the use name and password will be
searched in the user's .authinfo file, whose location you can specify by
setting variable *authinfo-file-name*. There user name and password for
the nntp server should be specified in the .authinfo file like this:

    machine server-name login user-name password pass-word

For example if the server name is nntp.giganews.com, the use name is
xxxer and the password is zzz555 the line would look like this:

    machine nntp.giganews.com login xxxer password zzz555

If a authentication through the authinfo file does not succeed, the user
will be asked to supply user name and password.


## The Network News Transfer Protocol (NNTP)

NNTP is a protocol for reading and posting Usenet articles [\[1\]](#links).
It is described in several RFC's:

 * [RFC 6048](http://www.rfc-editor.org/rfc/rfc6048.txt) - Network News
   Transfer Protocol (NNTP) Additions to LIST Command

 * [RFC 4643](http://www.rfc-editor.org/rfc/rfc4643.txt) - Network News
   Transfer Protocol (NNTP) - Extension for Authentication

 * [RFC 4642](http://www.rfc-editor.org/rfc/rfc4642.txt) - Using
   Transport Layer Security (TLS) with Network News Transfer Protocol
   (NNTP)

 * [RFC 3977](http://www.rfc-editor.org/rfc/rfc3977.txt) - Network News
   Transfer Protocol (NNTP)

 * [RFC 2980](http://www.rfc-editor.org/rfc/rfc2980.txt) - Common NNTP
   Extensions

## Links
<a name="links"/>
1. [The NNTP protocol](http://en.wikipedia.org/wiki/Network_News_Transfer_Protocol)

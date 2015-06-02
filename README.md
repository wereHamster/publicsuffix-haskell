Haskell bindings to query the publicsuffix list
-----------------------------------------------

> A "public suffix" is one under which Internet users can (or historically
could) directly register names. Some examples of public suffixes are .com,
.co.uk and pvt.k12.ma.us. The Public Suffix List is a list of all known public
suffixes.

(from https://publicsuffix.org/)

This package exposes functions to query the publicsuffix list to see if
a particular domain is a public suffix or not. This is used for example in HTTP
user agents to determine whether a cookie can be accepted (example of a bad
cookie: `Set-Cookie: session=X; domain=*.com`) or when checking X.509
certificates (SSL/TLS) to see if a common name is valid (invalid CN: `*.com`).

The list is compiled into the package, the functions are pure and don't need IO
(not even hidden internally). Since the publicsuffix list is updated
periodically, you should watch for changes and update your lower bounds
on this package.


### Version number

The package version follows the template `<api version>.<date>` where `date`
is when the publicsuffix list was last updated from upstream, in the format
`YYYYMMDD`.

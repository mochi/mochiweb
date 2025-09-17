Version 3.3.0 released 2025-09-17

* Add OTP 28 to CI and fix crypto:start() deprecation warning.
  Removed support for OTP 18 and OTP 19.
  https://github.com/mochi/mochiweb/pull/264

Version 3.2.2 released 2024-03-21

* Use single quotes around 'maybe' atom for compatibility with OTP 27
  https://github.com/mochi/mochiweb/pull/262
* Update Erlang CI images
  https://github.com/mochi/mochiweb/pull/261

Version 3.2.1 released 2023-09-22

* mochinum:digits/1: fix handling of -0.0 for OTP-26.1/27.0
  https://github.com/mochi/mochiweb/pull/260

Version 3.2.0 released 2023-08-31

* Add new mochiweb_request:is_closed/1 function
  https://github.com/mochi/mochiweb/pull/258

Version 3.1.2 released 2023-04-20

* Fix rebar edoc settings
  https://github.com/mochi/mochiweb/pull/257

Version 3.1.1 released 2022-10-11

* OTP 25 added to test matrix
  https://github.com/mochi/mochiweb/pull/251
* Fix for chunk length parsing edge case
  https://github.com/mochi/mochiweb/pull/249

Version 3.1.0 released 2022-08-13

* Leading and trailing whitespace in header values are now trimmed
  for better RFC 7230 compliance.
  https://github.com/mochi/mochiweb/pull/247
  https://github.com/mochi/mochiweb/pull/248

Version 3.0.0 released 2022-05-09

* rebar3 is now the preferred build tool (finally)
  https://github.com/mochi/mochiweb/pull/241
  https://github.com/mochi/mochiweb/pull/243
* Minimum OTP version is now 18, which
  allows us to remove a number of backwards
  compatibility hacks while still supporting
  almost 7 years of Erlang releases.
  https://github.com/mochi/mochiweb/pull/239
* Crashing client processes now exit with reason
  `{shutdown, Error}`. This ensures processes
  linked to the connection process are also
  cleaned up. If exit `normal` was caught in a
  request loop callback, for example in a
  `try ... catch exit:normal ...` expression,
  that expression might have to be updated to
  handle the `{shutdown, Error}` error reason.
  https://github.com/mochi/mochiweb/pull/238
  https://github.com/mochi/mochiweb/pull/242

Version 2.22.0 released 2021-08-23

* Renamed master branch to main
* Add unquote_path/1 for separate '+' encoding
  https://github.com/mochi/mochiweb/pull/231

Version 2.21.0 released 2021-06-06

* Upgrade crypto functions to support OTP 23
  https://github.com/mochi/mochiweb/pull/231
* Switch from Travis to GitHub Actions for testing
  https://github.com/mochi/mochiweb/pull/232

Version 2.20.1 released 2020-02-03

* Removed deprecated metadata from .app file

Version 2.20.0 released 2019-07-14

* Expand testing matrix to include Erlang/OTP 22.0 and Erlang/OTP 21.3
* Add support for SameSite=none in cookies
  https://github.com/mochi/mochiweb/pull/225
* Fix parsing of certain unquoted cookie values
  https://github.com/mochi/mochiweb/pull/212

Version 2.19.0 released 2019-01-17

* Fix warning in 21.2.3 and crash on incompatible releases
  (21.2, 21.2.1, 21.2.2 have a SSL bug)
  https://github.com/mochi/mochiweb/pull/210
* Erlang/OTP 21 compatibility
  https://github.com/mochi/mochiweb/pull/198
  https://github.com/mochi/mochiweb/pull/204
* New `{buffer, Buffer}` socket server option
  https://github.com/mochi/mochiweb/pull/208
* New `{format, map}` option for mochijson2:decode/2
  https://github.com/mochi/mochiweb/pull/206
* No longer crash when a socket is closed server-side
  https://github.com/mochi/mochiweb/pull/205
* Support for SameSite cookie setting
  https://github.com/mochi/mochiweb/pull/203

Version 2.18.0 released 2018-05-12

* Add the 100.64.0.0/10 private IP shared address range
  https://github.com/mochi/mochiweb/pull/193

Version 2.17.0 released 2017-08-12

* Fix deprecation warnings for Erlang/OTP 20.0
  https://github.com/mochi/mochiweb/pull/186
* Updated mochiweb_html singleton tag heuristic for HTML5
  https://github.com/mochi/mochiweb/pull/190
* Send 400 Bad Request if request line exceeds recbuf (regression fix)
  https://github.com/mochi/mochiweb/pull/191

Version 2.16.0 released 2016-12-19

* Added support for encoding maps to mochijson2 (where available)
  https://github.com/mochi/mochiweb/pull/184
* Added missing RFC1918 address spaces to the allowed x-forwarded-for header
  https://github.com/mochi/mochiweb/pull/183

Version 2.15.1 released 2016-06-24

* Fixed deprecation warnings in Erlang/OTP 19.0
  https://github.com/mochi/mochiweb/pull/177

Version 2.15.0 released 2016-05-08

* mochiweb_request now normalizes paths such that duplicate slashes are
  discarded (and thus all path segments except the last are non-empty).
  https://github.com/mochi/mochiweb/pull/173

Version 2.14.0 released 2016-04-11

* mochiweb_html now requires a letter to begin a HTML tag
  https://github.com/mochi/mochiweb/pull/171

Version 2.13.2 released 2016-03-18

* Allow mochijson2 to handle code points that xmerl_ucs considered
  invalid
  https://github.com/mochi/mochiweb/issues/168

Version 2.13.1 released 2016-03-13

* Fix mochiweb_html regression parsing invalid charref sequences
  https://github.com/mochi/mochiweb/issues/167

Version 2.13.0 released 2016-02-08

* Support parsing of UTF-16 surrogate pairs encoded as character
  references in mochiweb_html
  https://github.com/mochi/mochiweb/issues/164
* Avoid swallowing messages that are not related to the socket
  during request parsing
  https://github.com/mochi/mochiweb/pull/161
* Ensure correct ordering of Set-Cookie headers: first in, first out
  https://github.com/mochi/mochiweb/issues/162
* Improve response times by caching a formatted date once per second
  for the response headers with a mochiweb_clock service
  https://github.com/mochi/mochiweb/pull/158

Version 2.12.2 released 2015-02-21

* Close connections quietly when setopts fails with a closed socket.
  https://github.com/mochi/mochiweb/pull/152

Version 2.12.1 released 2015-02-01

* Fix active_socket accounting
  https://github.com/mochi/mochiweb/issues/149
* Added full MIT license preludes to each source file to make it
  easier for mochiweb's code to be used piecemeal
  https://github.com/mochi/mochiweb/pull/148

Version 2.12.0 released 2015-01-16

* Send "Connection: close" header when the server is going to close
  a Keep-Alive connection, usually due to unread data from the
  client
  https://github.com/mochi/mochiweb/issues/146

Version 2.11.2 released 2015-01-16

* Fix regression introduced in #147
  https://github.com/mochi/mochiweb/pull/147

Version 2.11.1 released 2015-01-16

* Accept range end position which exceededs the resource size
  https://github.com/mochi/mochiweb/pull/147

Version 2.11.0 released 2015-01-12

* Perform SSL handshake after releasing acceptor back into the pool,
  and slow accept rate when file descriptors are not available,
  to mitigate a potential DoS attack. Adds new mochiweb_socket
  functions transport_accept/1 and finish_accept/1 which should be
  used in preference to the now deprecated accept/1 function.
  https://github.com/mochi/mochiweb/issues/138

Version 2.10.1 released 2015-01-11

* Fixes issue with SSL and mochiweb_websocket. Note that
  mochiweb_websocket is still experimental and the API
  is subject to change in future versions.
  https://github.com/mochi/mochiweb/pull/144

Version 2.10.0 released 2014-12-17

* Added new `recbuf` option to mochiweb_http to allow the receive
  buffer to be configured.
  https://github.com/mochi/mochiweb/pull/134

Version 2.9.2 released 2014-10-16

* Add timeouts to SSL connect to prevent DoS by opening a connection
  and not doing anything.
  https://github.com/mochi/mochiweb/pull/140
* Prevent using ECDH cipher in R16B because it is broken
  https://github.com/mochi/mochiweb/pull/140
* For default SSL connections, remove usage of sslv3 and not-so-secure
  ciphers.
  https://github.com/mochi/mochiweb/pull/140

Version 2.9.1 released 2014-09-29

* Fix Makefile rule for building docs
  https://github.com/mochi/mochiweb/issues/135
* Minimize gen_tcp:send calls to optimize performance.
  https://github.com/mochi/mochiweb/pull/137

Version 2.9.0 released 2014-06-24

* Increased timeout in test suite for FreeBSD
  https://github.com/mochi/mochiweb/pull/121
* Updated rebar to v2.5.0 and fixed associated build issues
  https://github.com/mochi/mochiweb/issues/131

Version 2.8.0 released 2014-01-01

* Websocket support
  https://github.com/mochi/mochiweb/pull/120
* Force files named "crossdomain.xml" to have MIME type
  text/x-cross-domain-policy.
  https://github.com/mochi/mochiweb/pull/118

Version 2.7.0 released 2013-08-01

* Fix 0-length range responses
  https://github.com/mochi/mochiweb/pull/87
* Add support for all possible `erlang:decode_packet/3` responses,
  previously these would just crash.
  https://github.com/mochi/mochiweb/pull/114
* Makefile fixed to make `make test` work before `make all`
  https://github.com/mochi/mochiweb/pull/116
* Usage of the crypto module made R16B01+ compatible
  https://github.com/mochi/mochiweb/pull/115
* Build fixed for R16B01
  https://github.com/mochi/mochiweb/pull/112
* `mochiweb_socket_server:stop/1` is now a synchronous
  call instead of an asynchronous cast
* `mochiweb_html:parse_tokens/1` (and `parse/1`) will now create a
  html element to wrap documents that have a HTML5 doctype
  (`<!doctype html>`) but no html element
  https://github.com/mochi/mochiweb/issues/110

Version 2.6.0 released 2013-04-15

* Enable R15B gen_tcp workaround only on R15B
  https://github.com/mochi/mochiweb/pull/107

Version 2.5.0 released 2013-03-04

* Replace now() with os:timestamp() in acceptor (optimization)
  https://github.com/mochi/mochiweb/pull/102
* New mochiweb_session module for managing session cookies.
  NOTE: this module is only supported on R15B02 and later!
  https://github.com/mochi/mochiweb/pull/94
* New mochiweb_base64url module for base64url encoding
  (URL and Filename safe alphabet, see RFC 4648).
* Fix rebar.config in mochiwebapp_skel to use {branch, "master"}
  https://github.com/mochi/mochiweb/issues/105

Version 2.4.2 released 2013-02-05

* Fixed issue in mochiweb_response introduced in v2.4.0
  https://github.com/mochi/mochiweb/pull/100

Version 2.4.1 released 2013-01-30

* Fixed issue in mochiweb_request introduced in v2.4.0
  https://github.com/mochi/mochiweb/issues/97
* Fixed issue in mochifmt_records introduced in v2.4.0
  https://github.com/mochi/mochiweb/issues/96

Version 2.4.0 released 2013-01-23

* Switch from parameterized modules to explicit tuple module calls for
  R16 compatibility (#95)
* Fix for mochiweb_acceptor crash with extra-long HTTP headers under
  R15B02 (#91)
* Fix case in handling range headers (#85)
* Handle combined Content-Length header (#88)
* Windows security fix for `safe_relative_path`, any path with a
  backslash on any platform is now considered unsafe (#92)

Version 2.3.2 released 2012-07-27

* Case insensitive match for "Connection: close" (#81)

Version 2.3.1 released 2012-03-31

* Fix edoc warnings (#63)
* Fix mochiweb_html handling of invalid charref sequences (unescaped &) (#69).
* Add a manual garbage collection between requests to avoid worst case behavior
  on keep-alive sockets.
* Fix dst cookie bug (#73)
* Removed unnecessary template_dir option, see
  https://github.com/basho/rebar/issues/203

Version 2.3.0 released 2011-10-14

* Handle ssl_closed message in mochiweb_http (#59)
* Added support for new MIME types (otf, eot, m4v, svg, svgz, ttc, ttf,
  vcf, webm, webp, woff) (#61)
* Updated mochiweb_charref to support all HTML5 entities. Note that
  if you are using this module directly, the spec has changed to return
  `[integer()]` for some entities. (#64)

Version 2.2.1 released 2011-08-31

* Removed `mochiweb_skel` module from the pre-rebar era

Version 2.2.0 released 2011-08-29

* Added new `mochiweb_http:start_link/1` and
  `mochiweb_socket_server:start_link/1` APIs to explicitly start linked
  servers. Also added `{link, false}` option to the `start/1` variants
  to explicitly start unlinked. This is in expectation that we will
  eventually change the default behavior of `start/1` to be unlinked as you
  would expect it to. See https://github.com/mochi/mochiweb/issues/58 for
  discussion.

Version 2.1.0 released 2011-08-29

* Added new `mochijson2:decode/2` with `{format, struct | proplist | eep18}`
  options for easy decoding to various proplist formats. Also added encoding
  support for eep18 style objects.

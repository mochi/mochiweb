Version 2.7.0 released XXXX-XX-XX

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

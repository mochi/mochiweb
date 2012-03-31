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

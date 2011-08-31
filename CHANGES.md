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


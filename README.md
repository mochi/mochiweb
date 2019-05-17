testMochiWeb is an Erlang library for building lightweight HTTP servers.

The latest version of MochiWeb is available at http://github.com/mochi/mochiweb

The mailing list for MochiWeb is at http://groups.google.com/group/mochiweb/

To create a new mochiweb using project:
   make app PROJECT=project_name

To create a new mochiweb using project in a specific directory:
   make app PROJECT=project_name PREFIX=$HOME/projects/

MochiWeb is currently tested with Erlang/OTP R15B03 through 21.2.3.

# OTP 21.2, 21.2.1, 21.2.2 warning

OTP 21.2 (up to and including 21.2.2) introduced an SSL regression that
makes these releases unsafe to use. See [ERL-830](https://bugs.erlang.org/browse/ERL-830).
This issue was resolved in OTP 21.2.3.

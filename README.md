MochiWeb is an Erlang library for building lightweight HTTP servers.

The latest version of MochiWeb is available at https://github.com/mochi/mochiweb

The mailing list for MochiWeb is at https://groups.google.com/group/mochiweb/

Erlang OTP is required for setting up the MochiWeb environment and is available at https://www.erlang.org/

To create a new mochiweb using project:
   make app PROJECT=project_name

To create a new mochiweb using project in a specific directory:
   make app PROJECT=project_name PREFIX=$HOME/projects/

Information about Rebar (Erlang build tool) is available at https://github.com/rebar/rebar

MochiWeb is currently tested with Erlang/OTP 18.3 through 24.0,
but may still be compatible back to R15B-03.

# OTP 21.2, 21.2.1, 21.2.2 warning

OTP 21.2 (up to and including 21.2.2) introduced an SSL regression that
makes these releases unsafe to use. See [ERL-830](https://bugs.erlang.org/browse/ERL-830).
This issue was resolved in OTP 21.2.3.

# MochiWeb - Open Source

## Introduction
[testMochiWeb](http://alexmarandon.com/articles/mochiweb_tutorial/) is an Erlang library for building lightweight HTTP servers.

It’s not a framework: it doesn’t come with URL dispatch, templating or data persistence. Despite not having an official website or narrative documentation, MochiWeb is a popular choice to build web services in Erlang. 

I assume you’re familiar with basic sequential Erlang. If it’s not the case, I’d suggest you study the first few chapters of [this guide](https://learnyousomeerlang.com/). No knowledge of concurrent and distributed Erlang is needed to follow this tutorial.

If you get stuck, you can [get the code](https://github.com/amarandon/greeting) corresponding to this tutorial. Each commit corresponds to a section of the tutorial so you can easily see the code for a specific step by checking out the corresponding commit.

## Getting Started

Start by getting a copy of MochiWeb using Git:

`$ git clone git://github.com/mochi/mochiweb.git`

To create a new project:

`$ cd mochiweb`

`$ make app PROJECT=project_name`

To create a new mochiweb using project in a specific directory:
   
`$ make app PROJECT=project_name PREFIX=$HOME/projects/`

We can now compile the code for our new app and start it:

`$ cd ../project_name/`

`$ make`

`$ ./start-dev.sh`

MochiWeb is currently tested with Erlang/OTP R15B03 through 21.2.3.

## Contact

The mailing list for MochiWeb is at http://groups.google.com/group/mochiweb/ if you have any questions.

## Contribution

If you are interested in fixing issue and contributing directly to the code base:

- to report [issues](https://github.com/mochi/mochiweb/issues)

- to pull request first fork then [pull request](https://github.com/mochi/mochiweb/pulls)


# OTP 21.2, 21.2.1, 21.2.2 warning

OTP 21.2 (up to and including 21.2.2) introduced an SSL regression that
makes these releases unsafe to use. See [ERL-830](https://bugs.erlang.org/browse/ERL-830).
This issue was resolved in OTP 21.2.3.

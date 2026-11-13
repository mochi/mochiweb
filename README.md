# MochiWeb
MochiWeb is an Erlang library for building lightweight HTTP servers.

## Overview
MochiWeb provides a lightweight and fast solution for building HTTP servers in Erlang. The library provides features for building robust and scalable HTTP servers.

## Getting Started
Before you can use MochiWeb, you'll need to have [Erlang OTP](https://www.erlang.org/) 25 or later installed. Once you have Erlang OTP installed, you can download the latest version of MochiWeb from the [GitHub repository](https://github.com/mochi/mochiweb).

For a MochiWeb project, first obtain a copy of MochiWeb using Git by running the command.


```erlang-repl
$ git clone git://github.com/mochi/mochiweb.git.
```
To create a project.

```erlang-repl
$ cd mochiweb
$ make app PROJECT=exampleName
```

You can now start the project with.

```erlang-repl
$ cd ../exampleName/
$ make
$ ./start-dev.sh
```

You can access the app by navigating to http://localhost:8080 in your browser.

For an example, view the `example_project` in the `examples/` folder.


## Benefits
* Lightweight: MochiWeb is designed to be lightweight and fast, making it ideal for use in resource-constrained environments.

* Easy to use: MochiWeb provides a simple and intuitive API that makes it easy to get started with building HTTP servers.

* Robust: MochiWeb provides a comprehensive set of features for building robust and scalable HTTP servers.

## Documentations and Resources
[Information about Rebar (Erlang build tool)](https://github.com/erlang/rebar3)

[Mailing list](https://groups.google.com/group/mochiweb/)

## Contributing
MochiWeb is an open-source project and welcomes contributions from the community.

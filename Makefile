REBAR?=rebar3

.PHONY: all edoc test clean

build:
	@$(REBAR) get-deps # rebar2 compatibility, it's no-op on rebar3
	@$(REBAR) compile

test: build
	@$(REBAR) eunit

edoc: build
	@$(REBAR) edoc

clean:
	@$(REBAR) clean

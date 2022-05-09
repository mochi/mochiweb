REBAR?=rebar3

.PHONY: all edoc test clean app

build:
	@$(REBAR) get-deps # rebar2 compatibility, it's no-op on rebar3
	@$(REBAR) compile

test:
	@$(REBAR) eunit

edoc:
	@$(REBAR) edoc

clean:
	@$(REBAR) clean

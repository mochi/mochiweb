PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

.PHONY: all edoc test clean build_plt dialyzer app

all:
	@$(REBAR) -r get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

app:
	@$(REBAR) -r create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

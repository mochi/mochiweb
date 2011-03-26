
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar
DIALYZER=dialyzer

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) compile
	@$(DIALYZER) -Wunderspecs ebin

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)


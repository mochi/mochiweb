
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=`which rebar || ./rebar`
DIALYZER=dialyzer

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
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


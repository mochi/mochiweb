
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all: 
	@$(REBAR) get-deps compile	

edoc:
	@$(REBAR) doc

test: 
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build_plt

dialyzer:
	@$(REBAR) analyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)


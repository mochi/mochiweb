PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
DEPS_DIR:=/deps/
DEPS:=$(PREFIX)$(PROJECT)$(DEPS_DIR)
MOCHIWEB_DIR:=$(shell pwd)
REBAR=./rebar

.PHONY: all edoc test clean build_plt dialyzer app

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
	@$(REBAR) dialyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)
	mkdir $(DEPS)
	ln -s $(MOCHIWEB_DIR) $(DEPS)mochiweb

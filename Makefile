REBAR=./rebar

all: 
	@$(REBAR) compile	

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

all: ebin/
	(cd src;$(MAKE) all)

edoc:
	(cd src;$(MAKE) edoc)

test: ebin/
	(cd src;$(MAKE) test)

clean:
	rm -rf ebin

clean_plt:
	(cd src;$(MAKE) clean_plt)

dialyzer:
	(cd src;$(MAKE) dialyzer)

ebin/:
	@mkdir -p ebin

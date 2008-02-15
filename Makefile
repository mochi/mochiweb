all:
	(cd src;$(MAKE))

test:
	(cd src;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)

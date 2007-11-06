all:
	(cd src;$(MAKE))

mmall: 
	(cd src;$(MAKE) mmbuild)

local:
	(cd src;$(MAKE) local)

clean:
	(cd src;$(MAKE) clean)

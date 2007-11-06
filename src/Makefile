include ../support/include.mk

all: $(EBIN_FILES)

mmbuild: $(EBIN_FILES_NO_DOCS)

mmdocs: $(EBIN_DOCUMENTS)

local: $(ERL_OBJECTS_LOCAL)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

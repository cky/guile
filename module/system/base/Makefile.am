SOURCES = syntax.scm compile.scm language.scm
# we don't deal well with syncase yet
NOCOMP_SOURCES = pmatch.scm
GOBJECTS = $(SOURCES:%.scm=%.go)

vmdir = $(guiledir)/system/vm
vm_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go
%.go: %.scm
	$(GUILEC) $<

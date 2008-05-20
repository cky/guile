NOCOMP_SOURCES = describe.scm
SOURCES = repl.scm common.scm command.scm
GOBJECTS = $(SOURCES:%.scm=%.go)

vmdir = $(guiledir)/system/repl
vm_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go
%.go: %.scm
	$(GUILEC) $<

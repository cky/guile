SOURCES = repl.scm common.scm command.scm describe.scm
GOBJECTS = $(SOURCES:%.scm=%.go)

vmdir = $(guiledir)/system/repl
vm_DATA = $(SOURCES) $(GOBJECTS)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go
%.go: %.scm
	$(GUILEC) $<

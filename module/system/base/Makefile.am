SOURCES = syntax.scm compile.scm language.scm pmatch.scm
GOBJECTS = $(SOURCES:%.scm=%.go)

vmdir = $(guiledir)/system/vm
vm_DATA = $(SOURCES) $(GOBJECTS)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go
%.go: %.scm
	$(GUILEC) $<

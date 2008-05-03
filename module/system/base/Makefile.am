SOURCES = compile.scm language.scm pmatch.scm
## syntax.scm
GOBJECTS = $(SOURCES:%.scm=%.go)

vmdir = $(guiledir)/system/vm
vm_DATA = $(SOURCES) $(GOBJECTS)

CLEANFILES = $(GOBJECTS)
MAINTAINERCLEANFILES = Makefile.in

SUFFIXES = .scm .go
%.go: %.scm
	$(GUILEC) $<

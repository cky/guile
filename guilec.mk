GOBJECTS = $(SOURCES:%.scm=%.go)

moddir = $(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)/$(modpath)
mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/pre-inst-guile -s \$(top_builddir)/src/guilec $<

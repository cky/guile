GOBJECTS = $(SOURCES:%.scm=%.go)

mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go
.scm.go:
	GUILE_LOAD_PATH=\$(top_srcdir)/module \
	LD_LIBRARY_PATH=\$(top_builddir)/src/.libs \
	$(top_builddir)/pre-inst-guile-env \
	  guile -s \$(top_builddir)/src/guilec $<

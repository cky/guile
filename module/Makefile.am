DISTDIRS = $(srcdir)/system $(srcdir)/language $(srcdir)/guile $(srcdir)/slib
EXCLUDES = --exclude=CVS --exclude=*.go --exclude=*~

all: slibcat

clean:
	rm -f slibcat slib/*.go

slibcat:
	guile -s $(top_srcdir)/src/guilec slib/*.scm

dist-hook:
	$(TAR) cf - $(EXCLUDES) $(DISTDIRS) | (cd $(distdir); $(TAR) xf -)

DISTDIRS = $(srcdir)/language $(srcdir)/system $(srcdir)/slib
EXCLUDES = --exclude=CVS --exclude='*~'

all: slibcat

clean:
	rm -f slibcat slib/*.go

slibcat:
	guile -s slib-comp.scm

dist-hook:
	$(TAR) cf - $(EXCLUDES) $(DISTDIRS) | (cd $(distdir); $(TAR) xf -)

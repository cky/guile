DISTDIRS = $(srcdir)/language $(srcdir)/system
EXCLUDES = --exclude=CVS --exclude='*~'

dist-hook:
	$(TAR) cf - $(EXCLUDES) $(DISTDIRS) | (cd $(distdir); $(TAR) xf -)

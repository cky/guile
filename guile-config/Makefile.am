#### Makefile.in template for guile-core/guile-config.
#### Jim Blandy <jimb@red-bean.com> --- September 1997

bin_SCRIPTS=guile-config
CLEANFILES=guile-config
EXTRA_DIST=guile-config.in

## We use @-...-@ as the substitution brackets here, instead of the
## usual @...@, so autoconf doesn't go and substitute the values
## directly into the left-hand sides of the sed substitutions.  *sigh*
guile-config: guile-config.in
	rm -f guile-config.tmp
	sed < ${srcdir}/guile-config.in > guile-config.tmp \
	    -e s:@-bindir-@:${bindir}: \
	    -e s:@-GUILE_VERSION-@:${GUILE_VERSION}:
	chmod +x guile-config.tmp
	mv guile-config.tmp guile-config

## Get rid of any copies of the configuration script under the old
## name, so people don't end up running ancient copies of it.
install-exec-local:
	rm -f ${bindir}/build-guile

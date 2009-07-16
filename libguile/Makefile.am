## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##
##   GUILE is free software; you can redistribute it and/or modify it
##   under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##
##   You should have received a copy of the GNU Lesser General Public
##   License along with GUILE; see the file COPYING.LESSER.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA 02110-1301 USA

AUTOMAKE_OPTIONS = gnu

## Prevent automake from adding extra -I options
DEFS = @DEFS@

# Override Automake's `DEFAULT_INCLUDES'.  By default, it contains
# "-I$(srcdir)", which causes problems on Tru64 where our "random.h"
# is picked up by <stdlib.h> instead of the libc's <random.h>.
DEFAULT_INCLUDES =

## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.  Also look for Gnulib headers in `lib'.
AM_CPPFLAGS = -DBUILDING_LIBGUILE=1 -I$(top_srcdir) -I$(top_builddir) \
	      -I$(top_srcdir)/lib -I$(top_builddir)/lib

AM_CFLAGS = $(GCC_CFLAGS) $(CFLAG_VISIBILITY)

## The Gnulib Libtool archive.
gnulib_library = $(top_builddir)/lib/libgnu.la

ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

lib_LTLIBRARIES = libguile.la						\
		  libguile-i18n-v-@LIBGUILE_I18N_MAJOR@.la
bin_PROGRAMS = guile

noinst_PROGRAMS = guile_filter_doc_snarfage gen-scmconfig

gen_scmconfig_SOURCES = gen-scmconfig.c

## Override default rule; this should be compiled for BUILD host.
## For some reason, OBJEXT does not include the dot
gen-scmconfig.$(OBJEXT): gen-scmconfig.c
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CC_FOR_BUILD) $(DEFS) $(DEFAULT_INCLUDES) $(AM_CPPFLAGS) -c -o $@ $<; \
	else \
		$(COMPILE) -c -o $@ $<; \
	fi

## Override default rule; this should run on BUILD host.
gen-scmconfig$(EXEEXT): $(gen_scmconfig_OBJECTS) $(gen_scmconfig_DEPENDENCIES) 
	@rm -f gen-scmconfig$(EXEEXT)
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CCLD_FOR_BUILD) -o $@ $(gen_scmconfig_OBJECTS); \
	else \
		$(LINK) $(gen_scmconfig_OBJECTS) $(LDADD) $(LIBS); \
	fi

scmconfig.h: ${top_builddir}/config.h gen-scmconfig$(EXEEXT)
	rm -f scmconfig.h.tmp
	cat $(srcdir)/scmconfig.h.top > scmconfig.h.tmp
	./gen-scmconfig$(EXEEXT) >> scmconfig.h.tmp
	chmod 444 scmconfig.h.tmp
	rm -f scmconfig.h
	mv scmconfig.h.tmp scmconfig.h

guile_filter_doc_snarfage_SOURCES = c-tokenize.c

## Override default rule; this should be compiled for BUILD host.
## For some reason, OBJEXT does not include the dot
c-tokenize.$(OBJEXT): c-tokenize.c
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CC_FOR_BUILD) $(DEFS) $(AM_CPPFLAGS) -c -o $@ $<; \
	else \
		$(filter-out -Werror,$(COMPILE)) -c -o $@ $<; \
	fi

## Override default rule; this should run on BUILD host.
guile_filter_doc_snarfage$(EXEEXT): $(guile_filter_doc_snarfage_OBJECTS) $(guile_filter_doc_snarfage_DEPENDENCIES) 
	@rm -f guile_filter_doc_snarfage$(EXEEXT)
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CCLD_FOR_BUILD) -o $@ $(guile_filter_doc_snarfage_OBJECTS); \
	else \
		$(LINK) $(guile_filter_doc_snarfage_OBJECTS) $(LDADD) $(LIBS); \
	fi


guile_SOURCES = guile.c
guile_CFLAGS = $(GUILE_CFLAGS) $(AM_CFLAGS)
guile_LDADD = libguile.la
guile_LDFLAGS = $(GUILE_CFLAGS)

libguile_la_CFLAGS = $(GUILE_CFLAGS) $(AM_CFLAGS)

libguile_la_SOURCES =				\
	alist.c					\
	arbiters.c				\
	array-handle.c				\
	array-map.c				\
	async.c					\
	backtrace.c				\
	boolean.c				\
	bytevectors.c				\
	chars.c					\
	continuations.c				\
	debug.c					\
	deprecated.c				\
	deprecation.c				\
	discouraged.c				\
	dynwind.c				\
	eq.c					\
	error.c					\
	eval.c					\
	evalext.c				\
	extensions.c				\
	feature.c				\
	fluids.c				\
	fports.c				\
	frames.c				\
	futures.c				\
	gc-card.c				\
	gc-freelist.c				\
	gc-malloc.c				\
	gc-mark.c				\
	gc-segment-table.c			\
	gc-segment.c				\
	gc.c					\
	gc_os_dep.c				\
	gdbint.c				\
	gettext.c				\
	goops.c					\
	gsubr.c					\
	guardians.c				\
	hash.c					\
	hashtab.c				\
	hooks.c					\
	init.c					\
	inline.c				\
	instructions.c				\
	ioext.c					\
	keywords.c				\
	lang.c					\
	list.c					\
	load.c					\
	macros.c				\
	mallocs.c				\
	modules.c				\
	null-threads.c				\
	numbers.c				\
	objcodes.c				\
	objects.c				\
	objprop.c				\
	options.c				\
	pairs.c					\
	ports.c					\
	print.c					\
	procprop.c				\
	procs.c					\
	programs.c				\
	properties.c				\
	r6rs-ports.c				\
	random.c				\
	rdelim.c				\
	read.c					\
	root.c					\
	rw.c					\
	scmsigs.c				\
	script.c				\
	simpos.c				\
	smob.c					\
	sort.c					\
	srcprop.c				\
	srfi-13.c				\
	srfi-14.c				\
	srfi-4.c				\
	stackchk.c				\
	stacks.c				\
	stime.c					\
	strings.c				\
	strorder.c				\
	strports.c				\
	struct.c				\
	symbols.c				\
	threads.c				\
	throw.c					\
	unif.c					\
	values.c				\
	variable.c				\
	vectors.c				\
	version.c				\
	vm.c					\
	vports.c				\
	weaks.c

libguile_i18n_v_@LIBGUILE_I18N_MAJOR@_la_SOURCES = i18n.c
libguile_i18n_v_@LIBGUILE_I18N_MAJOR@_la_CFLAGS =	\
   $(libguile_la_CFLAGS)
libguile_i18n_v_@LIBGUILE_I18N_MAJOR@_la_LIBADD =	\
   libguile.la $(gnulib_library)
libguile_i18n_v_@LIBGUILE_I18N_MAJOR@_la_LDFLAGS =	\
   -module -L$(builddir) -lguile				\
   -version-info @LIBGUILE_I18N_INTERFACE@

DOT_X_FILES =					\
	alist.x					\
	arbiters.x				\
	array-handle.x				\
	array-map.x				\
	async.x					\
	backtrace.x				\
	boolean.x				\
	bytevectors.x				\
	chars.x					\
	continuations.x				\
	debug.x					\
	deprecated.x				\
	deprecation.x				\
	discouraged.x				\
	dynl.x					\
	dynwind.x				\
	eq.x					\
	error.x					\
	eval.x					\
	evalext.x				\
	extensions.x				\
	feature.x				\
	fluids.x				\
	fports.x				\
	futures.x				\
	gc-card.x				\
	gc-malloc.x				\
	gc-mark.x				\
	gc-segment-table.x			\
	gc-segment.x				\
	gc.x					\
	gettext.x				\
	goops.x					\
	gsubr.x					\
	guardians.x				\
	hash.x					\
	hashtab.x				\
	hooks.x					\
	i18n.x					\
	init.x					\
	ioext.x					\
	keywords.x				\
	lang.x					\
	list.x					\
	load.x					\
	macros.x				\
	mallocs.x				\
	modules.x				\
	numbers.x				\
	objects.x				\
	objprop.x				\
	options.x				\
	pairs.x					\
	ports.x					\
	print.x					\
	procprop.x				\
	procs.x					\
	properties.x				\
	r6rs-ports.x				\
	random.x				\
	rdelim.x				\
	read.x					\
	root.x					\
	rw.x					\
	scmsigs.x				\
	script.x				\
	simpos.x				\
	smob.x					\
	sort.x					\
	srcprop.x				\
	srfi-13.x				\
	srfi-14.x				\
	srfi-4.x				\
	stackchk.x				\
	stacks.x				\
	stime.x					\
	strings.x				\
	strorder.x				\
	strports.x				\
	struct.x				\
	symbols.x				\
	threads.x				\
	throw.x					\
	unif.x					\
	values.x				\
	variable.x				\
	vectors.x				\
	version.x				\
	vports.x				\
	weaks.x

# vm-related snarfs
DOT_X_FILES += frames.x instructions.x objcodes.x programs.x vm.x

EXTRA_DOT_X_FILES = @EXTRA_DOT_X_FILES@

DOT_DOC_FILES = 				\
	alist.doc				\
	arbiters.doc				\
	array-handle.doc			\
	array-map.doc				\
	async.doc				\
	backtrace.doc				\
	boolean.doc				\
	bytevectors.doc				\
	chars.doc				\
	continuations.doc			\
	debug.doc				\
	deprecated.doc				\
	deprecation.doc				\
	discouraged.doc				\
	dynl.doc				\
	dynwind.doc				\
	eq.doc					\
	error.doc				\
	eval.doc				\
	evalext.doc				\
	extensions.doc				\
	feature.doc				\
	fluids.doc				\
	fports.doc				\
	futures.doc				\
	gc-card.doc				\
	gc-malloc.doc				\
	gc-mark.doc				\
	gc-segment-table.doc			\
	gc-segment.doc				\
	gc.doc					\
	gettext.doc				\
	goops.doc				\
	gsubr.doc				\
	guardians.doc				\
	hash.doc				\
	hashtab.doc				\
	hooks.doc				\
	i18n.doc				\
	init.doc				\
	ioext.doc				\
	keywords.doc				\
	lang.doc				\
	list.doc				\
	load.doc				\
	macros.doc				\
	mallocs.doc				\
	modules.doc				\
	numbers.doc				\
	objects.doc				\
	objprop.doc				\
	options.doc				\
	pairs.doc				\
	ports.doc				\
	print.doc				\
	procprop.doc				\
	procs.doc				\
	properties.doc				\
	r6rs-ports.doc				\
	random.doc				\
	rdelim.doc				\
	read.doc				\
	root.doc				\
	rw.doc					\
	scmsigs.doc				\
	script.doc				\
	simpos.doc				\
	smob.doc				\
	sort.doc				\
	srcprop.doc				\
	srfi-13.doc				\
	srfi-14.doc				\
	srfi-4.doc				\
	stackchk.doc				\
	stacks.doc				\
	stime.doc				\
	strings.doc				\
	strorder.doc				\
	strports.doc				\
	struct.doc				\
	symbols.doc				\
	threads.doc				\
	throw.doc				\
	unif.doc				\
	values.doc				\
	variable.doc				\
	vectors.doc				\
	version.doc				\
	vports.doc				\
	weaks.doc

EXTRA_DOT_DOC_FILES = @EXTRA_DOT_DOC_FILES@

DOT_I_FILES = vm-i-system.i vm-i-scheme.i vm-i-loader.i

.c.i:
	grep '^VM_DEFINE' $< > $@

BUILT_SOURCES = cpp_err_symbols.c cpp_sig_symbols.c libpath.h \
    version.h scmconfig.h \
    $(DOT_I_FILES) $(DOT_X_FILES) $(EXTRA_DOT_X_FILES)

EXTRA_libguile_la_SOURCES = _scm.h		\
    inet_aton.c memmove.c putenv.c strerror.c	\
    dynl.c regex-posix.c			\
    filesys.c posix.c net_db.c socket.c		\
    debug-malloc.c mkstemp.c			\
    win32-uname.c win32-dirent.c win32-socket.c	\
    locale-categories.h

## delete guile-snarf.awk from the installation bindir, in case it's
## lingering there due to an earlier guile version not having been
## wiped out.
install-exec-hook:
	rm -f $(DESTDIR)$(bindir)/guile-snarf.awk

## This is kind of nasty... there are ".c" files that we don't want to
## compile, since they are #included.  So instead we list them here.
## Perhaps we can deal with them normally once the merge seems to be
## working.
noinst_HEADERS = conv-integer.i.c conv-uinteger.i.c		\
                 eval.i.c ieee-754.h				\
                 srfi-4.i.c					\
                 quicksort.i.c                                  \
                 win32-uname.h win32-dirent.h win32-socket.h	\
	         private-gc.h private-options.h

# vm instructions
noinst_HEADERS += vm-engine.c vm-i-system.c vm-i-scheme.c vm-i-loader.c

libguile_la_DEPENDENCIES = @LIBLOBJS@
libguile_la_LIBADD = @LIBLOBJS@ $(gnulib_library)
libguile_la_LDFLAGS = @LTLIBINTL@ -version-info @LIBGUILE_INTERFACE_CURRENT@:@LIBGUILE_INTERFACE_REVISION@:@LIBGUILE_INTERFACE_AGE@ -export-dynamic -no-undefined

# These are headers visible as <guile/mumble.h>
pkginclude_HEADERS = 

# These are headers visible as <libguile/mumble.h>.
modincludedir = $(includedir)/libguile
modinclude_HEADERS =				\
	__scm.h					\
	alist.h					\
	arbiters.h				\
	array-handle.h				\
	array-map.h				\
	async.h					\
	backtrace.h				\
	boolean.h				\
	bytevectors.h				\
	chars.h					\
	continuations.h				\
	debug-malloc.h				\
	debug.h					\
	deprecated.h				\
	deprecation.h				\
	discouraged.h				\
	dynl.h					\
	dynwind.h				\
	eq.h					\
	error.h					\
	eval.h					\
	evalext.h				\
	extensions.h				\
	feature.h				\
	filesys.h				\
	fluids.h				\
	fports.h				\
	frames.h				\
	futures.h				\
	gc.h					\
	gdb_interface.h				\
	gdbint.h				\
	gettext.h				\
	goops.h					\
	gsubr.h					\
	guardians.h				\
	hash.h					\
	hashtab.h				\
	hooks.h					\
	i18n.h					\
	init.h					\
	inline.h				\
	instructions.h				\
	ioext.h					\
	iselect.h				\
	keywords.h				\
	lang.h					\
	list.h					\
	load.h					\
	macros.h				\
	mallocs.h				\
	modules.h				\
	net_db.h				\
	null-threads.h				\
	numbers.h				\
	objcodes.h				\
	objects.h				\
	objprop.h				\
	options.h				\
	pairs.h					\
	ports.h					\
	posix.h					\
	print.h					\
	procprop.h				\
	procs.h					\
	programs.h				\
	properties.h				\
	pthread-threads.h			\
	r6rs-ports.h				\
	random.h				\
	rdelim.h				\
	read.h					\
	regex-posix.h				\
	root.h					\
	rw.h					\
	scmsigs.h				\
	script.h				\
	simpos.h				\
	smob.h					\
	snarf.h					\
	socket.h				\
	sort.h					\
	srcprop.h				\
	srfi-13.h				\
	srfi-14.h				\
	srfi-4.h				\
	stackchk.h				\
	stacks.h				\
	stime.h					\
	strings.h				\
	strorder.h				\
	strports.h				\
	struct.h				\
	symbols.h				\
	tags.h					\
	threads.h				\
	throw.h					\
	unif.h					\
	validate.h				\
	values.h				\
	variable.h				\
	vectors.h				\
	vm-bootstrap.h				\
	vm-engine.h				\
	vm-expand.h				\
	vm.h					\
	vports.h				\
	weaks.h

nodist_modinclude_HEADERS = version.h scmconfig.h

bin_SCRIPTS = guile-snarf

# We can re-enable install for some of these if/when they are documented
# and people feel like maintaining them.  For now, this is not the case.
noinst_SCRIPTS = guile-doc-snarf guile-snarf-docs guile-func-name-check

EXTRA_DIST = ChangeLog-scm ChangeLog-threads		\
    ChangeLog-1996-1999 ChangeLog-2000 ChangeLog-2008 cpp_signal.c	\
    cpp_errno.c cpp_err_symbols.in cpp_err_symbols.c			\
    cpp_sig_symbols.c cpp_sig_symbols.in cpp_cnvt.awk			\
    c-tokenize.lex version.h.in						\
    scmconfig.h.top libgettext.h
#    $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES) \
#    guile-procedures.txt guile.texi

## We use @-...-@ as the substitution brackets here, instead of the
## usual @...@, so autoconf doesn't go and substitute the values
## directly into the left-hand sides of the sed substitutions.  *sigh*
version.h: version.h.in
	sed < $(srcdir)/version.h.in > $@.tmp \
	  -e s:@-GUILE_MAJOR_VERSION-@:${GUILE_MAJOR_VERSION}: \
	  -e s:@-GUILE_MINOR_VERSION-@:${GUILE_MINOR_VERSION}: \
	  -e s:@-GUILE_MICRO_VERSION-@:${GUILE_MICRO_VERSION}:
	mv $@.tmp $@

## FIXME: Consider using timestamp file, to avoid unnecessary rebuilds.
libpath.h: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	@echo "Generating libpath.h..."
	@rm -f libpath.tmp
	@echo '/* generated by Makefile */' > libpath.tmp
	@echo '#define SCM_PKGDATA_DIR "$(pkgdatadir)"' >> libpath.tmp
	@echo '#define SCM_LIBRARY_DIR "$(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)"'>>libpath.tmp
	@echo '#define SCM_SITE_DIR "$(pkgdatadir)/site"' >> libpath.tmp
	@echo '#define SCM_CCACHE_DIR "$(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/ccache"' >> libpath.tmp
	@echo '#define SCM_EFFECTIVE_VERSION "$(GUILE_EFFECTIVE_VERSION)"' >> libpath.tmp
	@echo '#define SCM_BUILD_INFO { \' >> libpath.tmp
	@echo '	{ "srcdir", "'"`cd @srcdir@; pwd`"'" }, \' >> libpath.tmp
	@echo ' { "top_srcdir",    "@top_srcdir_absolute@" }, \' >> libpath.tmp
	@echo '	{ "prefix",	   "@prefix@" }, \' >> libpath.tmp
	@echo '	{ "exec_prefix",   "@exec_prefix@" }, \' >> libpath.tmp
	@echo '	{ "bindir",	   "@bindir@" }, \' >> libpath.tmp
	@echo '	{ "sbindir",	   "@sbindir@" }, \' >> libpath.tmp
	@echo '	{ "libexecdir",	   "@libexecdir@" }, \' >> libpath.tmp
	@echo '	{ "datadir",	   "@datadir@" }, \' >> libpath.tmp
	@echo '	{ "sysconfdir",	   "@sysconfdir@" }, \' >> libpath.tmp
	@echo '	{ "sharedstatedir", "@sharedstatedir@" }, \' >> libpath.tmp
	@echo '	{ "localstatedir", "@localstatedir@" }, \' >> libpath.tmp
	@echo '	{ "libdir",	   "@libdir@" }, \' >> libpath.tmp
	@echo '	{ "ccachedir",	   SCM_CCACHE_DIR }, \' >> libpath.tmp
	@echo '	{ "infodir",	   "@infodir@" }, \' >> libpath.tmp
	@echo '	{ "mandir",	   "@mandir@" }, \' >> libpath.tmp
	@echo '	{ "includedir",	   "@includedir@" }, \' >> libpath.tmp
	@echo '	{ "pkgdatadir",	   "@pkgdatadir@" }, \' >> libpath.tmp
	@echo '	{ "pkglibdir",	   "@pkglibdir@" }, \' >> libpath.tmp
	@echo '	{ "pkgincludedir", "@pkgincludedir@" }, \' \
		>> libpath.tmp
	@echo '	{ "guileversion", "@GUILE_VERSION@" }, \' >> libpath.tmp
	@echo '	{ "libguileinterface", "@LIBGUILE_INTERFACE@" }, \' \
		>> libpath.tmp
	@echo '	{ "LIBS",	   "@GUILE_LIBS@" }, \' >> libpath.tmp
	@echo '	{ "CFLAGS",	   "@GUILE_CFLAGS@" }, \' >> libpath.tmp
	@echo '	{ "buildstamp",    "'"`date`"'" }, \' >> libpath.tmp
	@echo '}' >> libpath.tmp
	@mv libpath.tmp libpath.h


snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

SUFFIXES = .x .doc
.c.x:
	./guile-snarf -o $@ $< $(snarfcppopts)
.c.doc:
	-$(AWK) -f ./guile-func-name-check $<
	(./guile-snarf-docs $(snarfcppopts) $< | \
	./guile_filter_doc_snarfage$(EXEEXT) --filter-snarfage) > $@ || { rm $@; false; }

$(DOT_X_FILES) $(EXTRA_DOT_X_FILES): scmconfig.h snarf.h guile-snarf.in

$(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES): scmconfig.h snarf.h guile-snarf-docs.in guile_filter_doc_snarfage$(EXEEXT)

error.x: cpp_err_symbols.c
posix.x: cpp_sig_symbols.c
load.x: libpath.h

alldotdocfiles    = $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES)
snarf2checkedtexi = GUILE_AUTO_COMPILE=0 $(top_builddir)/meta/uninstalled-env guile-tools snarf-check-and-output-texi
dotdoc2texi       = cat $(alldotdocfiles) | $(snarf2checkedtexi)

guile.texi: $(alldotdocfiles) guile$(EXEEXT)
	$(dotdoc2texi) --manual > $@ || { rm $@; false; }

guile-procedures.texi: $(alldotdocfiles) guile$(EXEEXT)
	$(dotdoc2texi)          > $@ || { rm $@; false; }

if HAVE_MAKEINFO

guile-procedures.txt: guile-procedures.texi
	rm -f $@
	makeinfo --force -o $@ guile-procedures.texi || test -f $@

else

guile-procedures.txt: guile-procedures.texi
	cp guile-procedures.texi $@

endif

c-tokenize.c: c-tokenize.lex
	flex -t $(srcdir)/c-tokenize.lex > $@ || { rm $@; false; }

schemelibdir = $(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)
schemelib_DATA = guile-procedures.txt

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

cpp_err_symbols.c: cpp_err_symbols.in cpp_cnvt.awk
	$(AWK) -f $(srcdir)/cpp_cnvt.awk < $(srcdir)/cpp_err_symbols.in > \
		cpp_err_symbols.tmp
	mv cpp_err_symbols.tmp cpp_err_symbols.c

cpp_sig_symbols.c: cpp_sig_symbols.in cpp_cnvt.awk
	$(AWK) -f $(srcdir)/cpp_cnvt.awk < $(srcdir)/cpp_sig_symbols.in > \
		cpp_sig_symbols.tmp
	mv cpp_sig_symbols.tmp cpp_sig_symbols.c

## Create a new version of the cpp_sig_symbols.in file, including all SIGXXX
## macros defined on this platform.
check_signals:
	gcc -undef -dM -E $(srcdir)/cpp_signal.c | egrep ' SIG[A-Z]+' \
	  | cut -f2 -d' ' | sort > cpp_sig_symbols_here
	diff -u $(srcdir)/cpp_sig_symbols.in cpp_sig_symbols_here \
          | egrep '^\+S' \
	  | cut -c2- > cpp_sig_symbols_diff
	if test -s cpp_sig_symbols_diff ; then \
	  cat $(srcdir)/cpp_sig_symbols.in cpp_sig_symbols_diff \
	    | sort > cpp_sig_symbols_new ;\
	  echo "cpp_sig_symbols_new has the following additions:" ;\
	  cat cpp_sig_symbols_diff ;\
	else echo "No new symbols found."; \
	fi

## Likewise for cpp_err_symbols.in.
check_errnos:
	gcc -undef -dM -E $(srcdir)/cpp_errno.c | egrep ' E.+' \
	  | cut -f2 -d' ' | sort > cpp_err_symbols_here
	diff -u $(srcdir)/cpp_err_symbols.in cpp_err_symbols_here \
          | egrep '^\+E' \
	  | cut -c2- > cpp_err_symbols_diff
	if test -s cpp_err_symbols_diff ; then \
	  cat $(srcdir)/cpp_err_symbols.in cpp_err_symbols_diff \
	    | sort > cpp_err_symbols_new ;\
	  echo "cpp_err_symbols_new has the following additions:" ;\
	  cat cpp_err_symbols_diff ;\
	else echo "No new symbols found."; \
	fi

MOSTLYCLEANFILES = \
	cpp_err_symbols_here cpp_err_symbols_diff cpp_err_symbols_new \
	cpp_sig_symbols_here cpp_sig_symbols_diff cpp_sig_symbols_new \
	version.h version.h.tmp \
	scmconfig.h scmconfig.h.tmp

CLEANFILES = libpath.h *.x *.doc guile-procedures.txt guile-procedures.texi guile.texi \
	vm-i-*.i

MAINTAINERCLEANFILES = c-tokenize.c

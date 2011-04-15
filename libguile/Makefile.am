## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

include $(top_srcdir)/am/snarf

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
	      -I$(top_srcdir)/lib -I$(top_builddir)/lib $(LIBFFI_CFLAGS)

AM_CFLAGS = $(GCC_CFLAGS) $(CFLAG_VISIBILITY)

## The Gnulib Libtool archive.
gnulib_library = $(top_builddir)/lib/libgnu.la

ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

lib_LTLIBRARIES = libguile-@GUILE_EFFECTIVE_VERSION@.la
bin_PROGRAMS = guile

noinst_PROGRAMS = guile_filter_doc_snarfage gen-scmconfig

gen_scmconfig_SOURCES = gen-scmconfig.c

## Override default rule; this should be compiled for BUILD host.
## For some reason, OBJEXT does not include the dot
gen-scmconfig.$(OBJEXT): gen-scmconfig.c
	$(AM_V_GEN)							\
	if [ "$(cross_compiling)" = "yes" ]; then			\
		$(CC_FOR_BUILD) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES)	\
		  $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)	\
	          -c -o $@ $<;						\
	else								\
		$(COMPILE) -c -o $@ $<;					\
	fi

## Override default rule; this should run on BUILD host.
gen-scmconfig$(EXEEXT): $(gen_scmconfig_OBJECTS) $(gen_scmconfig_DEPENDENCIES)
	$(AM_V_GEN) \
	rm -f gen-scmconfig$(EXEEXT); \
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CCLD_FOR_BUILD) -o $@ $(gen_scmconfig_OBJECTS); \
	else \
		$(LINK) $(gen_scmconfig_OBJECTS) $(LDADD) $(LIBS); \
	fi

scmconfig.h: ${top_builddir}/config.h gen-scmconfig$(EXEEXT)
	$(AM_V_GEN)(rm -f scmconfig.h.tmp; \
	cat $(srcdir)/scmconfig.h.top > scmconfig.h.tmp; \
	./gen-scmconfig$(EXEEXT) >> scmconfig.h.tmp; \
	chmod 444 scmconfig.h.tmp; \
	rm -f scmconfig.h; \
	mv scmconfig.h.tmp scmconfig.h)


guile_filter_doc_snarfage_SOURCES = c-tokenize.c

## Override default rule; this should be compiled for BUILD host.
## For some reason, OBJEXT does not include the dot
c-tokenize.$(OBJEXT): c-tokenize.c
	$(AM_V_GEN) \
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CC_FOR_BUILD) -c -o $@ $<; \
	else \
		$(COMPILE) -c -o $@ $<; \
	fi

## Override default rule; this should run on BUILD host.
guile_filter_doc_snarfage$(EXEEXT): $(guile_filter_doc_snarfage_OBJECTS) $(guile_filter_doc_snarfage_DEPENDENCIES)
	$(AM_V_GEN) \
	rm -f guile_filter_doc_snarfage$(EXEEXT); \
	if [ "$(cross_compiling)" = "yes" ]; then \
		$(CCLD_FOR_BUILD) -o $@ $(guile_filter_doc_snarfage_OBJECTS); \
	else \
		$(LINK) $(guile_filter_doc_snarfage_OBJECTS) $(LDADD) $(LIBS); \
	fi


guile_SOURCES = guile.c
guile_CFLAGS = $(GUILE_CFLAGS) $(AM_CFLAGS)
guile_LDADD = libguile-@GUILE_EFFECTIVE_VERSION@.la
guile_LDFLAGS = $(GUILE_CFLAGS)

libguile_@GUILE_EFFECTIVE_VERSION@_la_CFLAGS = $(GUILE_CFLAGS) $(AM_CFLAGS)

libguile_@GUILE_EFFECTIVE_VERSION@_la_SOURCES =				\
	alist.c					\
	arbiters.c				\
	array-handle.c				\
	array-map.c				\
	arrays.c				\
	async.c					\
	backtrace.c				\
	boolean.c				\
	bitvectors.c				\
	bytevectors.c				\
	chars.c					\
	control.c				\
	continuations.c				\
	debug.c					\
	deprecated.c				\
	deprecation.c				\
	dynwind.c				\
	eq.c					\
	error.c					\
	eval.c					\
	evalext.c				\
	expand.c				\
	extensions.c				\
	feature.c				\
	filesys.c				\
	fluids.c				\
	foreign.c				\
	fports.c				\
	frames.c				\
	gc-malloc.c				\
	gc.c					\
	gdbint.c				\
	gettext.c				\
	generalized-arrays.c			\
	generalized-vectors.c			\
	goops.c					\
	gsubr.c					\
	guardians.c				\
	hash.c					\
	hashtab.c				\
	hooks.c					\
	i18n.c					\
	init.c					\
	inline.c				\
	instructions.c				\
	ioext.c					\
	keywords.c				\
	list.c					\
	load.c					\
	macros.c				\
	mallocs.c				\
	memoize.c				\
	modules.c				\
	null-threads.c				\
	numbers.c				\
	objcodes.c				\
	objprop.c				\
	options.c				\
	pairs.c					\
	poll.c					\
	ports.c					\
	print.c					\
	procprop.c				\
	procs.c					\
	programs.c				\
	promises.c				\
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
	srfi-1.c				\
	srfi-4.c				\
	srfi-13.c				\
	srfi-14.c				\
	srfi-60.c				\
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
	trees.c					\
	uniform.c				\
	values.c				\
	variable.c				\
	vectors.c				\
	version.c				\
	vm.c					\
	vports.c				\
	weaks.c

DOT_X_FILES =					\
	alist.x					\
	arbiters.x				\
	array-handle.x				\
	array-map.x				\
	arrays.x				\
	async.x					\
	backtrace.x				\
	boolean.x				\
	bitvectors.x				\
	bytevectors.x				\
	chars.x					\
	control.x				\
	continuations.x				\
	debug.x					\
	deprecated.x				\
	deprecation.x				\
	dynl.x					\
	dynwind.x				\
	eq.x					\
	error.x					\
	eval.x					\
	evalext.x				\
	expand.x				\
	extensions.x				\
	feature.x				\
	filesys.x				\
	fluids.x				\
	foreign.x				\
	fports.x				\
	gc-malloc.x				\
	gc.x					\
	gettext.x				\
	generalized-arrays.x			\
	generalized-vectors.x			\
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
	list.x					\
	load.x					\
	macros.x				\
	mallocs.x				\
	memoize.x				\
	modules.x				\
	numbers.x				\
	objprop.x				\
	options.x				\
	pairs.x					\
	ports.x					\
	print.x					\
	procprop.x				\
	procs.x					\
	promises.x				\
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
	srfi-1.x				\
	srfi-4.x				\
	srfi-13.x				\
	srfi-14.x				\
	srfi-60.x				\
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
	trees.x					\
	uniform.x				\
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
	arrays.doc				\
	async.doc				\
	backtrace.doc				\
	boolean.doc				\
	bitvectors.doc				\
	bytevectors.doc				\
	chars.doc				\
	control.doc				\
	continuations.doc			\
	debug.doc				\
	deprecated.doc				\
	deprecation.doc				\
	dynl.doc				\
	dynwind.doc				\
	eq.doc					\
	error.doc				\
	eval.doc				\
	evalext.doc				\
	expand.doc				\
	extensions.doc				\
	feature.doc				\
	filesys.doc 				\
	fluids.doc				\
	foreign.doc				\
	fports.doc				\
	gc-malloc.doc				\
	gc.doc					\
	gettext.doc				\
	generalized-arrays.doc			\
	generalized-vectors.doc			\
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
	list.doc				\
	load.doc				\
	macros.doc				\
	mallocs.doc				\
	memoize.doc				\
	modules.doc				\
	numbers.doc				\
	objprop.doc				\
	options.doc				\
	pairs.doc				\
	ports.doc				\
	print.doc				\
	procprop.doc				\
	procs.doc				\
	promises.doc				\
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
	srfi-1.doc				\
	srfi-4.doc				\
	srfi-13.doc				\
	srfi-14.doc				\
	srfi-60.doc				\
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
	trees.doc				\
	uniform.doc				\
	values.doc				\
	variable.doc				\
	vectors.doc				\
	version.doc				\
	vports.doc				\
	weaks.doc

EXTRA_DOT_DOC_FILES = @EXTRA_DOT_DOC_FILES@

DOT_I_FILES = vm-i-system.i vm-i-scheme.i vm-i-loader.i

.c.i:
	$(AM_V_GEN)$(GREP) '^VM_DEFINE' $< > $@

BUILT_SOURCES = cpp-E.c cpp-SIG.c libpath.h \
    scmconfig.h \
    $(DOT_I_FILES) $(DOT_X_FILES) $(EXTRA_DOT_X_FILES)

EXTRA_libguile_@GUILE_EFFECTIVE_VERSION@_la_SOURCES = _scm.h		\
    memmove.c strerror.c			\
    dynl.c regex-posix.c			\
    posix.c net_db.c socket.c			\
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
                 ieee-754.h					\
                 srfi-14.i.c					\
                 quicksort.i.c                                  \
                 win32-uname.h win32-dirent.h win32-socket.h	\
		 private-gc.h private-options.h

# vm instructions
noinst_HEADERS += vm-engine.c vm-i-system.c vm-i-scheme.c vm-i-loader.c

libguile_@GUILE_EFFECTIVE_VERSION@_la_DEPENDENCIES = @LIBLOBJS@

libguile_@GUILE_EFFECTIVE_VERSION@_la_LIBADD =		\
  @LIBLOBJS@ $(gnulib_library) $(LTLIBGMP) $(LTLIBLTDL)

# Libtool version info.
version_info = @LIBGUILE_INTERFACE_CURRENT@:@LIBGUILE_INTERFACE_REVISION@:@LIBGUILE_INTERFACE_AGE@

libguile_@GUILE_EFFECTIVE_VERSION@_la_LDFLAGS =	\
  $(BDW_GC_LIBS) $(LIBFFI_LIBS)			\
  $(CEIL_LIBM)					\
  $(FLOOR_LIBM)					\
  $(FREXP_LIBM)					\
  $(GETADDRINFO_LIB)				\
  $(HOSTENT_LIB)				\
  $(INET_NTOP_LIB)				\
  $(INET_PTON_LIB)				\
  $(ISNAND_LIBM)				\
  $(ISNANF_LIBM)				\
  $(ISNANL_LIBM)				\
  $(LDEXP_LIBM)					\
  $(LIBSOCKET)					\
  $(LOG1P_LIBM)					\
  $(LTLIBICONV)					\
  $(LTLIBINTL)					\
  $(LTLIBUNISTRING)				\
  $(SERVENT_LIB)				\
  $(TRUNC_LIBM)					\
  -version-info $(version_info)			\
  -export-dynamic -no-undefined			\
  $(GNU_LD_FLAGS)

if HAVE_LD_VERSION_SCRIPT

libguile_@GUILE_EFFECTIVE_VERSION@_la_LDFLAGS += -Wl,--version-script="$(srcdir)/libguile.map"

endif HAVE_LD_VERSION_SCRIPT


# These are headers visible as <guile/mumble.h>
pkginclude_HEADERS =

# These are headers visible as <libguile/mumble.h>.
modincludedir = $(pkgincludedir)/$(GUILE_EFFECTIVE_VERSION)/libguile
modinclude_HEADERS =				\
	__scm.h					\
	alist.h					\
	arbiters.h				\
	array-handle.h				\
	array-map.h				\
	arrays.h				\
	async.h					\
	backtrace.h				\
	bdw-gc.h				\
	boolean.h				\
	bitvectors.h				\
	bytevectors.h				\
	chars.h					\
	control.h				\
	continuations.h				\
	debug-malloc.h				\
	debug.h					\
	deprecated.h				\
	deprecation.h				\
	dynl.h					\
	dynwind.h				\
	eq.h					\
	error.h					\
	eval.h					\
	evalext.h				\
	expand.h				\
	extensions.h				\
	feature.h				\
	filesys.h				\
	fluids.h				\
	foreign.h				\
	fports.h				\
	frames.h				\
	gc.h					\
	gdb_interface.h				\
	gdbint.h				\
	gettext.h				\
	generalized-arrays.h			\
	generalized-vectors.h			\
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
	list.h					\
	load.h					\
	macros.h				\
	mallocs.h				\
	memoize.h				\
	modules.h				\
	net_db.h				\
	null-threads.h				\
	numbers.h				\
	objcodes.h				\
	objprop.h				\
	options.h				\
	pairs.h					\
	poll.h					\
	ports.h					\
	posix.h					\
	print.h					\
	procprop.h				\
	procs.h					\
	programs.h				\
	promises.h				\
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
	srfi-1.h				\
	srfi-4.h				\
	srfi-13.h				\
	srfi-14.h				\
	srfi-60.h				\
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
	trees.h					\
	validate.h				\
	uniform.h				\
	values.h				\
	variable.h				\
	vectors.h				\
	vm-engine.h				\
	vm-expand.h				\
	vm.h					\
	vports.h				\
	weaks.h

nodist_modinclude_HEADERS = version.h scmconfig.h

bin_SCRIPTS = guile-snarf

# We can re-enable install for some of these if/when they are documented
# and people feel like maintaining them.  For now, this is not the case.
noinst_SCRIPTS = guile-snarf-docs

EXTRA_DIST = ChangeLog-scm ChangeLog-threads		\
    ChangeLog-1996-1999 ChangeLog-2000 ChangeLog-2008	\
    guile-func-name-check				\
    cpp-E.syms cpp-E.c cpp-SIG.syms cpp-SIG.c			\
    c-tokenize.lex						\
    scmconfig.h.top libgettext.h unidata_to_charset.pl libguile.map
#    $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES) \
#    guile-procedures.txt guile.texi

## FIXME: Consider using timestamp file, to avoid unnecessary rebuilds.
libpath.h: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	@rm -f libpath.tmp
	@echo '/* generated by Makefile */' > libpath.tmp
	@echo '#define SCM_PKGDATA_DIR "$(pkgdatadir)"' >> libpath.tmp
	@echo '#define SCM_LIBRARY_DIR "$(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)"'>>libpath.tmp
	@echo '#define SCM_SITE_DIR "$(sitedir)"' >> libpath.tmp
	@echo '#define SCM_GLOBAL_SITE_DIR "$(pkgdatadir)/site"' >> libpath.tmp
	@echo '#define SCM_LIB_DIR "$(libdir)"' >> libpath.tmp
	@echo '#define SCM_EXTENSIONS_DIR "$(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/extensions"' >> libpath.tmp
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
	@echo '	{ "pkgdatadir",	   "$(pkgdatadir)" }, \' >> libpath.tmp
	@echo '	{ "pkglibdir",	   "$(pkglibdir)" }, \' >> libpath.tmp
	@echo '	{ "pkgincludedir", "$(pkgincludedir)" }, \' \
		>> libpath.tmp
	@echo '	{ "extensiondir", "$(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/extensions" }, \' >> libpath.tmp
	@echo '	{ "guileversion", "@GUILE_VERSION@" }, \' >> libpath.tmp
	@echo '	{ "libguileinterface", "@LIBGUILE_INTERFACE@" }, \' \
		>> libpath.tmp
	@echo '	{ "LIBS",	   "@GUILE_LIBS@" }, \' >> libpath.tmp
	@echo '	{ "CFLAGS",	   "@GUILE_CFLAGS@" }, \' >> libpath.tmp
	@echo '	{ "buildstamp",    "'`date -u +'%Y-%m-%d %T'`'" }, \' >> libpath.tmp
	@echo '}' >> libpath.tmp
	$(AM_V_GEN)mv libpath.tmp libpath.h


snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)

SUFFIXES = .x .doc

.c.x:
	$(AM_V_SNARF)$(GUILE_SNARF) -o $@ $< $(snarfcppopts)
.c.doc:
	$(AM_V_SNARF)./guile-snarf-docs -o $@ $< -- $(snarfcppopts)

$(DOT_X_FILES) $(EXTRA_DOT_X_FILES): scmconfig.h snarf.h guile-snarf.in version.h

$(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES): scmconfig.h snarf.h guile-snarf-docs.in guile_filter_doc_snarfage$(EXEEXT)

error.x: cpp-E.c
posix.x: cpp-SIG.c
load.x: libpath.h

alldotdocfiles    = $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES)
snarf2checkedtexi = GUILE_AUTO_COMPILE=0 $(top_builddir)/meta/uninstalled-env guile-tools snarf-check-and-output-texi
dotdoc2texi       = cat $(alldotdocfiles) | $(snarf2checkedtexi)

guile.texi: $(alldotdocfiles) guile$(EXEEXT)
	$(dotdoc2texi) --manual > $@ || { rm $@; false; }

guile-procedures.texi: $(alldotdocfiles) guile$(EXEEXT)
	$(AM_V_GEN)$(dotdoc2texi)          > $@ || { rm $@; false; }

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


# This page is for maintenance of the lists of CPP symbols that are eventually
# included in error.c (‘errno’ values: E*) and posix.c (signal names: SIG*),
# in the funcs scm_init_{error,posix}, respectively.
#
# The lists (files cpp-{E,SIG}.syms) are not included verbatim, but processed
# (via pattern rule ‘.syms.c’ below) so that each CPP symbol is transformed to
# fragment of #ifdef'd C code that defines a Scheme variable of the same name.
# The resulting files, cpp-{E,SIG}.c, are the ones #include:d.
#
# To maintain the (source) lists of symbols, we use a GCC-specific feature
# to list all symbols #define:d when expanding <{errno,signal}.h>, grep
# the output for {E,SIG}*, massage the result into a usable form, and
# construct a union of the current and new symbols.  This is written to
# files cpp-{E,SIG}.syms.NEW for review.  If things look ok, you can then
# "mv foo.NEW foo" and commit.  Generating the foo.NEW files and showing
# an informative message is the job of targets chknew-{E,SIG}.

# For each line in foo.syms, transform:
#	SYMBOL
# to:
#	#ifdef SYMBOL
#	scm_c_define ("SYMBOL", scm_from_int (SYMBOL));
#	#endif
# writing output to foo.c.  We use a while-read loop instead of a
# one-line sed script because some seds (e.g., FreeBSD) don't read
# '\n' as a newline (like GNU sed does).  Sad, but true.
.syms.c:
	$(AM_V_GEN) 							\
	cat $< | while read sym ; do					\
	  echo "#ifdef $$sym" ;						\
	  echo "scm_c_define (\"$$sym\", scm_from_int ($$sym));" ;	\
	  echo "#endif" ;						\
	done > $@T
	@mv $@T $@

# Write $(srcdir)/cpp-{E,SIG}.syms.NEW if there are any not-yet-seen
# ("new" to us) E* or SIG* symbols in <errno.h> or <signal.h>, respectively.
chknew-E chknew-SIG:						\
	@bit=`echo $@ | sed s/^chknew-//` ;			\
	old="$(srcdir)/cpp-$$bit.syms" ;			\
	echo "#include <$${bit}.h>"				\
	 | sed 's/E/errno/;s/SIG/signal/'			\
	 | gcc -dM -E -						\
	 | sed 's/^#define //;/^'$$bit'[A-Z][A-Z]*/!d;s/ .*//'	\
	 | sort | diff -u $$old - | sed '1,2d;/^+/!d;s/^.//'	\
	 > TMP ;						\
	if [ -s TMP ] ; then new="$$old.NEW" ;			\
	  cat $$old TMP | sort > $$new ;			\
	  echo "$$new: `sed -n '$$=' TMP` new symbol(s)." ;	\
	  sed 's/^/  /' TMP ;					\
	else echo "No new symbols found." ;			\
	fi ; rm TMP



MOSTLYCLEANFILES = \
	scmconfig.h scmconfig.h.tmp

CLEANFILES = libpath.h *.x *.doc guile-procedures.txt guile-procedures.texi guile.texi \
	vm-i-*.i

MAINTAINERCLEANFILES = c-tokenize.c

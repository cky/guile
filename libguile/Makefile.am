## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##   
##   GUILE is free software; you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as
##   published by the Free Software Foundation; either version 2, or
##   (at your option) any later version.
##   
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##   
##   You should have received a copy of the GNU General Public
##   License along with GUILE; see the file COPYING.  If not, write
##   to the Free Software Foundation, Inc., 59 Temple Place, Suite
##   330, Boston, MA 02111-1307 USA

AUTOMAKE_OPTIONS = foreign

## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.
INCLUDES = -I.. -I$(srcdir)/.. ${THREAD_CPPFLAGS}

ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(PROC\|PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/'

lib_LTLIBRARIES = libguile.la
bin_PROGRAMS = guile

guile_SOURCES = guile.c
guile_LDADD = libguile.la ${THREAD_LIBS_LOCAL}

libguile_la_SOURCES = \
    alist.c arbiters.c async.c backtrace.c boolean.c chars.c		\
    continuations.c debug.c dynl.c dynwind.c eq.c error.c eval.c	\
    evalext.c feature.c filesys.c fluids.c fports.c gc.c gdbint.c	\
    gh_data.c gh_eval.c gh_funcs.c gh_init.c gh_io.c gh_list.c		\
    gh_predicates.c gsubr.c guardians.c hash.c hashtab.c init.c		\
    ioext.c keywords.c lang.c list.c load.c macros.c mallocs.c		\
    modules.c net_db.c numbers.c objects.c objprop.c options.c pairs.c	\
    ports.c posix.c print.c procprop.c procs.c ramap.c random.c read.c	\
    root.c scmsigs.c script.c simpos.c smob.c socket.c sort.c		\
    srcprop.c stackchk.c stacks.c stime.c strings.c strop.c strorder.c	\
    strports.c struct.c symbols.c tag.c throw.c unif.c variable.c	\
    vectors.c version.c vports.c weaks.c

BUILT_SOURCES = \
    cpp_err_symbols.c cpp_sig_symbols.c libpath.h alist.x arbiters.x	\
    async.x backtrace.x boolean.x chars.x continuations.x debug.x	\
    dynl.x dynwind.x eq.x error.x eval.x evalext.x feature.x filesys.x	\
    fluids.x fports.x gc.x gdbint.x gh_data.x gh_eval.x			\
    gh_funcs.x gh_init.x gh_io.x gh_list.x gh_predicates.x gsubr.x	\
    guardians.x hash.x hashtab.x init.x ioext.x iselect.x keywords.x	\
    lang.x list.x load.x macros.x mallocs.x modules.x net_db.x		\
    numbers.x objects.x objprop.x options.x pairs.x ports.x posix.x	\
    print.x procprop.x procs.x random.x ramap.x read.x regex-posix.x	\
    root.x scmsigs.x script.x simpos.x smob.x socket.x sort.x		\
    srcprop.x stackchk.x stacks.x stime.x strings.x strop.x strorder.x	\
    strports.x struct.x symbols.x tag.x threads.x throw.x unif.x	\
    variable.x vectors.x version.x vports.x weaks.x

EXTRA_libguile_la_SOURCES = _scm.h \
    strerror.c inet_aton.c putenv.c \
    threads.c alloca.c \
    regex-posix.c iselect.c

## This is kind of nasty... there are ".c" files that we don't want to
## compile, since they are #included in threads.c.  So instead we list
## them here.  Perhaps we can deal with them normally once the merge
## seems to be working.
noinst_HEADERS = coop-threads.c coop-threads.h coop.c

libguile_la_DEPENDENCIES = @LIBLOBJS@
libguile_la_LIBADD = @LIBLOBJS@
libguile_la_LDFLAGS = -version-info 4:0 -export-dynamic

include_HEADERS = libguile.h

# These are headers visible as <guile/mumble.h> 
pkginclude_HEADERS = gh.h

# These are headers visible as <libguile/mumble.h>.
modincludedir = $(includedir)/libguile
modinclude_HEADERS = \
    __scm.h alist.h arbiters.h async.h backtrace.h boolean.h chars.h	\
    continuations.h debug.h dynl.h dynwind.h eq.h error.h eval.h	\
    evalext.h feature.h filesys.h fports.h gc.h gdb_interface.h		\
    gdbint.h genio.h gsubr.h guardians.h hash.h hashtab.h init.h	\
    ioext.h keywords.h kw.h lang.h list.h load.h macros.h mallocs.h 	\
    modules.h net_db.h numbers.h objects.h objprop.h options.h pairs.h	\
    ports.h posix.h regex-posix.h print.h procprop.h procs.h random.h	\
    ramap.h read.h root.h scmsigs.h script.h simpos.h smob.h socket.h	\
    sort.h srcprop.h stackchk.h stacks.h stime.h strings.h strop.h	\
    strorder.h strports.h struct.h symbols.h tag.h tags.h throw.h	\
    unif.h variable.h vectors.h version.h vports.h weaks.h snarf.h	\
    threads.h coop-defs.h fluids.h iselect.h

## This file is generated at configure time.  That is why it is DATA
## and not a header -- headers are included in the distribution.
modinclude_DATA = scmconfig.h

bin_SCRIPTS = guile-snarf

check_ldadd = libguile.la ${THREAD_LIBS_LOCAL}
check_PROGRAMS = gh_test_c gh_test_repl
gh_test_c_SOURCES = gh_test_c.c
gh_test_c_LDADD = ${check_ldadd}
gh_test_repl_SOURCES = gh_test_repl.c
gh_test_repl_LDADD = ${check_ldadd}

check-local:
	echo '(+ 1 2 3)' | ./gh_test_c; echo
	echo '(+ 1 2 3)' | ./gh_test_repl; echo

EXTRA_DIST = ChangeLog-gh ChangeLog-scm ChangeLog-threads dynl-dl.c \
    dynl-dld.c dynl-shl.c dynl-vms.c DYNAMIC-LINKING cpp_signal.c cpp_errno.c \
    cpp_err_symbols.in cpp_sig_symbols.in cpp_cnvt.awk coop-threads.h.cygnus \
    coop-threads.c.cygnus mit-pthreads.h mit-pthreads.c fsu-pthreads.h

## FIXME: Consider using timestamp file, to avoid unnecessary rebuilds.
libpath.h: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	@echo "Generating libpath.h..." 
	@rm -f libpath.tmp
	@echo '/* generated by Makefile */' > libpath.tmp
	@echo '#define SCM_PKGDATA_DIR "$(pkgdatadir)"' >> libpath.tmp
	@echo '#define SCM_LIBRARY_DIR "$(pkgdatadir)/$(VERSION)"'>>libpath.tmp
	@echo '#define SCM_SITE_DIR "$(pkgdatadir)/site"' >> libpath.tmp
	@echo '#define SCM_BUILD_INFO { \' >> libpath.tmp
	@echo '	{ "srcdir", "'"`cd @srcdir@; pwd`"'" }, \' >> libpath.tmp
	@echo '	{ "top_srcdir", "'"`cd @top_srcdir@; pwd`"'" }, \' \
		>> libpath.tmp
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
	@echo '	{ "infodir",	   "@infodir@" }, \' >> libpath.tmp
	@echo '	{ "mandir",	   "@mandir@" }, \' >> libpath.tmp
	@echo '	{ "includedir",	   "@includedir@" }, \' >> libpath.tmp
	@echo '	{ "pkgdatadir",	   "$(datadir)/@PACKAGE@" }, \' >> libpath.tmp
	@echo '	{ "pkglibdir",	   "$(libdir)/@PACKAGE@" }, \' >> libpath.tmp
	@echo '	{ "pkgincludedir", "$(includedir)/@PACKAGE@" }, \' \
		>> libpath.tmp
	@echo '	{ "LIBS",	   "@GUILE_LIBS@" }, \' >> libpath.tmp
	@echo '}' >> libpath.tmp
	@mv libpath.tmp libpath.h

SUFFIXES = .x
.c.x:
	./guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

cpp_err_symbols.c: cpp_err_symbols.in
	$(AWK) -f $(srcdir)/cpp_cnvt.awk < $(srcdir)/cpp_err_symbols.in > \
		cpp_err_symbols.tmp
	mv cpp_err_symbols.tmp cpp_err_symbols.c

cpp_sig_symbols.c: cpp_sig_symbols.in
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
	cpp_sig_symbols_here cpp_sig_symbols_diff cpp_sig_symbols_new

CLEANFILES = versiondat.h libpath.h

DISTCLEANFILES = *.x


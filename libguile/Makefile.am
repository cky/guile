## Process this file with Automake to create Makefile.in
##
##  	Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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

## Prevent automake from adding extra -I options
DEFS = @DEFS@
## Check for headers in $(srcdir)/.., so that #include
## <libguile/MUMBLE.h> will find MUMBLE.h in this dir when we're
## building.
INCLUDES = -I.. -I$(srcdir)/.. ${INCLTDL}

ETAGS_ARGS = --regex='/SCM_\(GLOBAL_\)?\(G?PROC\|G?PROC1\|SYMBOL\|VCELL\|CONST_LONG\).*\"\([^\"]\)*\"/\3/' \
   --regex='/[ \t]*SCM_[G]?DEFINE1?[ \t]*(\([^,]*\),[^,]*/\1/'

lib_LTLIBRARIES = libguile.la
bin_PROGRAMS = guile guile_filter_doc_snarfage

guile_SOURCES = guile.c
guile_LDADD = libguile.la ${THREAD_LIBS_LOCAL}
guile_LDFLAGS = @DLPREOPEN@

guile_filter_doc_snarfage_SOURCES = c-tokenize.c

libguile_la_SOURCES = alist.c arbiters.c async.c backtrace.c boolean.c	    \
    chars.c continuations.c debug.c deprecation.c dynl.c dynwind.c	    \
    environments.c eq.c error.c eval.c evalext.c extensions.c		    \
    feature.c fluids.c fports.c						    \
    gc.c gc_os_dep.c gdbint.c gh_data.c gh_eval.c gh_funcs.c gh_init.c	    \
    gh_io.c gh_list.c gh_predicates.c goops.c gsubr.c guardians.c hash.c    \
    hashtab.c hooks.c init.c ioext.c iselect.c keywords.c lang.c list.c	    \
    load.c macros.c mallocs.c modules.c numbers.c objects.c objprop.c	    \
    options.c pairs.c ports.c print.c procprop.c procs.c properties.c	    \
    random.c rdelim.c read.c root.c rw.c scmsigs.c script.c simpos.c smob.c \
    sort.c srcprop.c stackchk.c stacks.c stime.c strings.c strop.c	    \
    strorder.c strports.c struct.c symbols.c throw.c values.c 		    \
    variable.c vectors.c version.c vports.c weaks.c symbols-deprecated.c

DOT_X_FILES = alist.x arbiters.x async.x backtrace.x boolean.x chars.x	  \
    continuations.x debug.x deprecation.x dynl.x dynwind.x		  \
    environments.x eq.x							  \
    error.x eval.x evalext.x extensions.x feature.x fluids.x fports.x	  \
    gc.x goops.x							  \
    gsubr.x guardians.x hash.x hashtab.x hooks.x init.x ioext.x iselect.x \
    keywords.x lang.x list.x load.x macros.x mallocs.x modules.x	  \
    numbers.x objects.x objprop.x options.x pairs.x ports.x print.x	  \
    procprop.x procs.x properties.x random.x rdelim.x read.x root.x rw.x  \
    scmsigs.x script.x simpos.x smob.x sort.x srcprop.x			  \
    stackchk.x stacks.x stime.x strings.x strop.x strorder.x strports.x	  \
    struct.x symbols.x throw.x values.x variable.x vectors.x		  \
    version.x vports.x weaks.x symbols-deprecated.x

EXTRA_DOT_X_FILES = @EXTRA_DOT_X_FILES@

DOT_DOC_FILES = alist.doc arbiters.doc async.doc backtrace.doc		    \
    boolean.doc chars.doc continuations.doc debug.doc dynl.doc		    \
    dynwind.doc environments.doc eq.doc error.doc eval.doc evalext.doc	    \
    extensions.doc feature.doc fluids.doc fports.doc gc.doc goops.doc	    \
    gsubr.doc								    \
    guardians.doc hash.doc hashtab.doc hooks.doc init.doc ioext.doc	    \
    iselect.doc keywords.doc lang.doc list.doc load.doc macros.doc	    \
    mallocs.doc modules.doc numbers.doc objects.doc objprop.doc		    \
    options.doc pairs.doc ports.doc print.doc procprop.doc		    \
    procs.doc properties.doc random.doc rdelim.doc read.doc root.doc rw.doc \
    scmsigs.doc script.doc simpos.doc smob.doc sort.doc			    \
    srcprop.doc stackchk.doc stacks.doc stime.doc strings.doc strop.doc	    \
    strorder.doc strports.doc struct.doc symbols.doc throw.doc		    \
    values.doc variable.doc vectors.doc version.doc vports.doc weaks.doc    \
    symbols-deprecated.doc

EXTRA_DOT_DOC_FILES = @EXTRA_DOT_DOC_FILES@

BUILT_SOURCES = cpp_err_symbols.c cpp_sig_symbols.c libpath.h scmconfig.h \
    $(DOT_X_FILES) $(EXTRA_DOT_X_FILES)

EXTRA_libguile_la_SOURCES = _scm.h			\
    alloca.c inet_aton.c memmove.c putenv.c strerror.c	\
    threads.c regex-posix.c				\
    filesys.c posix.c net_db.c socket.c			\
    ramap.c unif.c debug-malloc.c

## In next release, threads will be factored out of libguile.
## Until then, the machine specific headers is a temporary kludge.
OMIT_DEPENDENCIES = libguile.h ltdl.h \
    axp.h hppa.h i386.h ksr.h m88k.h mips.h sparc.h vax.h

## delete guile-snarf.awk from the installation bindir, in case it's
## lingering there due to an earlier guile version not having been
## wiped out.
install-exec-hook:
	rm -f $(bindir)/guile-snarf.awk

## This is kind of nasty... there are ".c" files that we don't want to
## compile, since they are #included.  So instead we list them here.
## Perhaps we can deal with them normally once the merge seems to be
## working.
noinst_HEADERS = coop-threads.c coop-threads.h coop.c num2integral.i.c

libguile_la_DEPENDENCIES = @LIBLOBJS@
libguile_la_LIBADD = @LIBLOBJS@ $(LIBLTDL)
libguile_la_LDFLAGS = -version-info @LIBGUILE_MAJOR_VERSION@:@LIBGUILE_MINOR_VERSION@:@LIBGUILE_REVISION_VERSION@ -export-dynamic

# These are headers visible as <guile/mumble.h> 
pkginclude_HEADERS = gh.h

# These are headers visible as <libguile/mumble.h>.
modincludedir = $(includedir)/libguile
modinclude_HEADERS = __scm.h alist.h arbiters.h async.h backtrace.h boolean.h \
    chars.h continuations.h coop-defs.h debug.h debug-malloc.h deprecation.h  \
    dynl.h dynwind.h environments.h eq.h error.h eval.h evalext.h	      \
    extensions.h feature.h filesys.h fluids.h fports.h gc.h		      \
    gdb_interface.h gdbint.h						      \
    goops.h gsubr.h guardians.h hash.h hashtab.h hooks.h init.h ioext.h	      \
    iselect.h keywords.h lang.h list.h load.h macros.h mallocs.h modules.h    \
    net_db.h numbers.h objects.h objprop.h options.h pairs.h ports.h posix.h  \
    regex-posix.h print.h procprop.h procs.h properties.h random.h ramap.h    \
    rdelim.h read.h root.h rw.h scmsigs.h validate.h script.h simpos.h smob.h \
    snarf.h socket.h sort.h srcprop.h stackchk.h stacks.h stime.h strings.h   \
    strop.h strorder.h strports.h struct.h symbols.h tags.h threads.h	      \
    throw.h unif.h values.h variable.h vectors.h version.h vports.h weaks.h

## This file is generated at configure time.  That is why it is DATA
## and not a header -- headers are included in the distribution.
modinclude_DATA = scmconfig.h

bin_SCRIPTS = guile-snarf guile-doc-snarf guile-snarf-docs \
    guile-snarf-docs-texi guile-func-name-check

EXTRA_DIST = ChangeLog-gh ChangeLog-scm ChangeLog-threads cpp_signal.c	\
    cpp_errno.c cpp_err_symbols.in cpp_sig_symbols.in cpp_cnvt.awk
#    $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES) \
#    guile-procedures.txt guile.texi

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
	@echo '	{ "guileversion", "@GUILE_VERSION@" }, \' >> libpath.tmp
	@echo '	{ "libguileversion", "@LIBGUILE_VERSION@" }, \' >> libpath.tmp
	@echo '	{ "LIBS",	   "@GUILE_LIBS@" }, \' >> libpath.tmp
	@echo '}' >> libpath.tmp
	@mv libpath.tmp libpath.h


SUFFIXES = .x .doc
.c.x:
	./guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }
.c.doc:
	-(test -n "${AWK+set}" || AWK="@AWK@"; ${AWK} -f ./guile-func-name-check $<)
	./guile-snarf-docs $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
        || { rm $@; false; }

$(DOT_X_FILES) $(EXTRA_DOT_DOC_FILES): snarf.h guile-snarf.in

$(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES): snarf.h guile-snarf-docs.in

error.x: cpp_err_symbols.c
posix.x: cpp_sig_symbols.c
load.x: libpath.h

guile.texi: $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES) guile-snarf-docs-texi.in guile guile_filter_doc_snarfage
	./guile-snarf-docs-texi $(DOT_DOC_FILES) $(EXTRA_DOT_DOC_FILES) > $@ \
	|| { rm $@; false; }

guile-procedures.txt: guile.texi
	rm -f $@
	makeinfo --force -o $@ $< || test -f $@

schemelibdir = $(pkgdatadir)/$(VERSION)
schemelib_DATA = guile-procedures.txt

## Add -MG to make the .x magic work with auto-dep code.
MKDEP = gcc -M -MG $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)

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
	cpp_sig_symbols_here cpp_sig_symbols_diff cpp_sig_symbols_new

CLEANFILES = libpath.h *.x *.doc guile-procedures.txt guile.texi

MAINTAINERCLEANFILES = c-tokenize.c

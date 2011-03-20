## Process this file with Automake to create Makefile.in
##
##   Copyright (C) 1998, 2004, 2006, 2008, 2009, 2010,
##     2011 Free Software Foundation, Inc.
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

BUILT_SOURCES = standard-library.texi

info_TEXINFOS = guile.texi

guile_TEXINFOS = preface.texi			\
                 intro.texi			\
                 tour.texi			\
                 libguile-program.texi		\
                 scheme-intro.texi		\
		 api-scm.texi			\
		 api-snarf.texi			\
		 api-smobs.texi			\
		 scheme-ideas.texi		\
		 api-data.texi			\
		 api-procedures.texi		\
		 api-macros.texi		\
		 api-utility.texi		\
		 api-binding.texi		\
		 api-control.texi		\
		 api-io.texi			\
		 api-foreign.texi		\
		 api-regex.texi			\
		 api-lalr.texi			\
		 api-languages.texi		\
		 api-evaluation.texi		\
		 api-memory.texi		\
		 api-modules.texi		\
		 api-scheduling.texi		\
		 api-options.texi		\
		 api-i18n.texi			\
		 api-debug.texi			\
		 api-coverage.texi		\
		 scheme-reading.texi		\
		 scheme-indices.texi		\
		 slib.texi			\
		 posix.texi			\
		 web.texi			\
		 expect.texi			\
		 scsh.texi			\
		 sxml-match.texi		\
		 scheme-scripts.texi		\
		 api-overview.texi		\
		 api-deprecated.texi		\
		 scheme-using.texi		\
		 indices.texi			\
		 script-getopt.texi		\
		 data-rep.texi			\
		 repl-modules.texi		\
		 srfi-modules.texi		\
		 r6rs.texi			\
		 match.texi			\
		 misc-modules.texi		\
		 api-compound.texi		\
		 autoconf.texi			\
		 autoconf-macros.texi		\
		 tools.texi			\
		 history.texi			\
		 vm.texi			\
		 compiler.texi			\
		 fdl.texi			\
		 libguile-concepts.texi		\
		 libguile-smobs.texi		\
		 libguile-snarf.texi		\
		 libguile-linking.texi		\
	 	 libguile-extensions.texi	\
		 api-init.texi			\
		 mod-getopt-long.texi		\
		 goops.texi			\
		 goops-tutorial.texi		\
		 effective-version.texi

ETAGS_ARGS = $(info_TEXINFOS) $(guile_TEXINFOS)

PICTURES = hierarchy.eps \
	   hierarchy.pdf \
	   hierarchy.png \
	   hierarchy.txt \
	   gds.eps \
	   gds.pdf \
	   gds.txt \
	   scheme.eps \
	   scheme.pdf \
	   scheme.txt \
	   mop.text

noinst_DATA = $(PICTURES)

EXTRA_DIST = ChangeLog-2008 $(PICTURES)

# Automated snarfing

autoconf.texi: autoconf-macros.texi
autoconf-macros.texi: $(top_srcdir)/meta/guile.m4
	GUILE_AUTO_COMPILE=0 $(top_builddir)/meta/uninstalled-env guile-tools \
	  snarf-guile-m4-docs $(top_srcdir)/meta/guile.m4 \
	  > $(srcdir)/$@

MAINTAINERCLEANFILES = autoconf-macros.texi

# Support for snarfing docs out of Scheme modules.
snarf_doc = standard-library

$(snarf_doc).am: $(snarf_doc).scm
	GUILE_AUTO_COMPILE=0 ;							\
	variable="`echo $(snarf_doc) | tr - _`_scm_files" ;			\
	"$(top_builddir_absolute)/meta/guile" -l "$(srcdir)/$(snarf_doc).scm"   \
	 -c "			 						\
	 (format #t \"# Automatically generated, do not edit.~%\")		\
	 (format #t \"$$variable = \")						\
	 (for-each (lambda (m)							\
		     (format #t \"$$""(top_srcdir)/module/~a.scm \"		\
			     (string-join (map symbol->string m) \"/\")))	\
		   (map car *modules*))" > "$@.tmp"
	mv "$@.tmp" "$@"

# The following line leads to the definition of $(standard_library_scm_files).
include standard-library.am

$(snarf_doc).texi: $(standard_library_scm_files)
	GUILE_AUTO_COMPILE=0				\
	"$(top_builddir_absolute)/meta/guile" "$(srcdir)/make-texinfo.scm"	\
	  "$(abs_srcdir)/$(snarf_doc).scm" > "$@.tmp"
	mv "$@.tmp" "$@"

DISTCLEANFILES = $(snarf_doc).texi
EXTRA_DIST += $(snarf_doc).scm make-texinfo.scm $(snarf_doc).texi $(snarf_doc).am


www-commit: html
	cd guile.html; \
	cvs -d :ext:cvs.sv.gnu.org:/web/guile \
	  import -m 'make www-commit in doc/ref' guile/docs/master/guile.html gnu \
	  $(shell date "+D%Y%m%d%H%M%S")

## Process this file with automake to produce Makefile.in.
##
## Copyright 2001, 2002, 2003, 2004 Software Foundation, Inc.
##
## This file is part of GUILE.
##
## GUILE is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 2, or
## (at your option) any later version.
##
## GUILE is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public
## License along with GUILE; see the file COPYING.  If not, write
## to the Free Software Foundation, Inc., 59 Temple Place, Suite
## 330, Boston, MA 02111-1307 USA

SUBDIRS = standalone

SCM_TESTS = tests/alist.test			\
	    tests/bit-operations.test		\
	    tests/c-api.test			\
	    tests/chars.test			\
	    tests/common-list.test		\
	    tests/elisp.test			\
	    tests/environments.test		\
	    tests/eval.test			\
	    tests/exceptions.test		\
	    tests/format.test			\
	    tests/gc.test			\
	    tests/getopt-long.test		\
	    tests/goops.test			\
	    tests/guardians.test		\
	    tests/hooks.test			\
	    tests/import.test			\
	    tests/interp.test			\
	    tests/list.test			\
	    tests/load.test			\
	    tests/multilingual.nottest		\
	    tests/numbers.test			\
	    tests/optargs.test			\
	    tests/options.test			\
	    tests/poe.test			\
	    tests/popen.test			\
	    tests/ports.test			\
	    tests/posix.test			\
	    tests/q.test			\
	    tests/r4rs.test			\
	    tests/reader.test			\
	    tests/regexp.test			\
	    tests/srcprop.test			\
	    tests/srfi-1.test			\
	    tests/srfi-6.test			\
	    tests/srfi-10.test			\
	    tests/srfi-13.test			\
	    tests/srfi-14.test			\
	    tests/srfi-19.test			\
	    tests/srfi-34.test			\
	    tests/srfi-4.test			\
	    tests/srfi-9.test			\
	    tests/strings.test			\
	    tests/symbols.test			\
	    tests/syncase.test			\
	    tests/syntax.test			\
	    tests/threads.test			\
	    tests/time.test			\
	    tests/version.test			\
	    tests/weaks.test

SCM_TESTS_DIRS = tests/asmobs \
                 tests/c-api

EXTRA_DIST = guile-test lib.scm $(SCM_TESTS)

## Automake should be able to handle the distribution of tests/asmobs
## etc without any help, but not all version can handle 'deep'
## directories.  So we do it on our own.
dist-hook:
	for d in $(SCM_TESTS_DIRS); do \
	  cp -pR $(srcdir)/$$d $(distdir)/$$d; \
          rm -rf $(distdir)/$$d/CVS; \
        done

## Process this file with automake to produce Makefile.in.
##
## Copyright 2005, 2006, 2008, 2009, 2010 Software Foundation, Inc.
##
## This file is part of GUILE.
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

TESTS_ENVIRONMENT = \
	$(top_builddir)/meta/guile \
	-l $(srcdir)/run-vm-tests.scm -e run-vm-tests

TESTS =						\
      t-basic-contructs.scm			\
      t-global-bindings.scm			\
      t-catch.scm				\
      t-call-cc.scm				\
      t-closure.scm				\
      t-closure2.scm				\
      t-closure3.scm				\
      t-closure4.scm				\
      t-do-loop.scm				\
      t-literal-integers.scm			\
      t-macros.scm				\
      t-macros2.scm				\
      t-map.scm					\
      t-or.scm					\
      t-proc-with-setter.scm			\
      t-quasiquote.scm				\
      t-values.scm				\
      t-records.scm				\
      t-match.scm				\
      t-mutual-toplevel-defines.scm

EXTRA_DIST = run-vm-tests.scm $(TESTS)

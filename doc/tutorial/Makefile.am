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

AUTOMAKE_OPTIONS = gnu

info_TEXINFOS = guile-tut.texi

guile_tut_TEXINFOS = ../AUTHORS

# Optionally support building an HTML version of the reference manual.

if HTMLDOC

htmldir = $(prefix)/html/guile-$(VERSION)

html_DATA = guile-tut_toc.html $(shell ls guile-tut_*.html 2>/dev/null)

guile-tut_toc.html: guile-tut.texi
	$(TEXI2HTML) -split_chapter guile-tut.texi

endif

## Process this file with Automake to create Makefile.in
##
##   Copyright (C) 2001 Free Software Foundation, Inc.
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

EXTRA_DIST = README box.c

CFLAGS=`guile-config compile`
LIBS=`guile-config link`

libbox-module: box.lo
	sh ../../libtool --mode=link $(CC) $< $(LIBS) -rpath $(prefix)/lib -o libbox-module.la

box.lo: box.c
	sh ../../libtool --mode=compile $(CC) $(CFLAGS) -c $< 
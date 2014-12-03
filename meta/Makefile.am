## Process this file with Automake to create Makefile.in
## Jim Blandy <jimb@red-bean.com> --- September 1997
##
##  	Copyright (C) 1998, 1999, 2001, 2006, 2007, 2008, 2009, 2011,
##        2012, 2013, 2014 Free Software Foundation, Inc.
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

bin_SCRIPTS = guile-config guild
EXTRA_DIST= \
  guile.m4 ChangeLog-2008			\
  guile-2.0.pc.in guile-2.0-uninstalled.pc.in	\
  guild.in guile-config.in

# What we now call `guild' used to be known as `guile-tools'.
install-exec-hook:
	guild="`echo $(ECHO_N) guild				\
	   | $(SED) -e '$(program_transform_name)'`" ;		\
	guile_tools="`echo $(ECHO_N) guile-tools		\
	   | $(SED) -e '$(program_transform_name)'`" ;		\
	cd $(DESTDIR)$(bindir) && rm -f "$$guile_tools" &&	\
	$(LN_S) "$$guild" "$$guile_tools"

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = guile-2.0.pc

## FIXME: in the future there will be direct automake support for
## doing this.  When that happens, switch over.
aclocaldir = $(datadir)/aclocal
aclocal_DATA = guile.m4

# Script to instantiate substitution variables found on the standard
# input.  We do this from the Makefile because we need things like
# $(program_transform_name), which are not usable from 'configure.ac'.
substitute =									\
  guile="`echo guile | $(SED) -e '$(program_transform_name)'`" ;		\
  guild="`echo guild | $(SED) -e '$(program_transform_name)'`" ;		\
  installed_guile="@bindir@/$$guile" ;						\
  $(SED) -e "s,[@]PACKAGE_NAME[@],$(PACKAGE_NAME),g"				\
	 -e "s,[@]PACKAGE_BUGREPORT[@],$(PACKAGE_BUGREPORT),g"			\
	 -e "s,[@]GUILE_EFFECTIVE_VERSION[@],$(GUILE_EFFECTIVE_VERSION),g"	\
	 -e "s,[@]GUILE_VERSION[@],$(GUILE_VERSION),g"				\
	 -e "s,[@]GUILE_CFLAGS[@],$(GUILE_CFLAGS),g"				\
	 -e "s,[@]GUILE_LIBS[@],$(GUILE_LIBS),g"				\
	 -e "s,[@]LIBGUILE_INTERFACE[@],$(LIBGUILE_INTERFACE),g"		\
	 $(dependency_substitutions)						\
	 -e "s,[@]PKG_CONFIG[@],$(PKG_CONFIG),g"				\
	 -e "s,[@]pkgconfigdir[@],$(pkgconfigdir),g"				\
	 -e "s,[@]prefix[@],$(prefix),g"					\
	 -e "s,[@]exec_prefix[@],$(exec_prefix),g"				\
	 -e "s,[@]bindir[@],$(bindir),g"					\
	 -e "s,[@]libdir[@],$(libdir),g"					\
	 -e "s,[@]includedir[@],$(includedir),g"				\
	 -e "s,[@]datarootdir[@],$(datarootdir),g"				\
	 -e "s,[@]datadir[@],$(datadir),g"					\
	 -e "s,[@]sitedir[@],$(sitedir),g"					\
	 -e "s,[@]guile[@],$$guile,g"						\
	 -e "s,[@]guild[@],$$guild,g"						\
	 -e "s,[@]installed_guile[@],$$installed_guile,g"

# Substitutions for dependencies that appear in 'guile-2.0.pc'.
dependency_substitutions =				\
  -e "s,[@]BDW_GC_CFLAGS[@],$(BDW_GC_CFLAGS),g"		\
  -e "s,[@]BDW_GC_LIBS[@],$(BDW_GC_LIBS),g"		\
  -e "s,[@]LIB_CLOCK_GETTIME[@],$(LIB_CLOCK_GETTIME),g"	\
  -e "s,[@]LIBGMP[@],$(LIBGMP),g"			\
  -e "s,[@]LIBLTDL[@],$(LIBLTDL),g"			\
  -e "s,[@]LIBFFI_LIBS[@],$(LIBFFI_LIBS),g"		\
  -e "s,[@]LIBUNISTRING[@],$(LIBUNISTRING),g"		\
  -e "s,[@]LIBICONV[@],$(LIBICONV),g"			\
  -e "s,[@]LIBINTL[@],$(LIBINTL),g"			\
  -e "s,[@]LIBSOCKET[@],$(LIBSOCKET),g"			\
  -e "s,[@]SERVENT_LIB[@],$(SERVENT_LIB),g"		\
  -e "s,[@]HOSTENT_LIB[@],$(HOSTENT_LIB),g"		\
  -e "s,[@]GETADDRINFO_LIB[@],$(GETADDRINFO_LIB),g"	\
  -e "s,[@]INET_NTOP_LIB[@],$(INET_NTOP_LIB),g"		\
  -e "s,[@]INET_PTON_LIB[@],$(INET_PTON_LIB),g"

guile-2.0.pc: guile-2.0.pc.in
	$(substitute) < "$<" > "$@.out"
	mv "$@.out" "$@"

guile-2.0-uninstalled.pc: guile-2.0-uninstalled.pc.in
	$(substitute) < "$<" > "$@.out"
	mv "$@.out" "$@"

guile-config: $(srcdir)/guile-config.in $(top_builddir)/config.status
	$(substitute) < "$<" > "$@.out"
	chmod +x "$@.out"
	mv "$@.out" "$@"

guild: $(srcdir)/guild.in $(top_builddir)/config.status
	$(substitute) < "$<" > "$@.out"
	chmod +x "$@.out"
	mv "$@.out" "$@"

CLEANFILES =					\
  guile-config guild				\
  guile-2.0.pc guile-2.0-uninstalled.pc

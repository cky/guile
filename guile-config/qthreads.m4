dnl   Autoconf macros for configuring the QuickThreads package
dnl   Jim Blandy <jimb@red-bean.com> --- July 1998
dnl
dnl  	Copyright (C) 1998, 1999 Free Software Foundation, Inc.
dnl
dnl   This file is part of GUILE.
dnl
dnl   GUILE is free software; you can redistribute it and/or modify
dnl   it under the terms of the GNU General Public License as
dnl   published by the Free Software Foundation; either version 2, or
dnl   (at your option) any later version.
dnl
dnl   GUILE is distributed in the hope that it will be useful, but
dnl   WITHOUT ANY WARRANTY; without even the implied warranty of
dnl   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl   GNU General Public License for more details.
dnl
dnl   You should have received a copy of the GNU General Public
dnl   License along with GUILE; see the file COPYING.  If not, write
dnl   to the Free Software Foundation, Inc., 59 Temple Place, Suite
dnl   330, Boston, MA 02111-1307 USA



dnl   QTHREADS_CONFIGURE configures the QuickThreads package.  The QT
dnl   sources should be in $srcdir/qt.  If configuration succeeds, this
dnl   macro creates the appropriate symlinks in the qt object directory,
dnl   and sets the following variables, used in building libqthreads.a:
dnl      QTHREAD_LTLIBS --- set to libqthreads.la if configuration
dnl         succeeds, or the empty string if configuration fails.
dnl      qtmd_h, qtmds_s, qtmdc_c, qtdmdb_s --- the names of the machine-
dnl         dependent source files.
dnl      qthread_asflags --- flags to pass to the compiler when processing
dnl         assembly-language files.
dnl
dnl   It also sets the following variables, which describe how clients
dnl   can link against libqthreads.a:
dnl      THREAD_PACKAGE --- set to "QT" if configuration succeeds, or
dnl         the empty string if configuration fails.
dnl	 THREAD_LIBS_LOCAL --- linker options for use in this source tree
dnl	 THREAD_LIBS_INSTALLED --- linker options for use after this package
dnl	    is installed
dnl   It would be nice if all thread configuration packages for Guile
dnl   followed the same conventions.
dnl
dnl   All of the above variables will be substituted into Makefiles in
dnl   the usual autoconf fashion.
dnl
dnl   We distinguish between THREAD_LIBS_LOCAL and
dnl   THREAD_LIBS_INSTALLED because the thread library might be in
dnl   this tree, and be built using libtool.  This means that:
dnl	 1) when building other executables in this tree, one must
dnl	    pass the relative path to the ../libfoo.la file, but
dnl	 2) once the whole package has been installed, users should
dnl	    link using -lfoo.
dnl   Normally, we only care about the first case, but since the
dnl   guile-config script needs to give users all the flags they need
dnl   to link programs against guile, the GUILE_WITH_THREADS macro
dnl   needs to supply the second piece of information as well.
dnl
dnl   This whole thing is a little confused about what ought to be
dnl   done in the top-level configure script, and what ought to be
dnl   taken care of in the subdirectory.  For example, qtmds_s and
dnl   friends really ought not to be even mentioned in the top-level
dnl   configure script, but here they are.

AC_DEFUN([QTHREADS_CONFIGURE],[
  AC_REQUIRE([AC_PROG_LN_S])

  AC_MSG_CHECKING(QuickThreads configuration)

  changequote(,)dnl We use [ and ] in a regexp in the case

  THREAD_PACKAGE=QT
  qthread_asflags=''
  case "$host" in
    i[3456]86-*-*)
      port_name=i386
      qtmd_h=md/i386.h
      qtmds_s=md/i386.s
      qtmdc_c=md/null.c
      qtdmdb_s=
      case "$host" in
        *-*-netbsd* )
          ## NetBSD needs to be told to pass the assembly code through
          ## the C preprocessor.  Other GCC installations seem to do
          ## this by default, but NetBSD's doesn't.  We could get the
          ## same effect by giving the file a name ending with .S
          ## instead of .s, but I don't see how to tell automake to do
          ## that.
          qthread_asflags='-x assembler-with-cpp'
        ;;
      esac
      ;;
    mips-sgi-irix[56]*)
      port_name=irix
      qtmd_h=md/mips.h
      qtmds_s=md/mips-irix5.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/mips_b.s
      ;;
    mips-*-*)
      port_name=mips
      qtmd_h=md/mips.h
      qtmds_s=md/mips.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/mips_b.s
      ;;
    sparc-*-sunos*)
      port_name=sparc-sunos
      qtmd_h=md/sparc.h
      qtmds_s=md/_sparc.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/_sparc_b.s
      ;;
    sparc*-*-*)
      port_name=sparc
      qtmd_h=md/sparc.h
      qtmds_s=md/sparc.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/sparc_b.s
      ;;
    alpha*-*-*)
      port_name=alpha
      qtmd_h=md/axp.h
      qtmds_s=md/axp.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/axp_b.s
      ;;
    arm*-*-*)
      port_name=arm
      qtmd_h=md/arm.h
      qtmds_s=md/arm.s
      qtmdc_c=md/null.c
      qtdmdb_s=
      ;;
    *)
      echo "Unknown configuration; threads package disabled"
      THREAD_PACKAGE=""
      ;;
  esac
  changequote([, ])

  # Did configuration succeed?
  if test -n "$THREAD_PACKAGE"; then
    AC_MSG_RESULT($port_name)
    QTHREAD_LTLIBS=libqthreads.la
    THREAD_LIBS_LOCAL="../qt/libqthreads.la"
    THREAD_LIBS_INSTALLED="-lqthreads"
  else
    AC_MSG_RESULT(none; disabled)
  fi

  AC_SUBST(QTHREAD_LTLIBS)
  AC_SUBST(qtmd_h)
  AC_SUBST(qtmds_s)
  AC_SUBST(qtmdc_c)
  AC_SUBST(qtdmdb_s)
  AC_SUBST(qthread_asflags)
  AC_SUBST(THREAD_PACKAGE)
  AC_SUBST(THREAD_LIBS_LOCAL)
  AC_SUBST(THREAD_LIBS_INSTALLED)
])

dnl qthreads.m4 ends here

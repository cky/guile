dnl   Autoconf macros for configuring the QuickThreads package

dnl   QTHREADS_CONFIGURE configures the QuickThreads package.  The QT
dnl   sources should be in $srcdir/qt.  If configuration succeeds, this
dnl   macro creates the appropriate symlinks in the qt object directory,
dnl   and sets the following variables, used in building libqthreads.a:
dnl      QTHREAD_LTLIBS --- set to libqthreads.la if configuration
dnl         succeeds, or the empty string if configuration fails.
dnl      qtmd_h --- the name of the machine-dependent header file.
dnl
dnl   It also sets the following variables, which describe how clients
dnl   can link against libqthreads.a:
dnl      THREAD_PACKAGE --- set to "QT" if configuration succeeds, or
dnl         the empty string if configuration fails.
dnl	 THREAD_CPPFLAGS --- set to `-I' flags for thread header files
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
dnl   build-guile script needs to give users all the flags they need
dnl   to link programs against guile, the GUILE_WITH_THREADS macro
dnl   needs to supply the second piece of information as well.
dnl
dnl   This whole thing is a little confused about what ought to be
dnl   done in the top-level configure script, and what ought to be
dnl   taken care of in the subdirectory.  For example, qtmdc_lo and
dnl   friends really ought not to be even mentioned in the top-level
dnl   configure script, but here they are.

AC_DEFUN([QTHREADS_CONFIGURE],[

  # For some reason, AC_REQUIRE doesn't seem to work with the aclocal
  # program.  So we'll just do this runtime check.
  if test "${LN_S}" = ""; then
    f=''
    AC_MSG_ERROR(The QTHREADS${f}_CONFIGURE macro requires A${f}C_PROG_LN_S)
  fi

  # How can we refer to the qt source directory from within the qt build
  # directory?  For headers, we can rely on the fact that the qt src
  # directory appears in the #include path.
  qtsrcdir="`(cd $srcdir; pwd)`/qt"

  changequote(,)dnl We use [ and ] in a regexp in the case

  THREAD_PACKAGE=QT
  case "$host" in
    i[3456]86-*-*)
      qtmd_h=md/i386.h
      qtmds_s=md/i386.s
      qtmdc_c=md/null.c 
      qtdmdb_s=
      ;;
    mips-sgi-irix[56]*)
      qtmd_h=md/mips.h
      qtmds_s=md/mips-irix5.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/mips_b.s 
      ;;
    mips-*-*)
      qtmd_h=md/mips.h
      qtmds_s=md/mips.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/mips_b.s 
      ;;
    sparc-*-sunos*)
      qtmd_h=md/sparc.h
      qtmds_s=md/_sparc.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/_sparc_b.s 
      ;;
    sparc-*-*)
      qtmd_h=md/sparc.h
      qtmds_s=md/sparc.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/sparc_b.s 
      ;;
    alpha-*-*)
      qtmd_h=md/axp.h
      qtmds_s=md/axp.s
      qtmdc_c=md/null.c
      qtdmdb_s=md/axp_b.s 
      ;;
    *)
      echo "Unknown configuration; threads package disabled"
      THREAD_PACKAGE=""
      ;;
  esac
  changequote([, ])

  # Did configuration succeed?
  if test -n "$THREAD_PACKAGE"; then
    QTHREAD_LTLIBS=libqthreads.la
    THREAD_CPPFLAGS="-I$qtsrcdir -I../qt"
    THREAD_LIBS_LOCAL="../qt/libqthreads.la"
    THREAD_LIBS_INSTALLED="-lqthreads"
  fi

  AC_SUBST(QTHREAD_LTLIBS)
  AC_SUBST(qtmd_h)
  AC_SUBST(qtmds_s)
  AC_SUBST(qtmdc_c)
  AC_SUBST(qtdmdb_s)
  AC_SUBST(THREAD_PACKAGE)
  AC_SUBST(THREAD_CPPFLAGS)
  AC_SUBST(THREAD_LIBS_LOCAL)
  AC_SUBST(THREAD_LIBS_INSTALLED)
])

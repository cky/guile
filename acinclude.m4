dnl  On the NeXT, #including <utime.h> doesn't give you a definition for
dnl  struct utime, unless you #define _POSIX_SOURCE.

AC_DEFUN(GUILE_STRUCT_UTIMBUF, [
  AC_CACHE_CHECK([whether we need POSIX to get struct utimbuf],
    guile_cv_struct_utimbuf_needs_posix,
    [AC_TRY_CPP([
#ifdef __EMX__
#include <sys/utime.h>
#else
#include <utime.h>
#endif
struct utime blah;
],
                guile_cv_struct_utimbuf_needs_posix=no,
		guile_cv_struct_utimbuf_needs_posix=yes)])
  if test "$guile_cv_struct_utimbuf_needs_posix" = yes; then
     AC_DEFINE(UTIMBUF_NEEDS_POSIX)
  fi])




dnl
dnl Apparently, at CMU they have a weird version of libc.h that is
dnl installed in /usr/local/include and conflicts with unistd.h.
dnl In these situations, we should not #include libc.h.
dnl This test arranges to #define LIBC_H_WITH_UNISTD_H iff libc.h is
dnl present on the system, and is safe to #include.
dnl
AC_DEFUN([GUILE_HEADER_LIBC_WITH_UNISTD],
  [
    AC_CHECK_HEADERS(libc.h unistd.h)
    AC_CACHE_CHECK(
      "whether libc.h and unistd.h can be included together",
      guile_cv_header_libc_with_unistd,
      [
        if test "$ac_cv_header_libc_h" = "no"; then
          guile_cv_header_libc_with_unistd="no"
        elif test "$ac_cv_header_unistd_h" = "no"; then
          guile_cv_header_libc_with_unistd="yes"
        else
          AC_TRY_COMPILE(
	    [
#             include <libc.h>
#             include <unistd.h>
	    ],
	    [],
	    [guile_cv_header_libc_with_unistd=yes],
	    [guile_cv_header_libc_with_unistd=no]
          )
        fi
      ]
    )
    if test "$guile_cv_header_libc_with_unistd" = yes; then
      AC_DEFINE(LIBC_H_WITH_UNISTD_H)
    fi
  ]
)



dnl This is needed when we want to check for the same function repeatedly
dnl with other parameters, such as libraries, varying.
dnl
dnl GUILE_NAMED_CHECK_FUNC(FUNCTION, TESTNAME,
dnl                        [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
AC_DEFUN(GUILE_NAMED_CHECK_FUNC,
[AC_MSG_CHECKING([for $1])
AC_CACHE_VAL(ac_cv_func_$1_$2,
[AC_TRY_LINK(
dnl Don't include <ctype.h> because on OSF/1 3.0 it includes <sys/types.h>
dnl which includes <sys/select.h> which contains a prototype for
dnl select.  Similarly for bzero.
[/* System header to define __stub macros and hopefully few prototypes,
    which can conflict with char $1(); below.  */
#include <assert.h>
/* Override any gcc2 internal prototype to avoid an error.  */
]ifelse(AC_LANG, CPLUSPLUS, [#ifdef __cplusplus
extern "C"
#endif
])dnl
[/* We use char because int might match the return type of a gcc2
    builtin and then its argument prototype would still apply.  */
char $1();
], [
/* The GNU C library defines this for functions which it implements
    to always fail with ENOSYS.  Some functions are actually named
    something starting with __ and the normal name is an alias.  */
#if defined (__stub_$1) || defined (__stub___$1)
choke me
#else
$1();
#endif
], eval "ac_cv_func_$1_$2=yes", eval "ac_cv_func_$1_$2=no")])
if eval "test \"`echo '$ac_cv_func_'$1'_'$2`\" = yes"; then
  AC_MSG_RESULT(yes)
  ifelse([$3], , :, [$3])
else
  AC_MSG_RESULT(no)
ifelse([$4], , , [$4
])dnl
fi
])



dnl Check checks whether dlsym (if present) requires a leading underscore.
dnl Written by Dan Hagerty <hag@ai.mit.edu> for scsh-0.5.0.
AC_DEFUN(GUILE_DLSYM_USCORE, [
  AC_MSG_CHECKING(for underscore before symbols)
  AC_CACHE_VAL(guile_cv_uscore,[
    echo "main(){int i=1;}
    fnord(){int i=23; int ltuae=42;}" > conftest.c
    ${CC} conftest.c > /dev/null
    if (nm a.out | grep _fnord) > /dev/null; then
      guile_cv_uscore=yes
    else
      guile_cv_uscore=no
    fi])
  AC_MSG_RESULT($guile_cv_uscore)
  rm -f conftest.c a.out

  if test $guile_cv_uscore = yes; then
    AC_DEFINE(USCORE)

    if test $ac_cv_func_dlopen = yes -o $ac_cv_lib_dl_dlopen = yes ; then
	AC_MSG_CHECKING(whether dlsym always adds an underscore for us)
	AC_CACHE_VAL(guile_cv_dlsym_adds_uscore,AC_TRY_RUN( [
#include <dlfcn.h>
#include <stdio.h>
fnord() { int i=42;}
main() { void *self, *ptr1, *ptr2; self=dlopen(NULL,RTLD_LAZY);
    if(self) { ptr1=dlsym(self,"fnord"); ptr2=dlsym(self,"_fnord");
    if(ptr1 && !ptr2) exit(0); } exit(1); } 
], [guile_cv_dlsym_adds_uscore=yes
	AC_DEFINE(DLSYM_ADDS_USCORE) ], guile_cv_dlsym_adds_uscore=no,
	guile_cv_dlsym_adds_uscore=no))

        AC_MSG_RESULT($guile_cv_dlsym_adds_uscore)
    fi
  fi
])

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
  # How can we refer to the qt source directory from within the qt build
  # directory?  For headers, we can rely on the fact that the qt src
  # directory appears in the #include path.
  qtsrcdir="`(cd $srcdir; pwd)`/qt"

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
    sparc-*-*)
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
    THREAD_CPPFLAGS="-I$qtsrcdir -I../qt"
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
  AC_SUBST(THREAD_CPPFLAGS)
  AC_SUBST(THREAD_LIBS_LOCAL)
  AC_SUBST(THREAD_LIBS_INSTALLED)
])

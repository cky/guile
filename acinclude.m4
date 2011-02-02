dnl -*- Autoconf -*-

dnl Copyright (C) 1997, 1999, 2000, 2001, 2002, 2004, 2006,
dnl   2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
dnl
dnl This file is part of GUILE
dnl
dnl GUILE is free software; you can redistribute it and/or modify it under
dnl the terms of the GNU Lesser General Public License as published by the
dnl Free Software Foundation; either version 3, or (at your option) any
dnl later version.
dnl
dnl GUILE is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl License for more details.
dnl
dnl You should have received a copy of the GNU Lesser General Public
dnl License along with GUILE; see the file COPYING.LESSER.  If not, write
dnl to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
dnl Floor, Boston, MA 02110-1301, USA.


dnl  On the NeXT, #including <utime.h> doesn't give you a definition for
dnl  struct utime, unless you #define _POSIX_SOURCE.

AC_DEFUN([GUILE_STRUCT_UTIMBUF], [
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
     AC_DEFINE([UTIMBUF_NEEDS_POSIX], 1,
       [Define this if <utime.h> doesn't define struct utimbuf unless
        _POSIX_SOURCE is defined.  See GUILE_STRUCT_UTIMBUF in aclocal.m4.])
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
      [whether libc.h and unistd.h can be included together],
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
      AC_DEFINE([LIBC_H_WITH_UNISTD_H], 1,
        [Define this if we should include <libc.h> when we've already
         included <unistd.h>.  On some systems, they conflict, and libc.h
         should be omitted.  See GUILE_HEADER_LIBC_WITH_UNISTD in
         aclocal.m4.])
    fi
  ]
)



dnl This is needed when we want to check for the same function repeatedly
dnl with other parameters, such as libraries, varying.
dnl
dnl GUILE_NAMED_CHECK_FUNC(FUNCTION, TESTNAME,
dnl                        [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
AC_DEFUN([GUILE_NAMED_CHECK_FUNC],
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
#ifdef __cplusplus
extern "C"
#endif
/* We use char because int might match the return type of a gcc2
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




dnl Available from the Autoconf Macro Archive at:
dnl http://autoconf-archive.cryp.to/acx_pthread.html
dnl
AC_DEFUN([ACX_PTHREAD], [
AC_REQUIRE([AC_CANONICAL_HOST])
AC_LANG_SAVE
AC_LANG_C
acx_pthread_ok=no

# We used to check for pthread.h first, but this fails if pthread.h
# requires special compiler flags (e.g. on True64 or Sequent).
# It gets checked for in the link test anyway.

# First of all, check if the user has set any of the PTHREAD_LIBS,
# etcetera environment variables, and if threads linking works using
# them:
if test x"$PTHREAD_LIBS$PTHREAD_CFLAGS" != x; then
        save_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
        save_LIBS="$LIBS"
        LIBS="$PTHREAD_LIBS $LIBS"
        AC_MSG_CHECKING([for pthread_join in LIBS=$PTHREAD_LIBS with CFLAGS=$PTHREAD_CFLAGS])
        AC_TRY_LINK_FUNC(pthread_join, acx_pthread_ok=yes)
        AC_MSG_RESULT($acx_pthread_ok)
        if test x"$acx_pthread_ok" = xno; then
                PTHREAD_LIBS=""
                PTHREAD_CFLAGS=""
        fi
        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"
fi

# We must check for the threads library under a number of different
# names; the ordering is very important because some systems
# (e.g. DEC) have both -lpthread and -lpthreads, where one of the
# libraries is broken (non-POSIX).

# Create a list of thread flags to try.  Items starting with a "-" are
# C compiler flags, and other items are library names, except for "none"
# which indicates that we try without any flags at all, and "pthread-config"
# which is a program returning the flags for the Pth emulation library.

acx_pthread_flags="pthreads none -Kthread -kthread lthread -pthread -pthreads -mthreads pthread --thread-safe -mt pthread-config"

# The ordering *is* (sometimes) important.  Some notes on the
# individual items follow:

# pthreads: AIX (must check this before -lpthread)
# none: in case threads are in libc; should be tried before -Kthread and
#       other compiler flags to prevent continual compiler warnings
# -Kthread: Sequent (threads in libc, but -Kthread needed for pthread.h)
# -kthread: FreeBSD kernel threads (preferred to -pthread since SMP-able)
# lthread: LinuxThreads port on FreeBSD (also preferred to -pthread)
# -pthread: Linux/gcc (kernel threads), BSD/gcc (userland threads)
# -pthreads: Solaris/gcc
# -mthreads: Mingw32/gcc, Lynx/gcc
# -mt: Sun Workshop C (may only link SunOS threads [-lthread], but it
#      doesn't hurt to check since this sometimes defines pthreads too;
#      also defines -D_REENTRANT)
#      ... -mt is also the pthreads flag for HP/aCC
# pthread: Linux, etcetera
# --thread-safe: KAI C++
# pthread-config: use pthread-config program (for GNU Pth library)

case "${host_cpu}-${host_os}" in
        *solaris*)

        # On Solaris (at least, for some versions), libc contains stubbed
        # (non-functional) versions of the pthreads routines, so link-based
        # tests will erroneously succeed.  (We need to link with -pthreads/-mt/
        # -lpthread.)  (The stubs are missing pthread_cleanup_push, or rather
        # a function called by this macro, so we could check for that, but
        # who knows whether they'll stub that too in a future libc.)  So,
        # we'll just look for -pthreads and -lpthread first:

        acx_pthread_flags="-pthreads pthread -mt -pthread $acx_pthread_flags"
        ;;
esac

if test x"$acx_pthread_ok" = xno; then
for flag in $acx_pthread_flags; do

        case $flag in
                none)
                AC_MSG_CHECKING([whether pthreads work without any flags])
                ;;

                -*)
                AC_MSG_CHECKING([whether pthreads work with $flag])
                PTHREAD_CFLAGS="$flag"
                ;;

                pthread-config)
                AC_CHECK_PROG(acx_pthread_config, pthread-config, yes, no)
                if test x"$acx_pthread_config" = xno; then continue; fi
                PTHREAD_CFLAGS="`pthread-config --cflags`"
                PTHREAD_LIBS="`pthread-config --ldflags` `pthread-config --libs`"
                ;;

                *)
                AC_MSG_CHECKING([for the pthreads library -l$flag])
                PTHREAD_LIBS="-l$flag"
                ;;
        esac

        save_LIBS="$LIBS"
        save_CFLAGS="$CFLAGS"
        LIBS="$PTHREAD_LIBS $LIBS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"

        # Check for various functions.  We must include pthread.h,
        # since some functions may be macros.  (On the Sequent, we
        # need a special flag -Kthread to make this header compile.)
        # We check for pthread_join because it is in -lpthread on IRIX
        # while pthread_create is in libc.  We check for pthread_attr_init
        # due to DEC craziness with -lpthreads.  We check for
        # pthread_cleanup_push because it is one of the few pthread
        # functions on Solaris that doesn't have a non-functional libc stub.
        # We try pthread_create on general principles.
        AC_TRY_LINK([#include <pthread.h>],
                    [pthread_t th; pthread_join(th, 0);
                     pthread_attr_init(0); pthread_cleanup_push(0, 0);
                     pthread_create(0,0,0,0); pthread_cleanup_pop(0); ],
                    [acx_pthread_ok=yes])

        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"

        AC_MSG_RESULT($acx_pthread_ok)
        if test "x$acx_pthread_ok" = xyes; then
                break;
        fi

        PTHREAD_LIBS=""
        PTHREAD_CFLAGS=""
done
fi

# Various other checks:
if test "x$acx_pthread_ok" = xyes; then
        save_LIBS="$LIBS"
        LIBS="$PTHREAD_LIBS $LIBS"
        save_CFLAGS="$CFLAGS"
        CFLAGS="$CFLAGS $PTHREAD_CFLAGS"

        # Detect AIX lossage: JOINABLE attribute is called UNDETACHED.
        AC_MSG_CHECKING([for joinable pthread attribute])
        attr_name=unknown
        for attr in PTHREAD_CREATE_JOINABLE PTHREAD_CREATE_UNDETACHED; do
            AC_TRY_LINK([#include <pthread.h>], [int attr=$attr; return attr;],
                        [attr_name=$attr; break])
        done
        AC_MSG_RESULT($attr_name)
        if test "$attr_name" != PTHREAD_CREATE_JOINABLE; then
            AC_DEFINE_UNQUOTED([PTHREAD_CREATE_JOINABLE], $attr_name,
                               [Define to necessary symbol if this constant
                                uses a non-standard name on your system.])
        fi

        AC_MSG_CHECKING([if more special flags are required for pthreads])
        flag=no
        case "${host_cpu}-${host_os}" in
            *-aix* | *-freebsd* | *-darwin*) flag="-D_THREAD_SAFE";;
            *solaris* | *-osf* | *-hpux*) flag="-D_REENTRANT";;
        esac
        AC_MSG_RESULT(${flag})
        if test "x$flag" != xno; then
            PTHREAD_CFLAGS="$flag $PTHREAD_CFLAGS"
        fi

        LIBS="$save_LIBS"
        CFLAGS="$save_CFLAGS"

        # More AIX lossage: must compile with xlc_r or cc_r
        if test x"$GCC" != xyes; then
          AC_CHECK_PROGS(PTHREAD_CC, xlc_r cc_r, ${CC})
        else
          PTHREAD_CC=$CC
        fi
else
        PTHREAD_CC="$CC"
fi

AC_SUBST(PTHREAD_LIBS)
AC_SUBST(PTHREAD_CFLAGS)
AC_SUBST(PTHREAD_CC)

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_pthread_ok" = xyes; then
        ifelse([$1],,AC_DEFINE([HAVE_PTHREAD],1,[Define if you have POSIX threads libraries and header files.]),[$1])
        :
else
        acx_pthread_ok=no
        $2
fi
AC_LANG_RESTORE
])dnl ACX_PTHREAD

dnl GUILE_GNU_LD_RELRO
dnl
dnl Check whether GNU ld's read-only relocations (the `PT_GNU_RELRO'
dnl ELF segment header) are supported.  This allows things like
dnl statically allocated cells (1) to eventually be remapped read-only
dnl by the loader, and (2) to be identified as pointerless by the
dnl garbage collector.  Substitute `GNU_LD_FLAGS' with the relevant
dnl flags.
AC_DEFUN([GUILE_GNU_LD_RELRO], [
  AC_MSG_CHECKING([whether the linker understands `-z relro'])

  GNU_LD_FLAGS="-Wl,-z -Wl,relro"

  save_LDFLAGS="$LDFLAGS"
  LDFLAGS="$LDFLAGS $GNU_LD_FLAGS"
  AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
    [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])
     GNU_LD_FLAGS=""])
  LDFLAGS="$save_LDFLAGS"

  AC_SUBST([GNU_LD_FLAGS])
])

dnl GUILE_THREAD_LOCAL_STORAGE
dnl
dnl Check for compiler thread-local storage (TLS) support.
AC_DEFUN([GUILE_THREAD_LOCAL_STORAGE], [
  AC_REQUIRE([AC_CANONICAL_HOST])

  AC_CACHE_CHECK([whether the `__thread' storage class is available],
    [ac_cv_have_thread_storage_class],
    [dnl On some systems, e.g., NetBSD 5.0 with GCC 4.1, `__thread' is
     dnl properly compiled but fails to link due to the lack of TLS
     dnl support in the C library.  Thus we try to link, not just
     dnl compile.  Unfortunately, this test is not enough, so we
     dnl explicitly check for known-broken systems.  See
     dnl http://lists.gnu.org/archive/html/guile-devel/2009-10/msg00138.html
     dnl for details.
     dnl
     dnl Known broken systems includes:
     dnl   - x86_64-unknown-netbsd5.0.
     dnl   - sparc-sun-solaris2.8
     dnl
     dnl On `x86_64-unknown-freebsd8.0', thread-local storage appears to
     dnl be reclaimed at the wrong time, leading to a segfault when
     dnl running `threads.test'.  So disable it.
     case "$enable_shared--$host_os" in
       [yes--netbsd[0-5].[0-9].|yes--solaris2.8|yes--freebsd[0-8]*])
         ac_cv_have_thread_storage_class="no"
         ;;
       *)
         AC_LINK_IFELSE([AC_LANG_PROGRAM([__thread int tls_integer;],
                          [tls_integer = 123;])],
           [ac_cv_have_thread_storage_class="yes"],
           [ac_cv_have_thread_storage_class="no"])
         ;;
     esac])

  if test "x$ac_cv_have_thread_storage_class" = "xyes"; then
     SCM_I_GSC_HAVE_THREAD_STORAGE_CLASS=1
  else
     SCM_I_GSC_HAVE_THREAD_STORAGE_CLASS=0
  fi

  AC_SUBST([SCM_I_GSC_HAVE_THREAD_STORAGE_CLASS])
])

dnl GUILE_READLINE
dnl
dnl Check all the things needed by `guile-readline', the Readline
dnl bindings.
AC_DEFUN([GUILE_READLINE], [
  for termlib in ncurses curses termcap terminfo termlib pdcurses ; do
     AC_CHECK_LIB(${termlib}, [tgoto],
       [READLINE_LIBS="-l${termlib} $READLINE_LIBS"; break])
  done

  AC_LIB_LINKFLAGS([readline])

  if test "x$LTLIBREADLINE" = "x"; then
    AC_MSG_WARN([GNU Readline was not found on your system.])
  else
    rl_save_LIBS="$LIBS"
    LIBS="$LIBREADLINE $READLINE_LIBS $LIBS"

    AC_CHECK_FUNCS([siginterrupt rl_clear_signals rl_cleanup_after_signal])

    dnl Check for modern readline naming
    AC_CHECK_FUNCS([rl_filename_completion_function])
    AC_CHECK_DECLS([rl_catch_signals, rl_catch_sigwinch], [], [],
                   [[#include <stdio.h>]
                    [#include <readline/readline.h>]])

    dnl Check for rl_get_keymap.  We only use this for deciding whether to
    dnl install paren matching on the Guile command line (when using
    dnl readline for input), so it's completely optional.
    AC_CHECK_FUNCS([rl_get_keymap])

    AC_CACHE_CHECK([for rl_getc_function pointer in readline],
		     ac_cv_var_rl_getc_function,
		     [AC_TRY_LINK([
    #include <stdio.h>
    #include <readline/readline.h>],
				  [printf ("%ld", (long) rl_getc_function)],
				  [ac_cv_var_rl_getc_function=yes],
				  [ac_cv_var_rl_getc_function=no])])
    if test "${ac_cv_var_rl_getc_function}" = "yes"; then
      AC_DEFINE([HAVE_RL_GETC_FUNCTION], 1,
	[Define if your readline library has the rl_getc_function variable.])
    fi

    if test $ac_cv_var_rl_getc_function = no; then
      AC_MSG_WARN([*** GNU Readline is too old on your system.])
      AC_MSG_WARN([*** You need readline version 2.1 or later.])
      LTLIBREADLINE=""
      LIBREADLINE=""
    fi

    LIBS="$rl_save_LIBS"

    READLINE_LIBS="$LTLIBREADLINE $READLINE_LIBS"
  fi

  AM_CONDITIONAL([HAVE_READLINE], [test "x$LTLIBREADLINE" != "x"])

  AC_CHECK_FUNCS([strdup])

  AC_SUBST([READLINE_LIBS])

  . $srcdir/guile-readline/LIBGUILEREADLINE-VERSION
  AC_SUBST(LIBGUILEREADLINE_MAJOR)
  AC_SUBST(LIBGUILEREADLINE_INTERFACE_CURRENT)
  AC_SUBST(LIBGUILEREADLINE_INTERFACE_REVISION)
  AC_SUBST(LIBGUILEREADLINE_INTERFACE_AGE)
  AC_SUBST(LIBGUILEREADLINE_INTERFACE)
])

dnl Declare file $1 to be a script that needs configuring,
dnl and arrange to make it executable in the process.
AC_DEFUN([GUILE_CONFIG_SCRIPT],[AC_CONFIG_FILES([$1],[chmod +x $1])])

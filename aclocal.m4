dnl aclocal.m4 generated automatically by aclocal 1.4

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

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
        elif test "$ac_cv_header_unistd.h" = "no"; then
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

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AC_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN(AM_MISSING_PROG,
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])

# Add --enable-maintainer-mode option to configure.
# From Jim Meyering

# serial 1

AC_DEFUN(AM_MAINTAINER_MODE,
[AC_MSG_CHECKING([whether to enable maintainer-specific portions of Makefiles])
  dnl maintainer-mode is disabled by default
  AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable make rules and dependencies not useful
                          (and sometimes confusing) to the casual installer],
      USE_MAINTAINER_MODE=$enableval,
      USE_MAINTAINER_MODE=no)
  AC_MSG_RESULT($USE_MAINTAINER_MODE)
  AM_CONDITIONAL(MAINTAINER_MODE, test $USE_MAINTAINER_MODE = yes)
  MAINT=$MAINTAINER_MODE_TRUE
  AC_SUBST(MAINT)dnl
]
)

# Define a conditional.

AC_DEFUN(AM_CONDITIONAL,
[AC_SUBST($1_TRUE)
AC_SUBST($1_FALSE)
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi])

# Like AC_CONFIG_HEADER, but automatically create stamp file.

AC_DEFUN(AM_CONFIG_HEADER,
[AC_PREREQ([2.12])
AC_CONFIG_HEADER([$1])
dnl When config.status generates a header, we must update the stamp-h file.
dnl This file resides in the same directory as the config header
dnl that is generated.  We must strip everything past the first ":",
dnl and everything past the last "/".
AC_OUTPUT_COMMANDS(changequote(<<,>>)dnl
ifelse(patsubst(<<$1>>, <<[^ ]>>, <<>>), <<>>,
<<test -z "<<$>>CONFIG_HEADERS" || echo timestamp > patsubst(<<$1>>, <<^\([^:]*/\)?.*>>, <<\1>>)stamp-h<<>>dnl>>,
<<am_indx=1
for am_file in <<$1>>; do
  case " <<$>>CONFIG_HEADERS " in
  *" <<$>>am_file "*<<)>>
    echo timestamp > `echo <<$>>am_file | sed -e 's%:.*%%' -e 's%[^/]*$%%'`stamp-h$am_indx
    ;;
  esac
  am_indx=`expr "<<$>>am_indx" + 1`
done<<>>dnl>>)
changequote([,]))])


# serial 25 AM_PROG_LIBTOOL
AC_DEFUN(AM_PROG_LIBTOOL,
[AC_REQUIRE([AM_ENABLE_SHARED])dnl
AC_REQUIRE([AM_ENABLE_STATIC])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_PROG_RANLIB])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AM_PROG_LD])dnl
AC_REQUIRE([AM_PROG_NM])dnl
AC_REQUIRE([AC_PROG_LN_S])dnl
dnl
# Always use our own libtool.
LIBTOOL='$(SHELL) $(top_builddir)/libtool'
AC_SUBST(LIBTOOL)dnl

# Check for any special flags to pass to ltconfig.
libtool_flags=
test "$enable_shared" = no && libtool_flags="$libtool_flags --disable-shared"
test "$enable_static" = no && libtool_flags="$libtool_flags --disable-static"
test "$silent" = yes && libtool_flags="$libtool_flags --silent"
test "$ac_cv_prog_gcc" = yes && libtool_flags="$libtool_flags --with-gcc"
test "$ac_cv_prog_gnu_ld" = yes && libtool_flags="$libtool_flags --with-gnu-ld"

# Some flags need to be propagated to the compiler or linker for good
# libtool support.
case "$host" in
*-*-irix6*)
  # Find out which ABI we are using.
  echo '[#]line __oline__ "configure"' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case "`/usr/bin/file conftest.o`" in
    *32-bit*)
      LD="${LD-ld} -32"
      ;;
    *N32*)
      LD="${LD-ld} -n32"
      ;;
    *64-bit*)
      LD="${LD-ld} -64"
      ;;
    esac
  fi
  rm -rf conftest*
  ;;

*-*-sco3.2v5*)
  # On SCO OpenServer 5, we need -belf to get full-featured binaries.
  CFLAGS="$CFLAGS -belf"
  ;;
esac

# Actually configure libtool.  ac_aux_dir is where install-sh is found.
CC="$CC" CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" \
LD="$LD" NM="$NM" RANLIB="$RANLIB" LN_S="$LN_S" \
${CONFIG_SHELL-/bin/sh} $ac_aux_dir/ltconfig --no-reexec \
$libtool_flags --no-verify $ac_aux_dir/ltmain.sh $host \
|| AC_MSG_ERROR([libtool configure failed])

# Redirect the config.log output again, so that the ltconfig log is not
# clobbered by the next message.
exec 5>>./config.log
])

# AM_ENABLE_SHARED - implement the --enable-shared flag
# Usage: AM_ENABLE_SHARED[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AM_ENABLE_SHARED,
[define([AM_ENABLE_SHARED_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(shared,
changequote(<<, >>)dnl
<<  --enable-shared[=PKGS]  build shared libraries [default=>>AM_ENABLE_SHARED_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_shared=yes ;;
no) enable_shared=no ;;
*)
  enable_shared=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_shared=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_shared=AM_ENABLE_SHARED_DEFAULT)dnl
])

# AM_DISABLE_SHARED - set the default shared flag to --disable-shared
AC_DEFUN(AM_DISABLE_SHARED,
[AM_ENABLE_SHARED(no)])

# AM_DISABLE_STATIC - set the default static flag to --disable-static
AC_DEFUN(AM_DISABLE_STATIC,
[AM_ENABLE_STATIC(no)])

# AM_ENABLE_STATIC - implement the --enable-static flag
# Usage: AM_ENABLE_STATIC[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AM_ENABLE_STATIC,
[define([AM_ENABLE_STATIC_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(static,
changequote(<<, >>)dnl
<<  --enable-static[=PKGS]  build static libraries [default=>>AM_ENABLE_STATIC_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_static=yes ;;
no) enable_static=no ;;
*)
  enable_static=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_static=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_static=AM_ENABLE_STATIC_DEFAULT)dnl
])


# AM_PROG_LD - find the path to the GNU or non-GNU linker
AC_DEFUN(AM_PROG_LD,
[AC_ARG_WITH(gnu-ld,
[  --with-gnu-ld           assume the C compiler uses GNU ld [default=no]],
test "$withval" = no || with_gnu_ld=yes, with_gnu_ld=no)
AC_REQUIRE([AC_PROG_CC])
ac_prog=ld
if test "$ac_cv_prog_gcc" = yes; then
  # Check if gcc -print-prog-name=ld gives a path.
  AC_MSG_CHECKING([for ld used by GCC])
  ac_prog=`($CC -print-prog-name=ld) 2>&5`
  case "$ac_prog" in
  # Accept absolute paths.
changequote(,)dnl
  /* | [A-Za-z]:\\*)
changequote([,])dnl
    test -z "$LD" && LD="$ac_prog"
    ;;
  "")
    # If it fails, then pretend we aren't using GCC.
    ac_prog=ld
    ;;
  *)
    # If it is relative, then search for the first ld in PATH.
    with_gnu_ld=unknown
    ;;
  esac
elif test "$with_gnu_ld" = yes; then
  AC_MSG_CHECKING([for GNU ld])
else
  AC_MSG_CHECKING([for non-GNU ld])
fi
AC_CACHE_VAL(ac_cv_path_LD,
[if test -z "$LD"; then
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  for ac_dir in $PATH; do
    test -z "$ac_dir" && ac_dir=.
    if test -f "$ac_dir/$ac_prog"; then
      ac_cv_path_LD="$ac_dir/$ac_prog"
      # Check to see if the program is GNU ld.  I'd rather use --version,
      # but apparently some GNU ld's only accept -v.
      # Break only if it was the GNU/non-GNU ld that we prefer.
      if "$ac_cv_path_LD" -v 2>&1 < /dev/null | egrep '(GNU|with BFD)' > /dev/null; then
	test "$with_gnu_ld" != no && break
      else
        test "$with_gnu_ld" != yes && break
      fi
    fi
  done
  IFS="$ac_save_ifs"
else
  ac_cv_path_LD="$LD" # Let the user override the test with a path.
fi])
LD="$ac_cv_path_LD"
if test -n "$LD"; then
  AC_MSG_RESULT($LD)
else
  AC_MSG_RESULT(no)
fi
test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
AC_SUBST(LD)
AM_PROG_LD_GNU
])

AC_DEFUN(AM_PROG_LD_GNU,
[AC_CACHE_CHECK([if the linker ($LD) is GNU ld], ac_cv_prog_gnu_ld,
[# I'd rather use --version here, but apparently some GNU ld's only accept -v.
if $LD -v 2>&1 </dev/null | egrep '(GNU|with BFD)' 1>&5; then
  ac_cv_prog_gnu_ld=yes
else
  ac_cv_prog_gnu_ld=no
fi])
])

# AM_PROG_NM - find the path to a BSD-compatible name lister
AC_DEFUN(AM_PROG_NM,
[AC_MSG_CHECKING([for BSD-compatible nm])
AC_CACHE_VAL(ac_cv_path_NM,
[if test -n "$NM"; then
  # Let the user override the test.
  ac_cv_path_NM="$NM"
else
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  for ac_dir in /usr/ucb /usr/ccs/bin $PATH /bin; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/nm; then
      # Check to see if the nm accepts a BSD-compat flag.
      # Adding the `sed 1q' prevents false positives on HP-UX, which says:
      #   nm: unknown option "B" ignored
      if ($ac_dir/nm -B /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
        ac_cv_path_NM="$ac_dir/nm -B"
      elif ($ac_dir/nm -p /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
        ac_cv_path_NM="$ac_dir/nm -p"
      else
        ac_cv_path_NM="$ac_dir/nm"
      fi
      break
    fi
  done
  IFS="$ac_save_ifs"
  test -z "$ac_cv_path_NM" && ac_cv_path_NM=nm
fi])
NM="$ac_cv_path_NM"
AC_MSG_RESULT([$NM])
AC_SUBST(NM)
])


# serial 1

# @defmac AC_PROG_CC_STDC
# @maindex PROG_CC_STDC
# @ovindex CC
# If the C compiler in not in ANSI C mode by default, try to add an option
# to output variable @code{CC} to make it so.  This macro tries various
# options that select ANSI C on some system or another.  It considers the
# compiler to be in ANSI C mode if it handles function prototypes correctly.
#
# If you use this macro, you should check after calling it whether the C
# compiler has been set to accept ANSI C; if not, the shell variable
# @code{am_cv_prog_cc_stdc} is set to @samp{no}.  If you wrote your source
# code in ANSI C, you can make an un-ANSIfied copy of it by using the
# program @code{ansi2knr}, which comes with Ghostscript.
# @end defmac

AC_DEFUN(AM_PROG_CC_STDC,
[AC_REQUIRE([AC_PROG_CC])
AC_BEFORE([$0], [AC_C_INLINE])
AC_BEFORE([$0], [AC_C_CONST])
dnl Force this before AC_PROG_CPP.  Some cpp's, eg on HPUX, require
dnl a magic option to avoid problems with ANSI preprocessor commands
dnl like #elif.
dnl FIXME: can't do this because then AC_AIX won't work due to a
dnl circular dependency.
dnl AC_BEFORE([$0], [AC_PROG_CPP])
AC_MSG_CHECKING(for ${CC-cc} option to accept ANSI C)
AC_CACHE_VAL(am_cv_prog_cc_stdc,
[am_cv_prog_cc_stdc=no
ac_save_CC="$CC"
# Don't try gcc -ansi; that turns off useful extensions and
# breaks some systems' header files.
# AIX			-qlanglvl=ansi
# Ultrix and OSF/1	-std1
# HP-UX			-Aa -D_HPUX_SOURCE
# SVR4			-Xc -D__EXTENSIONS__
for ac_arg in "" -qlanglvl=ansi -std1 "-Aa -D_HPUX_SOURCE" "-Xc -D__EXTENSIONS__"
do
  CC="$ac_save_CC $ac_arg"
  AC_TRY_COMPILE(
[#include <stdarg.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
/* Most of the following tests are stolen from RCS 5.7's src/conf.sh.  */
struct buf { int x; };
FILE * (*rcsopen) (struct buf *, struct stat *, int);
static char *e (p, i)
     char **p;
     int i;
{
  return p[i];
}
static char *f (char * (*g) (char **, int), char **p, ...)
{
  char *s;
  va_list v;
  va_start (v,p);
  s = g (p, va_arg (v,int));
  va_end (v);
  return s;
}
int test (int i, double x);
struct s1 {int (*f) (int a);};
struct s2 {int (*f) (double a);};
int pairnames (int, char **, FILE *(*)(struct buf *, struct stat *, int), int, int);
int argc;
char **argv;
], [
return f (e, argv, 0) != argv[0]  ||  f (e, argv, 1) != argv[1];
],
[am_cv_prog_cc_stdc="$ac_arg"; break])
done
CC="$ac_save_CC"
])
if test -z "$am_cv_prog_cc_stdc"; then
  AC_MSG_RESULT([none needed])
else
  AC_MSG_RESULT($am_cv_prog_cc_stdc)
fi
case "x$am_cv_prog_cc_stdc" in
  x|xno) ;;
  *) CC="$CC $am_cv_prog_cc_stdc" ;;
esac
])

dnl   Autoconf macros for configuring the QuickThreads package
dnl   Jim Blandy <jimb@red-bean.com> --- July 1998
dnl
dnl  	Copyright (C) 1998 Free Software Foundation, Inc.
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
    alpha-*-*)
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


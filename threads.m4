dnl
dnl CY_AC_WITH_THREADS determines which thread library the user intends
dnl to put underneath guile.  Pass it the path to find the guile top-level
dnl source directory.  Eg CY_AC_WITH_THREADS(../..) for tcl/unix.
dnl

AC_DEFUN([CY_AC_WITH_THREADS],[
AC_CACHE_CHECK("threads package type",cy_cv_threads_package,[
AC_CACHE_VAL(cy_cv_threads_cflags,[
AC_CACHE_VAL(cy_cv_threads_libs,[
use_threads=no;
AC_ARG_WITH(threads,[  --with-threads          thread interface],
            use_threads=$withval, use_threads=no)
test -n "$use_threads" || use_threads=qt
threads_package=unknown
if test "$use_threads" != no; then
dnl
dnl Test for the qt threads package - used for cooperative threads
dnl This may not necessarily be built yet - so just check for the
dnl header files.
dnl
  if test "$use_threads" = yes || test "$use_threads" = qt; then
     # Look for qt in source directory.  This is a hack: we look in
     # "./qt" because this check might be run at the top level.
     if test -f $srcdir/../qt/qt.c || test -f $srcdir/qt/qt.c; then
	threads_package=COOP
	cy_cv_threads_cflags="-I$srcdir/../qt -I../qt"
	cy_cv_threads_libs="../threads/libthreads.a ../qt/libqt.a"
     fi
  else
     if test -f $use_threads/qt.c; then
	# FIXME seems as though we should try to use an installed qt here.
	threads_package=COOP
	cy_cv_threads_cflags="-I$use_threads -I../qt"
	cy_cv_threads_libs="../threads/libthreads.a ../qt/libqt.a"
     fi
  fi
  if test "$use_threads" = pthreads; then
     # Look for pthreads in srcdir.  See above to understand why
     # we always set threads_package.
     if test -f $srcdir/../../pthreads/pthreads/queue.c \
	  || test -f $srcdir/../pthreads/pthreads/queue.c; then
	threads_package=MIT
	cy_cv_threads_cflags="-I$srcdir/../../pthreads/include"
	cy_cv_threads_libs="-L../../pthreads/lib -lpthread"
     fi
  fi
  saved_CPP="$CPPFLAGS"
  saved_LD="$LDFLAGS"
  saved_LIBS="$LIBS"
  if test "$threads_package" = unknown; then
dnl
dnl Test for the FSU threads package
dnl
    CPPFLAGS="-I$use_threads/include"
    LDFLAGS="-L$use_threads/lib"
    LIBS="-lgthreads -lmalloc"
    AC_TRY_LINK([#include <pthread.h>],[
pthread_equal(NULL,NULL);
], threads_package=FSU)
  fi
  if test "$threads_package" = unknown; then
dnl
dnl Test for the MIT threads package
dnl
    LIBS="-lpthread"
    AC_TRY_LINK([#include <pthread.h>],[
pthread_equal(NULL,NULL);
], threads_package=MIT)
  fi
  if test "$threads_package" = unknown; then
dnl
dnl Test for the PCthreads package
dnl
    LIBS="-lpthreads"
    AC_TRY_LINK([#include <pthread.h>],[
pthread_equal(NULL,NULL);
], threads_package=PCthreads)
  fi
dnl
dnl Set the appropriate flags!
dnl 
  cy_cv_threads_cflags="$CPPFLAGS $cy_cv_threads_cflags"
  cy_cv_threads_libs="$LDFLAGS $LIBS $cy_cv_threads_libs"
  cy_cv_threads_package=$threads_package
  CPPFLAGS="$saved_CPP"
  LDFLAGS="$saved_LD"
  LIBS="$saved_LIBS"
  if test "$threads_package" = unknown; then
    AC_MSG_ERROR("cannot find thread library installation")
  fi
fi
])
])
],
dnl
dnl Set flags according to what is cached.
dnl
CPPFLAGS="$cy_cv_threads_cflags"
LIBS="$cy_cv_threads_libs"
)
])

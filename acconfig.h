/* acconfig.h --- documentation for symbols possibly defined in scmconfig.h
 * The `autoheader' command, from the autoconf suite, generates
 * libguile/scmconfig.h, based on configure.in and this file.
 *
 *	Copyright (C) 1998, 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


/* Define these two if you want support for debugging of Scheme
   programs.  */
#undef DEBUG_EXTENSIONS
#undef READER_EXTENSIONS

/* Define this if you want to debug the free list (helps w/ GC bugs) */
#undef GUILE_DEBUG_FREELIST

/* Define this if you want to debug scm_must_malloc/realloc/free calls */
#undef GUILE_DEBUG_MALLOC

/* Define this if your system defines S_ISLNK in sys/stat.h */
#undef HAVE_S_ISLNK

/* Define this if your system defines struct linger, for use with the
   getsockopt and setsockopt system calls.  */
#undef HAVE_STRUCT_LINGER

/* Define this if your system defines struct timespec via <time.h>. */
#undef HAVE_STRUCT_TIMESPEC

/* Define this if floats are the same size as longs.  */
#undef SCM_SINGLES

/* Define this if a callee's stack frame has a higher address than the
   caller's stack frame.  On most machines, this is not the case.  */
#undef SCM_STACK_GROWS_UP

/* Define this if <utime.h> doesn't define struct utimbuf unless
   _POSIX_SOURCE is #defined.  See GUILE_STRUCT_UTIMBUF in aclocal.m4.  */
#undef UTIMBUF_NEEDS_POSIX

/* Define this if we should #include <libc.h> when we've already
   #included <unistd.h>.  On some systems, they conflict, and libc.h
   should be omitted.  See GUILE_HEADER_LIBC_WITH_UNISTD in
   aclocal.m4.  */
#undef LIBC_H_WITH_UNISTD_H

/* Define this to include various undocumented functions used to debug
   the Guile library itself.  */
#undef GUILE_DEBUG

/* Define to implement scm_internal_select */
#undef GUILE_ISELECT

/* Define to enable workaround for COOP-linuxthreads compatibility */
#undef GUILE_PTHREAD_COMPAT

/* Define if using cooperative multithreading.  */
#undef USE_COOP_THREADS

/* Define if using any sort of threads.  */
#undef USE_THREADS

/* Define if you want support for dynamic linking. */
#undef DYNAMIC_LINKING

/* Define if symbol tables on this system use leading underscores. */
#undef USCORE

/* Define if dlsym automatically supplies a leading underscore. */
#undef DLSYM_ADDS_USCORE

/* Define if h_errno is declared in netdb.h.  */
#undef HAVE_H_ERRNO

/* Define if localtime caches the TZ setting.  */
#undef LOCALTIME_CACHE

/* Define if the operating system can restart system calls.  */
#undef HAVE_RESTARTS

/* Define if the system supports Unix-domain (file-domain) sockets.  */
#undef HAVE_UNIX_DOMAIN_SOCKETS

/* Define this if you want support for arrays and uniform arrays.  */
#undef HAVE_ARRAYS

/* This is included as part of a workaround for a autoheader bug. */
#undef HAVE_REGCOMP

/* Define this if you want support for POSIX system calls in Guile.  */
#undef HAVE_POSIX

/* Define this if you want support for networking in Guile.  */
#undef HAVE_NETWORKING

/* Define if the operating system supplies bzero without declaring it. */
#undef MISSING_BZERO_DECL

/* Define if the operating system supplies strptime without declaring it. */
#undef MISSING_STRPTIME_DECL

/* Define if the operating system supplies sleep without declaring it. */
#undef MISSING_SLEEP_DECL

/* Define if the operating system supplies usleep without declaring it. */
#undef MISSING_USLEEP_DECL

/* Define if the system headers declare usleep to return void.  */
#undef USLEEP_RETURNS_VOID

/* Define if your readline library has the rl_getc_function variable.  */
#undef HAVE_RL_GETC_FUNCTION

/* Define if the compiler supports long longs.  */
#undef HAVE_LONG_LONGS

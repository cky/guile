/* acconfig.h --- documentation for symbols possibly defined in scmconfig.h
   Jim Blandy <jimb@cyclic.com> --- August 1996
   The `autoheader' command, from the autoconf suite, generates
   libguile/scmconfig.h, based on configure.in and this file.  */

/* Define these two if you want support for debugging of Scheme
   programs.  */
#undef DEBUG_EXTENSIONS
#undef READER_EXTENSIONS

/* Define this if your system has a way to set a stdio stream's file
   descriptor.  */
#undef FD_SETTER

/* Set this to the name of a field in FILE which contains the number
   of buffered characters waiting to be read.  */
#undef FILE_CNT_FIELD

/* Define this if your stdio has _gptr and _egptr fields which can
   be compared to give the number of buffered characters waiting to
   be read.  */
#undef FILE_CNT_GPTR

/* Define this if your stdio has _IO_read_ptr and _IO_read_end fields
   which can be compared to give the number of buffered characters
   waiting to be read.  */
#undef FILE_CNT_READPTR

/* Define this if your system defines struct linger, for use with the
   getsockopt and setsockopt system calls.  */
#undef HAVE_STRUCT_LINGER

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

/* Define if using cooperative multithreading.  */
#undef USE_COOP_THREADS

/* Define if using "FSU" pthreads.  */
#undef USE_FSU_PTHREADS

/* Define if using MIT pthreads.  */
#undef USE_MIT_PTHREADS

/* Define if using PCthreads pthreads.  */
#undef USE_PCTHREADS_PTHREADS

/* Define if using any sort of threads.  */
#undef USE_THREADS

/* Define if you want support for dynamic linking. */
#undef DYNAMIC_LINKING

/* Define if the operating system can restart system calls.  */
#undef HAVE_RESTARTS

/* Define if the system supports Unix-domain (file-domain) sockets.  */
#undef HAVE_UNIX_DOMAIN_SOCKETS

/* This is included as part of a workaround for a autoheader bug. */
#undef HAVE_REGCOMP

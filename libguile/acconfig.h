/* acconfig.h --- documentation for symbols possibly defined in scmconfig.h
   Jim Blandy <jimb@cyclic.com> --- August 1996 */

/* Define these two if you want support for debugging of Scheme
   programs.  */
#undef DEBUG_EXTENSIONS
#undef READER_EXTENSIONS

/* Define this if your system has a way to set a stdio stream's file
   descriptor.  You should also copy fd.h.in to fd.h, and give the
   macro SET_FILE_FD_FIELD an appropriate definition.  See
   configure.in for more details.  */
#undef HAVE_FD_SETTER

/* Define this if your system defines struct linger, for use with the
   getsockopt and setsockopt system calls.  */
#undef HAVE_STRUCT_LINGER

/* Define this if floats are the same size as longs.  */
#undef SCM_SINGLES

/* Define this if a callee's stack frame has a higher address than the
   caller's stack frame.  On most machines, this is not the case.  */
#undef SCM_STACK_GROWS_UP

/* Define these to indicate the current version of Guile.  These
   values are supposed to be supplied by the configuration system.  */
#undef GUILE_MAJOR_VERSION
#undef GUILE_MINOR_VERSION
#undef GUILE_VERSION

/* classes: h_files */

#ifndef SCM__SCM_H
#define SCM__SCM_H

/* Copyright (C) 1995,1996,2000,2001, 2002, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



/**********************************************************************
 This file is Guile's central private header.

 When included by other files, this file should preceed any include
 other than __scm.h.  See __scm.h for details regarding the purpose of
 and differences between _scm.h and __scm.h.
 **********************************************************************/

#if defined(__ia64) && !defined(__ia64__)
# define __ia64__
#endif

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* The size of `scm_t_bits'.  */
#define SIZEOF_SCM_T_BITS SIZEOF_VOID_P

/* Undefine HAVE_STRUCT_TIMESPEC, because the libguile C code doesn't
   need it anymore, and because on MinGW:

   - the definition of struct timespec is provided (if at all) by
     pthread.h

   - pthread.h will _not_ define struct timespec if
     HAVE_STRUCT_TIMESPEC is 1, because then it thinks that it doesn't
     need to.

   The libguile C code doesn't need HAVE_STRUCT_TIMESPEC anymore,
   because the value of HAVE_STRUCT_TIMESPEC has already been
   incorporated in how scm_t_timespec is defined (in scmconfig.h), and
   the rest of the libguile C code now just uses scm_t_timespec.
 */
#ifdef HAVE_STRUCT_TIMESPEC
#undef HAVE_STRUCT_TIMESPEC
#endif

#include <errno.h>
#include <verify.h>
#include <alignof.h>
#include "libguile/__scm.h"

/* Include headers for those files central to the implementation.  The
   rest should be explicitly #included in the C files themselves.  */
#include "libguile/error.h"	/* Everyone signals errors.  */
#include "libguile/print.h"	/* Everyone needs to print.  */
#include "libguile/pairs.h"	/* Everyone conses.  */
#include "libguile/list.h"	/* Everyone makes lists.  */
#include "libguile/gc.h"	/* Everyone allocates.  */
#include "libguile/gsubr.h"	/* Everyone defines global functions.  */
#include "libguile/procs.h"	/* Same.  */
#include "libguile/numbers.h"	/* Everyone deals with fixnums.  */
#include "libguile/symbols.h"	/* For length, chars, values, miscellany.  */
#include "libguile/boolean.h"	/* Everyone wonders about the truth.  */
#include "libguile/threads.h"	/* You are not alone. */
#include "libguile/snarf.h"	/* Everyone snarfs. */
#include "libguile/foreign.h"	/* Snarfing needs the foreign data structures. */
#include "libguile/programs.h"	/* ... and program.h. */
#include "libguile/variable.h"
#include "libguile/modules.h"
#include "libguile/inline.h"
#include "libguile/strings.h"

/* ASYNC_TICK after finding EINTR in order to handle pending signals, if
   any. See comment in scm_syserror. */
#ifndef SCM_SYSCALL
#ifdef vms
# ifndef __GNUC__
#  include <ssdef.h>
#   define SCM_SYSCALL(line)                                    \
  do                                                            \
    {                                                           \
      errno = 0;                                                \
      line;                                                     \
      if (EVMSERR==errno && (vaxc$errno>>3)==(SS$_CONTROLC>>3)) \
        {                                                       \
          SCM_ASYNC_TICK;                                       \
          continue;                                             \
        }                                                       \
    }                                                           \
  while(0)
# endif /* ndef __GNUC__ */
#endif /* def vms */
#endif /* ndef SCM_SYSCALL  */

#ifndef SCM_SYSCALL
# ifdef EINTR
#  if (EINTR > 0)
#   define SCM_SYSCALL(line)                    \
  do                                            \
    {                                           \
      errno = 0;                                \
      line;                                     \
      if (errno == EINTR)                       \
        {                                       \
          SCM_ASYNC_TICK;                       \
          continue;                             \
        }                                       \
    }                                           \
  while(0)
#  endif /*  (EINTR > 0) */
# endif /* def EINTR */
#endif /* ndef SCM_SYSCALL */

#ifndef SCM_SYSCALL
# define SCM_SYSCALL(line) line;
#endif /* ndef SCM_SYSCALL */



#ifndef min
#define min(A, B) ((A) <= (B) ? (A) : (B))
#endif
#ifndef max
#define max(A, B) ((A) >= (B) ? (A) : (B))
#endif

/* Return the first integer greater than or equal to LEN such that
   LEN % ALIGN == 0.  Return LEN if ALIGN is zero.  */
#define ROUND_UP(len, align)					\
  ((align) ? (((len) - 1UL) | ((align) - 1UL)) + 1UL : (len))


#if defined GUILE_USE_64_CALLS && GUILE_USE_64_CALLS && defined(HAVE_STAT64)
#define CHOOSE_LARGEFILE(foo,foo64)     foo64
#else
#define CHOOSE_LARGEFILE(foo,foo64)     foo
#endif

/* These names are a bit long, but they make it clear what they represent. */
#if SCM_HAVE_STRUCT_DIRENT64 == 1
# define dirent_or_dirent64             CHOOSE_LARGEFILE(dirent,dirent64)
#else
# define dirent_or_dirent64             dirent
#endif
#define fstat_or_fstat64                CHOOSE_LARGEFILE(fstat,fstat64)
#define ftruncate_or_ftruncate64        CHOOSE_LARGEFILE(ftruncate,ftruncate64)
#define lseek_or_lseek64                CHOOSE_LARGEFILE(lseek,lseek64)
#define lstat_or_lstat64                CHOOSE_LARGEFILE(lstat,lstat64)
#define off_t_or_off64_t                CHOOSE_LARGEFILE(off_t,off64_t)
#define open_or_open64                  CHOOSE_LARGEFILE(open,open64)
#define readdir_or_readdir64            CHOOSE_LARGEFILE(readdir,readdir64)
#if SCM_HAVE_READDIR64_R == 1
# define readdir_r_or_readdir64_r       CHOOSE_LARGEFILE(readdir_r,readdir64_r)
#else
# define readdir_r_or_readdir64_r       readdir_r
#endif
#define stat_or_stat64                  CHOOSE_LARGEFILE(stat,stat64)
#define truncate_or_truncate64          CHOOSE_LARGEFILE(truncate,truncate64)
#define scm_from_off_t_or_off64_t       CHOOSE_LARGEFILE(scm_from_off_t,scm_from_int64)
#define scm_from_ino_t_or_ino64_t       CHOOSE_LARGEFILE(scm_from_ulong,scm_from_uint64)
#define scm_from_blkcnt_t_or_blkcnt64_t CHOOSE_LARGEFILE(scm_from_ulong,scm_from_uint64)
#define scm_to_off_t_or_off64_t         CHOOSE_LARGEFILE(scm_to_off_t,scm_to_int64)

#if SIZEOF_OFF_T == 4
#  define scm_to_off_t    scm_to_int32
#  define scm_from_off_t  scm_from_int32
#elif SIZEOF_OFF_T == 8
#  define scm_to_off_t    scm_to_int64
#  define scm_from_off_t  scm_from_int64
#else
#  error sizeof(off_t) is not 4 or 8.
#endif
#define scm_to_off64_t    scm_to_int64
#define scm_from_off64_t  scm_from_int64


/* The endianness marker in objcode.  */
#ifdef WORDS_BIGENDIAN
# define SCM_OBJCODE_ENDIANNESS "BE"
#else
# define SCM_OBJCODE_ENDIANNESS "LE"
#endif

#define _SCM_CPP_STRINGIFY(x)  # x
#define SCM_CPP_STRINGIFY(x)   _SCM_CPP_STRINGIFY (x)

/* The word size marker in objcode.  */
#define SCM_OBJCODE_WORD_SIZE  SCM_CPP_STRINGIFY (SIZEOF_VOID_P)

/* Major and minor versions must be single characters. */
#define SCM_OBJCODE_MAJOR_VERSION 2
#define SCM_OBJCODE_MINOR_VERSION 0
#define SCM_OBJCODE_MAJOR_VERSION_STRING        \
  SCM_CPP_STRINGIFY(SCM_OBJCODE_MAJOR_VERSION)
#define SCM_OBJCODE_MINOR_VERSION_STRING        \
  SCM_CPP_STRINGIFY(SCM_OBJCODE_MINOR_VERSION)
#define SCM_OBJCODE_VERSION_STRING                                      \
  SCM_OBJCODE_MAJOR_VERSION_STRING "." SCM_OBJCODE_MINOR_VERSION_STRING
#define SCM_OBJCODE_MACHINE_VERSION_STRING                              \
  SCM_OBJCODE_ENDIANNESS "-" SCM_OBJCODE_WORD_SIZE "-" SCM_OBJCODE_VERSION_STRING

/* The objcode magic header.  */
#define SCM_OBJCODE_COOKIE                              \
  "GOOF----" SCM_OBJCODE_MACHINE_VERSION_STRING

#endif  /* SCM__SCM_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

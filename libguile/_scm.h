/* classes: h_files */

#ifndef _SCMH
#define _SCMH
/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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


#include "__scm.h"

/* This file is only visible to the libguile sources */

/* Include headers for those files central to the implementation.  The
   rest should be explicitly #included in the C files themselves.  */
#include "error.h"		/* Everyone signals errors.  */
#include "print.h"		/* Everyone needs to print.  */
#include "pairs.h"		/* Everyone conses.  */
#include "list.h"		/* Everyone makes lists.  */
#include "gc.h"			/* Everyone allocates.  */
#include "gsubr.h"		/* Everyone defines global functions.  */
#include "procs.h"		/* Same.  */
#include "numbers.h"		/* Everyone deals with fixnums.  */
#include "symbols.h"		/* For length, chars, values, miscellany.  */
#include "boolean.h"		/* Everyone wonders about the truth.  */
#include "strings.h"		/* Everyone loves string.  */
#include "vectors.h"		/* Vectors are used for structures a lot.  */
#include "root.h"		/* Everyone uses these objects.  */
#include "ports.h"		/* Everyone does I/O.  */
#include "async.h"		/* Everyone allows/disallows ints.  */
#ifdef USE_THREADS
#include "threads.h"		/* The cooperative thread package does
				   switching at async ticks.  */
#endif
#include "snarf.h"		/* Everyone snarfs. */

/* On VMS, GNU C's errno.h contains a special hack to get link attributes
 * for errno correct for linking to the C RTL.
 */
#include <errno.h>

/* SCM_SYSCALL retries system calls that have been interrupted (EINTR) */
#ifdef vms
# ifndef __GNUC__
#  include <ssdef.h>
#  define SCM_SYSCALL(line) do{errno = 0;line;} \
	while(EVMSERR==errno && (vaxc$errno>>3)==(SS$_CONTROLC>>3))
# endif /* ndef __GNUC__ */
#endif /* def vms */

#ifndef SCM_SYSCALL
# ifdef EINTR
#  if (EINTR > 0)
#   define SCM_SYSCALL(line) do{errno = 0;line;}while(EINTR==errno)
#  endif /*  (EINTR > 0) */
# endif /* def EINTR */
#endif /* ndef SCM_SYSCALL */

#ifndef SCM_SYSCALL
# define SCM_SYSCALL(line) {line;}
#endif /* ndef SCM_SYSCALL */

#ifndef MSDOS
# ifdef ARM_ULIB
    extern volatile int errno;
# else
    extern int errno;
# endif /* def ARM_ULIB */
#endif /* ndef MSDOS */
#ifdef __TURBOC__
# if (__TURBOC__==1)
 /* Needed for TURBOC V1.0 */
 extern int errno;
# endif /*  (__TURBOC__==1) */
#endif /* def __TURBOC__ */



#ifndef min
#define min(A,B) ((A) <= (B) ? (A) : (B))
#endif
#ifndef max
#define max(A,B) ((A) >= (B) ? (A) : (B))
#endif

#endif  /* _SCMH */


/* classes: h_files */

#ifndef SCM___SCM_H
#define SCM___SCM_H

/* Copyright (C) 1995,1996,1998,1999,2000,2001,2002 Free Software Foundation, Inc.
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



/* "What's the difference between _scm.h and __scm.h?"

   _scm.h is not installed; it's only visible to the libguile sources
   themselves.

   __scm.h is installed, and is #included by <libguile.h>.  If both
   the client and libguile need some piece of information, and it
   doesn't fit well into the header file for any particular module, it
   should go in __scm.h.  */


/* {Compiler hints}
 *
 * The following macros are used to provide additional information for the
 * compiler, which may help to do better error checking and code
 * optimization.  A second benefit of these macros is, that they also provide
 * additional information to the developers.
 */

/* The macro SCM_NORETURN indicates that a function will never return.
 * Examples:
 *   1) int foo (char arg) SCM_NORETURN;
 */
#ifdef __GNUC__
#define SCM_NORETURN __attribute__ ((noreturn))
#else
#define SCM_NORETURN
#endif

/* The macro SCM_UNUSED indicates that a function, function argument or
 * variable may potentially be unused.
 * Examples:
 *   1) static int unused_function (char arg) SCM_UNUSED;
 *   2) int foo (char unused_argument SCM_UNUSED);
 *   3) int unused_variable SCM_UNUSED;
 */
#ifdef __GNUC__
#define SCM_UNUSED __attribute__ ((unused))
#else
#define SCM_UNUSED
#endif


/* {Supported Options}
 *
 * These may be defined or undefined.
 */

/* #define GUILE_DEBUG_FREELIST */

/* All the number support there is.
 */
#define BIGNUMS

/* GC should relinquish empty cons-pair arenas. */
/* cmm:FIXME look at this after done mangling the GC */
/* #define GC_FREE_SEGMENTS */

/* Provide a scheme-accessible count-down timer that
 * generates a pseudo-interrupt.
 */
#define TICKS


/* Use engineering notation when converting numbers strings?
 */
#undef ENGNOT


/* {Unsupported Options}
 *
 * These must be defined as given here.
 */


#define CCLO

/* Guile Scheme supports the #f/() distinction; Guile Lisp won't.  We
   have horrible plans for their unification.  */
#undef SICP



/* Random options (not yet supported or in final form). */

#define STACK_CHECKING
#undef NO_CEVAL_STACK_CHECKING



/* SCM_API is a macro prepended to all function and data definitions
   which should be exported or imported in the resulting dynamic link
   library (DLL) in the Win32 port. */

#if defined (SCM_IMPORT)
# define SCM_API __declspec (dllimport) extern
#elif defined (SCM_EXPORT) || defined (DLL_EXPORT)
# define SCM_API __declspec (dllexport) extern
#else
# define SCM_API extern
#endif



/* What did the configure script discover about the outside world?  */
#include "libguile/scmconfig.h"



/* {Debugging Options}
 *
 * These compile time options determine whether to include code that is only
 * useful for debugging guile itself or C level extensions to guile.  The
 * common prefix for all option macros of this kind is "SCM_DEBUG_".  It is
 * guaranteed that a macro named SCM_DEBUG_XXX is defined to be either 0 or 1,
 * i. e. there is no need to test for the undefined case.  This allows to use
 * these definitions comfortably within code, as in the following example:
 *   #define FOO do { if (SCM_DEBUG_XXX) bar(); else baz(); } while (0)
 * Any sane compiler will remove the unused branch without any performance
 * penalty for the resulting code.
 *
 * Note:  Some SCM_DEBUG_XXX options are not settable at configure time.
 * To change the value of such options you will have to edit this header
 * file or give suitable options to make, like:
 *   make all CFLAGS="-DSCM_DEBUG_XXX=1 ..."
 */


/* The value of SCM_DEBUG determines the default for most of the not yet
 * defined debugging options.  This allows, for example, to enable most of the
 * debugging options by simply defining SCM_DEBUG as 1.
 */
#ifndef SCM_DEBUG
#define SCM_DEBUG 0
#endif

/* If SCM_DEBUG_CELL_ACCESSES is set to 1, cell accesses will perform
 * exhaustive parameter checking:  It will be verified that cell parameters
 * actually point to a valid heap cell.  Note:  If this option is enabled,
 * guile will run about ten times slower than normally.
 */
#ifndef SCM_DEBUG_CELL_ACCESSES
#define SCM_DEBUG_CELL_ACCESSES SCM_DEBUG
#endif

/* If SCM_DEBUG_INTERRUPTS is set to 1, with every deferring and allowing of
 * interrupts a consistency check will be performed.
 */
#ifndef SCM_DEBUG_INTERRUPTS
#define SCM_DEBUG_INTERRUPTS SCM_DEBUG
#endif

/* If SCM_DEBUG_PAIR_ACCESSES is set to 1, accesses to cons cells will be
 * exhaustively checked.  Note:  If this option is enabled, guile will run
 * slower than normally.
 */
#ifndef SCM_DEBUG_PAIR_ACCESSES
#define SCM_DEBUG_PAIR_ACCESSES SCM_DEBUG
#endif

/* If SCM_DEBUG_REST_ARGUMENT is set to 1, functions that take rest arguments
 * will check whether the rest arguments are actually passed as a proper list.
 * Otherwise, if SCM_DEBUG_REST_ARGUMENT is 0, functions that take rest
 * arguments will take it for granted that these are passed as a proper list.
 */
#ifndef SCM_DEBUG_REST_ARGUMENT
#define SCM_DEBUG_REST_ARGUMENT SCM_DEBUG
#endif

/* Use this for _compile time_ type checking only, since the compiled result
 * will be quite inefficient.  The right way to make use of this option is to
 * do a 'make clean; make CFLAGS=-DSCM_DEBUG_TYPING_STRICTNESS=1', fix your
 * errors, and then do 'make clean; make'.
 */
#ifndef SCM_DEBUG_TYPING_STRICTNESS
#define SCM_DEBUG_TYPING_STRICTNESS 1
#endif



/* {Feature Options}
 *
 * These compile time options determine whether code for certain features
 * should be compiled into guile.  The common prefix for all option macros
 * of this kind is "SCM_ENABLE_".  It is guaranteed that a macro named
 * SCM_ENABLE_XXX is defined to be either 0 or 1, i. e. there is no need to
 * test for the undefined case.  This allows to use these definitions
 * comfortably within code, as in the following example:
 *   #define FOO do { if (SCM_ENABLE_XXX) bar(); else baz(); } while (0)
 * Any sane compiler will remove the unused branch without any performance
 * penalty for the resulting code.
 *
 * Note:  Some SCM_ENABLE_XXX options are not settable at configure time.
 * To change the value of such options you will have to edit this header
 * file or give suitable options to make, like:
 *   make all CFLAGS="-DSCM_ENABLE_XXX=1 ..."
 */

/* If SCM_ENABLE_DEPRECATED is set to 1, deprecated code will be included in
 * guile, as well as some functions to issue run-time warnings about uses of
 * deprecated functions.
 */
#ifndef SCM_ENABLE_DEPRECATED
#define SCM_ENABLE_DEPRECATED 0
#endif



/* {Architecture and compiler properties}
 *
 * Guile as of today can only work on systems which fulfill at least the
 * following requirements:
 * - long ints have at least 32 bits.
 *   Guile's type system is based on this assumption.
 * - long ints consist of at least four characters.
 *   It is assumed that cells, i. e. pairs of long ints, are eight character
 *   aligned, because three bits of a cell pointer are used for type data.
 * - sizeof (void*) == sizeof (long int)
 *   Pointers are stored in SCM objects, and sometimes SCM objects are passed
 *   as void*.  Thus, there has to be a one-to-one correspondence.
 * - numbers are encoded using two's complement.
 *   The implementation of the bitwise scheme level operations is based on
 *   this assumption.
 * - ... add more
 */

#if SIZEOF_UINTPTR_T != 0 && defined(UINTPTR_MAX) \
                          && defined(INTPTR_MAX) \
                          && defined(INTPTR_MIN)
/* Used as SCM if available, so we bundle related attributes to avoid possible
   type incon[st][oi]n[ae]nce later.  Word in tags.h.  */
#define HAVE_UINTPTR_T 1
#endif

#if SIZEOF_PTRDIFF_T != 0
#define HAVE_PTRDIFF_T 1
#endif

#if SIZEOF_LONG_LONG != 0
#define HAVE_LONG_LONGS 1
#define HAVE_LONG_LONG 1
#endif

#ifndef HAVE_PTRDIFF_T
typedef long ptrdiff_t;
#endif

#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif

#ifdef CHAR_BIT
# define SCM_CHAR_BIT CHAR_BIT
#else
# define SCM_CHAR_BIT 8
#endif

#ifdef LONG_BIT
# define SCM_LONG_BIT LONG_BIT
#else
# define SCM_LONG_BIT (SCM_CHAR_BIT * sizeof (long) / sizeof (char))
#endif

#ifdef UCHAR_MAX
# define SCM_CHAR_CODE_LIMIT (UCHAR_MAX + 1L)
#else
# define SCM_CHAR_CODE_LIMIT 256L
#endif



#ifdef STDC_HEADERS
# include <stdlib.h>
# if HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# if HAVE_SYS_STDTYPES_H
#  include <sys/stdtypes.h>
# endif
#  include <stddef.h>
#endif /* def STDC_HEADERS */



/* Define some additional CPP macros on Win32 platforms. */
#if USE_DLL_IMPORT
# define __REGEX_IMPORT__ 1
# define __CRYPT_IMPORT__ 1
# define __READLINE_IMPORT__ 1
# define QT_IMPORT 1
#endif



#include "libguile/tags.h"


#ifdef vms
# ifndef CHEAP_CONTINUATIONS
   typedef int jmp_buf[17];
   extern int setjump(jmp_buf env);
   extern int longjump(jmp_buf env, int ret);
#  define setjmp setjump
#  define longjmp longjump
# else
#  include <setjmp.h>
# endif
#else				/* ndef vms */
# ifdef _CRAY1
    typedef int jmp_buf[112];
    extern int setjump(jmp_buf env);
    extern int longjump(jmp_buf env, int ret);
#  define setjmp setjump
#  define longjmp longjump
# else				/* ndef _CRAY1 */
#  include <setjmp.h>
# endif				/* ndef _CRAY1 */
#endif				/* ndef vms */

/* James Clark came up with this neat one instruction fix for
 * continuations on the SPARC.  It flushes the register windows so
 * that all the state of the process is contained in the stack.
 */

#ifdef sparc
# define SCM_FLUSH_REGISTER_WINDOWS asm("ta 3")
#else
# define SCM_FLUSH_REGISTER_WINDOWS /* empty */
#endif

/* If stack is not longword aligned then
 */

/* #define SHORT_ALIGN */
#ifdef THINK_C
# define SHORT_ALIGN
#endif
#ifdef MSDOS
# define SHORT_ALIGN
#endif
#ifdef atarist
# define SHORT_ALIGN
#endif

#ifdef SHORT_ALIGN
typedef short SCM_STACKITEM;
#else
typedef long SCM_STACKITEM;
#endif


#define SCM_ASYNC_TICK /*fixme* should change names */ \
do { \
  if (scm_root->pending_asyncs) \
    scm_async_click (); \
} while (0)

#if (SCM_DEBUG_INTERRUPTS == 1)
#include <stdio.h>
#define SCM_CHECK_NOT_DISABLED \
  do { \
    if (scm_ints_disabled) \
      fprintf(stderr, "ints already disabled (at %s:%d)\n", \
              __FILE__, __LINE__); \
  } while (0)

#define SCM_CHECK_NOT_ENABLED \
  do { \
    if (!scm_ints_disabled) \
      fprintf(stderr, "ints already enabled (at %s:%d)\n", \
              __FILE__, __LINE__); \
  } while (0)

#else
#define SCM_CHECK_NOT_DISABLED
#define SCM_CHECK_NOT_ENABLED
#endif


/* Anthony Green writes:
   When the compiler sees...
	   DEFER_INTS;
	   [critical code here]
	   ALLOW_INTS;
   ...it doesn't actually promise to keep the critical code within the
   boundries of the DEFER/ALLOW_INTS instructions. It may very well
   schedule it outside of the magic defined in those macros.

   However, GCC's volatile asm feature forms a barrier over which code is
   never moved. So if you add...
	   asm ("");
   ...to each of the DEFER_INTS and ALLOW_INTS macros, the critical
   code will always remain in place.  asm's without inputs or outputs
   are implicitly volatile. */
#ifdef __GNUC__
#define SCM_FENCE asm /* volatile */ ("")
#else
#define SCM_FENCE
#endif

#define SCM_DEFER_INTS \
do { \
  SCM_FENCE; \
  SCM_CHECK_NOT_DISABLED; \
  SCM_CRITICAL_SECTION_START; \
  SCM_FENCE; \
  scm_ints_disabled = 1; \
  SCM_FENCE; \
} while (0)


#define SCM_ALLOW_INTS_ONLY \
do { \
  SCM_CRITICAL_SECTION_END; \
  scm_ints_disabled = 0; \
} while (0)


#define SCM_ALLOW_INTS \
do { \
  SCM_FENCE; \
  SCM_CHECK_NOT_ENABLED; \
  SCM_CRITICAL_SECTION_END; \
  SCM_FENCE; \
  scm_ints_disabled = 0; \
  SCM_FENCE; \
  SCM_THREAD_SWITCHING_CODE; \
  SCM_FENCE; \
} while (0)


#define SCM_REDEFER_INTS  \
do { \
  SCM_FENCE; \
  SCM_CRITICAL_SECTION_START; \
  ++scm_ints_disabled; \
  SCM_FENCE; \
} while (0)


#define SCM_REALLOW_INTS \
do { \
  SCM_FENCE; \
  SCM_CRITICAL_SECTION_END; \
  SCM_FENCE; \
  --scm_ints_disabled; \
  SCM_FENCE; \
} while (0)


#define SCM_TICK \
do { \
  SCM_ASYNC_TICK; \
  SCM_THREAD_SWITCHING_CODE; \
} while (0)



/* Classification of critical sections
 *
 * When Guile moves to POSIX threads, it won't be possible to prevent
 * context switching.  In fact, the whole idea of context switching is
 * bogus if threads are run by different processors.  Therefore, we
 * must ultimately eliminate all critical sections or enforce them by
 * use of mutecis.
 *
 * All instances of SCM_DEFER_INTS and SCM_ALLOW_INTS should therefore
 * be classified and replaced by one of the delimiters below.  If you
 * understand what this is all about, I'd like to encourage you to
 * help with this task.  The set of classes below must of course be
 * incrementally augmented.
 *
 * MDJ 980419 <djurfeldt@nada.kth.se>
 */

/* A sections
 *
 * Allocation of a cell with type tag in the CAR.
 *
 * With POSIX threads, each thread will have a private pool of free
 * cells.  Therefore, this type of section can be removed.  But!  It
 * is important that the CDR is initialized first (with the CAR still
 * indicating a free cell) so that we can guarantee a consistent heap
 * at all times.
 */

#define SCM_ENTER_A_SECTION SCM_CRITICAL_SECTION_START
#define SCM_EXIT_A_SECTION SCM_CRITICAL_SECTION_END



/** SCM_ASSERT
 **
 **/


#ifdef SCM_RECKLESS
#define SCM_ASSERT(_cond, _arg, _pos, _subr)
#define SCM_ASSERT_TYPE(_cond, _arg, _pos, _subr, _msg)
#define SCM_ASRTGO(_cond, _label)
#else
#define SCM_ASSERT(_cond, _arg, _pos, _subr) \
	if (!(_cond)) \
          scm_wrong_type_arg (_subr, _pos, _arg)
#define SCM_ASSERT_TYPE(_cond, _arg, _pos, _subr, _msg) \
	if (!(_cond)) \
          scm_wrong_type_arg_msg(_subr, _pos, _arg, _msg)
#define SCM_ASRTGO(_cond, _label) \
        if (!(_cond)) \
          goto _label
#endif

/*
 * SCM_WTA_DISPATCH
 */

/* Dirk:FIXME:: In all of the SCM_WTA_DISPATCH_* macros it is assumed that
 * 'gf' is zero if uninitialized.  It would be cleaner if some valid SCM value
 * like SCM_BOOL_F or SCM_UNDEFINED was chosen.
 */

SCM_API SCM scm_call_generic_0 (SCM gf);

#define SCM_WTA_DISPATCH_0(gf, subr)			        \
  return (SCM_UNPACK (gf)					\
	  ? scm_call_generic_0 ((gf))				\
	  : (scm_error_num_args_subr ((subr)), SCM_UNSPECIFIED))
#define SCM_GASSERT0(cond, gf, subr) \
  if (!(cond)) SCM_WTA_DISPATCH_0((gf), (subr))

SCM_API SCM scm_call_generic_1 (SCM gf, SCM a1);

#define SCM_WTA_DISPATCH_1(gf, a1, pos, subr)			\
  return (SCM_UNPACK (gf)					\
	  ? scm_call_generic_1 ((gf), (a1))			\
	  : (scm_wrong_type_arg ((subr), (pos), (a1)), SCM_UNSPECIFIED))
#define SCM_GASSERT1(cond, gf, a1, pos, subr) \
  if (!(cond)) SCM_WTA_DISPATCH_1((gf), (a1), (pos), (subr))

SCM_API SCM scm_call_generic_2 (SCM gf, SCM a1, SCM a2);

#define SCM_WTA_DISPATCH_2(gf, a1, a2, pos, subr)			\
  return (SCM_UNPACK (gf)						\
	  ? scm_call_generic_2 ((gf), (a1), (a2))			\
	  : (scm_wrong_type_arg ((subr), (pos),				\
				 (pos) == SCM_ARG1 ? (a1) : (a2)),	\
	     SCM_UNSPECIFIED))
#define SCM_GASSERT2(cond, gf, a1, a2, pos, subr) \
  if (!(cond)) SCM_WTA_DISPATCH_2((gf), (a1), (a2), (pos), (subr))

SCM_API SCM scm_apply_generic (SCM gf, SCM args);

#define SCM_WTA_DISPATCH_n(gf, args, pos, subr)				  \
  return (SCM_UNPACK (gf)						  \
	  ? scm_apply_generic ((gf), (args))				  \
	  : (scm_wrong_type_arg ((subr), (pos),				  \
				 scm_list_ref ((args),			  \
					       SCM_MAKINUM ((pos) - 1))), \
	     SCM_UNSPECIFIED))
#define SCM_GASSERTn(cond, gf, args, pos, subr) \
  if (!(cond)) SCM_WTA_DISPATCH_n((gf), (args), (pos), (subr))

#ifndef SCM_MAGIC_SNARFER
/* Let these macros pass through if
   we are snarfing;  thus we can tell the
   difference between the use of an actual
   number vs. the use of one of these macros --
   actual numbers in SCM_VALIDATE_* and SCM_ASSERT
   constructs must match the formal argument name,
   but using SCM_ARG* avoids the test */

#define SCM_ARGn 		0
#define SCM_ARG1 		1
#define SCM_ARG2 		2
#define SCM_ARG3 		3
#define SCM_ARG4 		4
#define SCM_ARG5 		5
#define SCM_ARG6 		6
#define SCM_ARG7 		7

#endif /* SCM_MAGIC_SNARFER */



/* SCM_EXIT_SUCCESS is the default code to return from SCM if no errors
 * were encountered.  SCM_EXIT_FAILURE is the default code to return from
 * SCM if errors were encountered.  The return code can be explicitly
 * specified in a SCM program with (scm_quit <n>).
 */

#ifndef SCM_EXIT_SUCCESS
#ifdef vms
#define SCM_EXIT_SUCCESS 1
#else
#define SCM_EXIT_SUCCESS 0
#endif /* def vms */
#endif /* ndef SCM_EXIT_SUCCESS */
#ifndef SCM_EXIT_FAILURE
#ifdef vms
#define SCM_EXIT_FAILURE 2
#else
#define SCM_EXIT_FAILURE 1
#endif /* def vms */
#endif /* ndef SCM_EXIT_FAILURE */

#endif  /* SCM___SCM_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

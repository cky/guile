/* classes: h_files */

#ifndef SCM_INLINE_H
#define SCM_INLINE_H

/* Copyright (C) 2001 Free Software Foundation, Inc.
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

/* This file is for inline functions.  On platforms that don't support
   inlining functions, they are turned into ordinary functions.  See
   "inline.c".
*/


#if (SCM_DEBUG_CELL_ACCESSES == 1)
#include <stdio.h>

#endif

#include "libguile/pairs.h"
#include "libguile/gc.h"


SCM_API SCM scm_cell (scm_t_bits car, scm_t_bits cdr);
SCM_API SCM scm_double_cell (scm_t_bits car, scm_t_bits cbr,
			     scm_t_bits ccr, scm_t_bits cdr);

#ifdef HAVE_INLINE

#ifndef EXTERN_INLINE
#define EXTERN_INLINE extern inline
#endif

extern unsigned scm_newcell2_count;
extern unsigned scm_newcell_count;



EXTERN_INLINE
SCM
scm_cell (scm_t_bits car, scm_t_bits cdr)
{
  SCM z;

  if (SCM_NULLP (scm_i_freelist))
    {
      z = scm_gc_for_newcell (&scm_i_master_freelist, &scm_i_freelist);
    }
  else
    {
      z = scm_i_freelist;
      scm_i_freelist = SCM_FREE_CELL_CDR (scm_i_freelist);
    }

  /*
    We update scm_cells_allocated from this function. If we don't
    update this explicitly, we will have to walk a freelist somewhere
    later on, which seems a lot more expensive.
   */
  scm_cells_allocated += 1;  

#if (SCM_DEBUG_CELL_ACCESSES == 1)
    if (scm_debug_cell_accesses_p)
      {
	if (SCM_GC_MARK_P (z))
	  {
	    fprintf(stderr, "scm_cell tried to allocate a marked cell.\n");
	    abort();
	  }
	else if (SCM_GC_CELL_TYPE(z) != scm_tc_free_cell)
	  {
	    fprintf(stderr, "cell from freelist is not a free cell.\n");
	    abort();
	  }
      }

    /*
      Always set mark. Otherwise cells that are alloced before
      scm_debug_cell_accesses_p is toggled seem invalid.
    */
    SCM_SET_GC_MARK (z);

    /*
      TODO: figure out if this use of mark bits is valid with
      threading. What if another thread is doing GC at this point
      ... ?
     */
      
#endif

  
  /* Initialize the type slot last so that the cell is ignored by the
     GC until it is completely initialized.  This is only relevant
     when the GC can actually run during this code, which it can't for
     cooperating threads, but it might be important when we get true
     preemptive threads.
  */
  SCM_GC_SET_CELL_WORD (z, 1, cdr);
  SCM_GC_SET_CELL_WORD (z, 0, car);

#ifdef USE_THREADS
#if !defined(USE_COOP_THREADS) && !defined(USE_NULL_THREADS)
  /* When we are using preemtive threads, we might need to make
     sure that the initial values for the slots are protected until
     the cell is completely initialized.
  */
#error review me
  scm_remember_upto_here_1 (SCM_PACK (cdr));
#endif
#endif


#if (SCM_DEBUG_CELL_ACCESSES == 1)
  if (scm_expensive_debug_cell_accesses_p )
    scm_i_expensive_validation_check (z);
#endif
  
  return z;
}

EXTERN_INLINE
SCM
scm_double_cell (scm_t_bits car, scm_t_bits cbr,
		 scm_t_bits ccr, scm_t_bits cdr)
{
  SCM z;


  if (SCM_NULLP (scm_i_freelist2))
    {
      z = scm_gc_for_newcell (&scm_i_master_freelist2, &scm_i_freelist2);
    }
  else
    {
      z = scm_i_freelist2;
      scm_i_freelist2 = SCM_FREE_CELL_CDR (scm_i_freelist2);
    }

  scm_cells_allocated += 2;

  /* Initialize the type slot last so that the cell is ignored by the
     GC until it is completely initialized.  This is only relevant
     when the GC can actually run during this code, which it can't for
     cooperating threads, but it might be important when we get true
     preemptive threads.
  */
  SCM_GC_SET_CELL_WORD (z, 1, cbr);
  SCM_GC_SET_CELL_WORD (z, 2, ccr);
  SCM_GC_SET_CELL_WORD (z, 3, cdr);
  SCM_GC_SET_CELL_WORD (z, 0, car);

#ifdef USE_THREADS
#if !defined(USE_COOP_THREADS) && !defined(USE_NULL_THREADS)
  /* When we are using non-cooperating threads, we might need to make
     sure that the initial values for the slots are protected until
     the cell is completely initialized.
  */
#error review me
  scm_remember_upto_here_3 (SCM_PACK (cbr), SCM_PACK (ccr), SCM_PACK (cdr));
#endif
#endif


#if (SCM_DEBUG_CELL_ACCESSES == 1)
  if (scm_debug_cell_accesses_p)
    {
      if (SCM_GC_MARK_P (z))
	{
	  fprintf(stderr,
		  "scm_double_cell tried to allocate a marked cell.\n");
	  abort();
	}
    }

  /* see above. */
  SCM_SET_GC_MARK (z);

#endif

  /* When this function is inlined, it's possible that the last
     SCM_GC_SET_CELL_WORD above will be adjacent to a following
     initialization of z.  E.g., it occurred in scm_make_real.  GCC
     from around version 3 (e.g., certainly 3.2) began taking
     advantage of strict C aliasing rules which say that it's OK to
     interchange the initialization above and the one below when the
     pointer types appear to differ sufficiently.  We don't want that,
     of course.  GCC allows this behaviour to be disabled with the
     -fno-strict-aliasing option, but would also need to be supplied
     by Guile users.  Instead, the following statements prevent the
     reordering.
   */
#ifdef __GNUC__
  asm volatile ("" : : : "memory");
#else
  /* portable version, just in case any other compiler does the same
     thing.  */
  scm_remember_upto_here_1 (z);
#endif

  return z;
}



#endif
#endif

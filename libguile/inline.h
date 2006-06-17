/* classes: h_files */

#ifndef SCM_INLINE_H
#define SCM_INLINE_H

/* Copyright (C) 2001, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/* This file is for inline functions.  On platforms that don't support
   inlining functions, they are turned into ordinary functions.  See
   "inline.c".
*/

#include "libguile/__scm.h"

#if (SCM_DEBUG_CELL_ACCESSES == 1)
#include <stdio.h>
#endif

#include "libguile/pairs.h"
#include "libguile/gc.h"
#include "libguile/threads.h"
#include "libguile/unif.h"
#include "libguile/pairs.h"


SCM_API SCM scm_cell (scm_t_bits car, scm_t_bits cdr);
SCM_API SCM scm_double_cell (scm_t_bits car, scm_t_bits cbr,
			     scm_t_bits ccr, scm_t_bits cdr);

SCM_API SCM scm_array_handle_ref (scm_t_array_handle *h, ssize_t pos);
SCM_API void scm_array_handle_set (scm_t_array_handle *h, ssize_t pos, SCM val);


#if defined SCM_C_INLINE || defined SCM_INLINE_C_INCLUDING_INLINE_H
/* either inlining, or being included from inline.c.  We use (and
   repeat) this long #if test here and below so that we don't have to
   introduce any extraneous symbols into the public namespace.  We
   only need SCM_C_INLINE to be seen publically . */

extern unsigned scm_newcell2_count;
extern unsigned scm_newcell_count;

#if defined SCM_C_INLINE && ! defined SCM_INLINE_C_INCLUDING_INLINE_H
/* definitely inlining */
#ifdef __GNUC__
extern
#else
static
#endif
SCM_C_INLINE
#endif
SCM
scm_cell (scm_t_bits car, scm_t_bits cdr)
{
  SCM z;
  SCM *freelist = SCM_FREELIST_LOC (scm_i_freelist);

  if (scm_is_null (*freelist))
    z = scm_gc_for_newcell (&scm_i_master_freelist, freelist);
  else
    {
      z = *freelist;
      *freelist = SCM_FREE_CELL_CDR (*freelist);
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
	else if (SCM_GC_CELL_WORD(z, 0) != scm_tc_free_cell)
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
     when the GC can actually run during this code, which it can't
     since the GC only runs when all other threads are stopped.
  */
  SCM_GC_SET_CELL_WORD (z, 1, cdr);
  SCM_GC_SET_CELL_WORD (z, 0, car);

#if (SCM_DEBUG_CELL_ACCESSES == 1)
  if (scm_expensive_debug_cell_accesses_p )
    scm_i_expensive_validation_check (z);
#endif
  
  return z;
}

#if defined SCM_C_INLINE && ! defined SCM_INLINE_C_INCLUDING_INLINE_H
/* definitely inlining */
#ifdef __GNUC__
extern
#else
static
#endif
SCM_C_INLINE
#endif
SCM
scm_double_cell (scm_t_bits car, scm_t_bits cbr,
		 scm_t_bits ccr, scm_t_bits cdr)
{
  SCM z;
  SCM *freelist = SCM_FREELIST_LOC (scm_i_freelist2);

  if (scm_is_null (*freelist))
    z = scm_gc_for_newcell (&scm_i_master_freelist2, freelist);
  else
    {
      z = *freelist;
      *freelist = SCM_FREE_CELL_CDR (*freelist);
    }

  scm_cells_allocated += 2;

  /* Initialize the type slot last so that the cell is ignored by the
     GC until it is completely initialized.  This is only relevant
     when the GC can actually run during this code, which it can't
     since the GC only runs when all other threads are stopped.
  */
  SCM_GC_SET_CELL_WORD (z, 1, cbr);
  SCM_GC_SET_CELL_WORD (z, 2, ccr);
  SCM_GC_SET_CELL_WORD (z, 3, cdr);
  SCM_GC_SET_CELL_WORD (z, 0, car);

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
  __asm__ volatile ("" : : : "memory");
#else
  /* portable version, just in case any other compiler does the same
     thing.  */
  scm_remember_upto_here_1 (z);
#endif

  return z;
}

#if defined SCM_C_INLINE && ! defined SCM_INLINE_C_INCLUDING_INLINE_H
/* definitely inlining */
#ifdef __GNUC__
extern
#else
static
#endif
SCM_C_INLINE
#endif
SCM
scm_array_handle_ref (scm_t_array_handle *h, ssize_t p)
{
  return h->ref (h, p);
}

#if defined SCM_C_INLINE && ! defined SCM_INLINE_C_INCLUDING_INLINE_H
/* definitely inlining */
#ifdef __GNUC__
extern
#else
static
#endif
SCM_C_INLINE
#endif
void
scm_array_handle_set (scm_t_array_handle *h, ssize_t p, SCM v)
{
  h->set (h, p, v);
}

#if defined SCM_C_INLINE && ! defined SCM_INLINE_C_INCLUDING_INLINE_H
/* definitely inlining */
#ifdef __GNUC__
extern
#else
static
#endif
SCM_C_INLINE
#endif
int
scm_is_pair (SCM x)
{
  /* The following "workaround_for_gcc_295" avoids bad code generated by
     i386 gcc 2.95.4 (the Debian packaged 2.95.4-24 at least).

     Under the default -O2 the inlined SCM_I_CONSP test gets "optimized" so
     the fetch of the tag word from x is done before confirming it's a
     non-immediate (SCM_NIMP).  Needless to say that bombs badly if x is a
     immediate.  This was seen to afflict scm_srfi1_split_at and something
     deep in the bowels of ceval().  In both cases segvs resulted from
     deferencing a random immediate value.  srfi-1.test exposes the problem
     through a short list, the immediate being SCM_EOL in that case.
     Something in syntax.test exposed the ceval() problem.

     Just "volatile SCM workaround_for_gcc_295 = lst" is enough to avoid the
     problem, without even using that variable.  The "w=w" is just to
     prevent a warning about it being unused.
     */
#if defined (__GNUC__) && __GNUC__ == 2 && __GNUC_MINOR__ == 95
  volatile SCM workaround_for_gcc_295 = x;
  workaround_for_gcc_295 = workaround_for_gcc_295;
#endif

  return SCM_I_CONSP (x);
}

#endif
#endif

/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002 Free Software Foundation, Inc.
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

#include <assert.h>
#include <stdio.h>

#include "libguile/private-gc.h"
#include "libguile/gc.h"
#include "libguile/deprecation.h"
#include "libguile/private-gc.h"

scm_t_cell_type_statistics scm_i_master_freelist;
scm_t_cell_type_statistics scm_i_master_freelist2;




/*

In older versions of GUILE GC there was extensive support for
debugging freelists. This was useful, since the freelist was kept
inside the heap, and writing to an object that was GC'd would mangle
the list. Mark bits are now separate, and checking for sane cell
access can be done much more easily by simply checking if the mark bit
is unset before allocation.  --hwn



*/

#if (SCM_ENABLE_DEPRECATED == 1)
#if defined(GUILE_DEBUG_FREELIST)

SCM_DEFINE (scm_map_free_list, "map-free-list", 0, 0, 0,
            (),
	    "DEPRECATED\n")
#define FUNC_NAME s_scm_map_free_list
{
  scm_c_issue_deprecation_warning ("map-free-list has been removed from GUILE. Doing nothing\n");
  return SCM_UNSPECIFIED;
}  

SCM_DEFINE (scm_gc_set_debug_check_freelist_x, "gc-set-debug-check-freelist!", 1, 0, 0,
            (SCM flag),
	    "DEPRECATED.\n")
#define FUNC_NAME s_scm_gc_set_debug_check_freelist_x
{
  scm_c_issue_deprecation_warning ("gc-set-debug-check-freelist! has been removed from GUILE. Doing nothing\n");
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


#endif /* defined (GUILE_DEBUG) */
#endif /* deprecated */




/*
  This adjust FREELIST variables to decide wether or not to allocate
  more heap in the next GC run. It uses scm_gc_cells_collected and scm_gc_cells_collected1
 */

void
scm_i_adjust_min_yield (scm_t_cell_type_statistics *freelist)
{
  /* min yield is adjusted upwards so that next predicted total yield
   * (allocated cells actually freed by GC) becomes
   * `min_yield_fraction' of total heap size.  Note, however, that
   * the absolute value of min_yield will correspond to `collected'
   * on one master (the one which currently is triggering GC).
   *
   * The reason why we look at total yield instead of cells collected
   * on one list is that we want to take other freelists into account.
   * On this freelist, we know that (local) yield = collected cells,
   * but that's probably not the case on the other lists.
   *
   * (We might consider computing a better prediction, for example
   *  by computing an average over multiple GC:s.)
   */
  if (freelist->min_yield_fraction)
    {
      /* Pick largest of last two yields. */
      long delta = ((SCM_HEAP_SIZE * freelist->min_yield_fraction / 100)
		   - (long) SCM_MAX (scm_gc_cells_collected_1, scm_gc_cells_collected));
#ifdef DEBUGINFO
      fprintf (stderr, " after GC = %lu, delta = %ld\n",
	       (unsigned long) scm_cells_allocated,
	       (long) delta);
#endif
      if (delta > 0)
	freelist->min_yield += delta;
    }
}


static void
scm_init_freelist (scm_t_cell_type_statistics *freelist,
	       int span,
	       int min_yield)
{
  freelist->heap_segment_idx = -1;
  freelist->min_yield = 0;
  freelist->min_yield_fraction = min_yield;
  freelist->span = span;
  freelist->collected = 0;
  freelist->collected_1 = 0;
  freelist->heap_size = 0;
}

#if (SCM_ENABLE_DEPRECATED == 1)
 size_t scm_default_init_heap_size_1;
 int scm_default_min_yield_1;
 size_t scm_default_init_heap_size_2;
 int scm_default_min_yield_2;
 size_t scm_default_max_segment_size;
#endif

void
scm_gc_init_freelist (void)
{
  size_t init_heap_size_1
    = scm_getenv_int ("GUILE_INIT_SEGMENT_SIZE_1", SCM_DEFAULT_INIT_HEAP_SIZE_1);

  size_t init_heap_size_2
    = scm_getenv_int ("GUILE_INIT_SEGMENT_SIZE_2", SCM_DEFAULT_INIT_HEAP_SIZE_2);

  scm_i_freelist = SCM_EOL;
  scm_i_freelist2 = SCM_EOL;
  
  scm_init_freelist (&scm_i_master_freelist2, 2, 
		     scm_getenv_int ("GUILE_MIN_YIELD_2", SCM_DEFAULT_MIN_YIELD_2));
  scm_init_freelist (&scm_i_master_freelist, 1,
		     scm_getenv_int ("GUILE_MIN_YIELD_1", SCM_DEFAULT_MIN_YIELD_1));


  scm_max_segment_size = scm_getenv_int ("GUILE_MAX_SEGMENT_SIZE", SCM_DEFAULT_MAX_SEGMENT_SIZE);
  
  scm_i_make_initial_segment (init_heap_size_1, &scm_i_master_freelist);
  scm_i_make_initial_segment (init_heap_size_2, &scm_i_master_freelist2);

  
#if (SCM_ENABLE_DEPRECATED == 1)
  if ( scm_default_init_heap_size_1 ||
       scm_default_min_yield_1||
       scm_default_init_heap_size_2||
       scm_default_min_yield_2||
       scm_default_max_segment_size)
    {
      scm_c_issue_deprecation_warning ("Tuning heap parameters with C variables is deprecated. Use environment variables instead.");
    }
#endif
}


void
scm_i_gc_sweep_freelist_reset (scm_t_cell_type_statistics *freelist)
{
  freelist->collected_1 = freelist->collected;
  freelist->collected = 0;
  
  /*
    at the end we simply start with the lowest segment again.
   */
  freelist->heap_segment_idx = -1;
}

int
scm_i_gc_grow_heap_p (scm_t_cell_type_statistics * freelist)
{
  return SCM_MAX (freelist->collected,freelist->collected_1)  < freelist->min_yield;
}

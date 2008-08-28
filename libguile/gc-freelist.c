/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2006 Free Software Foundation, Inc.
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
#define FUNC_NAME "s_scm_map_free_list"
{
  scm_c_issue_deprecation_warning ("map-free-list has been removed from GUILE. Doing nothing\n");
  return SCM_UNSPECIFIED;
}  
#undef FUNC_NAME

SCM_DEFINE (scm_gc_set_debug_check_freelist_x, "gc-set-debug-check-freelist!", 1, 0, 0,
            (SCM flag),
	    "DEPRECATED.\n")
#define FUNC_NAME "s_scm_gc_set_debug_check_freelist_x"
{
  scm_c_issue_deprecation_warning ("gc-set-debug-check-freelist! has been removed from GUILE. Doing nothing\n");
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


#endif /* defined (GUILE_DEBUG) */
#endif /* deprecated */

static void
scm_init_freelist (scm_t_cell_type_statistics *freelist,
		   int span,
		   int min_yield_percentage)
{
  if (min_yield_percentage < 1)
    min_yield_percentage = 1;
  if (min_yield_percentage > 99)
    min_yield_percentage = 99;

  freelist->heap_segment_idx = -1;
  freelist->min_yield_fraction = min_yield_percentage / 100.0;
  freelist->span = span;
  freelist->swept = 0;
  freelist->collected = 0;
  freelist->heap_total_cells = 0;
}

#if (SCM_ENABLE_DEPRECATED == 1)
size_t scm_default_init_heap_size_1;
int scm_default_min_yield_1;
size_t scm_default_init_heap_size_2;
int scm_default_min_yield_2;
size_t scm_default_max_segment_size;

static void
check_deprecated_heap_vars (void)  {
  if (scm_default_init_heap_size_1 ||
      scm_default_min_yield_1||
      scm_default_init_heap_size_2||
      scm_default_min_yield_2||
      scm_default_max_segment_size)
    {
      scm_c_issue_deprecation_warning ("Tuning heap parameters with C variables is deprecated. Use environment variables instead.");
    }
}
#else
static void check_deprecated_heap_vars (void) { }
#endif

void
scm_gc_init_freelist (void)
{
  const char *error_message =
    "Could not allocate initial heap of %uld.\n"
    "Try adjusting GUILE_INIT_SEGMENT_SIZE_%d\n";
 
  int init_heap_size_1
    = scm_getenv_int ("GUILE_INIT_SEGMENT_SIZE_1", SCM_DEFAULT_INIT_HEAP_SIZE_1);
  int init_heap_size_2
    = scm_getenv_int ("GUILE_INIT_SEGMENT_SIZE_2", SCM_DEFAULT_INIT_HEAP_SIZE_2);

  scm_init_freelist (&scm_i_master_freelist2, 2, 
		     scm_getenv_int ("GUILE_MIN_YIELD_2", SCM_DEFAULT_MIN_YIELD_2));
  scm_init_freelist (&scm_i_master_freelist, 1,
		     scm_getenv_int ("GUILE_MIN_YIELD_1", SCM_DEFAULT_MIN_YIELD_1));

  scm_max_segment_size = scm_getenv_int ("GUILE_MAX_SEGMENT_SIZE", SCM_DEFAULT_MAX_SEGMENT_SIZE);

  if (scm_max_segment_size <= 0)
    scm_max_segment_size = SCM_DEFAULT_MAX_SEGMENT_SIZE;
   
  if (scm_i_get_new_heap_segment (&scm_i_master_freelist,
				  init_heap_size_1, return_on_error) == -1)  {
    fprintf (stderr, error_message, init_heap_size_1, 1);
    abort ();
  }
  if (scm_i_get_new_heap_segment (&scm_i_master_freelist2,
				  init_heap_size_2, return_on_error) == -1) {
    fprintf (stderr, error_message, init_heap_size_2, 2);
    abort ();
  }

  check_deprecated_heap_vars ();
}



void
scm_i_gc_sweep_freelist_reset (scm_t_cell_type_statistics *freelist)
{
  freelist->collected = 0;
  freelist->swept = 0;
  /*
    at the end we simply start with the lowest segment again.
   */
  freelist->heap_segment_idx = -1;
}


/*
  Returns how many more cells we should allocate according to our
  policy.  May return negative if we don't need to allocate more. 


  The new yield should at least equal gc fraction of new heap size, i.e.

  c + dh > f * (h + dh)

  c : collected
  f : min yield fraction
  h : heap size
  dh : size of new heap segment

  this gives dh > (f * h - c) / (1 - f).
*/
float
scm_i_gc_heap_size_delta (scm_t_cell_type_statistics * freelist)
{
  float f = freelist->min_yield_fraction;
  float collected = freelist->collected;
  float swept = freelist->swept;
  float delta = ((f * swept - collected) / (1.0 - f));

#if 0
  assert (freelist->heap_total_cells >= freelist->collected);
  assert (freelist->swept == freelist->heap_total_cells);
  assert (swept >= collected);
#endif

  return delta;
}

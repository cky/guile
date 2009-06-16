/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2006, 2008 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h> 
#include <stdio.h>
#include <string.h>

#include <count-one-bits.h>

#include "libguile/_scm.h"
#include "libguile/pairs.h"
#include "libguile/gc.h"
#include "libguile/private-gc.h"

size_t scm_max_segment_size;

/* Important entry point: try to grab some memory, and make it into a
   segment; return the index of the segment.  SWEEP_STATS should contain
   global GC sweep statistics collected since the last full GC.

   Returns the index of the segment.  If error_policy !=
   abort_on_error, we return -1 on failure.
*/
int
scm_i_get_new_heap_segment (scm_t_cell_type_statistics *freelist,
			    size_t len,
			    policy_on_error error_policy)
{
  if (len > scm_max_segment_size)
    len = scm_max_segment_size;

  if (len < SCM_MIN_HEAP_SEG_SIZE)
    len = SCM_MIN_HEAP_SEG_SIZE;

  /* todo: consider having a more flexible lower bound. */
  {
    scm_t_heap_segment *seg = scm_i_make_empty_heap_segment (freelist);

    /* Allocate with decaying ambition. */
    while (len >= SCM_MIN_HEAP_SEG_SIZE)
      {
	if (scm_i_initialize_heap_segment_data (seg, len))
	  return scm_i_insert_segment (seg);
	
	len /= 2;
      }
  }

  if (error_policy == abort_on_error)
    {
      fprintf (stderr, "scm_i_get_new_heap_segment: Could not grow heap.\n");
      abort ();
    }
  return -1;
}


scm_t_heap_segment *
scm_i_make_empty_heap_segment (scm_t_cell_type_statistics *fl)
{
  scm_t_heap_segment *shs = calloc (1, sizeof (scm_t_heap_segment));

  if (!shs)
    {
      fprintf (stderr, "scm_i_get_new_heap_segment: out of memory.\n");
      abort ();
    }
  
  shs->span = fl->span;
  shs->freelist  = fl;
  
  return shs;
}

void
scm_i_heap_segment_statistics (scm_t_heap_segment *seg, SCM tab)
{
  scm_t_cell *p = seg->bounds[0];
  while (p <  seg->bounds[1])
    {
      scm_i_card_statistics (p, tab, seg); 
      p += SCM_GC_CARD_N_CELLS;
    }
}

/*
  count number of marked bits, so we know how much cells are live.
 */
int
scm_i_heap_segment_marked_count (scm_t_heap_segment *seg)
{
  scm_t_c_bvec_long *bvec = (scm_t_c_bvec_long *) seg->bounds[1];
  scm_t_c_bvec_long *bvec_end =
    (bvec +
     scm_i_segment_card_count (seg) * SCM_GC_CARD_BVEC_SIZE_IN_LONGS);
  
  int count = 0;
  while (bvec < bvec_end)
    {
      count += count_one_bits_l (*bvec);
      bvec ++;
    }
  return count * seg->span;
}

int
scm_i_segment_card_number (scm_t_heap_segment *seg,
			   scm_t_cell *card)
{
  return (card - seg->bounds[0]) / SCM_GC_CARD_N_CELLS;
}

/*
  Fill SEGMENT with memory both for data and mark bits.

  RETURN: 1 on success, 0 failure  
 */
int 
scm_i_initialize_heap_segment_data (scm_t_heap_segment *segment, size_t requested)
{
  /*
    round upwards
   */
  int card_data_cell_count = (SCM_GC_CARD_N_CELLS - SCM_GC_CARD_N_HEADER_CELLS);
  int card_count = 1 + (requested / sizeof (scm_t_cell)) / card_data_cell_count; 

  /*
    one card extra due to alignment
  */
  size_t mem_needed = (1 + card_count) * SCM_GC_SIZEOF_CARD
    + SCM_GC_CARD_BVEC_SIZE_IN_LONGS * card_count * SCM_SIZEOF_LONG;
  scm_t_cell *memory = 0;

  /*
    We use calloc to alloc the heap, so it is nicely initialized.
   */
  SCM_SYSCALL (memory = (scm_t_cell *) calloc (1, mem_needed));

  if (memory == NULL)
    return 0;

  segment->malloced = memory;
  segment->bounds[0] = SCM_GC_CARD_UP (memory);
  segment->bounds[1] = segment->bounds[0] + card_count * SCM_GC_CARD_N_CELLS;
  segment->freelist->heap_total_cells += scm_i_segment_cell_count (segment);

  /*
    Don't init the mem or the bitvector. This is handled by lazy
    sweeping.
  */
  segment->next_free_card = segment->bounds[0];
  segment->first_time = 1;
  return 1;
}

int
scm_i_segment_card_count (scm_t_heap_segment *seg)
{
  return (seg->bounds[1] - seg->bounds[0]) / SCM_GC_CARD_N_CELLS;
}

/*
  Return the number of available single-cell data cells. 
 */
int
scm_i_segment_cell_count (scm_t_heap_segment *seg)
{
  return scm_i_segment_card_count (seg)
    * scm_i_segment_cells_per_card (seg);
}

int
scm_i_segment_cells_per_card (scm_t_heap_segment *seg)
{
  return (SCM_GC_CARD_N_CELLS - SCM_GC_CARD_N_HEADER_CELLS
	  + ((seg->span == 2) ? -1 : 0));
}

void
scm_i_clear_segment_mark_space (scm_t_heap_segment *seg)
{
  scm_t_cell *markspace = seg->bounds[1];

  memset (markspace, 0x00,
	  scm_i_segment_card_count (seg) * SCM_GC_CARD_BVEC_SIZE_IN_LONGS * SCM_SIZEOF_LONG);
}


/*
  Force a sweep of this entire segment.
 */
void
scm_i_sweep_segment (scm_t_heap_segment *seg,
		     scm_t_sweep_statistics *sweep_stats)
{
  int infinity = 1 << 30;
  scm_t_cell *remember = seg->next_free_card;  
  while (scm_i_sweep_some_cards (seg, sweep_stats, infinity) != SCM_EOL)
    ;
  seg->next_free_card = remember;
}


/* Sweep cards from SEG until we've gathered THRESHOLD cells.  On
   return, SWEEP_STATS, if non-NULL, contains the number of cells that
   have been visited and collected.  A freelist is returned,
   potentially empty.  */
SCM
scm_i_sweep_some_cards (scm_t_heap_segment *seg,
			scm_t_sweep_statistics *sweep_stats,
			int threshold)
{
  SCM cells = SCM_EOL;
  int collected = 0;
  int (*sweeper) (scm_t_cell *, SCM *, scm_t_heap_segment *)
    = (seg->first_time) ? &scm_i_init_card_freelist : &scm_i_sweep_card;

  scm_t_cell *next_free = seg->next_free_card;
  int cards_swept = 0;
  while (collected < threshold && next_free < seg->bounds[1])
    {
      collected += (*sweeper) (next_free, &cells, seg);
      next_free += SCM_GC_CARD_N_CELLS;
      cards_swept ++;
    }

  if (sweep_stats != NULL)
    {
      int swept = cards_swept 
	* ((SCM_GC_CARD_N_CELLS - SCM_GC_CARD_N_HEADER_CELLS)
	   - seg->span + 1);
      int collected_cells = collected * seg->span;
      sweep_stats->swept += swept;
      sweep_stats->collected += collected_cells;
    }
  
  if (next_free == seg->bounds[1])
    {
      seg->first_time = 0;
    }

  seg->next_free_card = next_free;
  return cells;
}



SCM
scm_i_sweep_for_freelist (scm_t_cell_type_statistics *freelist)
{
  scm_t_sweep_statistics stats = { 0 };
  SCM result = scm_i_sweep_some_segments (freelist, &stats);

  scm_i_gc_sweep_stats.collected += stats.collected;
  scm_i_gc_sweep_stats.swept += stats.swept;

  freelist->collected += stats.collected;
  freelist->swept += stats.swept; 
  return result;
}


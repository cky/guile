/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2006, 2008 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h> 
#include <stdio.h>
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/pairs.h"
#include "libguile/gc.h"
#include "libguile/private-gc.h"


/*
  Heap segment table.

  The table is sorted by the address of the data itself. This makes
  for easy lookups. This is not portable: according to ANSI C,
  pointers can only be compared within the same object (i.e. the same
  block of malloced memory.). For machines with weird architectures,
  this should be revised.
  
  (Apparently, for this reason 1.6 and earlier had macros for pointer
  comparison. )
  
  perhaps it is worthwhile to remove the 2nd level of indirection in
  the table, but this certainly makes for cleaner code.
*/
scm_t_heap_segment **scm_i_heap_segment_table;
size_t scm_i_heap_segment_table_size;
static scm_t_cell *lowest_cell;
static scm_t_cell *highest_cell; 


/*
  RETURN: index of inserted segment.
 */
int
scm_i_insert_segment (scm_t_heap_segment *seg)
{
  size_t size = (scm_i_heap_segment_table_size + 1) * sizeof (scm_t_heap_segment *);
  SCM_SYSCALL (scm_i_heap_segment_table
	      = ((scm_t_heap_segment **)
		 realloc ((char *)scm_i_heap_segment_table, size)));

  /*
    We can't alloc 4 more bytes. This is hopeless.
   */
  if (!scm_i_heap_segment_table)
    {
      fprintf (stderr, "scm_i_get_new_heap_segment: Could not grow heap segment table.\n");
      abort ();
    }

  if (!lowest_cell)
    {
      lowest_cell = seg->bounds[0];
      highest_cell = seg->bounds[1];
    }
  else
    {
      lowest_cell = SCM_MIN (lowest_cell, seg->bounds[0]);
      highest_cell = SCM_MAX (highest_cell, seg->bounds[1]);
    }


  {
    int i = 0;
    int j = 0;

    while (i < scm_i_heap_segment_table_size
	   && scm_i_heap_segment_table[i]->bounds[0] <= seg->bounds[0])
      i++;

    /*
      We insert a new entry; if that happens to be before the
      "current" segment of a freelist, we must move the freelist index
      as well.
    */
    if (scm_i_master_freelist.heap_segment_idx >= i)
      scm_i_master_freelist.heap_segment_idx ++;
    if (scm_i_master_freelist2.heap_segment_idx >= i)
      scm_i_master_freelist2.heap_segment_idx ++;

    for (j = scm_i_heap_segment_table_size; j > i; --j)
      scm_i_heap_segment_table[j] = scm_i_heap_segment_table[j - 1];

    scm_i_heap_segment_table[i] = seg;
    scm_i_heap_segment_table_size ++;

    return i;
  }
}


/*
  Determine whether the given value does actually represent a cell in
  some heap segment.  If this is the case, the number of the heap
  segment is returned.  Otherwise, -1 is returned.  Binary search is
  used to determine the heap segment that contains the cell.

  I think this function is too long to be inlined. --hwn
*/

int
scm_i_find_heap_segment_containing_object (SCM obj)
{
  if (!CELL_P (obj))
    return -1;

  scm_i_find_heap_calls ++;
  if ((scm_t_cell *) obj < lowest_cell || (scm_t_cell *) obj >= highest_cell)
    return -1;
  
  {
    scm_t_cell *ptr = SCM2PTR (obj);
    unsigned int i = 0;
    unsigned int j = scm_i_heap_segment_table_size - 1;

    if (ptr < scm_i_heap_segment_table[i]->bounds[0])
      return -1;
    else if (scm_i_heap_segment_table[j]->bounds[1] <= ptr)
      return -1;
    else
      {
	while (i < j)
	  {
	    if (ptr < scm_i_heap_segment_table[i]->bounds[1])
	      {
		break;
	      }
	    else if (scm_i_heap_segment_table[j]->bounds[0] <= ptr)
	      {
		i = j;
		break;
	      }
	    else
	      {
		unsigned long int k = (i + j) / 2;

		if (k == i)
		  return -1;
		else if (ptr <  scm_i_heap_segment_table[k]->bounds[1])
		  {
		    j = k;
		    ++i;
		    if (ptr <  scm_i_heap_segment_table[i]->bounds[0])
		      return -1;
		  }
		else if (scm_i_heap_segment_table[k]->bounds[0] <= ptr)
		  {
		    i = k;
		    --j;
		    if (scm_i_heap_segment_table[j]->bounds[1] <= ptr)
		      return -1;
		  }
	      }
	  }

	if (!SCM_DOUBLECELL_ALIGNED_P (obj) && scm_i_heap_segment_table[i]->span == 2)
	  return -1;
	else if (SCM_GC_IN_CARD_HEADERP (ptr))
	  return -1;
	else
	  return i;
      }
  }
}


int
scm_i_marked_count (void)
{
  int i = 0;
  int c = 0;
  for (; i < scm_i_heap_segment_table_size; i++)
    {
      c += scm_i_heap_segment_marked_count (scm_i_heap_segment_table[i]);
    }
  return c;
}


SCM
scm_i_sweep_some_segments (scm_t_cell_type_statistics *freelist,
			   scm_t_sweep_statistics *sweep_stats)
{
  int i = freelist->heap_segment_idx;
  SCM collected = SCM_EOL;

  if (i == -1)			/* huh? --hwn */
    i++;

  for (;
       i < scm_i_heap_segment_table_size; i++)
    {
      if (scm_i_heap_segment_table[i]->freelist != freelist)
	continue;

      collected = scm_i_sweep_some_cards (scm_i_heap_segment_table[i],
					  sweep_stats,
					  DEFAULT_SWEEP_AMOUNT);

      if (collected != SCM_EOL)       /* Don't increment i */
	break;
    }

  freelist->heap_segment_idx = i;

  return collected;
}

void
scm_i_reset_segments (void)
{
  int i = 0;
  for (; i < scm_i_heap_segment_table_size; i++)
    {
      scm_t_heap_segment *seg = scm_i_heap_segment_table[i];
      seg->next_free_card = seg->bounds[0];
    }
}




/*
  Return a hashtab with counts of live objects, with tags as keys.
 */
SCM
scm_i_all_segments_statistics (SCM tab)
{
  int i = 0;
  for (; i < scm_i_heap_segment_table_size; i++)
    {
      scm_t_heap_segment *seg = scm_i_heap_segment_table[i];
      scm_i_heap_segment_statistics (seg, tab);
    }

  return tab;
}


unsigned long*
scm_i_segment_table_info (int* size)
{
  *size = scm_i_heap_segment_table_size;  
  unsigned long *bounds = malloc (sizeof (unsigned long) * *size * 2);
  int i;
  if (!bounds)
    abort ();
  for (i = *size; i-- > 0; )
    {
      bounds[2*i] = (unsigned long)scm_i_heap_segment_table[i]->bounds[0];
      bounds[2*i+1] = (unsigned long)scm_i_heap_segment_table[i]->bounds[1];
    }
  return bounds;
}


void
scm_i_sweep_all_segments (char const *reason,
			  scm_t_sweep_statistics *sweep_stats)
{
  unsigned i= 0;
  for (i = 0; i < scm_i_heap_segment_table_size; i++)
    {
      scm_i_sweep_segment (scm_i_heap_segment_table[i], sweep_stats);
    }
}


void
scm_i_clear_mark_space (void)
{
  int i = 0;
  for (; i < scm_i_heap_segment_table_size; i++)
    {
      scm_i_clear_segment_mark_space (scm_i_heap_segment_table[i]);
    }
}

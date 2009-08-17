/*
 * private-gc.h - private declarations for garbage collection.
 * 
 * Copyright (C) 2002, 03, 04, 05, 06, 07, 08, 09 Free Software Foundation, Inc.
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

#ifndef PRIVATE_GC
#define PRIVATE_GC

#include  "_scm.h"

/* {heap tuning parameters}
 *
 * These are parameters for controlling memory allocation.  The heap
 * is the area out of which scm_cons, and object headers are allocated.
 *
 * Each heap cell is 8 bytes on a 32 bit machine and 16 bytes on a
 * 64 bit machine.  The units of the _SIZE parameters are bytes.
 * Cons pairs and object headers occupy one heap cell.
 *
 * SCM_MIN_HEAP_SEG_SIZE is minimum size of heap to accept when more heap
 * is needed.
 */


/*
 * Heap size 45000 and 40% min yield gives quick startup and no extra
 * heap allocation.  Having higher values on min yield may lead to
 * large heaps, especially if code behaviour is varying its
 * maximum consumption between different freelists.
 */

/*
  These values used to be global C variables. However, they're also
  available through the environment, and having a double interface is
  confusing. Now they're #defines --hwn.
 */

#define SCM_DEFAULT_INIT_HEAP_SIZE_1  256*1024
#define SCM_DEFAULT_MIN_YIELD_1 40
#define SCM_DEFAULT_INIT_HEAP_SIZE_2 32*1024

/*
  How many cells to collect during one sweep call. This is the pool
  size of each thread.
 */
#define DEFAULT_SWEEP_AMOUNT 512

/* The following value may seem large, but note that if we get to GC at
 * all, this means that we have a numerically intensive application
 */
#define SCM_DEFAULT_MIN_YIELD_2 40

#define SCM_DEFAULT_MAX_SEGMENT_SIZE  (20*1024*1024L)

#define SCM_MIN_HEAP_SEG_SIZE (8 * SCM_GC_SIZEOF_CARD)
#define SCM_HEAP_SEG_SIZE (16384L * sizeof (scm_t_cell))

#define SCM_DOUBLECELL_ALIGNED_P(x)  (((2 * sizeof (scm_t_cell) - 1) & SCM_UNPACK (x)) == 0)


#define SCM_GC_CARD_BVEC_SIZE_IN_LONGS \
    ((SCM_GC_CARD_N_CELLS + SCM_C_BVEC_LONG_BITS - 1) / SCM_C_BVEC_LONG_BITS)
#define SCM_GC_IN_CARD_HEADERP(x) \
  (scm_t_cell *) (x) <  SCM_GC_CELL_CARD (x) + SCM_GC_CARD_N_HEADER_CELLS

int scm_getenv_int (const char *var, int def);


typedef enum { return_on_error, abort_on_error } policy_on_error;


#define SCM_MAX(A, B) ((A) > (B) ? (A) : (B))
#define SCM_MIN(A, B) ((A) < (B) ? (A) : (B))

/* CELL_P checks a random word whether it has the right form for a
   pointer to a cell.  Use scm_i_find_heap_segment_containing_object
   to find out whether it actually points to a real cell.

   The right form for a cell pointer is this: the low three bits must
   be scm_tc3_cons, and when the scm_tc3_cons tag is stripped, the
   resulting pointer must be correctly aligned.
   scm_i_initialize_heap_segment_data guarantees that the test below
   works.
*/
#define CELL_P(x)  ((SCM_UNPACK(x) & (sizeof(scm_t_cell)-1)) == scm_tc3_cons)

/*
  gc-mark
 */

/* Non-zero while in the mark phase.  */
SCM_INTERNAL int scm_i_marking;

SCM_INTERNAL void scm_mark_all (void);

extern long int scm_i_deprecated_memory_return;
extern long int scm_i_find_heap_calls;

SCM_INTERNAL char const *scm_i_tag_name (scm_t_bits tag); /* MOVEME */


/*
  global init funcs.
 */
void scm_gc_init_malloc (void);
void scm_gc_init_freelist (void);
void scm_gc_init_segments (void);
void scm_gc_init_mark (void);


#endif

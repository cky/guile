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

#ifndef SCM_PRIVATE_GC
#define SCM_PRIVATE_GC

#include  "_scm.h"

/* {heap tuning parameters}
 *
 * These are parameters for controlling memory allocation.  The heap
 * is the area out of which scm_cons, and object headers are allocated.
 *
 * Each heap cell is 8 bytes on a 32 bit machine and 16 bytes on a
 * 64 bit machine.  The units of the _SIZE parameters are bytes.
 * Cons pairs and object headers occupy one heap cell.
 */


#define SCM_DEFAULT_INIT_HEAP_SIZE_2 32*1024

#define SCM_DOUBLECELL_ALIGNED_P(x)  (((2 * sizeof (scm_t_cell) - 1) & SCM_UNPACK (x)) == 0)


SCM_INTERNAL int scm_getenv_int (const char *var, int def);


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

SCM_INTERNAL char const *scm_i_tag_name (scm_t_bits tag); /* MOVEME */

#endif

/* classes: h_files */

#ifndef GCH
#define GCH
/*	Copyright (C) 1995, 1996, 1998, 1999 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"


#define SCM_FREEP(x) (SCM_NIMP(x) && SCM_CARBITS (x)==scm_tc_free_cell)
#define SCM_NFREEP(x) (!SCM_FREEP(x))

/* 1. This shouldn't be used on immediates.
   2. It thinks that subrs are always unmarked (harmless). */
#define SCM_MARKEDP(x) ((SCM_CARBITS (x) & 5) == 5 \
			? SCM_GC8MARKP(x) \
			: SCM_GCMARKP(x))
#define SCM_NMARKEDP(x) (!SCM_MARKEDP(x))

extern struct scm_heap_seg_data *scm_heap_table;
extern int scm_n_heap_segs;
extern int scm_take_stdin;
extern int scm_block_gc;
extern int scm_gc_heap_lock;



extern unsigned long scm_heap_size;
extern SCM_CELLPTR scm_heap_org;
extern SCM scm_freelist;
extern unsigned long scm_gc_cells_collected;
extern unsigned long scm_gc_malloc_collected;
extern unsigned long scm_gc_ports_collected;
extern unsigned long scm_cells_allocated;
extern long scm_mallocated;
extern unsigned long scm_mtrigger;

#ifdef GUILE_DEBUG_FREELIST
extern SCM scm_map_free_list (void);
extern SCM scm_debug_newcell (void);
extern SCM scm_gc_set_debug_check_freelist_x (SCM flag);
#endif



extern SCM scm_object_address (SCM obj);
extern SCM scm_unhash_name (SCM name);
extern SCM scm_gc_stats (void);
extern void scm_gc_start (const char *what);
extern void scm_gc_end (void);
extern SCM scm_gc (void);
extern void scm_gc_for_alloc (int ncells, SCM * freelistp);
extern SCM scm_gc_for_newcell (void);
extern void scm_igc (const char *what);
extern void scm_gc_mark (SCM p);
extern void scm_mark_locations (SCM_STACKITEM x[], scm_sizet n);
extern int scm_cellp (SCM value);
extern void scm_gc_sweep (void);
extern void * scm_must_malloc (scm_sizet len, const char *what);
extern void * scm_must_realloc (void *where,
				scm_sizet olen, scm_sizet len,
				const char *what);
extern void scm_done_malloc (long size);
extern void scm_must_free (void *obj);
extern void scm_remember (SCM * ptr);
extern SCM scm_return_first (SCM elt, ...);
extern int scm_return_first_int (int x, ...);
extern SCM scm_permanent_object (SCM obj);
extern SCM scm_protect_object (SCM obj);
extern SCM scm_unprotect_object (SCM obj);
extern int scm_init_storage (scm_sizet init_heap_size);
extern void scm_init_gc (void);
#endif  /* GCH */

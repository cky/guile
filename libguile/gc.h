/* classes: h_files */

#ifndef GCH
#define GCH
/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include <libguile/__scm.h>


#define SCM_FREEP(x) (SCM_CAR(x)==scm_tc_free_cell)
#define SCM_NFREEP(x) (!SCM_FREEP(x))

extern struct scm_heap_seg_data *scm_heap_table;
extern int scm_n_heap_segs;
extern int scm_take_stdin;
extern int scm_block_gc;
extern int scm_gc_heap_lock;



extern long scm_heap_size;
extern SCM_CELLPTR scm_heap_org;
extern SCM scm_freelist;
extern unsigned long scm_gc_cells_collected;
extern unsigned long scm_gc_malloc_collected;
extern unsigned long scm_gc_ports_collected;
extern unsigned long scm_cells_allocated;
extern unsigned long scm_mallocated;
extern long scm_mtrigger;



extern SCM scm_gc_stats PROTO ((void));
extern void scm_gc_start PROTO ((char *what));
extern void scm_gc_end PROTO ((void));
extern SCM scm_gc PROTO ((void));
extern void scm_gc_for_alloc PROTO ((int ncells, SCM * freelistp));
extern SCM scm_gc_for_newcell PROTO ((void));
extern void scm_igc PROTO ((char *what));
extern void scm_gc_mark PROTO ((SCM p));
extern void scm_mark_locations PROTO ((SCM_STACKITEM x[], scm_sizet n));
extern void scm_gc_sweep PROTO ((void));
extern char * scm_must_malloc PROTO ((long len, char *what));
extern char * scm_must_realloc PROTO ((char *where, long olen, long len,
				       char *what));
extern void scm_must_free PROTO ((char *obj));
extern void scm_remember PROTO ((SCM * ptr));
extern SCM scm_return_first PROTO ((SCM elt, ...));
extern SCM scm_permanent_object PROTO ((SCM obj));
extern SCM scm_protect_object PROTO ((SCM obj));
extern SCM scm_unprotect_object PROTO ((SCM obj));
extern int scm_init_storage PROTO ((long init_heap_size));
extern void scm_init_gc PROTO ((void));

#include "marksweep.h"
#endif  /* GCH */

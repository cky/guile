/* Copyright (C) 1995, 96, 97, 98, 99, 2000 Free Software Foundation, Inc.
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

/* #define DEBUGINFO */


#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/stime.h"
#include "libguile/stackchk.h"
#include "libguile/struct.h"
#include "libguile/smob.h"
#include "libguile/unif.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/weaks.h"
#include "libguile/hashtab.h"

#include "libguile/validate.h"
#include "libguile/gc.h"

#ifdef GUILE_DEBUG_MALLOC
#include "libguile/debug-malloc.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __STDC__
#include <stdarg.h>
#define var_start(x, y) va_start(x, y)
#else
#include <varargs.h>
#define var_start(x, y) va_start(x)
#endif



unsigned int scm_gc_running_p = 0;



#if (SCM_DEBUG_CELL_ACCESSES == 1)

unsigned int scm_debug_cell_accesses_p = 0;


/* Assert that the given object is a valid reference to a valid cell.  This
 * test involves to determine whether the object is a cell pointer, whether
 * this pointer actually points into a heap segment and whether the cell
 * pointed to is not a free cell.
 */
void
scm_assert_cell_valid (SCM cell)
{
  if (scm_debug_cell_accesses_p)
    {
      scm_debug_cell_accesses_p = 0;  /* disable to avoid recursion */

      if (!scm_cellp (cell))
	{
	  fprintf (stderr, "scm_assert_cell_valid: Not a cell object: %lx\n", SCM_UNPACK (cell));
	  abort ();
	}
      else if (!scm_gc_running_p)
	{
	  /* Dirk::FIXME:: During garbage collection there occur references to
	     free cells.  This is allright during conservative marking, but
	     should not happen otherwise (I think).  The case of free cells
	     accessed during conservative marking is handled in function
	     scm_mark_locations.  However, there still occur accesses to free
	     cells during gc.  I don't understand why this happens.  If it is
	     a bug and gets fixed, the following test should also work while
	     gc is running.
	   */
	  if (SCM_FREE_CELL_P (cell))
	    {
	      fprintf (stderr, "scm_assert_cell_valid: Accessing free cell: %lx\n", SCM_UNPACK (cell));
	      abort ();
	    }
	}
      scm_debug_cell_accesses_p = 1;  /* re-enable */
    }
}


SCM_DEFINE (scm_set_debug_cell_accesses_x, "set-debug-cell-accesses!", 1, 0, 0,
	    (SCM flag),
	    "If FLAG is #f, cell access checking is disabled.\n"
	    "If FLAG is #t, cell access checking is enabled.\n"
	    "This procedure only exists because the compile-time flag\n"
	    "SCM_DEBUG_CELL_ACCESSES was set to 1.\n")
#define FUNC_NAME s_scm_set_debug_cell_accesses_x
{
  if (SCM_FALSEP (flag)) {
    scm_debug_cell_accesses_p = 0;
  } else if (SCM_EQ_P (flag, SCM_BOOL_T)) {
    scm_debug_cell_accesses_p = 1;
  } else {
    SCM_WRONG_TYPE_ARG (1, flag);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif  /* SCM_DEBUG_CELL_ACCESSES == 1 */



/* {heap tuning parameters}
 *
 * These are parameters for controlling memory allocation.  The heap
 * is the area out of which scm_cons, and object headers are allocated.
 *
 * Each heap cell is 8 bytes on a 32 bit machine and 16 bytes on a
 * 64 bit machine.  The units of the _SIZE parameters are bytes.
 * Cons pairs and object headers occupy one heap cell.
 *
 * SCM_INIT_HEAP_SIZE is the initial size of heap.  If this much heap is
 * allocated initially the heap will grow by half its current size
 * each subsequent time more heap is needed.
 *
 * If SCM_INIT_HEAP_SIZE heap cannot be allocated initially, SCM_HEAP_SEG_SIZE
 * will be used, and the heap will grow by SCM_HEAP_SEG_SIZE when more
 * heap is needed.  SCM_HEAP_SEG_SIZE must fit into type scm_sizet.  This code
 * is in scm_init_storage() and alloc_some_heap() in sys.c
 *
 * If SCM_INIT_HEAP_SIZE can be allocated initially, the heap will grow by
 * SCM_EXPHEAP(scm_heap_size) when more heap is needed.
 *
 * SCM_MIN_HEAP_SEG_SIZE is minimum size of heap to accept when more heap
 * is needed.
 *
 * INIT_MALLOC_LIMIT is the initial amount of malloc usage which will
 * trigger a GC.
 *
 * SCM_MTRIGGER_HYSTERESIS is the amount of malloc storage that must be
 * reclaimed by a GC triggered by must_malloc. If less than this is
 * reclaimed, the trigger threshold is raised. [I don't know what a
 * good value is. I arbitrarily chose 1/10 of the INIT_MALLOC_LIMIT to
 * work around a oscillation that caused almost constant GC.]
 */

/*
 * Heap size 45000 and 40% min yield gives quick startup and no extra
 * heap allocation.  Having higher values on min yield may lead to
 * large heaps, especially if code behaviour is varying its
 * maximum consumption between different freelists.
 */

#define SCM_DATA_CELLS2CARDS(n) (((n) + SCM_GC_CARD_N_DATA_CELLS - 1) / SCM_GC_CARD_N_DATA_CELLS)
#define SCM_CARDS_PER_CLUSTER SCM_DATA_CELLS2CARDS (2000L)
#define SCM_CLUSTER_SIZE_1 (SCM_CARDS_PER_CLUSTER * SCM_GC_CARD_N_DATA_CELLS)
int scm_default_init_heap_size_1 = (((SCM_DATA_CELLS2CARDS (45000L) + SCM_CARDS_PER_CLUSTER - 1)
                                     / SCM_CARDS_PER_CLUSTER) * SCM_GC_CARD_SIZE);
int scm_default_min_yield_1 = 40;

#define SCM_CLUSTER_SIZE_2 (SCM_CARDS_PER_CLUSTER * (SCM_GC_CARD_N_DATA_CELLS / 2))
int scm_default_init_heap_size_2 = (((SCM_DATA_CELLS2CARDS (2500L * 2) + SCM_CARDS_PER_CLUSTER - 1)
                                     / SCM_CARDS_PER_CLUSTER) * SCM_GC_CARD_SIZE);
/* The following value may seem large, but note that if we get to GC at
 * all, this means that we have a numerically intensive application
 */
int scm_default_min_yield_2 = 40;

int scm_default_max_segment_size = 2097000L;/* a little less (adm) than 2 Mb */

#define SCM_MIN_HEAP_SEG_SIZE (8 * SCM_GC_CARD_SIZE)
#ifdef _QC
# define SCM_HEAP_SEG_SIZE 32768L
#else
# ifdef sequent
#  define SCM_HEAP_SEG_SIZE (7000L * sizeof (scm_cell))
# else
#  define SCM_HEAP_SEG_SIZE (16384L * sizeof (scm_cell))
# endif
#endif
/* Make heap grow with factor 1.5 */
#define SCM_EXPHEAP(scm_heap_size) (scm_heap_size / 2)
#define SCM_INIT_MALLOC_LIMIT 100000
#define SCM_MTRIGGER_HYSTERESIS (SCM_INIT_MALLOC_LIMIT/10)

/* CELL_UP and CELL_DN are used by scm_init_heap_seg to find (scm_cell * span)
   aligned inner bounds for allocated storage */

#ifdef PROT386
/*in 386 protected mode we must only adjust the offset */
# define CELL_UP(p, span) MK_FP(FP_SEG(p), ~(8*(span)-1)&(FP_OFF(p)+8*(span)-1))
# define CELL_DN(p, span) MK_FP(FP_SEG(p), ~(8*(span)-1)&FP_OFF(p))
#else
# ifdef _UNICOS
#  define CELL_UP(p, span) (SCM_CELLPTR)(~(span) & ((long)(p)+(span)))
#  define CELL_DN(p, span) (SCM_CELLPTR)(~(span) & (long)(p))
# else
#  define CELL_UP(p, span) (SCM_CELLPTR)(~(sizeof(scm_cell)*(span)-1L) & ((long)(p)+sizeof(scm_cell)*(span)-1L))
#  define CELL_DN(p, span) (SCM_CELLPTR)(~(sizeof(scm_cell)*(span)-1L) & (long)(p))
# endif				/* UNICOS */
#endif				/* PROT386 */

#define ALIGNMENT_SLACK(freelist) (SCM_GC_CARD_SIZE - 1)
#define CLUSTER_SIZE_IN_BYTES(freelist) \
    (((freelist)->cluster_size / (SCM_GC_CARD_N_DATA_CELLS / (freelist)->span)) * SCM_GC_CARD_SIZE)


/* scm_freelists
 */

typedef struct scm_freelist_t {
  /* collected cells */
  SCM cells;
  /* number of cells left to collect before cluster is full */
  unsigned int left_to_collect;
  /* number of clusters which have been allocated */
  unsigned int clusters_allocated;
  /* a list of freelists, each of size cluster_size,
   * except the last one which may be shorter
   */
  SCM clusters;
  SCM *clustertail;
  /* this is the number of objects in each cluster, including the spine cell */
  int cluster_size;
  /* indicates that we should grow heap instead of GC:ing
   */
  int grow_heap_p;
  /* minimum yield on this list in order not to grow the heap
   */
  long min_yield;
  /* defines min_yield as percent of total heap size
   */
  int min_yield_fraction;
  /* number of cells per object on this list */
  int span;
  /* number of collected cells during last GC */
  long collected;
  /* number of collected cells during penultimate GC */
  long collected_1;
  /* total number of cells in heap segments
   * belonging to this list.
   */
  long heap_size;
} scm_freelist_t;

SCM scm_freelist = SCM_EOL;
scm_freelist_t scm_master_freelist = {
  SCM_EOL, 0, 0, SCM_EOL, 0, SCM_CLUSTER_SIZE_1, 0, 0, 0, 1, 0, 0
};
SCM scm_freelist2 = SCM_EOL;
scm_freelist_t scm_master_freelist2 = {
  SCM_EOL, 0, 0, SCM_EOL, 0, SCM_CLUSTER_SIZE_2, 0, 0, 0, 2, 0, 0
};

/* scm_mtrigger
 * is the number of bytes of must_malloc allocation needed to trigger gc.
 */
unsigned long scm_mtrigger;

/* scm_gc_heap_lock
 * If set, don't expand the heap.  Set only during gc, during which no allocation
 * is supposed to take place anyway.
 */
int scm_gc_heap_lock = 0;

/* GC Blocking
 * Don't pause for collection if this is set -- just
 * expand the heap.
 */
int scm_block_gc = 1;

/* During collection, this accumulates objects holding
 * weak references.
 */
SCM scm_weak_vectors;

/* During collection, this accumulates structures which are to be freed.
 */
SCM scm_structs_to_free;

/* GC Statistics Keeping
 */
unsigned long scm_cells_allocated = 0;
long scm_mallocated = 0;
unsigned long scm_gc_cells_collected;
unsigned long scm_gc_yield;
static unsigned long scm_gc_yield_1 = 0; /* previous GC yield */
unsigned long scm_gc_malloc_collected;
unsigned long scm_gc_ports_collected;
unsigned long scm_gc_time_taken = 0;
static unsigned long t_before_gc;
static unsigned long t_before_sweep;
unsigned long scm_gc_mark_time_taken = 0;
unsigned long scm_gc_sweep_time_taken = 0;
unsigned long scm_gc_times = 0;
unsigned long scm_gc_cells_swept = 0;
double scm_gc_cells_marked_acc = 0.;
double scm_gc_cells_swept_acc = 0.;

SCM_SYMBOL (sym_cells_allocated, "cells-allocated");
SCM_SYMBOL (sym_heap_size, "cell-heap-size");
SCM_SYMBOL (sym_mallocated, "bytes-malloced");
SCM_SYMBOL (sym_mtrigger, "gc-malloc-threshold");
SCM_SYMBOL (sym_heap_segments, "cell-heap-segments");
SCM_SYMBOL (sym_gc_time_taken, "gc-time-taken");
SCM_SYMBOL (sym_gc_mark_time_taken, "gc-mark-time-taken");
SCM_SYMBOL (sym_gc_sweep_time_taken, "gc-sweep-time-taken");
SCM_SYMBOL (sym_times, "gc-times");
SCM_SYMBOL (sym_cells_marked, "cells-marked");
SCM_SYMBOL (sym_cells_swept, "cells-swept");

typedef struct scm_heap_seg_data_t
{
  /* lower and upper bounds of the segment */
  SCM_CELLPTR bounds[2];

  /* address of the head-of-freelist pointer for this segment's cells.
     All segments usually point to the same one, scm_freelist.  */
  scm_freelist_t *freelist;

  /* number of cells per object in this segment */
  int span;
} scm_heap_seg_data_t;



static scm_sizet init_heap_seg (SCM_CELLPTR, scm_sizet, scm_freelist_t *);

typedef enum { return_on_error, abort_on_error } policy_on_error;
static void alloc_some_heap (scm_freelist_t *, policy_on_error);


#define SCM_HEAP_SIZE \
  (scm_master_freelist.heap_size + scm_master_freelist2.heap_size)
#define SCM_MAX(A, B) ((A) > (B) ? (A) : (B))

#define BVEC_GROW_SIZE  256
#define BVEC_GROW_SIZE_IN_LIMBS (SCM_GC_CARD_BVEC_SIZE_IN_LIMBS * BVEC_GROW_SIZE)
#define BVEC_GROW_SIZE_IN_BYTES (BVEC_GROW_SIZE_IN_LIMBS * sizeof (scm_c_bvec_limb_t))

/* mark space allocation */

typedef struct scm_mark_space_t
{
  scm_c_bvec_limb_t *bvec_space;
  struct scm_mark_space_t *next;
} scm_mark_space_t;

static scm_mark_space_t *current_mark_space;
static scm_mark_space_t **mark_space_ptr;
static int current_mark_space_offset;
static scm_mark_space_t *mark_space_head;

static scm_c_bvec_limb_t *
get_bvec ()
{
  scm_c_bvec_limb_t *res;

  if (!current_mark_space)
    {
      SCM_SYSCALL (current_mark_space = (scm_mark_space_t *) malloc (sizeof (scm_mark_space_t)));
      if (!current_mark_space)
        scm_wta (SCM_UNDEFINED, "could not grow", "heap");

      current_mark_space->bvec_space = NULL;
      current_mark_space->next = NULL;

      *mark_space_ptr = current_mark_space;
      mark_space_ptr = &(current_mark_space->next);

      return get_bvec ();
    }

  if (!(current_mark_space->bvec_space))
    {
      SCM_SYSCALL (current_mark_space->bvec_space =
                   (scm_c_bvec_limb_t *) calloc (BVEC_GROW_SIZE_IN_BYTES, 1));
      if (!(current_mark_space->bvec_space))
        scm_wta (SCM_UNDEFINED, "could not grow", "heap");

      current_mark_space_offset = 0;

      return get_bvec ();
    }

  if (current_mark_space_offset == BVEC_GROW_SIZE_IN_LIMBS)
    {
      current_mark_space = NULL;

      return get_bvec ();
    }

  res = current_mark_space->bvec_space + current_mark_space_offset;
  current_mark_space_offset += SCM_GC_CARD_BVEC_SIZE_IN_LIMBS;

  return res;
}

static void
clear_mark_space ()
{
  scm_mark_space_t *ms;

  for (ms = mark_space_head; ms; ms = ms->next)
    memset (ms->bvec_space, 0, BVEC_GROW_SIZE_IN_BYTES);
}



/* Debugging functions.  */

#if defined (GUILE_DEBUG) || defined (GUILE_DEBUG_FREELIST)

/* Return the number of the heap segment containing CELL.  */
static int
which_seg (SCM cell)
{
  int i;

  for (i = 0; i < scm_n_heap_segs; i++)
    if (SCM_PTR_LE (scm_heap_table[i].bounds[0], SCM2PTR (cell))
	&& SCM_PTR_GT (scm_heap_table[i].bounds[1], SCM2PTR (cell)))
      return i;
  fprintf (stderr, "which_seg: can't find segment containing cell %lx\n",
	   SCM_UNPACK (cell));
  abort ();
}


static void
map_free_list (scm_freelist_t *master, SCM freelist)
{
  int last_seg = -1, count = 0;
  SCM f;

  for (f = freelist; !SCM_NULLP (f); f = SCM_FREE_CELL_CDR (f))
    {
      int this_seg = which_seg (f);

      if (this_seg != last_seg)
	{
	  if (last_seg != -1)
	    fprintf (stderr, "  %5d %d-cells in segment %d\n",
		     count, master->span, last_seg);
	  last_seg = this_seg;
	  count = 0;
	}
      count++;
    }
  if (last_seg != -1)
    fprintf (stderr, "  %5d %d-cells in segment %d\n",
	     count, master->span, last_seg);
}

SCM_DEFINE (scm_map_free_list, "map-free-list", 0, 0, 0,
            (),
            "Print debugging information about the free-list.\n"
            "`map-free-list' is only included in --enable-guile-debug builds of Guile.")
#define FUNC_NAME s_scm_map_free_list
{
  int i;
  fprintf (stderr, "%d segments total (%d:%d",
	   scm_n_heap_segs,
	   scm_heap_table[0].span,
	   scm_heap_table[0].bounds[1] - scm_heap_table[0].bounds[0]);
  for (i = 1; i < scm_n_heap_segs; i++)
    fprintf (stderr, ", %d:%d",
	     scm_heap_table[i].span,
	     scm_heap_table[i].bounds[1] - scm_heap_table[i].bounds[0]);
  fprintf (stderr, ")\n");
  map_free_list (&scm_master_freelist, scm_freelist);
  map_free_list (&scm_master_freelist2, scm_freelist2);
  fflush (stderr);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static int last_cluster;
static int last_size;

static int
free_list_length (char *title, int i, SCM freelist)
{
  SCM ls;
  int n = 0;
  for (ls = freelist; !SCM_NULLP (ls); ls = SCM_FREE_CELL_CDR (ls))
    if (SCM_FREE_CELL_P (ls))
      ++n;
    else
      {
	fprintf (stderr, "bad cell in %s at position %d\n", title, n);
	abort ();
      }
  if (n != last_size)
    {
      if (i > 0)
	{
	  if (last_cluster == i - 1)
	    fprintf (stderr, "\t%d\n", last_size);
	  else
	    fprintf (stderr, "-%d\t%d\n", i - 1, last_size);
	}
      if (i >= 0)
	fprintf (stderr, "%s %d", title, i);
      else
	fprintf (stderr, "%s\t%d\n", title, n);
      last_cluster = i;
      last_size = n;
    }
  return n;
}

static void
free_list_lengths (char *title, scm_freelist_t *master, SCM freelist)
{
  SCM clusters;
  int i = 0, len, n = 0;
  fprintf (stderr, "%s\n\n", title);
  n += free_list_length ("free list", -1, freelist);
  for (clusters = master->clusters;
       SCM_NNULLP (clusters);
       clusters = SCM_CDR (clusters))
    {
      len = free_list_length ("cluster", i++, SCM_CAR (clusters));
      n += len;
    }
  if (last_cluster == i - 1)
    fprintf (stderr, "\t%d\n", last_size);
  else
    fprintf (stderr, "-%d\t%d\n", i - 1, last_size);
  fprintf (stderr, "\ntotal %d objects\n\n", n);
}

SCM_DEFINE (scm_free_list_length, "free-list-length", 0, 0, 0,
            (),
            "Print debugging information about the free-list.\n"
            "`free-list-length' is only included in --enable-guile-debug builds of Guile.")
#define FUNC_NAME s_scm_free_list_length
{
  free_list_lengths ("1-cells", &scm_master_freelist, scm_freelist);
  free_list_lengths ("2-cells", &scm_master_freelist2, scm_freelist2);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif

#ifdef GUILE_DEBUG_FREELIST

/* Number of calls to SCM_NEWCELL since startup.  */
static unsigned long scm_newcell_count;
static unsigned long scm_newcell2_count;

/* Search freelist for anything that isn't marked as a free cell.
   Abort if we find something.  */
static void
scm_check_freelist (SCM freelist)
{
  SCM f;
  int i = 0;

  for (f = freelist; !SCM_NULLP (f); f = SCM_FREE_CELL_CDR (f), i++)
    if (!SCM_FREE_CELL_P (f))
      {
	fprintf (stderr, "Bad cell in freelist on newcell %lu: %d'th elt\n",
		 scm_newcell_count, i);
	abort ();
      }
}

SCM_DEFINE (scm_gc_set_debug_check_freelist_x, "gc-set-debug-check-freelist!", 1, 0, 0,
            (SCM flag),
            "If FLAG is #t, check the freelist for consistency on each cell allocation.\n"
            "This procedure only exists because the GUILE_DEBUG_FREELIST \n"
            "compile-time flag was selected.\n")
#define FUNC_NAME s_scm_gc_set_debug_check_freelist_x
{
  /* [cmm] I did a double-take when I read this code the first time.
     well, FWIW. */
  SCM_VALIDATE_BOOL_COPY (1, flag, scm_debug_check_freelist);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM
scm_debug_newcell (void)
{
  SCM new;

  scm_newcell_count++;
  if (scm_debug_check_freelist)
    {
      scm_check_freelist (scm_freelist);
      scm_gc();
    }

  /* The rest of this is supposed to be identical to the SCM_NEWCELL
     macro.  */
  if (SCM_NULLP (scm_freelist))
    new = scm_gc_for_newcell (&scm_master_freelist, &scm_freelist);
  else
    {
      new = scm_freelist;
      scm_freelist = SCM_FREE_CELL_CDR (scm_freelist);
    }

  return new;
}

SCM
scm_debug_newcell2 (void)
{
  SCM new;

  scm_newcell2_count++;
  if (scm_debug_check_freelist)
    {
      scm_check_freelist (scm_freelist2);
      scm_gc ();
    }

  /* The rest of this is supposed to be identical to the SCM_NEWCELL
     macro.  */
  if (SCM_NULLP (scm_freelist2))
    new = scm_gc_for_newcell (&scm_master_freelist2, &scm_freelist2);
  else
    {
      new = scm_freelist2;
      scm_freelist2 = SCM_FREE_CELL_CDR (scm_freelist2);
    }

  return new;
}

#endif /* GUILE_DEBUG_FREELIST */



static unsigned long
master_cells_allocated (scm_freelist_t *master)
{
  /* the '- 1' below is to ignore the cluster spine cells. */
  int objects = master->clusters_allocated * (master->cluster_size - 1);
  if (SCM_NULLP (master->clusters))
    objects -= master->left_to_collect;
  return master->span * objects;
}

static unsigned long
freelist_length (SCM freelist)
{
  int n;
  for (n = 0; !SCM_NULLP (freelist); freelist = SCM_FREE_CELL_CDR (freelist))
    ++n;
  return n;
}

static unsigned long
compute_cells_allocated ()
{
  return (scm_cells_allocated
	  + master_cells_allocated (&scm_master_freelist)
	  + master_cells_allocated (&scm_master_freelist2)
	  - scm_master_freelist.span * freelist_length (scm_freelist)
	  - scm_master_freelist2.span * freelist_length (scm_freelist2));
}

/* {Scheme Interface to GC}
 */

SCM_DEFINE (scm_gc_stats, "gc-stats", 0, 0, 0,
            (),
	    "Returns an association list of statistics about Guile's current use of storage.  ")
#define FUNC_NAME s_scm_gc_stats
{
  int i;
  int n;
  SCM heap_segs;
  long int local_scm_mtrigger;
  long int local_scm_mallocated;
  long int local_scm_heap_size;
  long int local_scm_cells_allocated;
  long int local_scm_gc_time_taken;
  long int local_scm_gc_times;
  long int local_scm_gc_mark_time_taken;
  long int local_scm_gc_sweep_time_taken;
  double local_scm_gc_cells_swept;
  double local_scm_gc_cells_marked;
  SCM answer;

  SCM_DEFER_INTS;

  ++scm_block_gc;

 retry:
  heap_segs = SCM_EOL;
  n = scm_n_heap_segs;
  for (i = scm_n_heap_segs; i--; )
    heap_segs = scm_cons (scm_cons (scm_ulong2num ((unsigned long)scm_heap_table[i].bounds[1]),
				    scm_ulong2num ((unsigned long)scm_heap_table[i].bounds[0])),
			  heap_segs);
  if (scm_n_heap_segs != n)
    goto retry;

  --scm_block_gc;

  /* Below, we cons to produce the resulting list.  We want a snapshot of
   * the heap situation before consing.
   */
  local_scm_mtrigger = scm_mtrigger;
  local_scm_mallocated = scm_mallocated;
  local_scm_heap_size = SCM_HEAP_SIZE;
  local_scm_cells_allocated = compute_cells_allocated ();
  local_scm_gc_time_taken = scm_gc_time_taken;
  local_scm_gc_mark_time_taken = scm_gc_mark_time_taken;
  local_scm_gc_sweep_time_taken = scm_gc_sweep_time_taken;
  local_scm_gc_times = scm_gc_times;
  local_scm_gc_cells_swept = scm_gc_cells_swept_acc;
  local_scm_gc_cells_marked = scm_gc_cells_marked_acc;

  answer = scm_listify (scm_cons (sym_gc_time_taken, scm_ulong2num (local_scm_gc_time_taken)),
			scm_cons (sym_cells_allocated, scm_ulong2num (local_scm_cells_allocated)),
			scm_cons (sym_heap_size, scm_ulong2num (local_scm_heap_size)),
			scm_cons (sym_mallocated, scm_ulong2num (local_scm_mallocated)),
			scm_cons (sym_mtrigger, scm_ulong2num (local_scm_mtrigger)),
			scm_cons (sym_times, scm_ulong2num (local_scm_gc_times)),
                        scm_cons (sym_gc_mark_time_taken, scm_ulong2num (local_scm_gc_mark_time_taken)),
                        scm_cons (sym_gc_sweep_time_taken, scm_ulong2num (local_scm_gc_sweep_time_taken)),
                        scm_cons (sym_cells_marked, scm_dbl2big (local_scm_gc_cells_marked)),
                        scm_cons (sym_cells_swept, scm_dbl2big (local_scm_gc_cells_swept)),
			scm_cons (sym_heap_segments, heap_segs),
			SCM_UNDEFINED);
  SCM_ALLOW_INTS;
  return answer;
}
#undef FUNC_NAME


static void
gc_start_stats (const char *what)
{
  t_before_gc = scm_c_get_internal_run_time ();
  scm_gc_cells_swept = 0;
  scm_gc_cells_collected = 0;
  scm_gc_yield_1 = scm_gc_yield;
  scm_gc_yield = (scm_cells_allocated
		  + master_cells_allocated (&scm_master_freelist)
		  + master_cells_allocated (&scm_master_freelist2));
  scm_gc_malloc_collected = 0;
  scm_gc_ports_collected = 0;
}


static void
gc_end_stats ()
{
  unsigned long t = scm_c_get_internal_run_time ();
  scm_gc_time_taken += (t - t_before_gc);
  scm_gc_sweep_time_taken += (t - t_before_sweep);
  ++scm_gc_times;

  scm_gc_cells_marked_acc += scm_gc_cells_swept - scm_gc_cells_collected;
  scm_gc_cells_swept_acc += scm_gc_cells_swept;
}


SCM_DEFINE (scm_object_address, "object-address", 1, 0, 0,
            (SCM obj),
	    "Return an integer that for the lifetime of @var{obj} is uniquely\n"
	    "returned by this function for @var{obj}")
#define FUNC_NAME s_scm_object_address
{
  return scm_ulong2num ((unsigned long) SCM_UNPACK (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_gc, "gc", 0, 0, 0,
           (),
	    "Scans all of SCM objects and reclaims for further use those that are\n"
	    "no longer accessible.")
#define FUNC_NAME s_scm_gc
{
  SCM_DEFER_INTS;
  scm_igc ("call");
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* {C Interface For When GC is Triggered}
 */

static void
adjust_min_yield (scm_freelist_t *freelist)
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
      int delta = ((SCM_HEAP_SIZE * freelist->min_yield_fraction / 100)
		   - (long) SCM_MAX (scm_gc_yield_1, scm_gc_yield));
#ifdef DEBUGINFO
      fprintf (stderr, " after GC = %d, delta = %d\n",
	       scm_cells_allocated,
	       delta);
#endif
      if (delta > 0)
	freelist->min_yield += delta;
    }
}


/* When we get POSIX threads support, the master will be global and
 * common while the freelist will be individual for each thread.
 */

SCM
scm_gc_for_newcell (scm_freelist_t *master, SCM *freelist)
{
  SCM cell;
  ++scm_ints_disabled;
  do
    {
      if (SCM_NULLP (master->clusters))
	{
	  if (master->grow_heap_p || scm_block_gc)
	    {
	      /* In order to reduce gc frequency, try to allocate a new heap
	       * segment first, even if gc might find some free cells.  If we
	       * can't obtain a new heap segment, we will try gc later.
	       */
	      master->grow_heap_p = 0;
	      alloc_some_heap (master, return_on_error);
	    }
	  if (SCM_NULLP (master->clusters))
	    {
	      /* The heap was not grown, either because it wasn't scheduled to
	       * grow, or because there was not enough memory available.  In
	       * both cases we have to try gc to get some free cells.
	       */
#ifdef DEBUGINFO
	      fprintf (stderr, "allocated = %d, ",
		       scm_cells_allocated
		       + master_cells_allocated (&scm_master_freelist)
		       + master_cells_allocated (&scm_master_freelist2));
#endif
	      scm_igc ("cells");
	      adjust_min_yield (master);
	      if (SCM_NULLP (master->clusters))
		{
		  /* gc could not free any cells.  Now, we _must_ allocate a
		   * new heap segment, because there is no other possibility
		   * to provide a new cell for the caller.
		   */
		  alloc_some_heap (master, abort_on_error);
		}
	    }
	}
      cell = SCM_CAR (master->clusters);
      master->clusters = SCM_CDR (master->clusters);
      ++master->clusters_allocated;
    }
  while (SCM_NULLP (cell));

#ifdef GUILE_DEBUG_FREELIST
  scm_check_freelist (cell);
#endif

  --scm_ints_disabled;
  *freelist = SCM_FREE_CELL_CDR (cell);
  return cell;
}


#if 0
/* This is a support routine which can be used to reserve a cluster
 * for some special use, such as debugging.  It won't be useful until
 * free cells are preserved between garbage collections.
 */

void
scm_alloc_cluster (scm_freelist_t *master)
{
  SCM freelist, cell;
  cell = scm_gc_for_newcell (master, &freelist);
  SCM_SETCDR (cell, freelist);
  return cell;
}
#endif


scm_c_hook_t scm_before_gc_c_hook;
scm_c_hook_t scm_before_mark_c_hook;
scm_c_hook_t scm_before_sweep_c_hook;
scm_c_hook_t scm_after_sweep_c_hook;
scm_c_hook_t scm_after_gc_c_hook;


void
scm_igc (const char *what)
{
  int j;

  ++scm_gc_running_p;
  scm_c_hook_run (&scm_before_gc_c_hook, 0);
#ifdef DEBUGINFO
  fprintf (stderr,
	   SCM_NULLP (scm_freelist)
	   ? "*"
	   : (SCM_NULLP (scm_freelist2) ? "o" : "m"));
#endif
#ifdef USE_THREADS
  /* During the critical section, only the current thread may run. */
  SCM_THREAD_CRITICAL_SECTION_START;
#endif

  /* fprintf (stderr, "gc: %s\n", what); */

  if (!scm_stack_base || scm_block_gc)
    {
      --scm_gc_running_p;
      return;
    }

  gc_start_stats (what);

  if (scm_mallocated < 0)
    /* The byte count of allocated objects has underflowed.  This is
       probably because you forgot to report the sizes of objects you
       have allocated, by calling scm_done_malloc or some such.  When
       the GC freed them, it subtracted their size from
       scm_mallocated, which underflowed.  */
    abort ();

  if (scm_gc_heap_lock)
    /* We've invoked the collector while a GC is already in progress.
       That should never happen.  */
    abort ();

  ++scm_gc_heap_lock;

  /* flush dead entries from the continuation stack */
  {
    int x;
    int bound;
    SCM * elts;
    elts = SCM_VELTS (scm_continuation_stack);
    bound = SCM_LENGTH (scm_continuation_stack);
    x = SCM_INUM (scm_continuation_stack_ptr);
    while (x < bound)
      {
	elts[x] = SCM_BOOL_F;
	++x;
      }
  }

  scm_c_hook_run (&scm_before_mark_c_hook, 0);

  clear_mark_space ();

#ifndef USE_THREADS

  /* Protect from the C stack.  This must be the first marking
   * done because it provides information about what objects
   * are "in-use" by the C code.   "in-use" objects are  those
   * for which the values from SCM_LENGTH and SCM_CHARS must remain
   * usable.   This requirement is stricter than a liveness
   * requirement -- in particular, it constrains the implementation
   * of scm_vector_set_length_x.
   */
  SCM_FLUSH_REGISTER_WINDOWS;
  /* This assumes that all registers are saved into the jmp_buf */
  setjmp (scm_save_regs_gc_mark);
  scm_mark_locations ((SCM_STACKITEM *) scm_save_regs_gc_mark,
		      (   (scm_sizet) (sizeof (SCM_STACKITEM) - 1 +
				       sizeof scm_save_regs_gc_mark)
			  / sizeof (SCM_STACKITEM)));

  {
    scm_sizet stack_len = scm_stack_size (scm_stack_base);
#ifdef SCM_STACK_GROWS_UP
    scm_mark_locations (scm_stack_base, stack_len);
#else
    scm_mark_locations (scm_stack_base - stack_len, stack_len);
#endif
  }

#else /* USE_THREADS */

  /* Mark every thread's stack and registers */
  scm_threads_mark_stacks ();

#endif /* USE_THREADS */

  /* FIXME: insert a phase to un-protect string-data preserved
   * in scm_vector_set_length_x.
   */

  j = SCM_NUM_PROTECTS;
  while (j--)
    scm_gc_mark (scm_sys_protects[j]);

  /* FIXME: we should have a means to register C functions to be run
   * in different phases of GC
   */
  scm_mark_subr_table ();

#ifndef USE_THREADS
  scm_gc_mark (scm_root->handle);
#endif

  t_before_sweep = scm_c_get_internal_run_time ();
  scm_gc_mark_time_taken += (t_before_sweep - t_before_gc);

  scm_c_hook_run (&scm_before_sweep_c_hook, 0);

  scm_gc_sweep ();

  scm_c_hook_run (&scm_after_sweep_c_hook, 0);

  --scm_gc_heap_lock;
  gc_end_stats ();

#ifdef USE_THREADS
  SCM_THREAD_CRITICAL_SECTION_END;
#endif
  scm_c_hook_run (&scm_after_gc_c_hook, 0);
  --scm_gc_running_p;
}



/* {Mark/Sweep}
 */



/* Mark an object precisely.
 */
void
scm_gc_mark (SCM p)
#define FUNC_NAME "scm_gc_mark"
{
  register long i;
  register SCM ptr;

  ptr = p;

gc_mark_loop:
  if (SCM_IMP (ptr))
    return;

gc_mark_nimp:
  if (!SCM_CELLP (ptr))
    SCM_MISC_ERROR ("rogue pointer in heap", SCM_EOL);

#if (defined (GUILE_DEBUG_FREELIST))

  if (SCM_GC_IN_CARD_HEADERP (SCM2PTR (ptr)))
    scm_wta (ptr, "rogue pointer in heap", NULL);

#endif

  if (SCM_GCMARKP (ptr))
    return;

  SCM_SETGCMARK (ptr);

  switch (SCM_TYP7 (ptr))
    {
    case scm_tcs_cons_nimcar:
      if (SCM_IMP (SCM_CDR (ptr)))
	{
	  ptr = SCM_CAR (ptr);
	  goto gc_mark_nimp;
	}
      scm_gc_mark (SCM_CAR (ptr));
      ptr = SCM_CDR (ptr);
      goto gc_mark_nimp;
    case scm_tcs_cons_imcar:
      ptr = SCM_CDR (ptr);
      goto gc_mark_loop;
    case scm_tc7_pws:
      scm_gc_mark (SCM_CELL_OBJECT_2 (ptr));
      ptr = SCM_CDR (ptr);
      goto gc_mark_loop;
    case scm_tcs_cons_gloc:
      {
	/* Dirk:FIXME:: The following code is super ugly:  ptr may be a struct
	 * or a gloc.  If it is a gloc, the cell word #0 of ptr is a pointer
	 * to a heap cell.  If it is a struct, the cell word #0 of ptr is a
	 * pointer to a struct vtable data region. The fact that these are
	 * accessed in the same way restricts the possibilites to change the
	 * data layout of structs or heap cells.
	 */
	scm_bits_t word0 = SCM_CELL_WORD_0 (ptr) - scm_tc3_cons_gloc;
	scm_bits_t * vtable_data = (scm_bits_t *) word0; /* access as struct */
	if (vtable_data [scm_vtable_index_vcell] != 0)
	  {
            /* ptr is a gloc */
            SCM gloc_car = SCM_PACK (word0);
            scm_gc_mark (gloc_car);
            ptr = SCM_CDR (ptr);
            goto gc_mark_loop;
          }
        else
          {
            /* ptr is a struct */
            SCM layout = SCM_PACK (vtable_data [scm_vtable_index_layout]);
            int len = SCM_LENGTH (layout);
            char * fields_desc = SCM_CHARS (layout);
            scm_bits_t * struct_data = (scm_bits_t *) SCM_STRUCT_DATA (ptr);

            if (vtable_data[scm_struct_i_flags] & SCM_STRUCTF_ENTITY)
              {
                scm_gc_mark (SCM_PACK (struct_data[scm_struct_i_procedure]));
                scm_gc_mark (SCM_PACK (struct_data[scm_struct_i_setter]));
              }
            if (len)
              {
                int x;

                for (x = 0; x < len - 2; x += 2, ++struct_data)
                  if (fields_desc[x] == 'p')
                    scm_gc_mark (SCM_PACK (*struct_data));
                if (fields_desc[x] == 'p')
                  {
                    if (SCM_LAYOUT_TAILP (fields_desc[x + 1]))
                      for (x = *struct_data; x; --x)
                        scm_gc_mark (SCM_PACK (*++struct_data));
                    else
                      scm_gc_mark (SCM_PACK (*struct_data));
                  }
              }
            /* mark vtable */
            ptr = SCM_PACK (vtable_data [scm_vtable_index_vtable]);
            goto gc_mark_loop;
	  }
      }
      break;
    case scm_tcs_closures:
      if (SCM_IMP (SCM_CDR (ptr)))
	{
	  ptr = SCM_CLOSCAR (ptr);
	  goto gc_mark_nimp;
	}
      scm_gc_mark (SCM_CLOSCAR (ptr));
      ptr = SCM_CDR (ptr);
      goto gc_mark_nimp;
    case scm_tc7_vector:
    case scm_tc7_lvector:
#ifdef CCLO
    case scm_tc7_cclo:
#endif
      i = SCM_LENGTH (ptr);
      if (i == 0)
	break;
      while (--i > 0)
	if (SCM_NIMP (SCM_VELTS (ptr)[i]))
	  scm_gc_mark (SCM_VELTS (ptr)[i]);
      ptr = SCM_VELTS (ptr)[0];
      goto gc_mark_loop;
    case scm_tc7_contin:
      if (SCM_VELTS (ptr))
	scm_mark_locations (SCM_VELTS_AS_STACKITEMS (ptr),
			    (scm_sizet)
			    (SCM_LENGTH (ptr) +
			     (sizeof (SCM_STACKITEM) + -1 +
			      sizeof (scm_contregs)) /
			     sizeof (SCM_STACKITEM)));
      break;
#ifdef HAVE_ARRAYS
    case scm_tc7_bvect:
    case scm_tc7_byvect:
    case scm_tc7_ivect:
    case scm_tc7_uvect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
    case scm_tc7_svect:
#ifdef HAVE_LONG_LONGS
    case scm_tc7_llvect:
#endif
#endif
    case scm_tc7_string:
      break;

    case scm_tc7_substring:
      ptr = SCM_CDR (ptr);
      goto gc_mark_loop;

    case scm_tc7_wvect:
      SCM_WVECT_GC_CHAIN (ptr) = scm_weak_vectors;
      scm_weak_vectors = ptr;
      if (SCM_IS_WHVEC_ANY (ptr))
	{
	  int x;
	  int len;
	  int weak_keys;
	  int weak_values;

	  len = SCM_LENGTH (ptr);
	  weak_keys = SCM_IS_WHVEC (ptr) || SCM_IS_WHVEC_B (ptr);
	  weak_values = SCM_IS_WHVEC_V (ptr) || SCM_IS_WHVEC_B (ptr);

	  for (x = 0; x < len; ++x)
	    {
	      SCM alist;
	      alist = SCM_VELTS (ptr)[x];

	      /* mark everything on the alist except the keys or
	       * values, according to weak_values and weak_keys.  */
	      while (   SCM_CONSP (alist)
		     && !SCM_GCMARKP (alist)
		     && SCM_CONSP (SCM_CAR (alist)))
		{
		  SCM kvpair;
		  SCM next_alist;

		  kvpair = SCM_CAR (alist);
		  next_alist = SCM_CDR (alist);
		  /*
		   * Do not do this:
		   * 	SCM_SETGCMARK (alist);
		   *	SCM_SETGCMARK (kvpair);
		   *
		   * It may be that either the key or value is protected by
		   * an escaped reference to part of the spine of this alist.
		   * If we mark the spine here, and only mark one or neither of the
		   * key and value, they may never be properly marked.
		   * This leads to a horrible situation in which an alist containing
		   * freelist cells is exported.
		   *
		   * So only mark the spines of these arrays last of all marking.
		   * If somebody confuses us by constructing a weak vector
		   * with a circular alist then we are hosed, but at least we
		   * won't prematurely drop table entries.
		   */
		  if (!weak_keys)
		    scm_gc_mark (SCM_CAR (kvpair));
		  if (!weak_values)
		    scm_gc_mark (SCM_CDR (kvpair));
		  alist = next_alist;
		}
	      if (SCM_NIMP (alist))
		scm_gc_mark (alist);
	    }
	}
      break;

    case scm_tc7_msymbol:
      scm_gc_mark (SCM_SYMBOL_FUNC (ptr));
      ptr = SCM_SYMBOL_PROPS (ptr);
      goto gc_mark_loop;
    case scm_tc7_ssymbol:
    case scm_tcs_subrs:
      break;
    case scm_tc7_port:
      i = SCM_PTOBNUM (ptr);
      if (!(i < scm_numptob))
	goto def;
      if (SCM_PTAB_ENTRY(ptr))
	scm_gc_mark (SCM_PTAB_ENTRY(ptr)->file_name);
      if (scm_ptobs[i].mark)
	{
	  ptr = (scm_ptobs[i].mark) (ptr);
	  goto gc_mark_loop;
	}
      else
	return;
      break;
    case scm_tc7_smob:
      switch (SCM_TYP16 (ptr))
	{ /* should be faster than going through scm_smobs */
	case scm_tc_free_cell:
	  /* printf("found free_cell %X ", ptr); fflush(stdout); */
	case scm_tc16_big:
	case scm_tc16_real:
	case scm_tc16_complex:
	  break;
	default:
	  i = SCM_SMOBNUM (ptr);
	  if (!(i < scm_numsmob))
	    goto def;
	  if (scm_smobs[i].mark)
	    {
	      ptr = (scm_smobs[i].mark) (ptr);
	      goto gc_mark_loop;
	    }
	  else
	    return;
	}
      break;
    default:
    def:
      SCM_MISC_ERROR ("unknown type", SCM_EOL);
    }
}
#undef FUNC_NAME


/* Mark a Region Conservatively
 */

void
scm_mark_locations (SCM_STACKITEM x[], scm_sizet n)
{
  unsigned long m;

  for (m = 0; m < n; ++m)
    {
      SCM obj = * (SCM *) &x[m];
      if (SCM_CELLP (obj))
	{
	  SCM_CELLPTR ptr = SCM2PTR (obj);
	  int i = 0;
	  int j = scm_n_heap_segs - 1;
	  if (SCM_PTR_LE (scm_heap_table[i].bounds[0], ptr)
	      && SCM_PTR_GT (scm_heap_table[j].bounds[1], ptr))
	    {
	      while (i <= j)
		{
		  int seg_id;
		  seg_id = -1;
		  if ((i == j)
		      || SCM_PTR_GT (scm_heap_table[i].bounds[1], ptr))
		    seg_id = i;
		  else if (SCM_PTR_LE (scm_heap_table[j].bounds[0], ptr))
		    seg_id = j;
		  else
		    {
		      int k;
		      k = (i + j) / 2;
		      if (k == i)
			break;
		      if (SCM_PTR_GT (scm_heap_table[k].bounds[1], ptr))
			{
			  j = k;
			  ++i;
			  if (SCM_PTR_LE (scm_heap_table[i].bounds[0], ptr))
			    continue;
			  else
			    break;
			}
		      else if (SCM_PTR_LE (scm_heap_table[k].bounds[0], ptr))
			{
			  i = k;
			  --j;
			  if (SCM_PTR_GT (scm_heap_table[j].bounds[1], ptr))
			    continue;
			  else
			    break;
			}
		    }

                  if (SCM_GC_IN_CARD_HEADERP (ptr))
                    break;

		  if (scm_heap_table[seg_id].span == 1
		      || SCM_DOUBLE_CELLP (obj))
                    scm_gc_mark (obj);
                  
		  break;
		}
	    }
	}
    }
}


/* The function scm_cellp determines whether an SCM value can be regarded as a
 * pointer to a cell on the heap.  Binary search is used in order to determine
 * the heap segment that contains the cell.
 */
int
scm_cellp (SCM value)
{
  if (SCM_CELLP (value)) {
    scm_cell * ptr = SCM2PTR (value);
    unsigned int i = 0;
    unsigned int j = scm_n_heap_segs - 1;

    while (i < j) {
      int k = (i + j) / 2;
      if (SCM_PTR_GT (scm_heap_table[k].bounds[1], ptr)) {
	j = k;
      } else if (SCM_PTR_LE (scm_heap_table[k].bounds[0], ptr)) {
	i = k + 1;
      }
    }

    if (SCM_PTR_LE (scm_heap_table[i].bounds[0], ptr)
	&& SCM_PTR_GT (scm_heap_table[i].bounds[1], ptr)
	&& (scm_heap_table[i].span == 1 || SCM_DOUBLE_CELLP (value))
        && !SCM_GC_IN_CARD_HEADERP (ptr)
        )
      return 1;
    else
      return 0;
  } else
    return 0;
}


static void
gc_sweep_freelist_start (scm_freelist_t *freelist)
{
  freelist->cells = SCM_EOL;
  freelist->left_to_collect = freelist->cluster_size;
  freelist->clusters_allocated = 0;
  freelist->clusters = SCM_EOL;
  freelist->clustertail = &freelist->clusters;
  freelist->collected_1 = freelist->collected;
  freelist->collected = 0;
}

static void
gc_sweep_freelist_finish (scm_freelist_t *freelist)
{
  int collected;
  *freelist->clustertail = freelist->cells;
  if (!SCM_NULLP (freelist->cells))
    {
      SCM c = freelist->cells;
      SCM_SETCAR (c, SCM_CDR (c));
      SCM_SETCDR (c, SCM_EOL);
      freelist->collected +=
	freelist->span * (freelist->cluster_size - freelist->left_to_collect);
    }
  scm_gc_cells_collected += freelist->collected;

  /* Although freelist->min_yield is used to test freelist->collected
   * (which is the local GC yield for freelist), it is adjusted so
   * that *total* yield is freelist->min_yield_fraction of total heap
   * size.  This means that a too low yield is compensated by more
   * heap on the list which is currently doing most work, which is
   * just what we want.
   */
  collected = SCM_MAX (freelist->collected_1, freelist->collected);
  freelist->grow_heap_p = (collected < freelist->min_yield);
}

#define NEXT_DATA_CELL(ptr, span) \
    do { \
      scm_cell *nxt__ = CELL_UP ((char *) (ptr) + 1, (span)); \
      (ptr) = (SCM_GC_IN_CARD_HEADERP (nxt__) ? \
               CELL_UP (SCM_GC_CELL_CARD (nxt__) + SCM_GC_CARD_N_HEADER_CELLS, span) \
               : nxt__); \
    } while (0)

void
scm_gc_sweep ()
#define FUNC_NAME "scm_gc_sweep"
{
  register SCM_CELLPTR ptr;
  register SCM nfreelist;
  register scm_freelist_t *freelist;
  register long m;
  register int span;
  long i;
  scm_sizet seg_size;

  m = 0;

  gc_sweep_freelist_start (&scm_master_freelist);
  gc_sweep_freelist_start (&scm_master_freelist2);

  for (i = 0; i < scm_n_heap_segs; i++)
    {
      register unsigned int left_to_collect;
      register scm_sizet j;

      /* Unmarked cells go onto the front of the freelist this heap
	 segment points to.  Rather than updating the real freelist
	 pointer as we go along, we accumulate the new head in
	 nfreelist.  Then, if it turns out that the entire segment is
	 free, we free (i.e., malloc's free) the whole segment, and
	 simply don't assign nfreelist back into the real freelist.  */
      freelist = scm_heap_table[i].freelist;
      nfreelist = freelist->cells;
      left_to_collect = freelist->left_to_collect;
      span = scm_heap_table[i].span;

      ptr = CELL_UP (scm_heap_table[i].bounds[0], span);
      seg_size = CELL_DN (scm_heap_table[i].bounds[1], span) - ptr;

      /* use only data cells in seg_size */
      seg_size = (seg_size / SCM_GC_CARD_N_CELLS) * (SCM_GC_CARD_N_DATA_CELLS / span) * span;

      scm_gc_cells_swept += seg_size;

      for (j = seg_size + span; j -= span; ptr += span)
	{
	  SCM scmptr;

          if (SCM_GC_IN_CARD_HEADERP (ptr))
	    {
              SCM_CELLPTR nxt;

              /* cheat here */
              nxt = ptr;
              NEXT_DATA_CELL (nxt, span);
              j += span;

              ptr = nxt - span;
              continue;
            }

          scmptr = PTR2SCM (ptr);

          if (SCM_GCMARKP (scmptr))
              continue;

	  switch SCM_TYP7 (scmptr)
            {
	    case scm_tcs_cons_gloc:
	      {
		/* Dirk:FIXME:: Again, super ugly code:  scmptr may be a
		 * struct or a gloc.  See the corresponding comment in
		 * scm_gc_mark.
		 */
		scm_bits_t word0 = (SCM_CELL_WORD_0 (scmptr)
				    - scm_tc3_cons_gloc);
		/* access as struct */
		scm_bits_t * vtable_data = (scm_bits_t *) word0;
		if (vtable_data[scm_vtable_index_vcell] == 0)
		  {
		    /* Structs need to be freed in a special order.
		     * This is handled by GC C hooks in struct.c.
		     */
		    SCM_SET_STRUCT_GC_CHAIN (scmptr, scm_structs_to_free);
		    scm_structs_to_free = scmptr;
                    continue;
		  }
		/* fall through so that scmptr gets collected */
	      }
	      break;
	    case scm_tcs_cons_imcar:
	    case scm_tcs_cons_nimcar:
	    case scm_tcs_closures:
	    case scm_tc7_pws:
	      break;
	    case scm_tc7_wvect:
              m += (2 + SCM_LENGTH (scmptr)) * sizeof (SCM);
              scm_must_free ((char *)(SCM_VELTS (scmptr) - 2));
              break;
	    case scm_tc7_vector:
	    case scm_tc7_lvector:
#ifdef CCLO
	    case scm_tc7_cclo:
#endif
	      m += (SCM_LENGTH (scmptr) * sizeof (SCM));
	    freechars:
	      scm_must_free (SCM_CHARS (scmptr));
	      /*	SCM_SETCHARS(scmptr, 0);*/
	      break;
#ifdef HAVE_ARRAYS
	    case scm_tc7_bvect:
	      m += sizeof (long) * ((SCM_HUGE_LENGTH (scmptr) + SCM_LONG_BIT - 1) / SCM_LONG_BIT);
	      goto freechars;
	    case scm_tc7_byvect:
	      m += SCM_HUGE_LENGTH (scmptr) * sizeof (char);
	      goto freechars;
	    case scm_tc7_ivect:
	    case scm_tc7_uvect:
	      m += SCM_HUGE_LENGTH (scmptr) * sizeof (long);
	      goto freechars;
	    case scm_tc7_svect:
	      m += SCM_HUGE_LENGTH (scmptr) * sizeof (short);
	      goto freechars;
#ifdef HAVE_LONG_LONGS
	    case scm_tc7_llvect:
	      m += SCM_HUGE_LENGTH (scmptr) * sizeof (long_long);
	      goto freechars;
#endif
	    case scm_tc7_fvect:
	      m += SCM_HUGE_LENGTH (scmptr) * sizeof (float);
	      goto freechars;
	    case scm_tc7_dvect:
	      m += SCM_HUGE_LENGTH (scmptr) * sizeof (double);
	      goto freechars;
	    case scm_tc7_cvect:
	      m += SCM_HUGE_LENGTH (scmptr) * 2 * sizeof (double);
	      goto freechars;
#endif
	    case scm_tc7_substring:
	      break;
	    case scm_tc7_string:
	      m += SCM_HUGE_LENGTH (scmptr) + 1;
	      goto freechars;
	    case scm_tc7_msymbol:
	      m += (SCM_LENGTH (scmptr) + 1
		    + (SCM_CHARS (scmptr) - (char *) SCM_SLOTS (scmptr)));
	      scm_must_free ((char *)SCM_SLOTS (scmptr));
	      break;
	    case scm_tc7_contin:
	      m += SCM_LENGTH (scmptr) * sizeof (SCM_STACKITEM) + sizeof (scm_contregs);
	      if (SCM_VELTS (scmptr))
		goto freechars;
	    case scm_tc7_ssymbol:
	      break;
	    case scm_tcs_subrs:
              /* the various "subrs" (primitives) are never freed */
	      continue;
	    case scm_tc7_port:
	      if SCM_OPENP (scmptr)
		{
		  int k = SCM_PTOBNUM (scmptr);
		  if (!(k < scm_numptob))
		    goto sweeperr;
		  /* Keep "revealed" ports alive.  */
		  if (scm_revealed_count (scmptr) > 0)
		    continue;
		  /* Yes, I really do mean scm_ptobs[k].free */
		  /* rather than ftobs[k].close.  .close */
		  /* is for explicit CLOSE-PORT by user */
		  m += (scm_ptobs[k].free) (scmptr);
		  SCM_SETSTREAM (scmptr, 0);
		  scm_remove_from_port_table (scmptr);
		  scm_gc_ports_collected++;
		  SCM_SETAND_CAR (scmptr, ~SCM_OPN);
		}
	      break;
	    case scm_tc7_smob:
	      switch SCM_TYP16 (scmptr)
		{
		case scm_tc_free_cell:
		case scm_tc16_real:
		  break;
#ifdef SCM_BIGDIG
		case scm_tc16_big:
		  m += (SCM_NUMDIGS (scmptr) * SCM_BITSPERDIG / SCM_CHAR_BIT);
		  goto freechars;
#endif /* def SCM_BIGDIG */
		case scm_tc16_complex:
		  m += 2 * sizeof (double);
		  goto freechars;
		default:
		  {
		    int k;
		    k = SCM_SMOBNUM (scmptr);
		    if (!(k < scm_numsmob))
		      goto sweeperr;
		    m += (scm_smobs[k].free) (scmptr);
		    break;
		  }
		}
	      break;
	    default:
	    sweeperr:
	      SCM_MISC_ERROR ("unknown type", SCM_EOL);
	    }

	  if (!--left_to_collect)
	    {
	      SCM_SETCAR (scmptr, nfreelist);
	      *freelist->clustertail = scmptr;
	      freelist->clustertail = SCM_CDRLOC (scmptr);

	      nfreelist = SCM_EOL;
	      freelist->collected += span * freelist->cluster_size;
	      left_to_collect = freelist->cluster_size;
	    }
	  else
	    {
	      /* Stick the new cell on the front of nfreelist.  It's
		 critical that we mark this cell as freed; otherwise, the
		 conservative collector might trace it as some other type
		 of object.  */
	      SCM_SET_CELL_TYPE (scmptr, scm_tc_free_cell);
	      SCM_SET_FREE_CELL_CDR (scmptr, nfreelist);
	      nfreelist = scmptr;
	    }
	}

#ifdef GC_FREE_SEGMENTS
      if (n == seg_size)
	{
	  register long j;

	  freelist->heap_size -= seg_size;
	  free ((char *) scm_heap_table[i].bounds[0]);
	  scm_heap_table[i].bounds[0] = 0;
	  for (j = i + 1; j < scm_n_heap_segs; j++)
	    scm_heap_table[j - 1] = scm_heap_table[j];
	  scm_n_heap_segs -= 1;
	  i--;		/* We need to scan the segment just moved.  */
	}
      else
#endif /* ifdef GC_FREE_SEGMENTS */
	{
	  /* Update the real freelist pointer to point to the head of
	     the list of free cells we've built for this segment.  */
	  freelist->cells = nfreelist;
	  freelist->left_to_collect = left_to_collect;
	}

#ifdef GUILE_DEBUG_FREELIST
      scm_map_free_list ();
#endif
    }

  gc_sweep_freelist_finish (&scm_master_freelist);
  gc_sweep_freelist_finish (&scm_master_freelist2);

  /* When we move to POSIX threads private freelists should probably
     be GC-protected instead. */
  scm_freelist = SCM_EOL;
  scm_freelist2 = SCM_EOL;

  scm_cells_allocated = (SCM_HEAP_SIZE - scm_gc_cells_collected);
  scm_gc_yield -= scm_cells_allocated;
  scm_mallocated -= m;
  scm_gc_malloc_collected = m;
}
#undef FUNC_NAME



/* {Front end to malloc}
 *
 * scm_must_malloc, scm_must_realloc, scm_must_free, scm_done_malloc,
 * scm_done_free
 *
 * These functions provide services comperable to malloc, realloc, and
 * free.  They are for allocating malloced parts of scheme objects.
 * The primary purpose of the front end is to impose calls to gc.  */


/* scm_must_malloc
 * Return newly malloced storage or throw an error.
 *
 * The parameter WHAT is a string for error reporting.
 * If the threshold scm_mtrigger will be passed by this
 * allocation, or if the first call to malloc fails,
 * garbage collect -- on the presumption that some objects
 * using malloced storage may be collected.
 *
 * The limit scm_mtrigger may be raised by this allocation.
 */
void *
scm_must_malloc (scm_sizet size, const char *what)
{
  void *ptr;
  unsigned long nm = scm_mallocated + size;

  if (nm <= scm_mtrigger)
    {
      SCM_SYSCALL (ptr = malloc (size));
      if (NULL != ptr)
	{
	  scm_mallocated = nm;
#ifdef GUILE_DEBUG_MALLOC
	  scm_malloc_register (ptr, what);
#endif
	  return ptr;
	}
    }

  scm_igc (what);

  nm = scm_mallocated + size;
  SCM_SYSCALL (ptr = malloc (size));
  if (NULL != ptr)
    {
      scm_mallocated = nm;
      if (nm > scm_mtrigger - SCM_MTRIGGER_HYSTERESIS) {
	if (nm > scm_mtrigger)
	  scm_mtrigger = nm + nm / 2;
	else
	  scm_mtrigger += scm_mtrigger / 2;
      }
#ifdef GUILE_DEBUG_MALLOC
      scm_malloc_register (ptr, what);
#endif

      return ptr;
    }

  scm_memory_error (what);
}


/* scm_must_realloc
 * is similar to scm_must_malloc.
 */
void *
scm_must_realloc (void *where,
		  scm_sizet old_size,
		  scm_sizet size,
		  const char *what)
{
  void *ptr;
  scm_sizet nm = scm_mallocated + size - old_size;

  if (nm <= scm_mtrigger)
    {
      SCM_SYSCALL (ptr = realloc (where, size));
      if (NULL != ptr)
	{
	  scm_mallocated = nm;
#ifdef GUILE_DEBUG_MALLOC
	  scm_malloc_reregister (where, ptr, what);
#endif
	  return ptr;
	}
    }

  scm_igc (what);

  nm = scm_mallocated + size - old_size;
  SCM_SYSCALL (ptr = realloc (where, size));
  if (NULL != ptr)
    {
      scm_mallocated = nm;
      if (nm > scm_mtrigger - SCM_MTRIGGER_HYSTERESIS) {
	if (nm > scm_mtrigger)
	  scm_mtrigger = nm + nm / 2;
	else
	  scm_mtrigger += scm_mtrigger / 2;
      }
#ifdef GUILE_DEBUG_MALLOC
      scm_malloc_reregister (where, ptr, what);
#endif
      return ptr;
    }

  scm_memory_error (what);
}


void
scm_must_free (void *obj)
#define FUNC_NAME "scm_must_free"
{
#ifdef GUILE_DEBUG_MALLOC
  scm_malloc_unregister (obj);
#endif
  if (obj)
    free (obj);
  else
    SCM_MISC_ERROR ("freeing NULL pointer", SCM_EOL);
}
#undef FUNC_NAME


/* Announce that there has been some malloc done that will be freed
 * during gc.  A typical use is for a smob that uses some malloced
 * memory but can not get it from scm_must_malloc (for whatever
 * reason).  When a new object of this smob is created you call
 * scm_done_malloc with the size of the object.  When your smob free
 * function is called, be sure to include this size in the return
 * value.
 *
 * If you can't actually free the memory in the smob free function,
 * for whatever reason (like reference counting), you still can (and
 * should) report the amount of memory freed when you actually free it.
 * Do it by calling scm_done_malloc with the _negated_ size.  Clever,
 * eh?  Or even better, call scm_done_free. */

void
scm_done_malloc (long size)
{
  scm_mallocated += size;

  if (scm_mallocated > scm_mtrigger)
    {
      scm_igc ("foreign mallocs");
      if (scm_mallocated > scm_mtrigger - SCM_MTRIGGER_HYSTERESIS)
	{
	  if (scm_mallocated > scm_mtrigger)
	    scm_mtrigger = scm_mallocated + scm_mallocated / 2;
	  else
	    scm_mtrigger += scm_mtrigger / 2;
	}
    }
}

void
scm_done_free (long size)
{
  scm_mallocated -= size;
}



/* {Heap Segments}
 *
 * Each heap segment is an array of objects of a particular size.
 * Every segment has an associated (possibly shared) freelist.
 * A table of segment records is kept that records the upper and
 * lower extents of the segment;  this is used during the conservative
 * phase of gc to identify probably gc roots (because they point
 * into valid segments at reasonable offsets).  */

/* scm_expmem
 * is true if the first segment was smaller than INIT_HEAP_SEG.
 * If scm_expmem is set to one, subsequent segment allocations will
 * allocate segments of size SCM_EXPHEAP(scm_heap_size).
 */
int scm_expmem = 0;

scm_sizet scm_max_segment_size;

/* scm_heap_org
 * is the lowest base address of any heap segment.
 */
SCM_CELLPTR scm_heap_org;

scm_heap_seg_data_t * scm_heap_table = 0;
static unsigned int heap_segment_table_size = 0;
int scm_n_heap_segs = 0;

/* init_heap_seg
 * initializes a new heap segment and returns the number of objects it contains.
 *
 * The segment origin and segment size in bytes are input parameters.
 * The freelist is both input and output.
 *
 * This function presumes that the scm_heap_table has already been expanded
 * to accomodate a new segment record and that the markbit space was reserved
 * for all the cards in this segment.
 */

#define INIT_CARD(card, span) \
    do { \
      SCM_GC_CARD_BVEC (card) = get_bvec (); \
      if ((span) == 2) \
        SCM_GC_SET_CARD_DOUBLECELL (card); \
    } while (0)

static scm_sizet
init_heap_seg (SCM_CELLPTR seg_org, scm_sizet size, scm_freelist_t *freelist)
{
  register SCM_CELLPTR ptr;
  SCM_CELLPTR seg_end;
  int new_seg_index;
  int n_new_cells;
  int span = freelist->span;

  if (seg_org == NULL)
    return 0;

  /* Align the begin ptr up.
   */
  ptr = SCM_GC_CARD_UP (seg_org);

  /* Compute the ceiling on valid object pointers w/in this segment.
   */
  seg_end = SCM_GC_CARD_DOWN ((char *)seg_org + size);

  /* Find the right place and insert the segment record.
   *
   */
  for (new_seg_index = 0;
       (   (new_seg_index < scm_n_heap_segs)
	&& SCM_PTR_LE (scm_heap_table[new_seg_index].bounds[0], seg_org));
       new_seg_index++)
    ;

  {
    int i;
    for (i = scm_n_heap_segs; i > new_seg_index; --i)
      scm_heap_table[i] = scm_heap_table[i - 1];
  }

  ++scm_n_heap_segs;

  scm_heap_table[new_seg_index].span = span;
  scm_heap_table[new_seg_index].freelist = freelist;
  scm_heap_table[new_seg_index].bounds[0] = ptr;
  scm_heap_table[new_seg_index].bounds[1] = seg_end;

  /*n_new_cells*/
  n_new_cells = seg_end - ptr;

  freelist->heap_size += n_new_cells;

  /* Partition objects in this segment into clusters */
  {
    SCM clusters;
    SCM *clusterp = &clusters;

    NEXT_DATA_CELL (ptr, span);
    while (ptr < seg_end)
      {
        scm_cell *nxt = ptr;
        scm_cell *prv = NULL;
        scm_cell *last_card = NULL;
        int n_data_cells = (SCM_GC_CARD_N_DATA_CELLS / span) * SCM_CARDS_PER_CLUSTER - 1;
        NEXT_DATA_CELL(nxt, span);

	/* Allocate cluster spine
	 */
	*clusterp = PTR2SCM (ptr);
        SCM_SETCAR (*clusterp, PTR2SCM (nxt));
	clusterp = SCM_CDRLOC (*clusterp);
        ptr = nxt;

        while (n_data_cells--)
	  {
            scm_cell *card = SCM_GC_CELL_CARD (ptr);
	    SCM scmptr = PTR2SCM (ptr);
            nxt = ptr;
            NEXT_DATA_CELL (nxt, span);
            prv = ptr;

            if (card != last_card)
              {
                INIT_CARD (card, span);
                last_card = card;
              }

	    SCM_SET_CELL_TYPE (scmptr, scm_tc_free_cell);
	    SCM_SETCDR (scmptr, PTR2SCM (nxt));

            ptr = nxt;
	  }

	SCM_SET_FREE_CELL_CDR (PTR2SCM (prv), SCM_EOL);
      }

    /* sanity check */
    {
      scm_cell *ref = seg_end;
      NEXT_DATA_CELL (ref, span);
      if (ref != ptr)
        /* [cmm] looks like the segment size doesn't divide cleanly by
           cluster size.  bad cmm! */
        abort();
    }

    /* Patch up the last cluster pointer in the segment
     * to join it to the input freelist.
     */
    *clusterp = freelist->clusters;
    freelist->clusters = clusters;
  }

#ifdef DEBUGINFO
  fprintf (stderr, "H");
#endif
  return size;
}

static scm_sizet
round_to_cluster_size (scm_freelist_t *freelist, scm_sizet len)
{
  scm_sizet cluster_size_in_bytes = CLUSTER_SIZE_IN_BYTES (freelist);

  return
    (len + cluster_size_in_bytes - 1) / cluster_size_in_bytes * cluster_size_in_bytes
    + ALIGNMENT_SLACK (freelist);
}

static void
alloc_some_heap (scm_freelist_t *freelist, policy_on_error error_policy)
#define FUNC_NAME "alloc_some_heap"
{
  SCM_CELLPTR ptr;
  long len;

  if (scm_gc_heap_lock)
    {
      /* Critical code sections (such as the garbage collector) aren't
       * supposed to add heap segments.
       */
      fprintf (stderr, "alloc_some_heap: Can not extend locked heap.\n");
      abort ();
    }

  if (scm_n_heap_segs == heap_segment_table_size)
    {
      /* We have to expand the heap segment table to have room for the new
       * segment.  Do not yet increment scm_n_heap_segs -- that is done by
       * init_heap_seg only if the allocation of the segment itself succeeds.
       */
      unsigned int new_table_size = scm_n_heap_segs + 1;
      size_t size = new_table_size * sizeof (scm_heap_seg_data_t);
      scm_heap_seg_data_t * new_heap_table;

      SCM_SYSCALL (new_heap_table = ((scm_heap_seg_data_t *)
				     realloc ((char *)scm_heap_table, size)));
      if (!new_heap_table)
	{
	  if (error_policy == abort_on_error)
	    {
	      fprintf (stderr, "alloc_some_heap: Could not grow heap segment table.\n");
	      abort ();
	    }
	  else
	    {
	      return;
	    }
	}
      else
	{
	  scm_heap_table = new_heap_table;
	  heap_segment_table_size = new_table_size;
	}
    }

  /* Pick a size for the new heap segment.
   * The rule for picking the size of a segment is explained in
   * gc.h
   */
  {
    /* Assure that the new segment is predicted to be large enough.
     *
     * New yield should at least equal GC fraction of new heap size, i.e.
     *
     *   y + dh > f * (h + dh)
     *
     *    y : yield
     *    f : min yield fraction
     *    h : heap size
     *   dh : size of new heap segment
     *
     * This gives dh > (f * h - y) / (1 - f)
     */
    int f = freelist->min_yield_fraction;
    long h = SCM_HEAP_SIZE;
    long min_cells = (f * h - 100 * (long) scm_gc_yield) / (99 - f);
    len =  SCM_EXPHEAP (freelist->heap_size);
#ifdef DEBUGINFO
    fprintf (stderr, "(%d < %d)", len, min_cells);
#endif
    if (len < min_cells)
      len = min_cells + freelist->cluster_size;
    len *= sizeof (scm_cell);
    /* force new sampling */
    freelist->collected = LONG_MAX;
  }

  if (len > scm_max_segment_size)
    len = scm_max_segment_size;

  {
    scm_sizet smallest;

    smallest = CLUSTER_SIZE_IN_BYTES (freelist);

    if (len < smallest)
      len = smallest;

    /* Allocate with decaying ambition. */
    while ((len >= SCM_MIN_HEAP_SEG_SIZE)
	   && (len >= smallest))
      {
        scm_sizet rounded_len = round_to_cluster_size (freelist, len);
	SCM_SYSCALL (ptr = (SCM_CELLPTR) malloc (rounded_len));
	if (ptr)
	  {
	    init_heap_seg (ptr, rounded_len, freelist);
	    return;
	  }
	len /= 2;
      }
  }

  if (error_policy == abort_on_error)
    {
      fprintf (stderr, "alloc_some_heap: Could not grow heap.\n");
      abort ();
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_unhash_name, "unhash-name", 1, 0, 0,
            (SCM name),
	    "")
#define FUNC_NAME s_scm_unhash_name
{
  int x;
  int bound;
  SCM_VALIDATE_SYMBOL (1,name);
  SCM_DEFER_INTS;
  bound = scm_n_heap_segs;
  for (x = 0; x < bound; ++x)
    {
      SCM_CELLPTR p;
      SCM_CELLPTR pbound;
      p  = scm_heap_table[x].bounds[0];
      pbound = scm_heap_table[x].bounds[1];
      while (p < pbound)
	{
	  SCM cell = PTR2SCM (p);
	  if (SCM_TYP3 (cell) == scm_tc3_cons_gloc)
	    {
	      /* Dirk:FIXME:: Again, super ugly code:  cell may be a gloc or a
	       * struct cell.  See the corresponding comment in scm_gc_mark.
	       */
	      scm_bits_t word0 = SCM_CELL_WORD_0 (cell) - scm_tc3_cons_gloc;
	      SCM gloc_car = SCM_PACK (word0); /* access as gloc */
	      SCM vcell = SCM_CELL_OBJECT_1 (gloc_car);
	      if ((SCM_EQ_P (name, SCM_BOOL_T) || SCM_EQ_P (SCM_CAR (gloc_car), name))
		  && (SCM_UNPACK (vcell) != 0) && (SCM_UNPACK (vcell) != 1))
		{
		  SCM_SET_CELL_OBJECT_0 (cell, name);
		}
	    }
	  ++p;
	}
    }
  SCM_ALLOW_INTS;
  return name;
}
#undef FUNC_NAME



/* {GC Protection Helper Functions}
 */


void
scm_remember (SCM *ptr)
{ /* empty */ }


/*
  These crazy functions prevent garbage collection
  of arguments after the first argument by
  ensuring they remain live throughout the
  function because they are used in the last
  line of the code block.
  It'd be better to have a nice compiler hint to
  aid the conservative stack-scanning GC. --03/09/00 gjb */
SCM
scm_return_first (SCM elt, ...)
{
  return elt;
}

int
scm_return_first_int (int i, ...)
{
  return i;
}


SCM
scm_permanent_object (SCM obj)
{
  SCM_REDEFER_INTS;
  scm_permobjs = scm_cons (obj, scm_permobjs);
  SCM_REALLOW_INTS;
  return obj;
}


/* Protect OBJ from the garbage collector.  OBJ will not be freed, even if all
   other references are dropped, until the object is unprotected by calling
   scm_unprotect_object (OBJ).  Calls to scm_protect/unprotect_object nest,
   i. e. it is possible to protect the same object several times, but it is
   necessary to unprotect the object the same number of times to actually get
   the object unprotected.  It is an error to unprotect an object more often
   than it has been protected before.  The function scm_protect_object returns
   OBJ.
*/

/* Implementation note:  For every object X, there is a counter which
   scm_protect_object(X) increments and scm_unprotect_object(X) decrements.
*/

SCM
scm_protect_object (SCM obj)
{
  SCM handle;

  /* This critical section barrier will be replaced by a mutex. */
  SCM_REDEFER_INTS;

  handle = scm_hashq_create_handle_x (scm_protects, obj, SCM_MAKINUM (0));
  SCM_SETCDR (handle, SCM_MAKINUM (SCM_INUM (SCM_CDR (handle)) + 1));

  SCM_REALLOW_INTS;

  return obj;
}


/* Remove any protection for OBJ established by a prior call to
   scm_protect_object.  This function returns OBJ.

   See scm_protect_object for more information.  */
SCM
scm_unprotect_object (SCM obj)
{
  SCM handle;

  /* This critical section barrier will be replaced by a mutex. */
  SCM_REDEFER_INTS;

  handle = scm_hashq_get_handle (scm_protects, obj);

  if (SCM_IMP (handle))
    {
      fprintf (stderr, "scm_unprotect_object called on unprotected object\n");
      abort ();
    }
  else
    {
      unsigned long int count = SCM_INUM (SCM_CDR (handle)) - 1;
      if (count == 0)
	scm_hashq_remove_x (scm_protects, obj);
      else
	SCM_SETCDR (handle, SCM_MAKINUM (count));
    }

  SCM_REALLOW_INTS;

  return obj;
}

int terminating;

/* called on process termination.  */
#ifdef HAVE_ATEXIT
static void
cleanup (void)
#else
#ifdef HAVE_ON_EXIT
extern int on_exit (void (*procp) (), int arg);

static void
cleanup (int status, void *arg)
#else
#error Dont know how to setup a cleanup handler on your system.
#endif
#endif
{
  terminating = 1;
  scm_flush_all_ports ();
}


static int
make_initial_segment (scm_sizet init_heap_size, scm_freelist_t *freelist)
{
  scm_sizet rounded_size = round_to_cluster_size (freelist, init_heap_size);

  if (!init_heap_seg ((SCM_CELLPTR) malloc (rounded_size),
		      rounded_size,
		      freelist))
    {
      rounded_size = round_to_cluster_size (freelist, SCM_HEAP_SEG_SIZE);
      if (!init_heap_seg ((SCM_CELLPTR) malloc (rounded_size),
			  rounded_size,
			  freelist))
	return 1;
    }
  else
    scm_expmem = 1;

  if (freelist->min_yield_fraction)
    freelist->min_yield = (freelist->heap_size * freelist->min_yield_fraction
			    / 100);
  freelist->grow_heap_p = (freelist->heap_size < freelist->min_yield);

  return 0;
}


static void
init_freelist (scm_freelist_t *freelist,
	       int span,
	       int cluster_size,
	       int min_yield)
{
  freelist->clusters = SCM_EOL;
  freelist->cluster_size = cluster_size + 1;
  freelist->left_to_collect = 0;
  freelist->clusters_allocated = 0;
  freelist->min_yield = 0;
  freelist->min_yield_fraction = min_yield;
  freelist->span = span;
  freelist->collected = 0;
  freelist->collected_1 = 0;
  freelist->heap_size = 0;
}

int
scm_init_storage (scm_sizet init_heap_size_1, int gc_trigger_1,
		  scm_sizet init_heap_size_2, int gc_trigger_2,
		  scm_sizet max_segment_size)
{
  scm_sizet j;

  if (!init_heap_size_1)
    init_heap_size_1 = scm_default_init_heap_size_1;
  if (!init_heap_size_2)
    init_heap_size_2 = scm_default_init_heap_size_2;

  j = SCM_NUM_PROTECTS;
  while (j)
    scm_sys_protects[--j] = SCM_BOOL_F;
  scm_block_gc = 1;

  scm_freelist = SCM_EOL;
  scm_freelist2 = SCM_EOL;
  init_freelist (&scm_master_freelist,
		 1, SCM_CLUSTER_SIZE_1,
		 gc_trigger_1 ? gc_trigger_1 : scm_default_min_yield_1);
  init_freelist (&scm_master_freelist2,
		 2, SCM_CLUSTER_SIZE_2,
		 gc_trigger_2 ? gc_trigger_2 : scm_default_min_yield_2);
  scm_max_segment_size
    = max_segment_size ? max_segment_size : scm_default_max_segment_size;

  scm_expmem = 0;

  j = SCM_HEAP_SEG_SIZE;
  scm_mtrigger = SCM_INIT_MALLOC_LIMIT;
  scm_heap_table = ((scm_heap_seg_data_t *)
		    scm_must_malloc (sizeof (scm_heap_seg_data_t) * 2, "hplims"));
  heap_segment_table_size = 2;

  mark_space_ptr = &mark_space_head;

  if (make_initial_segment (init_heap_size_1, &scm_master_freelist) ||
      make_initial_segment (init_heap_size_2, &scm_master_freelist2))
    return 1;

  /* scm_hplims[0] can change. do not remove scm_heap_org */
  scm_heap_org = CELL_UP (scm_heap_table[0].bounds[0], 1);

  scm_c_hook_init (&scm_before_gc_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_before_mark_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_before_sweep_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_after_sweep_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_after_gc_c_hook, 0, SCM_C_HOOK_NORMAL);

  /* Initialise the list of ports.  */
  scm_port_table = (scm_port **)
    malloc (sizeof (scm_port *) * scm_port_table_room);
  if (!scm_port_table)
    return 1;

#ifdef HAVE_ATEXIT
  atexit (cleanup);
#else
#ifdef HAVE_ON_EXIT
  on_exit (cleanup, 0);
#endif
#endif

  scm_undefineds = scm_cons (SCM_UNDEFINED, SCM_EOL);
  SCM_SETCDR (scm_undefineds, scm_undefineds);

  scm_listofnull = scm_cons (SCM_EOL, SCM_EOL);
  scm_nullstr = scm_makstr (0L, 0);
  scm_nullvect = scm_make_vector (SCM_INUM0, SCM_UNDEFINED);
  scm_symhash = scm_make_vector (SCM_MAKINUM (scm_symhash_dim), SCM_EOL);
  scm_weak_symhash = scm_make_weak_key_hash_table (SCM_MAKINUM (scm_symhash_dim));
  scm_symhash_vars = scm_make_vector (SCM_MAKINUM (scm_symhash_dim), SCM_EOL);
  scm_stand_in_procs = SCM_EOL;
  scm_permobjs = SCM_EOL;
  scm_protects = scm_make_vector (SCM_MAKINUM (31), SCM_EOL);
  scm_sysintern ("most-positive-fixnum", SCM_MAKINUM (SCM_MOST_POSITIVE_FIXNUM));
  scm_sysintern ("most-negative-fixnum", SCM_MAKINUM (SCM_MOST_NEGATIVE_FIXNUM));
#ifdef SCM_BIGDIG
  scm_sysintern ("bignum-radix", SCM_MAKINUM (SCM_BIGRAD));
#endif

  return 0;
}



SCM scm_after_gc_hook;

#if (SCM_DEBUG_DEPRECATED == 0)
static SCM scm_gc_vcell;  /* the vcell for gc-thunk. */
#endif  /* SCM_DEBUG_DEPRECATED == 0 */
static SCM gc_async;


/* The function gc_async_thunk causes the execution of the after-gc-hook.  It
 * is run after the gc, as soon as the asynchronous events are handled by the
 * evaluator.
 */
static SCM
gc_async_thunk (void)
{
  scm_c_run_hook (scm_after_gc_hook, SCM_EOL);

#if (SCM_DEBUG_DEPRECATED == 0)

  /* The following code will be removed in Guile 1.5.  */
  if (SCM_NFALSEP (scm_gc_vcell))
    {
      SCM proc = SCM_CDR (scm_gc_vcell);

      if (SCM_NFALSEP (proc) && !SCM_UNBNDP (proc))
	scm_apply (proc, SCM_EOL, SCM_EOL);
    }

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

  return SCM_UNSPECIFIED;
}


/* The function mark_gc_async is run by the scm_after_gc_c_hook at the end of
 * the garbage collection.  The only purpose of this function is to mark the
 * gc_async (which will eventually lead to the execution of the
 * gc_async_thunk).
 */
static void *
mark_gc_async (void * hook_data, void *func_data, void *data)
{
  scm_system_async_mark (gc_async);
  return NULL;
}


void
scm_init_gc ()
{
  SCM after_gc_thunk;

  scm_after_gc_hook = scm_create_hook ("after-gc-hook", 0);

#if (SCM_DEBUG_DEPRECATED == 0)
  scm_gc_vcell = scm_sysintern ("gc-thunk", SCM_BOOL_F);
#endif  /* SCM_DEBUG_DEPRECATED == 0 */
  /* Dirk:FIXME:: We don't really want a binding here. */
  after_gc_thunk = scm_make_gsubr ("%gc-thunk", 0, 0, 0, gc_async_thunk);
  gc_async = scm_system_async (after_gc_thunk);

  scm_c_hook_add (&scm_after_gc_c_hook, mark_gc_async, NULL, 0);

#include "libguile/gc.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

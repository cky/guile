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


/* #define DEBUGINFO */

/* SECTION: This code is compiled once.
 */

#ifndef MARK_DEPENDENCIES


#include <stdio.h>
#include <errno.h>
#include <string.h>

#ifdef __ia64__
#include <ucontext.h>
extern unsigned long * __libc_ia64_register_backing_store_base;
#endif

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
#include "libguile/tags.h"

#include "libguile/validate.h"
#include "libguile/deprecation.h"
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



#define CELL_P(x)  (SCM_ITAG3 (x) == scm_tc3_cons)

unsigned int scm_gc_running_p = 0;



#if (SCM_DEBUG_CELL_ACCESSES == 1)

/* Set this to != 0 if every cell that is accessed shall be checked:
 */
unsigned int scm_debug_cell_accesses_p = 1;

/* Set this to 0 if no additional gc's shall be performed, otherwise set it to
 * the number of cell accesses after which a gc shall be called.
 */
static unsigned int debug_cells_gc_interval = 0;


/* Assert that the given object is a valid reference to a valid cell.  This
 * test involves to determine whether the object is a cell pointer, whether
 * this pointer actually points into a heap segment and whether the cell
 * pointed to is not a free cell.  Further, additional garbage collections may
 * get executed after a user defined number of cell accesses.  This helps to
 * find places in the C code where references are dropped for extremely short
 * periods.
 */
void
scm_assert_cell_valid (SCM cell)
{
  static unsigned int already_running = 0;

  if (scm_debug_cell_accesses_p && !already_running)
    {
      already_running = 1;  /* set to avoid recursion */

      if (!scm_cellp (cell))
	{
	  fprintf (stderr, "scm_assert_cell_valid: Not a cell object: %lux\n",
                   (unsigned long) SCM_UNPACK (cell));
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
	      fprintf (stderr, "scm_assert_cell_valid: Accessing free cell: %lux\n",
                       (unsigned long) SCM_UNPACK (cell));
	      abort ();
	    }

	  /* If desired, perform additional garbage collections after a user
	   * defined number of cell accesses.
	   */
	  if (debug_cells_gc_interval)
	    {
	      static unsigned int counter = 0;

	      if (counter != 0)
		{
		  --counter;
		}
	      else
		{
		  counter = debug_cells_gc_interval;
		  scm_igc ("scm_assert_cell_valid");
		}
	    }
	}
      already_running = 0;  /* re-enable */
    }
}


SCM_DEFINE (scm_set_debug_cell_accesses_x, "set-debug-cell-accesses!", 1, 0, 0,
	    (SCM flag),
	    "If @var{flag} is @code{#f}, cell access checking is disabled.\n"
	    "If @var{flag} is @code{#t}, cell access checking is enabled,\n"
	    "but no additional calls to garbage collection are issued.\n"
	    "If @var{flag} is a number, cell access checking is enabled,\n"
	    "with an additional garbage collection after the given\n"
	    "number of cell accesses.\n"
	    "This procedure only exists when the compile-time flag\n"
	    "@code{SCM_DEBUG_CELL_ACCESSES} was set to 1.")
#define FUNC_NAME s_scm_set_debug_cell_accesses_x
{
  if (SCM_FALSEP (flag)) {
    scm_debug_cell_accesses_p = 0;
  } else if (SCM_EQ_P (flag, SCM_BOOL_T)) {
    debug_cells_gc_interval = 0;
    scm_debug_cell_accesses_p = 1;
  } else if (SCM_INUMP (flag)) {
    long int f = SCM_INUM (flag);
    if (f <= 0) SCM_OUT_OF_RANGE (1, flag);
    debug_cells_gc_interval = f;
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
 * heap is needed.  SCM_HEAP_SEG_SIZE must fit into type size_t.  This code
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
 * SCM_MTRIGGER_HYSTERESIS is the amount of malloc storage that must
 * be reclaimed by a GC triggered by a malloc. If less than this is
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
size_t scm_default_init_heap_size_1 = (((SCM_DATA_CELLS2CARDS (45000L) + SCM_CARDS_PER_CLUSTER - 1)
                                     / SCM_CARDS_PER_CLUSTER) * SCM_GC_CARD_SIZE);
int scm_default_min_yield_1 = 40;

#define SCM_CLUSTER_SIZE_2 (SCM_CARDS_PER_CLUSTER * (SCM_GC_CARD_N_DATA_CELLS / 2))
size_t scm_default_init_heap_size_2 = (((SCM_DATA_CELLS2CARDS (2500L * 2) + SCM_CARDS_PER_CLUSTER - 1)
                                     / SCM_CARDS_PER_CLUSTER) * SCM_GC_CARD_SIZE);
/* The following value may seem large, but note that if we get to GC at
 * all, this means that we have a numerically intensive application
 */
int scm_default_min_yield_2 = 40;

size_t scm_default_max_segment_size = 2097000L;/* a little less (adm) than 2 Mb */

#define SCM_MIN_HEAP_SEG_SIZE (8 * SCM_GC_CARD_SIZE)
#ifdef _QC
# define SCM_HEAP_SEG_SIZE 32768L
#else
# ifdef sequent
#  define SCM_HEAP_SEG_SIZE (7000L * sizeof (scm_t_cell))
# else
#  define SCM_HEAP_SEG_SIZE (16384L * sizeof (scm_t_cell))
# endif
#endif
/* Make heap grow with factor 1.5 */
#define SCM_EXPHEAP(scm_heap_size) (scm_heap_size / 2)
#define SCM_INIT_MALLOC_LIMIT 100000
#define SCM_MTRIGGER_HYSTERESIS (SCM_INIT_MALLOC_LIMIT/10)

/* CELL_UP and CELL_DN are used by scm_init_heap_seg to find (scm_t_cell * span)
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
#  define CELL_UP(p, span) (SCM_CELLPTR)(~(sizeof(scm_t_cell)*(span)-1L) & ((long)(p)+sizeof(scm_t_cell)*(span)-1L))
#  define CELL_DN(p, span) (SCM_CELLPTR)(~(sizeof(scm_t_cell)*(span)-1L) & (long)(p))
# endif				/* UNICOS */
#endif				/* PROT386 */

#define DOUBLECELL_ALIGNED_P(x)  (((2 * sizeof (scm_t_cell) - 1) & SCM_UNPACK (x)) == 0)

#define ALIGNMENT_SLACK(freelist) (SCM_GC_CARD_SIZE - 1)
#define CLUSTER_SIZE_IN_BYTES(freelist) \
    (((freelist)->cluster_size / (SCM_GC_CARD_N_DATA_CELLS / (freelist)->span)) * SCM_GC_CARD_SIZE)


/* scm_freelists
 */

typedef struct scm_t_freelist {
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
  unsigned int cluster_size;
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
  unsigned long collected;
  /* number of collected cells during penultimate GC */
  unsigned long collected_1;
  /* total number of cells in heap segments
   * belonging to this list.
   */
  unsigned long heap_size;
} scm_t_freelist;

SCM scm_freelist = SCM_EOL;
scm_t_freelist scm_master_freelist = {
  SCM_EOL, 0, 0, SCM_EOL, 0, SCM_CLUSTER_SIZE_1, 0, 0, 0, 1, 0, 0, 0
};
SCM scm_freelist2 = SCM_EOL;
scm_t_freelist scm_master_freelist2 = {
  SCM_EOL, 0, 0, SCM_EOL, 0, SCM_CLUSTER_SIZE_2, 0, 0, 0, 2, 0, 0, 0
};

/* scm_mtrigger
 * is the number of bytes of malloc allocation needed to trigger gc.
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
unsigned long scm_mallocated = 0;
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

typedef struct scm_t_heap_seg_data
{
  /* lower and upper bounds of the segment */
  SCM_CELLPTR bounds[2];

  /* address of the head-of-freelist pointer for this segment's cells.
     All segments usually point to the same one, scm_freelist.  */
  scm_t_freelist *freelist;

  /* number of cells per object in this segment */
  int span;
} scm_t_heap_seg_data;



static size_t init_heap_seg (SCM_CELLPTR, size_t, scm_t_freelist *);

typedef enum { return_on_error, abort_on_error } policy_on_error;
static void alloc_some_heap (scm_t_freelist *, policy_on_error);


#define SCM_HEAP_SIZE \
  (scm_master_freelist.heap_size + scm_master_freelist2.heap_size)
#define SCM_MAX(A, B) ((A) > (B) ? (A) : (B))

#define BVEC_GROW_SIZE  256
#define BVEC_GROW_SIZE_IN_LIMBS (SCM_GC_CARD_BVEC_SIZE_IN_LIMBS * BVEC_GROW_SIZE)
#define BVEC_GROW_SIZE_IN_BYTES (BVEC_GROW_SIZE_IN_LIMBS * sizeof (scm_t_c_bvec_limb))

/* mark space allocation */

typedef struct scm_t_mark_space
{
  scm_t_c_bvec_limb *bvec_space;
  struct scm_t_mark_space *next;
} scm_t_mark_space;

static scm_t_mark_space *current_mark_space;
static scm_t_mark_space **mark_space_ptr;
static ptrdiff_t current_mark_space_offset;
static scm_t_mark_space *mark_space_head;

static scm_t_c_bvec_limb *
get_bvec ()
#define FUNC_NAME "get_bvec"
{
  scm_t_c_bvec_limb *res;

  if (!current_mark_space)
    {
      SCM_SYSCALL (current_mark_space = (scm_t_mark_space *) malloc (sizeof (scm_t_mark_space)));
      if (!current_mark_space)
        SCM_MISC_ERROR ("could not grow heap", SCM_EOL);

      current_mark_space->bvec_space = NULL;
      current_mark_space->next = NULL;

      *mark_space_ptr = current_mark_space;
      mark_space_ptr = &(current_mark_space->next);

      return get_bvec ();
    }

  if (!(current_mark_space->bvec_space))
    {
      SCM_SYSCALL (current_mark_space->bvec_space =
                   (scm_t_c_bvec_limb *) calloc (BVEC_GROW_SIZE_IN_BYTES, 1));
      if (!(current_mark_space->bvec_space))
        SCM_MISC_ERROR ("could not grow heap", SCM_EOL);

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
#undef FUNC_NAME


static void
clear_mark_space ()
{
  scm_t_mark_space *ms;

  for (ms = mark_space_head; ms; ms = ms->next)
    memset (ms->bvec_space, 0, BVEC_GROW_SIZE_IN_BYTES);
}



/* Debugging functions.  */

#if defined (GUILE_DEBUG) || defined (GUILE_DEBUG_FREELIST)

static long int heap_segment (SCM obj); /* forw decl: non-debugging func */

static void
map_free_list (scm_t_freelist *master, SCM freelist)
{
  long last_seg = -1, count = 0;
  SCM f;

  for (f = freelist; !SCM_NULLP (f); f = SCM_FREE_CELL_CDR (f))
    {
      long int this_seg = heap_segment (f);

      if (this_seg == -1)
	{
	  fprintf (stderr,
		   "map_free_list: can't find segment containing cell %lux\n",
		   (unsigned long int) SCM_UNPACK (f));
	  abort ();
	}
      else if (this_seg != last_seg)
	{
	  if (last_seg != -1)
	    fprintf (stderr, "  %5ld %d-cells in segment %ld\n",
		     (long) count, master->span, (long) last_seg);
	  last_seg = this_seg;
	  count = 0;
	}
      count++;
    }
  if (last_seg != -1)
    fprintf (stderr, "  %5ld %d-cells in segment %ld\n",
	     (long) count, master->span, (long) last_seg);
}

SCM_DEFINE (scm_map_free_list, "map-free-list", 0, 0, 0,
            (),
	    "Print debugging information about the free-list.\n"
	    "@code{map-free-list} is only included in\n"
	    "@code{--enable-guile-debug} builds of Guile.")
#define FUNC_NAME s_scm_map_free_list
{
  size_t i;

  fprintf (stderr, "%ld segments total (%d:%ld",
	   (long) scm_n_heap_segs,
	   scm_heap_table[0].span,
	   (long) (scm_heap_table[0].bounds[1] - scm_heap_table[0].bounds[0]));

  for (i = 1; i != scm_n_heap_segs; i++)
    fprintf (stderr, ", %d:%ld",
	     scm_heap_table[i].span,
	     (long) (scm_heap_table[i].bounds[1] - scm_heap_table[i].bounds[0]));
  fprintf (stderr, ")\n");
  map_free_list (&scm_master_freelist, scm_freelist);
  map_free_list (&scm_master_freelist2, scm_freelist2);
  fflush (stderr);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static long last_cluster;
static long last_size;

static long
free_list_length (char *title, long i, SCM freelist)
{
  SCM ls;
  long n = 0;
  for (ls = freelist; !SCM_NULLP (ls); ls = SCM_FREE_CELL_CDR (ls))
    if (SCM_FREE_CELL_P (ls))
      ++n;
    else
      {
	fprintf (stderr, "bad cell in %s at position %ld\n", title, (long) n);
	abort ();
      }
  if (n != last_size)
    {
      if (i > 0)
	{
	  if (last_cluster == i - 1)
	    fprintf (stderr, "\t%ld\n", (long) last_size);
	  else
	    fprintf (stderr, "-%ld\t%ld\n", (long) (i - 1), (long) last_size);
	}
      if (i >= 0)
	fprintf (stderr, "%s %ld", title, (long) i);
      else
	fprintf (stderr, "%s\t%ld\n", title, (long) n);
      last_cluster = i;
      last_size = n;
    }
  return n;
}

static void
free_list_lengths (char *title, scm_t_freelist *master, SCM freelist)
{
  SCM clusters;
  long i = 0, len, n = 0;
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
    fprintf (stderr, "\t%ld\n", (long) last_size);
  else
    fprintf (stderr, "-%ld\t%ld\n", (long) (i - 1), (long) last_size);
  fprintf (stderr, "\ntotal %ld objects\n\n", (long) n);
}

SCM_DEFINE (scm_free_list_length, "free-list-length", 0, 0, 0,
            (),
	    "Print debugging information about the free-list.\n"
	    "@code{free-list-length} is only included in\n"
	    "@code{--enable-guile-debug} builds of Guile.")
#define FUNC_NAME s_scm_free_list_length
{
  free_list_lengths ("1-cells", &scm_master_freelist, scm_freelist);
  free_list_lengths ("2-cells", &scm_master_freelist2, scm_freelist2);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif /* defined (GUILE_DEBUG) || defined (GUILE_DEBUG_FREELIST) */

#ifdef GUILE_DEBUG_FREELIST

/* Non-zero if freelist debugging is in effect.  Set this via
   `gc-set-debug-check-freelist!'. */
static int scm_debug_check_freelist = 0;

/* Number of calls to SCM_NEWCELL since startup.  */
static unsigned long scm_newcell_count;
static unsigned long scm_newcell2_count;

/* Search freelist for anything that isn't marked as a free cell.
   Abort if we find something.  */
static void
scm_check_freelist (SCM freelist)
{
  SCM f;
  long i = 0;

  for (f = freelist; !SCM_NULLP (f); f = SCM_FREE_CELL_CDR (f), i++)
    if (!SCM_FREE_CELL_P (f))
      {
	fprintf (stderr, "Bad cell in freelist on newcell %lu: %lu'th elt\n",
		 (long) scm_newcell_count, (long) i);
	abort ();
      }
}

SCM_DEFINE (scm_gc_set_debug_check_freelist_x, "gc-set-debug-check-freelist!", 1, 0, 0,
            (SCM flag),
	    "If @var{flag} is @code{#t}, check the freelist for consistency\n"
	    "on each cell allocation.  This procedure only exists when the\n"
	    "@code{GUILE_DEBUG_FREELIST} compile-time flag was selected.")
#define FUNC_NAME s_scm_gc_set_debug_check_freelist_x
{
  /* [cmm] I did a double-take when I read this code the first time.
     well, FWIW. */
  SCM_VALIDATE_BOOL_COPY (1, flag, scm_debug_check_freelist);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif /* GUILE_DEBUG_FREELIST */



static unsigned long
master_cells_allocated (scm_t_freelist *master)
{
  /* the '- 1' below is to ignore the cluster spine cells. */
  long objects = master->clusters_allocated * (master->cluster_size - 1);
  if (SCM_NULLP (master->clusters))
    objects -= master->left_to_collect;
  return master->span * objects;
}

static unsigned long
freelist_length (SCM freelist)
{
  long n;
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
	    "Return an association list of statistics about Guile's current\n"
	    "use of storage.")
#define FUNC_NAME s_scm_gc_stats
{
  long i;
  long n;
  SCM heap_segs;
  unsigned long int local_scm_mtrigger;
  unsigned long int local_scm_mallocated;
  unsigned long int local_scm_heap_size;
  unsigned long int local_scm_cells_allocated;
  unsigned long int local_scm_gc_time_taken;
  unsigned long int local_scm_gc_times;
  unsigned long int local_scm_gc_mark_time_taken;
  unsigned long int local_scm_gc_sweep_time_taken;
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

  answer = scm_list_n (scm_cons (sym_gc_time_taken, scm_ulong2num (local_scm_gc_time_taken)),
		       scm_cons (sym_cells_allocated, scm_ulong2num (local_scm_cells_allocated)),
		       scm_cons (sym_heap_size, scm_ulong2num (local_scm_heap_size)),
		       scm_cons (sym_mallocated, scm_ulong2num (local_scm_mallocated)),
		       scm_cons (sym_mtrigger, scm_ulong2num (local_scm_mtrigger)),
		       scm_cons (sym_times, scm_ulong2num (local_scm_gc_times)),
		       scm_cons (sym_gc_mark_time_taken, scm_ulong2num (local_scm_gc_mark_time_taken)),
		       scm_cons (sym_gc_sweep_time_taken, scm_ulong2num (local_scm_gc_sweep_time_taken)),
		       scm_cons (sym_cells_marked, scm_i_dbl2big (local_scm_gc_cells_marked)),
		       scm_cons (sym_cells_swept, scm_i_dbl2big (local_scm_gc_cells_swept)),
		       scm_cons (sym_heap_segments, heap_segs),
		       SCM_UNDEFINED);
  SCM_ALLOW_INTS;
  return answer;
}
#undef FUNC_NAME


static void
gc_start_stats (const char *what SCM_UNUSED)
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
adjust_min_yield (scm_t_freelist *freelist)
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
		   - (long) SCM_MAX (scm_gc_yield_1, scm_gc_yield));
#ifdef DEBUGINFO
      fprintf (stderr, " after GC = %lu, delta = %ld\n",
	       (long) scm_cells_allocated,
	       (long) delta);
#endif
      if (delta > 0)
	freelist->min_yield += delta;
    }
}


/* When we get POSIX threads support, the master will be global and
 * common while the freelist will be individual for each thread.
 */

SCM
scm_gc_for_newcell (scm_t_freelist *master, SCM *freelist)
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
	      fprintf (stderr, "allocated = %lu, ",
		       (long) (scm_cells_allocated
		       + master_cells_allocated (&scm_master_freelist)
                       + master_cells_allocated (&scm_master_freelist2)));
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
scm_alloc_cluster (scm_t_freelist *master)
{
  SCM freelist, cell;
  cell = scm_gc_for_newcell (master, &freelist);
  SCM_SETCDR (cell, freelist);
  return cell;
}
#endif


scm_t_c_hook scm_before_gc_c_hook;
scm_t_c_hook scm_before_mark_c_hook;
scm_t_c_hook scm_before_sweep_c_hook;
scm_t_c_hook scm_after_sweep_c_hook;
scm_t_c_hook scm_after_gc_c_hook;

#ifdef __ia64__
# define SCM_MARK_BACKING_STORE() do {                                \
    ucontext_t ctx;                                                   \
    SCM_STACKITEM * top, * bot;                                       \
    getcontext (&ctx);                                                \
    scm_mark_locations ((SCM_STACKITEM *) &ctx.uc_mcontext,           \
      ((size_t) (sizeof (SCM_STACKITEM) - 1 + sizeof ctx.uc_mcontext) \
       / sizeof (SCM_STACKITEM)));                                    \
    bot = (SCM_STACKITEM *) __libc_ia64_register_backing_store_base;  \
    top = (SCM_STACKITEM *) ctx.uc_mcontext.sc_ar_bsp;                \
    scm_mark_locations (bot, top - bot); } while (0)
#else
# define SCM_MARK_BACKING_STORE()
#endif

void
scm_igc (const char *what)
{
  long j;

  ++scm_gc_running_p;
  scm_c_hook_run (&scm_before_gc_c_hook, 0);
#ifdef DEBUGINFO
  fprintf (stderr,
	   SCM_NULLP (scm_freelist)
	   ? "*"
	   : (SCM_NULLP (scm_freelist2) ? "o" : "m"));
#endif
  /* During the critical section, only the current thread may run. */
  SCM_CRITICAL_SECTION_START;

  if (!scm_stack_base || scm_block_gc)
    {
      --scm_gc_running_p;
      return;
    }

  gc_start_stats (what);

  if (scm_gc_heap_lock)
    /* We've invoked the collector while a GC is already in progress.
       That should never happen.  */
    abort ();

  ++scm_gc_heap_lock;

  scm_c_hook_run (&scm_before_mark_c_hook, 0);

  clear_mark_space ();

#ifndef USE_THREADS

  /* Mark objects on the C stack. */
  SCM_FLUSH_REGISTER_WINDOWS;
  /* This assumes that all registers are saved into the jmp_buf */
  setjmp (scm_save_regs_gc_mark);
  scm_mark_locations ((SCM_STACKITEM *) scm_save_regs_gc_mark,
		      (   (size_t) (sizeof (SCM_STACKITEM) - 1 +
				       sizeof scm_save_regs_gc_mark)
			  / sizeof (SCM_STACKITEM)));

  {
    unsigned long stack_len = scm_stack_size (scm_stack_base);
#ifdef SCM_STACK_GROWS_UP
    scm_mark_locations (scm_stack_base, stack_len);
#else
    scm_mark_locations (scm_stack_base - stack_len, stack_len);
#endif
  }
  SCM_MARK_BACKING_STORE();

#else /* USE_THREADS */

  /* Mark every thread's stack and registers */
  scm_threads_mark_stacks ();

#endif /* USE_THREADS */

  j = SCM_NUM_PROTECTS;
  while (j--)
    scm_gc_mark (scm_sys_protects[j]);

  /* mark the registered roots */
  {
    size_t i;
    for (i = 0; i < SCM_VECTOR_LENGTH (scm_gc_registered_roots); ++i) {
      SCM l = SCM_VELTS (scm_gc_registered_roots)[i];
      for (; !SCM_NULLP (l); l = SCM_CDR (l)) {
        SCM *p = (SCM *) (scm_num2long (SCM_CAAR (l), 0, NULL));
        scm_gc_mark (*p);
      }
    }
  }

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

  SCM_CRITICAL_SECTION_END;
  scm_c_hook_run (&scm_after_gc_c_hook, 0);
  --scm_gc_running_p;
}



/* {Mark/Sweep}
 */

#define MARK scm_gc_mark
#define FNAME "scm_gc_mark"

#endif /*!MARK_DEPENDENCIES*/

/* Mark an object precisely.
 */
void
MARK (SCM p)
#define FUNC_NAME FNAME
{
  register long i;
  register SCM ptr;
  scm_t_bits cell_type;

#ifndef MARK_DEPENDENCIES
# define RECURSE scm_gc_mark
#else
  /* go through the usual marking, but not for self-cycles. */
# define RECURSE(x) do { if ((x) != p) scm_gc_mark (x); } while (0)
#endif
  ptr = p;

#ifdef MARK_DEPENDENCIES
  goto gc_mark_loop_first_time;
#endif

/* A simple hack for debugging.  Chose the second branch to get a
   meaningful backtrace for crashes inside the GC.
*/
#if 1
#define goto_gc_mark_loop goto gc_mark_loop
#define goto_gc_mark_nimp goto gc_mark_nimp
#else
#define goto_gc_mark_loop RECURSE(ptr); return
#define goto_gc_mark_nimp RECURSE(ptr); return
#endif

gc_mark_loop:
  if (SCM_IMP (ptr))
    return;

gc_mark_nimp:

#ifdef MARK_DEPENDENCIES
  if (SCM_EQ_P (ptr, p))
    return;

  scm_gc_mark (ptr);
  return;

gc_mark_loop_first_time:
#endif

#if (SCM_DEBUG_CELL_ACCESSES == 1) || (defined (GUILE_DEBUG_FREELIST))
  /* We are in debug mode.  Check the ptr exhaustively. */
  if (!scm_cellp (ptr))
    SCM_MISC_ERROR ("rogue pointer in heap", SCM_EOL);
#else
  /* In non-debug mode, do at least some cheap testing. */
  if (!CELL_P (ptr))
    SCM_MISC_ERROR ("rogue pointer in heap", SCM_EOL);
#endif

#ifndef MARK_DEPENDENCIES

  if (SCM_GCMARKP (ptr))
    return;

  SCM_SETGCMARK (ptr);

#endif

  cell_type = SCM_GC_CELL_TYPE (ptr);
  switch (SCM_ITAG7 (cell_type))
    {
    case scm_tcs_cons_nimcar:
      if (SCM_IMP (SCM_CDR (ptr)))
	{
	  ptr = SCM_CAR (ptr);
	  goto_gc_mark_nimp;
	}
      RECURSE (SCM_CAR (ptr));
      ptr = SCM_CDR (ptr);
      goto_gc_mark_nimp;
    case scm_tcs_cons_imcar:
      ptr = SCM_CDR (ptr);
      goto_gc_mark_loop;
    case scm_tc7_pws:
      RECURSE (SCM_SETTER (ptr));
      ptr = SCM_PROCEDURE (ptr);
      goto_gc_mark_loop;
    case scm_tcs_struct:
      {
	/* XXX - use less explicit code. */
	scm_t_bits word0 = SCM_CELL_WORD_0 (ptr) - scm_tc3_struct;
	scm_t_bits * vtable_data = (scm_t_bits *) word0;
	SCM layout = SCM_PACK (vtable_data [scm_vtable_index_layout]);
	long len = SCM_SYMBOL_LENGTH (layout);
	char * fields_desc = SCM_SYMBOL_CHARS (layout);
	scm_t_bits * struct_data = (scm_t_bits *) SCM_STRUCT_DATA (ptr);

	if (vtable_data[scm_struct_i_flags] & SCM_STRUCTF_ENTITY)
	  {
	    RECURSE (SCM_PACK (struct_data[scm_struct_i_procedure]));
	    RECURSE (SCM_PACK (struct_data[scm_struct_i_setter]));
	  }
	if (len)
	  {
	    long x;

	    for (x = 0; x < len - 2; x += 2, ++struct_data)
	      if (fields_desc[x] == 'p')
		RECURSE (SCM_PACK (*struct_data));
	    if (fields_desc[x] == 'p')
	      {
		if (SCM_LAYOUT_TAILP (fields_desc[x + 1]))
		  for (x = *struct_data++; x; --x, ++struct_data)
		    RECURSE (SCM_PACK (*struct_data));
		else
		  RECURSE (SCM_PACK (*struct_data));
	      }
	  }
	/* mark vtable */
	ptr = SCM_PACK (vtable_data [scm_vtable_index_vtable]);
	goto_gc_mark_loop;
      }
      break;
    case scm_tcs_closures:
      if (SCM_IMP (SCM_ENV (ptr)))
	{
	  ptr = SCM_CLOSCAR (ptr);
	  goto_gc_mark_nimp;
	}
      RECURSE (SCM_CLOSCAR (ptr));
      ptr = SCM_ENV (ptr);
      goto_gc_mark_nimp;
    case scm_tc7_vector:
      i = SCM_VECTOR_LENGTH (ptr);
      if (i == 0)
	break;
      while (--i > 0)
	if (SCM_NIMP (SCM_VELTS (ptr)[i]))
	  RECURSE (SCM_VELTS (ptr)[i]);
      ptr = SCM_VELTS (ptr)[0];
      goto_gc_mark_loop;
#ifdef CCLO
    case scm_tc7_cclo:
      {
	size_t i = SCM_CCLO_LENGTH (ptr);
	size_t j;
	for (j = 1; j != i; ++j)
	  {
	    SCM obj = SCM_CCLO_REF (ptr, j);
	    if (!SCM_IMP (obj))
	      RECURSE (obj);
	  }
	ptr = SCM_CCLO_REF (ptr, 0);
	goto_gc_mark_loop;
      }
#endif
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

    case scm_tc7_wvect:
      SCM_SET_WVECT_GC_CHAIN (ptr, scm_weak_vectors);
      scm_weak_vectors = ptr;
      if (SCM_IS_WHVEC_ANY (ptr))
	{
	  long x;
	  long len;
	  int weak_keys;
	  int weak_values;

	  len = SCM_VECTOR_LENGTH (ptr);
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
		    RECURSE (SCM_CAR (kvpair));
		  if (!weak_values)
		    RECURSE (SCM_CDR (kvpair));
		  alist = next_alist;
		}
	      if (SCM_NIMP (alist))
		RECURSE (alist);
	    }
	}
      break;

    case scm_tc7_symbol:
      ptr = SCM_PROP_SLOTS (ptr);
      goto_gc_mark_loop;
    case scm_tc7_variable:
      ptr = SCM_CELL_OBJECT_1 (ptr);
      goto_gc_mark_loop;
    case scm_tcs_subrs:
      break;
    case scm_tc7_port:
      i = SCM_PTOBNUM (ptr);
#if (SCM_DEBUG_CELL_ACCESSES == 1) || (defined (GUILE_DEBUG_FREELIST))
      if (!(i < scm_numptob))
	SCM_MISC_ERROR ("undefined port type", SCM_EOL);
#endif
      if (SCM_PTAB_ENTRY(ptr))
	RECURSE (SCM_FILENAME (ptr));
      if (scm_ptobs[i].mark)
	{
	  ptr = (scm_ptobs[i].mark) (ptr);
	  goto_gc_mark_loop;
	}
      else
	return;
      break;
    case scm_tc7_smob:
      switch (SCM_TYP16 (ptr))
	{ /* should be faster than going through scm_smobs */
	case scm_tc_free_cell:
	  /* We have detected a free cell.  This can happen if non-object data
	   * on the C stack points into guile's heap and is scanned during
	   * conservative marking.  */
	  break;
	case scm_tc16_big:
	case scm_tc16_real:
	case scm_tc16_complex:
	  break;
	default:
	  i = SCM_SMOBNUM (ptr);
#if (SCM_DEBUG_CELL_ACCESSES == 1) || (defined (GUILE_DEBUG_FREELIST))
	  if (!(i < scm_numsmob))
	    SCM_MISC_ERROR ("undefined smob type", SCM_EOL);
#endif
	  if (scm_smobs[i].mark)
	    {
	      ptr = (scm_smobs[i].mark) (ptr);
	      goto_gc_mark_loop;
	    }
	  else
	    return;
	}
      break;
    default:
      SCM_MISC_ERROR ("unknown type", SCM_EOL);
    }
#undef RECURSE
}
#undef FUNC_NAME

#ifndef MARK_DEPENDENCIES

#undef MARK
#undef FNAME

/* And here we define `scm_gc_mark_dependencies', by including this
 * same file in itself.
 */
#define MARK scm_gc_mark_dependencies
#define FNAME "scm_gc_mark_dependencies"
#define MARK_DEPENDENCIES
#include "gc.c"
#undef MARK_DEPENDENCIES
#undef MARK
#undef FNAME


/* Determine whether the given value does actually represent a cell in some
 * heap segment.  If this is the case, the number of the heap segment is
 * returned.  Otherwise, -1 is returned.  Binary search is used in order to
 * determine the heap segment that contains the cell.*/
/* FIXME:  To be used within scm_mark_locations and scm_cellp this function
 * should be an inline function.  */
static long int
heap_segment (SCM obj)
{
  if (!CELL_P (obj))
    return -1;
  else
    {
      SCM_CELLPTR ptr = SCM2PTR (obj);
      unsigned long int i = 0;
      unsigned long int j = scm_n_heap_segs - 1;

      if (SCM_PTR_LT (ptr, scm_heap_table[i].bounds[0]))
	return -1;
      else if (SCM_PTR_LE (scm_heap_table[j].bounds[1], ptr))
	return -1;
      else
	{
	  while (i < j)
	    {
	      if (SCM_PTR_LT (ptr, scm_heap_table[i].bounds[1]))
		{
		  break;
		}
	      else if (SCM_PTR_LE (scm_heap_table[j].bounds[0], ptr))
		{
		  i = j;
		  break;
		}
	      else
		{
		  unsigned long int k = (i + j) / 2;

		  if (k == i)
		    return -1;
		  else if (SCM_PTR_LT (ptr, scm_heap_table[k].bounds[1]))
		    {
		      j = k;
		      ++i;
		      if (SCM_PTR_LT (ptr, scm_heap_table[i].bounds[0]))
			return -1;
		    }
		  else if (SCM_PTR_LE (scm_heap_table[k].bounds[0], ptr))
		    {
		      i = k;
		      --j;
		      if (SCM_PTR_LE (scm_heap_table[j].bounds[1], ptr))
			return -1;
		    }
		}
	    }

	  if (!DOUBLECELL_ALIGNED_P (obj) && scm_heap_table[i].span == 2)
	    return -1;
	  else if (SCM_GC_IN_CARD_HEADERP (ptr))
	    return -1;
	  else
	    return i;
	}
    }
}


/* Mark a region conservatively */
void
scm_mark_locations (SCM_STACKITEM x[], unsigned long n)
{
  unsigned long m;

  for (m = 0; m < n; ++m)
    {
      SCM obj = * (SCM *) &x[m];
      long int segment = heap_segment (obj);
      if (segment >= 0)
	scm_gc_mark (obj);
    }
}


/* The function scm_cellp determines whether an SCM value can be regarded as a
 * pointer to a cell on the heap.
 */
int
scm_cellp (SCM value)
{
  long int segment = heap_segment (value);
  return (segment >= 0);
}


static void
gc_sweep_freelist_start (scm_t_freelist *freelist)
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
gc_sweep_freelist_finish (scm_t_freelist *freelist)
{
  long collected;
  *freelist->clustertail = freelist->cells;
  if (!SCM_NULLP (freelist->cells))
    {
      SCM c = freelist->cells;
      SCM_SET_CELL_WORD_0 (c, SCM_FREE_CELL_CDR (c));
      SCM_SET_CELL_WORD_1 (c, SCM_EOL);
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
      scm_t_cell *nxt__ = CELL_UP ((char *) (ptr) + 1, (span)); \
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
  register scm_t_freelist *freelist;
  register unsigned long m;
  register int span;
  size_t i;
  size_t seg_size;

  m = 0;

  gc_sweep_freelist_start (&scm_master_freelist);
  gc_sweep_freelist_start (&scm_master_freelist2);

  for (i = 0; i < scm_n_heap_segs; i++)
    {
      register long left_to_collect;
      register size_t j;

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
	    case scm_tcs_struct:
	      {
		/* Structs need to be freed in a special order.
		 * This is handled by GC C hooks in struct.c.
		 */
		SCM_SET_STRUCT_GC_CHAIN (scmptr, scm_structs_to_free);
		scm_structs_to_free = scmptr;
	      }
	      continue;
	    case scm_tcs_cons_imcar:
	    case scm_tcs_cons_nimcar:
	    case scm_tcs_closures:
	    case scm_tc7_pws:
	      break;
	    case scm_tc7_wvect:
	    case scm_tc7_vector:
	      {
		unsigned long int length = SCM_VECTOR_LENGTH (scmptr);
		if (length > 0)
		  {
		    scm_gc_free (SCM_VECTOR_BASE (scmptr),
				 length * sizeof (scm_t_bits),
				 "vector");
		  }
		break;
	      }
#ifdef CCLO
	    case scm_tc7_cclo:
	      scm_gc_free (SCM_CCLO_BASE (scmptr), 
			   SCM_CCLO_LENGTH (scmptr) * sizeof (SCM),
			   "compiled closure");
	      break;
#endif
#ifdef HAVE_ARRAYS
	    case scm_tc7_bvect:
	      {
		unsigned long int length = SCM_BITVECTOR_LENGTH (scmptr);
		if (length > 0)
		  {
		    scm_gc_free (SCM_BITVECTOR_BASE (scmptr),
				 (sizeof (long)
				  * ((length+SCM_LONG_BIT-1) / SCM_LONG_BIT)),
				 "vector");
		  }
	      }
	      break;
	    case scm_tc7_byvect:
	    case scm_tc7_ivect:
	    case scm_tc7_uvect:
	    case scm_tc7_svect:
#ifdef HAVE_LONG_LONGS
	    case scm_tc7_llvect:
#endif
	    case scm_tc7_fvect:
	    case scm_tc7_dvect:
	    case scm_tc7_cvect:
	      scm_gc_free (SCM_UVECTOR_BASE (scmptr), 
			   (SCM_UVECTOR_LENGTH (scmptr)
			    * scm_uniform_element_size (scmptr)),
			   "vector");
	      break;
#endif
	    case scm_tc7_string:
	      scm_gc_free (SCM_STRING_CHARS (scmptr), 
			   SCM_STRING_LENGTH (scmptr) + 1, "string");
	      break;
	    case scm_tc7_symbol:
	      scm_gc_free (SCM_SYMBOL_CHARS (scmptr), 
			   SCM_SYMBOL_LENGTH (scmptr) + 1, "symbol");
	      break;
            case scm_tc7_variable:
              break;
	    case scm_tcs_subrs:
              /* the various "subrs" (primitives) are never freed */
	      continue;
	    case scm_tc7_port:
	      if SCM_OPENP (scmptr)
		{
		  int k = SCM_PTOBNUM (scmptr);
		  size_t mm;
#if (SCM_DEBUG_CELL_ACCESSES == 1) || (defined (GUILE_DEBUG_FREELIST))
		  if (!(k < scm_numptob))
		    SCM_MISC_ERROR ("undefined port type", SCM_EOL);
#endif
		  /* Keep "revealed" ports alive.  */
		  if (scm_revealed_count (scmptr) > 0)
		    continue;
		  /* Yes, I really do mean scm_ptobs[k].free */
		  /* rather than ftobs[k].close.  .close */
		  /* is for explicit CLOSE-PORT by user */
		  mm = scm_ptobs[k].free (scmptr);

		  if (mm != 0)
		    {
#if SCM_ENABLE_DEPRECATED == 1
		      scm_c_issue_deprecation_warning
			("Returning non-0 from a port free function is "
			 "deprecated.  Use scm_gc_free et al instead.");
		      scm_c_issue_deprecation_warning_fmt
			("(You just returned non-0 while freeing a %s.)",
			 SCM_PTOBNAME (k));
		      m += mm;
#else
		      abort ();
#endif
		    }

		  SCM_SETSTREAM (scmptr, 0);
		  scm_remove_from_port_table (scmptr);
		  scm_gc_ports_collected++;
		  SCM_CLR_PORT_OPEN_FLAG (scmptr);
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
		  scm_gc_free (SCM_BDIGITS (scmptr),
			       ((SCM_NUMDIGS (scmptr) * SCM_BITSPERDIG
				 / SCM_CHAR_BIT)), "bignum");
		  break;
#endif /* def SCM_BIGDIG */
		case scm_tc16_complex:
		  scm_gc_free (SCM_COMPLEX_MEM (scmptr), 2*sizeof (double),
			       "complex");
		  break;
		default:
		  {
		    int k;
		    k = SCM_SMOBNUM (scmptr);
#if (SCM_DEBUG_CELL_ACCESSES == 1) || (defined (GUILE_DEBUG_FREELIST))
		    if (!(k < scm_numsmob))
		      SCM_MISC_ERROR ("undefined smob type", SCM_EOL);
#endif
		    if (scm_smobs[k].free)
		      {
			size_t mm;
			mm = scm_smobs[k].free (scmptr);
			if (mm != 0)
			  {
#if SCM_ENABLE_DEPRECATED == 1
			    scm_c_issue_deprecation_warning
			      ("Returning non-0 from a smob free function is "
			       "deprecated.  Use scm_gc_free et al instead.");
			    scm_c_issue_deprecation_warning_fmt
			      ("(You just returned non-0 while freeing a %s.)",
			       SCM_SMOBNAME (k));
			    m += mm;
#else
			    abort();
#endif
			  }
		      }
		    break;
		  }
		}
	      break;
	    default:
	      SCM_MISC_ERROR ("unknown type", SCM_EOL);
	    }

	  if (!--left_to_collect)
	    {
	      SCM_SET_CELL_WORD_0 (scmptr, nfreelist);
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

  if (scm_mallocated < m)
    {
      /* The byte count of allocated objects has underflowed.  This is
	 probably because you forgot to report the sizes of objects you
	 have allocated, by calling scm_done_malloc or some such.  When
	 the GC freed them, it subtracted their size from
	 scm_mallocated, which underflowed.  */
      fprintf (stderr,
	       "scm_gc_sweep: Byte count of allocated objects has underflowed.\n"
	       "This is probably because the GC hasn't been correctly informed\n"
	       "about object sizes\n");
      abort ();
    }

  scm_mallocated -= m;
  scm_gc_malloc_collected = m;
}
#undef FUNC_NAME



/* Function for non-cell memory management.
 */

void *
scm_malloc (size_t size)
{
  void *ptr;

  if (size == 0)
    return NULL;

  SCM_SYSCALL (ptr = malloc (size));
  if (ptr)
    return ptr;

  scm_igc ("malloc");
  SCM_SYSCALL (ptr = malloc (size));
  if (ptr)
    return ptr;

  scm_memory_error ("malloc");
}

void *
scm_realloc (void *mem, size_t size)
{
  void *ptr;

  SCM_SYSCALL (ptr = realloc (mem, size));
  if (ptr)
    return ptr;

  scm_igc ("realloc");
  SCM_SYSCALL (ptr = realloc (mem, size));
  if (ptr)
    return ptr;

  scm_memory_error ("realloc");
}

char *
scm_strndup (const char *str, size_t n)
{
  char *dst = scm_malloc (n+1);
  memcpy (dst, str, n);
  dst[n] = 0;
  return dst;
}

char *
scm_strdup (const char *str)
{
  return scm_strndup (str, strlen (str));
}

void
scm_gc_register_collectable_memory (void *mem, size_t size, const char *what)
{
  scm_mallocated += size;

  if (scm_mallocated > scm_mtrigger)
    {
      scm_igc (what);
      if (scm_mallocated > scm_mtrigger - SCM_MTRIGGER_HYSTERESIS)
	{
	  if (scm_mallocated > scm_mtrigger)
	    scm_mtrigger = scm_mallocated + scm_mallocated / 2;
	  else
	    scm_mtrigger += scm_mtrigger / 2;
	}
    }

#ifdef GUILE_DEBUG_MALLOC
  if (mem)
    scm_malloc_register (mem, what);
#endif
}

void
scm_gc_unregister_collectable_memory (void *mem, size_t size, const char *what)
{
  scm_mallocated -= size;

#ifdef GUILE_DEBUG_MALLOC
  if (mem)
    scm_malloc_unregister (mem);
#endif
}

void *
scm_gc_malloc (size_t size, const char *what)
{
  /* XXX - The straightforward implementation below has the problem
     that it might call the GC twice, once in scm_malloc and then
     again in scm_gc_register_collectable_memory.  We don't really
     want the second GC since it will not find new garbage.
  */

  void *ptr = scm_malloc (size);
  scm_gc_register_collectable_memory (ptr, size, what);
  return ptr;
}

void *
scm_gc_realloc (void *mem, size_t old_size, size_t new_size, const char *what)
{
  /* XXX - see scm_gc_malloc. */

  void *ptr = scm_realloc (mem, new_size);
  scm_gc_unregister_collectable_memory (mem, old_size, what);
  scm_gc_register_collectable_memory (ptr, new_size, what);
  return ptr;
}

void
scm_gc_free (void *mem, size_t size, const char *what)
{
  scm_gc_unregister_collectable_memory (mem, size, what);
  free (mem);
}

char *
scm_gc_strndup (const char *str, size_t n, const char *what)
{
  char *dst = scm_gc_malloc (n+1, what);
  memcpy (dst, str, n);
  dst[n] = 0;
  return dst;
}

char *
scm_gc_strdup (const char *str, const char *what)
{
  return scm_gc_strndup (str, strlen (str), what);
}

#if SCM_ENABLE_DEPRECATED == 1

/* {Deprecated front end to malloc}
 *
 * scm_must_malloc, scm_must_realloc, scm_must_free, scm_done_malloc,
 * scm_done_free
 *
 * These functions provide services comparable to malloc, realloc, and
 * free.  They should be used when allocating memory that will be under
 * control of the garbage collector, i.e., if the memory may be freed
 * during garbage collection.
 *
 * They are deprecated because they weren't really used the way
 * outlined above, and making sure to return the right amount from
 * smob free routines was sometimes difficult when dealing with nested
 * data structures.  We basically want everybody to review their code
 * and use the more symmetrical scm_gc_malloc/scm_gc_free functions
 * instead.  In some cases, where scm_must_malloc has been used
 * incorrectly (i.e. for non-GC-able memory), use scm_malloc/free.
 */

void *
scm_must_malloc (size_t size, const char *what)
{
  scm_c_issue_deprecation_warning
    ("scm_must_malloc is deprecated.  "
     "Use scm_gc_malloc and scm_gc_free instead.");

  return scm_gc_malloc (size, what);
}

void *
scm_must_realloc (void *where,
		  size_t old_size,
		  size_t size,
		  const char *what)
{
  scm_c_issue_deprecation_warning
    ("scm_must_realloc is deprecated.  "
     "Use scm_gc_realloc and scm_gc_free instead.");

  return scm_gc_realloc (where, old_size, size, what);
}

char *
scm_must_strndup (const char *str, size_t length)
{
  scm_c_issue_deprecation_warning
    ("scm_must_strndup is deprecated.  "
     "Use scm_gc_strndup and scm_gc_free instead.");

  return scm_gc_strndup (str, length, "string");
}

char *
scm_must_strdup (const char *str)
{
  scm_c_issue_deprecation_warning
    ("scm_must_strdup is deprecated.  "
     "Use scm_gc_strdup and scm_gc_free instead.");

  return scm_gc_strdup (str, "string");
}

void
scm_must_free (void *obj)
#define FUNC_NAME "scm_must_free"
{
  scm_c_issue_deprecation_warning
    ("scm_must_free is deprecated.  "
     "Use scm_gc_malloc and scm_gc_free instead.");

#ifdef GUILE_DEBUG_MALLOC
  scm_malloc_unregister (obj);
#endif
  if (obj)
    free (obj);
  else
    SCM_MISC_ERROR ("freeing NULL pointer", SCM_EOL);
}
#undef FUNC_NAME


void
scm_done_malloc (long size)
{
  scm_c_issue_deprecation_warning
    ("scm_done_malloc is deprecated.  "
     "Use scm_gc_register_collectable_memory instead.");

  scm_gc_register_collectable_memory (NULL, size, "foreign mallocs");
}

void
scm_done_free (long size)
{
  scm_c_issue_deprecation_warning
    ("scm_done_free is deprecated.  "
     "Use scm_gc_unregister_collectable_memory instead.");

  scm_gc_unregister_collectable_memory (NULL, size, "foreign mallocs");
}

#endif /* SCM_ENABLE_DEPRECATED == 1 */


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

size_t scm_max_segment_size;

/* scm_heap_org
 * is the lowest base address of any heap segment.
 */
SCM_CELLPTR scm_heap_org;

scm_t_heap_seg_data * scm_heap_table = 0;
static size_t heap_segment_table_size = 0;
size_t scm_n_heap_segs = 0;

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
      SCM_GC_SET_CARD_BVEC (card, get_bvec ()); \
      if ((span) == 2) \
        SCM_GC_SET_CARD_DOUBLECELL (card); \
    } while (0)

static size_t
init_heap_seg (SCM_CELLPTR seg_org, size_t size, scm_t_freelist *freelist)
{
  register SCM_CELLPTR ptr;
  SCM_CELLPTR seg_end;
  size_t new_seg_index;
  ptrdiff_t n_new_cells;
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
   */
  new_seg_index = 0;
  while (new_seg_index < scm_n_heap_segs
	 && SCM_PTR_LE (scm_heap_table[new_seg_index].bounds[0], seg_org))
    new_seg_index++;

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
        scm_t_cell *nxt = ptr;
        scm_t_cell *prv = NULL;
        scm_t_cell *last_card = NULL;
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
            scm_t_cell *card = SCM_GC_CELL_CARD (ptr);
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
	    SCM_SET_FREE_CELL_CDR (scmptr, PTR2SCM (nxt));

            ptr = nxt;
	  }

	SCM_SET_FREE_CELL_CDR (PTR2SCM (prv), SCM_EOL);
      }

    /* sanity check */
    {
      scm_t_cell *ref = seg_end;
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

static size_t
round_to_cluster_size (scm_t_freelist *freelist, size_t len)
{
  size_t cluster_size_in_bytes = CLUSTER_SIZE_IN_BYTES (freelist);

  return
    (len + cluster_size_in_bytes - 1) / cluster_size_in_bytes * cluster_size_in_bytes
    + ALIGNMENT_SLACK (freelist);
}

static void
alloc_some_heap (scm_t_freelist *freelist, policy_on_error error_policy)
#define FUNC_NAME "alloc_some_heap"
{
  SCM_CELLPTR ptr;
  size_t len;

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
      size_t new_table_size = scm_n_heap_segs + 1;
      size_t size = new_table_size * sizeof (scm_t_heap_seg_data);
      scm_t_heap_seg_data *new_heap_table;

      SCM_SYSCALL (new_heap_table = ((scm_t_heap_seg_data *)
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
    unsigned long h = SCM_HEAP_SIZE;
    size_t min_cells = (f * h - 100 * (long) scm_gc_yield) / (99 - f);
    len =  SCM_EXPHEAP (freelist->heap_size);
#ifdef DEBUGINFO
    fprintf (stderr, "(%ld < %ld)", (long) len, (long) min_cells);
#endif
    if (len < min_cells)
      len = min_cells + freelist->cluster_size;
    len *= sizeof (scm_t_cell);
    /* force new sampling */
    freelist->collected = LONG_MAX;
  }

  if (len > scm_max_segment_size)
    len = scm_max_segment_size;

  {
    size_t smallest;

    smallest = CLUSTER_SIZE_IN_BYTES (freelist);

    if (len < smallest)
      len = smallest;

    /* Allocate with decaying ambition. */
    while ((len >= SCM_MIN_HEAP_SEG_SIZE)
	   && (len >= smallest))
      {
        size_t rounded_len = round_to_cluster_size (freelist, len);
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


/* {GC Protection Helper Functions}
 */


/*
 * If within a function you need to protect one or more scheme objects from
 * garbage collection, pass them as parameters to one of the
 * scm_remember_upto_here* functions below.  These functions don't do
 * anything, but since the compiler does not know that they are actually
 * no-ops, it will generate code that calls these functions with the given
 * parameters.  Therefore, you can be sure that the compiler will keep those
 * scheme values alive (on the stack or in a register) up to the point where
 * scm_remember_upto_here* is called.  In other words, place the call to
 * scm_remember_upto_here* _behind_ the last code in your function, that
 * depends on the scheme object to exist.
 *
 * Example: We want to make sure that the string object str does not get
 * garbage collected during the execution of 'some_function' in the code
 * below, because otherwise the characters belonging to str would be freed and
 * 'some_function' might access freed memory.  To make sure that the compiler
 * keeps str alive on the stack or in a register such that it is visible to
 * the conservative gc we add the call to scm_remember_upto_here_1 _after_ the
 * call to 'some_function'.  Note that this would not be necessary if str was
 * used anyway after the call to 'some_function'.
 *   char *chars = SCM_STRING_CHARS (str);
 *   some_function (chars);
 *   scm_remember_upto_here_1 (str);  // str will be alive up to this point.
 */

void
scm_remember_upto_here_1 (SCM obj SCM_UNUSED)
{
  /* Empty.  Protects a single object from garbage collection. */
}

void
scm_remember_upto_here_2 (SCM obj1 SCM_UNUSED, SCM obj2 SCM_UNUSED)
{
  /* Empty.  Protects two objects from garbage collection. */
}

void
scm_remember_upto_here (SCM obj SCM_UNUSED, ...)
{
  /* Empty.  Protects any number of objects from garbage collection. */
}

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
   scm_gc_unprotect_object (OBJ).  Calls to scm_gc_protect/unprotect_object nest,
   i. e. it is possible to protect the same object several times, but it is
   necessary to unprotect the object the same number of times to actually get
   the object unprotected.  It is an error to unprotect an object more often
   than it has been protected before.  The function scm_protect_object returns
   OBJ.
*/

/* Implementation note:  For every object X, there is a counter which
   scm_gc_protect_object(X) increments and scm_gc_unprotect_object(X) decrements.
*/

SCM
scm_gc_protect_object (SCM obj)
{
  SCM handle;

  /* This critical section barrier will be replaced by a mutex. */
  SCM_REDEFER_INTS;

  handle = scm_hashq_create_handle_x (scm_protects, obj, SCM_MAKINUM (0));
  SCM_SETCDR (handle, scm_sum (SCM_CDR (handle), SCM_MAKINUM (1)));

  SCM_REALLOW_INTS;

  return obj;
}


/* Remove any protection for OBJ established by a prior call to
   scm_protect_object.  This function returns OBJ.

   See scm_protect_object for more information.  */
SCM
scm_gc_unprotect_object (SCM obj)
{
  SCM handle;

  /* This critical section barrier will be replaced by a mutex. */
  SCM_REDEFER_INTS;

  handle = scm_hashq_get_handle (scm_protects, obj);

  if (SCM_FALSEP (handle))
    {
      fprintf (stderr, "scm_unprotect_object called on unprotected object\n");
      abort ();
    }
  else
    {
      SCM count = scm_difference (SCM_CDR (handle), SCM_MAKINUM (1));
      if (SCM_EQ_P (count, SCM_MAKINUM (0)))
	scm_hashq_remove_x (scm_protects, obj);
      else
	SCM_SETCDR (handle, count);
    }

  SCM_REALLOW_INTS;

  return obj;
}

void
scm_gc_register_root (SCM *p)
{
  SCM handle;
  SCM key = scm_long2num ((long) p);

  /* This critical section barrier will be replaced by a mutex. */
  SCM_REDEFER_INTS;

  handle = scm_hashv_create_handle_x (scm_gc_registered_roots, key, SCM_MAKINUM (0));
  SCM_SETCDR (handle, scm_sum (SCM_CDR (handle), SCM_MAKINUM (1)));

  SCM_REALLOW_INTS;
}

void
scm_gc_unregister_root (SCM *p)
{
  SCM handle;
  SCM key = scm_long2num ((long) p);

  /* This critical section barrier will be replaced by a mutex. */
  SCM_REDEFER_INTS;

  handle = scm_hashv_get_handle (scm_gc_registered_roots, key);

  if (SCM_FALSEP (handle))
    {
      fprintf (stderr, "scm_gc_unregister_root called on unregistered root\n");
      abort ();
    }
  else
    {
      SCM count = scm_difference (SCM_CDR (handle), SCM_MAKINUM (1));
      if (SCM_EQ_P (count, SCM_MAKINUM (0)))
	scm_hashv_remove_x (scm_gc_registered_roots, key);
      else
	SCM_SETCDR (handle, count);
    }

  SCM_REALLOW_INTS;
}

void
scm_gc_register_roots (SCM *b, unsigned long n)
{
  SCM *p = b;
  for (; p < b + n; ++p)
    scm_gc_register_root (p);
}

void
scm_gc_unregister_roots (SCM *b, unsigned long n)
{
  SCM *p = b;
  for (; p < b + n; ++p)
    scm_gc_unregister_root (p);
}

int scm_i_terminating;

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
  scm_i_terminating = 1;
  scm_flush_all_ports ();
}


static int
make_initial_segment (size_t init_heap_size, scm_t_freelist *freelist)
{
  size_t rounded_size = round_to_cluster_size (freelist, init_heap_size);

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
init_freelist (scm_t_freelist *freelist,
	       int span,
	       long cluster_size,
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


/* Get an integer from an environment variable.  */
static int
scm_i_getenv_int (const char *var, int def)
{
  char *end, *val = getenv (var);
  long res;
  if (!val)
    return def;
  res = strtol (val, &end, 10);
  if (end == val)
    return def;
  return res;
}


int
scm_init_storage ()
{
  unsigned long gc_trigger_1;
  unsigned long gc_trigger_2;
  size_t init_heap_size_1;
  size_t init_heap_size_2;
  size_t j;

  j = SCM_NUM_PROTECTS;
  while (j)
    scm_sys_protects[--j] = SCM_BOOL_F;
  scm_block_gc = 1;

  scm_freelist = SCM_EOL;
  scm_freelist2 = SCM_EOL;
  gc_trigger_1 = scm_i_getenv_int ("GUILE_MIN_YIELD_1", scm_default_min_yield_1);
  init_freelist (&scm_master_freelist, 1, SCM_CLUSTER_SIZE_1, gc_trigger_1);
  gc_trigger_2 = scm_i_getenv_int ("GUILE_MIN_YIELD_2", scm_default_min_yield_2);
  init_freelist (&scm_master_freelist2, 2, SCM_CLUSTER_SIZE_2, gc_trigger_2);
  scm_max_segment_size = scm_i_getenv_int ("GUILE_MAX_SEGMENT_SIZE", scm_default_max_segment_size);

  scm_expmem = 0;

  j = SCM_HEAP_SEG_SIZE;
  scm_mtrigger = SCM_INIT_MALLOC_LIMIT;
  scm_heap_table = ((scm_t_heap_seg_data *)
		    scm_malloc (sizeof (scm_t_heap_seg_data) * 2));
  heap_segment_table_size = 2;

  mark_space_ptr = &mark_space_head;

  init_heap_size_1 = scm_i_getenv_int ("GUILE_INIT_SEGMENT_SIZE_1", scm_default_init_heap_size_1);
  init_heap_size_2 = scm_i_getenv_int ("GUILE_INIT_SEGMENT_SIZE_2", scm_default_init_heap_size_2);
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
  scm_port_table = (scm_t_port **)
    malloc (sizeof (scm_t_port *) * scm_port_table_room);
  if (!scm_port_table)
    return 1;

#ifdef HAVE_ATEXIT
  atexit (cleanup);
#else
#ifdef HAVE_ON_EXIT
  on_exit (cleanup, 0);
#endif
#endif

  scm_stand_in_procs = SCM_EOL;
  scm_permobjs = SCM_EOL;
  scm_protects = scm_c_make_hash_table (31);
  scm_gc_registered_roots = scm_c_make_hash_table (31);

  return 0;
}



SCM scm_after_gc_hook;

static SCM gc_async;

/* The function gc_async_thunk causes the execution of the after-gc-hook.  It
 * is run after the gc, as soon as the asynchronous events are handled by the
 * evaluator.
 */
static SCM
gc_async_thunk (void)
{
  scm_c_run_hook (scm_after_gc_hook, SCM_EOL);
  return SCM_UNSPECIFIED;
}


/* The function mark_gc_async is run by the scm_after_gc_c_hook at the end of
 * the garbage collection.  The only purpose of this function is to mark the
 * gc_async (which will eventually lead to the execution of the
 * gc_async_thunk).
 */
static void *
mark_gc_async (void * hook_data SCM_UNUSED,
	       void *func_data SCM_UNUSED,
	       void *data SCM_UNUSED)
{
  /* If cell access debugging is enabled, the user may choose to perform
   * additional garbage collections after an arbitrary number of cell
   * accesses.  We don't want the scheme level after-gc-hook to be performed
   * for each of these garbage collections for the following reason: The
   * execution of the after-gc-hook causes cell accesses itself.  Thus, if the
   * after-gc-hook was performed with every gc, and if the gc was performed
   * after a very small number of cell accesses, then the number of cell
   * accesses during the execution of the after-gc-hook will suffice to cause
   * the execution of the next gc.  Then, guile would keep executing the
   * after-gc-hook over and over again, and would never come to do other
   * things.
   *
   * To overcome this problem, if cell access debugging with additional
   * garbage collections is enabled, the after-gc-hook is never run by the
   * garbage collecter.  When running guile with cell access debugging and the
   * execution of the after-gc-hook is desired, then it is necessary to run
   * the hook explicitly from the user code.  This has the effect, that from
   * the scheme level point of view it seems that garbage collection is
   * performed with a much lower frequency than it actually is.  Obviously,
   * this will not work for code that depends on a fixed one to one
   * relationship between the execution counts of the C level garbage
   * collection hooks and the execution count of the scheme level
   * after-gc-hook.
   */
#if (SCM_DEBUG_CELL_ACCESSES == 1)
  if (debug_cells_gc_interval == 0)
    scm_system_async_mark (gc_async);
#else
  scm_system_async_mark (gc_async);
#endif

  return NULL;
}

#if SCM_ENABLE_DEPRECATED == 1

/* If an allocated cell is detected during garbage collection, this
 * means that some code has just obtained the object but was preempted
 * before the initialization of the object was completed.  This meanst
 * that some entries of the allocated cell may already contain SCM
 * objects.  Therefore, allocated cells are scanned conservatively.
 */

scm_t_bits scm_tc16_allocated;

static SCM
allocated_mark (SCM cell)
{
  unsigned long int cell_segment = heap_segment (cell);
  unsigned int span = scm_heap_table[cell_segment].span;
  unsigned int i;

  for (i = 1; i != span * 2; ++i)
    {
      SCM obj = SCM_CELL_OBJECT (cell, i);
      long int obj_segment = heap_segment (obj);
      if (obj_segment >= 0)
	scm_gc_mark (obj);
    }
  return SCM_BOOL_F;
}

SCM
scm_deprecated_newcell (void)
{
  scm_c_issue_deprecation_warning 
    ("SCM_NEWCELL is deprecated.  Use `scm_cell' instead.\n");

  return scm_cell (scm_tc16_allocated, 0);
}

SCM
scm_deprecated_newcell2 (void)
{
  scm_c_issue_deprecation_warning 
    ("SCM_NEWCELL2 is deprecated.  Use `scm_double_cell' instead.\n");

  return scm_double_cell (scm_tc16_allocated, 0, 0, 0);
}

#endif /* SCM_ENABLE_DEPRECATED == 1 */

void
scm_init_gc ()
{
  SCM after_gc_thunk;

#if SCM_ENABLE_DEPRECATED == 1
  scm_tc16_allocated = scm_make_smob_type ("allocated cell", 0);
  scm_set_smob_mark (scm_tc16_allocated, allocated_mark);
#endif

  scm_after_gc_hook = scm_permanent_object (scm_make_hook (SCM_INUM0));
  scm_c_define ("after-gc-hook", scm_after_gc_hook);

  after_gc_thunk = scm_c_make_subr ("%gc-thunk", scm_tc7_subr_0,
				    gc_async_thunk);
  gc_async = scm_system_async (after_gc_thunk);  /* protected via scm_asyncs */

  scm_c_hook_add (&scm_after_gc_c_hook, mark_gc_async, NULL, 0);

#ifndef SCM_MAGIC_SNARFER
#include "libguile/gc.x"
#endif
}

#endif /*MARK_DEPENDENCIES*/

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

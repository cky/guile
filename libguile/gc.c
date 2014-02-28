/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002, 2003, 2006,
 *   2008, 2009, 2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.
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

/* #define DEBUGINFO */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#define SCM_BUILDING_DEPRECATED_CODE

#include "libguile/gen-scmconfig.h"

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

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
#include "libguile/arrays.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/weaks.h"
#include "libguile/hashtab.h"
#include "libguile/tags.h"

#include "libguile/private-gc.h"
#include "libguile/validate.h"
#include "libguile/deprecation.h"
#include "libguile/gc.h"
#include "libguile/dynwind.h"

#include "libguile/bdw-gc.h"

/* For GC_set_start_callback.  */
#include <gc/gc_mark.h>

#ifdef GUILE_DEBUG_MALLOC
#include "libguile/debug-malloc.h"
#endif

#include <unistd.h>

/* Set this to != 0 if every cell that is accessed shall be checked:
 */
int scm_debug_cell_accesses_p = 0;
int scm_expensive_debug_cell_accesses_p = 0;

/* Set this to 0 if no additional gc's shall be performed, otherwise set it to
 * the number of cell accesses after which a gc shall be called.
 */
int scm_debug_cells_gc_interval = 0;

#if SCM_ENABLE_DEPRECATED == 1
/* Hash table that keeps a reference to objects the user wants to protect from
   garbage collection.  It could arguably be private but applications have come
   to rely on it (e.g., Lilypond 2.13.9).  */
SCM scm_protects;
#else
static SCM scm_protects;
#endif

#if (SCM_DEBUG_CELL_ACCESSES == 1)


/*
  
  Assert that the given object is a valid reference to a valid cell.  This
  test involves to determine whether the object is a cell pointer, whether
  this pointer actually points into a heap segment and whether the cell
  pointed to is not a free cell.  Further, additional garbage collections may
  get executed after a user defined number of cell accesses.  This helps to
  find places in the C code where references are dropped for extremely short
  periods.

*/
void
scm_i_expensive_validation_check (SCM cell)
{
  /* If desired, perform additional garbage collections after a user
   * defined number of cell accesses.
   */
  if (scm_debug_cells_gc_interval)
    {
      static unsigned int counter = 0;

      if (counter != 0)
	{
	  --counter;
	}
      else
	{
	  counter = scm_debug_cells_gc_interval;
	  scm_gc ();
	}
    }
}

/* Whether cell validation is already running.  */
static int scm_i_cell_validation_already_running = 0;

void
scm_assert_cell_valid (SCM cell)
{
  if (!scm_i_cell_validation_already_running && scm_debug_cell_accesses_p)
    {
      scm_i_cell_validation_already_running = 1;  /* set to avoid recursion */

      /*
	During GC, no user-code should be run, and the guile core
	should use non-protected accessors.
      */
      if (scm_gc_running_p)
	return;

      /*
	Only scm_in_heap_p and rescanning the heap is wildly
	expensive.
      */
      if (scm_expensive_debug_cell_accesses_p)
	scm_i_expensive_validation_check (cell);

      scm_i_cell_validation_already_running = 0;  /* re-enable */
    }
}



SCM_DEFINE (scm_set_debug_cell_accesses_x, "set-debug-cell-accesses!", 1, 0, 0,
	    (SCM flag),
	    "If @var{flag} is @code{#f}, cell access checking is disabled.\n"
	    "If @var{flag} is @code{#t}, cheap cell access checking is enabled,\n"
	    "but no additional calls to garbage collection are issued.\n"
	    "If @var{flag} is a number, strict cell access checking is enabled,\n"
	    "with an additional garbage collection after the given\n"
	    "number of cell accesses.\n"
	    "This procedure only exists when the compile-time flag\n"
	    "@code{SCM_DEBUG_CELL_ACCESSES} was set to 1.")
#define FUNC_NAME s_scm_set_debug_cell_accesses_x
{
  if (scm_is_false (flag))
    {
      scm_debug_cell_accesses_p = 0;
    }
  else if (scm_is_eq (flag, SCM_BOOL_T))
    {
      scm_debug_cells_gc_interval = 0;
      scm_debug_cell_accesses_p = 1;
      scm_expensive_debug_cell_accesses_p = 0;
    }
  else
    {
      scm_debug_cells_gc_interval = scm_to_signed_integer (flag, 0, INT_MAX);
      scm_debug_cell_accesses_p = 1;
      scm_expensive_debug_cell_accesses_p = 1;
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


#endif  /* SCM_DEBUG_CELL_ACCESSES == 1 */



/* Compatibility.  */

#ifndef HAVE_GC_GET_HEAP_USAGE_SAFE
static void
GC_get_heap_usage_safe (GC_word *pheap_size, GC_word *pfree_bytes,
                        GC_word *punmapped_bytes, GC_word *pbytes_since_gc,
                        GC_word *ptotal_bytes)
{
  *pheap_size = GC_get_heap_size ();
  *pfree_bytes = GC_get_free_bytes ();
#ifdef HAVE_GC_GET_UNMAPPED_BYTES
  *punmapped_bytes = GC_get_unmapped_bytes ();
#else
  *punmapped_bytes = 0;
#endif
  *pbytes_since_gc = GC_get_bytes_since_gc ();
  *ptotal_bytes = GC_get_total_bytes ();
}
#endif

#ifndef HAVE_GC_GET_FREE_SPACE_DIVISOR
static GC_word
GC_get_free_space_divisor (void)
{
  return GC_free_space_divisor;
}
#endif


/* Hooks.  */
scm_t_c_hook scm_before_gc_c_hook;
scm_t_c_hook scm_before_mark_c_hook;
scm_t_c_hook scm_before_sweep_c_hook;
scm_t_c_hook scm_after_sweep_c_hook;
scm_t_c_hook scm_after_gc_c_hook;


static void
run_before_gc_c_hook (void)
{
  if (!SCM_I_CURRENT_THREAD)
    /* GC while a thread is spinning up; punt.  */
    return;

  scm_c_hook_run (&scm_before_gc_c_hook, NULL);
}


/* GC Statistics Keeping
 */
unsigned long scm_gc_ports_collected = 0;
static long gc_time_taken = 0;
static long gc_start_time = 0;

static unsigned long free_space_divisor;
static unsigned long minimum_free_space_divisor;
static double target_free_space_divisor;

static unsigned long protected_obj_count = 0;


SCM_SYMBOL (sym_gc_time_taken, "gc-time-taken");
SCM_SYMBOL (sym_heap_size, "heap-size");
SCM_SYMBOL (sym_heap_free_size, "heap-free-size");
SCM_SYMBOL (sym_heap_total_allocated, "heap-total-allocated");
SCM_SYMBOL (sym_heap_allocated_since_gc, "heap-allocated-since-gc");
SCM_SYMBOL (sym_protected_objects, "protected-objects");
SCM_SYMBOL (sym_times, "gc-times");


/* {Scheme Interface to GC}
 */
extern int scm_gc_malloc_yield_percentage;
SCM_DEFINE (scm_gc_stats, "gc-stats", 0, 0, 0,
            (),
	    "Return an association list of statistics about Guile's current\n"
	    "use of storage.\n")
#define FUNC_NAME s_scm_gc_stats
{
  SCM answer;
  GC_word heap_size, free_bytes, unmapped_bytes, bytes_since_gc, total_bytes;
  size_t gc_times;

  GC_get_heap_usage_safe (&heap_size, &free_bytes, &unmapped_bytes,
                          &bytes_since_gc, &total_bytes);
#ifdef HAVE_GC_GET_GC_NO
  /* This function was added in 7.2alpha2 (June 2009).  */
  gc_times = GC_get_gc_no ();
#else
  /* This symbol is deprecated as of 7.3.  */
  gc_times = GC_gc_no;
#endif

  answer =
    scm_list_n (scm_cons (sym_gc_time_taken, scm_from_long (gc_time_taken)),
		scm_cons (sym_heap_size, scm_from_size_t (heap_size)),
		scm_cons (sym_heap_free_size, scm_from_size_t (free_bytes)),
		scm_cons (sym_heap_total_allocated,
			  scm_from_size_t (total_bytes)),
                scm_cons (sym_heap_allocated_since_gc,
			  scm_from_size_t (bytes_since_gc)),
		scm_cons (sym_protected_objects,
			  scm_from_ulong (protected_obj_count)),
		scm_cons (sym_times, scm_from_size_t (gc_times)),
		SCM_UNDEFINED);

  return answer;
}
#undef FUNC_NAME


SCM_DEFINE (scm_gc_dump, "gc-dump", 0, 0, 0,
	    (void),
	    "Dump information about the garbage collector's internal data "
	    "structures and memory usage to the standard output.")
#define FUNC_NAME s_scm_gc_dump
{
  GC_dump ();

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_object_address, "object-address", 1, 0, 0,
            (SCM obj),
	    "Return an integer that for the lifetime of @var{obj} is uniquely\n"
	    "returned by this function for @var{obj}")
#define FUNC_NAME s_scm_object_address
{
  return scm_from_ulong (SCM_UNPACK (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_gc_disable, "gc-disable", 0, 0, 0,
	    (),
	    "Disables the garbage collector.  Nested calls are permitted.  "
	    "GC is re-enabled once @code{gc-enable} has been called the "
	    "same number of times @code{gc-disable} was called.")
#define FUNC_NAME s_scm_gc_disable
{
  GC_disable ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gc_enable, "gc-enable", 0, 0, 0,
	    (),
	    "Enables the garbage collector.")
#define FUNC_NAME s_scm_gc_enable
{
  GC_enable ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_gc, "gc", 0, 0, 0,
           (),
	    "Scans all of SCM objects and reclaims for further use those that are\n"
	    "no longer accessible.")
#define FUNC_NAME s_scm_gc
{
  scm_i_gc ("call");
  /* If you're calling scm_gc(), you probably want synchronous
     finalization.  */
  GC_invoke_finalizers ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_i_gc (const char *what)
{
#ifndef HAVE_GC_SET_START_CALLBACK
  run_before_gc_c_hook ();
#endif
  GC_gcollect ();
}



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
 *   char *chars = scm_i_string_chars (str);
 *   some_function (chars);
 *   scm_remember_upto_here_1 (str);  // str will be alive up to this point.
 */

/* Remove any macro versions of these while defining the functions.
   Functions are always included in the library, for upward binary
   compatibility and in case combinations of GCC and non-GCC are used.  */
#undef scm_remember_upto_here_1
#undef scm_remember_upto_here_2

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
  return (scm_gc_protect_object (obj));
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
   scm_gc_protect_object (X) increments and scm_gc_unprotect_object (X) decrements.
*/



SCM
scm_gc_protect_object (SCM obj)
{
  SCM handle;

  /* This critical section barrier will be replaced by a mutex. */
  /* njrev: Indeed; if my comment above is correct, there is the same
     critsec/mutex inconsistency here. */
  SCM_CRITICAL_SECTION_START;

  handle = scm_hashq_create_handle_x (scm_protects, obj, scm_from_int (0));
  SCM_SETCDR (handle, scm_sum (SCM_CDR (handle), scm_from_int (1)));

  protected_obj_count ++;
  
  SCM_CRITICAL_SECTION_END;

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
  /* njrev: and again. */
  SCM_CRITICAL_SECTION_START;

  if (scm_gc_running_p)
    {
      fprintf (stderr, "scm_unprotect_object called during GC.\n");
      abort ();
    }
 
  handle = scm_hashq_get_handle (scm_protects, obj);

  if (scm_is_false (handle))
    {
      fprintf (stderr, "scm_unprotect_object called on unprotected object\n");
      abort ();
    }
  else
    {
      SCM count = scm_difference (SCM_CDR (handle), scm_from_int (1));
      if (scm_is_eq (count, scm_from_int (0)))
	scm_hashq_remove_x (scm_protects, obj);
      else
	SCM_SETCDR (handle, count);
    }
  protected_obj_count --;

  SCM_CRITICAL_SECTION_END;

  return obj;
}

void
scm_gc_register_root (SCM *p)
{
  /* Nothing.  */
}

void
scm_gc_unregister_root (SCM *p)
{
  /* Nothing.  */
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




/*
  MOVE THIS FUNCTION. IT DOES NOT HAVE ANYTHING TODO WITH GC.
 */

/* Get an integer from an environment variable.  */
int
scm_getenv_int (const char *var, int def)
{
  char *end = 0;
  char *val = getenv (var);
  long res = def;
  if (!val)
    return def;
  res = strtol (val, &end, 10);
  if (end == val)
    return def;
  return res;
}

#ifndef HAVE_GC_SET_FINALIZE_ON_DEMAND
static void
GC_set_finalize_on_demand (int foo)
{
  GC_finalize_on_demand = foo;
}
#endif

void
scm_storage_prehistory ()
{
#ifdef HAVE_GC_SET_ALL_INTERIOR_POINTERS
  /* This function was added in 7.2alpha2 (June 2009).  */
  GC_set_all_interior_pointers (0);
#else
  /* This symbol is deprecated in 7.3.  */
  GC_all_interior_pointers = 0;
#endif

  free_space_divisor = scm_getenv_int ("GC_FREE_SPACE_DIVISOR", 3);
  minimum_free_space_divisor = free_space_divisor;
  target_free_space_divisor = free_space_divisor;
  GC_set_free_space_divisor (free_space_divisor);
  GC_set_finalize_on_demand (1);

  GC_INIT ();

#if (! ((defined GC_VERSION_MAJOR) && (GC_VERSION_MAJOR >= 7))) \
    && (defined SCM_I_GSC_USE_PTHREAD_THREADS)
  /* When using GC 6.8, this call is required to initialize thread-local
     freelists (shouldn't be necessary with GC 7.0).  */
  GC_init ();
#endif

  GC_expand_hp (SCM_DEFAULT_INIT_HEAP_SIZE_2);

  /* We only need to register a displacement for those types for which the
     higher bits of the type tag are used to store a pointer (that is, a
     pointer to an 8-octet aligned region).  For `scm_tc3_struct', this is
     handled in `scm_alloc_struct ()'.  */
  GC_REGISTER_DISPLACEMENT (scm_tc3_cons);
  /* GC_REGISTER_DISPLACEMENT (scm_tc3_unused); */

  /* Sanity check.  */
  if (!GC_is_visible (&scm_protects))
    abort ();

  scm_c_hook_init (&scm_before_gc_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_before_mark_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_before_sweep_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_after_sweep_c_hook, 0, SCM_C_HOOK_NORMAL);
  scm_c_hook_init (&scm_after_gc_c_hook, 0, SCM_C_HOOK_NORMAL);
}

scm_i_pthread_mutex_t scm_i_gc_admin_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

void
scm_init_gc_protect_object ()
{
  scm_protects = scm_c_make_hash_table (31);

#if 0
  /* We can't have a cleanup handler since we have no thread to run it
     in. */

#ifdef HAVE_ATEXIT
  atexit (cleanup);
#else
#ifdef HAVE_ON_EXIT
  on_exit (cleanup, 0);
#endif
#endif

#endif
}



SCM scm_after_gc_hook;

static SCM after_gc_async_cell;

/* The function after_gc_async_thunk causes the execution of the
 * after-gc-hook.  It is run after the gc, as soon as the asynchronous
 * events are handled by the evaluator.
 */
static SCM
after_gc_async_thunk (void)
{
  /* Fun, no? Hook-run *and* run-hook?  */
  scm_c_hook_run (&scm_after_gc_c_hook, NULL);
  scm_c_run_hook (scm_after_gc_hook, SCM_EOL);
  return SCM_UNSPECIFIED;
}


/* The function queue_after_gc_hook is run by the scm_before_gc_c_hook
 * at the end of the garbage collection.  The only purpose of this
 * function is to mark the after_gc_async (which will eventually lead to
 * the execution of the after_gc_async_thunk).
 */
static void *
queue_after_gc_hook (void * hook_data SCM_UNUSED,
                      void *fn_data SCM_UNUSED,
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
  if (scm_debug_cells_gc_interval == 0)
#endif
    {
      scm_i_thread *t = SCM_I_CURRENT_THREAD;

      if (scm_is_false (SCM_CDR (after_gc_async_cell)))
        {
          SCM_SETCDR (after_gc_async_cell, t->active_asyncs);
          t->active_asyncs = after_gc_async_cell;
          t->pending_asyncs = 1;
        }
    }

  return NULL;
}



static void *
start_gc_timer (void * hook_data SCM_UNUSED,
                void *fn_data SCM_UNUSED,
                void *data SCM_UNUSED)
{
  if (!gc_start_time)
    gc_start_time = scm_c_get_internal_run_time ();

  return NULL;
}

static void *
accumulate_gc_timer (void * hook_data SCM_UNUSED,
                void *fn_data SCM_UNUSED,
                void *data SCM_UNUSED)
{
  if (gc_start_time)
    {
      long now = scm_c_get_internal_run_time ();
      gc_time_taken += now - gc_start_time;
      gc_start_time = 0;
    }

  return NULL;
}

/* Return some idea of the memory footprint of a process, in bytes.
   Currently only works on Linux systems.  */
static size_t
get_image_size (void)
{
  unsigned long size, resident, share;
  size_t ret = 0;

  FILE *fp = fopen ("/proc/self/statm", "r");

  if (fp && fscanf (fp, "%lu %lu %lu", &size, &resident, &share) == 3)
    ret = resident * 4096;

  if (fp)
    fclose (fp);

  return ret;
}

/* These are discussed later.  */
static size_t bytes_until_gc;
static scm_i_pthread_mutex_t bytes_until_gc_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

/* Make GC run more frequently when the process image size is growing,
   measured against the number of bytes allocated through the GC.

   If Guile is allocating at a GC-managed heap size H, libgc will tend
   to limit the process image size to H*N.  But if at the same time the
   user program is mallocating at a rate M bytes per GC-allocated byte,
   then the process stabilizes at H*N*M -- assuming that collecting data
   will result in malloc'd data being freed.  It doesn't take a very
   large M for this to be a bad situation.  To limit the image size,
   Guile should GC more often -- the bigger the M, the more often.

   Numeric functions that produce bigger and bigger integers are
   pessimal, because M is an increasing function of time.  Here is an
   example of such a function:

      (define (factorial n)
        (define (fac n acc)
          (if (<= n 1)
            acc
            (fac (1- n) (* n acc))))
        (fac n 1))

   It is possible for a process to grow for reasons that will not be
   solved by faster GC.  In that case M will be estimated as
   artificially high for a while, and so GC will happen more often on
   the Guile side.  But when it stabilizes, Guile can ease back the GC
   frequency.

   The key is to measure process image growth, not mallocation rate.
   For maximum effectiveness, Guile reacts quickly to process growth,
   and exponentially backs down when the process stops growing.

   See http://thread.gmane.org/gmane.lisp.guile.devel/12552/focus=12936
   for further discussion.
 */
static void *
adjust_gc_frequency (void * hook_data SCM_UNUSED,
                     void *fn_data SCM_UNUSED,
                     void *data SCM_UNUSED)
{
  static size_t prev_image_size = 0;
  static size_t prev_bytes_alloced = 0;
  size_t image_size;
  size_t bytes_alloced;
  
  scm_i_pthread_mutex_lock (&bytes_until_gc_lock);
  bytes_until_gc = GC_get_heap_size ();
  scm_i_pthread_mutex_unlock (&bytes_until_gc_lock);

  image_size = get_image_size ();
  bytes_alloced = GC_get_total_bytes ();

#define HEURISTICS_DEBUG 0

#if HEURISTICS_DEBUG
  fprintf (stderr, "prev image / alloced: %lu / %lu\n", prev_image_size, prev_bytes_alloced);
  fprintf (stderr, "     image / alloced: %lu / %lu\n", image_size, bytes_alloced);
  fprintf (stderr, "divisor %lu / %f\n", free_space_divisor, target_free_space_divisor);
#endif

  if (prev_image_size && bytes_alloced != prev_bytes_alloced)
    {
      double growth_rate, new_target_free_space_divisor;
      double decay_factor = 0.5;
      double hysteresis = 0.1;

      growth_rate = ((double) image_size - prev_image_size)
        / ((double)bytes_alloced - prev_bytes_alloced);
      
#if HEURISTICS_DEBUG
      fprintf (stderr, "growth rate %f\n", growth_rate);
#endif

      new_target_free_space_divisor = minimum_free_space_divisor;

      if (growth_rate > 0)
        new_target_free_space_divisor *= 1.0 + growth_rate;

#if HEURISTICS_DEBUG
      fprintf (stderr, "new divisor %f\n", new_target_free_space_divisor);
#endif

      if (new_target_free_space_divisor < target_free_space_divisor)
        /* Decay down.  */
        target_free_space_divisor =
          (decay_factor * target_free_space_divisor
           + (1.0 - decay_factor) * new_target_free_space_divisor);
      else
        /* Jump up.  */
        target_free_space_divisor = new_target_free_space_divisor;

#if HEURISTICS_DEBUG
      fprintf (stderr, "new target divisor %f\n", target_free_space_divisor);
#endif

      if (free_space_divisor + 0.5 + hysteresis < target_free_space_divisor
          || free_space_divisor - 0.5 - hysteresis > target_free_space_divisor)
        {
          free_space_divisor = lround (target_free_space_divisor);
#if HEURISTICS_DEBUG
          fprintf (stderr, "new divisor %lu\n", free_space_divisor);
#endif
          GC_set_free_space_divisor (free_space_divisor);
        }
    }

  prev_image_size = image_size;
  prev_bytes_alloced = bytes_alloced;

  return NULL;
}

/* The adjust_gc_frequency routine handles transients in the process
   image size.  It can't handle instense non-GC-managed steady-state
   allocation though, as it decays the FSD at steady-state down to its
   minimum value.

   The only real way to handle continuous, high non-GC allocation is to
   let the GC know about it.  This routine can handle non-GC allocation
   rates that are similar in size to the GC-managed heap size.
 */

void
scm_gc_register_allocation (size_t size)
{
  scm_i_pthread_mutex_lock (&bytes_until_gc_lock);
  if (bytes_until_gc - size > bytes_until_gc)
    {
      bytes_until_gc = GC_get_heap_size ();
      scm_i_pthread_mutex_unlock (&bytes_until_gc_lock);
      GC_gcollect ();
    }
  else
    {
      bytes_until_gc -= size;
      scm_i_pthread_mutex_unlock (&bytes_until_gc_lock);
    }
}




char const *
scm_i_tag_name (scm_t_bits tag)
{
  switch (tag & 0x7f) /* 7 bits */
    {
    case scm_tcs_struct:
      return "struct";
    case scm_tcs_cons_imcar:
      return "cons (immediate car)";
    case scm_tcs_cons_nimcar:
      return "cons (non-immediate car)";
    case scm_tc7_pointer:
      return "foreign";
    case scm_tc7_hashtable:
      return "hashtable";
    case scm_tc7_fluid:
      return "fluid";
    case scm_tc7_dynamic_state:
      return "dynamic state";
    case scm_tc7_frame:
      return "frame";
    case scm_tc7_objcode:
      return "objcode";
    case scm_tc7_vm:
      return "vm";
    case scm_tc7_vm_cont:
      return "vm continuation";
    case scm_tc7_wvect:
      return "weak vector";
    case scm_tc7_vector:
      return "vector";
    case scm_tc7_number:
      switch (tag)
	{
	case scm_tc16_real:
	  return "real";
	  break;
	case scm_tc16_big:
	  return "bignum";
	  break;
	case scm_tc16_complex:
	  return "complex number";
	  break;
	case scm_tc16_fraction:
	  return "fraction";
	  break;
	}
      break;
    case scm_tc7_string:
      return "string";
      break;
    case scm_tc7_stringbuf:
      return "string buffer";
      break;
    case scm_tc7_symbol:
      return "symbol";
      break;
    case scm_tc7_variable:
      return "variable";
      break;
    case scm_tc7_port:
      return "port";
      break;
    case scm_tc7_smob:
      {
        int k = 0xff & (tag >> 8);
        return (scm_smobs[k].name);
      }
      break; 
    }

  return NULL;
}




void
scm_init_gc ()
{
  /* `GC_INIT ()' was invoked in `scm_storage_prehistory ()'.  */

  scm_after_gc_hook = scm_make_hook (SCM_INUM0);
  scm_c_define ("after-gc-hook", scm_after_gc_hook);

  /* When the async is to run, the cdr of the gc_async pair gets set to
     the asyncs queue of the current thread.  */
  after_gc_async_cell = scm_cons (scm_c_make_gsubr ("%after-gc-thunk", 0, 0, 0,
                                                    after_gc_async_thunk),
                                  SCM_BOOL_F);

  scm_c_hook_add (&scm_before_gc_c_hook, queue_after_gc_hook, NULL, 0);
  scm_c_hook_add (&scm_before_gc_c_hook, start_gc_timer, NULL, 0);
  scm_c_hook_add (&scm_after_gc_c_hook, accumulate_gc_timer, NULL, 0);

#if HAVE_GC_GET_HEAP_USAGE_SAFE
  /* GC_get_heap_usage does not take a lock, and so can run in the GC
     start hook.  */
  scm_c_hook_add (&scm_before_gc_c_hook, adjust_gc_frequency, NULL, 0);
#else
  /* GC_get_heap_usage might take a lock (and did from 7.2alpha1 to
     7.2alpha7), so call it in the after_gc_hook.  */
  scm_c_hook_add (&scm_after_gc_c_hook, adjust_gc_frequency, NULL, 0);
#endif

#ifdef HAVE_GC_SET_START_CALLBACK
  GC_set_start_callback (run_before_gc_c_hook);
#endif

#include "libguile/gc.x"
}


void
scm_gc_sweep (void)
#define FUNC_NAME "scm_gc_sweep"
{
  /* FIXME */
  fprintf (stderr, "%s: doing nothing\n", FUNC_NAME);
}
#undef FUNC_NAME

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

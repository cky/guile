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



#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

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
#include "libguile/private-gc.h"
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


/*
  Entry point for this file.
 */
void
scm_mark_all (void)
{
  long j;
  
  
  scm_i_clear_mark_space ();
  
  /* Mark every thread's stack and registers */
  scm_threads_mark_stacks ();

  j = SCM_NUM_PROTECTS;
  while (j--)
    scm_gc_mark (scm_sys_protects[j]);

  /* mark the registered roots */
  {
    size_t i;
    for (i = 0; i < SCM_VECTOR_LENGTH (scm_gc_registered_roots); ++i)
      {
	SCM l = SCM_VELTS (scm_gc_registered_roots)[i];
	for (; !SCM_NULLP (l); l = SCM_CDR (l))
	  {
	    SCM *p = (SCM *) (scm_num2long (SCM_CAAR (l), 0, NULL));
	    scm_gc_mark (*p);
	  }
      }
  }
  

  /* FIXME: we should have a means to register C functions to be run
   * in different phases of GC
   */
  scm_mark_subr_table ();
}

/* {Mark/Sweep}
 */

/*
  Mark an object precisely, then recurse.
 */
void
scm_gc_mark (SCM ptr)
{
  if (SCM_IMP (ptr))
    return ;
  
  if (SCM_GC_MARK_P (ptr))
    {
      return;
    }

  SCM_SET_GC_MARK (ptr);
  scm_gc_mark_dependencies (ptr);
}

/*

Mark the dependencies of an object.

Prefetching:

Should prefetch objects before marking, i.e. if marking a cell, we
should prefetch the car, and then mark the cdr. This will improve CPU
cache misses, because the car is more likely to be in core when we
finish the cdr.

See http://www.hpl.hp.com/techreports/2000/HPL-2000-99.pdf, reducing
garbage collector cache misses.

Prefetch is supported on GCC >= 3.1 

(Some time later.)

Tried this with GCC 3.1.1 -- the time differences are barely measurable.
Perhaps this would work better with an explicit markstack?


*/
void
scm_gc_mark_dependencies (SCM p)
#define FUNC_NAME "scm_gc_mark_dependencies"
{
  register long i;
  register SCM ptr;
  scm_t_bits cell_type;

  ptr = p;
 scm_mark_dependencies_again:
  
  cell_type = SCM_GC_CELL_TYPE (ptr);
  switch (SCM_ITAG7 (cell_type))
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

      scm_gc_mark (SCM_SETTER (ptr));
      ptr = SCM_PROCEDURE (ptr);
      goto gc_mark_loop;
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
	    scm_gc_mark (SCM_PACK (struct_data[scm_struct_i_procedure]));
	    scm_gc_mark (SCM_PACK (struct_data[scm_struct_i_setter]));
	  }
	if (len)
	  {
	    long x;

	    for (x = 0; x < len - 2; x += 2, ++struct_data)
	      if (fields_desc[x] == 'p')
		scm_gc_mark (SCM_PACK (*struct_data));
	    if (fields_desc[x] == 'p')
	      {
		if (SCM_LAYOUT_TAILP (fields_desc[x + 1]))
		  for (x = *struct_data++; x; --x, ++struct_data)
		    scm_gc_mark (SCM_PACK (*struct_data));
		else
		  scm_gc_mark (SCM_PACK (*struct_data));
	      }
	  }
	/* mark vtable */
	ptr = SCM_PACK (vtable_data [scm_vtable_index_vtable]);
	goto gc_mark_loop;
      }
      break;
    case scm_tcs_closures:
      if (SCM_IMP (SCM_ENV (ptr)))
	{
	  ptr = SCM_CLOSCAR (ptr);
	  goto gc_mark_nimp;
	}
      scm_gc_mark (SCM_CLOSCAR (ptr));
      ptr = SCM_ENV (ptr);
      goto gc_mark_nimp;
    case scm_tc7_vector:
      i = SCM_VECTOR_LENGTH (ptr);
      if (i == 0)
	break;
      while (--i > 0)
	{
	  if (SCM_NIMP (SCM_VELTS (ptr)[i]))
	    scm_gc_mark (SCM_VELTS (ptr)[i]);
	}
      ptr = SCM_VELTS (ptr)[0];
      goto gc_mark_loop;
#ifdef CCLO
    case scm_tc7_cclo:
      {
	size_t i = SCM_CCLO_LENGTH (ptr);
	size_t j;
	for (j = 1; j != i; ++j)
	  {
	    SCM obj = SCM_CCLO_REF (ptr, j);
	    if (!SCM_IMP (obj))
	      scm_gc_mark (obj);
	  }
	ptr = SCM_CCLO_REF (ptr, 0);
	goto gc_mark_loop;
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
		     && !SCM_GC_MARK_P (alist)
		     && SCM_CONSP (SCM_CAR (alist)))
		{
		  SCM kvpair;
		  SCM next_alist;

		  kvpair = SCM_CAR (alist);
		  next_alist = SCM_CDR (alist);
		  /*
		   * Do not do this:
		   * 	SCM_SET_GC_MARK (alist);
		   *	SCM_SET_GC_MARK (kvpair);
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

    case scm_tc7_symbol:
      ptr = SCM_PROP_SLOTS (ptr);
      goto gc_mark_loop;
    case scm_tc7_variable:
      ptr = SCM_CELL_OBJECT_1 (ptr);
      goto gc_mark_loop;
    case scm_tcs_subrs:
      break;
    case scm_tc7_port:
      i = SCM_PTOBNUM (ptr);
#if (SCM_DEBUG_CELL_ACCESSES == 1) 
      if (!(i < scm_numptob))
	{
	  fprintf (stderr, "undefined port type");
	  abort();
	}
#endif
      if (SCM_PTAB_ENTRY(ptr))
	scm_gc_mark (SCM_FILENAME (ptr));
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
#if (SCM_DEBUG_CELL_ACCESSES == 1)
	  if (!(i < scm_numsmob))
	    {
	      fprintf (stderr, "undefined smob type");
	      abort();
	    }
#endif
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
      fprintf (stderr, "unknown type");
      abort();
    }

  /*
    If we got here, then exhausted recursion options for PTR.  we
    return (careful not to mark PTR, it might be the argument that we
    were called with.)
   */
  return ;
  
gc_mark_loop:
  if (SCM_IMP (ptr))
    return;

 gc_mark_nimp:
  {
    int valid_cell = CELL_P (ptr);

    
#if (SCM_DEBUG_CELL_ACCESSES == 1)
    if (scm_debug_cell_accesses_p)
      {
    /* We are in debug mode.  Check the ptr exhaustively. */
	
	valid_cell = valid_cell && (scm_i_find_heap_segment_containing_object (ptr) >= 0);
      }
    
#endif
    if (!valid_cell)
      {
	fprintf (stderr, "rogue pointer in heap");
	abort();
      }
  }
  
 if (SCM_GC_MARK_P (ptr))
  {
    return;
  }
  
  SCM_SET_GC_MARK (ptr);

  goto   scm_mark_dependencies_again;
  
}
#undef FUNC_NAME




/* Mark a region conservatively */
void
scm_mark_locations (SCM_STACKITEM x[], unsigned long n)
{
  unsigned long m;

  for (m = 0; m < n; ++m)
    {
      SCM obj = * (SCM *) &x[m];
      long int segment = scm_i_find_heap_segment_containing_object (obj);
      if (segment >= 0)
	scm_gc_mark (obj);
    }
}


/* The function scm_in_heap_p determines whether an SCM value can be regarded as a
 * pointer to a cell on the heap.
 */
int
scm_in_heap_p (SCM value)
{
  long int segment = scm_i_find_heap_segment_containing_object (value);
  return (segment >= 0);
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
  unsigned long int cell_segment = scm_i_find_heap_segment_containing_object (cell);
  unsigned int span = scm_i_heap_segment_table[cell_segment]->span;
  unsigned int i;

  for (i = 1; i != span * 2; ++i)
    {
      SCM obj = SCM_CELL_OBJECT (cell, i);
      long int obj_segment = scm_i_find_heap_segment_containing_object (obj);
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
scm_gc_init_mark(void)
{
#if SCM_ENABLE_DEPRECATED == 1
  scm_tc16_allocated = scm_make_smob_type ("allocated cell", 0);
  scm_set_smob_mark (scm_tc16_allocated, allocated_mark);
#endif
}


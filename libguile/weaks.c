/* Copyright (C) 1995,1996,1998,2000,2001 Free Software Foundation, Inc.
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
 * As a special exception, Free Software Foundation gives permission
 * for additional uses of the text contained in its release of this library.
 *
 * The exception is that, if you link this library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking this library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by 
 * Free Software Foundation as part of this library.  If you copy
 * code from other releases distributed under the terms of the GPL into a copy of
 * this library, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from such code.
 *
 * If you write modifications of your own for this library, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */




#include "libguile/_scm.h"
#include "libguile/vectors.h"
#include "libguile/lang.h"

#include "libguile/validate.h"
#include "libguile/weaks.h"



/* {Weak Vectors}
 */


/* Allocate memory for a weak vector on behalf of the caller.  The allocated
 * vector will be of the given weak vector subtype.  It will contain size
 * elements which are initialized with the 'fill' object, or, if 'fill' is
 * undefined, with an unspecified object.
 */
static SCM
allocate_weak_vector (scm_t_bits type, SCM size, SCM fill, const char* caller)
#define FUNC_NAME caller
{
  if (SCM_INUMP (size))
    {
      size_t c_size;
      SCM v;

      SCM_ASSERT_RANGE (1, size, SCM_INUM (size) >= 0);
      c_size = SCM_INUM (size);

      if (c_size > 0)
	{
	  scm_t_bits *base;
	  size_t j;

	  if (SCM_UNBNDP (fill))
	    fill = SCM_UNSPECIFIED;

	  SCM_ASSERT_RANGE (1, size, c_size <= SCM_VECTOR_MAX_LENGTH);
	  base = scm_gc_malloc (c_size * sizeof (scm_t_bits), "weak vector");
	  for (j = 0; j != c_size; ++j)
	    base[j] = SCM_UNPACK (fill);
	  v = scm_double_cell (SCM_MAKE_VECTOR_TAG (c_size, scm_tc7_wvect),
			       (scm_t_bits) base,
			       type,
			       SCM_UNPACK (SCM_EOL));
	  scm_remember_upto_here_1 (fill);
	}
      else
	{
	  v = scm_double_cell (SCM_MAKE_VECTOR_TAG (0, scm_tc7_wvect),
			       (scm_t_bits) NULL,
			       type,
			       SCM_UNPACK (SCM_EOL));
	}

      return v;
    }
  else if (SCM_BIGP (size))
    SCM_OUT_OF_RANGE (1, size);
  else
    SCM_WRONG_TYPE_ARG (1, size);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_vector, "make-weak-vector", 1, 1, 0,
	    (SCM size, SCM fill),
	    "Return a weak vector with @var{size} elements. If the optional\n"
	    "argument @var{fill} is given, all entries in the vector will be\n"
	    "set to @var{fill}. The default value for @var{fill} is the\n"
	    "empty list.")
#define FUNC_NAME s_scm_make_weak_vector
{
  return allocate_weak_vector (0, size, fill, FUNC_NAME);
}
#undef FUNC_NAME


SCM_REGISTER_PROC(s_list_to_weak_vector, "list->weak-vector", 1, 0, 0, scm_weak_vector);

SCM_DEFINE (scm_weak_vector, "weak-vector", 0, 0, 1, 
           (SCM l),
	    "@deffnx {Scheme Procedure} list->weak-vector l\n"
	    "Construct a weak vector from a list: @code{weak-vector} uses\n"
	    "the list of its arguments while @code{list->weak-vector} uses\n"
	    "its only argument @var{l} (a list) to construct a weak vector\n"
	    "the same way @code{list->vector} would.")
#define FUNC_NAME s_scm_weak_vector
{
  SCM res;
  SCM *data;
  long i;

  /* Dirk:FIXME:: In case of multiple threads, the list might get corrupted
     while the vector is being created. */
  i = scm_ilength (l);
  SCM_ASSERT (i >= 0, l, SCM_ARG1, FUNC_NAME);
  res = scm_make_weak_vector (SCM_MAKINUM (i), SCM_UNSPECIFIED);
  data = SCM_VELTS (res);

  while (!SCM_NULL_OR_NIL_P (l))
    {
      *data++ = SCM_CAR (l);
      l = SCM_CDR (l);
    }

  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_vector_p, "weak-vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak vector. Note that all\n"
	    "weak hashes are also weak vectors.")
#define FUNC_NAME s_scm_weak_vector_p
{
  return SCM_BOOL (SCM_WVECTP (obj) && !SCM_IS_WHVEC (obj));
}
#undef FUNC_NAME



SCM_DEFINE (scm_make_weak_key_hash_table, "make-weak-key-hash-table", 1, 0, 0, 
	    (SCM size),
	    "@deffnx {Scheme Procedure} make-weak-value-hash-table size\n"
	    "@deffnx {Scheme Procedure} make-doubly-weak-hash-table size\n"
	    "Return a weak hash table with @var{size} buckets. As with any\n"
	    "hash table, choosing a good size for the table requires some\n"
	    "caution.\n"
	    "\n"
	    "You can modify weak hash tables in exactly the same way you\n"
	    "would modify regular hash tables. (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_key_hash_table
{
  return allocate_weak_vector (1, size, SCM_EOL, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_hash_table, "make-weak-value-hash-table", 1, 0, 0, 
            (SCM size),
	    "Return a hash table with weak values with @var{size} buckets.\n"
	    "(@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_value_hash_table
{
  return allocate_weak_vector (2, size, SCM_EOL, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_doubly_weak_hash_table, "make-doubly-weak-hash-table", 1, 0, 0, 
            (SCM size),
	    "Return a hash table with weak keys and values with @var{size}\n"
	    "buckets.  (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_doubly_weak_hash_table
{
  return allocate_weak_vector (3, size, SCM_EOL, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_key_hash_table_p, "weak-key-hash-table?", 1, 0, 0, 
           (SCM obj),
	    "@deffnx {Scheme Procedure} weak-value-hash-table? obj\n"
	    "@deffnx {Scheme Procedure} doubly-weak-hash-table? obj\n"
	    "Return @code{#t} if @var{obj} is the specified weak hash\n"
	    "table. Note that a doubly weak hash table is neither a weak key\n"
	    "nor a weak value hash table.")
#define FUNC_NAME s_scm_weak_key_hash_table_p
{
  return SCM_BOOL (SCM_WVECTP (obj) && SCM_IS_WHVEC (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_value_hash_table_p, "weak-value-hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak value hash table.")
#define FUNC_NAME s_scm_weak_value_hash_table_p
{
  return SCM_BOOL (SCM_WVECTP (obj) && SCM_IS_WHVEC_V (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_doubly_weak_hash_table_p, "doubly-weak-hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a doubly weak hash table.")
#define FUNC_NAME s_scm_doubly_weak_hash_table_p
{
  return SCM_BOOL (SCM_WVECTP (obj) && SCM_IS_WHVEC_B (obj));
}
#undef FUNC_NAME


static void *
scm_weak_vector_gc_init (void *dummy1 SCM_UNUSED,
			 void *dummy2 SCM_UNUSED,
			 void *dummy3 SCM_UNUSED)
{
  scm_weak_vectors = SCM_EOL;

  return 0;
}


static void *
scm_mark_weak_vector_spines (void *dummy1 SCM_UNUSED,
			     void *dummy2 SCM_UNUSED,
			     void *dummy3 SCM_UNUSED)
{
  SCM w;

  for (w = scm_weak_vectors; !SCM_NULLP (w); w = SCM_WVECT_GC_CHAIN (w))
    {
      if (SCM_IS_WHVEC_ANY (w))
	{
	  SCM *ptr;
	  SCM obj;
	  long j;
	  long n;

	  obj = w;
	  ptr = SCM_VELTS (w);
	  n = SCM_VECTOR_LENGTH (w);
	  for (j = 0; j < n; ++j)
	    {
	      SCM alist;

	      alist = ptr[j];
	      while (   SCM_CONSP (alist)
		     && !SCM_GCMARKP (alist)
		     && SCM_CONSP  (SCM_CAR (alist)))
		{
		  SCM_SETGCMARK (alist);
		  SCM_SETGCMARK (SCM_CAR (alist));
		  alist = SCM_CDR (alist);
		}
	    }
	}
    }

  return 0;
}


static void *
scm_scan_weak_vectors (void *dummy1 SCM_UNUSED,
		       void *dummy2 SCM_UNUSED,
		       void *dummy3 SCM_UNUSED)
{
  SCM *ptr, w;
  for (w = scm_weak_vectors; !SCM_NULLP (w); w = SCM_WVECT_GC_CHAIN (w))
    {
      if (!SCM_IS_WHVEC_ANY (w))
	{
	  register long j, n;

	  ptr = SCM_VELTS (w);
	  n = SCM_VECTOR_LENGTH (w);
	  for (j = 0; j < n; ++j)
	    if (SCM_FREE_CELL_P (ptr[j]))
	      ptr[j] = SCM_BOOL_F;
	}
      else /* if (SCM_IS_WHVEC_ANY (scm_weak_vectors[i])) */
	{
	  SCM obj = w;
	  register long n = SCM_VECTOR_LENGTH (w);
	  register long j;
          int weak_keys = SCM_IS_WHVEC (obj) || SCM_IS_WHVEC_B (obj);
          int weak_values = SCM_IS_WHVEC_V (obj) || SCM_IS_WHVEC_B (obj);

	  ptr = SCM_VELTS (w);

	  for (j = 0; j < n; ++j)
	    {
	      SCM * fixup;
	      SCM alist;

	      fixup = ptr + j;
	      alist = *fixup;

	      while (   SCM_CONSP (alist)
			&& SCM_CONSP (SCM_CAR (alist)))
		{
		  SCM key;
		  SCM value;

		  key = SCM_CAAR (alist);
		  value = SCM_CDAR (alist);
		  if (   (weak_keys && SCM_FREE_CELL_P (key))
			 || (weak_values && SCM_FREE_CELL_P (value)))
		    {
		      *fixup = SCM_CDR (alist);
		    }
		  else
		    fixup = SCM_CDRLOC (alist);
		  alist = SCM_CDR (alist);
		}
	    }
	}
    }

  return 0;
}



void
scm_weaks_prehistory ()
{
  scm_c_hook_add (&scm_before_mark_c_hook, scm_weak_vector_gc_init, 0, 0);
  scm_c_hook_add (&scm_before_sweep_c_hook, scm_mark_weak_vector_spines, 0, 0);
  scm_c_hook_add (&scm_after_sweep_c_hook, scm_scan_weak_vectors, 0, 0);
}


void
scm_init_weaks ()
{
#include "libguile/weaks.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

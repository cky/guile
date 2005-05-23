/* Copyright (C) 1995,1996,1998,2000,2001, 2003 Free Software Foundation, Inc.
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
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
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
#include "libguile/hashtab.h"

#include "libguile/validate.h"
#include "libguile/weaks.h"



/* 1. The current hash table implementation in hashtab.c uses weak alist
 *    vectors (formerly called weak hash tables) internally.
 *
 * 2. All hash table operations still work on alist vectors.
 *
 * 3. The weak vector and alist vector Scheme API is accessed through
 *    the module (ice-9 weak-vector).
 */


/* {Weak Vectors}
 */


SCM_DEFINE (scm_make_weak_vector, "make-weak-vector", 1, 1, 0,
	    (SCM size, SCM fill),
	    "Return a weak vector with @var{size} elements. If the optional\n"
	    "argument @var{fill} is given, all entries in the vector will be\n"
	    "set to @var{fill}. The default value for @var{fill} is the\n"
	    "empty list.")
#define FUNC_NAME s_scm_make_weak_vector
{
  return scm_i_allocate_weak_vector (0, size, fill);
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
  scm_t_array_handle handle;
  SCM res, *data;
  long i;

  i = scm_ilength (l);
  SCM_ASSERT (i >= 0, l, SCM_ARG1, FUNC_NAME);

  res = scm_make_weak_vector (scm_from_int (i), SCM_UNSPECIFIED);
  data = scm_vector_writable_elements (res, &handle, NULL, NULL);

  while (scm_is_pair (l) && i > 0)
    {
      *data++ = SCM_CAR (l);
      l = SCM_CDR (l);
      i--;
    }

  scm_array_handle_release (&handle);

  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_vector_p, "weak-vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak vector. Note that all\n"
	    "weak hashes are also weak vectors.")
#define FUNC_NAME s_scm_weak_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && !SCM_IS_WHVEC (obj));
}
#undef FUNC_NAME



SCM_DEFINE (scm_make_weak_key_alist_vector, "make-weak-key-alist-vector", 0, 1, 0, 
	    (SCM size),
	    "@deffnx {Scheme Procedure} make-weak-value-alist-vector size\n"
	    "@deffnx {Scheme Procedure} make-doubly-weak-alist-vector size\n"
	    "Return a weak hash table with @var{size} buckets. As with any\n"
	    "hash table, choosing a good size for the table requires some\n"
	    "caution.\n"
	    "\n"
	    "You can modify weak hash tables in exactly the same way you\n"
	    "would modify regular hash tables. (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_key_alist_vector
{
  return scm_i_allocate_weak_vector
    (1, SCM_UNBNDP (size) ? scm_from_int (31) : size, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_alist_vector, "make-weak-value-alist-vector", 0, 1, 0, 
            (SCM size),
	    "Return a hash table with weak values with @var{size} buckets.\n"
	    "(@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_value_alist_vector
{
  return scm_i_allocate_weak_vector
    (2, SCM_UNBNDP (size) ? scm_from_int (31) : size, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_doubly_weak_alist_vector, "make-doubly-weak-alist-vector", 1, 0, 0, 
            (SCM size),
	    "Return a hash table with weak keys and values with @var{size}\n"
	    "buckets.  (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_doubly_weak_alist_vector
{
  return scm_i_allocate_weak_vector
    (3, SCM_UNBNDP (size) ? scm_from_int (31) : size, SCM_EOL);
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_key_alist_vector_p, "weak-key-alist-vector?", 1, 0, 0, 
           (SCM obj),
	    "@deffnx {Scheme Procedure} weak-value-alist-vector? obj\n"
	    "@deffnx {Scheme Procedure} doubly-weak-alist-vector? obj\n"
	    "Return @code{#t} if @var{obj} is the specified weak hash\n"
	    "table. Note that a doubly weak hash table is neither a weak key\n"
	    "nor a weak value hash table.")
#define FUNC_NAME s_scm_weak_key_alist_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && SCM_IS_WHVEC (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_value_alist_vector_p, "weak-value-alist-vector?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak value hash table.")
#define FUNC_NAME s_scm_weak_value_alist_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && SCM_IS_WHVEC_V (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_doubly_weak_alist_vector_p, "doubly-weak-alist-vector?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a doubly weak hash table.")
#define FUNC_NAME s_scm_doubly_weak_alist_vector_p
{
  return scm_from_bool (SCM_I_WVECTP (obj) && SCM_IS_WHVEC_B (obj));
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

  for (w = scm_weak_vectors; !scm_is_null (w); w = SCM_I_WVECT_GC_CHAIN (w))
    {
      if (SCM_IS_WHVEC_ANY (w))
	{
	  SCM const *ptr;
	  SCM obj;
	  long j;
	  long n;

	  obj = w;
	  ptr = SCM_I_WVECT_GC_WVELTS (w);
	  n = SCM_I_WVECT_LENGTH (w);
	  for (j = 0; j < n; ++j)
	    {
	      SCM alist;

	      alist = ptr[j];
	      while (   scm_is_pair (alist)
		     && !SCM_GC_MARK_P (alist)
		     && scm_is_pair  (SCM_CAR (alist)))
		{
		  SCM_SET_GC_MARK (alist);
		  SCM_SET_GC_MARK (SCM_CAR (alist));
		  alist = SCM_CDR (alist);
		}
	    }
	}
    }

  return 0;
}

#define UNMARKED_CELL_P(x) (SCM_NIMP(x) && !SCM_GC_MARK_P (x))

static void *
scm_scan_weak_vectors (void *dummy1 SCM_UNUSED,
		       void *dummy2 SCM_UNUSED,
		       void *dummy3 SCM_UNUSED)
{
  SCM *ptr, w;
  for (w = scm_weak_vectors; !scm_is_null (w); w = SCM_I_WVECT_GC_CHAIN (w))
    {
      if (!SCM_IS_WHVEC_ANY (w))
	{
	  register long j, n;

	  ptr = SCM_I_WVECT_GC_WVELTS (w);
	  n = SCM_I_WVECT_LENGTH (w);
	  for (j = 0; j < n; ++j)
	    if (UNMARKED_CELL_P (ptr[j]))
	      ptr[j] = SCM_BOOL_F;
	}
      /* check if we should scan the alist vector here (hashtables
	 have their own scan function in hashtab.c). */
      else if (!SCM_WVECT_NOSCAN_P (w))
	{
	  SCM obj = w;
	  register long n = SCM_I_WVECT_LENGTH (w);
	  register long j;
          int weak_keys = SCM_IS_WHVEC (obj) || SCM_IS_WHVEC_B (obj);
          int weak_values = SCM_IS_WHVEC_V (obj) || SCM_IS_WHVEC_B (obj);

	  ptr = SCM_I_WVECT_GC_WVELTS (w);

	  for (j = 0; j < n; ++j)
	    {
	      SCM * fixup;
	      SCM alist;

	      fixup = ptr + j;
	      alist = *fixup;

	      while (scm_is_pair (alist)
		     && scm_is_pair (SCM_CAR (alist)))
		{
		  SCM key;
		  SCM value;

		  key = SCM_CAAR (alist);
		  value = SCM_CDAR (alist);
		  if (   (weak_keys && UNMARKED_CELL_P (key))
			 || (weak_values && UNMARKED_CELL_P (value)))
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


SCM
scm_init_weaks_builtins ()
{
#include "libguile/weaks.x"
  return SCM_UNSPECIFIED;
}

void
scm_init_weaks ()
{
  scm_c_define_gsubr ("%init-weaks-builtins", 0, 0, 0,
		      scm_init_weaks_builtins);
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

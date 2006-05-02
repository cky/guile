/* Copyright (C) 1995,1996,1998,2000,2001, 2003, 2006 Free Software Foundation, Inc.
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




#include <stdio.h>

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
  return scm_i_make_weak_vector (0, size, fill);
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
  return scm_i_make_weak_vector_from_list (0, l);
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


/* Weak alist vectors, i.e., vectors of alists.

   The alist vector themselves are _not_ weak.  The `car' (or `cdr', or both)
   of the pairs within it are weak.  See `hashtab.c' for details.  */


/* FIXME: We used to have two implementations of weak hash tables: the one in
   here and the one in `hashtab.c'.  The difference is that weak alist
   vectors could be used as vectors while (weak) hash tables can't.  We need
   to unify that.  */

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
  return scm_make_weak_key_hash_table (size);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_alist_vector, "make-weak-value-alist-vector", 0, 1, 0, 
            (SCM size),
	    "Return a hash table with weak values with @var{size} buckets.\n"
	    "(@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_value_alist_vector
{
  return scm_make_weak_value_hash_table (size);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_doubly_weak_alist_vector, "make-doubly-weak-alist-vector", 1, 0, 0, 
            (SCM size),
	    "Return a hash table with weak keys and values with @var{size}\n"
	    "buckets.  (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_doubly_weak_alist_vector
{
  return scm_make_doubly_weak_alist_vector (size);
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

#define UNMARKED_CELL_P(x) 1 /* (SCM_NIMP(x) && !SCM_GC_MARK_P (x)) *//*
									FIXME */


static void
scm_i_remove_weaks (SCM w)
{
  return;  /* FIXME */
#if 0
  SCM *ptr = SCM_I_WVECT_GC_WVELTS (w);
  size_t n = SCM_I_WVECT_LENGTH (w);
  size_t i;

  if (!SCM_IS_WHVEC_ANY (w))
    {
      for (i = 0; i < n; ++i)
	if (UNMARKED_CELL_P (ptr[i]))
	  ptr[i] = SCM_BOOL_F;
    }
  else
    {
      size_t delta = 0;

      for (i = 0; i < n; ++i)
	{
	  SCM alist, *fixup;

	  fixup = ptr + i;
	  alist = *fixup;
	  while (scm_is_pair (alist) && !SCM_GC_MARK_P (alist))
	    {
	      if (UNMARKED_CELL_P (SCM_CAR (alist)))
		{
		  *fixup = SCM_CDR (alist);
		  delta++;
		}
	      else
		{
		  SCM_SET_GC_MARK (alist);
		  fixup = SCM_CDRLOC (alist);
		}
	      alist = *fixup;
	    }
	}
#if 0
      if (delta)
	fprintf (stderr, "vector %p, delta %d\n", w, delta);
#endif
      SCM_I_SET_WVECT_DELTA (w, delta);
    }
#endif
}

#if 0
void
scm_i_remove_weaks_from_weak_vectors ()
{
  SCM w = weak_vectors;
  while (!scm_is_null (w))
    {
      scm_i_remove_weaks (w);
      w = SCM_I_WVECT_GC_CHAIN (w);
    }
}
#endif



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

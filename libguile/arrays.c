/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003,2004, 2005, 2006, 2009 Free Software Foundation, Inc.
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


/*
  This file has code for arrays in lots of variants (double, integer,
  unsigned etc. ). It suffers from hugely repetitive code because
  there is similar (but different) code for every variant included. (urg.)

  --hwn
*/


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/__scm.h"
#include "libguile/eq.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/fports.h"
#include "libguile/smob.h"
#include "libguile/feature.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"
#include "libguile/srfi-4.h"
#include "libguile/vectors.h"
#include "libguile/bitvectors.h"
#include "libguile/bytevectors.h"
#include "libguile/list.h"
#include "libguile/dynwind.h"

#include "libguile/validate.h"
#include "libguile/arrays.h"
#include "libguile/array-map.h"
#include "libguile/print.h"
#include "libguile/read.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif


/* The set of uniform scm_vector types is:
 *  Vector of:		 Called:   Replaced by:
 * unsigned char	string
 * char			byvect     s8 or u8, depending on signedness of 'char'
 * boolean		bvect      
 * signed long		ivect      s32
 * unsigned long	uvect      u32
 * float		fvect      f32
 * double		dvect      d32
 * complex double	cvect      c64
 * short		svect      s16
 * long long		llvect     s64
 */

scm_t_bits scm_i_tc16_array;
scm_t_bits scm_i_tc16_enclosed_array;

#define SCM_SET_ARRAY_CONTIGUOUS_FLAG(x) \
  (SCM_SET_CELL_WORD_0 ((x), SCM_CELL_WORD_0 (x) | SCM_I_ARRAY_FLAG_CONTIGUOUS))
#define SCM_CLR_ARRAY_CONTIGUOUS_FLAG(x) \
  (SCM_SET_CELL_WORD_0 ((x), SCM_CELL_WORD_0 (x) & ~SCM_I_ARRAY_FLAG_CONTIGUOUS))

typedef SCM creator_proc (SCM len, SCM fill);

struct {
  char *type_name;
  SCM type;
  creator_proc *creator;
} type_creator_table[] = {
  { "a", SCM_UNSPECIFIED, scm_make_string },
  { "b", SCM_UNSPECIFIED, scm_make_bitvector },
  { "u8", SCM_UNSPECIFIED, scm_make_u8vector },
  { "s8", SCM_UNSPECIFIED, scm_make_s8vector },
  { "u16", SCM_UNSPECIFIED, scm_make_u16vector },
  { "s16", SCM_UNSPECIFIED, scm_make_s16vector },
  { "u32", SCM_UNSPECIFIED, scm_make_u32vector },
  { "s32", SCM_UNSPECIFIED, scm_make_s32vector },
  { "u64", SCM_UNSPECIFIED, scm_make_u64vector },
  { "s64", SCM_UNSPECIFIED, scm_make_s64vector },
  { "f32", SCM_UNSPECIFIED, scm_make_f32vector },
  { "f64", SCM_UNSPECIFIED, scm_make_f64vector },
  { "c32", SCM_UNSPECIFIED, scm_make_c32vector },
  { "c64", SCM_UNSPECIFIED, scm_make_c64vector },
  { "vu8", SCM_UNSPECIFIED, scm_make_bytevector },
  { NULL }
};

static void
init_type_creator_table ()
{
  int i;
  for (i = 0; type_creator_table[i].type_name; i++)
    {
      SCM sym = scm_from_locale_symbol (type_creator_table[i].type_name);
      type_creator_table[i].type = scm_permanent_object (sym);
    }
}

static creator_proc *
type_to_creator (SCM type)
{
  int i;

  if (scm_is_eq (type, SCM_BOOL_T))
    return scm_make_vector;
  for (i = 0; type_creator_table[i].type_name; i++)
    if (scm_is_eq (type, type_creator_table[i].type))
      return type_creator_table[i].creator;

  scm_misc_error (NULL, "unknown array type: ~a", scm_list_1 (type));
}

static SCM
make_typed_vector (SCM type, size_t len)
{
  creator_proc *creator = type_to_creator (type);
  return creator (scm_from_size_t (len), SCM_UNDEFINED);
}

int
scm_is_array (SCM obj)
{
  return (SCM_I_ENCLOSED_ARRAYP (obj)
	  || SCM_I_ARRAYP (obj)
	  || scm_is_generalized_vector (obj));
}

int
scm_is_typed_array (SCM obj, SCM type)
{
  if (SCM_I_ENCLOSED_ARRAYP (obj))
    {
      /* Enclosed arrays are arrays but are not of any type.
      */
      return 0;
    }

  /* Get storage vector. 
   */
  if (SCM_I_ARRAYP (obj))
    obj = SCM_I_ARRAY_V (obj);

  /* It must be a generalized vector (which includes vectors, strings, etc).
   */
  if (!scm_is_generalized_vector (obj))
    return 0;

  return scm_is_eq (type, scm_i_generalized_vector_type (obj));
}

/* We keep the old 2-argument C prototype for a while although the old
   PROT argument is always ignored now.  C code should probably use
   scm_is_array or scm_is_typed_array anyway.
*/

SCM_DEFINE (scm_array_p, "array?", 1, 0, 0,
           (SCM obj),
	    "Return @code{#t} if the @var{obj} is an array, and @code{#f} if\n"
	    "not.")
#define FUNC_NAME s_scm_array_p
{
  return scm_from_bool (scm_is_array (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_typed_array_p, "typed-array?", 2, 0, 0,
           (SCM obj, SCM type),
	    "Return @code{#t} if the @var{obj} is an array of type\n"
	    "@var{type}, and @code{#f} if not.")
#define FUNC_NAME s_scm_typed_array_p
{
  return scm_from_bool (scm_is_typed_array (obj, type));
}
#undef FUNC_NAME

size_t
scm_c_array_rank (SCM array)
{
  scm_t_array_handle handle;
  size_t res;

  scm_array_get_handle (array, &handle);
  res = scm_array_handle_rank (&handle);
  scm_array_handle_release (&handle);
  return res;
}

SCM_DEFINE (scm_array_rank, "array-rank", 1, 0, 0, 
           (SCM array),
	    "Return the number of dimensions of the array @var{array.}\n")
#define FUNC_NAME s_scm_array_rank
{
  return scm_from_size_t (scm_c_array_rank (array));
}
#undef FUNC_NAME


SCM_DEFINE (scm_array_dimensions, "array-dimensions", 1, 0, 0, 
           (SCM ra),
	    "@code{array-dimensions} is similar to @code{array-shape} but replaces\n"
	    "elements with a @code{0} minimum with one greater than the maximum. So:\n"
	    "@lisp\n"
	    "(array-dimensions (make-array 'foo '(-1 3) 5)) @result{} ((-1 3) 5)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_array_dimensions
{
  scm_t_array_handle handle;
  scm_t_array_dim *s;
  SCM res = SCM_EOL;
  size_t k;
      
  scm_array_get_handle (ra, &handle);
  s = scm_array_handle_dims (&handle);
  k = scm_array_handle_rank (&handle);

  while (k--)
    res = scm_cons (s[k].lbnd
		    ? scm_cons2 (scm_from_ssize_t (s[k].lbnd),
				 scm_from_ssize_t (s[k].ubnd),
				 SCM_EOL)
		    : scm_from_ssize_t (1 + s[k].ubnd),
		    res);

  scm_array_handle_release (&handle);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_shared_array_root, "shared-array-root", 1, 0, 0, 
           (SCM ra),
	    "Return the root vector of a shared array.")
#define FUNC_NAME s_scm_shared_array_root
{
  if (SCM_I_ARRAYP (ra) || SCM_I_ENCLOSED_ARRAYP (ra))
    return SCM_I_ARRAY_V (ra);
  else if (scm_is_generalized_vector (ra))
    return ra;
  scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME


SCM_DEFINE (scm_shared_array_offset, "shared-array-offset", 1, 0, 0, 
           (SCM ra),
	    "Return the root vector index of the first element in the array.")
#define FUNC_NAME s_scm_shared_array_offset
{
  scm_t_array_handle handle;
  SCM res;

  scm_array_get_handle (ra, &handle);
  res = scm_from_size_t (handle.base);
  scm_array_handle_release (&handle);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_shared_array_increments, "shared-array-increments", 1, 0, 0, 
           (SCM ra),
	    "For each dimension, return the distance between elements in the root vector.")
#define FUNC_NAME s_scm_shared_array_increments
{
  scm_t_array_handle handle;
  SCM res = SCM_EOL;
  size_t k;
  scm_t_array_dim *s;

  scm_array_get_handle (ra, &handle);
  k = scm_array_handle_rank (&handle);
  s = scm_array_handle_dims (&handle);
  while (k--)
    res = scm_cons (scm_from_ssize_t (s[k].inc), res);
  scm_array_handle_release (&handle);
  return res;
}
#undef FUNC_NAME

SCM 
scm_i_make_array (int ndim, int enclosed)
{
  scm_t_bits tag = enclosed? scm_i_tc16_enclosed_array : scm_i_tc16_array;
  SCM ra;
  SCM_NEWSMOB(ra, ((scm_t_bits) ndim << 17) + tag,
              scm_gc_malloc ((sizeof (scm_i_t_array) +
			      ndim * sizeof (scm_t_array_dim)),
			     "array"));
  SCM_I_ARRAY_V (ra) = SCM_BOOL_F;
  return ra;
}

static char s_bad_spec[] = "Bad scm_array dimension";


/* Increments will still need to be set. */

static SCM 
scm_i_shap2ra (SCM args)
{
  scm_t_array_dim *s;
  SCM ra, spec, sp;
  int ndim = scm_ilength (args);
  if (ndim < 0)
    scm_misc_error (NULL, s_bad_spec, SCM_EOL);

  ra = scm_i_make_array (ndim, 0);
  SCM_I_ARRAY_BASE (ra) = 0;
  s = SCM_I_ARRAY_DIMS (ra);
  for (; !scm_is_null (args); s++, args = SCM_CDR (args))
    {
      spec = SCM_CAR (args);
      if (scm_is_integer (spec))
	{
	  if (scm_to_long (spec) < 0)
	    scm_misc_error (NULL, s_bad_spec, SCM_EOL);
	  s->lbnd = 0;
	  s->ubnd = scm_to_long (spec) - 1;
	  s->inc = 1;
	}
      else
	{
	  if (!scm_is_pair (spec) || !scm_is_integer (SCM_CAR (spec)))
	    scm_misc_error (NULL, s_bad_spec, SCM_EOL);
	  s->lbnd = scm_to_long (SCM_CAR (spec));
	  sp = SCM_CDR (spec);
	  if (!scm_is_pair (sp) 
	      || !scm_is_integer (SCM_CAR (sp))
	      || !scm_is_null (SCM_CDR (sp)))
	    scm_misc_error (NULL, s_bad_spec, SCM_EOL);
	  s->ubnd = scm_to_long (SCM_CAR (sp));
	  s->inc = 1;
	}
    }
  return ra;
}

SCM_DEFINE (scm_make_typed_array, "make-typed-array", 2, 0, 1,
	    (SCM type, SCM fill, SCM bounds),
	    "Create and return an array of type @var{type}.")
#define FUNC_NAME s_scm_make_typed_array
{
  size_t k, rlen = 1;
  scm_t_array_dim *s;
  creator_proc *creator;
  SCM ra;
  
  creator = type_to_creator (type);
  ra = scm_i_shap2ra (bounds);
  SCM_SET_ARRAY_CONTIGUOUS_FLAG (ra);
  s = SCM_I_ARRAY_DIMS (ra);
  k = SCM_I_ARRAY_NDIM (ra);

  while (k--)
    {
      s[k].inc = rlen;
      SCM_ASSERT_RANGE (1, bounds, s[k].lbnd <= s[k].ubnd + 1);
      rlen = (s[k].ubnd - s[k].lbnd + 1) * s[k].inc;
    }

  if (scm_is_eq (fill, SCM_UNSPECIFIED))
    fill = SCM_UNDEFINED;

  SCM_I_ARRAY_V (ra) = creator (scm_from_size_t (rlen), fill);

  if (1 == SCM_I_ARRAY_NDIM (ra) && 0 == SCM_I_ARRAY_BASE (ra))
    if (s->ubnd < s->lbnd || (0 == s->lbnd && 1 == s->inc))
      return SCM_I_ARRAY_V (ra);
  return ra;
}
#undef FUNC_NAME

SCM
scm_from_contiguous_typed_array (SCM type, SCM bounds, const void *bytes,
                                 size_t byte_len)
#define FUNC_NAME "scm_from_contiguous_typed_array"
{
  size_t k, rlen = 1;
  scm_t_array_dim *s;
  creator_proc *creator;
  SCM ra;
  scm_t_array_handle h;
  void *base;
  size_t sz;
  
  creator = type_to_creator (type);
  ra = scm_i_shap2ra (bounds);
  SCM_SET_ARRAY_CONTIGUOUS_FLAG (ra);
  s = SCM_I_ARRAY_DIMS (ra);
  k = SCM_I_ARRAY_NDIM (ra);

  while (k--)
    {
      s[k].inc = rlen;
      SCM_ASSERT_RANGE (1, bounds, s[k].lbnd <= s[k].ubnd + 1);
      rlen = (s[k].ubnd - s[k].lbnd + 1) * s[k].inc;
    }
  SCM_I_ARRAY_V (ra) = creator (scm_from_size_t (rlen), SCM_UNDEFINED);


  scm_array_get_handle (ra, &h);
  base = scm_array_handle_uniform_writable_elements (&h);
  sz = scm_array_handle_uniform_element_size (&h);
  scm_array_handle_release (&h);

  if (byte_len % sz)
    SCM_MISC_ERROR ("byte length not a multiple of the unit size", SCM_EOL);
  if (byte_len / sz != rlen)
    SCM_MISC_ERROR ("byte length and dimensions do not match", SCM_EOL);

  memcpy (base, bytes, byte_len);

  if (1 == SCM_I_ARRAY_NDIM (ra) && 0 == SCM_I_ARRAY_BASE (ra))
    if (s->ubnd < s->lbnd || (0 == s->lbnd && 1 == s->inc))
      return SCM_I_ARRAY_V (ra);
  return ra;
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_array, "make-array", 1, 0, 1,
	    (SCM fill, SCM bounds),
	    "Create and return an array.")
#define FUNC_NAME s_scm_make_array
{
  return scm_make_typed_array (SCM_BOOL_T, fill, bounds);
}
#undef FUNC_NAME

static void 
scm_i_ra_set_contp (SCM ra)
{
  size_t k = SCM_I_ARRAY_NDIM (ra);
  if (k)
    {
      long inc = SCM_I_ARRAY_DIMS (ra)[k - 1].inc;
      while (k--)
	{
	  if (inc != SCM_I_ARRAY_DIMS (ra)[k].inc)
	    {
	      SCM_CLR_ARRAY_CONTIGUOUS_FLAG (ra);
	      return;
	    }
	  inc *= (SCM_I_ARRAY_DIMS (ra)[k].ubnd 
		  - SCM_I_ARRAY_DIMS (ra)[k].lbnd + 1);
	}
    }
  SCM_SET_ARRAY_CONTIGUOUS_FLAG (ra);
}


SCM_DEFINE (scm_make_shared_array, "make-shared-array", 2, 0, 1,
           (SCM oldra, SCM mapfunc, SCM dims),
	    "@code{make-shared-array} can be used to create shared subarrays of other\n"
	    "arrays.  The @var{mapper} is a function that translates coordinates in\n"
	    "the new array into coordinates in the old array.  A @var{mapper} must be\n"
	    "linear, and its range must stay within the bounds of the old array, but\n"
	    "it can be otherwise arbitrary.  A simple example:\n"
	    "@lisp\n"
	    "(define fred (make-array #f 8 8))\n"
	    "(define freds-diagonal\n"
	    "  (make-shared-array fred (lambda (i) (list i i)) 8))\n"
	    "(array-set! freds-diagonal 'foo 3)\n"
	    "(array-ref fred 3 3) @result{} foo\n"
	    "(define freds-center\n"
	    "  (make-shared-array fred (lambda (i j) (list (+ 3 i) (+ 3 j))) 2 2))\n"
	    "(array-ref freds-center 0 0) @result{} foo\n"
	    "@end lisp")
#define FUNC_NAME s_scm_make_shared_array
{
  scm_t_array_handle old_handle;
  SCM ra;
  SCM inds, indptr;
  SCM imap;
  size_t k;
  ssize_t i;
  long old_base, old_min, new_min, old_max, new_max;
  scm_t_array_dim *s;

  SCM_VALIDATE_REST_ARGUMENT (dims);
  SCM_VALIDATE_PROC (2, mapfunc);
  ra = scm_i_shap2ra (dims);

  scm_array_get_handle (oldra, &old_handle);

  if (SCM_I_ARRAYP (oldra))
    {
      SCM_I_ARRAY_V (ra) = SCM_I_ARRAY_V (oldra);
      old_base = old_min = old_max = SCM_I_ARRAY_BASE (oldra);
      s = scm_array_handle_dims (&old_handle);
      k = scm_array_handle_rank (&old_handle);
      while (k--)
	{
	  if (s[k].inc > 0)
	    old_max += (s[k].ubnd - s[k].lbnd) * s[k].inc;
	  else
	    old_min += (s[k].ubnd - s[k].lbnd) * s[k].inc;
	}
    }
  else
    {
      SCM_I_ARRAY_V (ra) = oldra;
      old_base = old_min = 0;
      old_max = scm_c_generalized_vector_length (oldra) - 1;
    }

  inds = SCM_EOL;
  s = SCM_I_ARRAY_DIMS (ra);
  for (k = 0; k < SCM_I_ARRAY_NDIM (ra); k++)
    {
      inds = scm_cons (scm_from_long (s[k].lbnd), inds);
      if (s[k].ubnd < s[k].lbnd)
	{
	  if (1 == SCM_I_ARRAY_NDIM (ra))
	    ra = make_typed_vector (scm_array_type (ra), 0);
	  else
	    SCM_I_ARRAY_V (ra) = make_typed_vector (scm_array_type (ra), 0);
	  scm_array_handle_release (&old_handle);
	  return ra;
	}
    }

  imap = scm_apply_0 (mapfunc, scm_reverse (inds));
  i = scm_array_handle_pos (&old_handle, imap);
  SCM_I_ARRAY_BASE (ra) = new_min = new_max = i + old_base;
  indptr = inds;
  k = SCM_I_ARRAY_NDIM (ra);
  while (k--)
    {
      if (s[k].ubnd > s[k].lbnd)
	{
	  SCM_SETCAR (indptr, scm_sum (SCM_CAR (indptr), scm_from_int (1)));
	  imap = scm_apply_0 (mapfunc, scm_reverse (inds));
	  s[k].inc = scm_array_handle_pos (&old_handle, imap) - i;
	  i += s[k].inc;
	  if (s[k].inc > 0)
	    new_max += (s[k].ubnd - s[k].lbnd) * s[k].inc;
	  else
	    new_min += (s[k].ubnd - s[k].lbnd) * s[k].inc;
	}
      else
	s[k].inc = new_max - new_min + 1;	/* contiguous by default */
      indptr = SCM_CDR (indptr);
    }

  scm_array_handle_release (&old_handle);

  if (old_min > new_min || old_max < new_max)
    SCM_MISC_ERROR ("mapping out of range", SCM_EOL);
  if (1 == SCM_I_ARRAY_NDIM (ra) && 0 == SCM_I_ARRAY_BASE (ra))
    {
      SCM v = SCM_I_ARRAY_V (ra);
      size_t length = scm_c_generalized_vector_length (v);
      if (1 == s->inc && 0 == s->lbnd && length == 1 + s->ubnd)
	return v;
      if (s->ubnd < s->lbnd)
	return make_typed_vector (scm_array_type (ra), 0);
    }
  scm_i_ra_set_contp (ra);
  return ra;
}
#undef FUNC_NAME


/* args are RA . DIMS */
SCM_DEFINE (scm_transpose_array, "transpose-array", 1, 0, 1, 
           (SCM ra, SCM args),
	    "Return an array sharing contents with @var{array}, but with\n"
	    "dimensions arranged in a different order.  There must be one\n"
	    "@var{dim} argument for each dimension of @var{array}.\n"
	    "@var{dim0}, @var{dim1}, @dots{} should be integers between 0\n"
	    "and the rank of the array to be returned.  Each integer in that\n"
	    "range must appear at least once in the argument list.\n"
	    "\n"
	    "The values of @var{dim0}, @var{dim1}, @dots{} correspond to\n"
	    "dimensions in the array to be returned, their positions in the\n"
	    "argument list to dimensions of @var{array}.  Several @var{dim}s\n"
	    "may have the same value, in which case the returned array will\n"
	    "have smaller rank than @var{array}.\n"
	    "\n"
	    "@lisp\n"
	    "(transpose-array '#2((a b) (c d)) 1 0) @result{} #2((a c) (b d))\n"
	    "(transpose-array '#2((a b) (c d)) 0 0) @result{} #1(a d)\n"
	    "(transpose-array '#3(((a b c) (d e f)) ((1 2 3) (4 5 6))) 1 1 0) @result{}\n"
	    "                #2((a 4) (b 5) (c 6))\n"
	    "@end lisp")
#define FUNC_NAME s_scm_transpose_array
{
  SCM res, vargs;
  scm_t_array_dim *s, *r;
  int ndim, i, k;

  SCM_VALIDATE_REST_ARGUMENT (args);
  SCM_ASSERT (SCM_NIMP (ra), ra, SCM_ARG1, FUNC_NAME);

  if (scm_is_generalized_vector (ra))
    {
      /* Make sure that we are called with a single zero as
	 arguments. 
      */
      if (scm_is_null (args) || !scm_is_null (SCM_CDR (args)))
	SCM_WRONG_NUM_ARGS ();
      SCM_VALIDATE_INT_COPY (SCM_ARG2, SCM_CAR (args), i);
      SCM_ASSERT_RANGE (SCM_ARG2, SCM_CAR (args), i == 0);
      return ra;
    }

  if (SCM_I_ARRAYP (ra) || SCM_I_ENCLOSED_ARRAYP (ra))
    {
      vargs = scm_vector (args);
      if (SCM_SIMPLE_VECTOR_LENGTH (vargs) != SCM_I_ARRAY_NDIM (ra))
	SCM_WRONG_NUM_ARGS ();
      ndim = 0;
      for (k = 0; k < SCM_I_ARRAY_NDIM (ra); k++)
	{
	  i = scm_to_signed_integer (SCM_SIMPLE_VECTOR_REF (vargs, k),
				     0, SCM_I_ARRAY_NDIM(ra));
	  if (ndim < i)
	    ndim = i;
	}
      ndim++;
      res = scm_i_make_array (ndim, 0);
      SCM_I_ARRAY_V (res) = SCM_I_ARRAY_V (ra);
      SCM_I_ARRAY_BASE (res) = SCM_I_ARRAY_BASE (ra);
      for (k = ndim; k--;)
	{
	  SCM_I_ARRAY_DIMS (res)[k].lbnd = 0;
	  SCM_I_ARRAY_DIMS (res)[k].ubnd = -1;
	}
      for (k = SCM_I_ARRAY_NDIM (ra); k--;)
	{
	  i = scm_to_int (SCM_SIMPLE_VECTOR_REF (vargs, k));
	  s = &(SCM_I_ARRAY_DIMS (ra)[k]);
	  r = &(SCM_I_ARRAY_DIMS (res)[i]);
	  if (r->ubnd < r->lbnd)
	    {
	      r->lbnd = s->lbnd;
	      r->ubnd = s->ubnd;
	      r->inc = s->inc;
	      ndim--;
	    }
	  else
	    {
	      if (r->ubnd > s->ubnd)
		r->ubnd = s->ubnd;
	      if (r->lbnd < s->lbnd)
		{
		  SCM_I_ARRAY_BASE (res) += (s->lbnd - r->lbnd) * r->inc;
		  r->lbnd = s->lbnd;
		}
	      r->inc += s->inc;
	    }
	}
      if (ndim > 0)
	SCM_MISC_ERROR ("bad argument list", SCM_EOL);
      scm_i_ra_set_contp (res);
      return res;
    }

  scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME

/* args are RA . AXES */
SCM_DEFINE (scm_enclose_array, "enclose-array", 1, 0, 1, 
           (SCM ra, SCM axes),
	    "@var{dim0}, @var{dim1} @dots{} should be nonnegative integers less than\n"
	    "the rank of @var{array}.  @var{enclose-array} returns an array\n"
	    "resembling an array of shared arrays.  The dimensions of each shared\n"
	    "array are the same as the @var{dim}th dimensions of the original array,\n"
	    "the dimensions of the outer array are the same as those of the original\n"
	    "array that did not match a @var{dim}.\n\n"
	    "An enclosed array is not a general Scheme array.  Its elements may not\n"
	    "be set using @code{array-set!}.  Two references to the same element of\n"
	    "an enclosed array will be @code{equal?} but will not in general be\n"
	    "@code{eq?}.  The value returned by @var{array-prototype} when given an\n"
	    "enclosed array is unspecified.\n\n"
	    "examples:\n"
	    "@lisp\n"
	    "(enclose-array '#3(((a b c) (d e f)) ((1 2 3) (4 5 6))) 1) @result{}\n"
	    "   #<enclosed-array (#1(a d) #1(b e) #1(c f)) (#1(1 4) #1(2 5) #1(3 6))>\n\n"
	    "(enclose-array '#3(((a b c) (d e f)) ((1 2 3) (4 5 6))) 1 0) @result{}\n"
	    "   #<enclosed-array #2((a 1) (d 4)) #2((b 2) (e 5)) #2((c 3) (f 6))>\n"
	    "@end lisp")
#define FUNC_NAME s_scm_enclose_array
{
  SCM axv, res, ra_inr;
  const char *c_axv;
  scm_t_array_dim vdim, *s = &vdim;
  int ndim, j, k, ninr, noutr;

  SCM_VALIDATE_REST_ARGUMENT (axes);
  if (scm_is_null (axes))
    axes = scm_cons ((SCM_I_ARRAYP (ra) ? scm_from_size_t (SCM_I_ARRAY_NDIM (ra) - 1) : SCM_INUM0), SCM_EOL);
  ninr = scm_ilength (axes);
  if (ninr < 0)
    SCM_WRONG_NUM_ARGS ();
  ra_inr = scm_i_make_array (ninr, 0);

  if (scm_is_generalized_vector (ra))
    {
      s->lbnd = 0;
      s->ubnd = scm_c_generalized_vector_length (ra) - 1;
      s->inc = 1;
      SCM_I_ARRAY_V (ra_inr) = ra;
      SCM_I_ARRAY_BASE (ra_inr) = 0;
      ndim = 1;
    }
  else if (SCM_I_ARRAYP (ra))
    {
      s = SCM_I_ARRAY_DIMS (ra);
      SCM_I_ARRAY_V (ra_inr) = SCM_I_ARRAY_V (ra);
      SCM_I_ARRAY_BASE (ra_inr) = SCM_I_ARRAY_BASE (ra);
      ndim = SCM_I_ARRAY_NDIM (ra);
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, ra, "array");

  noutr = ndim - ninr;
  if (noutr < 0)
    SCM_WRONG_NUM_ARGS ();
  axv = scm_make_string (scm_from_int (ndim), SCM_MAKE_CHAR (0));
  res = scm_i_make_array (noutr, 1);
  SCM_I_ARRAY_BASE (res) = SCM_I_ARRAY_BASE (ra_inr);
  SCM_I_ARRAY_V (res) = ra_inr;
  for (k = 0; k < ninr; k++, axes = SCM_CDR (axes))
    {
      if (!scm_is_integer (SCM_CAR (axes)))
	SCM_MISC_ERROR ("bad axis", SCM_EOL);
      j = scm_to_int (SCM_CAR (axes));
      SCM_I_ARRAY_DIMS (ra_inr)[k].lbnd = s[j].lbnd;
      SCM_I_ARRAY_DIMS (ra_inr)[k].ubnd = s[j].ubnd;
      SCM_I_ARRAY_DIMS (ra_inr)[k].inc = s[j].inc;
      scm_c_string_set_x (axv, j, SCM_MAKE_CHAR (1));
    }
  c_axv = scm_i_string_chars (axv);
  for (j = 0, k = 0; k < noutr; k++, j++)
    {
      while (c_axv[j])
	j++;
      SCM_I_ARRAY_DIMS (res)[k].lbnd = s[j].lbnd;
      SCM_I_ARRAY_DIMS (res)[k].ubnd = s[j].ubnd;
      SCM_I_ARRAY_DIMS (res)[k].inc = s[j].inc;
    }
  scm_remember_upto_here_1 (axv);
  scm_i_ra_set_contp (ra_inr);
  scm_i_ra_set_contp (res);
  return res;
}
#undef FUNC_NAME



SCM_DEFINE (scm_array_in_bounds_p, "array-in-bounds?", 1, 0, 1, 
           (SCM v, SCM args),
	    "Return @code{#t} if its arguments would be acceptable to\n"
	    "@code{array-ref}.")
#define FUNC_NAME s_scm_array_in_bounds_p
{
  SCM res = SCM_BOOL_T;

  SCM_VALIDATE_REST_ARGUMENT (args);

  if (SCM_I_ARRAYP (v) || SCM_I_ENCLOSED_ARRAYP (v))
    {
      size_t k, ndim = SCM_I_ARRAY_NDIM (v);
      scm_t_array_dim *s = SCM_I_ARRAY_DIMS (v);

      for (k = 0; k < ndim; k++)
	{
	  long ind;

	  if (!scm_is_pair (args))
	    SCM_WRONG_NUM_ARGS ();
	  ind = scm_to_long (SCM_CAR (args));
	  args = SCM_CDR (args);

	  if (ind < s[k].lbnd || ind > s[k].ubnd)
	    {
	      res = SCM_BOOL_F;
	      /* We do not stop the checking after finding a violation
		 since we want to validate the type-correctness and
		 number of arguments in any case.
	      */
	    }
	}
    }
  else if (scm_is_generalized_vector (v))
    {
      /* Since real arrays have been covered above, all generalized
	 vectors are guaranteed to be zero-origin here.
      */

      long ind;

      if (!scm_is_pair (args))
	SCM_WRONG_NUM_ARGS ();
      ind = scm_to_long (SCM_CAR (args));
      args = SCM_CDR (args);
      res = scm_from_bool (ind >= 0
			   && ind < scm_c_generalized_vector_length (v));
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "array");

  if (!scm_is_null (args))
    SCM_WRONG_NUM_ARGS ();

  return res;
}
#undef FUNC_NAME

SCM 
scm_i_cvref (SCM v, size_t pos, int enclosed)
{
  if (enclosed)
    {
      int k = SCM_I_ARRAY_NDIM (v);
      SCM res = scm_i_make_array (k, 0);
      SCM_I_ARRAY_V (res) = SCM_I_ARRAY_V (v);
      SCM_I_ARRAY_BASE (res) = pos;
      while (k--)
	{
	  SCM_I_ARRAY_DIMS (res)[k].ubnd = SCM_I_ARRAY_DIMS (v)[k].ubnd;
	  SCM_I_ARRAY_DIMS (res)[k].lbnd = SCM_I_ARRAY_DIMS (v)[k].lbnd;
	  SCM_I_ARRAY_DIMS (res)[k].inc = SCM_I_ARRAY_DIMS (v)[k].inc;
	}
      return res;
    }
  else
    return scm_c_generalized_vector_ref (v, pos);
}

SCM_DEFINE (scm_array_ref, "array-ref", 1, 0, 1,
           (SCM v, SCM args),
	    "Return the element at the @code{(index1, index2)} element in\n"
	    "@var{array}.")
#define FUNC_NAME s_scm_array_ref
{
  scm_t_array_handle handle;
  SCM res;

  scm_array_get_handle (v, &handle);
  res = scm_array_handle_ref (&handle, scm_array_handle_pos (&handle, args));
  scm_array_handle_release (&handle);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_array_set_x, "array-set!", 2, 0, 1, 
           (SCM v, SCM obj, SCM args),
	    "Set the element at the @code{(index1, index2)} element in @var{array} to\n"
	    "@var{new-value}.  The value returned by array-set! is unspecified.")
#define FUNC_NAME s_scm_array_set_x           
{
  scm_t_array_handle handle;

  scm_array_get_handle (v, &handle);
  scm_array_handle_set (&handle, scm_array_handle_pos (&handle, args), obj);
  scm_array_handle_release (&handle);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* attempts to unroll an array into a one-dimensional array.
   returns the unrolled array or #f if it can't be done.  */
  /* if strict is not SCM_UNDEFINED, return #f if returned array
		     wouldn't have contiguous elements.  */
SCM_DEFINE (scm_array_contents, "array-contents", 1, 1, 0,
           (SCM ra, SCM strict),
	    "If @var{array} may be @dfn{unrolled} into a one dimensional shared array\n"
	    "without changing their order (last subscript changing fastest), then\n"
	    "@code{array-contents} returns that shared array, otherwise it returns\n"
	    "@code{#f}.  All arrays made by @var{make-array} and\n"
	    "@var{make-uniform-array} may be unrolled, some arrays made by\n"
	    "@var{make-shared-array} may not be.\n\n"
	    "If the optional argument @var{strict} is provided, a shared array will\n"
	    "be returned only if its elements are stored internally contiguous in\n"
	    "memory.")
#define FUNC_NAME s_scm_array_contents
{
  SCM sra;

  if (scm_is_generalized_vector (ra))
    return ra;

  if (SCM_I_ARRAYP (ra))
    {
      size_t k, ndim = SCM_I_ARRAY_NDIM (ra), len = 1;
      if (!SCM_I_ARRAYP (ra) || !SCM_I_ARRAY_CONTP (ra))
	return SCM_BOOL_F;
      for (k = 0; k < ndim; k++)
	len *= SCM_I_ARRAY_DIMS (ra)[k].ubnd - SCM_I_ARRAY_DIMS (ra)[k].lbnd + 1;
      if (!SCM_UNBNDP (strict))
	{
	  if (ndim && (1 != SCM_I_ARRAY_DIMS (ra)[ndim - 1].inc))
	    return SCM_BOOL_F;
	  if (scm_is_bitvector (SCM_I_ARRAY_V (ra)))
	    {
	      if (len != scm_c_bitvector_length (SCM_I_ARRAY_V (ra)) ||
		  SCM_I_ARRAY_BASE (ra) % SCM_LONG_BIT ||
		  len % SCM_LONG_BIT)
		return SCM_BOOL_F;
	    }
	}
      
      {
	SCM v = SCM_I_ARRAY_V (ra);
	size_t length = scm_c_generalized_vector_length (v);
	if ((len == length) && 0 == SCM_I_ARRAY_BASE (ra) && SCM_I_ARRAY_DIMS (ra)->inc)
	  return v;
      }
      
      sra = scm_i_make_array (1, 0);
      SCM_I_ARRAY_DIMS (sra)->lbnd = 0;
      SCM_I_ARRAY_DIMS (sra)->ubnd = len - 1;
      SCM_I_ARRAY_V (sra) = SCM_I_ARRAY_V (ra);
      SCM_I_ARRAY_BASE (sra) = SCM_I_ARRAY_BASE (ra);
      SCM_I_ARRAY_DIMS (sra)->inc = (ndim ? SCM_I_ARRAY_DIMS (ra)[ndim - 1].inc : 1);
      return sra;
    }
  else if (SCM_I_ENCLOSED_ARRAYP (ra))
    scm_wrong_type_arg_msg (NULL, 0, ra, "non-enclosed array");
  else
    scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME


SCM 
scm_ra2contig (SCM ra, int copy)
{
  SCM ret;
  long inc = 1;
  size_t k, len = 1;
  for (k = SCM_I_ARRAY_NDIM (ra); k--;)
    len *= SCM_I_ARRAY_DIMS (ra)[k].ubnd - SCM_I_ARRAY_DIMS (ra)[k].lbnd + 1;
  k = SCM_I_ARRAY_NDIM (ra);
  if (SCM_I_ARRAY_CONTP (ra) && ((0 == k) || (1 == SCM_I_ARRAY_DIMS (ra)[k - 1].inc)))
    {
      if (!scm_is_bitvector (SCM_I_ARRAY_V (ra)))
	return ra;
      if ((len == scm_c_bitvector_length (SCM_I_ARRAY_V (ra)) &&
	   0 == SCM_I_ARRAY_BASE (ra) % SCM_LONG_BIT &&
	   0 == len % SCM_LONG_BIT))
	return ra;
    }
  ret = scm_i_make_array (k, 0);
  SCM_I_ARRAY_BASE (ret) = 0;
  while (k--)
    {
      SCM_I_ARRAY_DIMS (ret)[k].lbnd = SCM_I_ARRAY_DIMS (ra)[k].lbnd;
      SCM_I_ARRAY_DIMS (ret)[k].ubnd = SCM_I_ARRAY_DIMS (ra)[k].ubnd;
      SCM_I_ARRAY_DIMS (ret)[k].inc = inc;
      inc *= SCM_I_ARRAY_DIMS (ra)[k].ubnd - SCM_I_ARRAY_DIMS (ra)[k].lbnd + 1;
    }
  SCM_I_ARRAY_V (ret) = make_typed_vector (scm_array_type (ra), inc);
  if (copy)
    scm_array_copy_x (ra, ret);
  return ret;
}



SCM_DEFINE (scm_uniform_array_read_x, "uniform-array-read!", 1, 3, 0,
           (SCM ura, SCM port_or_fd, SCM start, SCM end),
	    "@deffnx {Scheme Procedure} uniform-vector-read! uve [port-or-fdes] [start] [end]\n"
	    "Attempt to read all elements of @var{ura}, in lexicographic order, as\n"
	    "binary objects from @var{port-or-fdes}.\n"
	    "If an end of file is encountered,\n"
	    "the objects up to that point are put into @var{ura}\n"
	    "(starting at the beginning) and the remainder of the array is\n"
	    "unchanged.\n\n"
	    "The optional arguments @var{start} and @var{end} allow\n"
	    "a specified region of a vector (or linearized array) to be read,\n"
	    "leaving the remainder of the vector unchanged.\n\n"
	    "@code{uniform-array-read!} returns the number of objects read.\n"
	    "@var{port-or-fdes} may be omitted, in which case it defaults to the value\n"
	    "returned by @code{(current-input-port)}.")
#define FUNC_NAME s_scm_uniform_array_read_x
{
  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_current_input_port ();

  if (scm_is_uniform_vector (ura))
    {
      return scm_uniform_vector_read_x (ura, port_or_fd, start, end);
    }
  else if (SCM_I_ARRAYP (ura))
    {
      size_t base, vlen, cstart, cend;
      SCM cra, ans;
      
      cra = scm_ra2contig (ura, 0);
      base = SCM_I_ARRAY_BASE (cra);
      vlen = SCM_I_ARRAY_DIMS (cra)->inc *
	(SCM_I_ARRAY_DIMS (cra)->ubnd - SCM_I_ARRAY_DIMS (cra)->lbnd + 1);

      cstart = 0;
      cend = vlen;
      if (!SCM_UNBNDP (start))
	{
	  cstart = scm_to_unsigned_integer (start, 0, vlen);
	  if (!SCM_UNBNDP (end))
	    cend = scm_to_unsigned_integer (end, cstart, vlen);
	}

      ans = scm_uniform_vector_read_x (SCM_I_ARRAY_V (cra), port_or_fd,
				       scm_from_size_t (base + cstart),
				       scm_from_size_t (base + cend));

      if (!scm_is_eq (cra, ura))
	scm_array_copy_x (cra, ura);
      return ans;
    }
  else if (SCM_I_ENCLOSED_ARRAYP (ura))
    scm_wrong_type_arg_msg (NULL, 0, ura, "non-enclosed array");    
  else
    scm_wrong_type_arg_msg (NULL, 0, ura, "array");
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_array_write, "uniform-array-write", 1, 3, 0,
           (SCM ura, SCM port_or_fd, SCM start, SCM end),
	    "Writes all elements of @var{ura} as binary objects to\n"
	    "@var{port-or-fdes}.\n\n"
	    "The optional arguments @var{start}\n"
	    "and @var{end} allow\n"
	    "a specified region of a vector (or linearized array) to be written.\n\n"
	    "The number of objects actually written is returned.\n"
	    "@var{port-or-fdes} may be\n"
	    "omitted, in which case it defaults to the value returned by\n"
	    "@code{(current-output-port)}.")
#define FUNC_NAME s_scm_uniform_array_write
{
  if (SCM_UNBNDP (port_or_fd))
    port_or_fd = scm_current_output_port ();

  if (scm_is_uniform_vector (ura))
    {
      return scm_uniform_vector_write (ura, port_or_fd, start, end);
    }
  else if (SCM_I_ARRAYP (ura))
    {
      size_t base, vlen, cstart, cend;
      SCM cra, ans;
      
      cra = scm_ra2contig (ura, 1);
      base = SCM_I_ARRAY_BASE (cra);
      vlen = SCM_I_ARRAY_DIMS (cra)->inc *
	(SCM_I_ARRAY_DIMS (cra)->ubnd - SCM_I_ARRAY_DIMS (cra)->lbnd + 1);

      cstart = 0;
      cend = vlen;
      if (!SCM_UNBNDP (start))
	{
	  cstart = scm_to_unsigned_integer (start, 0, vlen);
	  if (!SCM_UNBNDP (end))
	    cend = scm_to_unsigned_integer (end, cstart, vlen);
	}

      ans = scm_uniform_vector_write (SCM_I_ARRAY_V (cra), port_or_fd,
				      scm_from_size_t (base + cstart),
				      scm_from_size_t (base + cend));

      return ans;
    }
  else if (SCM_I_ENCLOSED_ARRAYP (ura))
    scm_wrong_type_arg_msg (NULL, 0, ura, "non-enclosed array");    
  else
    scm_wrong_type_arg_msg (NULL, 0, ura, "array");
}
#undef FUNC_NAME


static SCM 
ra2l (SCM ra, unsigned long base, unsigned long k)
{
  SCM res = SCM_EOL;
  long inc;
  size_t i;
  int enclosed = SCM_I_ENCLOSED_ARRAYP (ra);
  
  if (k == SCM_I_ARRAY_NDIM (ra))
    return scm_i_cvref (SCM_I_ARRAY_V (ra), base, enclosed);

  inc = SCM_I_ARRAY_DIMS (ra)[k].inc;
  if (SCM_I_ARRAY_DIMS (ra)[k].ubnd < SCM_I_ARRAY_DIMS (ra)[k].lbnd)
    return SCM_EOL;
  i = base + (1 + SCM_I_ARRAY_DIMS (ra)[k].ubnd - SCM_I_ARRAY_DIMS (ra)[k].lbnd) * inc;
  do
    {
      i -= inc;
      res = scm_cons (ra2l (ra, i, k + 1), res);
    }
  while (i != base);
  return res;
}


SCM_DEFINE (scm_array_to_list, "array->list", 1, 0, 0, 
           (SCM v),
	    "Return a list consisting of all the elements, in order, of\n"
	    "@var{array}.")
#define FUNC_NAME s_scm_array_to_list
{
  if (scm_is_generalized_vector (v))
    return scm_generalized_vector_to_list (v);
  else if (SCM_I_ARRAYP (v) || SCM_I_ENCLOSED_ARRAYP (v))
    return ra2l (v, SCM_I_ARRAY_BASE (v), 0);

  scm_wrong_type_arg_msg (NULL, 0, v, "array");
}
#undef FUNC_NAME


static void l2ra (SCM lst, scm_t_array_handle *handle, ssize_t pos, size_t k);

SCM_DEFINE (scm_list_to_typed_array, "list->typed-array", 3, 0, 0,
           (SCM type, SCM shape, SCM lst),
	    "Return an array of the type @var{type}\n"
	    "with elements the same as those of @var{lst}.\n"
	    "\n"
	    "The argument @var{shape} determines the number of dimensions\n"
	    "of the array and their shape.  It is either an exact integer,\n"
	    "giving the\n"
	    "number of dimensions directly, or a list whose length\n"
	    "specifies the number of dimensions and each element specified\n"
	    "the lower and optionally the upper bound of the corresponding\n"
	    "dimension.\n"
	    "When the element is list of two elements, these elements\n"
	    "give the lower and upper bounds.  When it is an exact\n"
	    "integer, it gives only the lower bound.")
#define FUNC_NAME s_scm_list_to_typed_array
{
  SCM row;
  SCM ra;
  scm_t_array_handle handle;

  row = lst;
  if (scm_is_integer (shape))
    {
      size_t k = scm_to_size_t (shape);
      shape = SCM_EOL;
      while (k-- > 0)
	{
	  shape = scm_cons (scm_length (row), shape);
	  if (k > 0 && !scm_is_null (row))
	    row = scm_car (row);
	}
    }
  else
    {
      SCM shape_spec = shape;
      shape = SCM_EOL;
      while (1)
	{
	  SCM spec = scm_car (shape_spec);
	  if (scm_is_pair (spec))
	    shape = scm_cons (spec, shape);
	  else
	    shape = scm_cons (scm_list_2 (spec,
					  scm_sum (scm_sum (spec,
							    scm_length (row)),
						   scm_from_int (-1))),
			      shape);
	  shape_spec = scm_cdr (shape_spec);
	  if (scm_is_pair (shape_spec))
	    {
	      if (!scm_is_null (row))
		row = scm_car (row);
	    }
	  else
	    break;
	}
    }

  ra = scm_make_typed_array (type, SCM_UNSPECIFIED,
			     scm_reverse_x (shape, SCM_EOL));

  scm_array_get_handle (ra, &handle);
  l2ra (lst, &handle, 0, 0);
  scm_array_handle_release (&handle);

  return ra;
}
#undef FUNC_NAME

SCM_DEFINE (scm_list_to_array, "list->array", 2, 0, 0,
           (SCM ndim, SCM lst),
	    "Return an array with elements the same as those of @var{lst}.")
#define FUNC_NAME s_scm_list_to_array
{
  return scm_list_to_typed_array (SCM_BOOL_T, ndim, lst);
}
#undef FUNC_NAME

static void
l2ra (SCM lst, scm_t_array_handle *handle, ssize_t pos, size_t k)
{
  if (k == scm_array_handle_rank (handle))
    scm_array_handle_set (handle, pos, lst);
  else
    {
      scm_t_array_dim *dim = scm_array_handle_dims (handle) + k;
      ssize_t inc = dim->inc;
      size_t len = 1 + dim->ubnd - dim->lbnd, n;
      char *errmsg = NULL;

      n = len;
      while (n > 0 && scm_is_pair (lst))
	{
	  l2ra (SCM_CAR (lst), handle, pos, k + 1);
	  pos += inc;
	  lst = SCM_CDR (lst);
	  n -= 1;
	}
      if (n != 0)
	errmsg = "too few elements for array dimension ~a, need ~a";
      if (!scm_is_null (lst))
	errmsg = "too many elements for array dimension ~a, want ~a";
      if (errmsg)
	scm_misc_error (NULL, errmsg, scm_list_2 (scm_from_ulong (k),
						  scm_from_size_t (len)));
    }
}

/* Print dimension DIM of ARRAY.
 */

static int
scm_i_print_array_dimension (SCM array, int dim, int base, int enclosed,
			     SCM port, scm_print_state *pstate)
{
  scm_t_array_dim *dim_spec = SCM_I_ARRAY_DIMS (array) + dim;
  long idx;

  scm_putc ('(', port);

  for (idx = dim_spec->lbnd; idx <= dim_spec->ubnd; idx++)
    {
      if (dim < SCM_I_ARRAY_NDIM(array)-1)
	scm_i_print_array_dimension (array, dim+1, base, enclosed, 
				     port, pstate);
      else
	scm_iprin1 (scm_i_cvref (SCM_I_ARRAY_V (array), base, enclosed), 
		    port, pstate);
      if (idx < dim_spec->ubnd)
	scm_putc (' ', port);
      base += dim_spec->inc;
    }

  scm_putc (')', port);
  return 1;
}

/* Print an array.  (Only for strict arrays, not for generalized vectors.)
*/

static int
scm_i_print_array (SCM array, SCM port, scm_print_state *pstate)
{
  long ndim = SCM_I_ARRAY_NDIM (array);
  scm_t_array_dim *dim_specs = SCM_I_ARRAY_DIMS (array);
  SCM v = SCM_I_ARRAY_V (array);
  unsigned long base = SCM_I_ARRAY_BASE (array);
  long i;
  int print_lbnds = 0, zero_size = 0, print_lens = 0;

  scm_putc ('#', port);
  if (ndim != 1 || dim_specs[0].lbnd != 0)
    scm_intprint (ndim, 10, port);
  if (scm_is_uniform_vector (v))
    scm_puts (scm_i_uniform_vector_tag (v), port);
  else if (scm_is_bitvector (v))
    scm_puts ("b", port);
  else if (scm_is_string (v))
    scm_puts ("a", port);
  else if (!scm_is_vector (v))
    scm_puts ("?", port);
  
  for (i = 0; i < ndim; i++)
    {
      if (dim_specs[i].lbnd != 0)
	print_lbnds = 1;
      if (dim_specs[i].ubnd - dim_specs[i].lbnd + 1 == 0)
	zero_size = 1;
      else if (zero_size)
	print_lens = 1;
    }

  if (print_lbnds || print_lens)
    for (i = 0; i < ndim; i++)
      {
	if (print_lbnds)
	  {
	    scm_putc ('@', port);
	    scm_intprint (dim_specs[i].lbnd, 10, port);
	  }
	if (print_lens)
	  {
	    scm_putc (':', port);
	    scm_intprint (dim_specs[i].ubnd - dim_specs[i].lbnd + 1,
			  10, port);
	  }
      }

  if (ndim == 0)
    {
      /* Rank zero arrays, which are really just scalars, are printed
	 specially.  The consequent way would be to print them as

            #0 OBJ

         where OBJ is the printed representation of the scalar, but we
         print them instead as

            #0(OBJ)

         to make them look less strange.

	 Just printing them as

            OBJ

         would be correct in a way as well, but zero rank arrays are
         not really the same as Scheme values since they are boxed and
         can be modified with array-set!, say.
      */
      scm_putc ('(', port);
      scm_iprin1 (scm_i_cvref (v, base, 0), port, pstate);
      scm_putc (')', port);
      return 1;
    }
  else
    return scm_i_print_array_dimension (array, 0, base, 0, port, pstate);
}

static int
scm_i_print_enclosed_array (SCM array, SCM port, scm_print_state *pstate)
{
  size_t base;

  scm_putc ('#', port);
  base = SCM_I_ARRAY_BASE (array);
  scm_puts ("<enclosed-array ", port);
  scm_i_print_array_dimension (array, 0, base, 1, port, pstate);
  scm_putc ('>', port);
  return 1;
}

/* Read an array.  This function can also read vectors and uniform
   vectors.  Also, the conflict between '#f' and '#f32' and '#f64' is
   handled here.

   C is the first character read after the '#'.
*/

static SCM
tag_to_type (const char *tag, SCM port)
{
  if (*tag == '\0')
    return SCM_BOOL_T;
  else
    return scm_from_locale_symbol (tag);
}

static int
read_decimal_integer (SCM port, int c, ssize_t *resp)
{
  ssize_t sign = 1;
  ssize_t res = 0;
  int got_it = 0;

  if (c == '-')
    {
      sign = -1;
      c = scm_getc (port);
    }

  while ('0' <= c && c <= '9')
    {
      res = 10*res + c-'0';
      got_it = 1;
      c = scm_getc (port);
    }

  if (got_it)
    *resp = sign * res;
  return c;
}

SCM
scm_i_read_array (SCM port, int c)
{
  ssize_t rank;
  int got_rank;
  char tag[80];
  int tag_len;

  SCM shape = SCM_BOOL_F, elements;

  /* XXX - shortcut for ordinary vectors.  Shouldn't be necessary but
     the array code can not deal with zero-length dimensions yet, and
     we want to allow zero-length vectors, of course.
  */
  if (c == '(')
    {
      scm_ungetc (c, port);
      return scm_vector (scm_read (port));
    }

  /* Disambiguate between '#f' and uniform floating point vectors.
   */
  if (c == 'f')
    {
      c = scm_getc (port);
      if (c != '3' && c != '6')
	{
	  if (c != EOF)
	    scm_ungetc (c, port);
	  return SCM_BOOL_F;
	}
      rank = 1;
      got_rank = 1;
      tag[0] = 'f';
      tag_len = 1;
      goto continue_reading_tag;
    }

  /* Read rank. 
   */
  rank = 1;
  c = read_decimal_integer (port, c, &rank);
  if (rank < 0)
    scm_i_input_error (NULL, port, "array rank must be non-negative",
		       SCM_EOL);

  /* Read tag. 
   */
  tag_len = 0;
 continue_reading_tag:
  while (c != EOF && c != '(' && c != '@' && c != ':' && tag_len < 80)
    {
      tag[tag_len++] = c;
      c = scm_getc (port);
    }
  tag[tag_len] = '\0';
  
  /* Read shape. 
   */
  if (c == '@' || c == ':')
    {
      shape = SCM_EOL;
      
      do
	{
	  ssize_t lbnd = 0, len = 0;
	  SCM s;

	  if (c == '@')
	    {
	      c = scm_getc (port);
	      c = read_decimal_integer (port, c, &lbnd);
	    }
	  
	  s = scm_from_ssize_t (lbnd);

	  if (c == ':')
	    {
	      c = scm_getc (port);
	      c = read_decimal_integer (port, c, &len);
	      if (len < 0)
		scm_i_input_error (NULL, port,
				   "array length must be non-negative",
				   SCM_EOL);

	      s = scm_list_2 (s, scm_from_ssize_t (lbnd+len-1));
	    }

	  shape = scm_cons (s, shape);
	} while (c == '@' || c == ':');

      shape = scm_reverse_x (shape, SCM_EOL);
    }

  /* Read nested lists of elements.
   */
  if (c != '(')
    scm_i_input_error (NULL, port,
		       "missing '(' in vector or array literal",
		       SCM_EOL);
  scm_ungetc (c, port);
  elements = scm_read (port);

  if (scm_is_false (shape))
    shape = scm_from_ssize_t (rank);
  else if (scm_ilength (shape) != rank)
    scm_i_input_error 
      (NULL, port,
       "the number of shape specifications must match the array rank",
       SCM_EOL);

  /* Handle special print syntax of rank zero arrays; see
     scm_i_print_array for a rationale.
  */
  if (rank == 0)
    {
      if (!scm_is_pair (elements))
	scm_i_input_error (NULL, port,
			   "too few elements in array literal, need 1",
			   SCM_EOL);
      if (!scm_is_null (SCM_CDR (elements)))
	scm_i_input_error (NULL, port,
			   "too many elements in array literal, want 1",
			   SCM_EOL);
      elements = SCM_CAR (elements);
    }

  /* Construct array. 
   */
  return scm_list_to_typed_array (tag_to_type (tag, port), shape, elements);
}

SCM_DEFINE (scm_array_type, "array-type", 1, 0, 0, 
           (SCM ra),
	    "")
#define FUNC_NAME s_scm_array_type
{
  if (SCM_I_ARRAYP (ra))
    return scm_i_generalized_vector_type (SCM_I_ARRAY_V (ra));
  else if (scm_is_generalized_vector (ra))
    return scm_i_generalized_vector_type (ra);
  else if (SCM_I_ENCLOSED_ARRAYP (ra))
    scm_wrong_type_arg_msg (NULL, 0, ra, "non-enclosed array");
  else
    scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME

static SCM
array_mark (SCM ptr)
{
  return SCM_I_ARRAY_V (ptr);
}

static size_t
array_free (SCM ptr)
{
  scm_gc_free (SCM_I_ARRAY_MEM (ptr),
	       (sizeof (scm_i_t_array) 
		+ SCM_I_ARRAY_NDIM (ptr) * sizeof (scm_t_array_dim)),
	       "array");
  return 0;
}

static SCM
array_handle_ref (scm_t_array_handle *h, size_t pos)
{
  return scm_c_generalized_vector_ref (SCM_I_ARRAY_V (h->array), pos);
}

static void
array_handle_set (scm_t_array_handle *h, size_t pos, SCM val)
{
  scm_c_generalized_vector_set_x (SCM_I_ARRAY_V (h->array), pos, val);
}

/* FIXME: should be handle for vect? maybe not, because of dims */
static void
array_get_handle (SCM array, scm_t_array_handle *h)
{
  scm_t_array_handle vh;
  scm_array_get_handle (SCM_I_ARRAY_V (array), &vh);
  h->element_type = vh.element_type;
  h->elements = vh.elements;
  h->writable_elements = vh.writable_elements;
  scm_array_handle_release (&vh);

  h->dims = SCM_I_ARRAY_DIMS (array);
  h->ndims = SCM_I_ARRAY_NDIM (array);
  h->base = SCM_I_ARRAY_BASE (array);
}

SCM_ARRAY_IMPLEMENTATION (scm_i_tc16_array, 0xffff,
                          array_handle_ref, array_handle_set,
                          array_get_handle);

void
scm_init_arrays ()
{
  scm_i_tc16_array = scm_make_smob_type ("array", 0);
  scm_set_smob_mark (scm_i_tc16_array, array_mark);
  scm_set_smob_free (scm_i_tc16_array, array_free);
  scm_set_smob_print (scm_i_tc16_array, scm_i_print_array);
  scm_set_smob_equalp (scm_i_tc16_array, scm_array_equal_p);

  scm_i_tc16_enclosed_array = scm_make_smob_type ("enclosed-array", 0);
  scm_set_smob_mark (scm_i_tc16_enclosed_array, array_mark);
  scm_set_smob_free (scm_i_tc16_enclosed_array, array_free);
  scm_set_smob_print (scm_i_tc16_enclosed_array, scm_i_print_enclosed_array);
  scm_set_smob_equalp (scm_i_tc16_enclosed_array, scm_array_equal_p);

  scm_add_feature ("array");

  init_type_creator_table ();

#include "libguile/arrays.x"

}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

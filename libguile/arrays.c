/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003,2004, 2005, 2006, 2009, 2010, 2011 Free Software Foundation, Inc.
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
#include "libguile/read.h"

#include "libguile/validate.h"
#include "libguile/arrays.h"
#include "libguile/array-map.h"
#include "libguile/generalized-vectors.h"
#include "libguile/generalized-arrays.h"
#include "libguile/uniform.h"


scm_t_bits scm_i_tc16_array;
#define SCM_SET_ARRAY_CONTIGUOUS_FLAG(x) \
  (SCM_SET_SMOB_FLAGS ((x), SCM_SMOB_FLAGS (x) | SCM_I_ARRAY_FLAG_CONTIGUOUS))
#define SCM_CLR_ARRAY_CONTIGUOUS_FLAG(x) \
  (SCM_SET_SMOB_FLAGS ((x), SCM_SMOB_FLAGS (x) & ~SCM_I_ARRAY_FLAG_CONTIGUOUS))


SCM_DEFINE (scm_shared_array_root, "shared-array-root", 1, 0, 0, 
           (SCM ra),
	    "Return the root vector of a shared array.")
#define FUNC_NAME s_scm_shared_array_root
{
  if (SCM_I_ARRAYP (ra))
    return SCM_I_ARRAY_V (ra);
  else if (scm_is_generalized_vector (ra))
    return ra;
  scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARG1, ra, "array");
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
scm_i_make_array (int ndim)
{
  SCM ra;
  SCM_NEWSMOB(ra, ((scm_t_bits) ndim << 17) + scm_i_tc16_array,
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

  ra = scm_i_make_array (ndim);
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
  SCM ra;
  
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

  SCM_I_ARRAY_V (ra) =
    scm_make_generalized_vector (type, scm_from_size_t (rlen), fill);

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
  SCM ra;
  scm_t_array_handle h;
  void *elts;
  size_t sz;
  
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
  SCM_I_ARRAY_V (ra) =
    scm_make_generalized_vector (type, scm_from_size_t (rlen), SCM_UNDEFINED);


  scm_array_get_handle (ra, &h);
  elts = h.writable_elements;
  sz = scm_array_handle_uniform_element_bit_size (&h);
  scm_array_handle_release (&h);

  if (sz >= 8 && ((sz % 8) == 0))
    {
      if (byte_len % (sz / 8))
        SCM_MISC_ERROR ("byte length not a multiple of the unit size", SCM_EOL);
      if (byte_len / (sz / 8) != rlen)
        SCM_MISC_ERROR ("byte length and dimensions do not match", SCM_EOL);
    }
  else if (sz < 8)
    {
      /* byte_len ?= ceil (rlen * sz / 8) */
      if (byte_len != (rlen * sz + 7) / 8)
        SCM_MISC_ERROR ("byte length and dimensions do not match", SCM_EOL);
    }
  else
    /* an internal guile error, really */
    SCM_MISC_ERROR ("uniform elements larger than 8 bits must fill whole bytes", SCM_EOL);

  memcpy (elts, bytes, byte_len);

  if (1 == SCM_I_ARRAY_NDIM (ra) && 0 == SCM_I_ARRAY_BASE (ra))
    if (s->ubnd < s->lbnd || (0 == s->lbnd && 1 == s->inc))
      return SCM_I_ARRAY_V (ra);
  return ra;
}
#undef FUNC_NAME

SCM
scm_from_contiguous_array (SCM bounds, const SCM *elts, size_t len)
#define FUNC_NAME "scm_from_contiguous_array"
{
  size_t k, rlen = 1;
  scm_t_array_dim *s;
  SCM ra;
  scm_t_array_handle h;
  
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
  if (rlen != len)
    SCM_MISC_ERROR ("element length and dimensions do not match", SCM_EOL);

  SCM_I_ARRAY_V (ra) = scm_c_make_vector (rlen, SCM_UNDEFINED);
  scm_array_get_handle (ra, &h);
  memcpy (h.writable_elements, elts, rlen * sizeof(SCM));
  scm_array_handle_release (&h);

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
	    ra = scm_make_generalized_vector (scm_array_type (ra),
                                              SCM_INUM0, SCM_UNDEFINED);
	  else
	    SCM_I_ARRAY_V (ra) =
              scm_make_generalized_vector (scm_array_type (ra),
                                           SCM_INUM0, SCM_UNDEFINED);
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
	return scm_make_generalized_vector (scm_array_type (ra), SCM_INUM0,
                                            SCM_UNDEFINED);
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

  if (SCM_I_ARRAYP (ra))
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
      res = scm_i_make_array (ndim);
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
      if (!SCM_UNBNDP (strict) && scm_is_true (strict))
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
      
      sra = scm_i_make_array (1);
      SCM_I_ARRAY_DIMS (sra)->lbnd = 0;
      SCM_I_ARRAY_DIMS (sra)->ubnd = len - 1;
      SCM_I_ARRAY_V (sra) = SCM_I_ARRAY_V (ra);
      SCM_I_ARRAY_BASE (sra) = SCM_I_ARRAY_BASE (ra);
      SCM_I_ARRAY_DIMS (sra)->inc = (ndim ? SCM_I_ARRAY_DIMS (ra)[ndim - 1].inc : 1);
      return sra;
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME


static void
list_to_array (SCM lst, scm_t_array_handle *handle, ssize_t pos, size_t k)
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
	  list_to_array (SCM_CAR (lst), handle, pos, k + 1);
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
  list_to_array (lst, &handle, 0, 0);
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

/* Print dimension DIM of ARRAY.
 */

static int
scm_i_print_array_dimension (scm_t_array_handle *h, int dim, int pos,
			     SCM port, scm_print_state *pstate)
{
  if (dim == h->ndims)
    scm_iprin1 (scm_array_handle_ref (h, pos), port, pstate);
  else
    {
      ssize_t i;
      scm_putc ('(', port);
      for (i = h->dims[dim].lbnd; i <= h->dims[dim].ubnd;
           i++, pos += h->dims[dim].inc)
        {
          scm_i_print_array_dimension (h, dim+1, pos, port, pstate);
          if (i < h->dims[dim].ubnd)
            scm_putc (' ', port);
        }
      scm_putc (')', port);
    }
  return 1;
}

/* Print an array.
*/

static int
scm_i_print_array (SCM array, SCM port, scm_print_state *pstate)
{
  scm_t_array_handle h;
  long i;
  int print_lbnds = 0, zero_size = 0, print_lens = 0;

  scm_array_get_handle (array, &h);

  scm_putc ('#', port);
  if (h.ndims != 1 || h.dims[0].lbnd != 0)
    scm_intprint (h.ndims, 10, port);
  if (h.element_type != SCM_ARRAY_ELEMENT_TYPE_SCM)
    scm_write (scm_array_handle_element_type (&h), port);
  
  for (i = 0; i < h.ndims; i++)
    {
      if (h.dims[i].lbnd != 0)
	print_lbnds = 1;
      if (h.dims[i].ubnd - h.dims[i].lbnd + 1 == 0)
	zero_size = 1;
      else if (zero_size)
	print_lens = 1;
    }

  if (print_lbnds || print_lens)
    for (i = 0; i < h.ndims; i++)
      {
	if (print_lbnds)
	  {
	    scm_putc ('@', port);
	    scm_intprint (h.dims[i].lbnd, 10, port);
	  }
	if (print_lens)
	  {
	    scm_putc (':', port);
	    scm_intprint (h.dims[i].ubnd - h.dims[i].lbnd + 1,
			  10, port);
	  }
      }

  if (h.ndims == 0)
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
      scm_i_print_array_dimension (&h, 0, 0, port, pstate);
      scm_putc (')', port);
      return 1;
    }
  else
    return scm_i_print_array_dimension (&h, 0, 0, port, pstate);
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

SCM_ARRAY_IMPLEMENTATION (SCM_SMOB_TYPE_BITS (scm_i_tc16_array),
                          SCM_SMOB_TYPE_MASK,
                          array_handle_ref, array_handle_set,
                          array_get_handle)

void
scm_init_arrays ()
{
  scm_i_tc16_array = scm_make_smob_type ("array", 0);
  scm_set_smob_print (scm_i_tc16_array, scm_i_print_array);
  scm_set_smob_equalp (scm_i_tc16_array, scm_array_equal_p);

  scm_add_feature ("array");

#include "libguile/arrays.x"

}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

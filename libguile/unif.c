/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003,2004 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */


/*
  This file has code for arrays in lots of variants (double, integer,
  unsigned etc. ). It suffers from hugely repetitive code because
  there is similar (but different) code for every variant included. (urg.)

  --hwn
*/


#if HAVE_CONFIG_H
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
#include "libguile/list.h"
#include "libguile/deprecation.h"
#include "libguile/dynwind.h"

#include "libguile/validate.h"
#include "libguile/unif.h"
#include "libguile/ramap.h"
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

scm_t_bits scm_tc16_array;
scm_t_bits scm_tc16_enclosed_array;

SCM scm_i_proc_make_vector;
SCM scm_i_proc_make_string;
SCM scm_i_proc_make_bitvector;

#if SCM_ENABLE_DEPRECATED

SCM_SYMBOL (scm_sym_s, "s");
SCM_SYMBOL (scm_sym_l, "l");

static SCM
scm_i_convert_old_prototype (SCM proto)
{
  SCM new_proto;

  /* All new 'prototypes' are creator procedures. 
   */
  if (scm_is_true (scm_procedure_p (proto)))
    return proto;

  if (scm_is_eq (proto, SCM_BOOL_T))
    new_proto = scm_i_proc_make_bitvector;
  else if (scm_is_eq (proto, SCM_MAKE_CHAR ('a')))
    new_proto = scm_i_proc_make_string;
  else if (scm_is_eq (proto, SCM_MAKE_CHAR (0)))
    new_proto = scm_i_proc_make_s8vector;
  else if (scm_is_eq (proto, scm_sym_s))
    new_proto = scm_i_proc_make_s16vector;
  else if (scm_is_true (scm_eqv_p (proto, scm_from_int (1))))
    new_proto = scm_i_proc_make_u32vector;
  else if (scm_is_true (scm_eqv_p (proto, scm_from_int (-1))))
    new_proto = scm_i_proc_make_s32vector;
  else if (scm_is_eq (proto, scm_sym_l))
    new_proto = scm_i_proc_make_s64vector;
  else if (scm_is_true (scm_eqv_p (proto, scm_from_double (1.0))))
    new_proto = scm_i_proc_make_f32vector;
  else if (scm_is_true (scm_eqv_p (proto, scm_divide (scm_from_int (1),
							 scm_from_int (3)))))
    new_proto = scm_i_proc_make_f64vector;
  else if (scm_is_true (scm_eqv_p (proto, scm_c_make_rectangular (0, 1))))
    new_proto = scm_i_proc_make_c64vector;
  else if (scm_is_null (proto))
    new_proto = scm_i_proc_make_vector;
  else
    new_proto = proto;

  scm_c_issue_deprecation_warning
    ("Using prototypes with arrays is deprecated.  "
     "Use creator functions instead.");

  return new_proto;
}

static SCM
scm_i_get_old_prototype (SCM uvec)
{
  if (scm_is_bitvector (uvec))
    return SCM_BOOL_T;
  else if (scm_is_string (uvec))
    return SCM_MAKE_CHAR ('a');
  else if (scm_is_true (scm_s8vector_p (uvec)))
    return SCM_MAKE_CHAR ('\0');
  else if (scm_is_true (scm_s16vector_p (uvec)))
    return scm_sym_s;
  else if (scm_is_true (scm_u32vector_p (uvec)))
    return scm_from_int (1);
  else if (scm_is_true (scm_s32vector_p (uvec)))
    return scm_from_int (-1);
  else if (scm_is_true (scm_s64vector_p (uvec)))
    return scm_sym_l;
  else if (scm_is_true (scm_f32vector_p (uvec)))
    return scm_from_double (1.0);
  else if (scm_is_true (scm_f64vector_p (uvec)))
    return scm_divide (scm_from_int (1), scm_from_int (3));
  else if (scm_is_true (scm_c64vector_p (uvec)))
    return scm_c_make_rectangular (0, 1);
  else if (scm_is_vector (uvec))
    return SCM_EOL;
  else
    scm_misc_error (NULL, "~a has no prototype", scm_list_1 (uvec));
}

#endif

SCM 
scm_make_uve (long k, SCM prot)
#define FUNC_NAME "scm_make_uve"
{
  SCM res;
#if SCM_ENABLE_DEPRECATED
  prot = scm_i_convert_old_prototype (prot);
#endif
  res = scm_call_1 (prot, scm_from_long (k));
  if (!scm_is_generalized_vector (res))
    scm_wrong_type_arg_msg (NULL, 0, res, "generalized vector");
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_array_p, "array?", 1, 1, 0,
           (SCM v, SCM prot),
	    "Return @code{#t} if the @var{obj} is an array, and @code{#f} if\n"
	    "not.  The @var{prototype} argument is used with uniform arrays\n"
	    "and is described elsewhere.")
#define FUNC_NAME s_scm_array_p
{
  if (SCM_ENCLOSED_ARRAYP (v))
    {
      /* Enclosed arrays are arrays but are not created by any known
	 creator procedure.
      */
      if (SCM_UNBNDP (prot))
	return SCM_BOOL_T;
      else
	return SCM_BOOL_F;
    }

  /* Get storage vector. 
   */
  if (SCM_ARRAYP (v))
    v = SCM_ARRAY_V (v);

  /* It must be a generalized vector (which includes vectors, strings, etc).
   */
  if (!scm_is_generalized_vector (v))
    return SCM_BOOL_F;

  if (SCM_UNBNDP (prot))
    return SCM_BOOL_T;

#if SCM_ENABLE_DEPRECATED
  prot = scm_i_convert_old_prototype (prot);
#endif
  return scm_eq_p (prot, scm_i_generalized_vector_creator (v));
}
#undef FUNC_NAME


SCM_DEFINE (scm_array_rank, "array-rank", 1, 0, 0, 
           (SCM array),
	    "Return the number of dimensions of the array @var{array.}\n")
#define FUNC_NAME s_scm_array_rank
{
  if (scm_is_generalized_vector (array))
    return scm_from_int (1);

  if (SCM_ARRAYP (array) || SCM_ENCLOSED_ARRAYP (array))
    return scm_from_size_t (SCM_ARRAY_NDIM (array));
    
  scm_wrong_type_arg_msg (NULL, 0, array, "array");
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
  if (scm_is_generalized_vector (ra))
    return scm_cons (scm_generalized_vector_length (ra), SCM_EOL);

  if (SCM_ARRAYP (ra) || SCM_ENCLOSED_ARRAYP (ra))
    {
      SCM res = SCM_EOL;
      size_t k;
      scm_t_array_dim *s;
      
      k = SCM_ARRAY_NDIM (ra);
      s = SCM_ARRAY_DIMS (ra);
      while (k--)
	res = scm_cons (s[k].lbnd
			? scm_cons2 (scm_from_long (s[k].lbnd),
				     scm_from_long (s[k].ubnd),
				     SCM_EOL)
			: scm_from_long (1 + s[k].ubnd),
			res);
      return res;
    }

  scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME


SCM_DEFINE (scm_shared_array_root, "shared-array-root", 1, 0, 0, 
           (SCM ra),
	    "Return the root vector of a shared array.")
#define FUNC_NAME s_scm_shared_array_root
{
  SCM_ASSERT (SCM_ARRAYP (ra) || SCM_ENCLOSED_ARRAYP (ra), ra,
	      SCM_ARG1, FUNC_NAME);
  return SCM_ARRAY_V (ra);
}
#undef FUNC_NAME


SCM_DEFINE (scm_shared_array_offset, "shared-array-offset", 1, 0, 0, 
           (SCM ra),
	    "Return the root vector index of the first element in the array.")
#define FUNC_NAME s_scm_shared_array_offset
{
  SCM_ASSERT (SCM_ARRAYP (ra) || SCM_ENCLOSED_ARRAYP (ra), ra,
	      SCM_ARG1, FUNC_NAME);
  return scm_from_int (SCM_ARRAY_BASE (ra));
}
#undef FUNC_NAME


SCM_DEFINE (scm_shared_array_increments, "shared-array-increments", 1, 0, 0, 
           (SCM ra),
	    "For each dimension, return the distance between elements in the root vector.")
#define FUNC_NAME s_scm_shared_array_increments
{
  SCM res = SCM_EOL;
  size_t k;
  scm_t_array_dim *s;

  SCM_ASSERT (SCM_ARRAYP (ra) || SCM_ENCLOSED_ARRAYP (ra), ra,
	      SCM_ARG1, FUNC_NAME);
  k = SCM_ARRAY_NDIM (ra);
  s = SCM_ARRAY_DIMS (ra);
  while (k--)
    res = scm_cons (scm_from_long (s[k].inc), res);
  return res;
}
#undef FUNC_NAME


static char s_bad_ind[] = "Bad scm_array index";


long 
scm_aind (SCM ra, SCM args, const char *what)
{
  SCM ind;
  register long j;
  register unsigned long pos = SCM_ARRAY_BASE (ra);
  register unsigned long k = SCM_ARRAY_NDIM (ra);
  scm_t_array_dim *s = SCM_ARRAY_DIMS (ra);

  if (scm_is_integer (args))
    {
      if (k != 1)
	scm_error_num_args_subr (what);
      return pos + (scm_to_long (args) - s->lbnd) * (s->inc);
    }
  while (k && scm_is_pair (args))
    {
      ind = SCM_CAR (args);
      args = SCM_CDR (args);
      if (!scm_is_integer (ind))
	scm_misc_error (what, s_bad_ind, SCM_EOL);
      j = scm_to_long (ind);
      if (j < s->lbnd || j > s->ubnd)
	scm_out_of_range (what, ind);
      pos += (j - s->lbnd) * (s->inc);
      k--;
      s++;
    }
  if (k != 0 || !scm_is_null (args))
    scm_error_num_args_subr (what);

  return pos;
}


static SCM 
scm_i_make_ra (int ndim, scm_t_bits tag)
{
  SCM ra;
  SCM_NEWSMOB(ra, ((scm_t_bits) ndim << 17) + tag,
              scm_gc_malloc ((sizeof (scm_t_array) +
			      ndim * sizeof (scm_t_array_dim)),
			     "array"));
  SCM_ARRAY_V (ra) = SCM_BOOL_F;
  return ra;
}

SCM 
scm_make_ra (int ndim)
{
  return scm_i_make_ra (ndim, scm_tc16_array);
}


static char s_bad_spec[] = "Bad scm_array dimension";


/* Increments will still need to be set. */

SCM 
scm_shap2ra (SCM args, const char *what)
{
  scm_t_array_dim *s;
  SCM ra, spec, sp;
  int ndim = scm_ilength (args);
  if (ndim < 0)
    scm_misc_error (what, s_bad_spec, SCM_EOL);

  ra = scm_make_ra (ndim);
  SCM_ARRAY_BASE (ra) = 0;
  s = SCM_ARRAY_DIMS (ra);
  for (; !scm_is_null (args); s++, args = SCM_CDR (args))
    {
      spec = SCM_CAR (args);
      if (scm_is_integer (spec))
	{
	  if (scm_to_long (spec) < 0)
	    scm_misc_error (what, s_bad_spec, SCM_EOL);
	  s->lbnd = 0;
	  s->ubnd = scm_to_long (spec) - 1;
	  s->inc = 1;
	}
      else
	{
	  if (!scm_is_pair (spec) || !scm_is_integer (SCM_CAR (spec)))
	    scm_misc_error (what, s_bad_spec, SCM_EOL);
	  s->lbnd = scm_to_long (SCM_CAR (spec));
	  sp = SCM_CDR (spec);
	  if (!scm_is_pair (sp) 
	      || !scm_is_integer (SCM_CAR (sp))
	      || !scm_is_null (SCM_CDR (sp)))
	    scm_misc_error (what, s_bad_spec, SCM_EOL);
	  s->ubnd = scm_to_long (SCM_CAR (sp));
	  s->inc = 1;
	}
    }
  return ra;
}

SCM_DEFINE (scm_dimensions_to_uniform_array, "dimensions->uniform-array", 2, 1, 0,
	    (SCM dims, SCM prot, SCM fill),
	    "@deffnx {Scheme Procedure} make-uniform-vector length prototype [fill]\n"
	    "Create and return a uniform array or vector of type\n"
	    "corresponding to @var{prototype} with dimensions @var{dims} or\n"
	    "length @var{length}.  If @var{fill} is supplied, it's used to\n"
	    "fill the array, otherwise @var{prototype} is used.")
#define FUNC_NAME s_scm_dimensions_to_uniform_array
{
  size_t k;
  unsigned long rlen = 1;
  scm_t_array_dim *s;
  SCM ra;
  
  if (scm_is_integer (dims))
    {
      SCM answer = scm_make_uve (scm_to_long (dims), prot);
      if (!SCM_UNBNDP (fill))
	scm_array_fill_x (answer, fill);
      else if (scm_is_symbol (prot) || scm_is_eq (prot, SCM_MAKE_CHAR (0)))
	scm_array_fill_x (answer, scm_from_int (0));
      else if (scm_is_false (scm_procedure_p (prot)))
	scm_array_fill_x (answer, prot);
      return answer;
    }
  
  SCM_ASSERT (scm_is_null (dims) || scm_is_pair (dims),
              dims, SCM_ARG1, FUNC_NAME);
  ra = scm_shap2ra (dims, FUNC_NAME);
  SCM_SET_ARRAY_CONTIGUOUS_FLAG (ra);
  s = SCM_ARRAY_DIMS (ra);
  k = SCM_ARRAY_NDIM (ra);

  while (k--)
    {
      s[k].inc = rlen;
      SCM_ASSERT_RANGE (1, dims, s[k].lbnd <= s[k].ubnd);
      rlen = (s[k].ubnd - s[k].lbnd + 1) * s[k].inc;
    }

  SCM_ARRAY_V (ra) = scm_make_uve (rlen, prot);

  if (!SCM_UNBNDP (fill))
    scm_array_fill_x (ra, fill);
  else if (scm_is_symbol (prot) || scm_is_eq (prot, SCM_MAKE_CHAR (0)))
    scm_array_fill_x (ra, scm_from_int (0));
  else if (scm_is_false (scm_procedure_p (prot)))
    scm_array_fill_x (ra, prot);

  if (1 == SCM_ARRAY_NDIM (ra) && 0 == SCM_ARRAY_BASE (ra))
    if (s->ubnd < s->lbnd || (0 == s->lbnd && 1 == s->inc))
      return SCM_ARRAY_V (ra);
  return ra;
}
#undef FUNC_NAME


void 
scm_ra_set_contp (SCM ra)
{
  /* XXX - correct?  one-dimensional arrays are always 'contiguous',
     is that right?
   */
  size_t k = SCM_ARRAY_NDIM (ra);
  if (k)
    {
      long inc = SCM_ARRAY_DIMS (ra)[k - 1].inc;
      while (k--)
	{
	  if (inc != SCM_ARRAY_DIMS (ra)[k].inc)
	    {
	      SCM_CLR_ARRAY_CONTIGUOUS_FLAG (ra);
	      return;
	    }
	  inc *= (SCM_ARRAY_DIMS (ra)[k].ubnd 
		  - SCM_ARRAY_DIMS (ra)[k].lbnd + 1);
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
  SCM ra;
  SCM inds, indptr;
  SCM imap;
  size_t k, i;
  long old_min, new_min, old_max, new_max;
  scm_t_array_dim *s;

  SCM_VALIDATE_REST_ARGUMENT (dims);
  SCM_VALIDATE_ARRAY (1, oldra);
  SCM_VALIDATE_PROC (2, mapfunc);
  ra = scm_shap2ra (dims, FUNC_NAME);
  if (SCM_ARRAYP (oldra))
    {
      SCM_ARRAY_V (ra) = SCM_ARRAY_V (oldra);
      old_min = old_max = SCM_ARRAY_BASE (oldra);
      s = SCM_ARRAY_DIMS (oldra);
      k = SCM_ARRAY_NDIM (oldra);
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
      SCM_ARRAY_V (ra) = oldra;
      old_min = 0;
      old_max = scm_c_generalized_vector_length (oldra) - 1;
    }
  inds = SCM_EOL;
  s = SCM_ARRAY_DIMS (ra);
  for (k = 0; k < SCM_ARRAY_NDIM (ra); k++)
    {
      inds = scm_cons (scm_from_long (s[k].lbnd), inds);
      if (s[k].ubnd < s[k].lbnd)
	{
	  if (1 == SCM_ARRAY_NDIM (ra))
	    ra = scm_make_uve (0L, scm_array_creator (ra));
	  else
	    SCM_ARRAY_V (ra) = scm_make_uve (0L, scm_array_creator (ra));
	  return ra;
	}
    }
  imap = scm_apply_0 (mapfunc, scm_reverse (inds));
  if (SCM_ARRAYP (oldra))
      i = (size_t) scm_aind (oldra, imap, FUNC_NAME);
  else
    {
      if (!scm_is_integer (imap))
	{
	  if (scm_ilength (imap) != 1 || !scm_is_integer (SCM_CAR (imap)))
	    SCM_MISC_ERROR (s_bad_ind, SCM_EOL);
	  imap = SCM_CAR (imap);
	}
      i = scm_to_size_t (imap);
    }
  SCM_ARRAY_BASE (ra) = new_min = new_max = i;
  indptr = inds;
  k = SCM_ARRAY_NDIM (ra);
  while (k--)
    {
      if (s[k].ubnd > s[k].lbnd)
	{
	  SCM_SETCAR (indptr, scm_sum (SCM_CAR (indptr), scm_from_int (1)));
	  imap = scm_apply_0 (mapfunc, scm_reverse (inds));
	  if (SCM_ARRAYP (oldra))

	      s[k].inc = scm_aind (oldra, imap, FUNC_NAME) - i;
	  else
	    {
	      if (!scm_is_integer (imap))
		{
		  if (scm_ilength (imap) != 1 || !scm_is_integer (SCM_CAR (imap)))
		    SCM_MISC_ERROR (s_bad_ind, SCM_EOL);
		  imap = SCM_CAR (imap);
		}
	      s[k].inc = scm_to_long (imap) - i;
	    }
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
  if (old_min > new_min || old_max < new_max)
    SCM_MISC_ERROR ("mapping out of range", SCM_EOL);
  if (1 == SCM_ARRAY_NDIM (ra) && 0 == SCM_ARRAY_BASE (ra))
    {
      SCM v = SCM_ARRAY_V (ra);
      unsigned long int length = scm_to_ulong (scm_uniform_vector_length (v));
      if (1 == s->inc && 0 == s->lbnd && length == 1 + s->ubnd)
	return v;
      if (s->ubnd < s->lbnd)
	return scm_make_uve (0L, scm_array_creator (ra));
    }
  scm_ra_set_contp (ra);
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
  SCM const *ve = &vargs;
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

  if (SCM_ARRAYP (ra) || SCM_ENCLOSED_ARRAYP (ra))
    {
      vargs = scm_vector (args);
      if (SCM_VECTOR_LENGTH (vargs) != SCM_ARRAY_NDIM (ra))
	SCM_WRONG_NUM_ARGS ();
      ve = SCM_VELTS (vargs);
      ndim = 0;
      for (k = 0; k < SCM_ARRAY_NDIM (ra); k++)
	{
	  i = scm_to_signed_integer (ve[k], 0, SCM_ARRAY_NDIM(ra));
	  if (ndim < i)
	    ndim = i;
	}
      ndim++;
      res = scm_make_ra (ndim);
      SCM_ARRAY_V (res) = SCM_ARRAY_V (ra);
      SCM_ARRAY_BASE (res) = SCM_ARRAY_BASE (ra);
      for (k = ndim; k--;)
	{
	  SCM_ARRAY_DIMS (res)[k].lbnd = 0;
	  SCM_ARRAY_DIMS (res)[k].ubnd = -1;
	}
      for (k = SCM_ARRAY_NDIM (ra); k--;)
	{
	  i = scm_to_int (ve[k]);
	  s = &(SCM_ARRAY_DIMS (ra)[k]);
	  r = &(SCM_ARRAY_DIMS (res)[i]);
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
		  SCM_ARRAY_BASE (res) += (s->lbnd - r->lbnd) * r->inc;
		  r->lbnd = s->lbnd;
		}
	      r->inc += s->inc;
	    }
	}
      if (ndim > 0)
	SCM_MISC_ERROR ("bad argument list", SCM_EOL);
      scm_ra_set_contp (res);
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
    axes = scm_cons ((SCM_ARRAYP (ra) ? scm_from_size_t (SCM_ARRAY_NDIM (ra) - 1) : SCM_INUM0), SCM_EOL);
  ninr = scm_ilength (axes);
  if (ninr < 0)
    SCM_WRONG_NUM_ARGS ();
  ra_inr = scm_make_ra (ninr);

  if (scm_is_generalized_vector (ra))
    {
      s->lbnd = 0;
      s->ubnd = scm_to_long (scm_uniform_vector_length (ra)) - 1;
      s->inc = 1;
      SCM_ARRAY_V (ra_inr) = ra;
      SCM_ARRAY_BASE (ra_inr) = 0;
      ndim = 1;
    }
  else if (SCM_ARRAYP (ra))
    {
      s = SCM_ARRAY_DIMS (ra);
      SCM_ARRAY_V (ra_inr) = SCM_ARRAY_V (ra);
      SCM_ARRAY_BASE (ra_inr) = SCM_ARRAY_BASE (ra);
      ndim = SCM_ARRAY_NDIM (ra);
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, ra, "array");

  noutr = ndim - ninr;
  if (noutr < 0)
    SCM_WRONG_NUM_ARGS ();
  axv = scm_make_string (scm_from_int (ndim), SCM_MAKE_CHAR (0));
  res = scm_i_make_ra (noutr, scm_tc16_enclosed_array);
  SCM_ARRAY_BASE (res) = SCM_ARRAY_BASE (ra_inr);
  SCM_ARRAY_V (res) = ra_inr;
  for (k = 0; k < ninr; k++, axes = SCM_CDR (axes))
    {
      if (!scm_is_integer (SCM_CAR (axes)))
	SCM_MISC_ERROR ("bad axis", SCM_EOL);
      j = scm_to_int (SCM_CAR (axes));
      SCM_ARRAY_DIMS (ra_inr)[k].lbnd = s[j].lbnd;
      SCM_ARRAY_DIMS (ra_inr)[k].ubnd = s[j].ubnd;
      SCM_ARRAY_DIMS (ra_inr)[k].inc = s[j].inc;
      scm_c_string_set_x (axv, j, SCM_MAKE_CHAR (1));
    }
  c_axv = scm_i_string_chars (axv);
  for (j = 0, k = 0; k < noutr; k++, j++)
    {
      while (c_axv[j])
	j++;
      SCM_ARRAY_DIMS (res)[k].lbnd = s[j].lbnd;
      SCM_ARRAY_DIMS (res)[k].ubnd = s[j].ubnd;
      SCM_ARRAY_DIMS (res)[k].inc = s[j].inc;
    }
  scm_remember_upto_here_1 (axv);
  scm_ra_set_contp (ra_inr);
  scm_ra_set_contp (res);
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

  if (scm_is_generalized_vector (v))
    {
      long ind;

      if (!scm_is_pair (args))
	SCM_WRONG_NUM_ARGS ();
      ind = scm_to_long (SCM_CAR (args));
      args = SCM_CDR (args);
      res = scm_from_bool (ind >= 0
			   && ind < scm_c_generalized_vector_length (v));
    }
  else if (SCM_ARRAYP (v) || SCM_ENCLOSED_ARRAYP (v))
    {
      size_t k = SCM_ARRAY_NDIM (v);
      scm_t_array_dim *s = SCM_ARRAY_DIMS (v);

      while (k > 0)
	{
	  long ind;

	  if (!scm_is_pair (args))
	    SCM_WRONG_NUM_ARGS ();
	  ind = scm_to_long (SCM_CAR (args));
	  args = SCM_CDR (args);
	  k -= 1;

	  if (ind < s->lbnd || ind > s->ubnd)
	    {
	      res = SCM_BOOL_F;
	      /* We do not stop the checking after finding a violation
		 since we want to validate the type-correctness and
		 number of arguments in any case.
	      */
	    }
	}
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
      int k = SCM_ARRAY_NDIM (v);
      SCM res = scm_make_ra (k);
      SCM_ARRAY_V (res) = SCM_ARRAY_V (v);
      SCM_ARRAY_BASE (res) = pos;
      while (k--)
	{
	  SCM_ARRAY_DIMS (res)[k].ubnd = SCM_ARRAY_DIMS (v)[k].ubnd;
	  SCM_ARRAY_DIMS (res)[k].lbnd = SCM_ARRAY_DIMS (v)[k].lbnd;
	  SCM_ARRAY_DIMS (res)[k].inc = SCM_ARRAY_DIMS (v)[k].inc;
	}
      return res;
    }
  else
    return scm_c_generalized_vector_ref (v, pos);
}

SCM
scm_cvref (SCM v, unsigned long pos, SCM last)
{
  return scm_i_cvref (v, pos, 0);
}

SCM_DEFINE (scm_array_ref, "array-ref", 1, 0, 1,
           (SCM v, SCM args),
	    "Return the element at the @code{(index1, index2)} element in\n"
	    "@var{array}.")
#define FUNC_NAME s_scm_array_ref
{
  long pos;
  int enclosed = 0;

  if (SCM_ARRAYP (v) || SCM_ENCLOSED_ARRAYP (v))
    {
      enclosed = SCM_ENCLOSED_ARRAYP (v);
      pos = scm_aind (v, args, FUNC_NAME);
      v = SCM_ARRAY_V (v);
    }
  else
    {
      size_t length;
      if (SCM_NIMP (args))
	{
	  SCM_ASSERT (scm_is_pair (args), args, SCM_ARG2, FUNC_NAME);
	  pos = scm_to_long (SCM_CAR (args));
	  SCM_ASRTGO (scm_is_null (SCM_CDR (args)), wna);
	}
      else
	pos = scm_to_long (args);
      length = scm_c_generalized_vector_length (v);
      SCM_ASRTGO (pos >= 0 && pos < length, outrng);
    }

  return scm_i_cvref (v, pos, enclosed);

 wna:
  scm_wrong_num_args (NULL);
 outrng:
  scm_out_of_range (NULL, scm_from_long (pos));
}
#undef FUNC_NAME


SCM_DEFINE (scm_array_set_x, "array-set!", 2, 0, 1, 
           (SCM v, SCM obj, SCM args),
	    "Set the element at the @code{(index1, index2)} element in @var{array} to\n"
	    "@var{new-value}.  The value returned by array-set! is unspecified.")
#define FUNC_NAME s_scm_array_set_x           
{
  long pos = 0;

  if (SCM_ARRAYP (v))
    {
      pos = scm_aind (v, args, FUNC_NAME);
      v = SCM_ARRAY_V (v);
    }
  else if (SCM_ENCLOSED_ARRAYP (v))
    scm_wrong_type_arg_msg (NULL, 0, v, "non-enclosed array");
  else if (scm_is_generalized_vector (v))
    {
      size_t length;
      if (scm_is_pair (args))
	{
	  SCM_ASRTGO (scm_is_null (SCM_CDR (args)), wna);
	  pos = scm_to_long (SCM_CAR (args));
	}
      else
	pos = scm_to_long (args);
      length = scm_c_generalized_vector_length (v);
      SCM_ASRTGO (pos >= 0 && pos < length, outrng);
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "array");
    
  scm_c_generalized_vector_set_x (v, pos, obj);
  return SCM_UNSPECIFIED;

 outrng:
  scm_out_of_range (NULL, scm_from_long (pos));
 wna:
  scm_wrong_num_args (NULL);
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

  if (SCM_ARRAYP (ra))
    {
      size_t k, ndim = SCM_ARRAY_NDIM (ra), len = 1;
      if (!SCM_ARRAYP (ra) || !SCM_ARRAY_CONTP (ra))
	return SCM_BOOL_F;
      for (k = 0; k < ndim; k++)
	len *= SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd + 1;
      if (!SCM_UNBNDP (strict))
	{
	  if (ndim && (1 != SCM_ARRAY_DIMS (ra)[ndim - 1].inc))
	    return SCM_BOOL_F;
	  if (scm_is_bitvector (SCM_ARRAY_V (ra)))
	    {
	      if (len != scm_c_bitvector_length (SCM_ARRAY_V (ra)) ||
		  SCM_ARRAY_BASE (ra) % SCM_LONG_BIT ||
		  len % SCM_LONG_BIT)
		return SCM_BOOL_F;
	    }
	}
      
      {
	SCM v = SCM_ARRAY_V (ra);
	size_t length = scm_c_generalized_vector_length (v);
	if ((len == length) && 0 == SCM_ARRAY_BASE (ra) && SCM_ARRAY_DIMS (ra)->inc)
	  return v;
      }
      
      sra = scm_make_ra (1);
      SCM_ARRAY_DIMS (sra)->lbnd = 0;
      SCM_ARRAY_DIMS (sra)->ubnd = len - 1;
      SCM_ARRAY_V (sra) = SCM_ARRAY_V (ra);
      SCM_ARRAY_BASE (sra) = SCM_ARRAY_BASE (ra);
      SCM_ARRAY_DIMS (sra)->inc = (ndim ? SCM_ARRAY_DIMS (ra)[ndim - 1].inc : 1);
      return sra;
    }
  else if (SCM_ENCLOSED_ARRAYP (ra))
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
  for (k = SCM_ARRAY_NDIM (ra); k--;)
    len *= SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd + 1;
  k = SCM_ARRAY_NDIM (ra);
  if (SCM_ARRAY_CONTP (ra) && ((0 == k) || (1 == SCM_ARRAY_DIMS (ra)[k - 1].inc)))
    {
      if (!scm_is_bitvector (SCM_ARRAY_V (ra)))
	return ra;
      if ((len == scm_c_bitvector_length (SCM_ARRAY_V (ra)) &&
	   0 == SCM_ARRAY_BASE (ra) % SCM_LONG_BIT &&
	   0 == len % SCM_LONG_BIT))
	return ra;
    }
  ret = scm_make_ra (k);
  SCM_ARRAY_BASE (ret) = 0;
  while (k--)
    {
      SCM_ARRAY_DIMS (ret)[k].lbnd = SCM_ARRAY_DIMS (ra)[k].lbnd;
      SCM_ARRAY_DIMS (ret)[k].ubnd = SCM_ARRAY_DIMS (ra)[k].ubnd;
      SCM_ARRAY_DIMS (ret)[k].inc = inc;
      inc *= SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd + 1;
    }
  SCM_ARRAY_V (ret) = scm_make_uve (inc, scm_array_creator (ra));
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
    port_or_fd = scm_cur_inp;

  if (scm_is_uniform_vector (ura))
    {
      return scm_uniform_vector_read_x (ura, port_or_fd, start, end);
    }
  else if (SCM_ARRAYP (ura))
    {
      size_t base, vlen, cstart, cend;
      SCM cra, ans;
      
      cra = scm_ra2contig (ura, 0);
      base = SCM_ARRAY_BASE (cra);
      vlen = SCM_ARRAY_DIMS (cra)->inc *
	(SCM_ARRAY_DIMS (cra)->ubnd - SCM_ARRAY_DIMS (cra)->lbnd + 1);

      cstart = 0;
      cend = vlen;
      if (!SCM_UNBNDP (start))
	{
	  cstart = scm_to_unsigned_integer (start, 0, vlen);
	  if (!SCM_UNBNDP (end))
	    cend = scm_to_unsigned_integer (end, cstart, vlen);
	}

      ans = scm_uniform_vector_read_x (SCM_ARRAY_V (cra), port_or_fd,
				       scm_from_size_t (base + cstart),
				       scm_from_size_t (base + cend));

      if (!scm_is_eq (cra, ura))
	scm_array_copy_x (cra, ura);
      return ans;
    }
  else if (SCM_ENCLOSED_ARRAYP (ura))
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
    port_or_fd = scm_cur_outp;

  if (scm_is_uniform_vector (ura))
    {
      return scm_uniform_vector_write (ura, port_or_fd, start, end);
    }
  else if (SCM_ARRAYP (ura))
    {
      size_t base, vlen, cstart, cend;
      SCM cra, ans;
      
      cra = scm_ra2contig (ura, 1);
      base = SCM_ARRAY_BASE (cra);
      vlen = SCM_ARRAY_DIMS (cra)->inc *
	(SCM_ARRAY_DIMS (cra)->ubnd - SCM_ARRAY_DIMS (cra)->lbnd + 1);

      cstart = 0;
      cend = vlen;
      if (!SCM_UNBNDP (start))
	{
	  cstart = scm_to_unsigned_integer (start, 0, vlen);
	  if (!SCM_UNBNDP (end))
	    cend = scm_to_unsigned_integer (end, cstart, vlen);
	}

      ans = scm_uniform_vector_write (SCM_ARRAY_V (cra), port_or_fd,
				      scm_from_size_t (base + cstart),
				      scm_from_size_t (base + cend));

      return ans;
    }
  else if (SCM_ENCLOSED_ARRAYP (ura))
    scm_wrong_type_arg_msg (NULL, 0, ura, "non-enclosed array");    
  else
    scm_wrong_type_arg_msg (NULL, 0, ura, "array");
}
#undef FUNC_NAME


/** Bit vectors */

static scm_t_bits scm_tc16_bitvector;

#define IS_BITVECTOR(obj)       SCM_SMOB_PREDICATE(scm_tc16_bitvector,(obj))
#define BITVECTOR_BITS(obj)     ((scm_t_uint32 *)SCM_SMOB_DATA(obj))
#define BITVECTOR_LENGTH(obj)   ((size_t)SCM_SMOB_DATA_2(obj))

static size_t
bitvector_free (SCM vec)
{
  scm_gc_free (BITVECTOR_BITS (vec),
	       sizeof (scm_t_uint32) * ((BITVECTOR_LENGTH (vec)+31)/32),
	       "bitvector");
  return 0;
}

static int
bitvector_print (SCM vec, SCM port, scm_print_state *pstate)
{
  size_t bit_len = BITVECTOR_LENGTH (vec);
  size_t word_len = (bit_len+31)/32;
  scm_t_uint32 *bits = BITVECTOR_BITS (vec);
  size_t i, j;

  scm_puts ("#*", port);
  for (i = 0; i < word_len; i++, bit_len -= 32)
    {
      scm_t_uint32 mask = 1;
      for (j = 0; j < 32 && j < bit_len; j++, mask <<= 1)
	scm_putc ((bits[i] & mask)? '1' : '0', port);
    }
    
  return 1;
}

static SCM
bitvector_equalp (SCM vec1, SCM vec2)
{
  size_t bit_len = BITVECTOR_LENGTH (vec1);
  size_t word_len = (bit_len + 31) / 32;
  scm_t_uint32 last_mask =  ((scm_t_uint32)-1) >> (32*word_len - bit_len);
  scm_t_uint32 *bits1 = BITVECTOR_BITS (vec1);
  scm_t_uint32 *bits2 = BITVECTOR_BITS (vec2);

  /* compare lengths */
  if (BITVECTOR_LENGTH (vec2) != bit_len)
    return SCM_BOOL_F;
  /* avoid underflow in word_len-1 below. */
  if (bit_len == 0)
    return SCM_BOOL_T;
  /* compare full words */
  if (memcmp (bits1, bits2, sizeof (scm_t_uint32) * (word_len-1)))
    return SCM_BOOL_F;
  /* compare partial last words */
  if ((bits1[word_len-1] & last_mask) != (bits2[word_len-1] & last_mask))
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}

int
scm_is_bitvector (SCM vec)
{
  return IS_BITVECTOR (vec);
}

SCM_DEFINE (scm_bitvector_p, "bitvector?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} when @var{obj} is a bitvector, else\n"
	    "return @code{#f}.")
#define FUNC_NAME s_scm_bitvector_p
{
  return scm_from_bool (scm_is_bitvector (obj));
}
#undef FUNC_NAME

SCM
scm_c_make_bitvector (size_t len, SCM fill)
{
  size_t word_len = (len + 31) / 32;
  scm_t_uint32 *bits;
  SCM res;

  bits = scm_gc_malloc (sizeof (scm_t_uint32) * word_len,
			"bitvector");
  SCM_NEWSMOB2 (res, scm_tc16_bitvector, bits, len);

  if (!SCM_UNBNDP (fill))
    scm_bitvector_fill_x (res, fill);
      
  return res;
}

SCM_DEFINE (scm_make_bitvector, "make-bitvector", 1, 1, 0,
	    (SCM len, SCM fill),
	    "Create a new bitvector of length @var{len} and\n"
	    "optionally initialize all elements to @var{fill}.")
#define FUNC_NAME s_scm_make_bitvector
{
  return scm_c_make_bitvector (scm_to_size_t (len), fill);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bitvector, "bitvector", 0, 0, 1,
	    (SCM bits),
	    "Create a new bitvector with the arguments as elements.")
#define FUNC_NAME s_scm_bitvector
{
  return scm_list_to_bitvector (bits);
}
#undef FUNC_NAME

size_t
scm_c_bitvector_length (SCM vec)
{
  scm_assert_smob_type (scm_tc16_bitvector, vec);
  return BITVECTOR_LENGTH (vec);
}

SCM_DEFINE (scm_bitvector_length, "bitvector-length", 1, 0, 0,
	    (SCM vec),
	    "Return the length of the bitvector @var{vec}.")
#define FUNC_NAME s_scm_bitvector_length
{
  return scm_from_size_t (scm_c_bitvector_length (vec));
}
#undef FUNC_NAME

scm_t_uint32 *
scm_bitvector_elements (SCM vec)
{
  scm_assert_smob_type (scm_tc16_bitvector, vec);
  return BITVECTOR_BITS (vec);
}

void
scm_bitvector_release (SCM vec)
{
  /* Nothing to do right now, but this function might come in handy
     when bitvectors need to be locked when giving away a pointer
     to their elements.
     
     Also, a call to scm_bitvector_release acts like
     scm_remember_upto_here, which is needed in any case.
  */
}

void
scm_frame_bitvector_release (SCM vec)
{
  scm_frame_unwind_handler_with_scm (scm_bitvector_release, vec,
				     SCM_F_WIND_EXPLICITLY);
}

SCM
scm_c_bitvector_ref (SCM vec, size_t idx)
{
  if (idx < scm_c_bitvector_length (vec))
    {
      scm_t_uint32 *bits = scm_bitvector_elements (vec);
      SCM res = (bits[idx/32] & (1L << (idx%32)))? SCM_BOOL_T : SCM_BOOL_F;
      scm_bitvector_release (vec);
      return res;
    }
  else
    scm_out_of_range (NULL, scm_from_size_t (idx));
}

SCM_DEFINE (scm_bitvector_ref, "bitvector-ref", 2, 0, 0,
	    (SCM vec, SCM idx),
	    "Return the element at index @var{idx} of the bitvector\n"
	    "@var{vec}.")
#define FUNC_NAME s_scm_bitvector_ref
{
  return scm_c_bitvector_ref (vec, scm_to_size_t (idx));
}
#undef FUNC_NAME

void
scm_c_bitvector_set_x (SCM vec, size_t idx, SCM val)
{
  if (idx < scm_c_bitvector_length (vec))
    {
      scm_t_uint32 *bits = scm_bitvector_elements (vec);
      scm_t_uint32 mask = 1L << (idx%32);
      if (scm_is_true (val))
	bits[idx/32] |= mask;
      else
	bits[idx/32] &= ~mask;
      scm_bitvector_release (vec);
    }
  else
    scm_out_of_range (NULL, scm_from_size_t (idx));
}

SCM_DEFINE (scm_bitvector_set_x, "bitvector-set!", 3, 0, 0,
	    (SCM vec, SCM idx, SCM val),
	    "Set the element at index @var{idx} of the bitvector\n"
	    "@var{vec} when @var{val} is true, else clear it.")
#define FUNC_NAME s_scm_bitvector_set_x
{
  scm_c_bitvector_set_x (vec, scm_to_size_t (idx), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bitvector_fill_x, "bitvector-fill!", 2, 0, 0,
	    (SCM vec, SCM val),
	    "Set all elements of the bitvector\n"
	    "@var{vec} when @var{val} is true, else clear them.")
#define FUNC_NAME s_scm_bitvector_fill_x
{
  scm_t_uint32 *bits = scm_bitvector_elements (vec);
  size_t bit_len = BITVECTOR_LENGTH (vec);
  size_t word_len = (bit_len + 31) / 32;
  memset (bits, scm_is_true (val)? -1:0, sizeof (scm_t_uint32) * word_len);
  scm_bitvector_release (vec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_list_to_bitvector, "list->bitvector", 1, 0, 0,
	    (SCM list),
	    "Return a new bitvector initialized with the elements\n"
	    "of @var{list}.")
#define FUNC_NAME s_scm_list_to_bitvector
{
  size_t bit_len = scm_to_size_t (scm_length (list));
  SCM vec = scm_c_make_bitvector (bit_len, SCM_UNDEFINED);
  size_t word_len = (bit_len+31)/32;
  scm_t_uint32 *bits = scm_bitvector_elements (vec);
  size_t i, j;

  for (i = 0; i < word_len && scm_is_pair (list); i++, bit_len -= 32)
    {
      scm_t_uint32 mask = 1;
      bits[i] = 0;
      for (j = 0; j < 32 && j < bit_len;
	   j++, mask <<= 1, list = SCM_CDR (list))
	if (scm_is_true (SCM_CAR (list)))
	  bits[i] |= mask;
    }
  
  scm_bitvector_release (vec);
  return vec;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bitvector_to_list, "bitvector->list", 1, 0, 0,
	    (SCM vec),
	    "Return a new list initialized with the elements\n"
	    "of the bitvector @var{vec}.")
#define FUNC_NAME s_scm_bitvector_to_list
{
  size_t bit_len = scm_c_bitvector_length (vec);
  SCM res = SCM_EOL;
  size_t word_len = (bit_len+31)/32;
  scm_t_uint32 *bits = scm_bitvector_elements (vec);
  size_t i, j;

  for (i = 0; i < word_len; i++, bit_len -= 32)
    {
      scm_t_uint32 mask = 1;
      for (j = 0; j < 32 && j < bit_len; j++, mask <<= 1)
	res = scm_cons ((bits[i] & mask)? SCM_BOOL_T : SCM_BOOL_F, res);
    }
  
  scm_bitvector_release (vec);
  return scm_reverse_x (res, SCM_EOL);
}
#undef FUNC_NAME

/* From mmix-arith.w by Knuth.

  Here's a fun way to count the number of bits in a tetrabyte.

  [This classical trick is called the ``Gillies--Miller method for
  sideways addition'' in {\sl The Preparation of Programs for an
  Electronic Digital Computer\/} by Wilkes, Wheeler, and Gill, second
  edition (Reading, Mass.:\ Addison--Wesley, 1957), 191--193. Some of
  the tricks used here were suggested by Balbir Singh, Peter
  Rossmanith, and Stefan Schwoon.]
*/

static size_t
count_ones (scm_t_uint32 x)
{
  x=x-((x>>1)&0x55555555);
  x=(x&0x33333333)+((x>>2)&0x33333333);
  x=(x+(x>>4))&0x0f0f0f0f;
  x=x+(x>>8);
  return (x+(x>>16)) & 0xff;
}

SCM_DEFINE (scm_bit_count, "bit-count", 2, 0, 0,
	    (SCM b, SCM bitvector),
	    "Return the number of occurrences of the boolean @var{b} in\n"
	    "@var{bitvector}.")
#define FUNC_NAME s_scm_bit_count
{
  size_t bit_len = scm_c_bitvector_length (bitvector);
  size_t word_len = (bit_len + 31) / 32;
  scm_t_uint32 last_mask =  ((scm_t_uint32)-1) >> (32*word_len - bit_len);
  scm_t_uint32 *bits = scm_bitvector_elements (bitvector);

  int bit = scm_to_bool (b);
  size_t count = 0, i;

  if (bit_len == 0)
    return 0;

  for (i = 0; i < word_len-1; i++)
    count += count_ones (bits[i]);
  count += count_ones (bits[i] & last_mask);

  scm_bitvector_release (bitvector);
  return scm_from_size_t (bit? count : bit_len-count);
}
#undef FUNC_NAME

/* returns 32 for x == 0. 
*/
static size_t
find_first_one (scm_t_uint32 x)
{
  size_t pos = 0;
  /* do a binary search in x. */
  if ((x & 0xFFFF) == 0)
    x >>= 16, pos += 16;
  if ((x & 0xFF) == 0)
    x >>= 8, pos += 8;
  if ((x & 0xF) == 0)
    x >>= 4, pos += 4;
  if ((x & 0x3) == 0)
    x >>= 2, pos += 2;
  if ((x & 0x1) == 0)
    pos += 1;
  return pos;
}

SCM_DEFINE (scm_bit_position, "bit-position", 3, 0, 0,
           (SCM item, SCM v, SCM k),
	    "Return the index of the first occurrance of @var{item} in bit\n"
	    "vector @var{v}, starting from @var{k}.  If there is no\n"
	    "@var{item} entry between @var{k} and the end of\n"
	    "@var{bitvector}, then return @code{#f}.  For example,\n"
	    "\n"
	    "@example\n"
	    "(bit-position #t #*000101 0)  @result{} 3\n"
	    "(bit-position #f #*0001111 3) @result{} #f\n"
	    "@end example")
#define FUNC_NAME s_scm_bit_position
{
  size_t bit_len = scm_c_bitvector_length (v);
  size_t word_len = (bit_len + 31) / 32;
  scm_t_uint32 last_mask =  ((scm_t_uint32)-1) >> (32*word_len - bit_len);
  scm_t_uint32 *bits = scm_bitvector_elements (v);
  size_t first_bit = scm_to_unsigned_integer (k, 0, bit_len);
  size_t first_word = first_bit / 32;
  scm_t_uint32 first_mask =  ((scm_t_uint32)-1) << (first_bit - 32*first_word);
  scm_t_uint32 w;

  int bit = scm_to_bool (item);
  size_t i;
  SCM res = SCM_BOOL_F;

  if (bit_len == 0)
    return 0;

  for (i = first_word; i < word_len; i++)
    {
      w = (bit? bits[i] : ~bits[i]);
      if (i == first_word)
	w &= first_mask;
      if (i == word_len-1)
	w &= last_mask;
      if (w)
	{
	  res = scm_from_size_t (32*i + find_first_one (w));
	  break;
	}
    }

  scm_bitvector_release (v);
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bit_set_star_x, "bit-set*!", 3, 0, 0,
	    (SCM v, SCM kv, SCM obj),
	    "Set entries of bit vector @var{v} to @var{obj}, with @var{kv}\n"
	    "selecting the entries to change.  The return value is\n"
	    "unspecified.\n"
	    "\n"
	    "If @var{kv} is a bit vector, then those entries where it has\n"
	    "@code{#t} are the ones in @var{v} which are set to @var{obj}.\n"
	    "@var{kv} and @var{v} must be the same length.  When @var{obj}\n"
	    "is @code{#t} it's like @var{kv} is OR'ed into @var{v}.  Or when\n"
	    "@var{obj} is @code{#f} it can be seen as an ANDNOT.\n"
	    "\n"
	    "@example\n"
	    "(define bv #*01000010)\n"
	    "(bit-set*! bv #*10010001 #t)\n"
	    "bv\n"
	    "@result{} #*11010011\n"
	    "@end example\n"
	    "\n"
	    "If @var{kv} is a u32vector, then its elements are\n"
	    "indices into @var{v} which are set to @var{obj}.\n"
	    "\n"
	    "@example\n"
	    "(define bv #*01000010)\n"
	    "(bit-set*! bv #u32(5 2 7) #t)\n"
	    "bv\n"
	    "@result{} #*01100111\n"
	    "@end example")
#define FUNC_NAME s_scm_bit_set_star_x
{
  if (scm_is_bitvector (kv))
    {
      size_t bit_len = scm_c_bitvector_length (kv);
      size_t word_len = (bit_len + 31) / 32;
      scm_t_uint32 *bits1, *bits2;
      size_t i;
      int bit = scm_to_bool (obj);
 
      if (scm_c_bitvector_length (v) != bit_len)
	scm_misc_error (NULL,
			"bit vectors must have equal length",
			SCM_EOL);

      bits1 = scm_bitvector_elements (v);
      bits2 = scm_bitvector_elements (kv);

      if (bit  == 0)
	for (i = 0; i < word_len; i++)
	  bits1[i] &= ~bits2[i];
      else
	for (i = 0; i < word_len; i++)
	  bits1[i] |= bits2[i];

      scm_bitvector_release (kv);
      scm_bitvector_release (v);
    }
  else if (scm_is_true (scm_u32vector_p (kv)))
    {
      size_t ulen, i;
      scm_t_uint32 *indices;

      /* assert that obj is a boolean. 
       */
      scm_to_bool (obj);

      scm_frame_begin (0);

      ulen = scm_c_uniform_vector_length (kv);
      indices = scm_u32vector_elements (kv);
      scm_frame_uniform_vector_release (kv);

      for (i = 0; i < ulen; i++)
	scm_c_bitvector_set_x (v, (size_t)indices[i], obj);

      scm_frame_end ();
    }
  else 
    scm_wrong_type_arg_msg (NULL, 0, kv, "bitvector or u32vector");

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_bit_count_star, "bit-count*", 3, 0, 0,
           (SCM v, SCM kv, SCM obj),
	    "Return a count of how many entries in bit vector @var{v} are\n"
	    "equal to @var{obj}, with @var{kv} selecting the entries to\n"
	    "consider.\n"
	    "\n"
	    "If @var{kv} is a bit vector, then those entries where it has\n"
	    "@code{#t} are the ones in @var{v} which are considered.\n"
	    "@var{kv} and @var{v} must be the same length.\n"
	    "\n"
	    "If @var{kv} is a u32vector, then it contains\n"
	    "the indexes in @var{v} to consider.\n"
	    "\n"
	    "For example,\n"
	    "\n"
	    "@example\n"
	    "(bit-count* #*01110111 #*11001101 #t) @result{} 3\n"
	    "(bit-count* #*01110111 #u32(7 0 4) #f)  @result{} 2\n"
	    "@end example")
#define FUNC_NAME s_scm_bit_count_star
{
  if (scm_is_bitvector (kv))
    {
      size_t bit_len = scm_c_bitvector_length (kv);
      size_t word_len = (bit_len + 31) / 32;
      scm_t_uint32 last_mask =  ((scm_t_uint32)-1) >> (32*word_len - bit_len);
      scm_t_uint32 xor_mask = scm_to_bool (obj)? 0 : ((scm_t_uint32)-1);
      scm_t_uint32 *bits1, *bits2;
      size_t count = 0, i;

      if (scm_c_bitvector_length (v) != bit_len)
	scm_misc_error (NULL,
			"bit vectors must have equal length",
			SCM_EOL);

      if (bit_len == 0)
	return scm_from_size_t (0);

      bits1 = scm_bitvector_elements (v);
      bits2 = scm_bitvector_elements (kv);

      for (i = 0; i < word_len-1; i++)
	count += count_ones ((bits1[i]^xor_mask) & bits2[i]);
      count += count_ones ((bits1[i]^xor_mask) & bits2[i] & last_mask);

      scm_bitvector_release (kv);
      scm_bitvector_release (v);

      return scm_from_size_t (count);
    }
  else if (scm_is_true (scm_u32vector_p (kv)))
    {
      size_t count = 0, ulen, i;
      scm_t_uint32 *indices;
      int bit = scm_to_bool (obj);

      scm_frame_begin (0);

      ulen = scm_c_uniform_vector_length (kv);
      indices = scm_u32vector_elements (kv);
      scm_frame_uniform_vector_release (kv);

      for (i = 0; i < ulen; i++)
	if ((scm_is_true (scm_c_bitvector_ref (v, (size_t)indices[i])) != 0)
	    == (bit != 0))
	  count++;

      scm_frame_end ();

      return scm_from_size_t (count);
    }
  else 
    scm_wrong_type_arg_msg (NULL, 0, kv, "bitvector or u32vector");
}
#undef FUNC_NAME


SCM_DEFINE (scm_bit_invert_x, "bit-invert!", 1, 0, 0, 
           (SCM v),
	    "Modify the bit vector @var{v} by replacing each element with\n"
	    "its negation.")
#define FUNC_NAME s_scm_bit_invert_x
{
  size_t bit_len = scm_c_bitvector_length (v);
  size_t word_len = (bit_len + 31) / 32;
  scm_t_uint32 *bits = scm_bitvector_elements (v);
  size_t i;

  for (i = 0; i < word_len; i++)
    bits[i] = ~bits[i];

  scm_bitvector_release (v);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM 
scm_istr2bve (SCM str)
{
  size_t len = scm_i_string_length (str);
  SCM vec = scm_c_make_bitvector (len, SCM_UNDEFINED);
  SCM res = vec;

  scm_t_uint32 mask;
  size_t k, j;
  const char *c_str = scm_i_string_chars (str);
  scm_t_uint32 *data = scm_bitvector_elements (vec);

  for (k = 0; k < (len + 31) / 32; k++)
    {
      data[k] = 0L;
      j = len - k * 32;
      if (j > 32)
	j = 32;
      for (mask = 1L; j--; mask <<= 1)
	switch (*c_str++)
	  {
	  case '0':
	    break;
	  case '1':
	    data[k] |= mask;
	    break;
	  default:
	    res = SCM_BOOL_F;
	    goto exit;
	  }
    }
  
 exit:
  scm_remember_upto_here_1 (str);
  scm_bitvector_release (vec);
  return res;
}



static SCM 
ra2l (SCM ra, unsigned long base, unsigned long k)
{
  SCM res = SCM_EOL;
  long inc = SCM_ARRAY_DIMS (ra)[k].inc;
  size_t i;
  int enclosed = SCM_ENCLOSED_ARRAYP (ra);
  
  if (SCM_ARRAY_DIMS (ra)[k].ubnd < SCM_ARRAY_DIMS (ra)[k].lbnd)
    return SCM_EOL;
  i = base + (1 + SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd) * inc;
  if (k < SCM_ARRAY_NDIM (ra) - 1)
    {
      do
	{
	  i -= inc;
	  res = scm_cons (ra2l (ra, i, k + 1), res);
	}
      while (i != base);
    }
  else
    do
      {
	i -= inc;
	res = scm_cons (scm_i_cvref (SCM_ARRAY_V (ra), i, enclosed),
			res);
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
  else if (SCM_ARRAYP (v) || SCM_ENCLOSED_ARRAYP (v))
    return ra2l (v, SCM_ARRAY_BASE (v), 0);

  scm_wrong_type_arg_msg (NULL, 0, v, "array");
}
#undef FUNC_NAME


static int l2ra(SCM lst, SCM ra, unsigned long base, unsigned long k);

SCM_DEFINE (scm_list_to_uniform_array, "list->uniform-array", 3, 0, 0,
           (SCM ndim, SCM prot, SCM lst),
	    "@deffnx {Scheme Procedure} list->uniform-vector prot lst\n"
	    "Return a uniform array of the type indicated by prototype\n"
	    "@var{prot} with elements the same as those of @var{lst}.\n"
	    "Elements must be of the appropriate type, no coercions are\n"
	    "done.\n"
	    "\n"
	    "The argument @var{ndim} determines the number of dimensions\n"
	    "of the array.  It is either an exact integer, giving the\n"
	    "number directly, or a list of exact integers, whose length\n"
	    "specifies the number of dimensions and each element is the\n"
	    "lower index bound of its dimension.")
#define FUNC_NAME s_scm_list_to_uniform_array
{
  SCM shape, row;
  SCM ra;
  unsigned long k;

  shape = SCM_EOL;
  row = lst;
  if (scm_is_integer (ndim))
    {
      size_t k = scm_to_size_t (ndim);
      while (k-- > 0)
	{
	  shape = scm_cons (scm_length (row), shape);
	  if (k > 0)
	    row = scm_car (row);
	}
    }
  else
    {
      while (1)
	{
	  shape = scm_cons (scm_list_2 (scm_car (ndim),
					scm_sum (scm_sum (scm_car (ndim),
							  scm_length (row)),
						 scm_from_int (-1))),
			    shape);
	  ndim = scm_cdr (ndim);
	  if (scm_is_pair (ndim))
	    row = scm_car (row);
	  else
	    break;
	}
    }

  ra = scm_dimensions_to_uniform_array (scm_reverse_x (shape, SCM_EOL), prot,
					SCM_UNDEFINED);

  if (scm_is_null (shape))
    {
      SCM_ASRTGO (1 == scm_ilength (lst), badlst);
      scm_array_set_x (ra, SCM_CAR (lst), SCM_EOL);
      return ra;
    }
  if (!SCM_ARRAYP (ra))
    {
      size_t length = scm_c_generalized_vector_length (ra);
      for (k = 0; k < length; k++, lst = SCM_CDR (lst))
	scm_c_generalized_vector_set_x (ra, k, SCM_CAR (lst));
      return ra;
    }
  if (l2ra (lst, ra, SCM_ARRAY_BASE (ra), 0))
    return ra;
  else
    badlst:SCM_MISC_ERROR ("Bad scm_array contents list: ~S",
			   scm_list_1 (lst));
}
#undef FUNC_NAME

static int 
l2ra (SCM lst, SCM ra, unsigned long base, unsigned long k)
{
  register long inc = SCM_ARRAY_DIMS (ra)[k].inc;
  register long n = (1 + SCM_ARRAY_DIMS (ra)[k].ubnd - SCM_ARRAY_DIMS (ra)[k].lbnd);
  int ok = 1;
  if (n <= 0)
    return (scm_is_null (lst));
  if (k < SCM_ARRAY_NDIM (ra) - 1)
    {
      while (n--)
	{
	  if (!scm_is_pair (lst))
	    return 0;
	  ok = ok && l2ra (SCM_CAR (lst), ra, base, k + 1);
	  base += inc;
	  lst = SCM_CDR (lst);
	}
      if (!scm_is_null (lst))
 return 0;
    }
  else
    {
      while (n--)
	{
	  if (!scm_is_pair (lst))
	    return 0;
	  scm_array_set_x (SCM_ARRAY_V (ra), SCM_CAR (lst), scm_from_ulong (base));
	  base += inc;
	  lst = SCM_CDR (lst);
	}
      if (!scm_is_null (lst))
	return 0;
    }
  return ok;
}


/* Print dimension DIM of ARRAY.
 */

static int
scm_i_print_array_dimension (SCM array, int dim, int base, int enclosed,
			     SCM port, scm_print_state *pstate)
{
  scm_t_array_dim *dim_spec = SCM_ARRAY_DIMS (array) + dim;
  long idx;

  scm_putc ('(', port);

  for (idx = dim_spec->lbnd; idx <= dim_spec->ubnd; idx++)
    {
      if (dim < SCM_ARRAY_NDIM(array)-1)
	scm_i_print_array_dimension (array, dim+1, base, enclosed, 
				     port, pstate);
      else
	scm_iprin1 (scm_i_cvref (SCM_ARRAY_V (array), base, enclosed), 
		    port, pstate);
      if (idx < dim_spec->ubnd)
	scm_putc (' ', port);
      base += dim_spec->inc;
    }

  scm_putc (')', port);
  return 1;
}

/* Print an array.  (Only for strict arrays, not for strings, uniform
   vectors, vectors and other stuff that can masquerade as an array.)
*/

/* The array tag is generally of the form
 *
 *   #<rank><unif><@lower><@lower>...
 *
 * <rank> is a positive integer in decimal giving the rank of the
 * array.  It is omitted when the rank is 1 and the array is
 * non-shared and has zero-origin.  For shared arrays and for a
 * non-zero origin, the rank is always printed even when it is 1 to
 * dinstinguish them from ordinary vectors.
 *
 * <unif> is the tag for a uniform (or homogenous) numeric vector,
 * like u8, s16, etc, as defined by SRFI-4.  It is omitted when the
 * array is not uniform.
 *
 * <@lower> is a 'at' sign followed by a integer in decimal giving the
 * lower bound of a dimension.  There is one <@lower> for each
 * dimension.  When all lower bounds are zero, all <@lower> are
 * omitted.
 *
 * Thus, 
 *
 *   #(1 2 3) is non-uniform array of rank 1 with lower bound 0 in
 *   dimension 0.  (I.e., a regular vector.)
 *
 *   #@2(1 2 3) is non-uniform array of rank 1 with lower bound 2 in
 *   dimension 0.
 *
 *   #2((1 2 3) (4 5 6)) is a non-uniform array of rank 2; a 3x3
 *   matrix with index ranges 0..2 and 0..2.
 *
 *   #u32(0 1 2) is a uniform u8 array of rank 1.
 *
 *   #2u32@2@3((1 2) (2 3)) is a uniform u8 array of rank 2 with index
 *   ranges 2..3 and 3..4.
 */

static int
scm_i_print_array (SCM array, SCM port, scm_print_state *pstate)
{
  long ndim = SCM_ARRAY_NDIM (array);
  scm_t_array_dim *dim_specs = SCM_ARRAY_DIMS (array);
  SCM v = SCM_ARRAY_V (array);
  unsigned long base = SCM_ARRAY_BASE (array);
  long i;

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
    if (dim_specs[i].lbnd != 0)
      {
	for (i = 0; i < ndim; i++)
	  {
	    scm_putc ('@', port);
	    scm_uintprint (dim_specs[i].lbnd, 10, port);
	  }
	break;
      }

  return scm_i_print_array_dimension (array, 0, base, 0, port, pstate);
}

static int
scm_i_print_enclosed_array (SCM array, SCM port, scm_print_state *pstate)
{
  size_t base;

  scm_putc ('#', port);
  base = SCM_ARRAY_BASE (array);
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

typedef struct {
  const char *tag;
  SCM *creator_var;
} tag_creator;

static tag_creator tag_creator_table[] = {
  { "", &scm_i_proc_make_vector },
  { "a", &scm_i_proc_make_string },
  { "b", &scm_i_proc_make_bitvector },
  { "u8", &scm_i_proc_make_u8vector },
  { "s8", &scm_i_proc_make_s8vector },
  { "u16", &scm_i_proc_make_u16vector },
  { "s16", &scm_i_proc_make_s16vector },
  { "u32", &scm_i_proc_make_u32vector },
  { "s32", &scm_i_proc_make_s32vector },
  { "u64", &scm_i_proc_make_u64vector },
  { "s64", &scm_i_proc_make_s64vector },
  { "f32", &scm_i_proc_make_f32vector },
  { "f64", &scm_i_proc_make_f64vector },
  { "c32", &scm_i_proc_make_c32vector },
  { "c64", &scm_i_proc_make_c64vector },
  { NULL, NULL }
};

static SCM
scm_i_tag_to_creator (const char *tag, SCM port)
{
  tag_creator *tp;

  for (tp = tag_creator_table; tp->tag; tp++)
    if (!strcmp (tp->tag, tag))
      return *(tp->creator_var);
  
#if SCM_ENABLE_DEPRECATED
  {
    /* Recognize the old syntax, producing the old prototypes.
     */
    SCM proto = SCM_EOL;
    const char *instead;
    switch (tag[0])
      {
      case 'u':
	proto = scm_from_int (1);
	instead = "u32";
	break;
      case 'e':
	proto = scm_from_int (-1);
	instead = "s32";
	break;
      case 's':
	proto = scm_from_double (1.0);
	instead = "f32";
	break;
      case 'i':
	proto = scm_divide (scm_from_int (1), scm_from_int (3));
	instead = "f64";
	break;
      case 'y':
	proto = SCM_MAKE_CHAR (0);
	instead = "s8";
	break;
      case 'h':
	proto = scm_from_locale_symbol ("s");
	instead = "s16";
	break;
      case 'l':
	proto = scm_from_locale_symbol ("l");
	instead = "s64";
	break;
      case 'c':
	proto = scm_c_make_rectangular (0.0, 1.0);
	instead = "c64";
	break;
      default:
	instead = "???";
	break;
      }
    if (!scm_is_eq (proto, SCM_EOL) && tag[1] == '\0')
      {
	scm_c_issue_deprecation_warning_fmt
	  ("The tag '%c' is deprecated for uniform vectors. "
	   "Use '%s' instead.", tag[0], instead);
	return proto;
      }
  }
#endif

  scm_i_input_error (NULL, port,
		     "unrecognized uniform array tag: ~a",
		     scm_list_1 (scm_from_locale_string (tag)));
  return SCM_BOOL_F;
}

SCM
scm_i_read_array (SCM port, int c)
{
  size_t rank;
  int got_rank;
  char tag[80];
  int tag_len;

  SCM lower_bounds, elements;

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

  /* Read rank.  We disallow arrays of rank zero since they do not
     seem to work reliably yet. */
  rank = 0;
  got_rank = 0;
  while ('0' <= c && c <= '9')
    {
      rank = 10*rank + c-'0';
      got_rank = 1;
      c = scm_getc (port);
    }
  if (!got_rank)
    rank = 1;
  else if (rank == 0)
    scm_i_input_error (NULL, port,
		       "array rank must be positive", SCM_EOL);

  /* Read tag. */
  tag_len = 0;
 continue_reading_tag:
  while (c != EOF && c != '(' && c != '@' && tag_len < 80)
    {
      tag[tag_len++] = c;
      c = scm_getc (port);
    }
  tag[tag_len] = '\0';
  
  /* Read lower bounds. */
  lower_bounds = SCM_EOL;
  while (c == '@')
    {
      /* Yeah, right, we should use some ready-made integer parsing
	 routine for this...
       */

      long lbnd = 0;
      long sign = 1;

      c = scm_getc (port);
      if (c == '-')
	{
	  sign = -1;
	  c = scm_getc (port);
	}
      while ('0' <= c && c <= '9')
	{
	  lbnd = 10*lbnd + c-'0';
	  c = scm_getc (port);
	}
      lower_bounds = scm_cons (scm_from_long (sign*lbnd), lower_bounds);
    }

  /* Read nested lists of elements.
   */
  if (c != '(')
    scm_i_input_error (NULL, port,
		       "missing '(' in vector or array literal",
		       SCM_EOL);
  scm_ungetc (c, port);
  elements = scm_read (port);

  if (scm_is_null (lower_bounds))
    lower_bounds = scm_from_size_t (rank);
  else if (scm_ilength (lower_bounds) != rank)
    scm_i_input_error (NULL, port,
		       "the number of lower bounds must match the array rank",
		       SCM_EOL);

  /* Construct array. */
  return scm_list_to_uniform_array (lower_bounds,
				    scm_i_tag_to_creator (tag, port),
				    elements);
}

int 
scm_raprin1 (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_iprin1 (exp, port, pstate);
  return 1;
}

SCM_DEFINE (scm_array_creator, "array-creator", 1, 0, 0, 
           (SCM ra),
	    "Return a procedure that would produce an array of the same type\n"
	    "as @var{array} if used as the @var{creator} with\n"
	    "@code{make-array*}.")
#define FUNC_NAME s_scm_array_creator
{
  if (SCM_ARRAYP (ra))
    return scm_i_generalized_vector_creator (SCM_ARRAY_V (ra));
  else if (scm_is_generalized_vector (ra))
    return scm_i_generalized_vector_creator (ra);
  else if (SCM_ENCLOSED_ARRAYP (ra))
    scm_wrong_type_arg_msg (NULL, 0, ra, "non-enclosed array");
  else
    scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME

#if SCM_ENABLE_DEPRECATED

SCM_DEFINE (scm_array_prototype, "array-prototype", 1, 0, 0, 
           (SCM ra),
	    "Return an object that would produce an array of the same type\n"
	    "as @var{array}, if used as the @var{prototype} for\n"
	    "@code{make-uniform-array}.")
#define FUNC_NAME s_scm_array_prototype
{
  if (SCM_ARRAYP (ra))
    return scm_i_get_old_prototype (SCM_ARRAY_V (ra));
  else if (scm_is_generalized_vector (ra))
    return scm_i_get_old_prototype (ra);
  else if (SCM_ENCLOSED_ARRAYP (ra))
    return SCM_UNSPECIFIED;
  else
    scm_wrong_type_arg_msg (NULL, 0, ra, "array");
}
#undef FUNC_NAME

#endif

static SCM
array_mark (SCM ptr)
{
  return SCM_ARRAY_V (ptr);
}


static size_t
array_free (SCM ptr)
{
  scm_gc_free (SCM_ARRAY_MEM (ptr),
	       (sizeof (scm_t_array) 
		+ SCM_ARRAY_NDIM (ptr) * sizeof (scm_t_array_dim)),
	       "array");
  return 0;
}

void
scm_init_unif ()
{
  scm_tc16_array = scm_make_smob_type ("array", 0);
  scm_set_smob_mark (scm_tc16_array, array_mark);
  scm_set_smob_free (scm_tc16_array, array_free);
  scm_set_smob_print (scm_tc16_array, scm_i_print_array);
  scm_set_smob_equalp (scm_tc16_array, scm_array_equal_p);

  scm_tc16_enclosed_array = scm_make_smob_type ("enclosed-array", 0);
  scm_set_smob_mark (scm_tc16_enclosed_array, array_mark);
  scm_set_smob_free (scm_tc16_enclosed_array, array_free);
  scm_set_smob_print (scm_tc16_enclosed_array, scm_i_print_enclosed_array);
  scm_set_smob_equalp (scm_tc16_enclosed_array, scm_array_equal_p);

  scm_add_feature ("array");

  scm_tc16_bitvector = scm_make_smob_type ("bitvector", 0);
  scm_set_smob_free (scm_tc16_bitvector, bitvector_free);
  scm_set_smob_print (scm_tc16_bitvector, bitvector_print);
  scm_set_smob_equalp (scm_tc16_bitvector, bitvector_equalp);

#include "libguile/unif.x"

  scm_i_proc_make_vector = scm_variable_ref (scm_c_lookup ("make-vector"));
  scm_i_proc_make_string = scm_variable_ref (scm_c_lookup ("make-string"));
  scm_i_proc_make_bitvector =
    scm_variable_ref (scm_c_lookup ("make-bitvector"));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003,2004, 2005, 2006, 2009, 2010, 2013 Free Software Foundation, Inc.
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
#include "libguile/array-handle.h"
#include "libguile/generalized-arrays.h"


SCM_INTERNAL SCM scm_i_array_ref (SCM v,
                                  SCM idx0, SCM idx1, SCM idxN);
SCM_INTERNAL SCM scm_i_array_set_x (SCM v, SCM obj,
                                    SCM idx0, SCM idx1, SCM idxN);


int
scm_is_array (SCM obj)
{
  return scm_i_array_implementation_for_obj (obj) ? 1 : 0;
}

SCM_DEFINE (scm_array_p_2, "array?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if the @var{obj} is an array, and @code{#f} if\n"
	    "not.")
#define FUNC_NAME s_scm_array_p_2
{
  return scm_from_bool (scm_is_array (obj));
}
#undef FUNC_NAME

/* The array type predicate, with an extra argument kept for backward
   compatibility.  Note that we can't use `SCM_DEFINE' directly because there
   would be an argument count mismatch that would be caught by
   `snarf-check-and-output-texi.scm'.  */
SCM
scm_array_p (SCM obj, SCM unused)
{
  return scm_array_p_2 (obj);
}

int
scm_is_typed_array (SCM obj, SCM type)
{
  int ret = 0;
  if (scm_i_array_implementation_for_obj (obj))
    {
      scm_t_array_handle h;

      scm_array_get_handle (obj, &h);
      ret = scm_is_eq (scm_array_handle_element_type (&h), type);
      scm_array_handle_release (&h);
    }

  return ret;
}

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


size_t
scm_c_array_length (SCM array)
{
  scm_t_array_handle handle;
  size_t res;

  scm_array_get_handle (array, &handle);
  if (scm_array_handle_rank (&handle) < 1)
    {
      scm_array_handle_release (&handle);
      scm_wrong_type_arg_msg (NULL, 0, array, "array of nonzero rank");
    }
  res = handle.dims[0].ubnd - handle.dims[0].lbnd + 1;
  scm_array_handle_release (&handle);

  return res;
}

SCM_DEFINE (scm_array_length, "array-length", 1, 0, 0, 
           (SCM array),
	    "Return the length of an array: its first dimension.\n"
            "It is an error to ask for the length of an array of rank 0.")
#define FUNC_NAME s_scm_array_length
{
  return scm_from_size_t (scm_c_array_length (array));
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

SCM_DEFINE (scm_array_type, "array-type", 1, 0, 0, 
           (SCM ra),
	    "")
#define FUNC_NAME s_scm_array_type
{
  scm_t_array_handle h;
  SCM type;

  scm_array_get_handle (ra, &h);
  type = scm_array_handle_element_type (&h);
  scm_array_handle_release (&h);
  
  return type;
}
#undef FUNC_NAME

SCM_DEFINE (scm_array_in_bounds_p, "array-in-bounds?", 1, 0, 1, 
           (SCM ra, SCM args),
	    "Return @code{#t} if its arguments would be acceptable to\n"
	    "@code{array-ref}.")
#define FUNC_NAME s_scm_array_in_bounds_p
{
  SCM res = SCM_BOOL_T;
  size_t k, ndim;
  scm_t_array_dim *s;
  scm_t_array_handle handle;

  SCM_VALIDATE_REST_ARGUMENT (args);

  scm_array_get_handle (ra, &handle);
  s = scm_array_handle_dims (&handle);
  ndim = scm_array_handle_rank (&handle);

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

  scm_array_handle_release (&handle);
  return res;
}
#undef FUNC_NAME


SCM
scm_c_array_ref_1 (SCM array, ssize_t idx0)
{
  scm_t_array_handle handle;
  SCM res;

  scm_array_get_handle (array, &handle);
  res = scm_array_handle_ref (&handle, scm_array_handle_pos_1 (&handle, idx0));
  scm_array_handle_release (&handle);
  return res;
}


SCM
scm_c_array_ref_2 (SCM array, ssize_t idx0, ssize_t idx1)
{
  scm_t_array_handle handle;
  SCM res;

  scm_array_get_handle (array, &handle);
  res = scm_array_handle_ref (&handle, scm_array_handle_pos_2 (&handle, idx0, idx1));
  scm_array_handle_release (&handle);
  return res;
}


SCM
scm_array_ref (SCM v, SCM args)
{
  scm_t_array_handle handle;
  SCM res;

  scm_array_get_handle (v, &handle);
  res = scm_array_handle_ref (&handle, scm_array_handle_pos (&handle, args));
  scm_array_handle_release (&handle);
  return res;
}


void
scm_c_array_set_1_x (SCM array, SCM obj, ssize_t idx0)
{
  scm_t_array_handle handle;

  scm_array_get_handle (array, &handle);
  scm_array_handle_set (&handle, scm_array_handle_pos_1 (&handle, idx0),
                        obj);
  scm_array_handle_release (&handle);
}


void
scm_c_array_set_2_x (SCM array, SCM obj, ssize_t idx0, ssize_t idx1)
{
  scm_t_array_handle handle;

  scm_array_get_handle (array, &handle);
  scm_array_handle_set (&handle, scm_array_handle_pos_2 (&handle, idx0, idx1),
                        obj);
  scm_array_handle_release (&handle);
}


SCM
scm_array_set_x (SCM v, SCM obj, SCM args)
{
  scm_t_array_handle handle;

  scm_array_get_handle (v, &handle);
  scm_array_handle_set (&handle, scm_array_handle_pos (&handle, args), obj);
  scm_array_handle_release (&handle);
  return SCM_UNSPECIFIED;
}


SCM_DEFINE (scm_i_array_ref, "array-ref", 1, 2, 1,
            (SCM v, SCM idx0, SCM idx1, SCM idxN),
	    "Return the element at the @code{(idx0, idx1, idxN...)}\n"
            "position in array @var{v}.")
#define FUNC_NAME s_scm_i_array_ref
{
  if (SCM_UNBNDP (idx0))
    return scm_array_ref (v, SCM_EOL);
  else if (SCM_UNBNDP (idx1))
    return scm_c_array_ref_1 (v, scm_to_ssize_t (idx0));
  else if (scm_is_null (idxN))
    return scm_c_array_ref_2 (v, scm_to_ssize_t (idx0), scm_to_ssize_t (idx1));
  else
    return scm_array_ref (v, scm_cons (idx0, scm_cons (idx1, idxN)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_i_array_set_x, "array-set!", 2, 2, 1,
            (SCM v, SCM obj, SCM idx0, SCM idx1, SCM idxN),
	    "Set the element at the @code{(idx0, idx1, idxN...)} position\n"
	    "in the array @var{v} to @var{obj}.  The value returned by\n"
            "@code{array-set!} is unspecified.")
#define FUNC_NAME s_scm_i_array_set_x
{
  if (SCM_UNBNDP (idx0))
    scm_array_set_x (v, obj, SCM_EOL);
  else if (SCM_UNBNDP (idx1))
    scm_c_array_set_1_x (v, obj, scm_to_ssize_t (idx0));
  else if (scm_is_null (idxN))
    scm_c_array_set_2_x (v, obj, scm_to_ssize_t (idx0), scm_to_ssize_t (idx1));
  else
    scm_array_set_x (v, obj, scm_cons (idx0, scm_cons (idx1, idxN)));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static SCM 
array_to_list (scm_t_array_handle *h, size_t dim, unsigned long pos)
{
  if (dim == scm_array_handle_rank (h))
    return scm_array_handle_ref (h, pos);
  else
    {
      SCM res = SCM_EOL;
      long inc;
      size_t i;

      i = h->dims[dim].ubnd - h->dims[dim].lbnd + 1;
      inc = h->dims[dim].inc;
      pos += (i - 1) * inc;

      for (; i > 0; i--, pos -= inc)
        res = scm_cons (array_to_list (h, dim + 1, pos), res);
      return res;
    }
}

SCM_DEFINE (scm_array_to_list, "array->list", 1, 0, 0, 
            (SCM array),
	    "Return a list representation of @var{array}.\n\n"
            "It is easiest to specify the behavior of this function by\n"
            "example:\n"
            "@example\n"
            "(array->list #0(a)) @result{} 1\n"
            "(array->list #1(a b)) @result{} (a b)\n"
            "(array->list #2((aa ab) (ba bb)) @result{} ((aa ab) (ba bb))\n"
            "@end example\n")
#define FUNC_NAME s_scm_array_to_list
{
  scm_t_array_handle h;
  SCM res;  
  
  scm_array_get_handle (array, &h);
  res = array_to_list (&h, 0, 0);
  scm_array_handle_release (&h);

  return res;
}
#undef FUNC_NAME

void
scm_init_generalized_arrays ()
{
#include "libguile/generalized-arrays.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

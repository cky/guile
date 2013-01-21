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

#include "libguile/_scm.h"
#include "libguile/__scm.h"

#include "libguile/uniform.h"


const size_t scm_i_array_element_type_sizes[SCM_ARRAY_ELEMENT_TYPE_LAST + 1] = {
  0,
  0,
  1,
  8,
  8, 8,
  16, 16,
  32, 32,
  64, 64,
  32, 64,
  64, 128
};

size_t
scm_array_handle_uniform_element_size (scm_t_array_handle *h)
{
  size_t ret = scm_i_array_element_type_sizes[h->element_type];
  if (ret && ret % 8 == 0)
    return ret / 8;
  else if (ret)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "byte-aligned uniform array");
  else
    scm_wrong_type_arg_msg (NULL, 0, h->array, "uniform array");
}

size_t
scm_array_handle_uniform_element_bit_size (scm_t_array_handle *h)
{
  size_t ret = scm_i_array_element_type_sizes[h->element_type];
  if (ret)
    return ret;
  else
    scm_wrong_type_arg_msg (NULL, 0, h->array, "uniform array");
}

const void *
scm_array_handle_uniform_elements (scm_t_array_handle *h)
{
  return scm_array_handle_uniform_writable_elements (h);
}

void *
scm_array_handle_uniform_writable_elements (scm_t_array_handle *h)
{
  size_t esize;
  scm_t_uint8 *ret;

  esize = scm_array_handle_uniform_element_size (h);
  ret = ((scm_t_uint8*) h->writable_elements) + h->base * esize;
  return ret;
}

int
scm_is_uniform_vector (SCM obj)
{
  scm_t_array_handle h;
  int ret = 0;

  if (scm_is_generalized_vector (obj))
    {
      scm_generalized_vector_get_handle (obj, &h);
      ret = SCM_ARRAY_ELEMENT_TYPE_IS_UNBOXED (h.element_type);
      scm_array_handle_release (&h);
    }
  return ret;
}

size_t
scm_c_uniform_vector_length (SCM uvec)
{
  if (!scm_is_uniform_vector (uvec))
    scm_wrong_type_arg_msg ("uniform-vector-length", 1, uvec,
                            "uniform vector");

  return scm_c_generalized_vector_length (uvec);
}

SCM_DEFINE (scm_uniform_vector_p, "uniform-vector?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a uniform vector.")
#define FUNC_NAME s_scm_uniform_vector_p
{
  return scm_from_bool (scm_is_uniform_vector (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_element_type, "uniform-vector-element-type", 1, 0, 0,
	    (SCM v),
	    "Return the type of the elements in the uniform vector, @var{v}.")
#define FUNC_NAME s_scm_uniform_vector_element_type
{
  scm_t_array_handle h;
  SCM ret;
  
  if (!scm_is_uniform_vector (v))
    scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARG1, v, "uniform vector");
  scm_array_get_handle (v, &h);
  ret = scm_array_handle_element_type (&h);
  scm_array_handle_release (&h);
  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_element_size, "uniform-vector-element-size", 1, 0, 0,
	    (SCM v),
	    "Return the number of bytes allocated to each element in the\n"
            "uniform vector, @var{v}.")
#define FUNC_NAME s_scm_uniform_vector_element_size
{
  scm_t_array_handle h;
  size_t len;
  ssize_t inc;
  SCM ret;
  scm_uniform_vector_elements (v, &h, &len, &inc);
  ret = scm_from_size_t (scm_array_handle_uniform_element_size (&h));
  scm_array_handle_release (&h);
  return ret;
}
#undef FUNC_NAME

SCM
scm_c_uniform_vector_ref (SCM v, size_t idx)
{
  if (!scm_is_uniform_vector (v))
    scm_wrong_type_arg_msg (NULL, 0, v, "uniform vector");
  return scm_c_generalized_vector_ref (v, idx);
}

SCM_DEFINE (scm_uniform_vector_ref, "uniform-vector-ref", 2, 0, 0,
	    (SCM v, SCM idx),
	    "Return the element at index @var{idx} of the\n"
	    "homogeneous numeric vector @var{v}.")
#define FUNC_NAME s_scm_uniform_vector_ref
{
  return scm_c_uniform_vector_ref (v, scm_to_size_t (idx));
}
#undef FUNC_NAME

void
scm_c_uniform_vector_set_x (SCM v, size_t idx, SCM val)
{
  if (!scm_is_uniform_vector (v))
    scm_wrong_type_arg_msg (NULL, 0, v, "uniform vector");
  scm_c_generalized_vector_set_x (v, idx, val);
}

SCM_DEFINE (scm_uniform_vector_set_x, "uniform-vector-set!", 3, 0, 0,
	    (SCM v, SCM idx, SCM val),
	    "Set the element at index @var{idx} of the\n"
	    "homogeneous numeric vector @var{v} to @var{val}.")
#define FUNC_NAME s_scm_uniform_vector_set_x
{
  scm_c_uniform_vector_set_x (v, scm_to_size_t (idx), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_to_list, "uniform-vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the uniform numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_uniform_vector_to_list
{
  if (!scm_is_uniform_vector (uvec))
    scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARG1, uvec, "uniform vector");
  return scm_array_to_list (uvec);
}
#undef FUNC_NAME

const void *
scm_uniform_vector_elements (SCM uvec, 
			     scm_t_array_handle *h,
			     size_t *lenp, ssize_t *incp)
{
  return scm_uniform_vector_writable_elements (uvec, h, lenp, incp);
}

void *
scm_uniform_vector_writable_elements (SCM uvec, 
				      scm_t_array_handle *h,
				      size_t *lenp, ssize_t *incp)
{
  void *ret;
  scm_generalized_vector_get_handle (uvec, h);
  /* FIXME nonlocal exit */
  ret = scm_array_handle_uniform_writable_elements (h);
  if (lenp)
    {
      scm_t_array_dim *dim = scm_array_handle_dims (h);
      *lenp = dim->ubnd - dim->lbnd + 1;
      *incp = dim->inc;
    }
  return ret;
}

SCM_DEFINE (scm_uniform_vector_length, "uniform-vector-length", 1, 0, 0, 
	    (SCM v),
	    "Return the number of elements in the uniform vector @var{v}.")
#define FUNC_NAME s_scm_uniform_vector_length
{
  return scm_from_size_t (scm_c_uniform_vector_length (v));
}
#undef FUNC_NAME


void
scm_init_uniform (void)
{
#include "libguile/uniform.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

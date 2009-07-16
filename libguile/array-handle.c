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




#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/__scm.h"

#include "libguile/array-handle.h"
#include "libguile/arrays.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/srfi-4.h"
#include "libguile/bitvectors.h"
#include "libguile/bytevectors.h"


static SCM
enclosed_ref (scm_t_array_handle *h, ssize_t pos)
{
  return scm_i_cvref (SCM_I_ARRAY_V (h->array), pos + h->base, 1);
}

static SCM
vector_ref (scm_t_array_handle *h, ssize_t pos)
{
  return ((const SCM *)h->elements)[pos];
}

static SCM
string_ref (scm_t_array_handle *h, ssize_t pos)
{
  pos += h->base;
  if (SCM_I_ARRAYP (h->array))
    return scm_c_string_ref (SCM_I_ARRAY_V (h->array), pos);
  else
    return scm_c_string_ref (h->array, pos);
}

static SCM
bitvector_ref (scm_t_array_handle *h, ssize_t pos)
{
  pos += scm_array_handle_bit_elements_offset (h);
  return
    scm_from_bool (((scm_t_uint32 *)h->elements)[pos/32] & (1l << (pos % 32)));
}

static SCM
bytevector_ref (scm_t_array_handle *h, ssize_t pos)
{
  return scm_from_uint8 (((scm_t_uint8 *) h->elements)[pos]);
}

static SCM
memoize_ref (scm_t_array_handle *h, ssize_t pos)
{
  SCM v = h->array;

  if (SCM_I_ENCLOSED_ARRAYP (v))
    {
      h->ref = enclosed_ref;
      return enclosed_ref (h, pos);
    }

  if (SCM_I_ARRAYP (v))
    v = SCM_I_ARRAY_V (v);

  if (scm_is_vector (v))
    {
      h->elements = scm_array_handle_elements (h);
      h->ref = vector_ref;
    }
  else if (scm_is_uniform_vector (v))
    {
      h->elements = scm_array_handle_uniform_elements (h);
      h->ref = scm_i_uniform_vector_ref_proc (v);
    }
  else if (scm_is_string (v))
    {
      h->ref = string_ref;
    }
  else if (scm_is_bitvector (v))
    {
      h->elements = scm_array_handle_bit_elements (h);
      h->ref = bitvector_ref;
    }
  else if (scm_is_bytevector (v))
    {
      h->elements = scm_array_handle_uniform_elements (h);
      h->ref = bytevector_ref;
    }
  else
    scm_misc_error (NULL, "unknown array type: ~a", scm_list_1 (h->array));

  return h->ref (h, pos);
}

static void
enclosed_set (scm_t_array_handle *h, ssize_t pos, SCM val)
{
  scm_wrong_type_arg_msg (NULL, 0, h->array, "non-enclosed array");
}

static void
vector_set (scm_t_array_handle *h, ssize_t pos, SCM val)
{
  ((SCM *)h->writable_elements)[pos] = val;
}

static void
string_set (scm_t_array_handle *h, ssize_t pos, SCM val)
{
  pos += h->base;
  if (SCM_I_ARRAYP (h->array))
    scm_c_string_set_x (SCM_I_ARRAY_V (h->array), pos, val);
  else
    scm_c_string_set_x (h->array, pos, val);
}

static void
bitvector_set (scm_t_array_handle *h, ssize_t pos, SCM val)
{
  scm_t_uint32 mask;
  pos += scm_array_handle_bit_elements_offset (h);
  mask = 1l << (pos % 32);
  if (scm_to_bool (val))
    ((scm_t_uint32 *)h->writable_elements)[pos/32] |= mask;
  else
    ((scm_t_uint32 *)h->writable_elements)[pos/32] &= ~mask;
}

static void
bytevector_set (scm_t_array_handle *h, ssize_t pos, SCM val)
{
  scm_t_uint8 c_value;
  scm_t_uint8 *elements;

  c_value = scm_to_uint8 (val);
  elements = (scm_t_uint8 *) h->elements;
  elements[pos] = (scm_t_uint8) c_value;
}

static void
memoize_set (scm_t_array_handle *h, ssize_t pos, SCM val)
{
  SCM v = h->array;

  if (SCM_I_ENCLOSED_ARRAYP (v))
    {
      h->set = enclosed_set;
      enclosed_set (h, pos, val);
      return;
    }

  if (SCM_I_ARRAYP (v))
    v = SCM_I_ARRAY_V (v);

  if (scm_is_vector (v))
    {
      h->writable_elements = scm_array_handle_writable_elements (h);
      h->set = vector_set;
    }
  else if (scm_is_uniform_vector (v))
    {
      h->writable_elements = scm_array_handle_uniform_writable_elements (h);
      h->set = scm_i_uniform_vector_set_proc (v);
    }
  else if (scm_is_string (v))
    {
      h->set = string_set;
    }
  else if (scm_is_bitvector (v))
    {
      h->writable_elements = scm_array_handle_bit_writable_elements (h);
      h->set = bitvector_set;
    }
  else if (scm_is_bytevector (v))
    {
      h->elements = scm_array_handle_uniform_writable_elements (h);
      h->set = bytevector_set;
    }
  else
    scm_misc_error (NULL, "unknown array type: ~a", scm_list_1 (h->array));

  h->set (h, pos, val);
}

void
scm_array_get_handle (SCM array, scm_t_array_handle *h)
{
  h->array = array;
  h->ref = memoize_ref;
  h->set = memoize_set;

  if (SCM_I_ARRAYP (array) || SCM_I_ENCLOSED_ARRAYP (array))
    {
      h->dims = SCM_I_ARRAY_DIMS (array);
      h->base = SCM_I_ARRAY_BASE (array);
    }
  else if (scm_is_generalized_vector (array))
    {
      h->dim0.lbnd = 0;
      h->dim0.ubnd = scm_c_generalized_vector_length (array) - 1;
      h->dim0.inc = 1;
      h->dims = &h->dim0;
      h->base = 0;
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, array, "array");
}

void
scm_array_handle_release (scm_t_array_handle *h)
{
  /* Nothing to do here until arrays need to be reserved for real.
   */
}

size_t
scm_array_handle_rank (scm_t_array_handle *h)
{
  if (SCM_I_ARRAYP (h->array) || SCM_I_ENCLOSED_ARRAYP (h->array))
    return SCM_I_ARRAY_NDIM (h->array);
  else
    return 1;
}

scm_t_array_dim *
scm_array_handle_dims (scm_t_array_handle *h)
{
  return h->dims;
}

const SCM *
scm_array_handle_elements (scm_t_array_handle *h)
{
  SCM vec = h->array;
  if (SCM_I_ARRAYP (vec))
    vec = SCM_I_ARRAY_V (vec);
  if (SCM_I_IS_VECTOR (vec))
    return SCM_I_VECTOR_ELTS (vec) + h->base;
  scm_wrong_type_arg_msg (NULL, 0, h->array, "non-uniform array");
}

SCM *
scm_array_handle_writable_elements (scm_t_array_handle *h)
{
  SCM vec = h->array;
  if (SCM_I_ARRAYP (vec))
    vec = SCM_I_ARRAY_V (vec);
  if (SCM_I_IS_VECTOR (vec))
    return SCM_I_VECTOR_WELTS (vec) + h->base;
  scm_wrong_type_arg_msg (NULL, 0, h->array, "non-uniform array");
}


void
scm_init_array_handle (void)
{
#include "libguile/array-handle.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003,2004, 2005, 2006, 2009, 2013 Free Software Foundation, Inc.
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


SCM scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_LAST + 1];


#define ARRAY_IMPLS_N_STATIC_ALLOC 7
static scm_t_array_implementation array_impls[ARRAY_IMPLS_N_STATIC_ALLOC];
static int num_array_impls_registered = 0;


void
scm_i_register_array_implementation (scm_t_array_implementation *impl)
{
  if (num_array_impls_registered >= ARRAY_IMPLS_N_STATIC_ALLOC)
    /* need to increase ARRAY_IMPLS_N_STATIC_ALLOC, buster */
    abort ();
  else
    array_impls[num_array_impls_registered++] = *impl;
}

scm_t_array_implementation*
scm_i_array_implementation_for_obj (SCM obj)
{
  int i;
  for (i = 0; i < num_array_impls_registered; i++)
    if (SCM_NIMP (obj)
        && (SCM_CELL_TYPE (obj) & array_impls[i].mask) == array_impls[i].tag)
      return &array_impls[i];
  return NULL;
}

void
scm_array_get_handle (SCM array, scm_t_array_handle *h)
{
  scm_t_array_implementation *impl = scm_i_array_implementation_for_obj (array);
  if (!impl)
    scm_wrong_type_arg_msg (NULL, 0, array, "array");
  h->array = array;
  h->impl = impl;
  h->base = 0;
  h->ndims = 0;
  h->dims = NULL;
  h->element_type = SCM_ARRAY_ELEMENT_TYPE_SCM; /* have to default to
                                                   something... */
  h->elements = NULL;
  h->writable_elements = NULL;
  h->impl->get_handle (array, h);
}

ssize_t
scm_array_handle_pos (scm_t_array_handle *h, SCM indices)
{
  scm_t_array_dim *s = scm_array_handle_dims (h);
  ssize_t pos = 0, i;
  size_t k = scm_array_handle_rank (h);
  
  while (k > 0 && scm_is_pair (indices))
    {
      i = scm_to_signed_integer (SCM_CAR (indices), s->lbnd, s->ubnd);
      pos += (i - s->lbnd) * s->inc;
      k--;
      s++;
      indices = SCM_CDR (indices);
    }
  if (k > 0 || !scm_is_null (indices))
    scm_misc_error (NULL, "wrong number of indices, expecting ~a",
		    scm_list_1 (scm_from_size_t (scm_array_handle_rank (h))));
  return pos;
}

static void
check_array_index_bounds (scm_t_array_dim *dim, ssize_t idx)
{
  if (idx < dim->lbnd || idx > dim->ubnd)
    scm_error (scm_out_of_range_key, NULL, "Value out of range ~S to ~S: ~S",
               scm_list_3 (scm_from_ssize_t (dim->lbnd),
                           scm_from_ssize_t (dim->ubnd),
                           scm_from_ssize_t (idx)),
               scm_list_1 (scm_from_ssize_t (idx)));
}

ssize_t
scm_array_handle_pos_1 (scm_t_array_handle *h, ssize_t idx0)
{
  scm_t_array_dim *dim = scm_array_handle_dims (h);

  if (scm_array_handle_rank (h) != 1)
    scm_misc_error (NULL, "wrong number of indices, expecting ~A",
		    scm_list_1 (scm_from_size_t (scm_array_handle_rank (h))));

  check_array_index_bounds (&dim[0], idx0);

  return (idx0 - dim[0].lbnd) * dim[0].inc;
}

ssize_t
scm_array_handle_pos_2 (scm_t_array_handle *h, ssize_t idx0, ssize_t idx1)
{
  scm_t_array_dim *dim = scm_array_handle_dims (h);

  if (scm_array_handle_rank (h) != 2)
    scm_misc_error (NULL, "wrong number of indices, expecting ~A",
		    scm_list_1 (scm_from_size_t (scm_array_handle_rank (h))));

  check_array_index_bounds (&dim[0], idx0);
  check_array_index_bounds (&dim[1], idx1);

  return ((idx0 - dim[0].lbnd) * dim[0].inc
          + (idx1 - dim[1].lbnd) * dim[1].inc);
}

SCM
scm_array_handle_element_type (scm_t_array_handle *h)
{
  if (h->element_type < 0 || h->element_type > SCM_ARRAY_ELEMENT_TYPE_LAST)
    abort (); /* guile programming error */
  return scm_i_array_element_types[h->element_type];
}

void
scm_array_handle_release (scm_t_array_handle *h)
{
  /* Nothing to do here until arrays need to be reserved for real.
   */
}

const SCM *
scm_array_handle_elements (scm_t_array_handle *h)
{
  if (h->element_type != SCM_ARRAY_ELEMENT_TYPE_SCM)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "non-uniform array");
  return ((const SCM*)h->elements) + h->base;
}

SCM *
scm_array_handle_writable_elements (scm_t_array_handle *h)
{
  if (h->element_type != SCM_ARRAY_ELEMENT_TYPE_SCM)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "non-uniform array");
  return ((SCM*)h->elements) + h->base;
}

void
scm_init_array_handle (void)
{
#define DEFINE_ARRAY_TYPE(tag, TAG)                             \
  scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_##TAG] = scm_from_locale_symbol (#tag)
  
  scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_SCM] = SCM_BOOL_T;
  DEFINE_ARRAY_TYPE (a, CHAR);
  DEFINE_ARRAY_TYPE (b, BIT);
  DEFINE_ARRAY_TYPE (vu8, VU8);
  DEFINE_ARRAY_TYPE (u8, U8);
  DEFINE_ARRAY_TYPE (s8, S8);
  DEFINE_ARRAY_TYPE (u16, U16);
  DEFINE_ARRAY_TYPE (s16, S16);
  DEFINE_ARRAY_TYPE (u32, U32);
  DEFINE_ARRAY_TYPE (s32, S32);
  DEFINE_ARRAY_TYPE (u64, U64);
  DEFINE_ARRAY_TYPE (s64, S64);
  DEFINE_ARRAY_TYPE (f32, F32);
  DEFINE_ARRAY_TYPE (f64, F64);
  DEFINE_ARRAY_TYPE (c32, C32);
  DEFINE_ARRAY_TYPE (c64, C64);

#include "libguile/array-handle.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

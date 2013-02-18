/* classes: h_files */

#ifndef SCM_ARRAY_HANDLE_H
#define SCM_ARRAY_HANDLE_H

/* Copyright (C) 1995, 1996, 1997, 1999, 2000, 2001, 2004, 2006,
 *   2008, 2009, 2011, 2013 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"
#include "libguile/error.h"
#include "libguile/numbers.h"



struct scm_t_array_handle;

typedef SCM (*scm_i_t_array_ref) (struct scm_t_array_handle *, size_t);
typedef void (*scm_i_t_array_set) (struct scm_t_array_handle *, size_t, SCM);

typedef struct
{
  scm_t_bits tag;
  scm_t_bits mask;
  scm_i_t_array_ref vref;
  scm_i_t_array_set vset;
  void (*get_handle)(SCM, struct scm_t_array_handle*);
} scm_t_array_implementation;
  
#define SCM_ARRAY_IMPLEMENTATION(tag_,mask_,vref_,vset_,handle_) \
  SCM_SNARF_INIT ({                                                     \
      scm_t_array_implementation impl;                                  \
      impl.tag = tag_; impl.mask = mask_;                               \
      impl.vref = vref_; impl.vset = vset_;                             \
      impl.get_handle = handle_;                                        \
      scm_i_register_array_implementation (&impl);                      \
  })
  

SCM_INTERNAL void scm_i_register_array_implementation (scm_t_array_implementation *impl);
SCM_INTERNAL scm_t_array_implementation* scm_i_array_implementation_for_obj (SCM obj);




typedef struct scm_t_array_dim
{
  ssize_t lbnd;
  ssize_t ubnd;
  ssize_t inc;
} scm_t_array_dim;

typedef enum
  {
    SCM_ARRAY_ELEMENT_TYPE_SCM = 0,   /* SCM values */
    SCM_ARRAY_ELEMENT_TYPE_CHAR = 1,  /* characters */
    SCM_ARRAY_ELEMENT_TYPE_BIT = 2,   /* packed numeric values */
    SCM_ARRAY_ELEMENT_TYPE_VU8 = 3,
    SCM_ARRAY_ELEMENT_TYPE_U8 = 4,
    SCM_ARRAY_ELEMENT_TYPE_S8 = 5,
    SCM_ARRAY_ELEMENT_TYPE_U16 = 6,
    SCM_ARRAY_ELEMENT_TYPE_S16 = 7,
    SCM_ARRAY_ELEMENT_TYPE_U32 = 8,
    SCM_ARRAY_ELEMENT_TYPE_S32 = 9,
    SCM_ARRAY_ELEMENT_TYPE_U64 = 10,
    SCM_ARRAY_ELEMENT_TYPE_S64 = 11,
    SCM_ARRAY_ELEMENT_TYPE_F32 = 12,
    SCM_ARRAY_ELEMENT_TYPE_F64 = 13,
    SCM_ARRAY_ELEMENT_TYPE_C32 = 14,
    SCM_ARRAY_ELEMENT_TYPE_C64 = 15,
    SCM_ARRAY_ELEMENT_TYPE_LAST = 15
  } scm_t_array_element_type;

SCM_INTERNAL SCM scm_i_array_element_types[];


typedef struct scm_t_array_handle {
  SCM array;
  scm_t_array_implementation *impl;
  /* `Base' is an offset into elements or writable_elements, corresponding to
     the first element in the array. It would be nicer just to adjust the
     elements/writable_elements pointer, but we can't because that element might
     not even be byte-addressable, as is the case with bitvectors. A nicer
     solution would be, well, nice.
   */
  size_t base;
  size_t ndims; /* ndims == the rank of the array */
  scm_t_array_dim *dims;
  scm_t_array_dim dim0;
  scm_t_array_element_type element_type;
  const void *elements;
  void *writable_elements;
} scm_t_array_handle;

#define scm_array_handle_rank(h) ((h)->ndims)
#define scm_array_handle_dims(h) ((h)->dims)

SCM_API void scm_array_get_handle (SCM array, scm_t_array_handle *h);
SCM_API ssize_t scm_array_handle_pos (scm_t_array_handle *h, SCM indices);
SCM_API ssize_t scm_array_handle_pos_1 (scm_t_array_handle *h, ssize_t idx0);
SCM_API ssize_t scm_array_handle_pos_2 (scm_t_array_handle *h, ssize_t idx0, ssize_t idx1);
SCM_API SCM scm_array_handle_element_type (scm_t_array_handle *h);
SCM_API void scm_array_handle_release (scm_t_array_handle *h);
SCM_API const SCM* scm_array_handle_elements (scm_t_array_handle *h);
SCM_API SCM* scm_array_handle_writable_elements (scm_t_array_handle *h);


SCM_INLINE SCM scm_array_handle_ref (scm_t_array_handle *h, ssize_t pos);
SCM_INLINE void scm_array_handle_set (scm_t_array_handle *h, ssize_t pos, SCM val);

#if SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES
/* Either inlining, or being included from inline.c.  */

SCM_INLINE_IMPLEMENTATION SCM
scm_array_handle_ref (scm_t_array_handle *h, ssize_t p)
{
  if (SCM_UNLIKELY (p < 0 && ((size_t)-p) > h->base))
    /* catch overflow */
    scm_out_of_range (NULL, scm_from_ssize_t (p));
  /* perhaps should catch overflow here too */
  return h->impl->vref (h, h->base + p);
}

SCM_INLINE_IMPLEMENTATION void
scm_array_handle_set (scm_t_array_handle *h, ssize_t p, SCM v)
{
  if (SCM_UNLIKELY (p < 0 && ((size_t)-p) > h->base))
    /* catch overflow */
    scm_out_of_range (NULL, scm_from_ssize_t (p));
  /* perhaps should catch overflow here too */
  h->impl->vset (h, h->base + p, v);
}

#endif


SCM_INTERNAL void scm_init_array_handle (void);


#endif  /* SCM_ARRAY_HANDLE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

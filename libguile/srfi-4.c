/* srfi-4.c --- Homogeneous numeric vector datatypes.
 *
 * 	Copyright (C) 2001, 2004 Free Software Foundation, Inc.
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

#include <libguile.h>
#include <string.h>
#include <stdio.h>

#include "libguile/srfi-4.h"
#include "libguile/error.h"
#include "libguile/read.h"
#include "libguile/ports.h"
#include "libguile/chars.h"

/* Smob type code for homogeneous numeric vectors.  */
int scm_tc16_uvec = 0;


/* Accessor macros for the three components of a homogeneous numeric
   vector:
   - The type tag (one of the symbolic constants below).
   - The vector's length (counted in elements).
   - The address of the data area (holding the elements of the
     vector). */
#define SCM_UVEC_TYPE(u)   (SCM_CELL_WORD_1(u))
#define SCM_UVEC_LENGTH(u) ((size_t)SCM_CELL_WORD_2(u))
#define SCM_UVEC_BASE(u)   ((void *)SCM_CELL_WORD_3(u))


/* Symbolic constants encoding the various types of homogeneous
   numeric vectors.  */
#define SCM_UVEC_U8  	0
#define SCM_UVEC_S8  	1
#define SCM_UVEC_U16 	2
#define SCM_UVEC_S16 	3
#define SCM_UVEC_U32 	4
#define SCM_UVEC_S32 	5
#define SCM_UVEC_U64 	6
#define SCM_UVEC_S64 	7
#define SCM_UVEC_F32 	8
#define SCM_UVEC_F64 	9
#define SCM_UVEC_C32   10
#define SCM_UVEC_C64   11


/* This array maps type tags to the size of the elements.  */
static const int uvec_sizes[12] = {
  1, 1,
  2, 2,
  4, 4,
  8, 8,
  sizeof(float), sizeof(double),
  2*sizeof(float), 2*sizeof(double)
};

static const char *uvec_tags[12] = {
  "u8", "s8",
  "u16", "s16",
  "u32", "s32",
  "u64", "s64",
  "f32", "f64",
  "c32", "c64",
};

static const char *uvec_names[12] = {
  "u8vector", "s8vector",
  "u16vector", "s16vector",
  "u32vector", "s32vector",
  "u64vector", "s64vector",
  "f32vector", "f64vector",
  "c32vector", "c64vector"
};

/* ================================================================ */
/* SMOB procedures.                                                 */
/* ================================================================ */


/* Smob print hook for homogeneous vectors.  */
static int
uvec_print (SCM uvec, SCM port, scm_print_state *pstate)
{
  union {
    scm_t_uint8 *u8;
    scm_t_int8 *s8;
    scm_t_uint16 *u16;
    scm_t_int16 *s16;
    scm_t_uint32 *u32;
    scm_t_int32 *s32;
#if SCM_HAVE_T_INT64
    scm_t_uint64 *u64;
    scm_t_int64 *s64;
#endif
    float *f32;
    double *f64;
  } np;

  size_t i = 0;
  const size_t uvlen = SCM_UVEC_LENGTH (uvec);
  void *uptr = SCM_UVEC_BASE (uvec);

  switch (SCM_UVEC_TYPE (uvec))
  {
    case SCM_UVEC_U8: np.u8 = (scm_t_uint8 *) uptr; break;
    case SCM_UVEC_S8: np.s8 = (scm_t_int8 *) uptr; break;
    case SCM_UVEC_U16: np.u16 = (scm_t_uint16 *) uptr; break;
    case SCM_UVEC_S16: np.s16 = (scm_t_int16 *) uptr; break;
    case SCM_UVEC_U32: np.u32 = (scm_t_uint32 *) uptr; break;
    case SCM_UVEC_S32: np.s32 = (scm_t_int32 *) uptr; break;
#if SCM_HAVE_T_INT64
    case SCM_UVEC_U64: np.u64 = (scm_t_uint64 *) uptr; break;
    case SCM_UVEC_S64: np.s64 = (scm_t_int64 *) uptr; break;
#endif
    case SCM_UVEC_F32: np.f32 = (float *) uptr; break;
    case SCM_UVEC_F64: np.f64 = (double *) uptr; break;
    case SCM_UVEC_C32: np.f32 = (float *) uptr; break;
    case SCM_UVEC_C64: np.f64 = (double *) uptr; break;
    default:
      abort ();			/* Sanity check.  */
      break;
  }

  scm_putc ('#', port);
  scm_puts (uvec_tags [SCM_UVEC_TYPE (uvec)], port);
  scm_putc ('(', port);

  while (i < uvlen)
    {
      if (i != 0) scm_puts (" ", port);
      switch (SCM_UVEC_TYPE (uvec))
	{
	case SCM_UVEC_U8: scm_uintprint (*np.u8, 10, port); np.u8++; break;
	case SCM_UVEC_S8: scm_intprint (*np.s8, 10, port); np.s8++; break;
	case SCM_UVEC_U16: scm_uintprint (*np.u16, 10, port); np.u16++; break;
	case SCM_UVEC_S16: scm_intprint (*np.s16, 10, port); np.s16++; break;
	case SCM_UVEC_U32: scm_uintprint (*np.u32, 10, port); np.u32++; break;
	case SCM_UVEC_S32: scm_intprint (*np.s32, 10, port); np.s32++; break;
#if SCM_HAVE_T_INT64
	case SCM_UVEC_U64: scm_uintprint (*np.u64, 10, port); np.u64++; break;
	case SCM_UVEC_S64: scm_intprint (*np.s64, 10, port); np.s64++; break;
#endif
	case SCM_UVEC_F32: scm_i_print_double (*np.f32, port); np.f32++; break;
	case SCM_UVEC_F64: scm_i_print_double (*np.f64, port); np.f64++; break;
	case SCM_UVEC_C32:
	  scm_i_print_complex (np.f32[0], np.f32[1], port);
	  np.f32 += 2;
	  break;
	case SCM_UVEC_C64:
	  scm_i_print_complex (np.f64[0], np.f64[1], port);
	  np.f64 += 2;
	  break;
	default:
	  abort ();			/* Sanity check.  */
	  break;
	}
      i++;
    }
  scm_remember_upto_here_1 (uvec);
  scm_puts (")", port);
  return 1;
}

const char *
scm_i_uniform_vector_tag (SCM uvec)
{
  return uvec_tags[SCM_UVEC_TYPE (uvec)];
}

static SCM
uvec_equalp (SCM a, SCM b)
{
  SCM result = SCM_BOOL_T;
  if (SCM_UVEC_TYPE (a) != SCM_UVEC_TYPE (b))
    result = SCM_BOOL_F;
  else if (SCM_UVEC_LENGTH (a) != SCM_UVEC_LENGTH (b))
    result = SCM_BOOL_F;
  else if (memcmp (SCM_UVEC_BASE (a), SCM_UVEC_BASE (b),
		   SCM_UVEC_LENGTH (a) * uvec_sizes[SCM_UVEC_TYPE(a)]) != 0)
    result = SCM_BOOL_F;

  scm_remember_upto_here_2 (a, b);
  return result;
}

/* Smob free hook for homogeneous numeric vectors. */
static size_t
uvec_free (SCM uvec)
{
  int type = SCM_UVEC_TYPE (uvec);
  scm_gc_free (SCM_UVEC_BASE (uvec),
	       SCM_UVEC_LENGTH (uvec) * uvec_sizes[type],
	       uvec_names[type]);
  return 0;
}

/* ================================================================ */
/* Utility procedures.                                              */
/* ================================================================ */

static SCM_C_INLINE int
is_uvec (int type, SCM obj)
{
  return (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj)
	  && SCM_UVEC_TYPE (obj) == type);
}

static SCM_C_INLINE SCM
uvec_p (int type, SCM obj)
{
  return scm_from_bool (is_uvec (type, obj));
}

static SCM_C_INLINE void
uvec_assert (int type, SCM obj)
{
  if (!is_uvec (type, obj))
    scm_wrong_type_arg_msg (NULL, 0, obj, uvec_names[type]);
}

static SCM
take_uvec (int type, const void *base, size_t len)
{
  SCM_RETURN_NEWSMOB3 (scm_tc16_uvec, type, len, (scm_t_bits) base);
}
  
/* Create a new, uninitialized homogeneous numeric vector of type TYPE
   with space for LEN elements.  */
static SCM
alloc_uvec (int type, size_t len)
{
  void *base = scm_gc_malloc (len * uvec_sizes[type], uvec_names[type]);
  return take_uvec (type, base, len);
}

/* GCC doesn't seem to want to optimize unused switch clauses away,
   so we use a big 'if' in the next two functions.
*/

static SCM_C_INLINE SCM
uvec_fast_ref (int type, void *base, size_t c_idx)
{
  if (type == SCM_UVEC_U8)
    return scm_from_uint8 (((scm_t_uint8*)base)[c_idx]);
  else if (type == SCM_UVEC_S8)
    return scm_from_int8 (((scm_t_int8*)base)[c_idx]);
  else if (type == SCM_UVEC_U16)
    return scm_from_uint16 (((scm_t_uint16*)base)[c_idx]);
  else if (type == SCM_UVEC_S16)
    return scm_from_int16 (((scm_t_int16*)base)[c_idx]);
  else if (type == SCM_UVEC_U32)
    return scm_from_uint32 (((scm_t_uint32*)base)[c_idx]);
  else if (type == SCM_UVEC_S32)
    return scm_from_int32 (((scm_t_int32*)base)[c_idx]);
#if SCM_HAVE_T_INT64
  else if (type == SCM_UVEC_U64)
    return scm_from_uint64 (((scm_t_uint64*)base)[c_idx]);
  else if (type == SCM_UVEC_S64)
    return scm_from_int64 (((scm_t_int64*)base)[c_idx]);
#endif
  else if (type == SCM_UVEC_F32)
    return scm_from_double (((float*)base)[c_idx]);
  else if (type == SCM_UVEC_F64)
    return scm_from_double (((double*)base)[c_idx]);
  else if (type == SCM_UVEC_C32)
    return scm_c_make_rectangular (((float*)base)[2*c_idx],
				   ((float*)base)[2*c_idx+1]);
  else if (type == SCM_UVEC_C64)
    return scm_c_make_rectangular (((double*)base)[2*c_idx],
				   ((double*)base)[2*c_idx+1]);
  else
    return SCM_BOOL_F;
}

static SCM_C_INLINE void
uvec_fast_set_x (int type, void *base, size_t c_idx, SCM val)
{
  if (type == SCM_UVEC_U8)
    (((scm_t_uint8*)base)[c_idx]) = scm_to_uint8 (val);
  else if (type == SCM_UVEC_S8)
    (((scm_t_int8*)base)[c_idx]) = scm_to_int8 (val);
  else if (type == SCM_UVEC_U16)
    (((scm_t_uint16*)base)[c_idx]) = scm_to_uint16 (val);
  else if (type == SCM_UVEC_S16)
    (((scm_t_int16*)base)[c_idx]) = scm_to_int16 (val);
  else if (type == SCM_UVEC_U32)
    (((scm_t_uint32*)base)[c_idx]) = scm_to_uint32 (val);
  else if (type == SCM_UVEC_S32)
    (((scm_t_int32*)base)[c_idx]) = scm_to_int32 (val);
#if SCM_HAVE_T_INT64
  else if (type == SCM_UVEC_U64)
    (((scm_t_uint64*)base)[c_idx]) = scm_to_uint64 (val);
  else if (type == SCM_UVEC_S64)
    (((scm_t_int64*)base)[c_idx]) = scm_to_int64 (val);
#endif
  else if (type == SCM_UVEC_F32)
    (((float*)base)[c_idx]) = scm_to_double (val);
  else if (type == SCM_UVEC_F64)
    (((double*)base)[c_idx]) = scm_to_double (val);
  else if (type == SCM_UVEC_C32)
    {
      (((float*)base)[2*c_idx])   = scm_c_real_part (val);
      (((float*)base)[2*c_idx+1]) = scm_c_imag_part (val);
    }
  else if (type == SCM_UVEC_C64)
    {
      (((double*)base)[2*c_idx])   = scm_c_real_part (val);
      (((double*)base)[2*c_idx+1]) = scm_c_imag_part (val);
    }
}

static SCM_C_INLINE SCM
make_uvec (int type, SCM len, SCM fill)
{
  size_t c_len = scm_to_unsigned_integer (len, 0,
					  SCM_I_SIZE_MAX / uvec_sizes[type]);
  SCM uvec = alloc_uvec (type, c_len);
  if (!SCM_UNBNDP (fill))
    {
      size_t idx;
      void *base = SCM_UVEC_BASE (uvec);
      for (idx = 0; idx < c_len; idx++)
	uvec_fast_set_x (type, base, idx, fill);
    }
  return uvec;
}

static SCM_C_INLINE SCM
uvec_length (int type, SCM uvec)
{
  uvec_assert (type, uvec);
  return scm_from_size_t (SCM_UVEC_LENGTH (uvec));
}

static SCM_C_INLINE SCM
uvec_ref (int type, SCM uvec, SCM idx)
{
  size_t c_idx;
  SCM res;

  uvec_assert (type, uvec);
  c_idx = scm_to_unsigned_integer (idx, 0, SCM_UVEC_LENGTH (uvec)-1);
  res = uvec_fast_ref (type, SCM_UVEC_BASE(uvec), c_idx);
  scm_remember_upto_here_1 (uvec);
  return res;
}

static SCM_C_INLINE SCM
uvec_set_x (int type, SCM uvec, SCM idx, SCM val)
{
  size_t c_idx;

  uvec_assert (type, uvec);
  c_idx = scm_to_unsigned_integer (idx, 0, SCM_UVEC_LENGTH (uvec)-1);
  uvec_fast_set_x (type, SCM_UVEC_BASE(uvec), c_idx, val);
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}

static SCM_C_INLINE SCM
uvec_to_list (int type, SCM uvec)
{
  size_t c_idx;
  void *base;
  SCM res = SCM_EOL;

  uvec_assert (type, uvec);
  c_idx = SCM_UVEC_LENGTH (uvec);
  base = SCM_UVEC_BASE (uvec);
  while (c_idx-- > 0)
    res = scm_cons (uvec_fast_ref (type, base, c_idx), res);
  scm_remember_upto_here_1 (uvec);
  return res;
}

static SCM_C_INLINE SCM
list_to_uvec (int type, SCM list)
{
  SCM uvec;
  void *base;
  long idx;
  long len = scm_ilength (list);
  if (len < 0)
    scm_wrong_type_arg_msg (NULL, 0, list, "proper list");

  uvec = alloc_uvec (type, len);
  base = SCM_UVEC_BASE (uvec);
  idx = 0;
  while (scm_is_pair (list) && idx < len)
    {
      uvec_fast_set_x (type, base, idx, SCM_CAR (list));
      list = SCM_CDR (list);
      idx++;
    }
  return uvec;
}

static SCM
coerce_to_uvec (int type, SCM obj)
{
  if (is_uvec (type, obj))
    return obj;
  else if (scm_is_pair (obj))
    return list_to_uvec (type, obj);
  else if (scm_is_true (scm_vector_p (obj)))
    {
      SCM len = scm_vector_length (obj);
      SCM uvec = make_uvec (type, len, SCM_UNDEFINED);
      size_t clen = scm_to_size_t (len), i;
      void *base = SCM_UVEC_BASE (uvec);
      for (i = 0; i < clen; i++)
	uvec_fast_set_x (type, base, i, SCM_VECTOR_REF (obj, i));
      return uvec;
    }
  else if (scm_is_uniform_vector (obj))
    {
      SCM len = scm_uniform_vector_length (obj);
      SCM uvec = make_uvec (type, len, SCM_UNDEFINED);
      size_t clen = scm_to_size_t (len), i;
      void *base = SCM_UVEC_BASE (uvec);
      for (i = 0; i < clen; i++)
	uvec_fast_set_x (type, base, i,
			 scm_uniform_vector_ref (obj, scm_from_size_t (i)));
      return uvec;
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, obj, "list, vector, or uniform vector");
}

static SCM *uvec_proc_vars[12] = {
  &scm_i_proc_make_u8vector,
  &scm_i_proc_make_s8vector,
  &scm_i_proc_make_u16vector,
  &scm_i_proc_make_s16vector,
  &scm_i_proc_make_u32vector,
  &scm_i_proc_make_s32vector,
  &scm_i_proc_make_u64vector,
  &scm_i_proc_make_s64vector,
  &scm_i_proc_make_f32vector,
  &scm_i_proc_make_f64vector,
  &scm_i_proc_make_c32vector,
  &scm_i_proc_make_c64vector
};

SCM
scm_i_uniform_vector_creator (SCM uvec)
{
  return *(uvec_proc_vars[SCM_UVEC_TYPE(uvec)]);
}

int
scm_is_uniform_vector (SCM obj)
{
  return SCM_SMOB_PREDICATE (scm_tc16_uvec, obj);
}

size_t
scm_c_uniform_vector_length (SCM v)
{
  if (scm_is_uniform_vector (v))
    return SCM_UVEC_LENGTH (v);
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "uniform vector");
}

size_t
scm_c_uniform_vector_size (SCM v)
{
  if (scm_is_uniform_vector (v))
    return SCM_UVEC_LENGTH (v) * uvec_sizes[SCM_UVEC_TYPE (v)];
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "uniform vector");
}

SCM_DEFINE (scm_uniform_vector_p, "uniform-vector?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a uniform vector.")
#define FUNC_NAME s_scm_uniform_vector_p
{
  return scm_from_bool (scm_is_uniform_vector (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_ref, "uniform-vector-ref", 2, 0, 0,
	    (SCM v, SCM idx),
	    "Return the element at index @var{idx} of the\n"
	    "homogenous numeric vector @var{v}.")
#define FUNC_NAME s_scm_uniform_vector_ref
{
  /* Support old argument convention.
   */
  if (scm_is_pair (idx))
    {
      if (!scm_is_null (SCM_CDR (idx)))
	scm_wrong_num_args (NULL);
      idx = SCM_CAR (idx);
    }

  if (scm_is_uniform_vector (v))
    return uvec_ref (SCM_UVEC_TYPE (v), v, idx);
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "uniform vector");
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_set_x, "uniform-vector-set!", 3, 0, 0,
	    (SCM v, SCM idx, SCM val),
	    "Set the element at index @var{idx} of the\n"
	    "homogenous numeric vector @var{v} to @var{val}.")
#define FUNC_NAME s_scm_uniform_vector_set_x
{
  /* Support old argument convention.
   */
  if (scm_is_pair (idx))
    {
      if (!scm_is_null (SCM_CDR (idx)))
	scm_wrong_num_args (NULL);
      idx = SCM_CAR (idx);
    }

  if (scm_is_uniform_vector (v))
    return uvec_set_x (SCM_UVEC_TYPE (v), v, idx, val);
  else
    scm_wrong_type_arg_msg (NULL, 0, v, "uniform vector");
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_vector_to_list, "uniform-vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_uniform_vector_to_list
{
  if (scm_is_uniform_vector (uvec))
    return uvec_to_list (SCM_UVEC_TYPE (uvec), uvec);
  else
    scm_wrong_type_arg_msg (NULL, 0, uvec, "uniform vector");
}
#undef FUNC_NAME

void *
scm_uniform_vector_elements (SCM uvec)
{
  if (scm_is_uniform_vector (uvec))
    return SCM_UVEC_BASE (uvec);
  else
    scm_wrong_type_arg_msg (NULL, 0, uvec, "uniform vector");
}

void
scm_uniform_vector_release (SCM uvec)
{
  /* Nothing to do right now, but this function might come in handy
     when uniform vectors need to be locked when giving away a pointer
     to their elements.
     
     Also, a call to scm_uniform_vector acts like
     scm_remember_upto_here, which is needed in any case.
  */
}

void
scm_frame_uniform_vector_release (SCM uvec)
{
  scm_frame_unwind_handler_with_scm (scm_uniform_vector_release, uvec,
				     SCM_F_WIND_EXPLICITLY);
}

size_t
scm_uniform_vector_element_size (SCM uvec)
{
  if (scm_is_uniform_vector (uvec))
    return uvec_sizes[SCM_UVEC_TYPE (uvec)];
  else
    scm_wrong_type_arg_msg (NULL, 0, uvec, "uniform vector");
}
  
/* return the size of an element in a uniform array or 0 if type not
   found.  */
size_t
scm_uniform_element_size (SCM obj)
{
  if (scm_is_uniform_vector (obj))
    return scm_uniform_vector_element_size (obj);
  else if (SCM_BITVECTOR_P (obj))
    return sizeof (long);
  else
    return 0;
}

SCM_DEFINE (scm_uniform_vector_length, "uniform-vector-length", 1, 0, 0, 
	    (SCM v),
	    "Return the number of elements in @var{uve}.")
#define FUNC_NAME s_scm_uniform_vector_length
{
  if (scm_is_uniform_vector (v))
    return scm_from_size_t (SCM_UVEC_LENGTH (v));
  else if (scm_is_string (v))
    return scm_string_length (v);
  else if (scm_is_true (scm_vector_p (v)))
    return scm_vector_length (v);
  else if (SCM_BITVECTOR_P (v))
    return scm_from_size_t (SCM_BITVECTOR_LENGTH (v));
  else
    scm_wrong_type_arg (NULL, 0, v);
}
#undef FUNC_NAME

/* ================================================================ */
/* Exported procedures.                                             */
/* ================================================================ */

#define TYPE  SCM_UVEC_U8
#define TAG   u8
#define CTYPE scm_t_uint8
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_S8
#define TAG   s8
#define CTYPE scm_t_int8
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_U16
#define TAG   u16
#define CTYPE scm_t_uint16
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_S16
#define TAG   s16
#define CTYPE scm_t_int16
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_U32
#define TAG   u32
#define CTYPE scm_t_uint32
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_S32
#define TAG   s32
#define CTYPE scm_t_int32
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_U64
#define TAG   u64
#define CTYPE scm_t_uint64
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_S64
#define TAG   s64
#define CTYPE scm_t_int64
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_F32
#define TAG   f32
#define CTYPE float
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_F64
#define TAG   f64
#define CTYPE double
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_C32
#define TAG   c32
#define CTYPE float
#include "libguile/srfi-4.i.c"

#define TYPE  SCM_UVEC_C64
#define TAG   c64
#define CTYPE double
#include "libguile/srfi-4.i.c"

SCM scm_i_proc_make_u8vector;
SCM scm_i_proc_make_s8vector;
SCM scm_i_proc_make_u16vector;
SCM scm_i_proc_make_s16vector;
SCM scm_i_proc_make_u32vector;
SCM scm_i_proc_make_s32vector;
SCM scm_i_proc_make_u64vector;
SCM scm_i_proc_make_s64vector;
SCM scm_i_proc_make_f32vector;
SCM scm_i_proc_make_f64vector;
SCM scm_i_proc_make_c32vector;
SCM scm_i_proc_make_c64vector;

/* Create the smob type for homogeneous numeric vectors and install
   the primitives.  */
void
scm_init_srfi_4 (void)
{
  scm_tc16_uvec = scm_make_smob_type ("uvec", 0);
  scm_set_smob_equalp (scm_tc16_uvec, uvec_equalp);
  scm_set_smob_free (scm_tc16_uvec, uvec_free);
  scm_set_smob_print (scm_tc16_uvec, uvec_print);
#include "libguile/srfi-4.x"

#define GETPROC(tag) \
  scm_i_proc_make_##tag##vector = \
    scm_variable_ref (scm_c_lookup ("make-"#tag"vector"))

  GETPROC (u8);
  GETPROC (s8);
  GETPROC (u16);
  GETPROC (s16);
  GETPROC (u32);
  GETPROC (s32);
  GETPROC (u64);
  GETPROC (s64);
  GETPROC (f32);
  GETPROC (f64);
  GETPROC (c32);
  GETPROC (c64);
}

/* End of srfi-4.c.  */

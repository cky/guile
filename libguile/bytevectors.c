/* Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
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
# include <config.h>
#endif

#include <alloca.h>
#include <assert.h>

#include <gmp.h>

#include "libguile/_scm.h"
#include "libguile/extensions.h"
#include "libguile/bytevectors.h"
#include "libguile/strings.h"
#include "libguile/validate.h"
#include "libguile/ieee-754.h"
#include "libguile/arrays.h"
#include "libguile/array-handle.h"
#include "libguile/uniform.h"
#include "libguile/srfi-4.h"

#include <byteswap.h>
#include <striconveh.h>
#include <uniconv.h>
#include <unistr.h>

#ifdef HAVE_LIMITS_H
# include <limits.h>
#else
/* Assuming 32-bit longs.  */
# define ULONG_MAX 4294967295UL
#endif

#include <string.h>



/* Utilities.  */

/* Convenience macros.  These are used by the various templates (macros) that
   are parameterized by integer signedness.  */
#define INT8_T_signed           scm_t_int8
#define INT8_T_unsigned         scm_t_uint8
#define INT16_T_signed          scm_t_int16
#define INT16_T_unsigned        scm_t_uint16
#define INT32_T_signed          scm_t_int32
#define INT32_T_unsigned        scm_t_uint32
#define is_signed_int8(_x)      (((_x) >= -128L) && ((_x) <= 127L))
#define is_unsigned_int8(_x)    ((_x) <= 255UL)
#define is_signed_int16(_x)     (((_x) >= -32768L) && ((_x) <= 32767L))
#define is_unsigned_int16(_x)   ((_x) <= 65535UL)
#define is_signed_int32(_x)     (((_x) >= -2147483648L) && ((_x) <= 2147483647L))
#define is_unsigned_int32(_x)   ((_x) <= 4294967295UL)
#define SIGNEDNESS_signed       1
#define SIGNEDNESS_unsigned     0

#define INT_TYPE(_size, _sign)  INT ## _size ## _T_ ## _sign
#define INT_SWAP(_size)         bswap_ ## _size
#define INT_VALID_P(_size, _sign) is_ ## _sign ## _int ## _size
#define SIGNEDNESS(_sign)       SIGNEDNESS_ ## _sign


#define INTEGER_ACCESSOR_PROLOGUE(_len, _sign)			\
  size_t c_len, c_index;					\
  _sign char *c_bv;						\
								\
  SCM_VALIDATE_BYTEVECTOR (1, bv);				\
  c_index = scm_to_uint (index);				\
								\
  c_len = SCM_BYTEVECTOR_LENGTH (bv);				\
  c_bv = (_sign char *) SCM_BYTEVECTOR_CONTENTS (bv);		\
								\
  if (SCM_UNLIKELY (c_index + ((_len) >> 3UL) - 1 >= c_len))	\
    scm_out_of_range (FUNC_NAME, index);

/* Template for fixed-size integer access (only 8, 16 or 32-bit).  */
#define INTEGER_REF(_len, _sign)                                \
  SCM result;                                                   \
                                                                \
  INTEGER_ACCESSOR_PROLOGUE (_len, _sign);                      \
  SCM_VALIDATE_SYMBOL (3, endianness);                          \
                                                                \
  {                                                             \
      INT_TYPE (_len, _sign)  c_result;                         \
                                                                \
    memcpy (&c_result, &c_bv[c_index], (_len) / 8);             \
    if (!scm_is_eq (endianness, scm_i_native_endianness))       \
      c_result = INT_SWAP (_len) (c_result);                    \
                                                                \
    result = SCM_I_MAKINUM (c_result);                          \
  }                                                             \
                                                                \
  return result;

/* Template for fixed-size integer access using the native endianness.  */
#define INTEGER_NATIVE_REF(_len, _sign)			\
  SCM result;						\
							\
  INTEGER_ACCESSOR_PROLOGUE (_len, _sign);		\
							\
  {							\
    INT_TYPE (_len, _sign)  c_result;			\
							\
    memcpy (&c_result, &c_bv[c_index], (_len) / 8);	\
    result = SCM_I_MAKINUM (c_result);			\
  }							\
							\
  return result;

/* Template for fixed-size integer modification (only 8, 16 or 32-bit).  */
#define INTEGER_SET(_len, _sign)				\
  INTEGER_ACCESSOR_PROLOGUE (_len, _sign);			\
  SCM_VALIDATE_SYMBOL (3, endianness);				\
								\
  {								\
    scm_t_signed_bits c_value;					\
    INT_TYPE (_len, _sign) c_value_short;			\
								\
    if (SCM_UNLIKELY (!SCM_I_INUMP (value)))			\
      scm_wrong_type_arg (FUNC_NAME, 3, value);			\
								\
    c_value = SCM_I_INUM (value);				\
    if (SCM_UNLIKELY (!INT_VALID_P (_len, _sign) (c_value)))	\
      scm_out_of_range (FUNC_NAME, value);			\
								\
    c_value_short = (INT_TYPE (_len, _sign)) c_value;		\
    if (!scm_is_eq (endianness, scm_i_native_endianness))       \
      c_value_short = INT_SWAP (_len) (c_value_short);		\
								\
    memcpy (&c_bv[c_index], &c_value_short, (_len) / 8);	\
  }								\
								\
  return SCM_UNSPECIFIED;

/* Template for fixed-size integer modification using the native
   endianness.  */
#define INTEGER_NATIVE_SET(_len, _sign)				\
  INTEGER_ACCESSOR_PROLOGUE (_len, _sign);			\
								\
  {								\
    scm_t_signed_bits c_value;					\
    INT_TYPE (_len, _sign) c_value_short;			\
								\
    if (SCM_UNLIKELY (!SCM_I_INUMP (value)))			\
      scm_wrong_type_arg (FUNC_NAME, 3, value);			\
								\
    c_value = SCM_I_INUM (value);				\
    if (SCM_UNLIKELY (!INT_VALID_P (_len, _sign) (c_value)))	\
      scm_out_of_range (FUNC_NAME, value);			\
								\
    c_value_short = (INT_TYPE (_len, _sign)) c_value;		\
								\
    memcpy (&c_bv[c_index], &c_value_short, (_len) / 8);	\
  }								\
								\
  return SCM_UNSPECIFIED;



/* Bytevector type.  */

#define SCM_BYTEVECTOR_HEADER_BYTES		\
  (SCM_BYTEVECTOR_HEADER_SIZE * sizeof (SCM))

#define SCM_BYTEVECTOR_SET_LENGTH(_bv, _len)            \
  SCM_SET_CELL_WORD_1 ((_bv), (scm_t_bits) (_len))
#define SCM_BYTEVECTOR_SET_CONTENTS(_bv, _contents)	\
  SCM_SET_CELL_WORD_2 ((_bv), (scm_t_bits) (_contents))
#define SCM_BYTEVECTOR_SET_CONTIGUOUS_P(bv, contiguous_p)	\
  SCM_SET_BYTEVECTOR_FLAGS ((bv),				\
			    SCM_BYTEVECTOR_ELEMENT_TYPE (bv)	\
			    | ((contiguous_p) << 8UL))

#define SCM_BYTEVECTOR_SET_ELEMENT_TYPE(bv, hint)			\
  SCM_SET_BYTEVECTOR_FLAGS ((bv),					\
                            (hint)					\
                            | (SCM_BYTEVECTOR_CONTIGUOUS_P (bv) << 8UL))
#define SCM_BYTEVECTOR_TYPE_SIZE(var)                           \
  (scm_i_array_element_type_sizes[SCM_BYTEVECTOR_ELEMENT_TYPE (var)]/8)
#define SCM_BYTEVECTOR_TYPED_LENGTH(var)                        \
  (SCM_BYTEVECTOR_LENGTH (var) / SCM_BYTEVECTOR_TYPE_SIZE (var))

/* The empty bytevector.  */
SCM scm_null_bytevector = SCM_UNSPECIFIED;


static inline SCM
make_bytevector (size_t len, scm_t_array_element_type element_type)
{
  SCM ret;
  size_t c_len;

  if (SCM_UNLIKELY (element_type > SCM_ARRAY_ELEMENT_TYPE_LAST
                    || scm_i_array_element_type_sizes[element_type] < 8
                    || len >= (SCM_I_SIZE_MAX
                               / (scm_i_array_element_type_sizes[element_type]/8))))
    /* This would be an internal Guile programming error */
    abort ();

  if (SCM_UNLIKELY (len == 0 && element_type == SCM_ARRAY_ELEMENT_TYPE_VU8
		    && SCM_BYTEVECTOR_P (scm_null_bytevector)))
    ret = scm_null_bytevector;
  else
    {
      signed char *contents;

      c_len = len * (scm_i_array_element_type_sizes[element_type] / 8);

      contents = scm_gc_malloc_pointerless (SCM_BYTEVECTOR_HEADER_BYTES + c_len,
					    SCM_GC_BYTEVECTOR);
      ret = PTR2SCM (contents);
      contents += SCM_BYTEVECTOR_HEADER_BYTES;

      SCM_BYTEVECTOR_SET_LENGTH (ret, c_len);
      SCM_BYTEVECTOR_SET_CONTENTS (ret, contents);
      SCM_BYTEVECTOR_SET_CONTIGUOUS_P (ret, 1);
      SCM_BYTEVECTOR_SET_ELEMENT_TYPE (ret, element_type);
    }

  return ret;
}

/* Return a bytevector of LEN elements of type ELEMENT_TYPE, with element
   values taken from CONTENTS.  Assume that the storage for CONTENTS will be
   automatically reclaimed when it becomes unreachable.  */
static inline SCM
make_bytevector_from_buffer (size_t len, void *contents,
			     scm_t_array_element_type element_type)
{
  SCM ret;

  if (SCM_UNLIKELY (len == 0))
    ret = make_bytevector (len, element_type);
  else
    {
      size_t c_len;

      ret = PTR2SCM (scm_gc_malloc (SCM_BYTEVECTOR_HEADER_BYTES,
				    SCM_GC_BYTEVECTOR));

      c_len = len * (scm_i_array_element_type_sizes[element_type] / 8);

      SCM_BYTEVECTOR_SET_LENGTH (ret, c_len);
      SCM_BYTEVECTOR_SET_CONTENTS (ret, contents);
      SCM_BYTEVECTOR_SET_CONTIGUOUS_P (ret, 0);
      SCM_BYTEVECTOR_SET_ELEMENT_TYPE (ret, element_type);
    }

  return ret;
}


/* Return a new bytevector of size LEN octets.  */
SCM
scm_c_make_bytevector (size_t len)
{
  return make_bytevector (len, SCM_ARRAY_ELEMENT_TYPE_VU8);
}

/* Return a new bytevector of size LEN elements.  */
SCM
scm_i_make_typed_bytevector (size_t len, scm_t_array_element_type element_type)
{
  return make_bytevector (len, element_type);
}

/* Return a bytevector of size LEN made up of CONTENTS.  The area pointed to
   by CONTENTS must have been allocated using `scm_gc_malloc ()'.  */
SCM
scm_c_take_bytevector (signed char *contents, size_t len)
{
  return make_bytevector_from_buffer (len, contents, SCM_ARRAY_ELEMENT_TYPE_VU8);
}

SCM
scm_c_take_typed_bytevector (signed char *contents, size_t len,
                             scm_t_array_element_type element_type)
{
  return make_bytevector_from_buffer (len, contents, element_type);
}

/* Shrink BV to C_NEW_LEN (which is assumed to be smaller than its current
   size) and return the new bytevector (possibly different from BV).  */
SCM
scm_c_shrink_bytevector (SCM bv, size_t c_new_len)
{
  SCM new_bv;
  size_t c_len;

  if (SCM_UNLIKELY (c_new_len % SCM_BYTEVECTOR_TYPE_SIZE (bv)))
    /* This would be an internal Guile programming error */
    abort ();

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  if (SCM_UNLIKELY (c_new_len > c_len))
    abort ();

  SCM_BYTEVECTOR_SET_LENGTH (bv, c_new_len);

  if (SCM_BYTEVECTOR_CONTIGUOUS_P (bv))
    new_bv = PTR2SCM (scm_gc_realloc (SCM2PTR (bv),
				      c_len + SCM_BYTEVECTOR_HEADER_BYTES,
				      c_new_len + SCM_BYTEVECTOR_HEADER_BYTES,
				      SCM_GC_BYTEVECTOR));
  else
    {
      signed char *c_bv;

      c_bv = scm_gc_realloc (SCM_BYTEVECTOR_CONTENTS (bv),
			     c_len, c_new_len, SCM_GC_BYTEVECTOR);
      SCM_BYTEVECTOR_SET_CONTENTS (bv, c_bv);

      new_bv = bv;
    }

  return new_bv;
}

int
scm_is_bytevector (SCM obj)
{
  return SCM_BYTEVECTOR_P (obj);
}

size_t
scm_c_bytevector_length (SCM bv)
#define FUNC_NAME "scm_c_bytevector_length"
{
  SCM_VALIDATE_BYTEVECTOR (1, bv);

  return SCM_BYTEVECTOR_LENGTH (bv);
}
#undef FUNC_NAME

scm_t_uint8
scm_c_bytevector_ref (SCM bv, size_t index)
#define FUNC_NAME "scm_c_bytevector_ref"
{
  size_t c_len;
  const scm_t_uint8 *c_bv;

  SCM_VALIDATE_BYTEVECTOR (1, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = (scm_t_uint8 *) SCM_BYTEVECTOR_CONTENTS (bv);

  if (SCM_UNLIKELY (index >= c_len))
    scm_out_of_range (FUNC_NAME, scm_from_size_t (index));

  return c_bv[index];
}
#undef FUNC_NAME

void
scm_c_bytevector_set_x (SCM bv, size_t index, scm_t_uint8 value)
#define FUNC_NAME "scm_c_bytevector_set_x"
{
  size_t c_len;
  scm_t_uint8 *c_bv;

  SCM_VALIDATE_BYTEVECTOR (1, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = (scm_t_uint8 *) SCM_BYTEVECTOR_CONTENTS (bv);

  if (SCM_UNLIKELY (index >= c_len))
    scm_out_of_range (FUNC_NAME, scm_from_size_t (index));

  c_bv[index] = value;
}
#undef FUNC_NAME



int
scm_i_print_bytevector (SCM bv, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  ssize_t ubnd, inc, i;
  scm_t_array_handle h;
  
  scm_array_get_handle (bv, &h);

  scm_putc ('#', port);
  scm_write (scm_array_handle_element_type (&h), port);
  scm_putc ('(', port);
  for (i = h.dims[0].lbnd, ubnd = h.dims[0].ubnd, inc = h.dims[0].inc;
       i <= ubnd; i += inc)
    {
      if (i > 0)
	scm_putc (' ', port);
      scm_write (scm_array_handle_ref (&h, i), port);
    }
  scm_putc (')', port);

  return 1;
}


/* General operations.  */

SCM_SYMBOL (scm_sym_big, "big");
SCM_SYMBOL (scm_sym_little, "little");

SCM scm_endianness_big, scm_endianness_little;

/* Host endianness (a symbol).  */
SCM scm_i_native_endianness = SCM_UNSPECIFIED;

/* Byte-swapping.  */
#ifndef bswap_24
# define bswap_24(_x)				\
  ((((_x) & 0xff0000) >> 16) |			\
   (((_x) & 0x00ff00))       |			\
   (((_x) & 0x0000ff) << 16))
#endif


SCM_DEFINE (scm_native_endianness, "native-endianness", 0, 0, 0,
	    (void),
	    "Return a symbol denoting the machine's native endianness.")
#define FUNC_NAME s_scm_native_endianness
{
  return scm_i_native_endianness;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_p, "bytevector?", 1, 0, 0,
	    (SCM obj),
	    "Return true if @var{obj} is a bytevector.")
#define FUNC_NAME s_scm_bytevector_p
{
  return scm_from_bool (scm_is_bytevector (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_bytevector, "make-bytevector", 1, 1, 0,
	    (SCM len, SCM fill),
	    "Return a newly allocated bytevector of @var{len} bytes, "
	    "optionally filled with @var{fill}.")
#define FUNC_NAME s_scm_make_bytevector
{
  SCM bv;
  unsigned c_len;
  signed char c_fill = '\0';

  SCM_VALIDATE_UINT_COPY (1, len, c_len);
  if (fill != SCM_UNDEFINED)
    {
      int value;

      value = scm_to_int (fill);
      if (SCM_UNLIKELY ((value < -128) || (value > 255)))
	scm_out_of_range (FUNC_NAME, fill);
      c_fill = (signed char) value;
    }

  bv = make_bytevector (c_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
  if (fill != SCM_UNDEFINED)
    {
      unsigned i;
      signed char *contents;

      contents = SCM_BYTEVECTOR_CONTENTS (bv);
      for (i = 0; i < c_len; i++)
	contents[i] = c_fill;
    }
  else
    memset (SCM_BYTEVECTOR_CONTENTS (bv), 0, c_len);

  return bv;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_length, "bytevector-length", 1, 0, 0,
	    (SCM bv),
	    "Return the length (in bytes) of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_length
{
  return scm_from_uint (scm_c_bytevector_length (bv));
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_eq_p, "bytevector=?", 2, 0, 0,
	    (SCM bv1, SCM bv2),
	    "Return is @var{bv1} equals to @var{bv2}---i.e., if they "
	    "have the same length and contents.")
#define FUNC_NAME s_scm_bytevector_eq_p
{
  SCM result = SCM_BOOL_F;
  unsigned c_len1, c_len2;

  SCM_VALIDATE_BYTEVECTOR (1, bv1);
  SCM_VALIDATE_BYTEVECTOR (2, bv2);

  c_len1 = SCM_BYTEVECTOR_LENGTH (bv1);
  c_len2 = SCM_BYTEVECTOR_LENGTH (bv2);

  if (c_len1 == c_len2 && (SCM_BYTEVECTOR_ELEMENT_TYPE (bv1)
                           == SCM_BYTEVECTOR_ELEMENT_TYPE (bv2)))
    {
      signed char *c_bv1, *c_bv2;

      c_bv1 = SCM_BYTEVECTOR_CONTENTS (bv1);
      c_bv2 = SCM_BYTEVECTOR_CONTENTS (bv2);

      result = scm_from_bool (!memcmp (c_bv1, c_bv2, c_len1));
    }

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_fill_x, "bytevector-fill!", 2, 0, 0,
	    (SCM bv, SCM fill),
	    "Fill bytevector @var{bv} with @var{fill}, a byte.")
#define FUNC_NAME s_scm_bytevector_fill_x
{
  unsigned c_len, i;
  signed char *c_bv, c_fill;

  SCM_VALIDATE_BYTEVECTOR (1, bv);
  c_fill = scm_to_int8 (fill);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = SCM_BYTEVECTOR_CONTENTS (bv);

  for (i = 0; i < c_len; i++)
    c_bv[i] = c_fill;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_copy_x, "bytevector-copy!", 5, 0, 0,
	    (SCM source, SCM source_start, SCM target, SCM target_start,
	     SCM len),
	    "Copy @var{len} bytes from @var{source} into @var{target}, "
	    "starting reading from @var{source_start} (a positive index "
	    "within @var{source}) and start writing at "
	    "@var{target_start}.")
#define FUNC_NAME s_scm_bytevector_copy_x
{
  unsigned c_len, c_source_len, c_target_len;
  unsigned c_source_start, c_target_start;
  signed char *c_source, *c_target;

  SCM_VALIDATE_BYTEVECTOR (1, source);
  SCM_VALIDATE_BYTEVECTOR (3, target);

  c_len = scm_to_uint (len);
  c_source_start = scm_to_uint (source_start);
  c_target_start = scm_to_uint (target_start);

  c_source = SCM_BYTEVECTOR_CONTENTS (source);
  c_target = SCM_BYTEVECTOR_CONTENTS (target);
  c_source_len = SCM_BYTEVECTOR_LENGTH (source);
  c_target_len = SCM_BYTEVECTOR_LENGTH (target);

  if (SCM_UNLIKELY (c_source_start + c_len > c_source_len))
    scm_out_of_range (FUNC_NAME, source_start);
  if (SCM_UNLIKELY (c_target_start + c_len > c_target_len))
    scm_out_of_range (FUNC_NAME, target_start);

  memcpy (c_target + c_target_start,
	  c_source + c_source_start,
	  c_len);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_copy, "bytevector-copy", 1, 0, 0,
	    (SCM bv),
	    "Return a newly allocated copy of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_copy
{
  SCM copy;
  unsigned c_len;
  signed char *c_bv, *c_copy;

  SCM_VALIDATE_BYTEVECTOR (1, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = SCM_BYTEVECTOR_CONTENTS (bv);

  copy = make_bytevector (c_len, SCM_BYTEVECTOR_ELEMENT_TYPE (bv));
  c_copy = SCM_BYTEVECTOR_CONTENTS (copy);
  memcpy (c_copy, c_bv, c_len);

  return copy;
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_array_to_bytevector, "uniform-array->bytevector",
            1, 0, 0, (SCM array),
	    "Return a newly allocated bytevector whose contents\n"
            "will be copied from the uniform array @var{array}.")
#define FUNC_NAME s_scm_uniform_array_to_bytevector
{
  SCM contents, ret;
  size_t len, sz, byte_len;
  scm_t_array_handle h;
  const void *elts;
  
  contents = scm_array_contents (array, SCM_BOOL_T);
  if (scm_is_false (contents))
    scm_wrong_type_arg_msg (FUNC_NAME, 0, array, "uniform contiguous array");

  scm_array_get_handle (contents, &h);
  assert (h.base == 0);

  elts = h.elements;
  len = h.dims->inc * (h.dims->ubnd - h.dims->lbnd + 1);
  sz = scm_array_handle_uniform_element_bit_size (&h);
  if (sz >= 8 && ((sz % 8) == 0))
    byte_len = len * (sz / 8);
  else if (sz < 8)
    /* byte_len = ceil (len * sz / 8) */
    byte_len = (len * sz + 7) / 8;
  else
    /* an internal guile error, really */
    SCM_MISC_ERROR ("uniform elements larger than 8 bits must fill whole bytes", SCM_EOL);

  ret = make_bytevector (byte_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
  memcpy (SCM_BYTEVECTOR_CONTENTS (ret), elts, byte_len);

  scm_array_handle_release (&h);

  return ret;
}
#undef FUNC_NAME


/* Operations on bytes and octets.  */

SCM_DEFINE (scm_bytevector_u8_ref, "bytevector-u8-ref", 2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the octet located at @var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_u8_ref
{
  INTEGER_NATIVE_REF (8, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s8_ref, "bytevector-s8-ref", 2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the byte located at @var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_s8_ref
{
  INTEGER_NATIVE_REF (8, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u8_set_x, "bytevector-u8-set!", 3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Return the octet located at @var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_u8_set_x
{
  INTEGER_NATIVE_SET (8, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s8_set_x, "bytevector-s8-set!", 3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Return the octet located at @var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_s8_set_x
{
  INTEGER_NATIVE_SET (8, signed);
}
#undef FUNC_NAME

#undef OCTET_ACCESSOR_PROLOGUE


SCM_DEFINE (scm_bytevector_to_u8_list, "bytevector->u8-list", 1, 0, 0,
	    (SCM bv),
	    "Return a newly allocated list of octets containing the "
	    "contents of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_to_u8_list
{
  SCM lst, pair;
  unsigned c_len, i;
  unsigned char *c_bv;

  SCM_VALIDATE_BYTEVECTOR (1, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);

  lst = scm_make_list (scm_from_uint (c_len), SCM_UNSPECIFIED);
  for (i = 0, pair = lst;
       i < c_len;
       i++, pair = SCM_CDR (pair))
    {
      SCM_SETCAR (pair, SCM_I_MAKINUM (c_bv[i]));
    }

  return lst;
}
#undef FUNC_NAME

SCM_DEFINE (scm_u8_list_to_bytevector, "u8-list->bytevector", 1, 0, 0,
	    (SCM lst),
	    "Turn @var{lst}, a list of octets, into a bytevector.")
#define FUNC_NAME s_scm_u8_list_to_bytevector
{
  SCM bv, item;
  long c_len, i;
  unsigned char *c_bv;

  SCM_VALIDATE_LIST_COPYLEN (1, lst, c_len);

  bv = make_bytevector (c_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
  c_bv = (unsigned char *) SCM_BYTEVECTOR_CONTENTS (bv);

  for (i = 0; i < c_len; lst = SCM_CDR (lst), i++)
    {
      item = SCM_CAR (lst);

      if (SCM_LIKELY (SCM_I_INUMP (item)))
	{
	  scm_t_signed_bits c_item;

	  c_item = SCM_I_INUM (item);
	  if (SCM_LIKELY ((c_item >= 0) && (c_item < 256)))
	    c_bv[i] = (unsigned char) c_item;
	  else
	    goto type_error;
	}
      else
	goto type_error;
    }

  return bv;

 type_error:
  scm_wrong_type_arg (FUNC_NAME, 1, item);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Compute the two's complement of VALUE (a positive integer) on SIZE octets
   using (2^(SIZE * 8) - VALUE).  */
static inline void
twos_complement (mpz_t value, size_t size)
{
  unsigned long bit_count;

  /* We expect BIT_COUNT to fit in a unsigned long thanks to the range
     checking on SIZE performed earlier.  */
  bit_count = (unsigned long) size << 3UL;

  if (SCM_LIKELY (bit_count < sizeof (unsigned long)))
    mpz_ui_sub (value, 1UL << bit_count, value);
  else
    {
      mpz_t max;

      mpz_init (max);
      mpz_ui_pow_ui (max, 2, bit_count);
      mpz_sub (value, max, value);
      mpz_clear (max);
    }
}

static inline SCM
bytevector_large_ref (const char *c_bv, size_t c_size, int signed_p,
		      SCM endianness)
{
  SCM result;
  mpz_t c_mpz;
  int c_endianness, negative_p = 0;

  if (signed_p)
    {
      if (scm_is_eq (endianness, scm_sym_big))
	negative_p = c_bv[0] & 0x80;
      else
	negative_p = c_bv[c_size - 1] & 0x80;
    }

  c_endianness = scm_is_eq (endianness, scm_sym_big) ? 1 : -1;

  mpz_init (c_mpz);
  mpz_import (c_mpz, 1 /* 1 word */, 1 /* word order doesn't matter */,
	      c_size /* word is C_SIZE-byte long */,
	      c_endianness,
	      0 /* nails */, c_bv);

  if (signed_p && negative_p)
    {
      twos_complement (c_mpz, c_size);
      mpz_neg (c_mpz, c_mpz);
    }

  result = scm_from_mpz (c_mpz);
  mpz_clear (c_mpz);  /* FIXME: Needed? */

  return result;
}

static inline int
bytevector_large_set (char *c_bv, size_t c_size, int signed_p,
		      SCM value, SCM endianness)
{
  mpz_t c_mpz;
  int c_endianness, c_sign, err = 0;

  c_endianness = scm_is_eq (endianness, scm_sym_big) ? 1 : -1;

  mpz_init (c_mpz);
  scm_to_mpz (value, c_mpz);

  c_sign = mpz_sgn (c_mpz);
  if (c_sign < 0)
    {
      if (SCM_LIKELY (signed_p))
	{
	  mpz_neg (c_mpz, c_mpz);
	  twos_complement (c_mpz, c_size);
	}
      else
	{
	  err = -1;
	  goto finish;
	}
    }

  if (c_sign == 0)
    /* Zero.  */
    memset (c_bv, 0, c_size);
  else
    {
      size_t word_count, value_size;

      value_size = (mpz_sizeinbase (c_mpz, 2) + (8 * c_size)) / (8 * c_size);
      if (SCM_UNLIKELY (value_size > c_size))
	{
	  err = -2;
	  goto finish;
	}


      mpz_export (c_bv, &word_count, 1 /* word order doesn't matter */,
		  c_size, c_endianness,
		  0 /* nails */, c_mpz);
      if (SCM_UNLIKELY (word_count != 1))
	/* Shouldn't happen since we already checked with VALUE_SIZE.  */
	abort ();
    }

 finish:
  mpz_clear (c_mpz);

  return err;
}

#define GENERIC_INTEGER_ACCESSOR_PROLOGUE(_sign)			\
  unsigned long c_len, c_index, c_size;					\
  char *c_bv;								\
									\
  SCM_VALIDATE_BYTEVECTOR (1, bv);					\
  c_index = scm_to_ulong (index);					\
  c_size = scm_to_ulong (size);						\
									\
  c_len = SCM_BYTEVECTOR_LENGTH (bv);					\
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);				\
									\
  /* C_SIZE must have its 3 higher bits set to zero so that		\
     multiplying it by 8 yields a number that fits in an		\
     unsigned long.  */							\
  if (SCM_UNLIKELY ((c_size == 0) || (c_size >= (ULONG_MAX >> 3L))))	\
    scm_out_of_range (FUNC_NAME, size);					\
  if (SCM_UNLIKELY (c_index + c_size > c_len))				\
    scm_out_of_range (FUNC_NAME, index);


/* Template of an integer reference function.  */
#define GENERIC_INTEGER_REF(_sign)					\
  SCM result;								\
									\
  if (c_size < 3)							\
    {									\
      int swap;								\
      _sign int value;							\
									\
      swap = !scm_is_eq (endianness, scm_i_native_endianness);		\
      switch (c_size)							\
	{								\
	case 1:								\
	  {								\
	    _sign char c_value8;					\
	    memcpy (&c_value8, c_bv, 1);				\
	    value = c_value8;						\
	  }								\
	  break;							\
	case 2:								\
	  {								\
	    INT_TYPE (16, _sign)  c_value16;				\
	    memcpy (&c_value16, c_bv, 2);				\
	    if (swap)							\
	      value = (INT_TYPE (16, _sign)) bswap_16 (c_value16);	\
	    else							\
	      value = c_value16;					\
	  }								\
	  break;							\
	default:							\
	  abort ();							\
	}								\
									\
      result = SCM_I_MAKINUM ((_sign int) value);			\
    }									\
  else									\
    result = bytevector_large_ref ((char *) c_bv,			\
				   c_size, SIGNEDNESS (_sign),		\
				   endianness);				\
									\
  return result;

static inline SCM
bytevector_signed_ref (const char *c_bv, size_t c_size, SCM endianness)
{
  GENERIC_INTEGER_REF (signed);
}

static inline SCM
bytevector_unsigned_ref (const char *c_bv, size_t c_size, SCM endianness)
{
  GENERIC_INTEGER_REF (unsigned);
}


/* Template of an integer assignment function.  */
#define GENERIC_INTEGER_SET(_sign)					\
  if (c_size < 3)							\
    {									\
      scm_t_signed_bits c_value;					\
									\
      if (SCM_UNLIKELY (!SCM_I_INUMP (value)))				\
	goto range_error;						\
									\
      c_value = SCM_I_INUM (value);					\
      switch (c_size)							\
	{								\
	case 1:								\
	  if (SCM_LIKELY (INT_VALID_P (8, _sign) (c_value)))		\
	    {								\
	      _sign char c_value8;					\
	      c_value8 = (_sign char) c_value;				\
	      memcpy (c_bv, &c_value8, 1);				\
	    }								\
	  else								\
	    goto range_error;						\
	  break;							\
									\
	case 2:								\
	  if (SCM_LIKELY (INT_VALID_P (16, _sign) (c_value)))		\
	    {								\
	      int swap;							\
	      INT_TYPE (16, _sign)  c_value16;				\
									\
	      swap = !scm_is_eq (endianness, scm_i_native_endianness);	\
									\
	      if (swap)							\
		c_value16 = (INT_TYPE (16, _sign)) bswap_16 (c_value);	\
	      else							\
		c_value16 = c_value;					\
									\
	      memcpy (c_bv, &c_value16, 2);				\
	    }								\
	  else								\
	    goto range_error;						\
	  break;							\
									\
	default:							\
	  abort ();							\
	}								\
    }									\
  else									\
    {									\
      int err;								\
									\
      err = bytevector_large_set (c_bv, c_size,				\
				  SIGNEDNESS (_sign),			\
				  value, endianness);			\
      if (err)								\
	goto range_error;						\
    }									\
									\
  return;								\
									\
 range_error:								\
  scm_out_of_range (FUNC_NAME, value);					\
  return;

static inline void
bytevector_signed_set (char *c_bv, size_t c_size,
		       SCM value, SCM endianness,
		       const char *func_name)
#define FUNC_NAME func_name
{
  GENERIC_INTEGER_SET (signed);
}
#undef FUNC_NAME

static inline void
bytevector_unsigned_set (char *c_bv, size_t c_size,
			 SCM value, SCM endianness,
			 const char *func_name)
#define FUNC_NAME func_name
{
  GENERIC_INTEGER_SET (unsigned);
}
#undef FUNC_NAME

#undef GENERIC_INTEGER_SET
#undef GENERIC_INTEGER_REF


SCM_DEFINE (scm_bytevector_uint_ref, "bytevector-uint-ref", 4, 0, 0,
	    (SCM bv, SCM index, SCM endianness, SCM size),
	    "Return the @var{size}-octet long unsigned integer at index "
	    "@var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_uint_ref
{
  GENERIC_INTEGER_ACCESSOR_PROLOGUE (unsigned);

  return (bytevector_unsigned_ref (&c_bv[c_index], c_size, endianness));
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_sint_ref, "bytevector-sint-ref", 4, 0, 0,
	    (SCM bv, SCM index, SCM endianness, SCM size),
	    "Return the @var{size}-octet long unsigned integer at index "
	    "@var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_sint_ref
{
  GENERIC_INTEGER_ACCESSOR_PROLOGUE (signed);

  return (bytevector_signed_ref (&c_bv[c_index], c_size, endianness));
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_uint_set_x, "bytevector-uint-set!", 5, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness, SCM size),
	    "Set the @var{size}-octet long unsigned integer at @var{index} "
	    "to @var{value}.")
#define FUNC_NAME s_scm_bytevector_uint_set_x
{
  GENERIC_INTEGER_ACCESSOR_PROLOGUE (unsigned);

  bytevector_unsigned_set (&c_bv[c_index], c_size, value, endianness,
			   FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_sint_set_x, "bytevector-sint-set!", 5, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness, SCM size),
	    "Set the @var{size}-octet long signed integer at @var{index} "
	    "to @var{value}.")
#define FUNC_NAME s_scm_bytevector_sint_set_x
{
  GENERIC_INTEGER_ACCESSOR_PROLOGUE (signed);

  bytevector_signed_set (&c_bv[c_index], c_size, value, endianness,
			 FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Operations on integers of arbitrary size.  */

#define INTEGERS_TO_LIST(_sign)						\
  SCM lst, pair;							\
  size_t i, c_len, c_size;						\
									\
  SCM_VALIDATE_BYTEVECTOR (1, bv);					\
  SCM_VALIDATE_SYMBOL (2, endianness);					\
  c_size = scm_to_uint (size);						\
									\
  c_len = SCM_BYTEVECTOR_LENGTH (bv);					\
  if (SCM_UNLIKELY (c_len == 0))					\
    lst = SCM_EOL;							\
  else if (SCM_UNLIKELY (c_len < c_size))				\
    scm_out_of_range (FUNC_NAME, size);					\
  else									\
    {									\
      const char *c_bv;							\
									\
      c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);			\
									\
      lst = scm_make_list (scm_from_uint (c_len / c_size),		\
			   SCM_UNSPECIFIED);				\
      for (i = 0, pair = lst;						\
	   i <= c_len - c_size;						\
	   i += c_size, c_bv += c_size, pair = SCM_CDR (pair))		\
	{								\
	  SCM_SETCAR (pair,						\
		      bytevector_ ## _sign ## _ref (c_bv, c_size,	\
						    endianness));	\
	}								\
    }									\
									\
  return lst;

SCM_DEFINE (scm_bytevector_to_sint_list, "bytevector->sint-list",
	    3, 0, 0,
	    (SCM bv, SCM endianness, SCM size),
	    "Return a list of signed integers of @var{size} octets "
	    "representing the contents of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_to_sint_list
{
  INTEGERS_TO_LIST (signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_to_uint_list, "bytevector->uint-list",
	    3, 0, 0,
	    (SCM bv, SCM endianness, SCM size),
	    "Return a list of unsigned integers of @var{size} octets "
	    "representing the contents of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_to_uint_list
{
  INTEGERS_TO_LIST (unsigned);
}
#undef FUNC_NAME

#undef INTEGER_TO_LIST


#define INTEGER_LIST_TO_BYTEVECTOR(_sign)				\
  SCM bv;								\
  long c_len;								\
  size_t c_size;							\
  char *c_bv, *c_bv_ptr;						\
									\
  SCM_VALIDATE_LIST_COPYLEN (1, lst, c_len);				\
  SCM_VALIDATE_SYMBOL (2, endianness);					\
  c_size = scm_to_uint (size);						\
									\
  if (SCM_UNLIKELY ((c_size == 0) || (c_size >= (ULONG_MAX >> 3L))))	\
    scm_out_of_range (FUNC_NAME, size);					\
									\
  bv = make_bytevector (c_len * c_size, SCM_ARRAY_ELEMENT_TYPE_VU8);     \
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);				\
									\
  for (c_bv_ptr = c_bv;							\
       !scm_is_null (lst);						\
       lst = SCM_CDR (lst), c_bv_ptr += c_size)				\
    {									\
      bytevector_ ## _sign ## _set (c_bv_ptr, c_size,			\
				    SCM_CAR (lst), endianness,		\
				    FUNC_NAME);				\
    }									\
									\
  return bv;


SCM_DEFINE (scm_uint_list_to_bytevector, "uint-list->bytevector",
	    3, 0, 0,
	    (SCM lst, SCM endianness, SCM size),
	    "Return a bytevector containing the unsigned integers "
	    "listed in @var{lst} and encoded on @var{size} octets "
	    "according to @var{endianness}.")
#define FUNC_NAME s_scm_uint_list_to_bytevector
{
  INTEGER_LIST_TO_BYTEVECTOR (unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_sint_list_to_bytevector, "sint-list->bytevector",
	    3, 0, 0,
	    (SCM lst, SCM endianness, SCM size),
	    "Return a bytevector containing the signed integers "
	    "listed in @var{lst} and encoded on @var{size} octets "
	    "according to @var{endianness}.")
#define FUNC_NAME s_scm_sint_list_to_bytevector
{
  INTEGER_LIST_TO_BYTEVECTOR (signed);
}
#undef FUNC_NAME

#undef INTEGER_LIST_TO_BYTEVECTOR



/* Operations on 16-bit integers.  */

SCM_DEFINE (scm_bytevector_u16_ref, "bytevector-u16-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the unsigned 16-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_u16_ref
{
  INTEGER_REF (16, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s16_ref, "bytevector-s16-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the signed 16-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_s16_ref
{
  INTEGER_REF (16, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u16_native_ref, "bytevector-u16-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 16-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u16_native_ref
{
  INTEGER_NATIVE_REF (16, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s16_native_ref, "bytevector-s16-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 16-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s16_native_ref
{
  INTEGER_NATIVE_REF (16, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u16_set_x, "bytevector-u16-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_u16_set_x
{
  INTEGER_SET (16, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s16_set_x, "bytevector-s16-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_s16_set_x
{
  INTEGER_SET (16, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u16_native_set_x, "bytevector-u16-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the unsigned integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u16_native_set_x
{
  INTEGER_NATIVE_SET (16, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s16_native_set_x, "bytevector-s16-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the signed integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s16_native_set_x
{
  INTEGER_NATIVE_SET (16, signed);
}
#undef FUNC_NAME



/* Operations on 32-bit integers.  */

/* Unfortunately, on 32-bit machines `SCM' is not large enough to hold
   arbitrary 32-bit integers.  Thus we fall back to using the
   `large_{ref,set}' variants on 32-bit machines.  */

#define LARGE_INTEGER_REF(_len, _sign)					\
  INTEGER_ACCESSOR_PROLOGUE(_len, _sign);				\
  SCM_VALIDATE_SYMBOL (3, endianness);					\
									\
  return (bytevector_large_ref ((char *) c_bv + c_index, _len / 8,	\
				SIGNEDNESS (_sign), endianness));

#define LARGE_INTEGER_SET(_len, _sign)					\
  int err;								\
  INTEGER_ACCESSOR_PROLOGUE (_len, _sign);				\
  SCM_VALIDATE_SYMBOL (4, endianness);					\
									\
  err = bytevector_large_set ((char *) c_bv + c_index, _len / 8,	\
			      SIGNEDNESS (_sign), value, endianness);	\
  if (SCM_UNLIKELY (err))						\
     scm_out_of_range (FUNC_NAME, value);				\
									\
  return SCM_UNSPECIFIED;

#define LARGE_INTEGER_NATIVE_REF(_len, _sign)				 \
  INTEGER_ACCESSOR_PROLOGUE(_len, _sign);				 \
  return (bytevector_large_ref ((char *) c_bv + c_index, _len / 8,	 \
				SIGNEDNESS (_sign), scm_i_native_endianness));

#define LARGE_INTEGER_NATIVE_SET(_len, _sign)				\
  int err;								\
  INTEGER_ACCESSOR_PROLOGUE (_len, _sign);				\
									\
  err = bytevector_large_set ((char *) c_bv + c_index, _len / 8,	\
			      SIGNEDNESS (_sign), value,		\
			      scm_i_native_endianness);			\
  if (SCM_UNLIKELY (err))						\
     scm_out_of_range (FUNC_NAME, value);				\
									\
  return SCM_UNSPECIFIED;


SCM_DEFINE (scm_bytevector_u32_ref, "bytevector-u32-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the unsigned 32-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_u32_ref
{
#if SIZEOF_VOID_P > 4
  INTEGER_REF (32, unsigned);
#else
  LARGE_INTEGER_REF (32, unsigned);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s32_ref, "bytevector-s32-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the signed 32-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_s32_ref
{
#if SIZEOF_VOID_P > 4
  INTEGER_REF (32, signed);
#else
  LARGE_INTEGER_REF (32, signed);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u32_native_ref, "bytevector-u32-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 32-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u32_native_ref
{
#if SIZEOF_VOID_P > 4
  INTEGER_NATIVE_REF (32, unsigned);
#else
  LARGE_INTEGER_NATIVE_REF (32, unsigned);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s32_native_ref, "bytevector-s32-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 32-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s32_native_ref
{
#if SIZEOF_VOID_P > 4
  INTEGER_NATIVE_REF (32, signed);
#else
  LARGE_INTEGER_NATIVE_REF (32, signed);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u32_set_x, "bytevector-u32-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_u32_set_x
{
#if SIZEOF_VOID_P > 4
  INTEGER_SET (32, unsigned);
#else
  LARGE_INTEGER_SET (32, unsigned);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s32_set_x, "bytevector-s32-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_s32_set_x
{
#if SIZEOF_VOID_P > 4
  INTEGER_SET (32, signed);
#else
  LARGE_INTEGER_SET (32, signed);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u32_native_set_x, "bytevector-u32-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the unsigned integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u32_native_set_x
{
#if SIZEOF_VOID_P > 4
  INTEGER_NATIVE_SET (32, unsigned);
#else
  LARGE_INTEGER_NATIVE_SET (32, unsigned);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s32_native_set_x, "bytevector-s32-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the signed integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s32_native_set_x
{
#if SIZEOF_VOID_P > 4
  INTEGER_NATIVE_SET (32, signed);
#else
  LARGE_INTEGER_NATIVE_SET (32, signed);
#endif
}
#undef FUNC_NAME



/* Operations on 64-bit integers.  */

/* For 64-bit integers, we use only the `large_{ref,set}' variant.  */

SCM_DEFINE (scm_bytevector_u64_ref, "bytevector-u64-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the unsigned 64-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_u64_ref
{
  LARGE_INTEGER_REF (64, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s64_ref, "bytevector-s64-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the signed 64-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_s64_ref
{
  LARGE_INTEGER_REF (64, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u64_native_ref, "bytevector-u64-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 64-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u64_native_ref
{
  LARGE_INTEGER_NATIVE_REF (64, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s64_native_ref, "bytevector-s64-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 64-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s64_native_ref
{
  LARGE_INTEGER_NATIVE_REF (64, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u64_set_x, "bytevector-u64-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_u64_set_x
{
  LARGE_INTEGER_SET (64, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s64_set_x, "bytevector-s64-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_s64_set_x
{
  LARGE_INTEGER_SET (64, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u64_native_set_x, "bytevector-u64-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the unsigned integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u64_native_set_x
{
  LARGE_INTEGER_NATIVE_SET (64, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s64_native_set_x, "bytevector-s64-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the signed integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s64_native_set_x
{
  LARGE_INTEGER_NATIVE_SET (64, signed);
}
#undef FUNC_NAME



/* Operations on IEEE-754 numbers.  */

/* There are two possible word endians, visible in glibc's <ieee754.h>.
   However, in R6RS, when the endianness is `little', little endian is
   assumed for both the byte order and the word order.  This is clear from
   Section 2.1 of R6RS-lib (in response to
   http://www.r6rs.org/formal-comments/comment-187.txt).  */


/* Convert to/from a floating-point number with different endianness.  This
   method is probably not the most efficient but it should be portable.  */

static inline void
float_to_foreign_endianness (union scm_ieee754_float *target,
			     float source)
{
  union scm_ieee754_float src;

  src.f = source;

#ifdef WORDS_BIGENDIAN
  /* Assuming little endian for both byte and word order.  */
  target->little_endian.negative = src.big_endian.negative;
  target->little_endian.exponent = src.big_endian.exponent;
  target->little_endian.mantissa = src.big_endian.mantissa;
#else
  target->big_endian.negative = src.little_endian.negative;
  target->big_endian.exponent = src.little_endian.exponent;
  target->big_endian.mantissa = src.little_endian.mantissa;
#endif
}

static inline float
float_from_foreign_endianness (const union scm_ieee754_float *source)
{
  union scm_ieee754_float result;

#ifdef WORDS_BIGENDIAN
  /* Assuming little endian for both byte and word order.  */
  result.big_endian.negative = source->little_endian.negative;
  result.big_endian.exponent = source->little_endian.exponent;
  result.big_endian.mantissa = source->little_endian.mantissa;
#else
  result.little_endian.negative = source->big_endian.negative;
  result.little_endian.exponent = source->big_endian.exponent;
  result.little_endian.mantissa = source->big_endian.mantissa;
#endif

  return (result.f);
}

static inline void
double_to_foreign_endianness (union scm_ieee754_double *target,
			      double source)
{
  union scm_ieee754_double src;

  src.d = source;

#ifdef WORDS_BIGENDIAN
  /* Assuming little endian for both byte and word order.  */
  target->little_little_endian.negative  = src.big_endian.negative;
  target->little_little_endian.exponent  = src.big_endian.exponent;
  target->little_little_endian.mantissa0 = src.big_endian.mantissa0;
  target->little_little_endian.mantissa1 = src.big_endian.mantissa1;
#else
  target->big_endian.negative  = src.little_little_endian.negative;
  target->big_endian.exponent  = src.little_little_endian.exponent;
  target->big_endian.mantissa0 = src.little_little_endian.mantissa0;
  target->big_endian.mantissa1 = src.little_little_endian.mantissa1;
#endif
}

static inline double
double_from_foreign_endianness (const union scm_ieee754_double *source)
{
  union scm_ieee754_double result;

#ifdef WORDS_BIGENDIAN
  /* Assuming little endian for both byte and word order.  */
  result.big_endian.negative  = source->little_little_endian.negative;
  result.big_endian.exponent  = source->little_little_endian.exponent;
  result.big_endian.mantissa0 = source->little_little_endian.mantissa0;
  result.big_endian.mantissa1 = source->little_little_endian.mantissa1;
#else
  result.little_little_endian.negative  = source->big_endian.negative;
  result.little_little_endian.exponent  = source->big_endian.exponent;
  result.little_little_endian.mantissa0 = source->big_endian.mantissa0;
  result.little_little_endian.mantissa1 = source->big_endian.mantissa1;
#endif

  return (result.d);
}

/* Template macros to abstract over doubles and floats.
   XXX: Guile can only convert to/from doubles.  */
#define IEEE754_UNION(_c_type)           union scm_ieee754_ ## _c_type
#define IEEE754_TO_SCM(_c_type)          scm_from_double
#define IEEE754_FROM_SCM(_c_type)        scm_to_double
#define IEEE754_FROM_FOREIGN_ENDIANNESS(_c_type)	\
   _c_type ## _from_foreign_endianness
#define IEEE754_TO_FOREIGN_ENDIANNESS(_c_type)	\
   _c_type ## _to_foreign_endianness


/* FIXME: SCM_VALIDATE_REAL rejects integers, etc. grrr */
#define VALIDATE_REAL(pos, v) \
  do { \
    SCM_ASSERT_TYPE (scm_is_real (v), v, pos, FUNC_NAME, "real"); \
  } while (0)

/* Templace getters and setters.  */

#define IEEE754_ACCESSOR_PROLOGUE(_type)			\
  INTEGER_ACCESSOR_PROLOGUE (sizeof (_type) << 3UL, signed);

#define IEEE754_REF(_type)					\
  _type c_result;						\
								\
  IEEE754_ACCESSOR_PROLOGUE (_type);				\
  SCM_VALIDATE_SYMBOL (3, endianness);				\
								\
  if (scm_is_eq (endianness, scm_i_native_endianness))		\
    memcpy (&c_result, &c_bv[c_index], sizeof (c_result));	\
  else								\
    {								\
      IEEE754_UNION (_type) c_raw;				\
								\
      memcpy (&c_raw, &c_bv[c_index], sizeof (c_raw));		\
      c_result =						\
	IEEE754_FROM_FOREIGN_ENDIANNESS (_type) (&c_raw);	\
    }								\
								\
  return (IEEE754_TO_SCM (_type) (c_result));

#define IEEE754_NATIVE_REF(_type)				\
  _type c_result;						\
								\
  IEEE754_ACCESSOR_PROLOGUE (_type);				\
								\
  memcpy (&c_result, &c_bv[c_index], sizeof (c_result));	\
  return (IEEE754_TO_SCM (_type) (c_result));

#define IEEE754_SET(_type)					\
  _type c_value;						\
								\
  IEEE754_ACCESSOR_PROLOGUE (_type);				\
  VALIDATE_REAL (3, value);					\
  SCM_VALIDATE_SYMBOL (4, endianness);				\
  c_value = IEEE754_FROM_SCM (_type) (value);			\
								\
  if (scm_is_eq (endianness, scm_i_native_endianness))		\
    memcpy (&c_bv[c_index], &c_value, sizeof (c_value));	\
  else								\
    {								\
      IEEE754_UNION (_type) c_raw;				\
								\
      IEEE754_TO_FOREIGN_ENDIANNESS (_type) (&c_raw, c_value);	\
      memcpy (&c_bv[c_index], &c_raw, sizeof (c_raw));		\
    }								\
								\
  return SCM_UNSPECIFIED;

#define IEEE754_NATIVE_SET(_type)			\
  _type c_value;					\
							\
  IEEE754_ACCESSOR_PROLOGUE (_type);			\
  VALIDATE_REAL (3, value);				\
  c_value = IEEE754_FROM_SCM (_type) (value);		\
							\
  memcpy (&c_bv[c_index], &c_value, sizeof (c_value));	\
  return SCM_UNSPECIFIED;


/* Single precision.  */

SCM_DEFINE (scm_bytevector_ieee_single_ref,
	    "bytevector-ieee-single-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the IEEE-754 single from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_ieee_single_ref
{
  IEEE754_REF (float);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_single_native_ref,
	    "bytevector-ieee-single-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the IEEE-754 single from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_ieee_single_native_ref
{
  IEEE754_NATIVE_REF (float);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_single_set_x,
	    "bytevector-ieee-single-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store real @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_ieee_single_set_x
{
  IEEE754_SET (float);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_single_native_set_x,
	    "bytevector-ieee-single-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the real @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_ieee_single_native_set_x
{
  IEEE754_NATIVE_SET (float);
}
#undef FUNC_NAME


/* Double precision.  */

SCM_DEFINE (scm_bytevector_ieee_double_ref,
	    "bytevector-ieee-double-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the IEEE-754 double from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_ieee_double_ref
{
  IEEE754_REF (double);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_double_native_ref,
	    "bytevector-ieee-double-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the IEEE-754 double from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_ieee_double_native_ref
{
  IEEE754_NATIVE_REF (double);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_double_set_x,
	    "bytevector-ieee-double-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store real @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_ieee_double_set_x
{
  IEEE754_SET (double);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_double_native_set_x,
	    "bytevector-ieee-double-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the real @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_ieee_double_native_set_x
{
  IEEE754_NATIVE_SET (double);
}
#undef FUNC_NAME


#undef IEEE754_UNION
#undef IEEE754_TO_SCM
#undef IEEE754_FROM_SCM
#undef IEEE754_FROM_FOREIGN_ENDIANNESS
#undef IEEE754_TO_FOREIGN_ENDIANNESS
#undef IEEE754_REF
#undef IEEE754_NATIVE_REF
#undef IEEE754_SET
#undef IEEE754_NATIVE_SET


/* Operations on strings.  */


/* Produce a function that returns the length of a UTF-encoded string.  */
#define UTF_STRLEN_FUNCTION(_utf_width)					\
static inline size_t							\
utf ## _utf_width ## _strlen (const uint ## _utf_width ## _t *str)	\
{									\
  size_t len = 0;							\
  const uint ## _utf_width ## _t *ptr;					\
  for (ptr = str;							\
       *ptr != 0;							\
       ptr++)								\
    {									\
      len++;								\
    }									\
									\
  return (len * ((_utf_width) / 8));					\
}

UTF_STRLEN_FUNCTION (8)


/* Return the length (in bytes) of STR, a UTF-(UTF_WIDTH) encoded string.  */
#define UTF_STRLEN(_utf_width, _str)		\
  utf ## _utf_width ## _strlen (_str)

/* Return the "portable" name of the UTF encoding of size UTF_WIDTH and
   ENDIANNESS (Gnulib's `iconv_open' module guarantees the portability of the
   encoding name).  */
static inline void
utf_encoding_name (char *name, size_t utf_width, SCM endianness)
{
  strcpy (name, "UTF-");
  strcat (name, ((utf_width == 8)
		 ? "8"
		 : ((utf_width == 16)
		    ? "16"
		    : ((utf_width == 32)
		       ? "32"
		       : "??"))));
  strcat (name,
	  ((scm_is_eq (endianness, scm_sym_big))
	   ? "BE"
	   : ((scm_is_eq (endianness, scm_sym_little))
	      ? "LE"
	      : "unknown")));
}

/* Maximum length of a UTF encoding name.  */
#define MAX_UTF_ENCODING_NAME_LEN  16

/* Produce the body of a `string->utf' function.  */
#define STRING_TO_UTF(_utf_width)                                       \
  SCM utf;                                                              \
  int err;                                                              \
  char c_utf_name[MAX_UTF_ENCODING_NAME_LEN];                           \
  char *c_utf = NULL;                                                   \
  size_t c_strlen, c_utf_len = 0;                                       \
                                                                        \
  SCM_VALIDATE_STRING (1, str);                                         \
  if (endianness == SCM_UNDEFINED)                                      \
    endianness = scm_sym_big;                                           \
  else                                                                  \
    SCM_VALIDATE_SYMBOL (2, endianness);                                \
                                                                        \
  utf_encoding_name (c_utf_name, (_utf_width), endianness);             \
                                                                        \
  c_strlen = scm_i_string_length (str);                                 \
  if (scm_i_is_narrow_string (str))                                     \
    {                                                                   \
      err = mem_iconveh (scm_i_string_chars (str), c_strlen,            \
                         "ISO-8859-1", c_utf_name,                      \
                         iconveh_question_mark, NULL,                   \
                         &c_utf, &c_utf_len);                           \
      if (SCM_UNLIKELY (err))                                           \
        scm_syserror_msg (FUNC_NAME, "failed to convert string: ~A",    \
                          scm_list_1 (str), err);                       \
    }                                                                   \
  else                                                                  \
    {                                                                   \
      const scm_t_wchar *wbuf = scm_i_string_wide_chars (str);          \
      c_utf = u32_conv_to_encoding (c_utf_name,                         \
                                    iconveh_question_mark,              \
                                    (scm_t_uint32 *) wbuf,              \
                                    c_strlen, NULL, NULL, &c_utf_len);  \
      if (SCM_UNLIKELY (c_utf == NULL))                                 \
        scm_syserror_msg (FUNC_NAME, "failed to convert string: ~A",    \
                          scm_list_1 (str), errno);                     \
    }                                                                   \
  scm_dynwind_begin (0);                                                \
  scm_dynwind_free (c_utf);                                             \
  utf = make_bytevector (c_utf_len, SCM_ARRAY_ELEMENT_TYPE_VU8);        \
  memcpy (SCM_BYTEVECTOR_CONTENTS (utf), c_utf, c_utf_len);             \
  scm_dynwind_end ();                                                   \
                                                                        \
  return (utf); 



SCM_DEFINE (scm_string_to_utf8, "string->utf8",
	    1, 0, 0,
	    (SCM str),
	    "Return a newly allocated bytevector that contains the UTF-8 "
	    "encoding of @var{str}.")
#define FUNC_NAME s_scm_string_to_utf8
{
  SCM utf;
  uint8_t *c_utf;
  size_t c_strlen, c_utf_len = 0;

  SCM_VALIDATE_STRING (1, str);

  c_strlen = scm_i_string_length (str);
  if (scm_i_is_narrow_string (str))
    c_utf = u8_conv_from_encoding ("ISO-8859-1", iconveh_question_mark,
                                   scm_i_string_chars (str), c_strlen,
                                   NULL, NULL, &c_utf_len);
  else
    {
      const scm_t_wchar *wbuf = scm_i_string_wide_chars (str);
      c_utf = u32_to_u8 ((const uint32_t *) wbuf, c_strlen, NULL, &c_utf_len);
    }
  if (SCM_UNLIKELY (c_utf == NULL))
    scm_syserror (FUNC_NAME);
  else
    {
      scm_dynwind_begin (0);
      scm_dynwind_free (c_utf);

      utf = make_bytevector (c_utf_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
      memcpy (SCM_BYTEVECTOR_CONTENTS (utf), c_utf, c_utf_len);

      scm_dynwind_end ();
    }

  return (utf);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_to_utf16, "string->utf16",
	    1, 1, 0,
	    (SCM str, SCM endianness),
	    "Return a newly allocated bytevector that contains the UTF-16 "
	    "encoding of @var{str}.")
#define FUNC_NAME s_scm_string_to_utf16
{
  STRING_TO_UTF (16);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_to_utf32, "string->utf32",
	    1, 1, 0,
	    (SCM str, SCM endianness),
	    "Return a newly allocated bytevector that contains the UTF-32 "
	    "encoding of @var{str}.")
#define FUNC_NAME s_scm_string_to_utf32
{
  STRING_TO_UTF (32);
}
#undef FUNC_NAME


/* Produce the body of a function that converts a UTF-encoded bytevector to a
   string.  */
#define UTF_TO_STRING(_utf_width)					\
  SCM str = SCM_BOOL_F;							\
  int err;								\
  char *c_str = NULL;                                                   \
  char c_utf_name[MAX_UTF_ENCODING_NAME_LEN];				\
  char *c_utf;                                                          \
  size_t c_strlen = 0, c_utf_len = 0;					\
									\
  SCM_VALIDATE_BYTEVECTOR (1, utf);					\
  if (endianness == SCM_UNDEFINED)					\
    endianness = scm_sym_big;						\
  else									\
    SCM_VALIDATE_SYMBOL (2, endianness);				\
									\
  c_utf_len = SCM_BYTEVECTOR_LENGTH (utf);				\
  c_utf = (char *) SCM_BYTEVECTOR_CONTENTS (utf);			\
  utf_encoding_name (c_utf_name, (_utf_width), endianness);		\
									\
  err = mem_iconveh (c_utf, c_utf_len,					\
		     c_utf_name, "UTF-8",				\
		     iconveh_question_mark, NULL,			\
		     &c_str, &c_strlen);				\
  if (SCM_UNLIKELY (err))						\
    scm_syserror_msg (FUNC_NAME, "failed to convert to string: ~A",	\
		      scm_list_1 (utf), err);				\
  else									\
    {                                                                   \
      str = scm_from_stringn (c_str, c_strlen, "UTF-8",                 \
                              SCM_FAILED_CONVERSION_ERROR);             \
      free (c_str);                                                     \
    }                                                                   \
  return (str);


SCM_DEFINE (scm_utf8_to_string, "utf8->string",
	    1, 0, 0,
	    (SCM utf),
	    "Return a newly allocate string that contains from the UTF-8-"
	    "encoded contents of bytevector @var{utf}.")
#define FUNC_NAME s_scm_utf8_to_string
{
  SCM str;
  const char *c_utf;
  size_t c_utf_len = 0;

  SCM_VALIDATE_BYTEVECTOR (1, utf);

  c_utf_len = SCM_BYTEVECTOR_LENGTH (utf);
  c_utf = (char *) SCM_BYTEVECTOR_CONTENTS (utf);
  str = scm_from_stringn (c_utf, c_utf_len, "UTF-8",
                          SCM_FAILED_CONVERSION_ERROR);

  return (str);
}
#undef FUNC_NAME

SCM_DEFINE (scm_utf16_to_string, "utf16->string",
	    1, 1, 0,
	    (SCM utf, SCM endianness),
	    "Return a newly allocate string that contains from the UTF-16-"
	    "encoded contents of bytevector @var{utf}.")
#define FUNC_NAME s_scm_utf16_to_string
{
  UTF_TO_STRING (16);
}
#undef FUNC_NAME

SCM_DEFINE (scm_utf32_to_string, "utf32->string",
	    1, 1, 0,
	    (SCM utf, SCM endianness),
	    "Return a newly allocate string that contains from the UTF-32-"
	    "encoded contents of bytevector @var{utf}.")
#define FUNC_NAME s_scm_utf32_to_string
{
  UTF_TO_STRING (32);
}
#undef FUNC_NAME


/* Bytevectors as generalized vectors & arrays.  */


static SCM
bytevector_ref_c32 (SCM bv, SCM idx)
{ /* FIXME add some checks */
  const float *contents = (const float*)SCM_BYTEVECTOR_CONTENTS (bv);
  size_t i = scm_to_size_t (idx);
  return scm_c_make_rectangular (contents[i/4], contents[i/4 + 1]);
}

static SCM
bytevector_ref_c64 (SCM bv, SCM idx)
{ /* FIXME add some checks */
  const double *contents = (const double*)SCM_BYTEVECTOR_CONTENTS (bv);
  size_t i = scm_to_size_t (idx);
  return scm_c_make_rectangular (contents[i/8], contents[i/8 + 1]);
}

typedef SCM (*scm_t_bytevector_ref_fn)(SCM, SCM);

const scm_t_bytevector_ref_fn bytevector_ref_fns[SCM_ARRAY_ELEMENT_TYPE_LAST + 1] = 
{
  NULL, /* SCM */
  NULL, /* CHAR */
  NULL, /* BIT */
  scm_bytevector_u8_ref, /* VU8 */
  scm_bytevector_u8_ref, /* U8 */
  scm_bytevector_s8_ref,
  scm_bytevector_u16_native_ref,
  scm_bytevector_s16_native_ref,
  scm_bytevector_u32_native_ref,
  scm_bytevector_s32_native_ref,
  scm_bytevector_u64_native_ref,
  scm_bytevector_s64_native_ref,
  scm_bytevector_ieee_single_native_ref,
  scm_bytevector_ieee_double_native_ref,
  bytevector_ref_c32,
  bytevector_ref_c64
};

static SCM
bv_handle_ref (scm_t_array_handle *h, size_t index)
{
  SCM byte_index;
  scm_t_bytevector_ref_fn ref_fn;
  
  ref_fn = bytevector_ref_fns[h->element_type];
  byte_index =
    scm_from_size_t (index * scm_array_handle_uniform_element_size (h));
  return ref_fn (h->array, byte_index);
}

/* FIXME add checks!!! */
static SCM
bytevector_set_c32 (SCM bv, SCM idx, SCM val)
{ float *contents = (float*)SCM_BYTEVECTOR_CONTENTS (bv);
  size_t i = scm_to_size_t (idx);
  contents[i/4] = scm_c_real_part (val);
  contents[i/4 + 1] = scm_c_imag_part (val);
  return SCM_UNSPECIFIED;
}

static SCM
bytevector_set_c64 (SCM bv, SCM idx, SCM val)
{ double *contents = (double*)SCM_BYTEVECTOR_CONTENTS (bv);
  size_t i = scm_to_size_t (idx);
  contents[i/8] = scm_c_real_part (val);
  contents[i/8 + 1] = scm_c_imag_part (val);
  return SCM_UNSPECIFIED;
}

typedef SCM (*scm_t_bytevector_set_fn)(SCM, SCM, SCM);

const scm_t_bytevector_set_fn bytevector_set_fns[SCM_ARRAY_ELEMENT_TYPE_LAST + 1] = 
{
  NULL, /* SCM */
  NULL, /* CHAR */
  NULL, /* BIT */
  scm_bytevector_u8_set_x, /* VU8 */
  scm_bytevector_u8_set_x, /* U8 */
  scm_bytevector_s8_set_x,
  scm_bytevector_u16_native_set_x,
  scm_bytevector_s16_native_set_x,
  scm_bytevector_u32_native_set_x,
  scm_bytevector_s32_native_set_x,
  scm_bytevector_u64_native_set_x,
  scm_bytevector_s64_native_set_x,
  scm_bytevector_ieee_single_native_set_x,
  scm_bytevector_ieee_double_native_set_x,
  bytevector_set_c32,
  bytevector_set_c64
};

static void
bv_handle_set_x (scm_t_array_handle *h, size_t index, SCM val)
{
  SCM byte_index;
  scm_t_bytevector_set_fn set_fn;
  
  set_fn = bytevector_set_fns[h->element_type];
  byte_index =
    scm_from_size_t (index * scm_array_handle_uniform_element_size (h));
  set_fn (h->array, byte_index, val);
}

static void
bytevector_get_handle (SCM v, scm_t_array_handle *h)
{
  h->array = v;
  h->ndims = 1;
  h->dims = &h->dim0;
  h->dim0.lbnd = 0;
  h->dim0.ubnd = SCM_BYTEVECTOR_TYPED_LENGTH (v) - 1;
  h->dim0.inc = 1;
  h->element_type = SCM_BYTEVECTOR_ELEMENT_TYPE (v);
  h->elements = h->writable_elements = SCM_BYTEVECTOR_CONTENTS (v);
}


/* Initialization.  */

void
scm_bootstrap_bytevectors (void)
{
  /* This must be instantiated here because the generalized-vector API may
     want to access bytevectors even though `(rnrs bytevectors)' hasn't been
     loaded.  */
  scm_null_bytevector = make_bytevector (0, SCM_ARRAY_ELEMENT_TYPE_VU8);

#ifdef WORDS_BIGENDIAN
  scm_i_native_endianness = scm_from_latin1_symbol ("big");
#else
  scm_i_native_endianness = scm_from_latin1_symbol ("little");
#endif

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_bytevectors",
			    (scm_t_extension_init_func) scm_init_bytevectors,
			    NULL);

  {
    scm_t_array_implementation impl;

    impl.tag = scm_tc7_bytevector;
    impl.mask = 0x7f;
    impl.vref = bv_handle_ref;
    impl.vset = bv_handle_set_x;
    impl.get_handle = bytevector_get_handle;
    scm_i_register_array_implementation (&impl);
    scm_i_register_vector_constructor
      (scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_VU8],
       scm_make_bytevector);
  }
}

void
scm_init_bytevectors (void)
{
#include "libguile/bytevectors.x"

  scm_endianness_big = scm_sym_big;
  scm_endianness_little = scm_sym_little;
}

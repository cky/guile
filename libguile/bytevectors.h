#ifndef SCM_BYTEVECTORS_H
#define SCM_BYTEVECTORS_H

/* Copyright (C) 2009 Free Software Foundation, Inc.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



#include "libguile/__scm.h"


/* R6RS bytevectors.  */

#define SCM_BYTEVECTOR_LENGTH(_bv)		\
  ((unsigned) SCM_SMOB_DATA (_bv))
#define SCM_BYTEVECTOR_CONTENTS(_bv)		\
  (SCM_BYTEVECTOR_INLINE_P (_bv)			\
   ? (signed char *) SCM_SMOB_OBJECT_2_LOC (_bv)	\
   : (signed char *) SCM_SMOB_DATA_2 (_bv))


SCM_API SCM scm_endianness_big;
SCM_API SCM scm_endianness_little;

SCM_API SCM scm_make_bytevector (SCM, SCM);
SCM_API SCM scm_c_make_bytevector (unsigned);
SCM_API SCM scm_native_endianness (void);
SCM_API SCM scm_bytevector_p (SCM);
SCM_API SCM scm_bytevector_length (SCM);
SCM_API SCM scm_bytevector_eq_p (SCM, SCM);
SCM_API SCM scm_bytevector_fill_x (SCM, SCM);
SCM_API SCM scm_bytevector_copy_x (SCM, SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_copy (SCM);

SCM_API SCM scm_uniform_array_to_bytevector (SCM);

SCM_API SCM scm_bytevector_to_u8_list (SCM);
SCM_API SCM scm_u8_list_to_bytevector (SCM);
SCM_API SCM scm_uint_list_to_bytevector (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_to_uint_list (SCM, SCM, SCM);
SCM_API SCM scm_sint_list_to_bytevector (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_to_sint_list (SCM, SCM, SCM);

SCM_API SCM scm_bytevector_u16_native_ref (SCM, SCM);
SCM_API SCM scm_bytevector_s16_native_ref (SCM, SCM);
SCM_API SCM scm_bytevector_u32_native_ref (SCM, SCM);
SCM_API SCM scm_bytevector_s32_native_ref (SCM, SCM);
SCM_API SCM scm_bytevector_u64_native_ref (SCM, SCM);
SCM_API SCM scm_bytevector_s64_native_ref (SCM, SCM);
SCM_API SCM scm_bytevector_u8_ref (SCM, SCM);
SCM_API SCM scm_bytevector_s8_ref (SCM, SCM);
SCM_API SCM scm_bytevector_uint_ref (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_sint_ref (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u16_ref (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s16_ref (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u32_ref (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s32_ref (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u64_ref (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s64_ref (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u16_native_set_x (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s16_native_set_x (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u32_native_set_x (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s32_native_set_x (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u64_native_set_x (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s64_native_set_x (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u8_set_x (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s8_set_x (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_uint_set_x (SCM, SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_sint_set_x (SCM, SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u16_set_x (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s16_set_x (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u32_set_x (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s32_set_x (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_u64_set_x (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_s64_set_x (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_ieee_single_ref (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_ieee_single_native_ref (SCM, SCM);
SCM_API SCM scm_bytevector_ieee_single_set_x (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_ieee_single_native_set_x (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_ieee_double_ref (SCM, SCM, SCM);
SCM_API SCM scm_bytevector_ieee_double_native_ref (SCM, SCM);
SCM_API SCM scm_bytevector_ieee_double_set_x (SCM, SCM, SCM, SCM);
SCM_API SCM scm_bytevector_ieee_double_native_set_x (SCM, SCM, SCM);
SCM_API SCM scm_string_to_utf8 (SCM);
SCM_API SCM scm_string_to_utf16 (SCM, SCM);
SCM_API SCM scm_string_to_utf32 (SCM, SCM);
SCM_API SCM scm_utf8_to_string (SCM);
SCM_API SCM scm_utf16_to_string (SCM, SCM);
SCM_API SCM scm_utf32_to_string (SCM, SCM);



/* Internal API.  */

/* The threshold (in octets) under which bytevectors are stored "in-line",
   i.e., without allocating memory beside the SMOB itself (a double cell).
   This optimization is necessary since small bytevectors are expected to be
   common.  */
#define SCM_BYTEVECTOR_INLINE_THRESHOLD  (2 * sizeof (SCM))
#define SCM_BYTEVECTOR_INLINEABLE_SIZE_P(_size)	\
  ((_size) <= SCM_BYTEVECTOR_INLINE_THRESHOLD)
#define SCM_BYTEVECTOR_INLINE_P(_bv)				     \
  (SCM_BYTEVECTOR_INLINEABLE_SIZE_P (SCM_BYTEVECTOR_LENGTH (_bv)))

/* Hint that is passed to `scm_gc_malloc ()' and friends.  */
#define SCM_GC_BYTEVECTOR "bytevector"

SCM_API void scm_init_bytevectors (void);

SCM_INTERNAL scm_t_bits scm_tc16_bytevector;
SCM_INTERNAL SCM scm_c_take_bytevector (signed char *, unsigned);

#define scm_c_shrink_bytevector(_bv, _len)		\
  (SCM_BYTEVECTOR_INLINE_P (_bv)			\
   ? (_bv)						\
   : scm_i_shrink_bytevector ((_bv), (_len)))

SCM_INTERNAL SCM scm_i_shrink_bytevector (SCM, unsigned);
SCM_INTERNAL SCM scm_null_bytevector;

#endif /* SCM_BYTEVECTORS_H */

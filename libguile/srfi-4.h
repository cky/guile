#ifndef SCM_SRFI_4_H
#define SCM_SRFI_4_H
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


#include "libguile/__scm.h"

/* Generic procedures.
 */

SCM_API SCM scm_uniform_vector_p (SCM v);
SCM_API SCM scm_uniform_vector_length (SCM v);
SCM_API SCM scm_uniform_vector_ref (SCM v, SCM idx);
SCM_API SCM scm_uniform_vector_set_x (SCM v, SCM idx, SCM val);
SCM_API SCM scm_uniform_vector_to_list (SCM v);

SCM_API int scm_is_uniform_vector (SCM obj);
SCM_API size_t scm_c_uniform_vector_length (SCM v);
SCM_API size_t scm_c_uniform_vector_size (SCM v);

SCM_API void *scm_uniform_vector_elements (SCM uvec);
SCM_API size_t scm_uniform_vector_element_size (SCM uvec);
SCM_API void scm_uniform_vector_release (SCM uvec);

/* Specific procedures.
 */

SCM_API SCM scm_u8vector_p (SCM obj);
SCM_API SCM scm_make_u8vector (SCM n, SCM fill);
SCM_API SCM scm_take_u8vector (const scm_t_uint8 *data, size_t n);
SCM_API SCM scm_u8vector (SCM l);
SCM_API SCM scm_u8vector_length (SCM uvec);
SCM_API SCM scm_u8vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u8vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u8vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u8vector (SCM l);
SCM_API SCM scm_any_to_u8vector (SCM obj);
SCM_API scm_t_uint8 *scm_u8vector_elements (SCM uvec);

SCM_API SCM scm_s8vector_p (SCM obj);
SCM_API SCM scm_make_s8vector (SCM n, SCM fill);
SCM_API SCM scm_take_s8vector (const scm_t_int8 *data, size_t n);
SCM_API SCM scm_s8vector (SCM l);
SCM_API SCM scm_s8vector_length (SCM uvec);
SCM_API SCM scm_s8vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s8vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s8vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s8vector (SCM l);
SCM_API SCM scm_any_to_s8vector (SCM obj);
SCM_API scm_t_int8 *scm_s8vector_elements (SCM uvec);

SCM_API SCM scm_u16vector_p (SCM obj);
SCM_API SCM scm_make_u16vector (SCM n, SCM fill);
SCM_API SCM scm_take_u16vector (const scm_t_uint16 *data, size_t n);
SCM_API SCM scm_u16vector (SCM l);
SCM_API SCM scm_u16vector_length (SCM uvec);
SCM_API SCM scm_u16vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u16vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u16vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u16vector (SCM l);
SCM_API SCM scm_any_to_u16vector (SCM obj);
SCM_API scm_t_uint16 *scm_u16vector_elements (SCM uvec);

SCM_API SCM scm_s16vector_p (SCM obj);
SCM_API SCM scm_make_s16vector (SCM n, SCM fill);
SCM_API SCM scm_take_s16vector (const scm_t_int16 *data, size_t n);
SCM_API SCM scm_s16vector (SCM l);
SCM_API SCM scm_s16vector_length (SCM uvec);
SCM_API SCM scm_s16vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s16vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s16vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s16vector (SCM l);
SCM_API SCM scm_any_to_s16vector (SCM obj);
SCM_API scm_t_int16 *scm_s16vector_elements (SCM uvec);

SCM_API SCM scm_u32vector_p (SCM obj);
SCM_API SCM scm_make_u32vector (SCM n, SCM fill);
SCM_API SCM scm_take_u32vector (const scm_t_uint32 *data, size_t n);
SCM_API SCM scm_u32vector (SCM l);
SCM_API SCM scm_u32vector_length (SCM uvec);
SCM_API SCM scm_u32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u32vector (SCM l);
SCM_API SCM scm_any_to_u32vector (SCM obj);
SCM_API scm_t_uint32 *scm_u32vector_elements (SCM uvec);

SCM_API SCM scm_s32vector_p (SCM obj);
SCM_API SCM scm_make_s32vector (SCM n, SCM fill);
SCM_API SCM scm_take_s32vector (const scm_t_int32 *data, size_t n);
SCM_API SCM scm_s32vector (SCM l);
SCM_API SCM scm_s32vector_length (SCM uvec);
SCM_API SCM scm_s32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s32vector (SCM l);
SCM_API SCM scm_any_to_s32vector (SCM obj);
SCM_API scm_t_int32 *scm_s32vector_elements (SCM uvec);

SCM_API SCM scm_u64vector_p (SCM obj);
SCM_API SCM scm_make_u64vector (SCM n, SCM fill);
SCM_API SCM scm_take_u64vector (const scm_t_uint64 *data, size_t n);
SCM_API SCM scm_u64vector (SCM l);
SCM_API SCM scm_u64vector_length (SCM uvec);
SCM_API SCM scm_u64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u64vector (SCM l);
SCM_API SCM scm_any_to_u64vector (SCM obj);
SCM_API scm_t_uint64 *scm_u64vector_elements (SCM uvec);

SCM_API SCM scm_s64vector_p (SCM obj);
SCM_API SCM scm_make_s64vector (SCM n, SCM fill);
SCM_API SCM scm_take_s64vector (const scm_t_int64 *data, size_t n);
SCM_API SCM scm_s64vector (SCM l);
SCM_API SCM scm_s64vector_length (SCM uvec);
SCM_API SCM scm_s64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s64vector (SCM l);
SCM_API SCM scm_any_to_s64vector (SCM obj);
SCM_API scm_t_int64 *scm_s64vector_elements (SCM uvec);

SCM_API SCM scm_f32vector_p (SCM obj);
SCM_API SCM scm_make_f32vector (SCM n, SCM fill);
SCM_API SCM scm_take_f32vector (const float *data, size_t n);
SCM_API SCM scm_f32vector (SCM l);
SCM_API SCM scm_f32vector_length (SCM uvec);
SCM_API SCM scm_f32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_f32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_f32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_f32vector (SCM l);
SCM_API SCM scm_any_to_f32vector (SCM obj);
SCM_API float *scm_f32vector_elements (SCM uvec);

SCM_API SCM scm_f64vector_p (SCM obj);
SCM_API SCM scm_make_f64vector (SCM n, SCM fill);
SCM_API SCM scm_take_f64vector (const double *data, size_t n);
SCM_API SCM scm_f64vector (SCM l);
SCM_API SCM scm_f64vector_length (SCM uvec);
SCM_API SCM scm_f64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_f64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_f64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_f64vector (SCM l);
SCM_API SCM scm_any_to_f64vector (SCM obj);
SCM_API double *scm_f64vector_elements (SCM uvec);

SCM_API SCM scm_c32vector_p (SCM obj);
SCM_API SCM scm_make_c32vector (SCM n, SCM fill);
SCM_API SCM scm_take_c32vector (const float *data, size_t n);
SCM_API SCM scm_c32vector (SCM l);
SCM_API SCM scm_c32vector_length (SCM uvec);
SCM_API SCM scm_c32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_c32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_c32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_c32vector (SCM l);
SCM_API SCM scm_any_to_c32vector (SCM obj);
SCM_API float *scm_c32vector_elements (SCM uvec);

SCM_API SCM scm_c64vector_p (SCM obj);
SCM_API SCM scm_make_c64vector (SCM n, SCM fill);
SCM_API SCM scm_take_c64vector (const double *data, size_t n);
SCM_API SCM scm_c64vector (SCM l);
SCM_API SCM scm_c64vector_length (SCM uvec);
SCM_API SCM scm_c64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_c64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_c64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_c64vector (SCM l);
SCM_API SCM scm_any_to_c64vector (SCM obj);
SCM_API double *scm_c64vector_elements (SCM uvec);

SCM_API SCM scm_i_uniform_vector_creator (SCM uvec);
SCM_API const char *scm_i_uniform_vector_tag (SCM uvec);

SCM_API size_t scm_uniform_element_size (SCM obj);

SCM_API SCM scm_i_proc_make_u8vector;
SCM_API SCM scm_i_proc_make_s8vector;
SCM_API SCM scm_i_proc_make_u16vector;
SCM_API SCM scm_i_proc_make_s16vector;
SCM_API SCM scm_i_proc_make_u32vector;
SCM_API SCM scm_i_proc_make_s32vector;
SCM_API SCM scm_i_proc_make_u64vector;
SCM_API SCM scm_i_proc_make_s64vector;
SCM_API SCM scm_i_proc_make_f32vector;
SCM_API SCM scm_i_proc_make_f64vector;
SCM_API SCM scm_i_proc_make_c32vector;
SCM_API SCM scm_i_proc_make_c64vector;

SCM_API void scm_init_srfi_4 (void);

#endif /* SCM_SRFI_4_H */

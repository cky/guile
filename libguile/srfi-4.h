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

SCM_API SCM scm_u8vector_p (SCM obj);
SCM_API SCM scm_make_u8vector (SCM n, SCM fill);
SCM_API SCM scm_u8vector (SCM l);
SCM_API SCM scm_u8vector_length (SCM uvec);
SCM_API SCM scm_u8vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u8vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u8vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u8vector (SCM l);

SCM_API SCM scm_s8vector_p (SCM obj);
SCM_API SCM scm_make_s8vector (SCM n, SCM fill);
SCM_API SCM scm_s8vector (SCM l);
SCM_API SCM scm_s8vector_length (SCM uvec);
SCM_API SCM scm_s8vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s8vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s8vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s8vector (SCM l);

SCM_API SCM scm_u16vector_p (SCM obj);
SCM_API SCM scm_make_u16vector (SCM n, SCM fill);
SCM_API SCM scm_u16vector (SCM l);
SCM_API SCM scm_u16vector_length (SCM uvec);
SCM_API SCM scm_u16vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u16vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u16vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u16vector (SCM l);

SCM_API SCM scm_s16vector_p (SCM obj);
SCM_API SCM scm_make_s16vector (SCM n, SCM fill);
SCM_API SCM scm_s16vector (SCM l);
SCM_API SCM scm_s16vector_length (SCM uvec);
SCM_API SCM scm_s16vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s16vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s16vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s16vector (SCM l);

SCM_API SCM scm_u32vector_p (SCM obj);
SCM_API SCM scm_make_u32vector (SCM n, SCM fill);
SCM_API SCM scm_u32vector (SCM l);
SCM_API SCM scm_u32vector_length (SCM uvec);
SCM_API SCM scm_u32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u32vector (SCM l);

SCM_API SCM scm_s32vector_p (SCM obj);
SCM_API SCM scm_make_s32vector (SCM n, SCM fill);
SCM_API SCM scm_s32vector (SCM l);
SCM_API SCM scm_s32vector_length (SCM uvec);
SCM_API SCM scm_s32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s32vector (SCM l);

SCM_API SCM scm_u64vector_p (SCM obj);
SCM_API SCM scm_make_u64vector (SCM n, SCM fill);
SCM_API SCM scm_u64vector (SCM l);
SCM_API SCM scm_u64vector_length (SCM uvec);
SCM_API SCM scm_u64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u64vector (SCM l);

SCM_API SCM scm_s64vector_p (SCM obj);
SCM_API SCM scm_make_s64vector (SCM n, SCM fill);
SCM_API SCM scm_s64vector (SCM l);
SCM_API SCM scm_s64vector_length (SCM uvec);
SCM_API SCM scm_s64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s64vector (SCM l);

SCM_API SCM scm_f32vector_p (SCM obj);
SCM_API SCM scm_make_f32vector (SCM n, SCM fill);
SCM_API SCM scm_f32vector (SCM l);
SCM_API SCM scm_f32vector_length (SCM uvec);
SCM_API SCM scm_f32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_f32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_f32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_f32vector (SCM l);

SCM_API SCM scm_f64vector_p (SCM obj);
SCM_API SCM scm_make_f64vector (SCM n, SCM fill);
SCM_API SCM scm_f64vector (SCM l);
SCM_API SCM scm_f64vector_length (SCM uvec);
SCM_API SCM scm_f64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_f64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_f64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_f64vector (SCM l);

SCM_API SCM scm_i_read_homogenous_vector (SCM port, char pfx);

SCM_API void scm_init_srfi_4 (void);

#endif /* SCM_SRFI_4_H */

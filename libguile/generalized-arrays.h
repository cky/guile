/* classes: h_files */

#ifndef SCM_GENERALIZED_ARRAYS_H
#define SCM_GENERALIZED_ARRAYS_H

/* Copyright (C) 1995,1996,1997,1999,2000,2001, 2004, 2006, 2008, 2009, 2013 Free Software Foundation, Inc.
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
#include "libguile/array-handle.h"



/* These functions operate on all kinds of arrays that Guile knows about.
 */


/** Arrays */

SCM_API int scm_is_array (SCM obj);
SCM_API SCM scm_array_p (SCM v, SCM unused);
SCM_INTERNAL SCM scm_array_p_2 (SCM);

SCM_API int scm_is_typed_array (SCM obj, SCM type);
SCM_API SCM scm_typed_array_p (SCM v, SCM type);

SCM_API size_t scm_c_array_rank (SCM ra);
SCM_API SCM scm_array_rank (SCM ra);

SCM_API size_t scm_c_array_length (SCM ra);
SCM_API SCM scm_array_length (SCM ra);

SCM_API SCM scm_array_dimensions (SCM ra);
SCM_API SCM scm_array_type (SCM ra);
SCM_API SCM scm_array_in_bounds_p (SCM v, SCM args);

SCM_API SCM scm_c_array_ref_1 (SCM v, ssize_t idx0);
SCM_API SCM scm_c_array_ref_2 (SCM v, ssize_t idx0, ssize_t idx1);

SCM_API void scm_c_array_set_1_x (SCM v, SCM obj, ssize_t idx0);
SCM_API void scm_c_array_set_2_x (SCM v, SCM obj, ssize_t idx0, ssize_t idx1);

SCM_API SCM scm_array_ref (SCM v, SCM args);
SCM_API SCM scm_array_set_x (SCM v, SCM obj, SCM args);
SCM_API SCM scm_array_to_list (SCM v);

SCM_INTERNAL void scm_init_generalized_arrays (void);


#endif  /* SCM_GENERALIZED_ARRAYS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

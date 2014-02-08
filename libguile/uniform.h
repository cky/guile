/* classes: h_files */

#ifndef SCM_UNIFORM_H
#define SCM_UNIFORM_H

/* Copyright (C) 1995,1996,1997,1999,2000,2001, 2004, 2006, 2008, 2009,
 * 2014 Free Software Foundation, Inc.
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
#include "libguile/generalized-vectors.h"



/*
 * Uniform vectors contain unboxed values. They are not necessarily contiguous.
 */

SCM_INTERNAL const size_t scm_i_array_element_type_sizes[];
#define SCM_ARRAY_ELEMENT_TYPE_IS_UNBOXED(t)    \
  (scm_i_array_element_type_sizes[(t)] != 0)

/* type size in bytes */
SCM_API size_t scm_array_handle_uniform_element_size (scm_t_array_handle *h);
/* type size in bits */
SCM_API size_t scm_array_handle_uniform_element_bit_size (scm_t_array_handle *h);

SCM_API const void *scm_array_handle_uniform_elements (scm_t_array_handle *h);
SCM_API void *scm_array_handle_uniform_writable_elements (scm_t_array_handle *h);

#if SCM_ENABLE_DEPRECATED

SCM_DEPRECATED SCM scm_uniform_vector_p (SCM v);
SCM_DEPRECATED SCM scm_uniform_vector_length (SCM v);
SCM_DEPRECATED SCM scm_uniform_vector_element_type (SCM v);
SCM_DEPRECATED SCM scm_uniform_vector_element_size (SCM v);
SCM_DEPRECATED SCM scm_uniform_vector_ref (SCM v, SCM idx);
SCM_DEPRECATED SCM scm_uniform_vector_set_x (SCM v, SCM idx, SCM val);
SCM_DEPRECATED SCM scm_uniform_vector_to_list (SCM v);
SCM_DEPRECATED SCM scm_uniform_vector_read_x (SCM v, SCM port_or_fd,
                                              SCM start, SCM end);
SCM_DEPRECATED SCM scm_uniform_vector_write (SCM v, SCM port_or_fd,
                                             SCM start, SCM end);

SCM_DEPRECATED int scm_is_uniform_vector (SCM obj);
SCM_DEPRECATED size_t scm_c_uniform_vector_length (SCM v);
SCM_DEPRECATED SCM scm_c_uniform_vector_ref (SCM v, size_t idx);
SCM_DEPRECATED void scm_c_uniform_vector_set_x (SCM v, size_t idx, SCM val);
SCM_DEPRECATED const void *scm_uniform_vector_elements (SCM uvec, 
                                                        scm_t_array_handle *h,
                                                        size_t *lenp,
                                                        ssize_t *incp);
SCM_DEPRECATED void *scm_uniform_vector_writable_elements (SCM uvec, 
                                                           scm_t_array_handle *h,
                                                           size_t *lenp,
                                                           ssize_t *incp);

#endif

SCM_INTERNAL void scm_init_uniform (void);

#endif  /* SCM_UNIFORM_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

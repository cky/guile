/* classes: h_files */

#ifndef SCM_GENERALIZED_VECTORS_H
#define SCM_GENERALIZED_VECTORS_H

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



/* Generalized vectors */

SCM_API int scm_is_generalized_vector (SCM obj);
SCM_API size_t scm_c_generalized_vector_length (SCM v);
SCM_API SCM scm_c_generalized_vector_ref (SCM v, size_t idx);
SCM_API void scm_c_generalized_vector_set_x (SCM v, size_t idx, SCM val);
SCM_API void scm_generalized_vector_get_handle (SCM vec,
						scm_t_array_handle *h);

SCM_API SCM scm_make_generalized_vector (SCM type, SCM len, SCM fill);
SCM_INTERNAL void scm_i_register_vector_constructor (SCM type, SCM (*ctor)(SCM, SCM));

#define SCM_VECTOR_IMPLEMENTATION(type, ctor)                   \
  SCM_SNARF_INIT (scm_i_register_vector_constructor             \
                  (scm_i_array_element_types[type], ctor))

SCM_INTERNAL void scm_init_generalized_vectors (void);

#endif  /* SCM_GENERALIZED_VECTORS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

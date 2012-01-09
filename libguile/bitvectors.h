/* classes: h_files */

#ifndef SCM_BITVECTORS_H
#define SCM_BITVECTORS_H

/* Copyright (C) 1995,1996,1997,1999,2000,2001, 2004, 2006, 2008, 2009 Free Software Foundation, Inc.
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



/* Bitvectors. Exciting stuff, maybe!
 */


/** Bit vectors */

SCM_API SCM scm_bitvector_p (SCM vec);
SCM_API SCM scm_bitvector (SCM bits);
SCM_API SCM scm_make_bitvector (SCM len, SCM fill);
SCM_API SCM scm_bitvector_length (SCM vec);
SCM_API SCM scm_bitvector_ref (SCM vec, SCM idx);
SCM_API SCM scm_bitvector_set_x (SCM vec, SCM idx, SCM val);
SCM_API SCM scm_list_to_bitvector (SCM list);
SCM_API SCM scm_bitvector_to_list (SCM vec);
SCM_API SCM scm_bitvector_fill_x (SCM vec, SCM val);

SCM_API SCM scm_bit_count (SCM item, SCM seq);
SCM_API SCM scm_bit_position (SCM item, SCM v, SCM k);
SCM_API SCM scm_bit_set_star_x (SCM v, SCM kv, SCM obj);
SCM_API SCM scm_bit_count_star (SCM v, SCM kv, SCM obj);
SCM_API SCM scm_bit_invert_x (SCM v);
SCM_API SCM scm_istr2bve (SCM str);

SCM_API int scm_is_bitvector (SCM obj);
SCM_API SCM scm_c_make_bitvector (size_t len, SCM fill);
SCM_API size_t scm_c_bitvector_length (SCM vec);
SCM_API SCM scm_c_bitvector_ref (SCM vec, size_t idx);
SCM_API void scm_c_bitvector_set_x (SCM vec, size_t idx, SCM val);
SCM_API const scm_t_uint32 *scm_array_handle_bit_elements (scm_t_array_handle *h);
SCM_API scm_t_uint32 *scm_array_handle_bit_writable_elements (scm_t_array_handle *h);
SCM_API size_t scm_array_handle_bit_elements_offset (scm_t_array_handle *h);
SCM_API const scm_t_uint32 *scm_bitvector_elements (SCM vec,
						    scm_t_array_handle *h,
						    size_t *offp,
						    size_t *lenp,
						    ssize_t *incp);
SCM_API scm_t_uint32 *scm_bitvector_writable_elements (SCM vec, 
						       scm_t_array_handle *h,
						       size_t *offp,
						       size_t *lenp,
						       ssize_t *incp);

SCM_INTERNAL int scm_i_print_bitvector (SCM vec, SCM port, scm_print_state *pstate);
SCM_INTERNAL SCM scm_i_bitvector_equal_p (SCM vec1, SCM vec2);
SCM_INTERNAL void scm_init_bitvectors (void);

#endif  /* SCM_BITVECTORS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

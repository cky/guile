/* classes: h_files */

#ifndef SCM_ARRAY_HANDLE_H
#define SCM_ARRAY_HANDLE_H

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



typedef struct scm_t_array_dim
{
  ssize_t lbnd;
  ssize_t ubnd;
  ssize_t inc;
} scm_t_array_dim;

struct scm_t_array_handle;

typedef SCM (*scm_i_t_array_ref) (struct scm_t_array_handle *, ssize_t);
typedef void (*scm_i_t_array_set) (struct scm_t_array_handle *, ssize_t, SCM);

typedef struct scm_t_array_handle {
  SCM array;
  size_t base;
  scm_t_array_dim *dims;
  scm_t_array_dim dim0;
  scm_i_t_array_ref ref;
  scm_i_t_array_set set;
  const void *elements;
  void *writable_elements;
} scm_t_array_handle;

SCM_API void scm_array_get_handle (SCM array, scm_t_array_handle *h);
SCM_API size_t scm_array_handle_rank (scm_t_array_handle *h);
SCM_API scm_t_array_dim *scm_array_handle_dims (scm_t_array_handle *h);
SCM_API ssize_t scm_array_handle_pos (scm_t_array_handle *h, SCM indices);
SCM_API const SCM *scm_array_handle_elements (scm_t_array_handle *h);
SCM_API SCM *scm_array_handle_writable_elements (scm_t_array_handle *h);
SCM_API void scm_array_handle_release (scm_t_array_handle *h);

/* See inline.h for scm_array_handle_ref and scm_array_handle_set */

SCM_INTERNAL void scm_init_array_handle (void);


#endif  /* SCM_ARRAY_HANDLE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

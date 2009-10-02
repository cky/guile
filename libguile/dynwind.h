/* classes: h_files */

#ifndef SCM_DYNWIND_H
#define SCM_DYNWIND_H

/* Copyright (C) 1995,1996,1998,1999,2000,2003,2004, 2006, 2008 Free Software Foundation, Inc.
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



typedef void (*scm_t_guard) (void *);
typedef SCM (*scm_t_inner) (void *);

SCM_API SCM scm_dynamic_wind (SCM thunk1, SCM thunk2, SCM thunk3);
SCM_API SCM scm_internal_dynamic_wind (scm_t_guard before,
				       scm_t_inner inner,
				       scm_t_guard after,
				       void *inner_data,
				       void *guard_data);
SCM_API void scm_dowinds (SCM to, long delta);
SCM_INTERNAL void scm_i_dowinds (SCM to, long delta,
				 void (*turn_func) (void *), void *data);
SCM_INTERNAL void scm_init_dynwind (void);

SCM_API void scm_swap_bindings (SCM vars, SCM vals);

/* Flags for scm_dynwind_begin. */
#define SCM_F_DYNWIND_REWINDABLE 1

/* Flags for scm_dynwind_unwind_handler(_with_scm) and
   scm_dynwind_rewind_handler(_with_scm). */
#define SCM_F_WIND_EXPLICITLY 1

SCM_API void scm_dynwind_begin (int);
SCM_API void scm_dynwind_end (void);

SCM_API void scm_dynwind_unwind_handler (void (*) (void *), void *, int);
SCM_API void scm_dynwind_rewind_handler (void (*) (void *), void *, int);

SCM_API void scm_dynwind_unwind_handler_with_scm (void (*) (SCM), SCM, int);
SCM_API void scm_dynwind_rewind_handler_with_scm (void (*) (SCM), SCM, int);

SCM_API void scm_dynwind_free (void *mem);

#ifdef GUILE_DEBUG
SCM_API SCM scm_wind_chain (void);
#endif /*GUILE_DEBUG*/

#endif  /* SCM_DYNWIND_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

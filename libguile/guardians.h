/* classes: h_files */

#ifndef SCM_GUARDIANS_H
#define SCM_GUARDIANS_H

/* Copyright (C) 1998,2000,2001 Free Software Foundation, Inc.
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

SCM_API SCM scm_make_guardian (SCM greedy_p);
SCM_API SCM scm_destroy_guardian_x (SCM guardian);

SCM_API SCM scm_guardian_greedy_p (SCM guardian);
SCM_API SCM scm_guardian_destroyed_p (SCM guardian);

/* these are to be called from C: */
SCM_API SCM scm_guard (SCM guardian, SCM obj, int throw_p);
SCM_API SCM scm_get_one_zombie (SCM guardian);

SCM_API void scm_init_guardians (void);

#endif  /* SCM_GUARDIANS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

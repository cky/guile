/* classes: h_files */

#ifndef SCM_DEPRECATION_H
#define SCM_DEPRECATION_H

/* Copyright (C) 2001 Free Software Foundation, Inc.
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



#if (SCM_ENABLE_DEPRECATED == 1)

/* These functions are _not_ deprecated, but we exclude them along
   with the really deprecated features to be sure that no-one is
   trying to emit deprecation warnings when libguile is supposed to be
   clean of them.
*/

SCM_API void scm_c_issue_deprecation_warning (const char *msg);
SCM_API void scm_c_issue_deprecation_warning_fmt (const char *msg, ...);
SCM_API SCM scm_issue_deprecation_warning (SCM msgs);

#endif

SCM_API SCM scm_include_deprecated_features (void);
SCM_API void scm_init_deprecation (void);

#endif  /* SCM_DEPRECATION_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

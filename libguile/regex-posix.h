/* classes: h_files */

#ifndef SCM_REGEX_POSIX_H
#define SCM_REGEX_POSIX_H

/* Copyright (C) 1997,1998,2000,2001 Free Software Foundation, Inc.
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

SCM_API scm_t_bits scm_tc16_regex;
#define SCM_RGX(X)	((regex_t *) SCM_CELL_WORD_1 (X))
#define SCM_RGXP(X)	(!SCM_IMP (X) && (SCM_CELL_TYPE (X) == scm_tc16_regex))

SCM_API SCM scm_make_regexp (SCM pat, SCM flags);
SCM_API SCM scm_regexp_p (SCM x);
SCM_API SCM scm_regexp_exec (SCM rx, SCM str, SCM start, SCM flags);
SCM_API void scm_init_regex_posix (void);

#endif  /* SCM_REGEX_POSIX_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

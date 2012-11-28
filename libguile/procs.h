/* classes: h_files */

#ifndef SCM_PROCS_H
#define SCM_PROCS_H

/* Copyright (C) 1995, 1996, 1998, 1999, 2000, 2001, 2006, 2008, 2009,
 *   2012 Free Software Foundation, Inc.
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



SCM_API SCM scm_procedure_p (SCM obj);
SCM_API SCM scm_thunk_p (SCM obj);
SCM_API SCM scm_procedure_documentation (SCM proc);
SCM_API SCM scm_procedure_with_setter_p (SCM obj);
SCM_API SCM scm_make_procedure_with_setter (SCM procedure, SCM setter);
SCM_API SCM scm_procedure (SCM proc);
SCM_API SCM scm_setter (SCM proc);
SCM_INTERNAL void scm_init_procs (void);

SCM_INTERNAL SCM scm_sym_documentation;

#endif  /* SCM_PROCS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

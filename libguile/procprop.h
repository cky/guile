/* classes: h_files */

#ifndef SCM_PROCPROP_H
#define SCM_PROCPROP_H

/* Copyright (C) 1995,1996,1998,2000, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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



SCM_API SCM scm_sym_name;
#if (SCM_ENABLE_DEPRECATED == 1)
SCM_DEPRECATED SCM scm_sym_arity;
#endif
SCM_API SCM scm_sym_system_procedure;



SCM_INTERNAL int scm_i_procedure_arity (SCM proc, int *req, int *opt, int *rest);
SCM_API SCM scm_set_procedure_minimum_arity_x (SCM proc, SCM req, SCM opt,
                                               SCM rest);
SCM_API SCM scm_procedure_minimum_arity (SCM proc);
SCM_API SCM scm_procedure_properties (SCM proc);
SCM_API SCM scm_set_procedure_properties_x (SCM proc, SCM alist);
SCM_API SCM scm_procedure_property (SCM proc, SCM key);
SCM_API SCM scm_set_procedure_property_x (SCM proc, SCM key, SCM val);
SCM_INTERNAL void scm_init_procprop (void);

#endif  /* SCM_PROCPROP_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

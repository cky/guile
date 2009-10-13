/* Copyright (C) 2001, 2009 Free Software Foundation, Inc.
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

#ifndef _SCM_PROGRAMS_H_
#define _SCM_PROGRAMS_H_

#include <libguile.h>
#include <libguile/objcodes.h>

/*
 * Programs
 */

#define SCM_F_PROGRAM_IS_BOOT 0x100

#define SCM_PROGRAM_P(x)	(!SCM_IMP (x) && SCM_TYP7(x) == scm_tc7_program)
#define SCM_PROGRAM_OBJCODE(x)	(SCM_CELL_OBJECT_1 (x))
#define SCM_PROGRAM_OBJTABLE(x)	(SCM_CELL_OBJECT_2 (x))
#define SCM_PROGRAM_FREE_VARIABLES(x) (SCM_CELL_OBJECT_3 (x))
#define SCM_PROGRAM_DATA(x)	(SCM_OBJCODE_DATA (SCM_PROGRAM_OBJCODE (x)))
#define SCM_VALIDATE_PROGRAM(p,x) SCM_MAKE_VALIDATE (p, x, PROGRAM_P)
#define SCM_PROGRAM_IS_BOOT(x)	(SCM_CELL_WORD_0 (x) & SCM_F_PROGRAM_IS_BOOT)

SCM_API SCM scm_make_program (SCM objcode, SCM objtable, SCM free_variables);

SCM_API SCM scm_program_p (SCM obj);
SCM_API SCM scm_program_base (SCM program);
SCM_API SCM scm_program_meta (SCM program);
SCM_API SCM scm_program_bindings (SCM program);
SCM_API SCM scm_program_sources (SCM program);
SCM_API SCM scm_program_source (SCM program, SCM ip);
SCM_API SCM scm_program_arities (SCM program);
SCM_API SCM scm_program_properties (SCM program);
SCM_API SCM scm_program_name (SCM program);
SCM_API SCM scm_program_objects (SCM program);
SCM_API SCM scm_program_module (SCM program);
SCM_API SCM scm_program_free_variables (SCM program);
SCM_API SCM scm_program_objcode (SCM program);

SCM_API SCM scm_c_program_source (SCM program, size_t ip);

SCM_INTERNAL int scm_i_program_arity (SCM program, int *req, int *opt, int *rest);
SCM_INTERNAL void scm_i_program_print (SCM program, SCM port,
                                       scm_print_state *pstate);
SCM_INTERNAL void scm_bootstrap_programs (void);
SCM_INTERNAL void scm_init_programs (void);

#endif /* _SCM_PROGRAMS_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

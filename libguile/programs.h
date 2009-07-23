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

typedef unsigned char scm_byte_t;

SCM_API scm_t_bits scm_tc16_program;

#define SCM_F_PROGRAM_IS_BOOT (1<<0)

#define SCM_PROGRAM_P(x)	(SCM_SMOB_PREDICATE (scm_tc16_program, x))
#define SCM_PROGRAM_OBJCODE(x)	(SCM_SMOB_OBJECT (x))
#define SCM_PROGRAM_OBJTABLE(x)	(SCM_SMOB_OBJECT_2 (x))
#define SCM_PROGRAM_FREE_VARS(x) (SCM_SMOB_OBJECT_3 (x))
#define SCM_PROGRAM_DATA(x)	(SCM_OBJCODE_DATA (SCM_PROGRAM_OBJCODE (x)))
#define SCM_VALIDATE_PROGRAM(p,x) SCM_MAKE_VALIDATE (p, x, PROGRAM_P)
#define SCM_PROGRAM_IS_BOOT(x)	(SCM_SMOB_FLAGS (x) & SCM_F_PROGRAM_IS_BOOT)

SCM_API SCM scm_make_program (SCM objcode, SCM objtable, SCM free_vars);

SCM_API SCM scm_program_p (SCM obj);
SCM_API SCM scm_program_base (SCM program);
SCM_API SCM scm_program_arity (SCM program);
SCM_API SCM scm_program_meta (SCM program);
SCM_API SCM scm_program_bindings (SCM program);
SCM_API SCM scm_program_sources (SCM program);
SCM_API SCM scm_program_source (SCM program, SCM ip);
SCM_API SCM scm_program_properties (SCM program);
SCM_API SCM scm_program_name (SCM program);
SCM_API SCM scm_program_objects (SCM program);
SCM_API SCM scm_program_module (SCM program);
SCM_API SCM scm_program_free_vars (SCM program);
SCM_API SCM scm_program_objcode (SCM program);

SCM_API SCM scm_c_program_source (SCM program, size_t ip);

SCM_INTERNAL void scm_bootstrap_programs (void);
SCM_INTERNAL void scm_init_programs (void);

#endif /* _SCM_PROGRAMS_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

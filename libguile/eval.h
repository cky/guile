/* classes: h_files */

#ifndef SCM_EVAL_H
#define SCM_EVAL_H

/* Copyright (C) 1995,1996,1998,1999,2000,2001,2002,2003,2004,2008,2009,2010,2011,2012
 * Free Software Foundation, Inc.
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

#include "libguile/struct.h"
#include "libguile/memoize.h"



/* {Options}
 */




/* {Ilocs}
 *
 * Ilocs are relative pointers into local environment structures.
 * 
 */
#define SCM_ILOCP(n)		(SCM_ITAG8(n)==scm_tc8_iloc)



/* {Evaluator}
 */

typedef SCM (*scm_t_trampoline_0) (SCM proc);
typedef SCM (*scm_t_trampoline_1) (SCM proc, SCM arg1);
typedef SCM (*scm_t_trampoline_2) (SCM proc, SCM arg1, SCM arg2);



#define SCM_EXTEND_ENV scm_acons



SCM_API SCM scm_call_0 (SCM proc);
SCM_API SCM scm_call_1 (SCM proc, SCM arg1);
SCM_API SCM scm_call_2 (SCM proc, SCM arg1, SCM arg2);
SCM_API SCM scm_call_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3);
SCM_API SCM scm_call_4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4);
SCM_API SCM scm_call_5 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4,
                        SCM arg5);
SCM_API SCM scm_call_6 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4,
                        SCM arg5, SCM arg6);
SCM_API SCM scm_call_7 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4,
                        SCM arg5, SCM arg6, SCM arg7);
SCM_API SCM scm_call_8 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4,
                        SCM arg5, SCM arg6, SCM arg7, SCM arg8);
SCM_API SCM scm_call_9 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4,
                        SCM arg5, SCM arg6, SCM arg7, SCM arg8, SCM arg9);
SCM_API SCM scm_call_n (SCM proc, SCM *argv, size_t nargs);
SCM_API SCM scm_call   (SCM proc, ...);
SCM_API SCM scm_apply_0 (SCM proc, SCM args);
SCM_API SCM scm_apply_1 (SCM proc, SCM arg1, SCM args);
SCM_API SCM scm_apply_2 (SCM proc, SCM arg1, SCM arg2, SCM args);
SCM_API SCM scm_apply_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM args);
SCM_API SCM scm_nconc2last (SCM lst);
SCM_API SCM scm_apply (SCM proc, SCM arg1, SCM args);
#define scm_dapply(proc,arg1,args) scm_apply (proc, arg1, args)
SCM_API SCM scm_map (SCM proc, SCM arg1, SCM args);
SCM_API SCM scm_for_each (SCM proc, SCM arg1, SCM args);
SCM_API SCM scm_primitive_eval (SCM exp);
#define scm_primitive_eval_x(exp) scm_primitive_eval (exp)
SCM_API SCM scm_eval (SCM exp, SCM module);
#define scm_eval_x(exp, module) scm_eval (exp, module)

SCM_INTERNAL void scm_init_eval (void);


#endif  /* SCM_EVAL_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

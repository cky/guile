/* classes: h_files */

#ifndef EVALH
#define EVALH
/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */


#include "libguile/__scm.h"



/* {Ilocs}
 *
 * Ilocs are relative pointers into local environment structures.
 * 
 */
#define SCM_ILOCP(n)		(SCM_ITAG8(n)==scm_tc8_iloc)
#define SCM_ILOC00		SCM_MAKE_ITAG8(0L, scm_tc8_iloc)
#define SCM_IDINC		(0x00100000L)
#define SCM_ICDR		(0x00080000L)
#define SCM_IFRINC		(0x00000100L)
#define SCM_IDSTMSK		(-SCM_IDINC)
#define SCM_IFRAME(n) 		((int)((SCM_ICDR-SCM_IFRINC)>>8) & ((int)(n)>>8))
#define SCM_IDIST(n) 		(((unsigned long)(n))>>20)
#define SCM_ICDRP(n) 		(SCM_ICDR & (n))




/* {Evaluator}
 *
 * For an explanation of symbols containing "EVAL", see beginning of eval.c.
 */
#ifdef DEBUG_EXTENSIONS
#define XEVAL(x, env) (SCM_IMP(x) \
		      ? (x) \
		      : (*scm_ceval_ptr) ((x), (env)))
#else
#define XEVAL(x, env) (SCM_IMP(x)?(x):scm_ceval((x), (env)))
#endif /* DEBUG_EXTENSIONS */

#define SCM_CEVAL scm_ceval
#define SIDEVAL(x, env) if SCM_NIMP(x) SCM_CEVAL((x), (env))



#define SCM_EXTEND_ENV scm_acons


extern SCM scm_i_dot;
extern SCM scm_i_quote;
extern SCM scm_i_quasiquote;
extern SCM scm_i_lambda;
extern SCM scm_i_let;
extern SCM scm_i_arrow;
extern SCM scm_i_else;
extern SCM scm_i_unquote;
extern SCM scm_i_uq_splicing;
extern SCM scm_i_apply;
extern SCM scm_i_name;


/* A resolved global variable reference in the CAR position
 * of a list is stored (in code only) as a pointer to a pair with a 
 * tag of 1.  This is called a "gloc".
 */

#define SCM_GLOC_SYM(x) (SCM_CAR((x)-1L))
#define SCM_GLOC_VAL(x) (SCM_CDR((x)-1L))


#ifdef __STDC__
extern SCM * scm_ilookup (SCM iloc, SCM env);
extern SCM * scm_lookupcar (SCM vloc, SCM genv);
extern SCM scm_unmemocar (SCM form, SCM env);
extern SCM scm_unmemocopy (SCM form, SCM env);
extern SCM scm_eval_car (SCM pair, SCM env);
extern SCM scm_eval_args (SCM i, SCM env);
extern SCM scm_deval_args (SCM l, SCM env, SCM *lloc);
extern SCM scm_m_quote (SCM xorig, SCM env);
extern SCM scm_m_begin (SCM xorig, SCM env);
extern SCM scm_m_if (SCM xorig, SCM env);
extern SCM scm_m_set (SCM xorig, SCM env);
extern SCM scm_m_vref (SCM xorig, SCM env);
extern SCM scm_m_vset (SCM xorig, SCM env);
extern SCM scm_m_and (SCM xorig, SCM env);
extern SCM scm_m_or (SCM xorig, SCM env);
extern SCM scm_m_case (SCM xorig, SCM env);
extern SCM scm_m_cond (SCM xorig, SCM env);
extern SCM scm_m_lambda (SCM xorig, SCM env);
extern SCM scm_m_letstar (SCM xorig, SCM env);
extern SCM scm_m_do (SCM xorig, SCM env);
extern SCM scm_m_quasiquote (SCM xorig, SCM env);
extern SCM scm_m_delay (SCM xorig, SCM env);
extern SCM scm_m_define (SCM x, SCM env);
extern SCM scm_m_letrec (SCM xorig, SCM env);
extern SCM scm_m_let (SCM xorig, SCM env);
extern SCM scm_m_apply (SCM xorig, SCM env);
extern SCM scm_m_cont (SCM xorig, SCM env);
extern SCM scm_m_undefine (SCM x, SCM env);
extern int scm_badargsp (SCM formals, SCM args);
extern SCM scm_ceval (SCM x, SCM env);
extern SCM scm_deval (SCM x, SCM env);
extern SCM scm_procedure_documentation (SCM proc);
extern SCM scm_nconc2last (SCM lst);
extern SCM scm_apply (SCM proc, SCM arg1, SCM args);
extern SCM scm_dapply (SCM proc, SCM arg1, SCM args);
extern SCM SCM_APPLY (SCM proc, SCM arg1, SCM args);
extern SCM scm_map (SCM proc, SCM arg1, SCM args);
extern SCM scm_for_each (SCM proc, SCM arg1, SCM args);
extern SCM scm_closure (SCM code, SCM env);
extern SCM scm_makprom (SCM code);
extern SCM scm_makacro (SCM code);
extern SCM scm_makmacro (SCM code);
extern SCM scm_makmmacro (SCM code);
extern SCM scm_force (SCM x);
extern SCM scm_promise_p (SCM x);
extern SCM scm_copy_tree (SCM obj);
extern SCM scm_eval_3 (SCM obj, int copyp, SCM env);
extern SCM scm_top_level_env (SCM thunk);
extern SCM scm_eval2 (SCM obj, SCM env_thunk);
extern SCM scm_eval (SCM obj);
extern SCM scm_eval_x (SCM obj);
extern SCM scm_macro_eval_x (SCM exp, SCM env);
extern SCM scm_definedp (SCM x, SCM env);
extern SCM scm_make_synt (char *name, SCM (*macroizer) (), SCM (*fcn) ());
extern void scm_init_eval (void);

#else /* STDC */
extern SCM * scm_ilookup ();
extern SCM * scm_lookupcar ();
extern SCM scm_unmemocar ();
extern SCM scm_unmemocopy ();
extern SCM scm_eval_car ();
extern SCM scm_eval_args ();
extern SCM scm_deval_args ();
extern SCM scm_m_quote ();
extern SCM scm_m_begin ();
extern SCM scm_m_if ();
extern SCM scm_m_set ();
extern SCM scm_m_vref ();
extern SCM scm_m_vset ();
extern SCM scm_m_and ();
extern SCM scm_m_or ();
extern SCM scm_m_case ();
extern SCM scm_m_cond ();
extern SCM scm_m_lambda ();
extern SCM scm_m_letstar ();
extern SCM scm_m_do ();
extern SCM scm_m_quasiquote ();
extern SCM scm_m_delay ();
extern SCM scm_m_define ();
extern SCM scm_m_letrec ();
extern SCM scm_m_let ();
extern SCM scm_m_apply ();
extern SCM scm_m_cont ();
extern SCM scm_m_undefine ();
extern int scm_badargsp ();
extern SCM scm_ceval ();
extern SCM scm_deval ();
extern SCM scm_procedure_documentation ();
extern SCM scm_nconc2last ();
extern SCM scm_apply ();
extern SCM scm_dapply ();
extern SCM SCM_APPLY ();
extern SCM scm_map ();
extern SCM scm_for_each ();
extern SCM scm_closure ();
extern SCM scm_makprom ();
extern SCM scm_makacro ();
extern SCM scm_makmacro ();
extern SCM scm_makmmacro ();
extern SCM scm_force ();
extern SCM scm_promise_p ();
extern SCM scm_copy_tree ();
extern SCM scm_eval_3 ();
extern SCM scm_top_level_env ();
extern SCM scm_eval2 ();
extern SCM scm_eval ();
extern SCM scm_eval_x ();
extern SCM scm_macro_eval_x ();
extern SCM scm_definedp ();
extern SCM scm_make_synt ();
extern void scm_init_eval ();

#endif /* STDC */

#endif  /* EVALH */

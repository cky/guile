/* Debugging extensions for Guile
 * Copyright (C) 1995, 1996 Mikael Djurfeldt
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
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
 */

#include <stdio.h>
#include "_scm.h"
#include "eval.h"
#include "throw.h"
#include "genio.h"
#include "smob.h"
#include "procprop.h"
#include "srcprop.h"
#include "alist.h"
#include "continuations.h"
#include "strports.h"
#include "read.h"
#include "feature.h"

#include "debug.h"


/* {Run time control of the debugging evaluator}
 */

SCM_PROC (s_debug_options, "debug-options-interface", 0, 1, 0, scm_debug_options);

SCM
scm_debug_options (setting)
     SCM setting;
{
  SCM ans;
  SCM_DEFER_INTS;
  ans = scm_options (setting,
		     scm_debug_opts,
		     SCM_N_DEBUG_OPTIONS,
		     s_debug_options);
#ifndef SCM_RECKLESS
  if (!(1 <= SCM_N_FRAMES && SCM_N_FRAMES <= SCM_MAX_FRAME_SIZE))
    {
      scm_options (ans, scm_debug_opts, SCM_N_DEBUG_OPTIONS, s_debug_options);
      scm_out_of_range (s_debug_options, setting);
    }
#endif
  SCM_RESET_DEBUG_MODE;
  scm_debug_eframe_size = 2 * SCM_N_FRAMES;
  SCM_ALLOW_INTS
  return ans;
}

SCM_PROC (s_evaluator_traps, "evaluator-traps-interface", 0, 1, 0, scm_evaluator_traps);

SCM
scm_evaluator_traps (setting)
     SCM setting;
{
  SCM ans;
  SCM_DEFER_INTS;
  ans = scm_options (setting,
		     scm_evaluator_trap_table,
		     SCM_N_EVALUATOR_TRAPS,
		     s_evaluator_traps);
  SCM_RESET_DEBUG_MODE;
  SCM_ALLOW_INTS
  return ans;
}

SCM_PROC (s_single_step, "single-step", 2, 0, 0, scm_single_step);

SCM
scm_single_step (cont, val)
     SCM cont;
     SCM val;
{
  SCM_DEFER_INTS;
  SCM_ENTER_FRAME_P = SCM_EXIT_FRAME_P = 1;
  SCM_RESET_DEBUG_MODE;
  SCM_ALLOW_INTS;
  scm_throw (cont, val);
  return SCM_BOOL_F; /* never returns */
}


static SCM scm_i_source, scm_i_more;
static SCM scm_i_proc, scm_i_args, scm_i_eval_args;
static SCM scm_i_procname;

/* {Memoized Source}
 */

long scm_tc16_memoized;


static int prinmemoized SCM_P ((SCM obj, SCM port, scm_print_state *pstate));

static int
prinmemoized (obj, port, pstate)
     SCM obj;
     SCM port;
     scm_print_state *pstate;
{
  int writingp = SCM_WRITINGP (pstate);
  scm_gen_puts (scm_regular_string, "#<memoized ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (scm_unmemoize (obj), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_gen_putc ('>', port);
  return 1;
}

static scm_smobfuns memoizedsmob =
{scm_markcdr, scm_free0, prinmemoized, 0};

SCM_PROC (s_memoized_p, "memoized?", 1, 0, 0, scm_memoized_p);

SCM
scm_memoized_p (obj)
     SCM obj;
{
  return SCM_NIMP (obj) && SCM_MEMOIZEDP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM
scm_make_memoized (exp, env)
     SCM exp;
     SCM env;
{
  register SCM z, ans;
  SCM_DEFER_INTS;
  SCM_NEWCELL (z);
  SCM_CAR (z) = exp;
  SCM_CDR (z) = env;
  SCM_NEWCELL (ans);
  SCM_CAR (ans) = scm_tc16_memoized;
  SCM_CDR (ans) = z;
  SCM_ALLOW_INTS;
  return ans;
}

SCM_PROC (s_unmemoize, "unmemoize", 1, 0, 0, scm_unmemoize);

SCM
scm_unmemoize (m)
     SCM m;
{
  SCM_ASSERT (SCM_MEMOIZEDP (m), m, SCM_ARG1, s_unmemoize);
  return scm_unmemocopy (SCM_MEMOIZED_EXP (m), SCM_MEMOIZED_ENV (m));
}

SCM_PROC (s_memoized_environment, "memoized-environment", 1, 0, 0, scm_memoized_environment);

SCM
scm_memoized_environment (m)
     SCM m;
{
  SCM_ASSERT (SCM_MEMOIZEDP (m), m, SCM_ARG1, s_unmemoize);
  return SCM_MEMOIZED_ENV (m);
}

SCM_PROC (s_procedure_name, "procedure-name", 1, 0, 0, scm_procedure_name);

SCM
scm_procedure_name (proc)
     SCM proc;
{
  SCM_ASSERT(scm_procedure_p (proc) == SCM_BOOL_T,
	     proc,
	     SCM_ARG1,
	     s_procedure_name);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_closures:
  case scm_tc7_cclo:
    {
      SCM name = scm_procedure_property (proc, scm_i_name);
#if 0
      /* Procedure property scm_i_procname not implemented yet... */
      SCM name = scm_source_property (SCM_CAR (SCM_CDR (SCM_CODE (proc))), scm_i_procname);
      if (SCM_FALSEP (name))
	name = scm_procedure_property (proc, scm_i_name);
#endif
      return name;
    }
  case scm_tcs_subrs:
    return SCM_SNAME (proc);
  default:
    return SCM_BOOL_F;
  }
}

SCM_PROC (s_procedure_source, "procedure-source", 1, 0, 0, scm_procedure_source);

SCM
scm_procedure_source (proc)
     SCM proc;
{
  SCM_ASSERT(SCM_NIMP (proc), proc, SCM_ARG1, s_procedure_source);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_closures:
    {
      SCM src;
      src = scm_source_property (SCM_CDR (SCM_CODE (proc)), scm_i_copy);
      if (src != SCM_BOOL_F)
	return scm_cons2 (scm_i_lambda, SCM_CAR (SCM_CODE (proc)), src);
      src = SCM_CODE (proc);
      return scm_cons (scm_i_lambda,
		       scm_unmemocopy (src,
				       SCM_EXTEND_ENV (SCM_CAR (src),
							   SCM_EOL,
							   SCM_ENV (proc))));
    }
  case scm_tc7_contin:
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
    /* It would indeed be a nice thing if we supplied source even for
       built in procedures! */
    return scm_procedure_property (proc, scm_i_source);
  default:
    scm_wta (proc, (char *) SCM_ARG1, s_procedure_source);
    return 0;
  }
}

SCM_PROC (s_procedure_environment, "procedure-environment", 1, 0, 0, scm_procedure_environment);

SCM
scm_procedure_environment (proc)
     SCM proc;
{
  SCM_ASSERT (SCM_NIMP (proc), proc, SCM_ARG1, s_procedure_environment);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_closures:
    return SCM_ENV (proc);
  case scm_tc7_contin:
  case scm_tcs_subrs:
#ifdef CCLO
  case scm_tc7_cclo:
#endif
    return SCM_EOL;
  default:
    scm_wta (proc, (char *) SCM_ARG1, s_procedure_environment);
    return 0;
  }
}



/* Eval in a local environment.  We would like to have the ability to
 * evaluate in a specified local environment, but due to the memoization
 * this isn't normally possible.  We solve it by copying the code before
 * evaluating.  Probably the best solution would be to have eval.c generate
 * yet another evaluator.  They are not very big actually.
 */
SCM_PROC (s_local_eval, "local-eval", 2, 0, 0, scm_local_eval);

SCM
scm_local_eval (exp, env)
     SCM exp;
     SCM env;
{
  return scm_eval_3 (exp, 1, env);
}

static char s_start_stack[] = "start-stack";
SCM
scm_m_start_stack (exp, env)
     SCM exp;
     SCM env;
{
  SCM answer;
  scm_debug_frame *oframe = scm_last_debug_frame;
  scm_debug_frame vframe;
  exp = SCM_CDR (exp);
  SCM_ASSERT (SCM_NIMP (exp)
	      && SCM_CONSP (exp)
	      && SCM_NIMP (SCM_CDR (exp))
	      && SCM_CONSP (SCM_CDR (exp))
	      && SCM_NULLP (SCM_CDDR (exp)),
	      exp,
	      SCM_WNA,
	      s_start_stack);
  vframe.prev = 0;
  vframe.status = SCM_VOIDFRAME;
  vframe.vect[0].id = scm_eval_car (exp, env);
  scm_last_debug_frame = &vframe;
  answer = scm_eval_car (SCM_CDR (exp), env);
  scm_last_debug_frame = oframe;
  return answer;
}

/* {Debug Objects}
 *
 * The debugging evaluator throws these on frame traps.
 */

long scm_tc16_debugobj;

static int prindebugobj SCM_P ((SCM obj, SCM port, scm_print_state *pstate));

static int
prindebugobj (obj, port, pstate)
     SCM obj;
     SCM port;
     scm_print_state *pstate;
{
  scm_gen_puts (scm_regular_string, "#<debug-object ", port);
  scm_intprint (SCM_DEBUGOBJ_FRAME (obj), 16, port);
  scm_gen_putc ('>', port);
  return 1;
}

static scm_smobfuns debugobjsmob =
{scm_mark0, scm_free0, prindebugobj, 0};

SCM_PROC (s_debug_object_p, "debug-object?", 1, 0, 0, scm_debug_object_p);

SCM
scm_debug_object_p (obj)
     SCM obj;
{
  return SCM_NIMP (obj) && SCM_DEBUGOBJP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM
scm_make_debugobj (frame)
     scm_debug_frame *frame;
{
  register SCM z;
  SCM_DEFER_INTS;
  SCM_NEWCELL (z);
  SCM_CAR (z) = scm_tc16_debugobj;
  SCM_DEBUGOBJ_FRAME (z) = (SCM) frame;
  SCM_ALLOW_INTS;
  return z;
}



void
scm_init_debug ()
{
  scm_init_opts (scm_debug_options, scm_debug_opts, SCM_N_DEBUG_OPTIONS);
  scm_init_opts (scm_evaluator_traps,
		 scm_evaluator_trap_table,
		 SCM_N_EVALUATOR_TRAPS);

  scm_tc16_memoized = scm_newsmob (&memoizedsmob);
  scm_tc16_debugobj = scm_newsmob (&debugobjsmob);

  scm_i_procname = SCM_CAR (scm_sysintern ("procname", SCM_UNDEFINED));
  scm_i_more = SCM_CAR (scm_sysintern ("...", SCM_UNDEFINED));
  scm_i_source = SCM_CAR (scm_sysintern ("source", SCM_UNDEFINED));
  scm_i_proc = SCM_CAR (scm_sysintern ("proc", SCM_UNDEFINED));
  scm_i_args = SCM_CAR (scm_sysintern ("args", SCM_UNDEFINED));
  scm_i_eval_args = SCM_CAR (scm_sysintern ("eval-args", SCM_UNDEFINED));

  scm_make_synt (s_start_stack, scm_makacro, scm_m_start_stack);
  
  scm_add_feature ("debug-extensions");

#include "debug.x"
}

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
#ifdef __STDC__
SCM
scm_debug_options (SCM setting)
#else
SCM
scm_debug_options (setting)
     SCM setting;
#endif
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
#ifdef __STDC__
SCM
scm_evaluator_traps (SCM setting)
#else
SCM
scm_evaluator_traps (setting)
     SCM setting;
#endif
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
#ifdef __STDC__
SCM
scm_single_step (SCM cont, SCM val)
#else
SCM
scm_single_step (cont, val)
     SCM cont, SCM val;
#endif
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

#ifdef __STDC__
static int
prinmemoized (SCM obj, SCM port, int writing)
#else
static int
prinmemoized (obj, port, writing)
     SCM obj;
     SCM port;
     int writing;
#endif
{
  scm_gen_puts (scm_regular_string, "#<memoized ", port);
  scm_iprin1 (scm_unmemoize (obj), port, 1);
  scm_gen_putc ('>', port);
  return 1;
}

static scm_smobfuns memoizedsmob =
{scm_markcdr, scm_free0, prinmemoized, 0};

SCM_PROC (s_memoized_p, "memoized?", 1, 0, 0, scm_memoized_p);
#ifdef __STDC__
SCM
scm_memoized_p (SCM obj)
#else
SCM
scm_memoized_p (obj)
     SCM obj;
#endif
{
  return SCM_NIMP (obj) && SCM_MEMOIZEDP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}

#ifdef __STDC__
SCM
scm_make_memoized (SCM exp, SCM env)
#else
SCM
scm_make_memoized (exp, env)
     SCM exp, SCM env;
#endif
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
#ifdef __STDC__
SCM
scm_unmemoize (SCM m)
#else
SCM
scm_unmemoize (m)
     SCM m;
#endif
{
  SCM_ASSERT (SCM_MEMOIZEDP (m), m, SCM_ARG1, s_unmemoize);
  return scm_unmemocopy (SCM_MEMOEXP (m), SCM_MEMOENV (m));
}

SCM_PROC (s_memoized_environment, "memoized-environment", 1, 0, 0, scm_memoized_environment);
#ifdef __STDC__
SCM
scm_memoized_environment (SCM m)
#else
SCM
scm_memoized_environment (m)
     SCM m;
#endif
{
  SCM_ASSERT (SCM_MEMOIZEDP (m), m, SCM_ARG1, s_unmemoize);
  return SCM_MEMOENV (m);
}

SCM_PROC (s_procedure_name, "procedure-name", 1, 0, 0, scm_procedure_name);
#ifdef __STDC__
SCM
scm_procedure_name (SCM proc)
#else
SCM
scm_procedure_name (proc)
     SCM proc;
#endif
{
  SCM_ASSERT(scm_procedure_p (proc) == SCM_BOOL_T,
	     proc,
	     SCM_ARG1,
	     s_procedure_name);
  switch (SCM_TYP7 (proc)) {
  case scm_tcs_closures:
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
#ifdef __STDC__
SCM
scm_procedure_source (SCM proc)
#else
SCM
scm_procedure_source (proc)
     SCM proc;
#endif
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
#ifdef __STDC__
SCM
scm_procedure_environment (SCM proc)
#else
SCM
scm_procedure_environment (proc)
     SCM proc;
#endif
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
#ifdef __STDC__
SCM
scm_local_eval (SCM exp, SCM env)
#else
SCM
scm_local_eval (exp, env)
     SCM exp;
     SCM env;
#endif
{
  return scm_eval_3 (exp, 1, env);
}

/* {Stack Frames}
 *
 * The stack is a list of stackframes, from root to current.
 *
 * A stackframe is a list of virtual stackframes, which occur due to
 * the evaluators tail recursion.  A virtual stackframe normally
 * corresponds to an eval/apply pair, but macros and special forms
 * (which are implemented as macros in scm...) only have eval
 * information and apply calls leads to apply only frames.
 *
 * A virtual stackframe is either a property list or the symbol
 * ... which marks the location of virtual stackframes which could not
 * be stored with the current debug-depth.
 *
 * Property	Type		Description
 *
 * These three only present in eval frames:
 *
 * sexpr	memoized	Source code expression and environment.
 * proc		procedure	The procedure being applied.
 *				(Not present if pre-apply state.)
 * args		list		The arguments evaluated so far.
 * eval-args	boolean		True if evaluation of arguments not finished.
 */

/* {Debug Objects}
 *
 * The debugging evaluator throws these on frame traps.
 */

long scm_tc16_debugobj;

#define DEBUGOBJP(x) (scm_tc16_debugobj == SCM_TYP16 (x))
#define DBGFRAME(x) SCM_CDR (x)

#ifdef __STDC__
static int
prindebugobj (SCM obj, SCM port, int writing)
#else
static int
prindebugobj (writing)
     SCM obj, SCM port, int writing;
#endif
{
  scm_gen_puts (scm_regular_string, "#<debug-object ", port);
  scm_intprint (DBGFRAME (obj), 16, port);
  scm_gen_putc ('>', port);
  return 1;
}

static scm_smobfuns debugobjsmob =
{scm_mark0, scm_free0, prindebugobj, 0};

SCM_PROC (s_debug_object_p, "debug-object?", 1, 0, 0, scm_debug_object_p);
#ifdef __STDC__
SCM
scm_debug_object_p (SCM obj)
#else
SCM
scm_debug_object_p (obj)
     SCM obj;
#endif
{
  return SCM_NIMP (obj) && DEBUGOBJP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}

#ifdef __STDC__
SCM
scm_make_debugobj (scm_debug_frame *frame)
#else
SCM
scm_make_debugobj (frame)
     scm_debug_frame *frame;
#endif
{
  register SCM z;
  SCM_DEFER_INTS;
  SCM_NEWCELL (z);
  SCM_CAR (z) = scm_tc16_debugobj;
  DBGFRAME (z) = (SCM) frame;
  SCM_ALLOW_INTS;
  return z;
}

#ifdef __STDC__
static SCM
_scm_stack_frame_to_plist (scm_debug_frame *frame, long offset)
#else
static SCM
_scm_stack_frame_to_plist (frame, offset)
     scm_debug_frame *frame;
     long offset;
#endif
{
  int size;
  scm_debug_info *info;
  if (SCM_EVALFRAMEP (*frame))
    {
      size = frame->status & SCM_MAX_FRAME_SIZE;
      info = (scm_debug_info *) (*((SCM_STACKITEM **) &frame->vect[size]) + offset);
      if ((info - frame->vect) & 1)
	{
	  /* Debug.vect ends with apply info. */
	  SCM p;
	  --info;
	  if (info[1].a.proc == SCM_UNDEFINED)
	    p = SCM_EOL;
	  else
	    p = scm_acons (scm_i_proc,
			   info[1].a.proc,
			   scm_acons (scm_i_args,
				      info[1].a.args,
				      SCM_ARGS_READY_P (*frame)
				      ? SCM_EOL
				      : scm_acons (scm_i_eval_args,
						   SCM_BOOL_T,
						   SCM_EOL)));
	  return scm_acons (scm_i_source,
			    scm_make_memoized (info[0].e.exp, info[0].e.env),
			    p);
	}
      else
	/* Debug.vect ends with eval info. */
	return scm_acons (scm_i_source,
			  scm_make_memoized (info[0].e.exp, info[0].e.env),
			  SCM_EOL);
    }
  else
    return scm_acons (scm_i_proc,
		      frame->vect[0].a.proc,
		      scm_acons (scm_i_args, frame->vect[0].a.args, SCM_EOL));
}

SCM_PROC (s_last_stack_frame, "last-stack-frame", 1, 0, 0, scm_last_stack_frame);
#ifdef __STDC__
SCM
scm_last_stack_frame (SCM obj)
#else
SCM
scm_last_stack_frame (obj)
     SCM obj;
#endif
{
  scm_debug_frame *frame;
  long offset = 0;
  SCM_ASSERT (SCM_NIMP (obj), obj, SCM_ARG1, s_last_stack_frame);
  if (scm_tc16_debugobj == SCM_TYP16 (obj))
    frame = (scm_debug_frame *) DBGFRAME (obj);
  else if (scm_tc7_contin == SCM_TYP7 (obj))
    {
      frame = SCM_DFRAME (obj);
      offset = (SCM_STACKITEM *) (SCM_CHARS (obj) + sizeof (regs)) - SCM_BASE (obj);
#ifndef STACK_GROWS_UP
      offset += SCM_LENGTH (obj);
#endif
    }
  else scm_wta (obj, (char *) SCM_ARG1, s_last_stack_frame);
  if (!frame)
    return SCM_BOOL_F;
  return _scm_stack_frame_to_plist ((scm_debug_frame *) ((SCM_STACKITEM *) frame + offset), offset);
}

/* Make a scheme object of the current evaluation stack.
 */

SCM_PROC (s_expr_stack, "expr-stack", 0, 1, 0, scm_expr_stack);
#ifdef __STDC__
SCM
scm_expr_stack (SCM obj)
#else
SCM
scm_expr_stack (obj)
     SCM obj;
#endif
{
  SCM frs = SCM_EOL, vfrs, p;
  int size;
  int max_vfrs = SCM_BACKTRACE_DEPTH;
  scm_debug_info *info;
  scm_debug_frame *frame;
  long offset = 0;
  if (SCM_UNBNDP (obj))
    frame = last_debug_info_frame;
  else
    {
      SCM_ASSERT (SCM_NIMP (obj), obj, SCM_ARG1, s_expr_stack);
      if (scm_tc16_debugobj == SCM_TYP16 (obj))
	frame = (scm_debug_frame *) DBGFRAME (obj);
      else if (scm_tc7_contin == SCM_TYP7 (obj))
	{
	  frame = SCM_DFRAME (obj);
	  offset = (SCM_STACKITEM *) (SCM_CHARS (obj) + sizeof (regs)) - SCM_BASE (obj);
#ifndef STACK_GROWS_UP
	  offset += SCM_LENGTH (obj);
#endif
	}
      else scm_wta (obj, (char *) SCM_ARG1, s_expr_stack);
    }
  for (; frame && max_vfrs > 0; frame = frame->prev)
    {
      frame = (scm_debug_frame *) ((SCM_STACKITEM *) frame + offset);
      p = _scm_stack_frame_to_plist (frame, offset);
      if (SCM_EVALFRAMEP (*frame))
	{
	  size = frame->status & SCM_MAX_FRAME_SIZE;
	  info = (scm_debug_info *) (*((SCM_STACKITEM **) &frame->vect[size]) + offset);
	  vfrs = SCM_EOL;
	  if ((info - frame->vect) & 1)
	    --info;
	  /* Data in the apply part of an eval info frame comes from
	     previous stack frame if the scm_debug_info vector is overflowed. */
	  else if (SCM_OVERFLOWP (*frame)
		   && !SCM_UNBNDP (info[1].a.proc))
	    {
	      vfrs = scm_cons (p, SCM_EOL);
	      --max_vfrs;
	      p = scm_acons (scm_i_proc,
			     info[1].a.proc,
			     scm_acons (scm_i_args, info[1].a.args, SCM_EOL));
	    }
	  info -= 2;
	  vfrs = scm_cons (p, vfrs);
	  --max_vfrs;
	  if (SCM_OVERFLOWP (*frame))
	    vfrs = scm_cons (scm_i_more, vfrs);
	  while (info >= frame->vect)
	    {
	      p = SCM_EOL;
	      if (!SCM_UNBNDP (info[1].a.proc))
		p = scm_acons (scm_i_proc,
			       info[1].a.proc,
			       scm_acons (scm_i_args, info[1].a.args, SCM_EOL));
	      p = scm_acons (scm_i_source,
			     scm_make_memoized (info[0].e.exp, info[0].e.env),
			     p);
	      info -= 2;
	      vfrs = scm_cons (p, vfrs);
	      --max_vfrs;
	    }
	}
      else
	{
	  vfrs = scm_cons (p, SCM_EOL);
	  --max_vfrs;
	}
      frs = scm_cons (vfrs, frs);
    }
  if (max_vfrs <= 0)
    frs = scm_cons (scm_i_more, frs);
  return frs;
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

  scm_add_feature ("debug-extensions");

#include "debug.x"
}

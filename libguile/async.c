/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002 Free Software Foundation, Inc.
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
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
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
 * If you do not wish that, delete this exception notice.  */




#include <signal.h>
#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/throw.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/lang.h"
#include "libguile/dynwind.h"
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/async.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* This is not used for anything except checking that DEFER_INTS and
   ALLOW_INTS are used properly.
 */
int scm_ints_disabled = 1;


/* {Asynchronous Events}
 *
 * There are two kinds of asyncs: system asyncs and user asyncs.  The
 * two kinds have some concepts in commen but work slightly
 * differently and are not interchangeable.
 *
 * System asyncs are used to run arbitrary code at the next safe point
 * in a specified thread.  You can use them to trigger execution of
 * Scheme code from signal handlers or to interrupt a thread, for
 * example.
 *
 * Each thread has a list of 'activated asyncs', which is a normal
 * Scheme list of procedures with zero arguments.  When a thread
 * executes a SCM_ASYNC_TICK statement (which is included in
 * SCM_TICK), it will call all procedures on this list.
 *
 * Also, a thread will wake up when a procedure is added to its list
 * of active asyncs and call them.  After that, it will go to sleep
 * again.  (Not implemented yet.)
 *
 *
 * User asyncs are a little data structure that consists of a
 * procedure of zero arguments and a mark.  There are functions for
 * setting the mark of a user async and for calling all procedures of
 * marked asyncs in a given list.  Nothing you couldn't quickly
 * implement yourself.
 */




/* User asyncs. */

static scm_t_bits tc16_async;

/* cmm: this has SCM_ prefix because SCM_MAKE_VALIDATE expects it.
   this is ugly.  */
#define SCM_ASYNCP(X)		SCM_TYP16_PREDICATE (tc16_async, X)
#define VALIDATE_ASYNC(pos, a)	SCM_MAKE_VALIDATE_MSG(pos, a, ASYNCP, "user async")

#define ASYNC_GOT_IT(X)        (SCM_CELL_WORD_0 (X) >> 16)
#define SET_ASYNC_GOT_IT(X, V) (SCM_SET_CELL_WORD_0 ((X), SCM_TYP16 (X) | ((V) << 16)))
#define ASYNC_THUNK(X)         SCM_CELL_OBJECT_1 (X)

static SCM
async_gc_mark (SCM obj)
{
  return ASYNC_THUNK (obj);
}

SCM_DEFINE (scm_async, "async", 1, 0, 0,
	    (SCM thunk),
	    "Create a new async for the procedure @var{thunk}.")
#define FUNC_NAME s_scm_async
{
  SCM_RETURN_NEWSMOB (tc16_async, SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM_DEFINE (scm_async_mark, "async-mark", 1, 0, 0,
            (SCM a),
	    "Mark the async @var{a} for future execution.")
#define FUNC_NAME s_scm_async_mark
{
  VALIDATE_ASYNC (1, a);
  SET_ASYNC_GOT_IT (a, 1);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_run_asyncs, "run-asyncs", 1, 0, 0,
	    (SCM list_of_a),
	    "Execute all thunks from the asyncs of the list @var{list_of_a}.")
#define FUNC_NAME s_scm_run_asyncs
{
  while (! SCM_NULL_OR_NIL_P (list_of_a))
    {
      SCM a;
      SCM_VALIDATE_CONS (1, list_of_a);
      a = SCM_CAR (list_of_a);
      VALIDATE_ASYNC (SCM_ARG1, a);
      if (ASYNC_GOT_IT (a))
	{
	  SET_ASYNC_GOT_IT (a, 0);
	  scm_call_0 (ASYNC_THUNK (a));
	}
      list_of_a = SCM_CDR (list_of_a);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME



/* System asyncs. */

void
scm_async_click ()
{
  SCM asyncs;

  if (scm_root->block_asyncs == 0)
    {
      while (!SCM_NULLP(asyncs = scm_root->active_asyncs))
	{
	  scm_root->active_asyncs = SCM_EOL;
	  do
	    {
	      SCM c = SCM_CDR (asyncs);
	      SCM_SETCDR (asyncs, SCM_BOOL_F);
	      scm_call_0 (SCM_CAR (asyncs));
	      asyncs = c;
	    }
	  while (!SCM_NULLP(asyncs));
	}
    }
}

#if (SCM_ENABLE_DEPRECATED == 1)

SCM_DEFINE (scm_system_async, "system-async", 1, 0, 0,
            (SCM thunk),
	    "This function is deprecated.  You can use @var{thunk} directly\n"
            "instead of explicitely creating an async object.\n")
#define FUNC_NAME s_scm_system_async
{
  scm_c_issue_deprecation_warning 
    ("'system-async' is deprecated.  "
     "Use the procedure directly with 'system-async-mark'.");
  return thunk;
}
#undef FUNC_NAME

#endif /* SCM_ENABLE_DEPRECATED == 1 */

void
scm_i_queue_async_cell (SCM c, scm_root_state *root)
{
  if (SCM_CDR (c) == SCM_BOOL_F)
    {
      SCM p = root->active_asyncs;
      SCM_SETCDR (c, SCM_EOL);
      if (p == SCM_EOL)
	root->active_asyncs = c;
      else
	{
	  SCM pp;
	  while ((pp = SCM_CDR(p)) != SCM_EOL)
	    {
	      if (SCM_CAR (p) == SCM_CAR (c))
		return;
	      p = pp;
	    }
	  SCM_SETCDR (p, c);
	}
    }
}

SCM_DEFINE (scm_system_async_mark_for_thread, "system-async-mark", 1, 1, 0,
           (SCM proc, SCM thread),
	    "Mark @var{proc} (a procedure with zero arguments) for future execution\n"
	    "in @var{thread}.  If @var{proc} has already been marked for\n"
	    "@var{thread} but has not been executed yet, this call has no effect.\n"
	    "If @var{thread} is omitted, the thread that called\n"
	    "@code{system-async-mark} is used.\n\n"
	    "This procedure is not safe to be called from C signal handlers.  Use\n"
	    "@code{scm_sigaction} or @code{scm_sigaction_for_thread} to install\n"
	    "signal handlers.")
#define FUNC_NAME s_scm_system_async_mark_for_thread
{
#ifdef USE_THREADS
  if (SCM_UNBNDP (thread))
    thread = scm_current_thread ();
  else
    SCM_VALIDATE_THREAD (2, thread);
      
  scm_i_queue_async_cell (scm_cons (proc, SCM_BOOL_F),
			  scm_i_thread_root (thread));
#else
  scm_i_queue_async_cell (scm_cons (proc, SCM_BOOL_F), scm_root);
#endif
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_system_async_mark (SCM proc)
#define FUNC_NAME s_scm_system_async_mark_for_thread
{
  return scm_system_async_mark_for_thread (proc, SCM_UNDEFINED);
}
#undef FUNC_NAME




SCM_DEFINE (scm_noop, "noop", 0, 0, 1,
	    (SCM args),
	    "Do nothing.  When called without arguments, return @code{#f},\n"
	    "otherwise return the first argument.")
#define FUNC_NAME s_scm_noop
{
  SCM_VALIDATE_REST_ARGUMENT (args);
  return (SCM_NULL_OR_NIL_P (args) ? SCM_BOOL_F : SCM_CAR (args));
}
#undef FUNC_NAME




#if (SCM_ENABLE_DEPRECATED == 1)

SCM_DEFINE (scm_unmask_signals, "unmask-signals", 0, 0, 0,
	    (),
	    "Unmask signals. The returned value is not specified.")
#define FUNC_NAME s_scm_unmask_signals
{
  scm_c_issue_deprecation_warning 
    ("'unmask-signals' is deprecated.  "
     "Use 'call-with-blocked-asyncs' instead.");

  if (scm_root->block_asyncs == 0)
    SCM_MISC_ERROR ("signals already unmasked", SCM_EOL);
  scm_root->block_asyncs = 0;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_mask_signals, "mask-signals", 0, 0, 0,
	    (),
	    "Mask signals. The returned value is not specified.")
#define FUNC_NAME s_scm_mask_signals
{
  scm_c_issue_deprecation_warning 
    ("'mask-signals' is deprecated.  Use 'call-with-blocked-asyncs' instead.");

  if (scm_root->block_asyncs > 0)
    SCM_MISC_ERROR ("signals already masked", SCM_EOL);
  scm_root->block_asyncs = 1;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#endif /* SCM_ENABLE_DEPRECATED == 1 */

static void
increase_block (void *unused)
{
  scm_root->block_asyncs++;
}

static void
decrease_block (void *unused)
{
  scm_root->block_asyncs--;
}

SCM_DEFINE (scm_call_with_blocked_asyncs, "call-with-blocked-asyncs", 1, 0, 0,
	    (SCM proc),
	    "Call @var{proc} with no arguments and block the execution\n"
	    "of system asyncs by one level for the current thread while\n"
	    "it is running.  Return the value returned by @var{proc}.\n")
#define FUNC_NAME s_scm_call_with_blocked_asyncs
{
  return scm_internal_dynamic_wind (increase_block,
				    (scm_t_inner) scm_call_0,
				    decrease_block,
				    proc, NULL);
}
#undef FUNC_NAME

void *
scm_c_call_with_blocked_asyncs (void *(*proc) (void *data), void *data)
{
  return scm_internal_dynamic_wind (increase_block,
				    (scm_t_inner) proc,
				    decrease_block,
				    data, NULL);
}


SCM_DEFINE (scm_call_with_unblocked_asyncs, "call-with-unblocked-asyncs", 1, 0, 0,
	    (SCM proc),
	    "Call @var{proc} with no arguments and unblock the execution\n"
	    "of system asyncs by one level for the current thread while\n"
	    "it is running.  Return the value returned by @var{proc}.\n")
#define FUNC_NAME s_scm_call_with_unblocked_asyncs
{
  if (scm_root->block_asyncs == 0)
    SCM_MISC_ERROR ("asyncs already unblocked", SCM_EOL);
  return scm_internal_dynamic_wind (decrease_block,
				    (scm_t_inner) scm_call_0,
				    increase_block,
				    proc, NULL);
}
#undef FUNC_NAME

void *
scm_c_call_with_unblocked_asyncs (void *(*proc) (void *data), void *data)
{
  if (scm_root->block_asyncs == 0)
    scm_misc_error ("scm_c_call_with_unblocked_asyncs",
		    "asyncs already unblocked", SCM_EOL);
  return scm_internal_dynamic_wind (decrease_block,
				     (scm_t_inner) proc,
				    increase_block,
				    data, NULL);
}



void
scm_init_async ()
{
  scm_asyncs = SCM_EOL;
  tc16_async = scm_make_smob_type ("async", 0);
  scm_set_smob_mark (tc16_async, async_gc_mark);

#include "libguile/async.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

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
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/async.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif



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

/* True between SCM_DEFER_INTS and SCM_ALLOW_INTS, and
 * when the interpreter is not running at all.
 */
int scm_ints_disabled = 1;
unsigned int scm_mask_ints = 1;



/* User asyncs. */

static scm_t_bits tc16_async;

/* cmm: this has SCM_ prefix because SCM_MAKE_VALIDATE expects it.
   this is ugly.  */
#define SCM_ASYNCP(X)		SCM_TYP16_PREDICATE (tc16_async, X)
#define VALIDATE_ASYNC(pos, a)	SCM_MAKE_VALIDATE(pos, a, ASYNCP)

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
      scm_mask_ints = 1;
      if (ASYNC_GOT_IT (a))
	{
	  SET_ASYNC_GOT_IT (a, 0);
	  scm_call_0 (ASYNC_THUNK (a));
	}
      scm_mask_ints = 0;
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

  if (!scm_mask_ints)
    {
      while (!SCM_NULLP(asyncs = scm_active_asyncs))
	{
	  scm_active_asyncs = SCM_EOL;
	  do
	    {
	      SCM c = SCM_CDR (asyncs);
	      SCM_SETCDR (asyncs, SCM_EOL);
	      scm_call_0 (SCM_CAR (asyncs));
	      asyncs = c;
	    }
	  while (!SCM_NULLP(asyncs));
	}
    }
}

SCM_DEFINE (scm_system_async, "system-async", 1, 0, 0,
            (SCM thunk),
	    "This function is deprecated.  You can use @var{thunk} directly\n"
            "instead of explicitely creating a asnc object.\n")
#define FUNC_NAME s_scm_system_async
{
  scm_c_issue_deprecation_warning 
    ("'system-async' is deprecated.  "
     "Use the procedure directly with 'system-async-mark'.");
  return thunk;
}
#undef FUNC_NAME

void
scm_i_queue_async_cell (SCM c, scm_root_state *root)
{
  if (SCM_CDR (c) == SCM_EOL)
    {
      SCM_SETCDR (c, root->active_asyncs);
      root->active_asyncs = c;
    }
}

SCM_DEFINE (scm_system_async_mark_for_thread, "system-async-mark", 1, 1, 0,
           (SCM proc, SCM thread),
	    "Register the procedure @var{proc} for future execution\n"
            "in @var{thread}.  When @var{thread} is not specified,\n"
	    "use the current thread.")
#define FUNC_NAME s_scm_system_async_mark_for_thread
{
  scm_i_queue_async_cell (scm_cons (proc, SCM_EOL),
			  (SCM_UNBNDP (thread)
			   ? scm_root
			   : scm_i_thread_root (thread)));
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




SCM_DEFINE (scm_unmask_signals, "unmask-signals", 0, 0, 0,
	    (),
	    "Unmask signals. The returned value is not specified.")
#define FUNC_NAME s_scm_unmask_signals
{
  scm_mask_ints = 0;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_mask_signals, "mask-signals", 0, 0, 0,
	    (),
	    "Mask signals. The returned value is not specified.")
#define FUNC_NAME s_scm_mask_signals
{
  scm_mask_ints = 1;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



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

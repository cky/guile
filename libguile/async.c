/* Copyright (C) 1995, 96, 97, 98, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include <signal.h>
#include "_scm.h"
#include "eval.h"
#include "throw.h"
#include "smob.h"

#include "validate.h"
#include "async.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif



/* {Asynchronous Events}
 *
 *
 * Async == thunk + mark.
 *
 * Setting the mark guarantees future execution of the thunk.  More
 * than one set may be satisfied by a single execution.
 * 
 * scm_tick_clock decremented once per SCM_ALLOW_INTS.
 * Async execution triggered by SCM_ALLOW_INTS when scm_tick_clock drops to 0.
 * Async execution prevented by scm_mask_ints != 0.
 *
 * If the clock reaches 0 when scm_mask_ints != 0, then reset the clock
 * to 1.
 *
 * If the clock reaches 0 any other time, run marked asyncs.
 *
 * From a unix signal handler, mark a corresponding async and set the clock
 * to 1.   Do SCM_REDEFER_INTS;/SCM_REALLOW_INTS so that if the signal handler is not
 * called in the dynamic scope of a critical section, it is excecuted immediately.
 *
 * Overall, closely timed signals of a particular sort may be combined.  Pending signals
 * are delivered in a fixed priority order, regardless of arrival order.
 *
 */

/* True between SCM_DEFER_INTS and SCM_ALLOW_INTS, and
 * when the interpreter is not running at all.
 */
int scm_ints_disabled = 1;

unsigned int scm_async_clock = 20;
static unsigned int scm_async_rate = 20;
unsigned int scm_mask_ints = 1;

static unsigned int scm_tick_clock = 0;
static unsigned int scm_tick_rate = 0;
static unsigned int scm_desired_tick_rate = 0;
static unsigned int scm_switch_clock = 0;
static unsigned int scm_switch_rate = 0;
static unsigned int scm_desired_switch_rate = 0;

static long scm_tc16_async;



int
scm_asyncs_pending ()
{
  SCM pos;
  pos = scm_asyncs;
  while (pos != SCM_EOL)
    {
      SCM a;
      struct scm_async * it;
      a = SCM_CAR (pos);
      it = SCM_ASYNC (a);
      if (it->got_it)
	return 1;
      pos = SCM_CDR (pos);
    }
  return 0;
}

#if 0
static SCM
scm_sys_tick_async_thunk (void)
{
  scm_deliver_signal (SCM_TICK_SIGNAL);
  return SCM_BOOL_F;
}
#endif

void
scm_async_click ()
{
  int owe_switch;
  int owe_tick;

  if (!scm_switch_rate)
    {
      owe_switch = 0;
      scm_switch_clock = scm_switch_rate = scm_desired_switch_rate;
      scm_desired_switch_rate = 0;
    }
  else
    {
      owe_switch = (scm_async_rate >= scm_switch_clock);
      if (owe_switch)
	{
	  if (scm_desired_switch_rate)
	    {
	      scm_switch_clock = scm_switch_rate = scm_desired_switch_rate;
	      scm_desired_switch_rate = 0;
	    }
	  else
	    scm_switch_clock = scm_switch_rate;
	}
      else
	{
	  if (scm_desired_switch_rate)
	    {
	      scm_switch_clock = scm_switch_rate = scm_desired_switch_rate;
	      scm_desired_switch_rate = 0;
	    }
	  else
	    scm_switch_clock -= scm_async_rate;
	}
    }

  if (scm_mask_ints)
    {
      if (owe_switch)
	scm_switch ();
      scm_async_clock = 1;
      return;;
    }
  
  if (!scm_tick_rate)
    {
      unsigned int r;
      owe_tick = 0;
      r = scm_desired_tick_rate;
      if (r)
	{
	  scm_desired_tick_rate = 0;
	  scm_tick_rate = r;
	  scm_tick_clock = r;
	}
    }
  else
    {
      owe_tick = (scm_async_rate >= scm_tick_clock);
      if (owe_tick)
	{
	  scm_tick_clock = scm_tick_rate = scm_desired_tick_rate;
	  scm_desired_tick_rate = 0;
	}
      else
	{
	  if (scm_desired_tick_rate)
	    {
	      scm_tick_clock = scm_tick_rate = scm_desired_tick_rate;
	      scm_desired_tick_rate = 0;
	    }
	  else
	    scm_tick_clock -= scm_async_rate;
	}
    }

  /* 
     if (owe_tick)
       scm_async_mark (system_signal_asyncs[SCM_SIG_ORD(SCM_TICK_SIGNAL)]);
     */

  SCM_DEFER_INTS;
  if (scm_tick_rate && scm_switch_rate)
    {
      scm_async_rate = min (scm_tick_clock,  scm_switch_clock);
      scm_async_clock = scm_async_rate;
    }
  else if (scm_tick_rate)
    {
      scm_async_clock = scm_async_rate = scm_tick_clock;
    }
  else if (scm_switch_rate)
    {
      scm_async_clock = scm_async_rate = scm_switch_clock;
    }
  else
    scm_async_clock = scm_async_rate = 1 << 16;
  SCM_ALLOW_INTS_ONLY;

 tail:
  scm_run_asyncs (scm_asyncs);

  SCM_DEFER_INTS;
  if (scm_asyncs_pending ())
    {
      SCM_ALLOW_INTS_ONLY;
      goto tail;
    }
  SCM_ALLOW_INTS;

  if (owe_switch)
    scm_switch ();
}





void
scm_switch ()
{
#if 0 /* Thread switching code should probably reside here, but the
         async switching code doesn't seem to work, so it's put in the
         SCM_ASYNC_TICK macro instead. /mdj */
  SCM_THREAD_SWITCHING_CODE;
#endif
}



static SCM
mark_async (SCM obj)
{
  struct scm_async * it;
  it = SCM_ASYNC (obj);
  return it->thunk;
}



SCM_DEFINE (scm_async, "async", 1, 0, 0, 
           (SCM thunk),
"")
#define FUNC_NAME s_scm_async
{
  SCM_RETURN_NEWSMOB2 (scm_tc16_async, 0, thunk);
}
#undef FUNC_NAME

SCM_DEFINE (scm_system_async, "system-async", 1, 0, 0, 
            (SCM thunk),
"")
#define FUNC_NAME s_scm_system_async
{
  SCM it;
  SCM list;

  it = scm_async (thunk);
  SCM_NEWCELL (list);
  SCM_SETCAR (list, it);
  SCM_SETCDR (list, scm_asyncs);
  scm_asyncs = list;
  return it;
}
#undef FUNC_NAME

SCM_DEFINE (scm_async_mark, "async-mark", 1, 0, 0, 
            (SCM a),
"")
#define FUNC_NAME s_scm_async_mark
{
  struct scm_async * it;
  SCM_VALIDATE_ASYNC_COPY (1,a,it);
  it->got_it = 1;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_system_async_mark, "system-async-mark", 1, 0, 0, 
           (SCM a),
"")
#define FUNC_NAME s_scm_system_async_mark
{
  struct scm_async * it;
  SCM_VALIDATE_ASYNC_COPY (1,a,it);
  SCM_REDEFER_INTS;
  it->got_it = 1;
  scm_async_rate = 1 + scm_async_rate - scm_async_clock;
  scm_async_clock = 1;
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_run_asyncs, "run-asyncs", 1, 0, 0, 
           (SCM list_of_a),
"")
#define FUNC_NAME s_scm_run_asyncs
{
  if (scm_mask_ints)
    return SCM_BOOL_F;
  while (list_of_a != SCM_EOL)
    {
      SCM a;
      struct scm_async * it;
      SCM_VALIDATE_CONS (1,list_of_a);
      a = SCM_CAR (list_of_a);
      SCM_VALIDATE_ASYNC_COPY (SCM_ARG1,a,it);
      scm_mask_ints = 1;
      if (it->got_it)
	{
	  it->got_it = 0;
	  scm_apply (it->thunk, SCM_EOL, SCM_EOL);
	}
      scm_mask_ints = 0;
      list_of_a = SCM_CDR (list_of_a);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME




SCM_DEFINE (scm_noop, "noop", 0, 0, 1, 
           (SCM args),
"")
#define FUNC_NAME s_scm_noop
{
  return (SCM_NULLP (args) ? SCM_BOOL_F : SCM_CAR (args));
}
#undef FUNC_NAME




SCM_DEFINE (scm_set_tick_rate, "set-tick-rate", 1, 0, 0, 
           (SCM n),
"")
#define FUNC_NAME s_scm_set_tick_rate
{
  unsigned int old_n;


  SCM_VALIDATE_INUM (1,n);

  old_n = scm_tick_rate;


  scm_desired_tick_rate = SCM_UNPACK (SCM_INUM (n));
  scm_async_rate = 1 + scm_async_rate - scm_async_clock;
  scm_async_clock = 1;
  return SCM_MAKINUM (old_n);
}
#undef FUNC_NAME




SCM_DEFINE (scm_set_switch_rate, "set-switch-rate", 1, 0, 0, 
           (SCM n),
"")
#define FUNC_NAME s_scm_set_switch_rate
{
  unsigned int old_n;
  SCM_VALIDATE_INUM (1,n);
  old_n = scm_switch_rate;
  scm_desired_switch_rate = SCM_UNPACK (SCM_INUM (n));
  scm_async_rate = 1 + scm_async_rate - scm_async_clock;
  scm_async_clock = 1;
  return SCM_MAKINUM (old_n);
}
#undef FUNC_NAME



/* points to the GC system-async, so that scm_gc_end can find it.  */
SCM scm_gc_async;

/* the vcell for gc-thunk.  */
static SCM scm_gc_vcell;

/* the thunk installed in the GC system-async, which is marked at the
   end of garbage collection.  */
static SCM
scm_sys_gc_async_thunk (void)
{
  if (SCM_NFALSEP (scm_gc_vcell))
    {
      SCM proc = SCM_CDR (scm_gc_vcell);

      if (SCM_NFALSEP (proc) && !SCM_UNBNDP (proc))
	scm_apply (proc, SCM_EOL, SCM_EOL);
    }
  return SCM_UNSPECIFIED;
}



SCM_DEFINE (scm_unmask_signals, "unmask-signals", 0, 0, 0, 
           (),
"")
#define FUNC_NAME s_scm_unmask_signals
{
  scm_mask_ints = 0;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_mask_signals, "mask-signals", 0, 0, 0, 
           (),
"")
#define FUNC_NAME s_scm_mask_signals
{
  scm_mask_ints = 1;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_init_async ()
{
  SCM a_thunk;
  scm_tc16_async = scm_make_smob_type_mfpe ("async", 0,
                                           mark_async, NULL, NULL, NULL);
  scm_gc_vcell = scm_sysintern ("gc-thunk", SCM_BOOL_F);
  a_thunk = scm_make_gsubr ("%gc-thunk", 0, 0, 0, scm_sys_gc_async_thunk);
  scm_gc_async = scm_system_async (a_thunk);

#include "async.x"
}

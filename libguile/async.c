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


#include <stdio.h>
#include <signal.h>
#include "_scm.h"
#include "eval.h"
#include "throw.h"
#include "smob.h"

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

static SCM system_signal_asyncs[SCM_NUM_SIGS];
static SCM handler_var;
static SCM symbol_signal;


struct scm_async
{
  int got_it;			/* needs to be delivered? */
  SCM thunk;			/* the handler. */
};


static long scm_tc16_async;

#define SCM_ASYNCP(X) 	(scm_tc16_async == SCM_GCTYP16 (X))
#define SCM_ASYNC(X) 	((struct scm_async *)SCM_CDR (X))






static int asyncs_pending SCM_P ((void));

static int
asyncs_pending ()
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

  if (owe_tick)
    scm_async_mark (system_signal_asyncs[SCM_SIG_ORD(SCM_TICK_SIGNAL)]);

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
  if (asyncs_pending ())
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



static void scm_deliver_signal SCM_P ((int num));

static void
scm_deliver_signal (num)
     int num;
{
  SCM handler;
  handler = SCM_CDR (handler_var);
  if (handler != SCM_BOOL_F)
    scm_apply (handler, SCM_MAKINUM (num), scm_listofnull);
  else
    {
      scm_mask_ints = 0;
      scm_throw (symbol_signal,
			   scm_listify (SCM_MAKINUM (num), SCM_UNDEFINED));
    }
}





static int print_async SCM_P ((SCM exp, SCM port, scm_print_state *pstate));

static int
print_async (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  scm_gen_puts (scm_regular_string, "#<async ", port);
  scm_intprint(exp, 16, port);
  scm_gen_putc('>', port);
  return 1;
}


static SCM mark_async SCM_P ((SCM obj));

static SCM
mark_async (obj)
     SCM obj;
{
  struct scm_async * it;
  if (SCM_GC8MARKP (obj))
    return SCM_BOOL_F;
  SCM_SETGC8MARK (obj);
  it = SCM_ASYNC (obj);
  return it->thunk;
}


static scm_sizet free_async SCM_P ((SCM obj));

static scm_sizet
free_async (obj)
     SCM obj;
{
  struct scm_async * it;
  it = SCM_ASYNC (obj);
  scm_must_free ((char *)it);
  return (sizeof (*it));
}


static scm_smobfuns  async_smob =
{
  mark_async,
  free_async,
  print_async,
  0
};




SCM_PROC(s_async, "async", 1, 0, 0, scm_async);

SCM
scm_async (thunk)
     SCM thunk;
{
  SCM it;
  struct scm_async * async;

  SCM_NEWCELL (it);
  SCM_DEFER_INTS;
  SCM_SETCDR (it, SCM_EOL);
  async = (struct scm_async *)scm_must_malloc (sizeof (*async), s_async);
  async->got_it = 0;
  async->thunk = thunk;
  SCM_SETCDR (it, (SCM)async);
  SCM_SETCAR (it, (SCM)scm_tc16_async);
  SCM_ALLOW_INTS;
  return it;
}

SCM_PROC(s_system_async, "system-async", 1, 0, 0, scm_system_async);

SCM 
scm_system_async (thunk)
     SCM thunk;
{
  SCM it;
  SCM list;

  it = scm_async (thunk);
  SCM_NEWCELL (list);
  SCM_DEFER_INTS;
  SCM_SETCAR (list, it);
  SCM_SETCDR (list, scm_asyncs);
  scm_asyncs = list;
  SCM_ALLOW_INTS;
  return it;
}

SCM_PROC(s_async_mark, "async-mark", 1, 0, 0, scm_async_mark);

SCM
scm_async_mark (a)
     SCM a;
{
  struct scm_async * it;
  SCM_ASSERT (SCM_NIMP (a) &&  SCM_ASYNCP (a), a, SCM_ARG1, s_async_mark);
  it = SCM_ASYNC (a);
  it->got_it = 1;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_system_async_mark, "system-async-mark", 1, 0, 0, scm_system_async_mark);

SCM
scm_system_async_mark (a)
     SCM a;
{
  struct scm_async * it;
  SCM_ASSERT (SCM_NIMP (a) &&  SCM_ASYNCP (a), a, SCM_ARG1, s_async_mark);
  it = SCM_ASYNC (a);
  SCM_REDEFER_INTS;
  it->got_it = 1;
  scm_async_rate = 1 + scm_async_rate - scm_async_clock;
  scm_async_clock = 1;
  SCM_REALLOW_INTS;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_run_asyncs, "run-asyncs", 1, 0, 0, scm_run_asyncs);

SCM
scm_run_asyncs (list_of_a)
     SCM list_of_a;
{
  SCM pos;

  if (scm_mask_ints)
    return SCM_BOOL_F;
  pos = list_of_a;
  while (pos != SCM_EOL)
    {
      SCM a;
      struct scm_async * it;
      SCM_ASSERT (SCM_NIMP (pos) && SCM_CONSP (pos), pos, SCM_ARG1, s_run_asyncs);
      a = SCM_CAR (pos);
      SCM_ASSERT (SCM_NIMP (a) &&  SCM_ASYNCP (a), a, SCM_ARG1, s_run_asyncs);
      it = SCM_ASYNC (a);
      scm_mask_ints = 1;
      if (it->got_it)
	{
	  it->got_it = 0;
	  scm_apply (it->thunk, SCM_EOL, SCM_EOL);
	}
      scm_mask_ints = 0;
      pos = SCM_CDR (pos);
    }
  return SCM_BOOL_T;
}




SCM_PROC(s_noop, "noop", 0, 0, 1, scm_noop);

SCM
scm_noop (args)
     SCM args;
{
  return (SCM_NULLP (args)
	  ? SCM_BOOL_F
	  : SCM_CAR (args));
}




SCM_PROC(s_set_tick_rate, "set-tick-rate", 1, 0, 0, scm_set_tick_rate);

SCM
scm_set_tick_rate (n)
     SCM n;
{
  unsigned int old_n;
  SCM_ASSERT (SCM_INUMP (n), n, SCM_ARG1, s_set_tick_rate);
  old_n = scm_tick_rate;
  scm_desired_tick_rate = SCM_INUM (n);
  scm_async_rate = 1 + scm_async_rate - scm_async_clock;
  scm_async_clock = 1;
  return SCM_MAKINUM (old_n);
}




SCM_PROC(s_set_switch_rate, "set-switch-rate", 1, 0, 0, scm_set_switch_rate);

SCM
scm_set_switch_rate (n)
     SCM n;
{
  unsigned int old_n;
  SCM_ASSERT (SCM_INUMP (n), n, SCM_ARG1, s_set_switch_rate);
  old_n = scm_switch_rate;
  scm_desired_switch_rate = SCM_INUM (n);
  scm_async_rate = 1 + scm_async_rate - scm_async_clock;
  scm_async_clock = 1;
  return SCM_MAKINUM (old_n);
}




static SCM scm_sys_hup_async_thunk SCM_P ((void));

static SCM
scm_sys_hup_async_thunk ()
{
  scm_deliver_signal (SCM_HUP_SIGNAL);
  return SCM_BOOL_F;
}


static SCM scm_sys_int_async_thunk SCM_P ((void));

static SCM
scm_sys_int_async_thunk ()
{
  scm_deliver_signal (SCM_INT_SIGNAL);
  return SCM_BOOL_F;
}


static SCM scm_sys_fpe_async_thunk SCM_P ((void));

static SCM
scm_sys_fpe_async_thunk ()
{
  scm_deliver_signal (SCM_FPE_SIGNAL);
  return SCM_BOOL_F;
}


static SCM scm_sys_bus_async_thunk SCM_P ((void));

static SCM
scm_sys_bus_async_thunk ()
{
  scm_deliver_signal (SCM_BUS_SIGNAL);
  return SCM_BOOL_F;
}


static SCM scm_sys_segv_async_thunk SCM_P ((void));

static SCM
scm_sys_segv_async_thunk ()
{
  scm_deliver_signal (SCM_SEGV_SIGNAL);
  return SCM_BOOL_F;
}


static SCM scm_sys_alrm_async_thunk SCM_P ((void));

static SCM
scm_sys_alrm_async_thunk ()
{
  scm_deliver_signal (SCM_ALRM_SIGNAL);
  return SCM_BOOL_F;
}


static SCM scm_sys_gc_async_thunk SCM_P ((void));

static SCM
scm_sys_gc_async_thunk ()
{
  scm_deliver_signal (SCM_GC_SIGNAL);
  return SCM_BOOL_F;
}


static SCM scm_sys_tick_async_thunk SCM_P ((void));

static SCM
scm_sys_tick_async_thunk ()
{
  scm_deliver_signal (SCM_TICK_SIGNAL);
  return SCM_BOOL_F;
}






SCM
scm_take_signal (n)
     int n;
{
  SCM ignored;
  if (!scm_ints_disabled)
    {
      /* For reasons of speed, the SCM_NEWCELL macro doesn't defer
	 interrupts.  Instead, it first sets its argument to point to
	 the first cell in the list, and then advances the freelist
	 pointer to the next cell.  Now, if this procedure is
	 interrupted, the only anomalous state possible is to have
	 both SCM_NEWCELL's argument and scm_freelist pointing to the
	 same cell.  To deal with this case, we always throw away the
	 first cell in scm_freelist here.

	 At least, that's the theory.  I'm not convinced that that's
	 the only anomalous path we need to worry about.  */
      SCM_NEWCELL (ignored);
    }
  scm_system_async_mark (system_signal_asyncs[SCM_SIG_ORD(n)]);
  return SCM_BOOL_F;
}



SCM_PROC(s_unmask_signals, "unmask-signals", 0, 0, 0, scm_unmask_signals);

SCM
scm_unmask_signals ()
{
  scm_mask_ints = 0;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_mask_signals, "mask-signals", 0, 0, 0, scm_mask_signals);

SCM
scm_mask_signals ()
{
  scm_mask_ints = 1;
  return SCM_UNSPECIFIED;
}




void
scm_init_async ()
{
  SCM a_thunk;
  scm_tc16_async = scm_newsmob (&async_smob);
  symbol_signal = SCM_CAR (scm_sysintern ("signal", SCM_UNDEFINED));
  scm_permanent_object (symbol_signal);

  /* These are in the opposite order of delivery priortity. 
   *
   * Error conditions are given low priority:
   */
  a_thunk = scm_make_gsubr ("%hup-thunk", 0, 0, 0, scm_sys_hup_async_thunk);
  system_signal_asyncs[SCM_SIG_ORD(SCM_HUP_SIGNAL)] = scm_system_async (a_thunk);
  a_thunk = scm_make_gsubr ("%int-thunk", 0, 0, 0, scm_sys_int_async_thunk);
  system_signal_asyncs[SCM_SIG_ORD(SCM_INT_SIGNAL)] = scm_system_async (a_thunk);
  a_thunk = scm_make_gsubr ("%fpe-thunk", 0, 0, 0, scm_sys_fpe_async_thunk);
  system_signal_asyncs[SCM_SIG_ORD(SCM_FPE_SIGNAL)] = scm_system_async (a_thunk);
  a_thunk = scm_make_gsubr ("%bus-thunk", 0, 0, 0, scm_sys_bus_async_thunk);
  system_signal_asyncs[SCM_SIG_ORD(SCM_BUS_SIGNAL)] = scm_system_async (a_thunk);
  a_thunk = scm_make_gsubr ("%segv-thunk", 0, 0, 0, scm_sys_segv_async_thunk);
  system_signal_asyncs[SCM_SIG_ORD(SCM_SEGV_SIGNAL)] = scm_system_async (a_thunk);


  a_thunk = scm_make_gsubr ("%gc-thunk", 0, 0, 0, scm_sys_gc_async_thunk);
  system_signal_asyncs[SCM_SIG_ORD(SCM_GC_SIGNAL)] = scm_system_async (a_thunk);

  /* Clock and PC driven conditions are given highest priority. */
  a_thunk = scm_make_gsubr ("%tick-thunk", 0, 0, 0, scm_sys_tick_async_thunk);
  system_signal_asyncs[SCM_SIG_ORD(SCM_TICK_SIGNAL)] = scm_system_async (a_thunk);
  a_thunk = scm_make_gsubr ("%alrm-thunk", 0, 0, 0, scm_sys_alrm_async_thunk);
  system_signal_asyncs[SCM_SIG_ORD(SCM_ALRM_SIGNAL)] = scm_system_async (a_thunk);

  handler_var = scm_sysintern ("signal-handler", SCM_UNDEFINED);
  SCM_SETCDR (handler_var, SCM_BOOL_F);
  scm_permanent_object (handler_var);
#include "async.x"
}

/*	Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
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


/* This file does some pretty hairy #inclusion.  It probably seemed
   like a good idea at the time, but it doesn't now.  Here's the
   structure, edited for relevance (!), last I checked:

      threads.c:
	threads.h
	  coop-defs.h
	    iselect.h
	mit-pthreads.c
	coop-threads.c
	  coop-threads.h
	    coop-defs.h*
	    ../qt/qt.h
	  coop.c
	    <qt.h>

    * second #inclusion
*/

#include <stdio.h>
#include "_scm.h"
#include "dynwind.h"
#include "smob.h"

#include "threads.h"



long scm_tc16_thread;

long scm_tc16_mutex;

long scm_tc16_condvar;


/* Scheme-visible thread functions. */

#ifdef USE_COOP_THREADS
SCM_PROC(s_single_thread_p, "single-active-thread?", 0, 0, 0, scm_single_thread_p);
#endif
SCM_PROC(s_yield, "yield", 0, 0, 0, scm_yield);
SCM_PROC(s_call_with_new_thread, "call-with-new-thread", 0, 0, 1, scm_call_with_new_thread);
SCM_PROC(s_join_thread, "join-thread", 1, 0, 0, scm_join_thread);
SCM_PROC(s_make_mutex, "make-mutex", 0, 0, 0, scm_make_mutex);
SCM_PROC(s_lock_mutex, "lock-mutex", 1, 0, 0, scm_lock_mutex);
SCM_PROC(s_unlock_mutex, "unlock-mutex", 1, 0, 0, scm_unlock_mutex);
SCM_PROC(s_make_condition_variable, "make-condition-variable", 0, 0, 0, scm_make_condition_variable);
SCM_PROC(s_wait_condition_variable, "wait-condition-variable", 2, 0, 0, scm_wait_condition_variable);
SCM_PROC(s_signal_condition_variable, "signal-condition-variable", 1, 0, 0, scm_signal_condition_variable);



#ifdef USE_MIT_PTHREADS
#include "mit-pthreads.c"
#endif

#ifdef USE_COOP_THREADS
#include "coop-threads.c"
#endif

static int
print_thread (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  scm_puts ("#<thread ", port);
  scm_intprint (SCM_CDR (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}

static scm_smobfuns thread_smob =
{
  0,
  scm_threads_free_thread,
  print_thread,
  0
};

static int
print_mutex (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  scm_puts ("#<mutex ", port);
  scm_intprint (SCM_CDR (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}

static scm_smobfuns mutex_smob =
{
  0,
  scm_threads_free_mutex,
  print_mutex,
  0
};

static int
print_condvar (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  scm_puts ("#<condition-variable ", port);
  scm_intprint (SCM_CDR (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}

static scm_smobfuns condvar_smob =
{
  0,
  scm_threads_free_condvar,
  print_condvar,
  0
};



#ifdef __STDC__
void
scm_init_threads (SCM_STACKITEM *i)
#else
void
scm_init_threads (i)
     SCM_STACKITEM *i;
#endif
{
  scm_tc16_thread = scm_newsmob (&thread_smob);
  scm_tc16_mutex = scm_newsmob (&mutex_smob);
  scm_tc16_condvar = scm_newsmob (&condvar_smob);
#include "threads.x"
  /* Initialize implementation specific details of the threads support */
  scm_threads_init (i);
}

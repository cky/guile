/*	Copyright (C) 1995, 1996, 1997, 1998, 2000, 2002 Free Software Foundation, Inc.
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
	coop-threads.c
	  coop-threads.h
	    coop-defs.h*
	    ../qt/qt.h
	  coop.c
	    <qt.h>

    * second #inclusion
*/

#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/dynwind.h"
#include "libguile/smob.h"

#include "libguile/threads.h"



scm_t_bits scm_tc16_thread;
scm_t_bits scm_tc16_mutex;
scm_t_bits scm_tc16_condvar;


/* Scheme-visible thread functions. */

#ifdef USE_COOP_THREADS
SCM_REGISTER_PROC(s_single_thread_p, "single-active-thread?", 0, 0, 0, scm_single_thread_p);
#endif

/* GJB:FIXME:DOC: SCM_REGISTER_PROC needs to permit a docstring,
   or these need to move into the file where the proc is defined. */

SCM_REGISTER_PROC(s_yield, "yield", 0, 0, 0, scm_yield);
/* If one or more threads are waiting to execute, calling yield forces an
immediate context switch to one of them. Otherwise, yield has no effect.
*/

SCM_REGISTER_PROC(s_call_with_new_thread, "call-with-new-thread", 0, 0, 1, scm_call_with_new_thread);
/* Evaluate @var{(thunk)} in a new thread, and new dynamic context,
returning a new thread object representing the thread.

If an error occurs during evaluation, call error-thunk, passing it an
error code describing the condition.  [Error codes are currently
meaningless integers.  In the future, real values will be specified.]
If this happens, the error-thunk is called outside the scope of the new
root -- it is called in the same dynamic context in which
with-new-thread was evaluated, but not in the callers thread.

All the evaluation rules for dynamic roots apply to threads.
*/

SCM_REGISTER_PROC(s_current_thread, "current-thread", 0, 0, 0, scm_current_thread);
SCM_REGISTER_PROC(s_all_thread, "all-threads", 0, 0, 0, scm_all_threads);

SCM_REGISTER_PROC(s_join_thread, "join-thread", 1, 0, 0, scm_join_thread);
/* Suspend execution of the calling thread until the target @var{thread}
terminates, unless the target @var{thread} has already terminated.
*/

SCM_DEFINE (scm_thread_exited_p, "thread-exited?", 1, 0, 0,
	    (SCM thread),
	    "Return @code{#t} iff @var{thread} has exited.\n")
#define FUNC_NAME s_scm_thread_exited_p
{
  return SCM_BOOL (scm_c_thread_exited_p (thread));
}
#undef FUNC_NAME

SCM_REGISTER_PROC(s_make_mutex, "make-mutex", 0, 0, 0, scm_make_mutex);
/* Create a new mutex object. */

SCM_REGISTER_PROC(s_lock_mutex, "lock-mutex", 1, 0, 0, scm_lock_mutex);
/* Lock @var{mutex}. If the mutex is already locked, the calling thread
blocks until the mutex becomes available. The function returns when
the calling thread owns the lock on @var{mutex}. */

SCM_REGISTER_PROC(s_try_mutex, "try-mutex", 1, 0, 0, scm_try_mutex);
/* Try to lock @var{mutex}. If the mutex is already locked by someone
else, return @code{#f}.  Else lock the mutex and return @code{#t}. */

SCM_REGISTER_PROC(s_unlock_mutex, "unlock-mutex", 1, 0, 0, scm_unlock_mutex);
/* Unlocks @var{mutex} if the calling thread owns the lock on @var{mutex}.
Calling unlock-mutex on a mutex not owned by the current thread results
in undefined behaviour. Once a mutex has been unlocked, one thread
blocked on @var{mutex} is awakened and grabs the mutex lock. */

SCM_REGISTER_PROC(s_make_condition_variable, "make-condition-variable", 0, 0, 0, scm_make_condition_variable);

SCM_REGISTER_PROC(s_wait_condition_variable, "wait-condition-variable", 2, 1, 0, scm_timed_wait_condition_variable);

SCM_REGISTER_PROC(s_signal_condition_variable, "signal-condition-variable", 1, 0, 0, scm_signal_condition_variable);

SCM_REGISTER_PROC(s_broadcast_condition_variable, "broadcast-condition-variable", 1, 0, 0, scm_broadcast_condition_variable);

SCM
scm_wait_condition_variable (SCM c, SCM m)
{
  return scm_timed_wait_condition_variable (c, m, SCM_UNDEFINED);
}



#ifdef USE_COOP_THREADS
#include "libguile/coop-threads.c"
#else
#ifdef USE_COPT_THREADS
#include "libguile/coop-pthreads.c"
#else
#include "libguile/null-threads.c"
#endif
#endif



#if (SCM_ENABLE_DEPRECATED == 1)

int
scm_mutex_init (scm_t_mutex *m)
{
  scm_gc_protect_object (m->m = scm_make_mutex ());
  return 0;
}

int
scm_mutex_lock (scm_t_mutex *m)
{
  scm_lock_mutex (m->m);
  return 0;
}

int
scm_mutex_trylock (scm_t_mutex *m)
{
  return SCM_FALSEP (scm_try_mutex (m->m))? EBUSY : 0;
}

int
scm_mutex_unlock (scm_t_mutex *m)
{
  scm_unlock_mutex (m->m);
  return 0;
}

int
scm_mutex_destroy (scm_t_mutex *m)
{
  scm_gc_unprotect_object (m->m);
  return 0;
}

SCM_API int
scm_cond_init (scm_t_cond *c, int *cattr)
{
  scm_gc_protect_object (c->c = scm_make_condition_variable ());
  return 0;
}

int
scm_cond_wait (scm_t_cond *c, scm_t_mutex *m)
{
  scm_wait_condition_variable (c->c, m->m);
  return 0;
}

int
scm_cond_timedwait (scm_t_cond *c, scm_t_mutex *m,
		    const struct timespec *t)
{
  return !SCM_FALSEP (scm_timed_wait_condition_variable (
    c->c, m->m, scm_cons (scm_long2num (t->tv_sec),
			  scm_long2num (t->tv_nsec/1000))));
}

int
scm_cond_signal (scm_t_cond *c)
{
  scm_signal_condition_variable (c->c);
  return 0;
}

int
scm_cond_broadcast (scm_t_cond *c)
{
  scm_broadcast_condition_variable (c->c);
  return 0;
}

int
scm_cond_destroy (scm_t_cond *c)
{
  scm_gc_unprotect_object (c->c);
  return 0;
}

#endif



void
scm_init_threads (SCM_STACKITEM *i)
{
  /* Initialize implementation specific details of the threads support */
  scm_threads_init (i);
}

void
scm_init_thread_procs ()
{
#include "libguile/threads.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* classes: h_files */

#ifndef SCM_THREADS_H
#define SCM_THREADS_H

/* Copyright (C) 1996,1997,1998,2000,2001, 2002 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"
#include "libguile/procs.h"
#include "libguile/throw.h"
#include "libguile/root.h"
#include "libguile/iselect.h"


/* smob tags for the thread datatypes */
SCM_API scm_t_bits scm_tc16_thread;
SCM_API scm_t_bits scm_tc16_mutex;
SCM_API scm_t_bits scm_tc16_condvar;

#define SCM_THREADP(x)      SCM_TYP16_PREDICATE (scm_tc16_thread, x)
#define SCM_THREAD_DATA(x)  ((void *) SCM_CELL_WORD_1 (x))

#define SCM_MUTEXP(x)       SCM_TYP16_PREDICATE (scm_tc16_mutex, x)
#define SCM_MUTEX_DATA(x)   ((void *) SCM_CELL_WORD_1 (x))

#define SCM_CONDVARP(x)     SCM_TYP16_PREDICATE (scm_tc16_condvar, x)
#define SCM_CONDVAR_DATA(x) ((void *) SCM_CELL_WORD_1 (x))

#define SCM_VALIDATE_THREAD(pos, a) \
 SCM_MAKE_VALIDATE_MSG (pos, a, THREADP, "thread")

#define SCM_VALIDATE_MUTEX(pos, a) \
 SCM_MAKE_VALIDATE_MSG (pos, a, MUTEXP, "mutex")

#define SCM_VALIDATE_CONDVAR(pos, a) \
 SCM_MAKE_VALIDATE_MSG (pos, a, CONDVARP, "condition variable")

SCM_API void scm_threads_mark_stacks (void);
SCM_API void scm_init_threads (SCM_STACKITEM *);
SCM_API void scm_init_thread_procs (void);

SCM_API SCM scm_spawn_thread (scm_t_catch_body body, void *body_data,
			      scm_t_catch_handler handler, void *handler_data);

/* These are versions of the ordinary sleep and usleep functions
   that play nicely with the thread system.  */
SCM_API unsigned long scm_thread_sleep (unsigned long);
SCM_API unsigned long scm_thread_usleep (unsigned long);

/* Critical sections */

/* Since only one thread can be active anyway, we don't need to do
   anything special around critical sections.  In fact, that's the
   reason we do only support cooperative threading: Guile's critical
   regions have not been completely identified yet.  (I think.) */

#define SCM_CRITICAL_SECTION_START 
#define SCM_CRITICAL_SECTION_END 

/* Switching */

SCM_API int scm_i_switch_counter;
#define SCM_I_THREAD_SWITCH_COUNT 50

#define SCM_THREAD_SWITCHING_CODE \
do { \
  scm_i_switch_counter--; \
  if (scm_i_switch_counter == 0) \
    { \
      scm_i_switch_counter = SCM_I_THREAD_SWITCH_COUNT; \
      scm_yield(); \
    } \
} while (0)

/* The C versions of the Scheme-visible thread functions.  */
SCM_API SCM scm_yield (void);
SCM_API SCM scm_call_with_new_thread (SCM thunk, SCM handler);
SCM_API SCM scm_join_thread (SCM t);
SCM_API SCM scm_make_mutex (void);
SCM_API SCM scm_lock_mutex (SCM m);
SCM_API SCM scm_try_mutex (SCM m);
SCM_API SCM scm_unlock_mutex (SCM m);
SCM_API SCM scm_make_condition_variable (void);
SCM_API SCM scm_wait_condition_variable (SCM cond, SCM mutex);
SCM_API SCM scm_timed_wait_condition_variable (SCM cond, SCM mutex,
					       SCM abstime);
SCM_API SCM scm_signal_condition_variable (SCM cond);
SCM_API SCM scm_broadcast_condition_variable (SCM cond);

SCM_API SCM scm_current_thread (void);
SCM_API SCM scm_all_threads (void);

SCM_API int scm_c_thread_exited_p (SCM thread);
SCM_API SCM scm_thread_exited_p (SCM thread);

SCM_API scm_root_state *scm_i_thread_root (SCM thread);

SCM_API void *scm_i_thread_data;
SCM_API void scm_i_set_thread_data (void *);
#define SCM_THREAD_LOCAL_DATA        scm_i_thread_data
#define SCM_SET_THREAD_LOCAL_DATA(x) scm_i_set_thread_data(x)

#ifndef HAVE_STRUCT_TIMESPEC
/* POSIX.4 structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  */
struct timespec
{
  long int tv_sec;		/* Seconds.  */
  long int tv_nsec;		/* Nanoseconds.  */
};
#endif

#ifdef USE_COPT_THREADS
#include "libguile/pthread-threads.h"
#else
#include "libguile/null-threads.h"
#endif

#endif  /* SCM_THREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

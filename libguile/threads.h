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
SCM_API scm_t_bits scm_tc16_fair_mutex;
SCM_API scm_t_bits scm_tc16_condvar;
SCM_API scm_t_bits scm_tc16_fair_condvar;

#define SCM_THREADP(x)        SCM_TYP16_PREDICATE (scm_tc16_thread, x)
#define SCM_THREAD_DATA(x)    ((scm_thread *) SCM_CELL_WORD_1 (x))

#define SCM_MUTEXP(x)         SCM_TYP16_PREDICATE (scm_tc16_mutex, x)
#define SCM_FAIR_MUTEX_P(x)   SCM_TYP16_PREDICATE (scm_tc16_fair_mutex, x)
#define SCM_MUTEX_DATA(x)     ((void *) SCM_CELL_WORD_1 (x))

#define SCM_CONDVARP(x)       SCM_TYP16_PREDICATE (scm_tc16_condvar, x)
#define SCM_FAIR_CONDVAR_P(x) SCM_TYP16_PREDICATE (scm_tc16_fair_condvar, x)
#define SCM_CONDVAR_DATA(x)   ((void *) SCM_CELL_WORD_1 (x))

#define SCM_VALIDATE_THREAD(pos, a) \
 SCM_MAKE_VALIDATE_MSG (pos, a, THREADP, "thread")

#define SCM_VALIDATE_MUTEX(pos, a) \
 SCM_ASSERT_TYPE (SCM_MUTEXP (a) || SCM_FAIR_MUTEX_P (a), \
                  a, pos, FUNC_NAME, "mutex");

#define SCM_VALIDATE_CONDVAR(pos, a) \
 SCM_ASSERT_TYPE (SCM_CONDVARP (a) || SCM_FAIR_CONDVAR_P (a), \
                  a, pos, FUNC_NAME, "condition variable");

SCM_API void scm_threads_mark_stacks (void);
SCM_API void scm_init_threads (SCM_STACKITEM *);
SCM_API void scm_init_thread_procs (void);

/*----------------------------------------------------------------------*/
/* Low-level C API */

/* The purpose of this API is seamless, simple and thread package
   independent interaction with Guile threads from the application.
 */

/* MDJ 021209 <djurfeldt@nada.kth.se>:
   The separation of the plugin interface (currently in
   pthread-threads.h and null-threads.h) and the low-level C API needs
   to be completed in a sensible way.
 */

/* Deprecate this name and rename to scm_thread_create?
   Introduce the other two arguments in pthread_create to prepare for
   the future?
 */
SCM_API SCM scm_spawn_thread (scm_t_catch_body body, void *body_data,
			      scm_t_catch_handler handler, void *handler_data);

#define scm_thread_join		scm_i_plugin_thread_join
#define scm_thread_detach	scm_i_plugin_thread_detach
#define scm_thread_self		scm_i_plugin_thread_self

#define scm_mutex_init		scm_i_plugin_mutex_init 
#define scm_mutex_destroy	scm_i_plugin_mutex_destroy
SCM_API int scm_mutex_lock (scm_t_mutex *m);
#define scm_mutex_trylock	scm_i_plugin_mutex_trylock 
#define scm_mutex_unlock	scm_i_plugin_mutex_unlock 

#define scm_cond_init		scm_i_plugin_cond_init 
#define scm_cond_destroy	scm_i_plugin_cond_destroy 
SCM_API int scm_cond_wait (scm_t_cond *c, scm_t_mutex *m);
SCM_API int scm_cond_timedwait (scm_t_cond *c,
				scm_t_mutex *m,
				const struct timespec *t);
#define scm_cond_signal		scm_i_plugin_cond_signal 
#define scm_cond_broadcast	scm_i_plugin_cond_broadcast 

#define scm_key_create		scm_i_plugin_key_create 
#define scm_key_delete		scm_i_plugin_key_delete 
#define scm_setspecific		scm_i_plugin_setspecific 
#define scm_getspecific		scm_i_plugin_getspecific 

#define scm_thread_select	scm_internal_select

/* The application must scm_leave_guile() before entering any piece of
   code which can
   1. block, or
   2. execute for any longer period of time without calling SCM_TICK

   Note, though, that it is *not* necessary to use these calls
   together with any call in this API.
 */

SCM_API void scm_enter_guile (void);
SCM_API void scm_leave_guile (void);

/* Better versions (although we need the former ones also in order to
   avoid forcing code restructuring in existing applications): */
/*fixme* Not implemented yet! */
SCM_API void *scm_in_guile (void (*func) (void*), void *data);
SCM_API void *scm_outside_guile (void (*func) (void*), void *data);

/* These are versions of the ordinary sleep and usleep functions
   that play nicely with the thread system.  */
SCM_API unsigned long scm_thread_sleep (unsigned long);
SCM_API unsigned long scm_thread_usleep (unsigned long);

/* End of low-level C API */
/*----------------------------------------------------------------------*/

typedef struct scm_thread scm_thread;

SCM_API void scm_i_enter_guile (scm_thread *t);
SCM_API scm_thread *scm_i_leave_guile (void);

/* Critical sections */

SCM_API scm_t_mutex scm_i_section_mutex;

/* This is the generic critical section for places where we are too
   lazy to allocate a specific mutex. */
SCM_DECLARE_NONREC_CRITICAL_SECTION (scm_i_critical_section);
#define SCM_CRITICAL_SECTION_START \
  SCM_NONREC_CRITICAL_SECTION_START (scm_i_critical_section)
#define SCM_CRITICAL_SECTION_END \
  SCM_NONREC_CRITICAL_SECTION_END (scm_i_critical_section)

/* This is the temporary support for the old ALLOW/DEFER ints sections */
SCM_DECLARE_REC_CRITICAL_SECTION (scm_i_defer);

extern int scm_i_thread_go_to_sleep;

void scm_i_thread_put_to_sleep (void);
void scm_i_thread_wake_up (void);
void scm_i_thread_invalidate_freelists (void);
void scm_i_thread_sleep_for_gc (void);
void scm_threads_prehistory (void);
void scm_threads_init_first_thread (void);

#define SCM_THREAD_SWITCHING_CODE \
do { \
  if (scm_i_thread_go_to_sleep) \
    scm_i_thread_sleep_for_gc (); \
} while (0)

/* The C versions of the Scheme-visible thread functions.  */
SCM_API SCM scm_call_with_new_thread (SCM thunk, SCM handler);
SCM_API SCM scm_join_thread (SCM t);
SCM_API SCM scm_make_mutex (void);
SCM_API SCM scm_make_fair_mutex (void);
SCM_API SCM scm_lock_mutex (SCM m);
SCM_API SCM scm_try_mutex (SCM m);
SCM_API SCM scm_unlock_mutex (SCM m);
SCM_API SCM scm_make_condition_variable (void);
SCM_API SCM scm_make_fair_condition_variable (void);
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

#define SCM_CURRENT_THREAD \
  ((scm_thread *) scm_i_plugin_getspecific (scm_i_thread_key))
extern scm_t_key scm_i_thread_key;

/* These macros have confusing names.
   They really refer to the root state of the running thread. */
#define SCM_THREAD_LOCAL_DATA (scm_i_plugin_getspecific (scm_i_root_state_key))
#define SCM_SET_THREAD_LOCAL_DATA(x) scm_i_set_thread_data(x)
extern scm_t_key scm_i_root_state_key;
SCM_API void scm_i_set_thread_data (void *);

#ifndef HAVE_STRUCT_TIMESPEC
/* POSIX.4 structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  */
struct timespec
{
  long int tv_sec;		/* Seconds.  */
  long int tv_nsec;		/* Nanoseconds.  */
};
#endif

#ifdef USE_PTHREAD_THREADS
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

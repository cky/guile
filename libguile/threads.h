/* classes: h_files */

#ifndef SCM_THREADS_H
#define SCM_THREADS_H

/* Copyright (C) 1996,1997,1998,2000,2001, 2002, 2003, 2004 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */



#include "libguile/__scm.h"
#include "libguile/procs.h"
#include "libguile/throw.h"
#include "libguile/root.h"
#include "libguile/iselect.h"

#include <pthread.h>



/* smob tags for the thread datatypes */
SCM_API scm_t_bits scm_tc16_thread;
SCM_API scm_t_bits scm_tc16_mutex;
SCM_API scm_t_bits scm_tc16_condvar;

typedef struct scm_thread {
  struct scm_thread *next_thread;

  /* For general blocking.
   */
  pthread_cond_t sleep_cond;

  /* This mutex represents this threads right to access the heap.
     That right can temporarily be taken away by the GC.  
  */
  pthread_mutex_t heap_mutex;
  SCM freelist, freelist2;
  int clear_freelists_p; /* set if GC was done while thread was asleep */
  
  SCM root;

  SCM handle;
  pthread_t pthread;
  SCM result;
  int exited;

  /* For keeping track of the stack and registers. */
  SCM_STACKITEM *base;
  SCM_STACKITEM *top;
  jmp_buf regs;

} scm_thread;

#define SCM_THREADP(x)        SCM_SMOB_PREDICATE (scm_tc16_thread, x)
#define SCM_THREAD_DATA(x)    ((scm_thread *) SCM_SMOB_DATA (x))

#define SCM_MUTEXP(x)         SCM_SMOB_PREDICATE (scm_tc16_mutex, x)
#define SCM_MUTEX_DATA(x)     ((void *) SCM_SMOB_DATA (x))

#define SCM_CONDVARP(x)       SCM_SMOB_PREDICATE (scm_tc16_condvar, x)
#define SCM_CONDVAR_DATA(x)   ((void *) SCM_SMOB_DATA (x))

#define SCM_VALIDATE_THREAD(pos, a) \
 SCM_MAKE_VALIDATE_MSG (pos, a, THREADP, "thread")

#define SCM_VALIDATE_MUTEX(pos, a) \
 SCM_ASSERT_TYPE (SCM_MUTEXP (a), \
                  a, pos, FUNC_NAME, "mutex");

#define SCM_VALIDATE_CONDVAR(pos, a) \
 SCM_ASSERT_TYPE (SCM_CONDVARP (a), \
                  a, pos, FUNC_NAME, "condition variable");

SCM_API SCM scm_spawn_thread (scm_t_catch_body body, void *body_data,
			      scm_t_catch_handler handler, void *handler_data);

/* The application must scm_leave_guile() before entering any piece of
   code which can block.
 */

SCM_API void scm_enter_guile (void);
SCM_API void scm_leave_guile (void);

SCM_API void *scm_with_guile (void *(*func)(void *), void *data);
SCM_API void *scm_without_guile (void *(*func)(void *), void *data);
SCM_API void *scm_i_with_guile_and_parent (void *(*func)(void *), void *data,
					   SCM parent);

/* Critical sections */

/* This is the generic critical section for places where we are too
   lazy to allocate a specific mutex. */
extern pthread_mutex_t scm_i_critical_section_mutex;

#define SCM_CRITICAL_SECTION_START \
  scm_pthread_mutex_lock (&scm_i_critical_section_mutex)
#define SCM_CRITICAL_SECTION_END \
  pthread_mutex_unlock (&scm_i_critical_section_mutex)

extern int scm_i_thread_go_to_sleep;

void scm_i_thread_put_to_sleep (void);
void scm_i_thread_wake_up (void);
void scm_i_thread_invalidate_freelists (void);
void scm_i_thread_sleep_for_gc (void);
void scm_threads_prehistory (SCM_STACKITEM *);
void scm_threads_init_first_thread (void);
SCM_API void scm_threads_mark_stacks (void);
SCM_API void scm_init_threads (void);
SCM_API void scm_init_thread_procs (void);
SCM_API void scm_init_threads_root_root (void);

#define SCM_THREAD_SWITCHING_CODE \
do { \
  if (scm_i_thread_go_to_sleep) \
    scm_i_thread_sleep_for_gc (); \
} while (0)

SCM scm_i_create_thread (scm_t_catch_body body, void *body_data,
			 scm_t_catch_handler handler, void *handler_data,
			 SCM protects);

/* The C versions of the Scheme-visible thread functions.  */
SCM_API SCM scm_call_with_new_thread (SCM thunk, SCM handler);
SCM_API SCM scm_yield (void);
SCM_API SCM scm_join_thread (SCM t);
SCM_API SCM scm_make_mutex (void);
SCM_API SCM scm_make_recursive_mutex (void);
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

#define SCM_CURRENT_THREAD \
  ((scm_thread *) pthread_getspecific (scm_i_thread_key))
SCM_API pthread_key_t scm_i_thread_key;

SCM_API pthread_mutex_t scm_i_misc_mutex;

/* Convenience functions for working with the pthread API in guile
   mode.
*/

SCM_API int scm_pthread_mutex_lock (pthread_mutex_t *mutex);
SCM_API void scm_frame_pthread_mutex_lock (pthread_mutex_t *mutex);
SCM_API int scm_pthread_cond_wait (pthread_cond_t *cond,
				   pthread_mutex_t *mutex);
SCM_API int scm_pthread_cond_timedwait (pthread_cond_t *cond,
					pthread_mutex_t *mutex,
					const struct timespec *abstime);
SCM_API unsigned long scm_thread_sleep (unsigned long);
SCM_API unsigned long scm_thread_usleep (unsigned long);

#endif  /* SCM_THREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

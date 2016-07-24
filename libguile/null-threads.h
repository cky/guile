/* classes: h_files */

#ifndef SCM_NULL_THREADS_H
#define SCM_NULL_THREADS_H

/* Copyright (C) 2005, 2006, 2010 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



/* The null-threads implementation.  We provide the subset of the
   standard pthread API that is used by Guile, but no new threads can
   be created.

   This file merely exits so that Guile can be compiled and run
   without using pthreads.  Improving performance via optimizations
   that are possible in a single-threaded program is not a primary
   goal.
*/

#include <stdlib.h>
#include <signal.h>
#include <errno.h>

/* Threads
*/
typedef int scm_i_pthread_t;
typedef void scm_i_pthread_attr_t;

static inline scm_i_pthread_t
scm_i_pthread_self (void)
{
  return 0;
}

static inline int
scm_i_pthread_create (scm_i_pthread_t *t, const scm_i_pthread_attr_t *attr,
                      void* (*f) (void*), void *arg)
{
  return ENOSYS;
}

static inline int
scm_i_pthread_detach (scm_i_pthread_t t)
{
  return 0;
}

static inline void
scm_i_pthread_exit (void *retval)
{
  exit (EXIT_SUCCESS);
}

static inline int
scm_i_pthread_cancel (scm_i_pthread_t t)
{
  return 0;
}

static inline int
scm_i_sched_yield (void)
{
  return 0;
}


/* Signals
 */
static inline int
scm_i_pthread_sigmask (int how, const sigset_t *set, sigset_t *oldset)
{
  return sigprocmask (how, set, oldset);
}

/* Mutexes
 */
typedef enum {
  SCM_I_PTHREAD_MUTEX_INITIALIZER = 0,
  SCM_I_PTHREAD_MUTEX_LOCKED = 1
} scm_i_pthread_mutex_t;
typedef int scm_i_pthread_mutexattr_t;

static inline int
scm_i_pthread_mutex_init (scm_i_pthread_mutex_t *m,
                          scm_i_pthread_mutexattr_t *attr)
{
  *m = SCM_I_PTHREAD_MUTEX_INITIALIZER;
  return 0;
}

static inline int
scm_i_pthread_mutex_destroy (scm_i_pthread_mutex_t *m)
{
  return 0;
}

static inline int
scm_i_pthread_mutex_trylock(scm_i_pthread_mutex_t *m)
{
  if (*m == SCM_I_PTHREAD_MUTEX_LOCKED)
    return EDEADLK;
  *m = SCM_I_PTHREAD_MUTEX_LOCKED;
  return 0;
}

static inline int
scm_i_pthread_mutex_lock (scm_i_pthread_mutex_t *m)
{
  *m = SCM_I_PTHREAD_MUTEX_LOCKED;
  return 0;
}

static inline int
scm_i_pthread_mutex_unlock (scm_i_pthread_mutex_t *m)
{
  *m = SCM_I_PTHREAD_MUTEX_INITIALIZER;
  return 0;
}

#define scm_i_pthread_mutexattr_recursive   0

/* Condition variables
 */
typedef enum {
  SCM_I_PTHREAD_COND_INITIALIZER = 0
} scm_i_pthread_cond_t;
typedef int scm_i_pthread_condattr_t;

static inline int
scm_i_pthread_cond_init (scm_i_pthread_cond_t *c,
                         scm_i_pthread_condattr_t *attr)
{
  *c = SCM_I_PTHREAD_COND_INITIALIZER;
  return 0;
}

static inline int
scm_i_pthread_cond_destroy (scm_i_pthread_cond_t *c)
{
  return 0;
}

static inline int
scm_i_pthread_cond_signal (scm_i_pthread_cond_t *c)
{
  return 0;
}

static inline int
scm_i_pthread_cond_broadcast (scm_i_pthread_cond_t *c)
{
  return 0;
}

static inline int
scm_i_pthread_cond_wait (scm_i_pthread_cond_t *c, scm_i_pthread_mutex_t *m)
{
  abort ();
  return 0;
}

static inline int
scm_i_pthread_cond_timedwait (scm_i_pthread_cond_t *c, scm_i_pthread_mutex_t *m,
                              const scm_t_timespec *t)
{
  abort();
  return 0;
}

/* Onces
 */
typedef enum {
  SCM_I_PTHREAD_ONCE_INIT = 0,
  SCM_I_PTHREAD_ONCE_ALREADY = 1
} scm_i_pthread_once_t;

static inline int
scm_i_pthread_once (scm_i_pthread_once_t *o, void(*init)(void))
{
  if (*o == SCM_I_PTHREAD_ONCE_INIT)
    {
      *o = SCM_I_PTHREAD_ONCE_ALREADY;
      init ();
    }
  return 0;
}

/* Thread specific storage
 */
typedef struct scm_i_pthread_key_t {
  struct scm_i_pthread_key_t *next;
  void *value;
  void (*destr_func) (void *);
} scm_i_pthread_key_t;

SCM_API int scm_i_pthread_key_create (scm_i_pthread_key_t *key,
				      void (*destr_func) (void *));
#define scm_i_pthread_setspecific(k,p)      ((k).value = (p))
#define scm_i_pthread_getspecific(k)        ((k).value)

/* Convenience functions
 */
#define scm_i_scm_pthread_mutex_lock        scm_i_pthread_mutex_lock
#define scm_i_dynwind_pthread_mutex_lock    scm_i_pthread_mutex_lock
#define scm_i_scm_pthread_cond_wait         scm_i_pthread_cond_wait
#define scm_i_scm_pthread_cond_timedwait    scm_i_pthread_cond_timedwait


#endif  /* SCM_NULL_THREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

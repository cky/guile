/* classes: h_files */

#ifndef SCM_THREADS_PLUGIN_H
#define SCM_THREADS_PLUGIN_H

/* Copyright (C) 1996,1997,1998,2000,2001, 2002 Free Software Foundation, Inc.
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


#include <pthread.h> /* This file should *not* need to include pthread.h */

/* Size is checked in scm_init_threads_plugin */
#define SCM_MUTEX_MAXSIZE (9 * sizeof (long))
typedef struct { char _[SCM_MUTEX_MAXSIZE]; } scm_t_mutex;

/*fixme* Should be defined similarly to scm_t_mutex. */
#define scm_t_mutexattr pthread_mutexattr_t

extern int scm_i_plugin_mutex_size;
typedef int (*scm_t_mutex_init) (scm_t_mutex *, const scm_t_mutexattr *);
typedef int (*scm_t_mutex_lock) (scm_t_mutex *);
typedef int (*scm_t_mutex_unlock) (scm_t_mutex *);
extern scm_t_mutex_init scm_i_plugin_mutex_init;
extern scm_t_mutex_lock scm_i_plugin_mutex_lock;
extern scm_t_mutex_unlock scm_i_plugin_mutex_unlock;

/* Size is checked in scm_init_threads_plugin */
#define SCM_REC_MUTEX_MAXSIZE (SCM_MUTEX_MAXSIZE + 3 * sizeof (long))
typedef struct { char _[SCM_REC_MUTEX_MAXSIZE]; } scm_t_rec_mutex;

extern int scm_i_plugin_rec_mutex_size;
typedef int (*scm_t_rec_mutex_init) (scm_t_rec_mutex *,
					   const scm_t_mutexattr *);
typedef int (*scm_t_rec_mutex_destroy) (scm_t_rec_mutex *);
typedef int (*scm_t_rec_mutex_lock) (scm_t_rec_mutex *);
typedef int (*scm_t_rec_mutex_trylock) (scm_t_rec_mutex *);
typedef int (*scm_t_rec_mutex_unlock) (scm_t_rec_mutex *);
extern scm_t_rec_mutex_init scm_i_plugin_rec_mutex_init;
extern scm_t_rec_mutex_destroy scm_i_plugin_rec_mutex_destroy;
extern scm_t_rec_mutex_lock scm_i_plugin_rec_mutex_lock;
extern scm_t_rec_mutex_trylock scm_i_plugin_rec_mutex_trylock;
extern scm_t_rec_mutex_unlock scm_i_plugin_rec_mutex_unlock;

/*fixme*/
#define scm_t_cond			pthread_cond_t

typedef int (*scm_t_cond_wait) (scm_t_cond *, scm_t_mutex *);
typedef int (*scm_t_cond_timedwait) (scm_t_cond *,
				     scm_t_mutex *,
				     const scm_t_timespec *);
extern scm_t_cond_wait scm_i_plugin_cond_wait;
extern scm_t_cond_timedwait scm_i_plugin_cond_timedwait;

extern void scm_init_threads_plugin (void);

#endif  /* SCM_THREADS_PLUGIN_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* classes: h_files */

#ifndef SCM_THREADS_PLUGIN_H
#define SCM_THREADS_PLUGIN_H

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
				     const struct timespec *);
extern scm_t_cond_wait scm_i_plugin_cond_wait;
extern scm_t_cond_timedwait scm_i_plugin_cond_timedwait;

extern void scm_init_threads_plugin (void);

#endif  /* SCM_THREADS_PLUGIN_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

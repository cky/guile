/* classes: h_files */

#ifndef SCM_THREADS_PTHREADS_H
#define SCM_THREADS_PTHREADS_H

/* Copyright (C) 2002 Free Software Foundation, Inc.
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



/* The pthreads-threads implementation.  This is a very simple mapping.
*/

#include <pthread.h>

#define scm_t_thread pthread_t

#define scm_thread_create(th,proc,data) \
  pthread_create ((th), NULL, (void *(*)(void *))(proc), (data))

#define scm_thread_join(th)   pthread_join (th, NULL)
#define scm_thread_detach(th) pthread_detach (th)
#define scm_thread_self()     pthread_self ()

#define scm_t_mutex pthread_mutex_t

#define scm_mutex_init(mx)    pthread_mutex_init (mx, NULL)
#define scm_mutex_destroy(mx) pthread_mutex_destroy (mx)
#define scm_mutex_lock(mx)    pthread_mutex_lock (mx)
#define scm_mutex_trylock(mx) pthread_mutex_trylock (mx)
#define scm_mutex_unlock(mx)  pthread_mutex_unlock (mx)

#define scm_t_cond pthread_cond_t

#define scm_cond_init(cv)     pthread_cond_init (cv, NULL)
#define scm_cond_destroy(cv)  pthread_cond_destroy (cv)
#define scm_cond_wait(cv,mx)  pthread_cond_wait (cv, mx)
#define scm_cond_timedwait(cv,mx,at) \
                              pthread_cond_timedwait (cv, mx, at)
#define scm_cond_signal(cv)   pthread_cond_signal (cv)
#define scm_cond_broadcast(cv) \
                              pthread_cond_broadcast (cv)

#define scm_t_key pthread_key_t

#define scm_key_create(keyp)  pthread_key_create (keyp, NULL)
#define scm_key_delete(key)   pthread_key_delete (key)
#define scm_key_setspecific(key, value) \
                              pthread_setspecific (key, value)
#define scm_key_getspecific(key) \
                              pthread_getspecific (key)

#define scm_thread_select    select

#endif  /* SCM_THREADS_NULL_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

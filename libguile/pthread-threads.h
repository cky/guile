/* classes: h_files */

#ifndef SCM_THREADS_PTHREADS_H
#define SCM_THREADS_PTHREADS_H

/* Copyright (C) 2002 Free Software Foundation, Inc.
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



/* The pthreads-threads implementation.  This is a direct mapping.
*/

/* This is an interface between Guile and the pthreads thread package. */

#include <pthread.h>
#include <sched.h>

#include "libguile/threads-plugin.h"

/* MDJ 021209 <djurfeldt@nada.kth.se>:
   The separation of the plugin interface and the low-level C API
   (currently in threads.h) needs to be completed in a sensible way.
 */

/* The scm_t_ types are temporarily used both in plugin and low-level API */
#define scm_t_thread			pthread_t

#define scm_i_plugin_thread_create	pthread_create

#define scm_i_plugin_thread_join	pthread_join 
#define scm_i_plugin_thread_detach	pthread_detach 
#define scm_i_plugin_thread_self	pthread_self
#define scm_i_plugin_thread_yield	sched_yield

extern scm_t_mutexattr scm_i_plugin_mutex; /* The "fast" mutex. */

#define scm_i_plugin_mutex_destroy(m) \
  pthread_mutex_destroy ((pthread_mutex_t *) (m))
#define scm_i_plugin_mutex_trylock(m) \
  pthread_mutex_trylock ((pthread_mutex_t *) (m))

extern scm_t_mutexattr scm_i_plugin_rec_mutex;

#define scm_i_plugin_cond_init		pthread_cond_init 
#define scm_i_plugin_cond_destroy	pthread_cond_destroy
#define scm_i_plugin_cond_signal	pthread_cond_signal 
#define scm_i_plugin_cond_broadcast	pthread_cond_broadcast 

#define scm_t_key			pthread_key_t

#define scm_i_plugin_key_create		pthread_key_create 
#define scm_i_plugin_key_delete		pthread_key_delete 
#define scm_i_plugin_setspecific	pthread_setspecific 
#define scm_i_plugin_getspecific	pthread_getspecific 

#define scm_i_plugin_select		select

#ifdef SCM_DEBUG_THREADS
void scm_i_assert_heap_locked (void);
#endif

void scm_init_pthread_threads (void);

#endif  /* SCM_THREADS_PTHREADS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

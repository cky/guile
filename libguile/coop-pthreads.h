/* classes: h_files */

#ifndef SCM_COOP_PTHREADS_H
#define SCM_COOP_PTHREADS_H

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



/* The coop-pthreads implementation.  We provide implement threads,
   mutices and condition variables using the pthread ones, but only
   one thread can ever be active inside Guile at any one time.
*/

#include <pthread.h>

/* Since only one thread can be active anyway, we don't need to do
   anything special around critical sections.  In fact, that's the
   reason we do only support cooperative threading: Guile's critical
   regions have not been completely identified yet.  (I think.) */

#define SCM_CRITICAL_SECTION_START 
#define SCM_CRITICAL_SECTION_END 

#define SCM_THREAD_SWITCH_COUNT       50

extern pthread_t guile_thread;                       /* for debugging */

#define SCM_THREAD_SWITCHING_CODE \
do { \
  if (guile_thread != pthread_self ()) \
    abort (); \
  scm_switch_counter--; \
  if (scm_switch_counter == 0) \
    { \
      scm_switch_counter = SCM_THREAD_SWITCH_COUNT; \
      scm_yield(); \
    } \
} while (0)

SCM_API int scm_switch_counter;

struct scm_copt_thread;

typedef struct scm_copt_thread {
  
  /* A condition variable for sleeping on.
   */
  pthread_cond_t block;
  
  scm_root_state *root;
  SCM handle;
  pthread_t pthread;
  SCM result;

} scm_copt_thread;

/* We implement our own mutex type since we want them to be 'fair', 
   we want to do fancy things while waiting for them (like running
   asyncs) and we want to support waiting on many things at once.
*/
typedef struct scm_copt_mutex {
  /* the mutex for this data structure. */
  pthread_mutex_t mutex;
  /* the thread currently owning the mutex, or NULL. */
  scm_copt_thread *owner;
  /* how much the owner owns us. */
  int level;
  /* the threads waiting for this mutex. */
  SCM waiting;
} scm_copt_mutex;

typedef scm_copt_mutex scm_t_mutex;

SCM_API void scm_copt_mutex_init (scm_copt_mutex *m);
SCM_API void scm_copt_mutex_lock (scm_copt_mutex *m);
SCM_API void scm_copt_mutex_unlock (scm_copt_mutex *m);
SCM_API void scm_copt_mutex_destroy (scm_copt_mutex *m);

#define scm_mutex_init    scm_copt_mutex_init
#define scm_mutex_lock    scm_copt_mutex_lock
#define scm_mutex_unlock  scm_copt_mutex_unlock
#define scm_mutex_destroy scm_copt_mutex_destroy

typedef pthread_cond_t scm_t_cond;

#define scm_cond_init(c)   pthread_cond_init ((c), NULL)
#define scm_cond_wait      pthread_cond_wait
#define scm_cond_signal    pthread_cond_signal
#define scm_cond_broadcast pthread_cond_broadcast
#define scm_cond_destroy   pthread_cond_destroy

#define SCM_THREAD_LOCAL_DATA          (scm_copt_thread_data ())
#define SCM_SET_THREAD_LOCAL_DATA(ptr) (scm_copt_set_thread_data (ptr))

SCM_API void *scm_copt_thread_data (void);
SCM_API void scm_copt_set_thread_data (void *);

#endif  /* SCM_COOP_PTHREAD_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

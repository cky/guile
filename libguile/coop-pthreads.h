/* classes: h_files */

#ifndef SCM_COOP_PTHREADS_H
#define SCM_COOP_PTHREADS_H

/* Copyright (C) 2002, 2006, 2008 Free Software Foundation, Inc.
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



/* The coop-pthreads implementation.  We use pthreads for the basic
   multi threading stuff, but rig it so that only one thread is ever
   active inside Guile.
*/

#include <pthread.h>

#include "libguile/iselect.h"

#if (SCM_ENABLE_DEPRECATED == 1)

/* Thread local data support --- generic C API */

typedef pthread_key_t scm_t_key;

#define scm_key_create pthread_key_create
#define scm_setspecific pthread_setspecific
#define scm_getspecific pthread_getspecific
#define scm_key_delete pthread_key_delete

#endif /* SCM_ENABLE_DEPRECATED == 1 */

/* Since only one thread can be active anyway, we don't need to do
   anything special around critical sections.  In fact, that's the
   reason we do only support cooperative threading: Guile's critical
   regions have not been completely identified yet.  (I think.) */

#define SCM_CRITICAL_SECTION_START 
#define SCM_CRITICAL_SECTION_END 

#define SCM_I_THREAD_SWITCH_COUNT       50

#define SCM_THREAD_SWITCHING_CODE \
do { \
  scm_i_switch_counter--; \
  if (scm_i_switch_counter == 0) \
    { \
      scm_i_switch_counter = SCM_I_THREAD_SWITCH_COUNT; \
      scm_yield(); \
    } \
} while (0)

SCM_API int scm_i_switch_counter;

#define SCM_THREAD_LOCAL_DATA          (scm_i_copt_thread_data)
#define SCM_SET_THREAD_LOCAL_DATA(ptr) (scm_i_copt_set_thread_data (ptr))

SCM_API void *scm_i_copt_thread_data;
SCM_INTERNAL void scm_i_copt_set_thread_data (void *data);

#endif  /* SCM_COOP_PTHREAD_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

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

#define SCM_THREAD_SWITCHING_CODE \
do { \
  scm_switch_counter--; \
  if (scm_switch_counter == 0) \
    { \
      scm_switch_counter = SCM_THREAD_SWITCH_COUNT; \
      scm_yield(); \
    } \
} while (0)

SCM_API int scm_switch_counter;

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

/* classes: h_files */

#ifndef MIT_PTHREADSH
#define MIT_PTHREADSH

/*	Copyright (C) 1996 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"

#define PTHREAD_KERNEL
#include <pthread.h>

/* Identify where the stack pointer can be found in a jmpbuf.
 */

/* Solaris 2.4 */
#if defined(__sparc_setjmp_h)
#  define THREAD_SP machdep_data.machdep_state[2]
#endif

/* Solaris 2.5 */
#if defined(__sparc)
#ifndef THREAD_SP
#  define THREAD_SP machdep_data.machdep_state[2]
#endif
#endif

#if defined(linux)
#  define THREAD_SP machdep_data.machdep_state[0].__sp
#endif

#if defined(sgi)
#  define THREAD_SP machdep_data.machdep_state[JB_SP]
#endif

/*  ...define THREAD_SP for your architecture here...
 */

#if !defined(THREAD_SP)
--> where is your stack pointer?
#endif



/* Boost the priority of this thread so that it is the only
   one running. PTHREAD_MAX_PRIORITY is reserved for this 
   purpose */

#define SCM_THREAD_CRITICAL_SECTION_START \
  struct sched_param param; \
  int previous_prio; \
  int policy; \
  pthread_getschedparam(pthread_self(), &policy, &param); \
  previous_prio = param.prio; \
  param.prio = PTHREAD_MAX_PRIORITY; \
  pthread_setschedparam(pthread_self(), policy, &param)

#define SCM_THREAD_CRITICAL_SECTION_END \
  param.prio = previous_prio; \
  pthread_setschedparam(pthread_self(), policy, &param)



#if 1

#define SCM_NO_CRITICAL_SECTION_OWNER 0

#define SCM_THREAD_DEFER pthread_kernel_lock++
#define SCM_THREAD_ALLOW pthread_kernel_lock--

#define SCM_THREAD_REDEFER pthread_kernel_lock++
#define SCM_THREAD_REALLOW_1 pthread_kernel_lock--
#define SCM_THREAD_REALLOW_2 \
do { \
  scm_critical_section_owner = SCM_NO_CRITICAL_SECTION_OWNER; \
  pthread_mutex_unlock(&scm_critical_section_mutex); \
} while (0)

#else

#define SCM_NO_CRITICAL_SECTION_OWNER 0

#define SCM_THREAD_DEFER \
do { \
  pthread_mutex_lock (&scm_critical_section_mutex); \
  scm_critical_section_owner = pthread_self(); \
} while (0) 

#define SCM_THREAD_ALLOW \
do { \
  scm_critical_section_owner = SCM_NO_CRITICAL_SECTION_OWNER; \
  pthread_mutex_unlock (&scm_critical_section_mutex); \
} while (0)

#define SCM_THREAD_REDEFER \
do { \
  if ((scm_critical_section_owner != pthread_self()) || \
      (scm_critical_section_owner == SCM_NO_CRITICAL_SECTION_OWNER)) \
    { \
      pthread_mutex_lock(&scm_critical_section_mutex); \
      scm_critical_section_owner = pthread_self(); \
    } \
} while (0)

#define SCM_THREAD_REALLOW_1
#define SCM_THREAD_REALLOW_2 \
do { \
  scm_critical_section_owner = SCM_NO_CRITICAL_SECTION_OWNER; \
  pthread_mutex_unlock (&scm_critical_section_mutex); \
} while (0)

#endif

#define SCM_THREAD_SWITCHING_CODE

#define SCM_THREAD_LOCAL_DATA (pthread_self () -> attr.arg_attr)
#define SCM_SET_THREAD_LOCAL_DATA(new_root) \
do { \
  pthread_t t = pthread_self (); \
  void *r = (new_root); \
  pthread_attr_setcleanup (&t -> attr, NULL, r); \
  pthreads_find_info (t) -> root = r; \
} while (0)




void scm_threads_init_mit_pthreads ();

typedef struct QUEUE {
  struct QUEUE *flink, *blink;
} queue;

extern pthread_mutex_t scm_critical_section_mutex;
extern pthread_t scm_critical_section_owner;

/* Key to thread specific data */
extern pthread_key_t info_key;

struct scm_pthread_create_info_type
{
  SCM thunk;
  SCM error;
  SCM *prots;
} scm_pthread_create_info;

#endif /* MIT_PTHREADSH */

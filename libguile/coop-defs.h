/* classes: h_files */

#ifndef COOP_DEFSH
#define COOP_DEFSH

/*	Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.
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


# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   ifdef HAVE_TIME_H
#    include <time.h>
#   endif
#  endif
# endif

#ifdef GUILE_ISELECT
#include "iselect.h"
#endif

/* This file is included by threads.h, which, in turn, is included by
   libguile.h while coop-threads.h only is included by
   coop-threads.c. */

/* The coop_t struct must be declared here, since macros in this file
   refer to the data member. */

/* The notion of a thread is merged with the notion of a queue.
   Thread stuff: thread status (sp) and stuff to use during
   (re)initialization.  Queue stuff: next thread in the queue
   (next). */

struct qt_t;

typedef struct coop_t {
  struct qt_t *sp;       /* QuickThreads handle. */
  void *sto;             /* `malloc'-allocated stack. */

  struct coop_t *next;    /* Next thread in the queue. */

  struct coop_t *all_next;    
  struct coop_t *all_prev;    

  void *data;            /* Thread local data */
  
  void *base;            /* Base of stack */
  void *top;             /* Top of stack */

  void *joining;         /* A queue of threads waiting to join this
			    thread */

#ifdef GUILE_ISELECT
  int nfds;
  SELECT_TYPE *readfds;
  SELECT_TYPE *writefds;
  SELECT_TYPE *exceptfds;
  int timeoutp;
  struct timeval wakeup_time;	/* Time to stop sleeping */
  int _errno;
  int retval;
#else
  time_t wakeup_time;    /* Time to stop sleeping */
#endif

} coop_t;

/* A queue is a circular list of threads.  The queue head is a
   designated list element.  If this is a uniprocessor-only
   implementation we can store the `main' thread in this, but in a
   multiprocessor there are several `heavy' threads but only one run
   queue.  A fancier implementation might have private run queues,
   which would lead to a simpler (trivial) implementation */

typedef struct coop_q_t {
  coop_t t;
  coop_t *tail;
} coop_q_t;

/* A Mutex variable is made up of a owner thread, and a queue of threads
   waiting on the mutex */

typedef struct coop_m {
  coop_t *owner;          /* Mutex owner */
  coop_q_t waiting;      /* Queue of waiting threads */
} coop_m;

typedef coop_m scm_mutex_t;

extern int coop_mutex_init (coop_m*);
extern int coop_mutex_lock (coop_m*);
extern int coop_mutex_unlock (coop_m*);
extern int coop_mutex_destroy (coop_m*);
#define scm_mutex_init coop_mutex_init
#define scm_mutex_lock coop_mutex_lock
#define scm_mutex_unlock coop_mutex_unlock
#define scm_mutex_destroy coop_mutex_destroy

/* A Condition variable is made up of a list of threads waiting on the
   condition. */

typedef struct coop_c {
  coop_q_t waiting;      /* Queue of waiting threads */
} coop_c;

typedef coop_c scm_cond_t;

extern int coop_condition_variable_init (coop_c*);
extern int coop_condition_variable_wait (coop_c*);
extern int coop_condition_variable_wait_mutex (coop_c*, coop_m*);
extern int coop_condition_variable_signal (coop_c*);
extern int coop_condition_variable_destroy (coop_c*);
#define scm_cond_init(cond, attr) coop_condition_variable_init (cond)
#define scm_cond_wait coop_condition_variable_wait_mutex
#define scm_cond_signal coop_condition_variable_signal
#define scm_cond_destroy coop_condition_variable_destroy

extern coop_t *coop_global_curr;       	/* Currently-executing thread. */

extern void coop_join (coop_t *t);
extern void coop_yield (void);

extern size_t scm_switch_counter;
extern size_t scm_thread_count;


/* Some iselect functions.  */ 

/* I'm not sure whether these three declarations should be here.
   They're really defined in iselect.c, so you'd think they'd go in
   iselect.h, but they use coop_t, defined above, which uses things
   defined in iselect.h.  Basically, we're making at best a flailing
   (and failing) attempt at modularity here, and I don't have time to
   rethink this at the moment.  This code awaits a Hero.  --JimB */
extern coop_t *coop_next_runnable_thread (void);
extern coop_t *coop_wait_for_runnable_thread_now (struct timeval *);
extern coop_t *coop_wait_for_runnable_thread (void);




/* Cooperative threads don't need to have these defined */

#define SCM_THREAD_CRITICAL_SECTION_START 
#define SCM_THREAD_CRITICAL_SECTION_END 



#define SCM_NO_CRITICAL_SECTION_OWNER 0
#define SCM_THREAD_SWITCH_COUNT       50 /* was 10 /mdj */



#define SCM_THREAD_DEFER
#define SCM_THREAD_ALLOW
#define SCM_THREAD_REDEFER
#define SCM_THREAD_REALLOW_1
#define SCM_THREAD_REALLOW_2

#if 0
#define SCM_THREAD_SWITCHING_CODE \
{ \
  if (scm_thread_count > 1) \
    coop_yield(); \
} \

#else
#define SCM_THREAD_SWITCHING_CODE \
{ \
  if (scm_thread_count > 1) \
  { \
    scm_switch_counter--; \
    if (scm_switch_counter == 0) \
      { \
        scm_switch_counter = SCM_THREAD_SWITCH_COUNT; \
        coop_yield(); \
      } \
  } \
} \

#endif

#define SCM_THREAD_LOCAL_DATA (coop_global_curr->data)
#define SCM_SET_THREAD_LOCAL_DATA(ptr) (coop_global_curr->data = (ptr))

#endif /* COOP_DEFSH */

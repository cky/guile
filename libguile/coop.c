/*	Copyright (C) 1995, 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
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


/* $Id: coop.c,v 1.17 2000-03-12 00:33:56 mdj Exp $ */

/* Cooperative thread library, based on QuickThreads */

#ifdef HAVE_UNISTD_H 
#include <unistd.h>
#endif

#include <errno.h>

#include <qt.h>
#include "eval.h"

/* #define COOP_STKSIZE (0x10000) */
#define COOP_STKSIZE (scm_eval_stack)

/* `alignment' must be a power of 2. */
#define COOP_STKALIGN(sp, alignment) \
((void *)((((qt_word_t)(sp)) + (alignment) - 1) & ~((alignment)-1)))



/* Queue access functions. */

static void
coop_qinit (coop_q_t *q)
{
  q->t.next = q->tail = &q->t;

  q->t.all_prev = NULL;
  q->t.all_next = NULL;
#ifdef GUILE_ISELECT
  q->t.nfds = 0;
  q->t.readfds = NULL;
  q->t.writefds = NULL;
  q->t.exceptfds = NULL;
  q->t.timeoutp = 0;
#endif
}


coop_t *
coop_qget (coop_q_t *q)
{
  coop_t *t;

  t = q->t.next;
  q->t.next = t->next;
  if (t->next == &q->t) {
    if (t == &q->t) {		/* If it was already empty .. */
      return (NULL);		/* .. say so. */
    }
    q->tail = &q->t;		/* Else now it is empty. */
  }
  return (t);
}


void
coop_qput (coop_q_t *q, coop_t *t)
{
  q->tail->next = t;
  t->next = &q->t;
  q->tail = t;
}

static void
coop_all_qput (coop_q_t *q, coop_t *t)
{
  if (q->t.all_next)
    q->t.all_next->all_prev = t;
  t->all_prev = NULL;
  t->all_next = q->t.all_next;
  q->t.all_next = t;
}

static void
coop_all_qremove (coop_q_t *q, coop_t *t)
{
  if (t->all_prev)
    t->all_prev->all_next = t->all_next;
  else
    q->t.all_next = t->all_next;
  if (t->all_next)
      t->all_next->all_prev = t->all_prev;
}

#ifdef GUILE_ISELECT
/* Insert thread t into the ordered queue q.
   q is ordered after wakeup_time.  Threads which aren't sleeping but
   waiting for I/O go last into the queue. */
void
coop_timeout_qinsert (coop_q_t *q, coop_t *t)
{
  coop_t *pred = &q->t;
  int sec = t->wakeup_time.tv_sec;
  int usec = t->wakeup_time.tv_usec;
  while (pred->next != &q->t
	 && pred->next->timeoutp
	 && (pred->next->wakeup_time.tv_sec < sec
	     || (pred->next->wakeup_time.tv_sec == sec
		 && pred->next->wakeup_time.tv_usec < usec)))
    pred = pred->next;
  t->next = pred->next;
  pred->next = t;
  if (t->next == &q->t)
    q->tail = t;
}
#endif


/* Thread routines. */

coop_q_t coop_global_runq;	/* A queue of runable threads. */
coop_q_t coop_global_sleepq;	/* A queue of sleeping threads. */
coop_q_t coop_tmp_queue;        /* A temp working queue */
coop_q_t coop_global_allq;      /* A queue of all threads. */
static coop_t coop_global_main; /* Thread for the process. */
coop_t *coop_global_curr;	/* Currently-executing thread. */

static void *coop_starthelp (qt_t *old, void *ignore0, void *ignore1);
static void coop_only (void *pu, void *pt, qt_userf_t *f);
static void *coop_aborthelp (qt_t *sp, void *old, void *null);
static void *coop_yieldhelp (qt_t *sp, void *old, void *blockq);


void
coop_init()
{
  coop_qinit (&coop_global_runq);
  coop_qinit (&coop_global_sleepq);
  coop_qinit (&coop_tmp_queue);
  coop_qinit (&coop_global_allq);
  coop_global_curr = &coop_global_main;
}


/* Return the next runnable thread. If no threads are currently runnable,
   and there are sleeping threads - wait until one wakes up. Otherwise,
   return NULL. */

#ifndef GUILE_ISELECT
coop_t *
coop_next_runnable_thread()
{
  int sleepers;
  coop_t *t;
  time_t now;

  do {
    sleepers = 0;
    now = time(NULL);

    /* Check the sleeping queue */
    while ((t = coop_qget(&coop_global_sleepq)) != NULL)
      {
	sleepers++;
	if (t->wakeup_time <= now)
	  coop_qput(&coop_global_runq, t);
	else
	  coop_qput(&coop_tmp_queue, t);
      }
    while ((t = coop_qget(&coop_tmp_queue)) != NULL)
      coop_qput(&coop_global_sleepq, t);
    
    t = coop_qget (&coop_global_runq);

  } while ((t == NULL) && (sleepers > 0));

  return t;
}
#endif

void
coop_start()
{
  coop_t *next;

  while ((next = coop_qget (&coop_global_runq)) != NULL) {
    coop_global_curr = next;
    QT_BLOCK (coop_starthelp, 0, 0, next->sp);
  }
}


static void *
coop_starthelp (qt_t *old, void *ignore0, void *ignore1)
{
  coop_global_main.sp = old;
  coop_global_main.joining = NULL;
  coop_qput (&coop_global_runq, &coop_global_main);
  return NULL; /* not used, but keeps compiler happy */
}

int
coop_mutex_init (coop_m *m)
{
  return coop_new_mutex_init (m, NULL);
}

int
coop_new_mutex_init (coop_m *m, coop_mattr *attr)
{
  m->owner = NULL;
  coop_qinit(&(m->waiting));
  return 0;
}

int
coop_mutex_trylock (coop_m *m)
{
  if (m->owner == NULL)
    {
      m->owner = coop_global_curr;
      return 0;
    }
  else
    return EBUSY;
}

int
coop_mutex_lock (coop_m *m)
{
  if (m->owner == NULL)
    {
      m->owner = coop_global_curr;
    }
  else
    {
      coop_t *old, *newthread;

      /* Record the current top-of-stack before going to sleep */
      coop_global_curr->top = &old;

#ifdef GUILE_ISELECT
      newthread = coop_wait_for_runnable_thread();
      if (newthread == coop_global_curr)
	coop_abort ();
#else
      newthread = coop_next_runnable_thread();
#endif
      old = coop_global_curr;
      coop_global_curr = newthread;
      QT_BLOCK (coop_yieldhelp, old, &(m->waiting), newthread->sp);
    }
  return 0;
}


int 
coop_mutex_unlock (coop_m *m)
{
  coop_t *old, *newthread;
  
  newthread = coop_qget (&(m->waiting));
  if (newthread != NULL)
    {
      /* Record the current top-of-stack before going to sleep */
      coop_global_curr->top = &old;

      old = coop_global_curr;
      coop_global_curr = newthread;
      /* The new thread came into m->waiting through a lock operation.
	 It now owns this mutex. */
      m->owner = coop_global_curr;
      QT_BLOCK (coop_yieldhelp, old, &coop_global_runq, newthread->sp);
    }
  else
    {
      m->owner = NULL;
    }
  return 0;
}


int 
coop_mutex_destroy (coop_m *m)
{
  return 0;
}


int 
coop_condition_variable_init (coop_c *c)
{
  return coop_new_condition_variable_init (c, NULL);
}

int
coop_new_condition_variable_init (coop_c *c, coop_cattr *a)
{
  coop_qinit(&(c->waiting));
  return 0;
}

int 
coop_condition_variable_wait_mutex (coop_c *c, coop_m *m)
{
  coop_t *old, *newthread;

  /* coop_mutex_unlock (m); */
  newthread = coop_qget (&(m->waiting));
  if (newthread != NULL)
    {
      m->owner = newthread;
    }
  else
    {
      m->owner = NULL;
      /*fixme* Should we really wait here?  Isn't it OK just to proceed? */
#ifdef GUILE_ISELECT
      newthread = coop_wait_for_runnable_thread();
      if (newthread == coop_global_curr)
	coop_abort ();
#else
      newthread = coop_next_runnable_thread();
#endif
    }
  coop_global_curr->top = &old;
  old = coop_global_curr;
  coop_global_curr = newthread;
  QT_BLOCK (coop_yieldhelp, old, &(c->waiting), newthread->sp);

  coop_mutex_lock (m);
  return 0;
}

int 
coop_condition_variable_timed_wait_mutex (coop_c *c,
					  coop_m *m,
					  const struct timespec *abstime)
{
  coop_t *old, *t;
  int res = ETIME;

  /* coop_mutex_unlock (m); */
  t = coop_qget (&(m->waiting));
  if (t != NULL)
    {
      m->owner = t;
    }
  else
    {
      m->owner = NULL;
#ifdef GUILE_ISELECT
      coop_global_curr->timeoutp = 1;
      coop_global_curr->wakeup_time.tv_sec = abstime->tv_sec;
      coop_global_curr->wakeup_time.tv_usec = abstime->tv_nsec / 1000;
      coop_timeout_qinsert (&coop_global_sleepq, coop_global_curr);
      t = coop_wait_for_runnable_thread();
#else
      /*fixme* Implement!*/
      t = coop_next_runnable_thread();
#endif
    }
  if (t != coop_global_curr)
    {
      coop_global_curr->top = &old;
      old = coop_global_curr;
      coop_global_curr = t;
      QT_BLOCK (coop_yieldhelp, old, &(c->waiting), t->sp);

      /* Are we still in the sleep queue? */
      old = &coop_global_sleepq.t;
      for (t = old->next; t != &coop_global_sleepq.t; old = t, t = t->next)
	if (t == coop_global_curr)
	  {
	    old->next = t->next; /* unlink */
	    res = 0;
	    break;
	  }
    }
  coop_mutex_lock (m);
  return res;
}

int 
coop_condition_variable_signal (coop_c *c)
{
  coop_t *newthread;

  while ((newthread = coop_qget (&(c->waiting))) != NULL)
    {
      coop_qput (&coop_global_runq, newthread);
    }
  return 0;
}

/* {Keys}
 */

static int n_keys = 0;
static int max_keys = 0;
static void (**destructors) (void *) = 0;

int
coop_key_create (coop_k *keyp, void (*destructor) (void *value))
{
  if (n_keys >= max_keys)
    {
      int i;
      max_keys = max_keys ? max_keys * 3 / 2 : 10;
      destructors = realloc (destructors, sizeof (void *) * max_keys);
      if (destructors == 0)
	{
	  fprintf (stderr, "Virtual memory exceeded in coop_key_create\n");
	  exit (1);
	}
      for (i = n_keys; i < max_keys; ++i)
	destructors[i] = NULL;
    }
  destructors[n_keys] = destructor;
  *keyp = n_keys++;
  return 0;
}

int
coop_setspecific (coop_k key, const void *value)
{
  int n_keys = coop_global_curr->n_keys;
  if (key >= n_keys)
    {
      int i;
      coop_global_curr->n_keys = max_keys;
      coop_global_curr->specific = realloc (n_keys
					    ? coop_global_curr->specific
					    : NULL,
					    sizeof (void *) * max_keys);
      if (coop_global_curr->specific == 0)
	{
	  fprintf (stderr, "Virtual memory exceeded in coop_setspecific\n");
	  exit (1);
	}
      for (i = n_keys; i < max_keys; ++i)
	coop_global_curr->specific[i] = NULL;
    }
  coop_global_curr->specific[key] = (void *) value;
  return 0;
}

void *
coop_getspecific (coop_k key)
{
  return (key < coop_global_curr->n_keys
	  ? coop_global_curr->specific[key]
	  : NULL);
}

int
coop_key_delete (coop_k key)
{
  return 0;
}


int 
coop_condition_variable_destroy (coop_c *c)
{
  return 0;
}

#ifdef GUILE_PTHREAD_COMPAT
static void *
dummy_start (void *coop_thread)
{
  coop_t *t = (coop_t *) coop_thread;
  t->sto = &t + 1;
  pthread_mutex_init (&t->dummy_mutex, NULL);
  pthread_mutex_lock (&t->dummy_mutex);
  pthread_cond_init (&t->dummy_cond, NULL);
  pthread_cond_wait (&t->dummy_cond, &t->dummy_mutex);
  return 0;
}
#endif

coop_t *
coop_create (coop_userf_t *f, void *pu)
{
  coop_t *t;
  void *sto;

  t = malloc (sizeof(coop_t));

  t->data = NULL;
  t->n_keys = 0;
#ifdef GUILE_PTHREAD_COMPAT
  pthread_create (&t->dummy_thread, NULL, dummy_start, t);
#else
  t->sto = malloc (COOP_STKSIZE);
#endif
  sto = COOP_STKALIGN (t->sto, QT_STKALIGN);
  t->sp = QT_SP (sto, COOP_STKSIZE - QT_STKALIGN);
  t->base = t->sp;
  t->sp = QT_ARGS (t->sp, pu, t, (qt_userf_t *)f, coop_only);
  t->joining = NULL;
  coop_qput (&coop_global_runq, t);
  coop_all_qput (&coop_global_allq, t);

  return t;
}


static void
coop_only (void *pu, void *pt, qt_userf_t *f)
{
  coop_global_curr = (coop_t *)pt;
  (*(coop_userf_t *)f)(pu);
  coop_abort();
  /* NOTREACHED */
}


void
coop_abort ()
{
  coop_t *old, *newthread;

  /* Wake up any threads that are waiting to join this one */
  if (coop_global_curr->joining)
    {
      while ((newthread = coop_qget ((coop_q_t *)(coop_global_curr->joining)))
	     != NULL)
	{
	  coop_qput (&coop_global_runq, newthread);
	}
      free(coop_global_curr->joining);
    }

#ifdef GUILE_ISELECT
  scm_I_am_dead = 1;
  do {
    newthread = coop_wait_for_runnable_thread();
  } while (newthread == coop_global_curr);
  scm_I_am_dead = 0;
#else
  newthread = coop_next_runnable_thread();
#endif
  coop_all_qremove(&coop_global_allq, coop_global_curr);
  old = coop_global_curr;
  coop_global_curr = newthread;
#ifdef GUILE_PTHREAD_COMPAT
  pthread_cond_signal (&old->dummy_cond);
#endif
  QT_ABORT (coop_aborthelp, old, (void *)NULL, newthread->sp);
}


static void *
coop_aborthelp (qt_t *sp, void *old, void *null)
{
  coop_t *oldthread = (coop_t *) old;

  free (oldthread->sto);

  /* "old" is freed in scm_threads_thread_die().
     Marking old->base NULL indicates that this thread is dead */

  oldthread->base = NULL;

  return NULL;
}


void 
coop_join(coop_t *t)
{
  coop_t *old, *newthread;
  
  /* Check if t is already finished */
  if (t->base == NULL)
    return;

  /* Create a join list if necessary */
  if (t->joining == NULL)
    {
      t->joining = malloc(sizeof(coop_q_t));
      coop_qinit((coop_q_t *) t->joining);
    }

#ifdef GUILE_ISELECT
  newthread = coop_wait_for_runnable_thread();
  if (newthread == coop_global_curr)
    return;
#else
  newthread = coop_next_runnable_thread();
#endif
  old = coop_global_curr;
  coop_global_curr = newthread;
  QT_BLOCK (coop_yieldhelp, old, (coop_q_t *) t->joining, newthread->sp);
}

void
coop_yield()
{
  coop_t *old = NULL;
  coop_t *newthread;

  newthread = coop_next_runnable_thread();

  /* There may be no other runnable threads. Return if this is the 
     case. */
#if GUILE_ISELECT
  if (newthread == coop_global_curr)
      return;
#else
  if (newthread == NULL)
      return;
#endif

  old = coop_global_curr;

  coop_global_curr = newthread;
  QT_BLOCK (coop_yieldhelp, old, &coop_global_runq, newthread->sp);
}


static void *
coop_yieldhelp (qt_t *sp, void *old, void *blockq)
{
  ((coop_t *)old)->sp = sp;
  coop_qput ((coop_q_t *)blockq, (coop_t *)old);
  return NULL;
}

/* Replacement for the system's sleep() function. Does the right thing
   for the process - but not for the system (it busy-waits) */

void *
coop_sleephelp (qt_t *sp, void *old, void *blockq)
{
  ((coop_t *)old)->sp = sp;
  /* old is already on the sleep queue - so there's no need to
     do anything extra here */
  return NULL;
}

#ifdef GUILE_ISELECT

unsigned long 
scm_thread_usleep (unsigned long usec)
{
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = usec;
  scm_internal_select (0, NULL, NULL, NULL, &timeout);
  return 0;  /* Maybe we should calculate actual time slept,
		but this is faster... :) */
}

unsigned long
scm_thread_sleep (unsigned long sec)
{
  time_t now = time (NULL);
  struct timeval timeout;
  unsigned long slept;
  timeout.tv_sec = sec;
  timeout.tv_usec = 0;
  scm_internal_select (0, NULL, NULL, NULL, &timeout);
  slept = time (NULL) - now;
  return slept > sec ? 0 : sec - slept;
}

#else /* GUILE_ISELECT */

unsigned long
scm_thread_sleep (unsigned long s)
{
  coop_t *newthread, *old;
  time_t now = time (NULL);
  coop_global_curr->wakeup_time = now + s;

  /* Put the current thread on the sleep queue */
  coop_qput (&coop_global_sleepq, coop_global_curr);

  newthread = coop_next_runnable_thread();

  /* If newthread is the same as the sleeping thread, do nothing */
  if (newthread == coop_global_curr)
    return s;

  old = coop_global_curr;

  coop_global_curr = newthread;
  QT_BLOCK (coop_sleephelp, old, NULL, newthread->sp);

  return s;
}

unsigned long 
scm_thread_usleep (unsigned long usec)
{
  /* We're so cheap.  */
  scm_thread_sleep (usec / 1000000);
  struct timeval timeout;
  return 0;  /* Maybe we should calculate actual time slept,
		but this is faster... :) */
}

#endif /* GUILE_ISELECT */

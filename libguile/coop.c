/*	Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
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


/* $Id: coop.c,v 1.8 1998-04-12 23:34:40 mdj Exp $ */

/* Cooperative thread library, based on QuickThreads */

#include <qt.h>
#include "eval.h"

/* #define COOP_STKSIZE (0x10000) */
#define COOP_STKSIZE (scm_eval_stack)

/* `alignment' must be a power of 2. */
#define COOP_STKALIGN(sp, alignment) \
((void *)((((qt_word_t)(sp)) + (alignment) - 1) & ~((alignment)-1)))



/* Queue access functions. */

#ifdef __STDC__
static void
coop_qinit (coop_q_t *q)
#else
static void
coop_qinit (q)
     coop_q_t *q;
#endif
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


#ifdef __STDC__
coop_t *
coop_qget (coop_q_t *q)
#else
coop_t *
coop_qget (q)
     coop_q_t *q;
#endif
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


#ifdef __STDC__
void
coop_qput (coop_q_t *q, coop_t *t)
#else
void
coop_qput (q, t)
     coop_q_t *q;
     coop_t *t;
#endif
{
  q->tail->next = t;
  t->next = &q->t;
  q->tail = t;
}

#ifdef __STDC__
static void
coop_all_qput (coop_q_t *q, coop_t *t)
#else
static void
coop_all_qput (q, t)
     coop_q_t *q;
     coop_t *t;
#endif
{
  if (q->t.all_next)
    q->t.all_next->all_prev = t;
  t->all_prev = NULL;
  t->all_next = q->t.all_next;
  q->t.all_next = t;
}

#ifdef __STDC__
static void
coop_all_qremove (coop_q_t *q, coop_t *t)
#else
static void
coop_all_qremove (q, t)
     coop_q_t *q;
     coop_t *t;
#endif
{
  if (t->all_prev)
    t->all_prev->all_next = t->all_next;
  else
    q->t.all_next = t->all_next;
  if (t->all_next)
      t->all_next->all_prev = t->all_prev;
}


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


#ifdef __STDC__
void
coop_init()
#else
void
coop_init()
#endif
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

#ifdef GUILE_ISELECT
extern coop_t *coop_next_runnable_thread ();
#else
#ifdef __STDC__
coop_t *
coop_next_runnable_thread()
#else
coop_t *
coop_next_runnable_thread()
#endif
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

#ifdef __STDC__
void
coop_start()
#else
void
coop_start()
#endif
{
  coop_t *next;

  while ((next = coop_qget (&coop_global_runq)) != NULL) {
    coop_global_curr = next;
    QT_BLOCK (coop_starthelp, 0, 0, next->sp);
  }
}


#ifdef __STDC__
static void *
coop_starthelp (qt_t *old, void *ignore0, void *ignore1)
#else
static void *
coop_starthelp (old, ignore0, ignore1)
     qt_t *old;
     void *ignore0;
     void *ignore1;
#endif
{
  coop_global_main.sp = old;
  coop_global_main.joining = NULL;
  coop_qput (&coop_global_runq, &coop_global_main);
  return NULL; /* not used, but keeps compiler happy */
}

#ifdef __STDC__
int
coop_mutex_init (coop_m *m)
#else
int
coop_mutex_init (m)
     coop_m *m;
#endif
{
  m->owner = NULL;
  coop_qinit(&(m->waiting));
  return 0;
}

#ifdef __STDC__
int
coop_mutex_lock (coop_m *m)
#else
int 
coop_mutex_lock ()
     coop_m *m;
#endif
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


#ifdef __STDC__
int 
coop_mutex_unlock (coop_m *m)
#else
int 
coop_mutex_unlock (m)
     coop_m *m;
#endif
{
  coop_t *old, *newthread;
  
  newthread = coop_qget (&(m->waiting));
  if (newthread != NULL)
    {
      /* Record the current top-of-stack before going to sleep */
      coop_global_curr->top = &old;

      old = coop_global_curr;
      coop_global_curr = newthread;
      m->owner = coop_global_curr;
      QT_BLOCK (coop_yieldhelp, old, &coop_global_runq, newthread->sp);
    }
  else
    {
      m->owner = NULL;
    }
  return 0;
}


#ifdef __STDC__
int 
coop_mutex_destroy (coop_m *m)
#else
int 
coop_mutex_destroy (m)
     coop_m *m;
#endif
{
  return 0;
}


#ifdef __STDC__
int 
coop_condition_variable_init (coop_c *c)
#else
int 
coop_condition_variable_init (c)
     coop_c *c;
#endif
{
  coop_qinit(&(c->waiting));
  return 0;
}

#ifdef __STDC__
int 
coop_condition_variable_wait (coop_c *c)
#else
int 
coop_condition_variable_wait (c)
     coop_c *c;
#endif
{
  coop_t *old, *newthread;

#ifdef GUILE_ISELECT
  newthread = coop_wait_for_runnable_thread();
  if (newthread == coop_global_curr)
    coop_abort ();
#else
  newthread = coop_next_runnable_thread();
#endif
  old = coop_global_curr;
  coop_global_curr = newthread;
  QT_BLOCK (coop_yieldhelp, old, &(c->waiting), newthread->sp);
  return 0;
}


#ifdef __STDC__
int 
coop_condition_variable_wait_mutex (coop_c *c, coop_m *m)
#else
int 
coop_condition_variable_wait_mutex (c, m)
     coop_c *c;
     coop_m *m;
#endif
{
  coop_mutex_unlock (m);
  coop_condition_variable_wait (c);
  coop_mutex_lock (m);
  return 0;
}


#ifdef __STDC__
int 
coop_condition_variable_signal (coop_c *c)
#else
int 
coop_condition_variable_signal (c)
     coop_c *c;
#endif
{
  coop_t *newthread;

  while ((newthread = coop_qget (&(c->waiting))) != NULL)
    {
      coop_qput (&coop_global_runq, newthread);
    }
  return 0;
}


#ifdef __STDC__
int 
coop_condition_variable_destroy (coop_c *c)
#else
int 
coop_condition_variable_destroy (c)
     coop_c *c;
#endif
{
  return 0;
}


#ifdef __STDC__
coop_t *
coop_create (coop_userf_t *f, void *pu)
#else
coop_t *
coop_create (f, pu)
     coop_userf_t *f;
     void *pu;
#endif
{
  coop_t *t;
  void *sto;

  t = malloc (sizeof(coop_t));

  t->data = NULL;
  t->sto = malloc (COOP_STKSIZE);
  sto = COOP_STKALIGN (t->sto, QT_STKALIGN);
  t->sp = QT_SP (sto, COOP_STKSIZE - QT_STKALIGN);
  t->base = t->sp;
  t->sp = QT_ARGS (t->sp, pu, t, (qt_userf_t *)f, coop_only);
  t->joining = NULL;
  coop_qput (&coop_global_runq, t);
  coop_all_qput (&coop_global_allq, t);

  return t;
}


#ifdef __STDC__
static void
coop_only (void *pu, void *pt, qt_userf_t *f)
#else
static void
coop_only (pu. pt, f)
     void *pu, 
     void *pt, 
     qt_userf_t *f;
#endif
{
  coop_global_curr = (coop_t *)pt;
  (*(coop_userf_t *)f)(pu);
  coop_abort();
  /* NOTREACHED */
}


#ifdef __STDC__
void
coop_abort ()
#else
void
coop_abort ()
#endif
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
  QT_ABORT (coop_aborthelp, old, (void *)NULL, newthread->sp);
}


#ifdef __STDC__
static void *
coop_aborthelp (qt_t *sp, void *old, void *null)
#else
static void *
coop_aborthelp (sp, old, null)
     qt_t *sp;
     void *old;
     void *null;
#endif
{
  coop_t *oldthread = (coop_t *) old;

  free (oldthread->sto);

  /* "old" is freed in scm_threads_thread_die().
     Marking old->base NULL indicates that this thread is dead */

  oldthread->base = NULL;

  return NULL;
}


#ifdef __STDC__
void 
coop_join(coop_t *t)
#else
void 
coop_join()
     coop_t *t;
#endif
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

#ifdef __STDC__
void
coop_yield()
#else
void
coop_yield()
#endif
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


#ifdef __STDC__
static void *
coop_yieldhelp (qt_t *sp, void *old, void *blockq)
#else
static void *
coop_yieldhelp (sp, old, blockq)
     qt_t *sp;
     void *old;
     void *blockq;
#endif
{
  ((coop_t *)old)->sp = sp;
  coop_qput ((coop_q_t *)blockq, (coop_t *)old);
  return NULL;
}

/* Replacement for the system's sleep() function. Does the right thing
   for the process - but not for the system (it busy-waits) */

#ifdef __STDC__
void *
coop_sleephelp (qt_t *sp, void *old, void *blockq)
#else
void *
coop_sleephelp (sp, old, bolckq)
     qt_t *sp;
     void *old;
     void *blockq;
#endif
{
  ((coop_t *)old)->sp = sp;
  /* old is already on the sleep queue - so there's no need to
     do anything extra here */
  return NULL;
}

#ifdef GUILE_ISELECT

void
usleep (unsigned usec)
{
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = usec;
  scm_internal_select (0, NULL, NULL, NULL, &timeout);
}

unsigned
sleep (unsigned sec)
{
  time_t now = time (NULL);
  struct timeval timeout;
  int slept;
  timeout.tv_sec = sec;
  timeout.tv_usec = 0;
  scm_internal_select (0, NULL, NULL, NULL, &timeout);
  slept = time (NULL) - now;
  return slept > sec ? 0 : sec - slept;
}

#else /* GUILE_ISELECT */

#ifdef __STDC__
unsigned
sleep (unsigned s)
#else
unsigned
sleep (s)
     unsigned s;
#endif
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

#endif /* GUILE_ISELECT */

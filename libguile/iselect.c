/*	Copyright (C) 1997 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <limits.h>
#include <string.h>

#include "_scm.h"

#include "iselect.h"
#include "coop-threads.h"



#define QEMPTYP(q) (q.t.next == &q.t)
#define QFIRST(q) (q.t.next)

#define SCM_BITS_PER_LONG (8 * sizeof (unsigned long))
#if ULONG_MAX >> 16 == 0
#define SCM_NLONGBITS(p) (bc[((unsigned char *)(p))[0]]\
			  + bc[((unsigned char *)(p))[1]])
#elif ULONG_MAX >> 32 == 0
#define SCM_NLONGBITS(p) (bc[((unsigned char *)(p))[0]]\
			  + bc[((unsigned char *)(p))[1]]\
			  + bc[((unsigned char *)(p))[2]]\
			  + bc[((unsigned char *)(p))[3]])
#elif ULONG_MAX >> 64 == 0
#define SCM_NLONGBITS(p) (bc[((unsigned char *)(p))[0]]\
			  + bc[((unsigned char *)(p))[1]]\
			  + bc[((unsigned char *)(p))[2]]\
			  + bc[((unsigned char *)(p))[3]]\
			  + bc[((unsigned char *)(p))[4]]\
			  + bc[((unsigned char *)(p))[5]]\
			  + bc[((unsigned char *)(p))[6]]\
			  + bc[((unsigned char *)(p))[7]])
#endif

#ifdef HAVE_BZERO
#define FD_ZERO_N(pos, n) bzero ((pos), (n))
#else
#define FD_ZERO_N(pos, n) memset ((void *) (pos), 0, (n))
#endif

typedef unsigned long *ulongptr;

static char bc[256]; /* Bit counting array.  bc[x] is the number of
			bits in x. */

int gnfds;
SELECT_TYPE greadfds;
SELECT_TYPE gwritefds;
SELECT_TYPE gexceptfds;
SELECT_TYPE rreadfds;
SELECT_TYPE rwritefds;
SELECT_TYPE rexceptfds;
static struct timeval timeout0;

/* Insert thread t into the ordered queue q.
   q is ordered after wakeup_time.  Threads which aren't sleeping but
   waiting for I/O go last into the queue. */
static void
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

#ifdef DEBUG_ISELECT
static void
qp (char* qn, coop_q_t *q)
{
  coop_t *t = q->t.next;
  fprintf (stderr, "%s:", qn);
  while (t != &q->t)
    {
      if (t->timeoutp)
	fprintf (stderr, " %04x (%d,%d)", (unsigned) t,
		 t->wakeup_time.tv_sec, t->wakeup_time.tv_usec);
      else
	fprintf (stderr, " %04x", (unsigned) t);
      t = t->next;
    }
  fprintf (stderr, "\n");
}
#endif

/* As select, but doesn't destroy the file descriptor sets passed as
   arguments. */
static int
safe_select (int nfds,
	     SELECT_TYPE *readfds,
	     SELECT_TYPE *writefds,
	     SELECT_TYPE *exceptfds,
	     struct timeval *timeout)
{
  int n = (nfds + 7) / 8;
  /* Copy file descriptor sets to result area */
  if (readfds == NULL)
    FD_ZERO (&rreadfds);
  else
    {
      memcpy (&rreadfds, readfds, n);
      FD_ZERO_N ((char *) &rreadfds + n, SELECT_SET_SIZE / 8 - n);
    }
  if (writefds == NULL)
    FD_ZERO (&rwritefds);
  else
    {
      memcpy (&rwritefds, writefds, n);
      FD_ZERO_N ((char *) &rwritefds + n, SELECT_SET_SIZE / 8 - n);
    }
  if (exceptfds == NULL)
    FD_ZERO (&rexceptfds);
  else
    {
      memcpy (&rexceptfds, exceptfds, n);
      FD_ZERO_N ((char *) &rexceptfds + n, SELECT_SET_SIZE / 8 - n);
    }
  return select (nfds, &rreadfds, &rwritefds, &rexceptfds, timeout);
}

/* Merge new file descriptor sets into the global sets.
   Return 0 on success.  Return -1 if some other thread is already
   waiting for one or more of the requested descriptors. */
static int
add_fd_sets (int nfds,
	     SELECT_TYPE *readfds,
	     SELECT_TYPE *writefds,
	     SELECT_TYPE *exceptfds)
{
  int i, n = (nfds + SCM_BITS_PER_LONG - 1) / SCM_BITS_PER_LONG;
  for (i = 0; i < n; ++i)
    if ((readfds != NULL
	 && (((ulongptr) readfds)[i] & ((ulongptr) &greadfds)[i]) != 0)
	|| (writefds != NULL
	    && (((ulongptr) writefds)[i] & ((ulongptr) &gwritefds)[i]) != 0)
	|| (exceptfds != NULL
	    && (((ulongptr) exceptfds)[i] & ((ulongptr) &gexceptfds)[i]) != 0))
      return -1;
  coop_global_curr->nfds = 0;
  coop_global_curr->readfds = readfds;
  coop_global_curr->writefds = writefds;
  coop_global_curr->exceptfds = exceptfds;
  while (i > 0)
    {
      --i;
      if (readfds != NULL && ((ulongptr) readfds)[i] != 0)
	{
	  ((ulongptr) &greadfds)[i] |= ((ulongptr) readfds)[i];
	  n = (i + 1) * SCM_BITS_PER_LONG;
	  coop_global_curr->nfds = n;
	  if (n > gnfds)
	    gnfds = n;
	  goto cont_read;
	}
      if (writefds != NULL && ((ulongptr) writefds)[i] != 0)
	{
	  ((ulongptr) &gwritefds)[i] |= ((ulongptr) writefds)[i];
	  n = (i + 1) * SCM_BITS_PER_LONG;
	  coop_global_curr->nfds = n;
	  if (n > gnfds)
	    gnfds = n;
	  goto cont_write;
	}
      if (exceptfds != NULL && ((ulongptr) exceptfds)[i] != 0)
	{
	  ((ulongptr) &gexceptfds)[i] |= ((ulongptr) exceptfds)[i];
	  n = (i + 1) * SCM_BITS_PER_LONG;
	  coop_global_curr->nfds = n;
	  if (n > gnfds)
	    gnfds = n;
	  goto cont_except;
	}
    }
  while (i > 0)
    {
      --i;
      if (readfds != NULL && ((ulongptr) readfds)[i] != 0)
	((ulongptr) &greadfds)[i] |= ((ulongptr) readfds)[i];
    cont_read:
      if (writefds != NULL && ((ulongptr) writefds)[i] != 0)
	((ulongptr) &gwritefds)[i] |= ((ulongptr) writefds)[i];
    cont_write:
      if (exceptfds != NULL && ((ulongptr) exceptfds)[i] != 0)
	((ulongptr) &gexceptfds)[i] |= ((ulongptr) exceptfds)[i];
    cont_except:
    }
  return 0;
}

static void
finalize_fd_sets (coop_t *t)
{
  int i = (t->nfds + SCM_BITS_PER_LONG - 1) / SCM_BITS_PER_LONG;
  int n_ones = 0;
  register unsigned long s;
  if (t->nfds == gnfds)
    {
      while (i > 0)
	{
	  --i;
	  if (t->readfds != NULL && (s = ((ulongptr) t->readfds)[i]) != 0)
	    {
	      ((ulongptr) t->readfds)[i] &= ((ulongptr) &rreadfds)[i];
	      ((ulongptr) &greadfds)[i] &= ~s;
	      n_ones += SCM_NLONGBITS (&((ulongptr) t->readfds)[i]);
	    }
	  if (((ulongptr) &greadfds)[i] != 0)
	    {
	      gnfds = (i + 1) * SCM_BITS_PER_LONG;
	      goto cont_read;
	    }
	  if (t->writefds != NULL && (s = ((ulongptr) t->writefds)[i]) != 0)
	    {
	      ((ulongptr) t->writefds)[i] &= ((ulongptr) &rwritefds)[i];
	      ((ulongptr) &gwritefds)[i] &= ~s;
	      n_ones += SCM_NLONGBITS (&((ulongptr) t->writefds)[i]);
	    }
	  if (((ulongptr) &gwritefds)[i] != 0)
	    {
	      gnfds = (i + 1) * SCM_BITS_PER_LONG;
	      goto cont_write;
	    }
	  if (t->exceptfds != NULL && (s = ((ulongptr) t->exceptfds)[i]) != 0)
	    {
	      ((ulongptr) t->exceptfds)[i] &= ((ulongptr) &rexceptfds)[i];
	      ((ulongptr) &gexceptfds)[i] &= ~s;
	      n_ones += SCM_NLONGBITS (&((ulongptr) t->exceptfds)[i]);
	    }
	  if (((ulongptr) &gexceptfds)[i] != 0)
	    {
	      gnfds = (i + 1) * SCM_BITS_PER_LONG;
	      goto cont_except;
	    }
	}
      gnfds = 0;
      t->retval = n_ones;
      return;
    }
  while (i > 0)
    {
      --i;
      if (t->readfds != NULL && (s = ((ulongptr) t->readfds)[i]) != 0)
	{
	  ((ulongptr) t->readfds)[i] &= ((ulongptr) &rreadfds)[i];
	  ((ulongptr) &greadfds)[i] &= ~s;
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->readfds)[i]);
	}
    cont_read:
      if (t->writefds != NULL && (s = ((ulongptr) t->writefds)[i]) != 0)
	{
	  ((ulongptr) t->writefds)[i] &= ((ulongptr) &rwritefds)[i];
	  ((ulongptr) &gwritefds)[i] &= ~s;
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->readfds)[i]);
	}
    cont_write:
      if (t->exceptfds != NULL && (s = ((ulongptr) t->exceptfds)[i]) != 0)
	{
	  ((ulongptr) t->exceptfds)[i] &= ((ulongptr) &rexceptfds)[i];
	  ((ulongptr) &gexceptfds)[i] &= ~s;
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->readfds)[i]);
	}
    cont_except:
    }
  t->retval = n_ones;
}

static int
first_interesting_fd (void)
{
  int i = 0;
  SELECT_TYPE *s;
  while (1)
    {
      if (((ulongptr) &rreadfds)[i] != 0)
	{
	  s = &rreadfds;
	  break;
	}
      if (((ulongptr) &rwritefds)[i] != 0)
	{
	  s = &rwritefds;
	  break;
	}
      if (((ulongptr) &rexceptfds)[i] != 0)
	{
	  s = &rexceptfds;
	  break;
	}
      ++i;
    }
  i *= SCM_BITS_PER_LONG;
  while (i < gnfds)
    {
      if (FD_ISSET (i, s))
	return i;
      ++i;
    }
  fprintf (stderr, "first_interesting_fd: internal error\n");
  exit (1);
}

static void
error_revive (void)
{
  coop_t *t;
  
  while ((t = coop_qget (&coop_global_sleepq)) != NULL)
    {
      t->errno = errno;
      t->retval = -1;
      coop_qput (&coop_global_runq, t);
    }
  gnfds = 0;
  FD_ZERO (&greadfds);
  FD_ZERO (&gwritefds);
  FD_ZERO (&gexceptfds);
}

static coop_t *
find_thread (int n, struct timeval *now)
{
  coop_t *t;
  int fd;

  if (n == 0)
    {
      while (!QEMPTYP (coop_global_sleepq)
	     && (t = QFIRST (coop_global_sleepq))->timeoutp
	     && (t->wakeup_time.tv_sec < now->tv_sec
		 || (t->wakeup_time.tv_sec == now->tv_sec
		     && t->wakeup_time.tv_usec <= now->tv_usec)))
	{
	  coop_qget (&coop_global_sleepq);
	  finalize_fd_sets (t);
	  coop_qput (&coop_global_runq, t);
	}
    }
  else if (n > 0)
    {
      /* Find the first interesting file descriptor */
      fd = first_interesting_fd ();
      /* Check the sleeping queue for this file descriptor.
	 Other file descriptors will be handled next time
	 coop_next_runnable_thread is called. */
      /* This code is inefficient.  We'll improve it later. */
      while ((t = coop_qget (&coop_global_sleepq)) != NULL)
	{
	  if ((t->readfds && FD_ISSET (fd, t->readfds))
	      || (t->writefds && FD_ISSET (fd, t->writefds))
	      || (t->exceptfds && FD_ISSET (fd, t->exceptfds))
	      || (t->timeoutp
		  && (t->wakeup_time.tv_sec < now->tv_sec
		      || (t->wakeup_time.tv_sec == now->tv_sec
			  && t->wakeup_time.tv_usec <= now->tv_usec))))
	    {
	      finalize_fd_sets (t);
	      coop_qput (&coop_global_runq, t);
	    }
	  else
	    coop_qput(&coop_tmp_queue, t);
	}
      while ((t = coop_qget (&coop_tmp_queue)) != NULL)
	coop_qput (&coop_global_sleepq, t);
    }
  else /* n < 0 */
    /* An error has occured.  It is not EBADF since
       scm_internal_select called select before putting the
       threads on the sleep queue.  The most robust and select
       compatible behaviour is probably to let all sleeping
       threads return with an error. */
    error_revive ();

  return coop_qget (&coop_global_runq);
}

/* Return next runnable thread on the run queue.
 * First update the queue with possible I/O or timeouts.
 * If no thread is found, return NULL.
 */
coop_t *
coop_next_runnable_thread ()
{
  coop_t *t;
  struct timeval now;
  int n;

  /* Just return next thread on the runq if the sleepq is empty. */
  if (QEMPTYP (coop_global_sleepq))
    return coop_qget (&coop_global_runq);

  ++scm_ints_disabled;
  if (gnfds > 0)
    n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, &timeout0);
  else
    n = 0;
  if (QFIRST (coop_global_sleepq)->timeoutp)
    {
      gettimeofday (&now, NULL);
      t = find_thread (n, &now);
    }
  else
    t = find_thread (n, 0);
  if (!--scm_ints_disabled)
    SCM_ASYNC_TICK;
  return t;
}

coop_t *
coop_wait_for_runnable_thread_now (struct timeval *now)
{
  int n;
  coop_t *t;

  ++scm_ints_disabled;
  if (gnfds > 0)
    n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, &timeout0);
  else
    n = 0;
  /* Is there any other runnable thread? */
  t = find_thread (n, now);
  while (t == NULL)
    {
      /* No.  Let the process go to sleep. */
      if ((t = QFIRST (coop_global_sleepq))->timeoutp)
	{
	  now->tv_sec = t->wakeup_time.tv_sec - now->tv_sec;
	  if (now->tv_usec > t->wakeup_time.tv_usec)
	    {
	      --now->tv_sec;
	      now->tv_usec = 1000000 + t->wakeup_time.tv_usec - now->tv_usec;
	    }
	  else
	    now->tv_usec = t->wakeup_time.tv_usec - now->tv_usec;
	  n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, now);
	}
      else
	n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, NULL);
      gettimeofday (now, NULL);
      t = find_thread (n, now);
    }

  if (!--scm_ints_disabled)
    SCM_ASYNC_TICK;
  return t;
}

coop_t *
coop_wait_for_runnable_thread ()
{
  struct timeval now;

  if (QEMPTYP (coop_global_sleepq))
    return coop_qget (&coop_global_runq);

  if (QFIRST (coop_global_sleepq)->timeoutp)
    gettimeofday (&now, NULL);
  
  return coop_wait_for_runnable_thread_now (&now);
}

int
scm_internal_select (int nfds,
		     SELECT_TYPE *readfds,
		     SELECT_TYPE *writefds,
		     SELECT_TYPE *exceptfds,
		     struct timeval *timeout)
{
  struct timeval now;
  coop_t *t, *curr = coop_global_curr;
  
  /* If the timeout is 0, we're polling and can handle it quickly. */
  if (timeout != NULL
      && timeout->tv_sec == 0
      && timeout->tv_usec == 0)
    return select (nfds, readfds, writefds, exceptfds, timeout);

  /* Add our file descriptor flags to the common set. */
  if (add_fd_sets (nfds, readfds, writefds, exceptfds))
    {
      errno = EBADF; /* Several threads can't select on same fds. */
      return -1;
    }

  /* Place ourselves on the sleep queue and get a new thread to run. */
  if (timeout == NULL)
    {
      curr->timeoutp = 0;
      coop_qput (&coop_global_sleepq, curr);
      t = coop_wait_for_runnable_thread ();
    }
  else
    {
      gettimeofday (&now, NULL);
      curr->timeoutp = 1;
      curr->wakeup_time.tv_sec = now.tv_sec + timeout->tv_sec;
      curr->wakeup_time.tv_usec = now.tv_usec + timeout->tv_usec;
      if (curr->wakeup_time.tv_usec >= 1000000)
	{
	  ++curr->wakeup_time.tv_sec;
	  curr->wakeup_time.tv_usec -= 1000000;
	}
      /* Insert the current thread at the right place in the sleep queue */
      coop_timeout_qinsert (&coop_global_sleepq, curr);
      t = coop_wait_for_runnable_thread_now (&now);
    }

  /* If the new thread is the same as the sleeping thread, do nothing */
  if (t != curr)
    {
      /* Do a context switch. */
      coop_global_curr = t;
      QT_BLOCK (coop_sleephelp, curr, NULL, t->sp);
    }

  if (curr->retval == -1)
    errno = curr->errno;
  return curr->retval;
}

/* Initialize bit counting array */
static void init_bc (int bit, int i, int n)
{
  if (bit == 0)
    bc[i] = n;
  else
    {
      init_bc (bit >> 1, i, n);
      init_bc (bit >> 1, i | bit, n + 1);
    }
}

void
scm_init_iselect ()
{
  gnfds = 0;
  FD_ZERO (&greadfds);
  FD_ZERO (&gwritefds);
  FD_ZERO (&gexceptfds);
  timeout0.tv_sec = 0;
  timeout0.tv_usec = 0;
  init_bc (0x80, 0, 0);
#include "iselect.x"
}

/*	Copyright (C) 1997, 1998, 2000 Free Software Foundation, Inc.
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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libguile/_scm.h"
#include "libguile/async.h"

#include "libguile/iselect.h"

#ifdef GUILE_ISELECT

#include "libguile/coop-threads.h"

#ifdef MISSING_BZERO_DECL
extern void bzero (void *, size_t);
#endif



/* COOP queue macros */
#define QEMPTYP(q) (q.t.next == &q.t)
#define QFIRST(q) (q.t.next)

/* These macros count the number of bits in a word.  */
#define SCM_BITS_PER_LONG (8 * sizeof (unsigned long))
/* Use LONG_MAX instead of ULONG_MAX here since not all systems define
   ULONG_MAX */
#if LONG_MAX >> 16 == 0
#define SCM_NLONGBITS(p) (bc[((unsigned char *)(p))[0]]\
			  + bc[((unsigned char *)(p))[1]])
#elif LONG_MAX >> 32 == 0 || LONG_MAX == 2147483647L /* bug in Sun CC 4.2 */
#define SCM_NLONGBITS(p) (bc[((unsigned char *)(p))[0]]\
			  + bc[((unsigned char *)(p))[1]]\
			  + bc[((unsigned char *)(p))[2]]\
			  + bc[((unsigned char *)(p))[3]])
#elif LONG_MAX >> 64 == 0
#define SCM_NLONGBITS(p) (bc[((unsigned char *)(p))[0]]\
			  + bc[((unsigned char *)(p))[1]]\
			  + bc[((unsigned char *)(p))[2]]\
			  + bc[((unsigned char *)(p))[3]]\
			  + bc[((unsigned char *)(p))[4]]\
			  + bc[((unsigned char *)(p))[5]]\
			  + bc[((unsigned char *)(p))[6]]\
			  + bc[((unsigned char *)(p))[7]])
#else
#error Could not determine suitable definition for SCM_NLONGBITS
#endif

#ifdef HAVE_BZERO
#define FD_ZERO_N(pos, n) bzero ((pos), (n))
#else
#define FD_ZERO_N(pos, n) memset ((void *) (pos), 0, (n))
#endif

typedef unsigned long *ulongptr;

static char bc[256]; /* Bit counting array.  bc[x] is the number of
			bits in x. */

int scm_I_am_dead;

/* This flag indicates that several threads are waiting on the same
   file descriptor.  When this is the case, the common fd sets are
   updated in a more inefficient way.  */
int collisionp;

/* These are the common fd sets.  When new select calls are made,
   those sets are merged into these.  */
int gnfds;
SELECT_TYPE greadfds;
SELECT_TYPE gwritefds;
SELECT_TYPE gexceptfds;

/* These are the result sets.  They are used when we call OS select.
   We couldn't use the common fd sets above, since that would destroy
   them.  */
SELECT_TYPE rreadfds;
SELECT_TYPE rwritefds;
SELECT_TYPE rexceptfds;

/* Constant timeval struct representing a zero timeout which we use
   when polling.  */
static struct timeval timeout0;

/* As select, but doesn't destroy the file descriptor sets passed as
   arguments.  The results are stored into the result sets.  */
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

/* Merge new file descriptor sets into the common sets.  */
static void
add_fd_sets (coop_t *t)
{
  int n = (t->nfds + SCM_BITS_PER_LONG - 1) / SCM_BITS_PER_LONG;
  int i;

  /* Detect if the fd sets of the thread have any bits in common with
     the rest of the waiting threads.  If that is so, set the
     collision flag.  This causes a more time consuming handling of
     the common fd sets---they need to recalculated every time a
     thread wakes up.  */
  if (!collisionp)
    for (i = 0; i < n; ++i)
      if ((t->readfds != NULL
	   && (((ulongptr) t->readfds)[i] & ((ulongptr) &greadfds)[i]) != 0)
	  || (t->writefds != NULL
	      && ((((ulongptr) t->writefds)[i] & ((ulongptr) &gwritefds)[i])
		  != 0))
	  || (t->exceptfds != NULL
	      && ((((ulongptr) t->exceptfds)[i] & ((ulongptr) &gexceptfds)[i])
		  != 0)))
	{
	  collisionp = 1;
	  break;
	}
  
  /* We recalculate nfds below.  The cost for this can be paid back
     with a great bonus since many programs are lazy with the nfds
     arg.  Many even pass 1024 when using one of the lowest fd:s!

     We approach from above, checking for non-zero bits.  As soon as
     we have determined the value of nfds, we jump down to code below
     which concludes the updating of the common sets.  */
  t->nfds = 0;
  i = n;
  while (i > 0)
    {
      --i;
      if (t->readfds != NULL && ((ulongptr) t->readfds)[i] != 0)
	{
	  ((ulongptr) &greadfds)[i] |= ((ulongptr) t->readfds)[i];
	  n = (i + 1) * SCM_BITS_PER_LONG;
	  t->nfds = n;
	  if (n > gnfds)
	    gnfds = n;
	  goto cont_read;
	}
      if (t->writefds != NULL && ((ulongptr) t->writefds)[i] != 0)
	{
	  ((ulongptr) &gwritefds)[i] |= ((ulongptr) t->writefds)[i];
	  n = (i + 1) * SCM_BITS_PER_LONG;
	  t->nfds = n;
	  if (n > gnfds)
	    gnfds = n;
	  goto cont_write;
	}
      if (t->exceptfds != NULL && ((ulongptr) t->exceptfds)[i] != 0)
	{
	  ((ulongptr) &gexceptfds)[i] |= ((ulongptr) t->exceptfds)[i];
	  n = (i + 1) * SCM_BITS_PER_LONG;
	  t->nfds = n;
	  if (n > gnfds)
	    gnfds = n;
	  goto cont_except;
	}
    }
  return;

  /* nfds is now determined.  Just finish updating the common sets.  */
  while (i > 0)
    {
      --i;
      if (t->readfds != NULL && ((ulongptr) t->readfds)[i] != 0)
	((ulongptr) &greadfds)[i] |= ((ulongptr) t->readfds)[i];
    cont_read:
      if (t->writefds != NULL && ((ulongptr) t->writefds)[i] != 0)
	((ulongptr) &gwritefds)[i] |= ((ulongptr) t->writefds)[i];
    cont_write:
      if (t->exceptfds != NULL && ((ulongptr) t->exceptfds)[i] != 0)
	((ulongptr) &gexceptfds)[i] |= ((ulongptr) t->exceptfds)[i];
    cont_except:
      ;
    }
}

/* Update the fd sets pointed to by the thread so that they reflect
   the status of the file descriptors which the thread was interested
   in.  Also clear those bits in the common sets.  This function is
   only called when there are no bit collisions.  */
static void
finalize_fd_sets (coop_t *t)
{
  int i = (t->nfds + SCM_BITS_PER_LONG - 1) / SCM_BITS_PER_LONG;
  int n_ones = 0;
  register unsigned long s;

  if (t->nfds == gnfds)
    {
      /* This thread is the one responsible for the current high value
	 of gnfds.  First do our other jobs while at the same time
	 trying to decrease gnfds.  */
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

  /* Either this thread wasn't responsible for gnfds or gnfds has been
     determined.  */
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
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->writefds)[i]);
	}
    cont_write:
      if (t->exceptfds != NULL && (s = ((ulongptr) t->exceptfds)[i]) != 0)
	{
	  ((ulongptr) t->exceptfds)[i] &= ((ulongptr) &rexceptfds)[i];
	  ((ulongptr) &gexceptfds)[i] &= ~s;
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->exceptfds)[i]);
	}
    cont_except:
      ;
    }
  t->retval = n_ones;
}

/* Just like finalize_fd_sets except that we don't have to update the
   global fd sets.  Those will be recalulated elsewhere.  */
static void
finalize_fd_sets_lazily (coop_t *t)
{
  int i = (t->nfds + SCM_BITS_PER_LONG - 1) / SCM_BITS_PER_LONG;
  int n_ones = 0;
  while (i > 0)
    {
      --i;
      if (t->readfds != NULL && ((ulongptr) t->readfds)[i] != 0)
	{
	  ((ulongptr) t->readfds)[i] &= ((ulongptr) &rreadfds)[i];
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->readfds)[i]);
	}
      if (t->writefds != NULL && ((ulongptr) t->writefds)[i] != 0)
	{
	  ((ulongptr) t->writefds)[i] &= ((ulongptr) &rwritefds)[i];
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->writefds)[i]);
	}
      if (t->exceptfds != NULL && ((ulongptr) t->exceptfds)[i] != 0)
	{
	  ((ulongptr) t->exceptfds)[i] &= ((ulongptr) &rexceptfds)[i];
	  n_ones += SCM_NLONGBITS (&((ulongptr) t->exceptfds)[i]);
	}
    }
  t->retval = n_ones;
}

/* Return first fd with a non-zero bit in any of the result sets.  */
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

/* Revive all threads with an error status.  */
void
scm_error_revive_threads (void)
{
  coop_t *t;
  
  while ((t = coop_qget (&coop_global_sleepq)) != NULL)
    {
      t->_errno = errno;
      t->retval = -1;
      if (t != coop_global_curr)
	coop_qput (&coop_global_runq, t);
    }
  collisionp = 0;
  gnfds = 0;
  FD_ZERO (&greadfds);
  FD_ZERO (&gwritefds);
  FD_ZERO (&gexceptfds);
}

/* Given the result of a call to safe_select and the current time,
   try to wake up some threads and return the first one.  Return NULL
   if we couldn't find any.  */
static coop_t *
find_thread (int n, struct timeval *now, int sleepingp)
{
  coop_t *t;
  int fd;

  if (n < 0)
    /* An error or a signal has occured.  Wake all threads.  Since we
       don't care to calculate if there is a sinner we report the
       error to all of them.  */
    {
      scm_error_revive_threads ();
      if (!scm_I_am_dead)
	return coop_global_curr;
    }
  else if (n == 0)
    {
      while (!QEMPTYP (coop_global_sleepq)
	     && (t = QFIRST (coop_global_sleepq))->timeoutp
	     && (t->wakeup_time.tv_sec < now->tv_sec
		 || (t->wakeup_time.tv_sec == now->tv_sec
		     && t->wakeup_time.tv_usec <= now->tv_usec)))
	{
	  coop_qget (&coop_global_sleepq);
	  if (collisionp)
	    finalize_fd_sets_lazily (t);
	  else
	    finalize_fd_sets (t);
	  coop_qput (&coop_global_runq, t);
	}
      if (collisionp)
	{
	  while ((t = coop_qget (&coop_global_sleepq)) != NULL)
	    coop_qput (&coop_tmp_queue, t);
	  goto rebuild_global_fd_sets;
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
	      if (collisionp)
		finalize_fd_sets_lazily (t);
	      else
		finalize_fd_sets (t);
	      coop_qput (&coop_global_runq, t);
	    }
	  else
	    coop_qput(&coop_tmp_queue, t);
	}
      if (collisionp)
	{
	rebuild_global_fd_sets:
	  collisionp = 0;
	  gnfds = 0;
	  FD_ZERO (&greadfds);
	  FD_ZERO (&gwritefds);
	  FD_ZERO (&gexceptfds);
	  while ((t = coop_qget (&coop_tmp_queue)) != NULL)
	    {
	      add_fd_sets (t);
	      coop_qput (&coop_global_sleepq, t);
	    }
	}
      else
	{
	  while ((t = coop_qget (&coop_tmp_queue)) != NULL)
	    coop_qput (&coop_global_sleepq, t);
	}
    }

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
    {
      if (QEMPTYP (coop_global_runq))
	return coop_global_curr;
      else
	return coop_qget (&coop_global_runq);
    }

  if (gnfds > 0)
    n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, &timeout0);
  else
    n = 0;
  if (QFIRST (coop_global_sleepq)->timeoutp)
    {
      gettimeofday (&now, NULL);
      t = find_thread (n, &now, 0);
    }
  else
    t = find_thread (n, 0, 0);
  return t == NULL ? coop_global_curr : t;
}

coop_t *
coop_wait_for_runnable_thread_now (struct timeval *now)
{
  int n;
  coop_t *t;

  if (gnfds > 0)
    n = safe_select (gnfds, &greadfds, &gwritefds, &gexceptfds, &timeout0);
  else
    n = 0;
  /* Is there any other runnable thread? */
  t = find_thread (n, now, 1);
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
      t = find_thread (n, now, 1);
    }

  return t;
}

coop_t *
coop_wait_for_runnable_thread ()
{
  struct timeval now;

  if (QEMPTYP (coop_global_sleepq))
    {
      if (QEMPTYP (coop_global_runq))
	return coop_global_curr;
      else
	return coop_qget (&coop_global_runq);
    }

  if (QFIRST (coop_global_sleepq)->timeoutp)
    gettimeofday (&now, NULL);
  
  return coop_wait_for_runnable_thread_now (&now);
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
#if 0 /* This is just symbolic */
  collisionp = 0;
  gnfds = 0;
  FD_ZERO (&greadfds);
  FD_ZERO (&gwritefds);
  FD_ZERO (&gexceptfds);
  timeout0.tv_sec = 0;
  timeout0.tv_usec = 0;
#endif
  init_bc (0x80, 0, 0);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/iselect.x"
#endif
}

#endif /* GUILE_ISELECT */

int
scm_internal_select (int nfds,
		     SELECT_TYPE *readfds,
		     SELECT_TYPE *writefds,
		     SELECT_TYPE *exceptfds,
		     struct timeval *timeout)
{
#ifndef GUILE_ISELECT
  int res = select (nfds, readfds, writefds, exceptfds, timeout);
  SCM_ASYNC_TICK;
  return res;
#else /* GUILE_ISELECT */
  struct timeval now;
  coop_t *t, *curr = coop_global_curr;

  /* If the timeout is 0, we're polling and can handle it quickly. */
  if (timeout != NULL
      && timeout->tv_sec == 0
      && timeout->tv_usec == 0)
    return select (nfds, readfds, writefds, exceptfds, timeout);

  SCM_DEFER_INTS;

  /* Add our file descriptor flags to the common set. */
  curr->nfds = nfds;
  curr->readfds = readfds;
  curr->writefds = writefds;
  curr->exceptfds = exceptfds;
  add_fd_sets (curr);

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
  if (t != coop_global_curr)
    {
      /* Do a context switch. */
      coop_global_curr = t;
      QT_BLOCK (coop_sleephelp, curr, NULL, t->sp);
    }

  if (coop_global_curr->retval == -1)
    errno = coop_global_curr->_errno;
  SCM_ALLOW_INTS;
  SCM_ASYNC_TICK;
  return coop_global_curr->retval;
#endif /* GUILE_ISELECT */
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002 Free Software Foundation, Inc.
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




/* This file implements nice Scheme level threads on top of the gastly
   C level threads.
*/

#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>

#include "libguile/_scm.h"
#include "libguile/validate.h"
#include "libguile/root.h"
#include "libguile/eval.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/threads.h"
#include "libguile/dynwind.h"
#include "libguile/iselect.h"

/*** Queues */

static SCM
make_queue ()
{
  return scm_cons (SCM_EOL, SCM_EOL);
}

static SCM
enqueue (SCM q, SCM t)
{
  SCM c = scm_cons (t, SCM_EOL);
  if (SCM_NULLP (SCM_CDR (q)))
    SCM_SETCDR (q, c);
  else
    SCM_SETCDR (SCM_CAR (q), c);
  SCM_SETCAR (q, c);
  return c;
}

static void
remqueue (SCM q, SCM c)
{
  SCM p, prev = q;
  for (p = SCM_CDR (q); !SCM_NULLP (p); p = SCM_CDR (p))
    {
      if (SCM_EQ_P (p, c))
	{
	  if (SCM_EQ_P (c, SCM_CAR (q)))
	    SCM_SETCAR (q, SCM_CDR (c));
	  SCM_SETCDR (prev, SCM_CDR (c));
	  return;
	}
      prev = p;
    }
  abort ();
}

static SCM
dequeue (SCM q)
{
  SCM c = SCM_CDR (q);
  if (SCM_NULLP (c))
    return SCM_BOOL_F;
  else
    {
      SCM_SETCDR (q, SCM_CDR (c));
      if (SCM_NULLP (SCM_CDR (q)))
	SCM_SETCAR (q, SCM_EOL);
      return SCM_CAR (c);
    }
}


/*** Threads */

typedef struct scm_thread {
  
  /* Blocking.
   */
  scm_t_cond sleep_cond;
  struct scm_thread *next_waiting;

  scm_root_state *root;
  SCM handle;
  scm_t_thread thread;
  SCM result;
  int exited;

  SCM joining_threads;

  /* For keeping track of the stack and registers. */
  SCM_STACKITEM *base;
  SCM_STACKITEM *top;
  jmp_buf regs;

} scm_thread;

static SCM
make_thread (SCM creation_protects)
{
  SCM z;
  scm_thread *t;
  z = scm_make_smob (scm_tc16_thread);
  t = SCM_THREAD_DATA (z);
  t->handle = z;
  t->result = creation_protects;
  t->base = NULL;
  t->joining_threads = make_queue ();
  scm_cond_init (&t->sleep_cond);
  t->exited = 0;
  return z;
}

static void
init_thread_creator (SCM thread, scm_t_thread th, scm_root_state *r)
{
  scm_thread *t = SCM_THREAD_DATA(thread);
  t->root = r;
  t->thread = th;
#ifdef DEBUG
  // fprintf (stderr, "%ld created %ld\n", pthread_self (), th);
#endif
}

static void
init_thread_creatant (SCM thread, SCM_STACKITEM *base)
{
  scm_thread *t = SCM_THREAD_DATA(thread);
  t->base = base;
  t->top = NULL;
}

static SCM
thread_mark (SCM obj)
{
  scm_thread *t = SCM_THREAD_DATA (obj);
  scm_gc_mark (t->result);
  scm_gc_mark (t->joining_threads);
  return t->root->handle;
}

static int
thread_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_thread *t = SCM_THREAD_DATA (exp);
  scm_puts ("#<thread ", port);
  scm_intprint ((unsigned long)t, 16, port);
  scm_putc ('>', port);
  return 1;
}

static size_t
thread_free (SCM obj)
{
  scm_thread *t = SCM_THREAD_DATA (obj);
  if (!t->exited)
    abort ();
  scm_gc_free (t, sizeof (*t), "thread");
  return 0;
}

/*** Fair mutexes */

/* C level mutexes (such as POSIX mutexes) are not necessarily fair
   but since we'd like to use a mutex for scheduling, we build a fair
   one on top of the C one.
*/

typedef struct fair_mutex {
  scm_t_mutex lock;
  scm_thread *owner;
  scm_thread *next_waiting, *last_waiting;
} fair_mutex;

static void
fair_mutex_init (fair_mutex *m)
{
  scm_mutex_init (&m->lock);
  m->owner = NULL;
  m->next_waiting = NULL;
  m->last_waiting = NULL;
}

static void
fair_mutex_lock_1 (fair_mutex *m, scm_thread *t)
{
  if (m->owner == NULL)
    m->owner = t;
  else
    {
      t->next_waiting = NULL;
      if (m->last_waiting)
	m->last_waiting->next_waiting = t;
      else
	m->next_waiting = t;
      m->last_waiting = t;
      do
	{
	  int err;
	  err = scm_cond_wait (&t->sleep_cond, &m->lock);
	  assert (err == 0);
	}
      while (m->owner != t);
      assert (m->next_waiting == t);
      m->next_waiting = t->next_waiting;
      if (m->next_waiting == NULL)
	m->last_waiting = NULL;
    }
  scm_mutex_unlock (&m->lock);
}

static void
fair_mutex_lock (fair_mutex *m, scm_thread *t)
{
  scm_mutex_lock (&m->lock);
  fair_mutex_lock_1 (m, t);
}

static void
fair_mutex_unlock_1 (fair_mutex *m)
{
  scm_thread *t;
  scm_mutex_lock (&m->lock);
  // fprintf (stderr, "%ld unlocking\n", m->owner->pthread);
  if ((t = m->next_waiting) != NULL)
    {
      m->owner = t;
      scm_cond_signal (&t->sleep_cond);
    }
  else
    m->owner = NULL;
  // fprintf (stderr, "%ld unlocked\n", pthread_self ());
}

static void
fair_mutex_unlock (fair_mutex *m)
{
  fair_mutex_unlock_1 (m);
  scm_mutex_unlock (&m->lock);
}

/*  Temporarily give up the mutex.  This function makes sure that we
    are on the wait queue before starting the next thread.  Otherwise
    the next thread might preempt us and we will have a hard time
    getting on the wait queue.
*/
static void
fair_mutex_yield (fair_mutex *m)
{
  scm_thread *self = m->owner;
  fair_mutex_unlock_1 (m);
  fair_mutex_lock_1 (m, self);
}

static int
fair_cond_wait (scm_t_cond *c, fair_mutex *m)
{
  scm_thread *t = m->owner;
  int err;
  fair_mutex_unlock_1 (m);
  err = scm_cond_wait (c, &m->lock);
  fair_mutex_lock_1 (m, t);
  return err;
}

static int
fair_cond_timedwait (scm_t_cond *c, fair_mutex *m, struct timespec *at)
{
  int err;
  scm_thread *t = m->owner;
  fair_mutex_unlock_1 (m);
  err = scm_cond_timedwait (c, &m->lock, at);  /* XXX - signals? */
  fair_mutex_lock_1 (m, t);
  return err;
}

/*** Scheduling */

/* When a thread wants to execute Guile functions, it locks the
   guile_mutex.
*/

static fair_mutex guile_mutex;

static SCM cur_thread;
void *scm_i_thread_data;

void
scm_i_set_thread_data (void *data)
{
  scm_thread *t = SCM_THREAD_DATA (cur_thread);
  scm_i_thread_data = data;
  t->root = (scm_root_state *)data;
}
  
static void
resume (scm_thread *t)
{
  cur_thread = t->handle;
  scm_i_thread_data = t->root;
  t->top = NULL;
}

static void
enter_guile (scm_thread *t)
{
  fair_mutex_lock (&guile_mutex, t);
  resume (t);
}

static scm_thread *
suspend ()
{
  SCM cur = cur_thread;
  scm_thread *c = SCM_THREAD_DATA (cur);

  /* record top of stack for the GC */
  c->top = (SCM_STACKITEM *)&c;
  /* save registers. */
  SCM_FLUSH_REGISTER_WINDOWS;
  setjmp (c->regs);

  return c;
}

static scm_thread *
leave_guile ()
{
  scm_thread *c = suspend ();
  fair_mutex_unlock (&guile_mutex);
  return c;
}

int scm_i_switch_counter;

SCM
scm_yield ()
{
  /* Testing guile_mutex.next_waiting without locking guile_mutex.lock
     is OK since the outcome is not critical.  Even when it changes
     after the test, we do the right thing.
  */
  if (guile_mutex.next_waiting)
    {
      scm_thread *t = suspend ();
      fair_mutex_yield (&guile_mutex);
      resume (t);
    }
  return SCM_BOOL_T;
}

/* Put the current thread to sleep until it is explicitely unblocked.
 */
static int
block ()
{
  int err;
  scm_thread *t = suspend ();
  err = fair_cond_wait (&t->sleep_cond, &guile_mutex);
  resume (t);
  return err;
}

/* Put the current thread to sleep until it is explicitely unblocked
   or until a signal arrives or until time AT (absolute time) is
   reached.  Return 0 when it has been unblocked; errno otherwise.
 */
static int
timed_block (struct timespec *at)
{
  int err;
  scm_thread *t = suspend ();
  err = fair_cond_timedwait (&t->sleep_cond, &guile_mutex, at);
  resume (t);
  return err;
}

/* Unblock a sleeping thread.
 */
static void
unblock (scm_thread *t)
{
  scm_cond_signal (&t->sleep_cond);
}

/*** Thread creation */

static SCM all_threads;
static int thread_count;

typedef struct launch_data {
  SCM thread;
  SCM rootcont;
  scm_t_catch_body body;
  void *body_data;
  scm_t_catch_handler handler;
  void *handler_data;
} launch_data;

static SCM
body_bootstrip (launch_data* data)
{
  /* First save the new root continuation */
  data->rootcont = scm_root->rootcont;
  return (data->body) (data->body_data);
}

static SCM
handler_bootstrip (launch_data* data, SCM tag, SCM throw_args)
{
  scm_root->rootcont = data->rootcont;
  return (data->handler) (data->handler_data, tag, throw_args);
}

static void
really_launch (SCM_STACKITEM *base, launch_data *data)
{
  SCM thread = data->thread;
  scm_thread *t = SCM_THREAD_DATA (thread);
  init_thread_creatant (thread, base);
  enter_guile (t);

  data->rootcont = SCM_BOOL_F;
  t->result =
    scm_internal_cwdr ((scm_t_catch_body) body_bootstrip,
		       data,
		       (scm_t_catch_handler) handler_bootstrip,
		       data, base);
  free (data);

  scm_thread_detach (t->thread);
  all_threads = scm_delq (thread, all_threads);
  t->exited = 1;
  thread_count--;
  leave_guile ();
}

static void
launch_thread (void *p)
{
  really_launch ((SCM_STACKITEM *)&p, (launch_data *)p);
}

static SCM
create_thread (scm_t_catch_body body, void *body_data,
	       scm_t_catch_handler handler, void *handler_data,
	       SCM protects)
{
  SCM thread;

  /* Make new thread.  The first thing the new thread will do is to
     lock guile_mutex.  Thus, we can safely complete its
     initialization after creating it.  While the new thread starts,
     all its data is protected via all_threads.
   */

  {
    scm_t_thread th;
    SCM root, old_winds;
    launch_data *data;
    int err;

    /* Unwind wind chain. */
    old_winds = scm_dynwinds;
    scm_dowinds (SCM_EOL, scm_ilength (scm_root->dynwinds));

    /* Allocate thread locals. */
    root = scm_make_root (scm_root->handle);
    data = scm_malloc (sizeof (launch_data));

    /* Make thread. */
    thread = make_thread (protects);
    data->thread = thread;
    data->body = body;
    data->body_data = body_data;
    data->handler = handler;
    data->handler_data = handler_data;
    err = scm_thread_create (&th, launch_thread, (void *) data);
    if (err == 0)
      {
	init_thread_creator (thread, th, SCM_ROOT_STATE (root));
	all_threads = scm_cons (thread, all_threads);
	thread_count++;
      }
    else
      ((scm_thread *)SCM_THREAD_DATA(thread))->exited = 1;

    /* Return to old dynamic context. */
    scm_dowinds (old_winds, - scm_ilength (old_winds));

    if (err)
      {
	errno = err;
	scm_syserror ("create-thread");
      }
  }

  return thread;
}

SCM_DEFINE (scm_call_with_new_thread, "call-with-new-thread", 2, 0, 0,
	    (SCM thunk, SCM handler),
"Evaluate @var{(thunk)} in a new thread, and new dynamic context, "
"returning a new thread object representing the thread. "
"If an error occurs during evaluation, call error-thunk, passing it an "
"error code describing the condition. "
"If this happens, the error-thunk is called outside the scope of the new "
"root -- it is called in the same dynamic context in which "
"with-new-thread was evaluated, but not in the callers thread. "
"All the evaluation rules for dynamic roots apply to threads.")
#define FUNC_NAME s_scm_call_with_new_thread
{
  SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (thunk)), thunk, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (handler)), handler, SCM_ARG2,
	      FUNC_NAME);

  return create_thread ((scm_t_catch_body) scm_call_0, thunk,
			(scm_t_catch_handler) scm_apply_1, handler,
			scm_cons (thunk, handler));
}
#undef FUNC_NAME

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  return create_thread (body, body_data, handler, handler_data, SCM_BOOL_F);
}

SCM_DEFINE (scm_join_thread, "join-thread", 1, 0, 0,
	    (SCM thread),
"Suspend execution of the calling thread until the target @var{thread} "
"terminates, unless the target @var{thread} has already terminated. ")
#define FUNC_NAME s_scm_join_thread
{
  scm_thread *t;
  SCM res;

  SCM_VALIDATE_THREAD (1, thread);
  if (SCM_EQ_P (cur_thread, thread))
    SCM_MISC_ERROR ("can not join the current thread", SCM_EOL);

  t = SCM_THREAD_DATA (thread);
  if (!t->exited)
    {
      scm_thread *c = leave_guile ();
      scm_thread_join (t->thread);
      enter_guile (c);
    }
  res = t->result;
  t->result = SCM_BOOL_F;
  return res;
}
#undef FUNC_NAME

/*** Mutexes */

/* We implement our own mutex type since we want them to be 'fair', we
   want to do fancy things while waiting for them (like running
   asyncs) and we want to support waiting on many things at once.
   Also, we might add things that are nice for debugging.
*/

typedef struct scm_mutex {
  /* the thread currently owning the mutex, or SCM_BOOL_F. */
  SCM owner;
  /* how much the owner owns us. */
  int level;
  /* the threads waiting for this mutex. */
  SCM waiting;
} scm_mutex;

static SCM
mutex_mark (SCM mx)
{
  scm_mutex *m = SCM_MUTEX_DATA (mx);
  scm_gc_mark (m->owner);
  return m->waiting;
}

SCM_DEFINE (scm_make_mutex, "make-mutex", 0, 0, 0,
	    (void),
	    "Create a new mutex object. ")
#define FUNC_NAME s_scm_make_mutex
{
  SCM mx = scm_make_smob (scm_tc16_mutex);
  scm_mutex *m = SCM_MUTEX_DATA (mx);
  m->owner = SCM_BOOL_F;
  m->level = 0;
  m->waiting = make_queue ();
  return mx;
}
#undef FUNC_NAME

SCM_DEFINE (scm_lock_mutex, "lock-mutex", 1, 0, 0,
	    (SCM mx),
"Lock @var{mutex}. If the mutex is already locked, the calling thread "
"blocks until the mutex becomes available. The function returns when "
"the calling thread owns the lock on @var{mutex}.  Locking a mutex that "
"a thread already owns will succeed right away and will not block the "
"thread.  That is, Guile's mutexes are @emph{recursive}. ")
#define FUNC_NAME s_scm_lock_mutex
{
  scm_mutex *m;
  SCM_VALIDATE_MUTEX (1, mx);
  m = SCM_MUTEX_DATA (mx);

  if (m->owner == SCM_BOOL_F)
    m->owner = cur_thread;
  else if (m->owner == cur_thread)
    m->level++;
  else
    {
      while (1)
	{
	  SCM c = enqueue (m->waiting, cur_thread);
	  int err = block ();
	  if (m->owner == cur_thread)
	    return SCM_BOOL_T;
	  remqueue (m->waiting, c);
	  if (err)
	    {
	      errno = err;
	      scm_syserror (FUNC_NAME);
	    }
	  SCM_ASYNC_TICK;
	}
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_try_mutex, "try-mutex", 1, 0, 0,
	    (SCM mx),
"Try to lock @var{mutex}. If the mutex is already locked by someone "
"else, return @code{#f}.  Else lock the mutex and return @code{#t}. ")
#define FUNC_NAME s_scm_try_mutex
{
  scm_mutex *m;
  SCM_VALIDATE_MUTEX (1, mx);
  m = SCM_MUTEX_DATA (mx);

  if (m->owner == SCM_BOOL_F)
    m->owner = cur_thread;
  else if (m->owner == cur_thread)
    m->level++;
  else
    return SCM_BOOL_F;
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_unlock_mutex, "unlock-mutex", 1, 0, 0,
	    (SCM mx),
"Unlocks @var{mutex} if the calling thread owns the lock on "
"@var{mutex}.  Calling unlock-mutex on a mutex not owned by the current "
"thread results in undefined behaviour. Once a mutex has been unlocked, "
"one thread blocked on @var{mutex} is awakened and grabs the mutex "
"lock.  Every call to @code{lock-mutex} by this thread must be matched "
"with a call to @code{unlock-mutex}.  Only the last call to "
"@code{unlock-mutex} will actually unlock the mutex. ")
#define FUNC_NAME s_scm_unlock_mutex
{
  scm_mutex *m;
  SCM_VALIDATE_MUTEX (1, mx);
  m = SCM_MUTEX_DATA (mx);

  if (m->owner != cur_thread)
    {
      if (m->owner == SCM_BOOL_F)
	SCM_MISC_ERROR ("mutex not locked", SCM_EOL);
      else
	SCM_MISC_ERROR ("mutex not locked by this thread", SCM_EOL);
    }
  else if (m->level > 0)
    m->level--;
  else
    {
      SCM next = dequeue (m->waiting);
      if (!SCM_FALSEP (next))
	{
	  m->owner = next;
	  unblock (SCM_THREAD_DATA (next));
	  scm_yield ();
	}
      else
	m->owner = SCM_BOOL_F;
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*** Condition variables */

/* Like mutexes, we implement our own condition variables using the
   primitives above.
*/

/* yeah, we don't need a structure for this, but more things (like a
   name) will likely follow... */

typedef struct scm_cond {
  /* the threads waiting for this condition. */
  SCM waiting;
} scm_cond;

static SCM
cond_mark (SCM cv)
{
  scm_cond *c = SCM_CONDVAR_DATA (cv);
  return c->waiting;
}

SCM_DEFINE (scm_make_condition_variable, "make-condition-variable", 0, 0, 0,
	    (void),
	    "Make a new condition variable.")
#define FUNC_NAME s_scm_make_condition_variable
{
  SCM cv = scm_make_smob (scm_tc16_condvar);
  scm_cond *c = SCM_CONDVAR_DATA (cv);
  c->waiting = make_queue ();
  return cv;
}
#undef FUNC_NAME

SCM_DEFINE (scm_timed_wait_condition_variable, "wait-condition-variable", 2, 1, 0,
	    (SCM cv, SCM mx, SCM t),
"Wait until @var{cond-var} has been signalled.  While waiting, "
"@var{mutex} is atomically unlocked (as with @code{unlock-mutex}) and "
"is locked again when this function returns.  When @var{time} is given, "
"it specifies a point in time where the waiting should be aborted.  It "
"can be either a integer as returned by @code{current-time} or a pair "
"as returned by @code{gettimeofday}.  When the waiting is aborted the "
"mutex is locked and @code{#f} is returned.  When the condition "
"variable is in fact signalled, the mutex is also locked and @code{#t} "
"is returned. ")
#define FUNC_NAME s_scm_timed_wait_condition_variable
{
  scm_cond *c;
  struct timespec waittime;
  int err;

  SCM_VALIDATE_CONDVAR (1, cv);
  SCM_VALIDATE_MUTEX (2, mx);

  if (!SCM_UNBNDP (t))
    {
      if (SCM_CONSP (t))
	{
	  SCM_VALIDATE_UINT_COPY (3, SCM_CAR(t), waittime.tv_sec);
	  SCM_VALIDATE_UINT_COPY (3, SCM_CDR(t), waittime.tv_nsec);
	  waittime.tv_nsec *= 1000;
	}
      else
	{
	  SCM_VALIDATE_UINT_COPY (3, t, waittime.tv_sec);
	  waittime.tv_nsec = 0;
	}
    }

  c = SCM_CONDVAR_DATA (cv);

  while (1)
    {
      enqueue (c->waiting, cur_thread);
      scm_unlock_mutex (mx);
      if (SCM_UNBNDP (t))
	err = block ();
      else
	err = timed_block (&waittime);
      scm_lock_mutex (mx);
      if (err)
	{
	  errno = err;
	  scm_syserror (FUNC_NAME);
	}
      /* XXX - check whether we have been signalled. */
      break;
    }
  return SCM_BOOL (err == 0);
}
#undef FUNC_NAME

SCM
scm_wait_condition_variable (SCM c, SCM m)
{
  return scm_timed_wait_condition_variable (c, m, SCM_UNDEFINED);
}

SCM_DEFINE (scm_signal_condition_variable, "signal-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up one thread that is waiting for @var{cv}")
#define FUNC_NAME s_scm_signal_condition_variable
{
  SCM th;
  scm_cond *c;

  SCM_VALIDATE_CONDVAR (1, cv);

  c = SCM_CONDVAR_DATA (cv);
  if (!SCM_FALSEP (th = dequeue (c->waiting)))
    unblock (SCM_THREAD_DATA (th));
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_broadcast_condition_variable, "broadcast-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up all threads that are waiting for @var{cv}. ")
#define FUNC_NAME s_scm_broadcast_condition_variable
{
  SCM th;
  scm_cond *c;

  SCM_VALIDATE_CONDVAR (1, cv);

  c = SCM_CONDVAR_DATA (cv);
  while (!SCM_FALSEP (th = dequeue (c->waiting)))
    unblock (SCM_THREAD_DATA (th));
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*** Marking stacks */

/* XXX - what to do with this?  Do we need to handle this for blocked
   threads as well?
*/
#ifdef __ia64__
# define SCM_MARK_BACKING_STORE() do {                                \
    ucontext_t ctx;                                                   \
    SCM_STACKITEM * top, * bot;                                       \
    getcontext (&ctx);                                                \
    scm_mark_locations ((SCM_STACKITEM *) &ctx.uc_mcontext,           \
      ((size_t) (sizeof (SCM_STACKITEM) - 1 + sizeof ctx.uc_mcontext) \
       / sizeof (SCM_STACKITEM)));                                    \
    bot = (SCM_STACKITEM *) __libc_ia64_register_backing_store_base;  \
    top = (SCM_STACKITEM *) ctx.uc_mcontext.sc_ar_bsp;                \
    scm_mark_locations (bot, top - bot); } while (0)
#else
# define SCM_MARK_BACKING_STORE()
#endif

void
scm_threads_mark_stacks (void)
{
  volatile SCM c;
  for (c = all_threads; !SCM_NULLP (c); c = SCM_CDR (c))
    {
      scm_thread *t = SCM_THREAD_DATA (SCM_CAR (c));
      if (t->base == NULL)
	{
	  /* Not fully initialized yet. */
	  continue;
	}
      if (t->top == NULL)
	{
	  /* Active thread */
	  /* stack_len is long rather than sizet in order to guarantee
	     that &stack_len is long aligned */
#ifdef STACK_GROWS_UP
	  long stack_len = ((SCM_STACKITEM *) (&t) -
			    (SCM_STACKITEM *) thread->base);
	  
	  /* Protect from the C stack.  This must be the first marking
	   * done because it provides information about what objects
	   * are "in-use" by the C code.   "in-use" objects are  those
	   * for which the information about length and base address must
	   * remain usable.   This requirement is stricter than a liveness
	   * requirement -- in particular, it constrains the implementation
	   * of scm_resizuve.
	   */
	  SCM_FLUSH_REGISTER_WINDOWS;
	  /* This assumes that all registers are saved into the jmp_buf */
	  setjmp (scm_save_regs_gc_mark);
	  scm_mark_locations ((SCM_STACKITEM *) scm_save_regs_gc_mark,
			      ((size_t) sizeof scm_save_regs_gc_mark
			       / sizeof (SCM_STACKITEM)));
	  
	  scm_mark_locations (((size_t) t->base,
			       (sizet) stack_len));
#else
	  long stack_len = ((SCM_STACKITEM *) t->base -
			    (SCM_STACKITEM *) (&t));
	  
	  /* Protect from the C stack.  This must be the first marking
	   * done because it provides information about what objects
	   * are "in-use" by the C code.   "in-use" objects are  those
	   * for which the information about length and base address must
	   * remain usable.   This requirement is stricter than a liveness
	   * requirement -- in particular, it constrains the implementation
	   * of scm_resizuve.
	   */
	  SCM_FLUSH_REGISTER_WINDOWS;
	  /* This assumes that all registers are saved into the jmp_buf */
	  setjmp (scm_save_regs_gc_mark);
	  scm_mark_locations ((SCM_STACKITEM *) scm_save_regs_gc_mark,
			      ((size_t) sizeof scm_save_regs_gc_mark
			       / sizeof (SCM_STACKITEM)));
	  
	  scm_mark_locations ((SCM_STACKITEM *) &t,
			      stack_len);
#endif
	}
      else
	{
	  /* Suspended thread */
#ifdef STACK_GROWS_UP
	  long stack_len = t->top - t->base;
	  scm_mark_locations (t->base, stack_len);
#else
	  long stack_len = t->base - t->top;
	  scm_mark_locations (t->top, stack_len);
#endif
	  scm_mark_locations ((SCM_STACKITEM *) t->regs,
			      ((size_t) sizeof(t->regs)
			       / sizeof (SCM_STACKITEM)));
	}
    }
}

/*** Select */

int
scm_internal_select (int nfds,
		     SELECT_TYPE *readfds,
		     SELECT_TYPE *writefds,
		     SELECT_TYPE *exceptfds,
		     struct timeval *timeout)
{
  int res, eno;
  scm_thread *c = leave_guile ();
  res = scm_thread_select (nfds, readfds, writefds, exceptfds, timeout);
  eno = errno;
  enter_guile (c);
  SCM_ASYNC_TICK;
  errno = eno;
  return res;
}

unsigned long
scm_thread_usleep (unsigned long usecs)
{
  struct timeval tv;
  tv.tv_usec = usecs % 1000000;
  tv.tv_sec = usecs / 1000000;
  scm_internal_select (0, NULL, NULL, NULL, &tv);
  return tv.tv_usec + tv.tv_sec*1000000;
}

unsigned long
scm_thread_sleep (unsigned long secs)
{
  struct timeval tv;
  tv.tv_usec = 0;
  tv.tv_sec = secs;
  scm_internal_select (0, NULL, NULL, NULL, &tv);
  return tv.tv_sec;
}

/*** Misc */

SCM_DEFINE (scm_current_thread, "current-thread", 0, 0, 0,
	    (void),
	    "Return the thread that called this function.")
#define FUNC_NAME s_scm_current_thread
{
  return cur_thread;
}
#undef FUNC_NAME

SCM_DEFINE (scm_all_threads, "all-threads", 0, 0, 0,
	    (void),
	    "Return a list of all threads.")
#define FUNC_NAME s_all_threads
{
  return all_threads;
}
#undef FUNC_NAME

scm_root_state *
scm_i_thread_root (SCM thread)
{
  return ((scm_thread *)SCM_THREAD_DATA (thread))->root;
}

SCM_DEFINE (scm_thread_exited_p, "thread-exited?", 1, 0, 0,
	    (SCM thread),
	    "Return @code{#t} iff @var{thread} has exited.\n")
#define FUNC_NAME s_scm_thread_exited_p
{
  return SCM_BOOL (scm_c_thread_exited_p (thread));
}
#undef FUNC_NAME

int
scm_c_thread_exited_p (SCM thread)
#define FUNC_NAME  s_scm_thread_exited_p
{
  scm_thread *t;
  SCM_VALIDATE_THREAD (1, thread);
  t = SCM_THREAD_DATA (thread);
  return t->exited;
}
#undef FUNC_NAME

/*** Initialization */

scm_t_bits scm_tc16_thread;
scm_t_bits scm_tc16_mutex;
scm_t_bits scm_tc16_condvar;

void
scm_init_threads (SCM_STACKITEM *base)
{
  scm_tc16_thread = scm_make_smob_type ("thread", sizeof (scm_thread));
  scm_tc16_mutex = scm_make_smob_type ("mutex", sizeof (scm_mutex));
  scm_tc16_condvar = scm_make_smob_type ("condition-variable",
					 sizeof (scm_cond));

  scm_i_switch_counter = SCM_I_THREAD_SWITCH_COUNT;

  fair_mutex_init (&guile_mutex);

  cur_thread = make_thread (SCM_BOOL_F);
  enter_guile (SCM_THREAD_DATA (cur_thread));
  /* root is set later from init.c */
  init_thread_creator (cur_thread, scm_thread_self(), NULL);
  init_thread_creatant (cur_thread, base);

  thread_count = 1;
  scm_gc_register_root (&all_threads);
  all_threads = scm_cons (cur_thread, SCM_EOL);

  scm_set_smob_mark (scm_tc16_thread, thread_mark);
  scm_set_smob_print (scm_tc16_thread, thread_print);
  scm_set_smob_free (scm_tc16_thread, thread_free);

  scm_set_smob_mark (scm_tc16_mutex, mutex_mark);

  scm_set_smob_mark (scm_tc16_condvar, cond_mark);
}

void
scm_init_thread_procs ()
{
#include "libguile/threads.x"
}

/* XXX */

void
scm_init_iselect ()
{
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/


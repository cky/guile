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

#define THREAD_INITIALIZED_P(t) (t->base != NULL)

struct scm_thread {
  
  /* Blocking.
   */
  scm_t_cond sleep_cond;
  struct scm_thread *next_waiting;

  /* This mutex represents this threads right to access the heap.
     That right can temporarily be taken away by the GC.  */
  scm_t_mutex heap_mutex;
  int clear_freelists_p; /* set if GC was done while thread was asleep */
  
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

};

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
  scm_i_plugin_cond_init (&t->sleep_cond, 0);
  scm_i_plugin_mutex_init (&t->heap_mutex, 0);
  t->clear_freelists_p = 0;
  t->exited = 0;
  return z;
}

static void
init_thread_creatant (SCM thread,
		      SCM_STACKITEM *base)
{
  scm_thread *t = SCM_THREAD_DATA (thread);
  t->thread = scm_thread_self ();
  t->base = base;
  t->top = NULL;
}

static SCM
thread_mark (SCM obj)
{
  scm_thread *t = SCM_THREAD_DATA (obj);
  scm_gc_mark (t->result);
  scm_gc_mark (t->joining_threads);
  return t->root->handle; /* mark root-state of this thread */
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

/*** Scheduling */

#define cur_thread (SCM_CURRENT_THREAD->handle)
scm_t_key scm_i_thread_key;
scm_t_key scm_i_root_state_key;

void
scm_i_set_thread_data (void *data)
{
  scm_thread *t = SCM_CURRENT_THREAD;
  scm_setspecific (scm_i_root_state_key, data);
  t->root = (scm_root_state *)data;
}
  
static void
resume (scm_thread *t)
{
  t->top = NULL;
  if (t->clear_freelists_p)
    {
      *SCM_FREELIST_LOC (scm_i_freelist) = SCM_EOL;
      *SCM_FREELIST_LOC (scm_i_freelist2) = SCM_EOL;
      t->clear_freelists_p = 0;
    }
}

void
scm_i_enter_guile (scm_thread *t)
{
  scm_i_plugin_mutex_lock (&t->heap_mutex);
  resume (t);
}

static scm_thread *
suspend ()
{
  scm_thread *c = SCM_CURRENT_THREAD;

  /* record top of stack for the GC */
  c->top = (SCM_STACKITEM *)&c;
  /* save registers. */
  SCM_FLUSH_REGISTER_WINDOWS;
  setjmp (c->regs);

  return c;
}

scm_thread *
scm_i_leave_guile ()
{
  scm_thread *t = suspend ();
  scm_i_plugin_mutex_unlock (&t->heap_mutex);
  return t;
}

/* Put the current thread to sleep until it is explicitely unblocked.
 */
static int
block ()
{
  int err;
  scm_thread *t = suspend ();
  err = scm_i_plugin_cond_wait (&t->sleep_cond, &t->heap_mutex);
  resume (t);
  return err;
}

/* Put the current thread to sleep until it is explicitely unblocked
   or until a signal arrives or until time AT (absolute time) is
   reached.  Return 0 when it has been unblocked; errno otherwise.
 */
static int
timed_block (const struct timespec *at)
{
  int err;
  scm_thread *t = suspend ();
  err = scm_i_plugin_cond_timedwait (&t->sleep_cond, &t->heap_mutex, at);
  resume (t);
  return err;
}

/* Unblock a sleeping thread.
 */
static void
unblock (scm_thread *t)
{
  scm_i_plugin_cond_signal (&t->sleep_cond);
}

/*** Thread creation */

static scm_t_mutex thread_admin_mutex;
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
  SCM thread;
  scm_thread *t;
  thread = data->thread;
  t = SCM_THREAD_DATA (thread);
  SCM_FREELIST_CREATE (scm_i_freelist);
  SCM_FREELIST_CREATE (scm_i_freelist2);
  scm_setspecific (scm_i_thread_key, t);
  scm_setspecific (scm_i_root_state_key, t->root);
  scm_i_plugin_mutex_lock (&t->heap_mutex); /* ensure that we "own" the heap */
  init_thread_creatant (thread, base); /* must own the heap */
  
  data->rootcont = SCM_BOOL_F;
  t->result =
    scm_internal_cwdr ((scm_t_catch_body) body_bootstrip,
		       data,
		       (scm_t_catch_handler) handler_bootstrip,
		       data, base);
  free (data);

  scm_thread_detach (t->thread);
  scm_i_plugin_mutex_lock (&thread_admin_mutex);
  all_threads = scm_delq_x (thread, all_threads);
  t->exited = 1;
  thread_count--;
  scm_i_plugin_mutex_unlock (&thread_admin_mutex);
  /* We're leaving with heap_mutex still locked. */
}

static void *
launch_thread (void *p)
{
  really_launch ((SCM_STACKITEM *)&p, (launch_data *)p);
  return 0;
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
    SCM root, old_winds, new_threads;
    launch_data *data;
    scm_thread *t;
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
    t = SCM_THREAD_DATA (thread);
    /* must initialize root state pointer before the thread is linked
       into all_threads */
    t->root = SCM_ROOT_STATE (root);
    
    /* In order to avoid the need of synchronization between parent
       and child thread, we need to insert the child into all_threads
       before creation. */
    new_threads = scm_cons (thread, SCM_BOOL_F); /* could cause GC */
    scm_i_plugin_mutex_lock (&thread_admin_mutex);
    SCM_SETCDR (new_threads, all_threads);
    all_threads = new_threads;
    thread_count++;
    scm_i_plugin_mutex_unlock (&thread_admin_mutex);
    
    err = scm_i_plugin_thread_create (&th, 0, launch_thread, (void *) data);
    if (err != 0)
      {
	scm_i_plugin_mutex_lock (&thread_admin_mutex);
	all_threads = scm_delq_x (thread, all_threads);
	((scm_thread *) SCM_THREAD_DATA(thread))->exited = 1;
	thread_count--;
	scm_i_plugin_mutex_unlock (&thread_admin_mutex);
      }

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
      scm_thread *c = scm_i_leave_guile ();
      while (!THREAD_INITIALIZED_P (t))
	SCM_TICK;
      scm_thread_join (t->thread, 0);
      scm_i_enter_guile (c);
    }
  res = t->result;
  t->result = SCM_BOOL_F;
  return res;
}
#undef FUNC_NAME

/*** Fair mutexes */

/* We implement our own mutex type since we want them to be 'fair', we
   want to do fancy things while waiting for them (like running
   asyncs) and we want to support waiting on many things at once.
   Also, we might add things that are nice for debugging.
*/

typedef struct fair_mutex {
  /* the thread currently owning the mutex, or SCM_BOOL_F. */
  scm_t_mutex lock;
  int lockedp;
  SCM owner;
  /* how much the owner owns us. */
  int level;
  /* the threads waiting for this mutex. */
  SCM waiting;
} fair_mutex;

static SCM
fair_mutex_mark (SCM mx)
{
  fair_mutex *m = SCM_MUTEX_DATA (mx);
  scm_gc_mark (m->owner);
  return m->waiting;
}

SCM_DEFINE (scm_make_fair_mutex, "make-fair-mutex", 0, 0, 0,
	    (void),
	    "Create a new fair mutex object. ")
#define FUNC_NAME s_scm_make_fair_mutex
{
  SCM mx = scm_make_smob (scm_tc16_fair_mutex);
  fair_mutex *m = SCM_MUTEX_DATA (mx);
  scm_i_plugin_mutex_init (&m->lock, 0);
  m->lockedp = 0;
  m->owner = SCM_BOOL_F;
  m->level = 0;
  m->waiting = make_queue ();
  return mx;
}
#undef FUNC_NAME

static int
fair_mutex_lock (fair_mutex *m)
{
  scm_i_plugin_mutex_lock (&m->lock);
#if 0
  /* Need to wait if another thread is just temporarily unlocking.
     This is happens very seldom and only when the other thread is
     between scm_mutex_unlock and scm_i_plugin_mutex_lock below. */
  while (m->lockedp)
    SCM_TICK;
    m->lockedp = 1;
#endif
  
  if (m->owner == SCM_BOOL_F)
    m->owner = cur_thread;
  else if (m->owner == cur_thread)
    m->level++;
  else
    {
      while (1)
	{
	  SCM c = enqueue (m->waiting, cur_thread);
	  int err;
	  /* Note: It's important that m->lock is never locked for
	     any longer amount of time since that could prevent GC */
	  scm_i_plugin_mutex_unlock (&m->lock);
	  err = block ();
	  if (m->owner == cur_thread)
	    return 0;
	  scm_i_plugin_mutex_lock (&m->lock);
	  remqueue (m->waiting, c);
	  scm_i_plugin_mutex_unlock (&m->lock);
	  if (err)
	    return err;
	  SCM_ASYNC_TICK;
	  scm_i_plugin_mutex_lock (&m->lock);
	}
    }
  scm_i_plugin_mutex_unlock (&m->lock);
  return 0;
}

static int
fair_mutex_trylock (fair_mutex *m)
{
  scm_i_plugin_mutex_lock (&m->lock);
  if (m->owner == SCM_BOOL_F)
    m->owner = cur_thread;
  else if (m->owner == cur_thread)
    m->level++;
  else
    {
      scm_i_plugin_mutex_unlock (&m->lock);
      return EBUSY;
    }
  scm_i_plugin_mutex_unlock (&m->lock);
  return 0;
}

static int
fair_mutex_unlock (fair_mutex *m)
{
  scm_i_plugin_mutex_lock (&m->lock);
  if (m->owner != cur_thread)
    {
      scm_i_plugin_mutex_unlock (&m->lock);
      return EPERM;
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
	}
      else
	m->owner = SCM_BOOL_F;
    }
  scm_i_plugin_mutex_unlock (&m->lock);
  return 0;
}

/*** Fair condition variables */

/* Like mutexes, we implement our own condition variables using the
   primitives above.
*/

typedef struct fair_cond {
  scm_t_mutex lock;
  /* the threads waiting for this condition. */
  SCM waiting;
} fair_cond;

static SCM
fair_cond_mark (SCM cv)
{
  fair_cond *c = SCM_CONDVAR_DATA (cv);
  return c->waiting;
}

SCM_DEFINE (scm_make_fair_condition_variable, "make-fair-condition-variable", 0, 0, 0,
	    (void),
	    "Make a new fair condition variable.")
#define FUNC_NAME s_scm_make_fair_condition_variable
{
  SCM cv = scm_make_smob (scm_tc16_fair_condvar);
  fair_cond *c = SCM_CONDVAR_DATA (cv);
  scm_i_plugin_mutex_init (&c->lock, 0);
  c->waiting = make_queue ();
  return cv;
}
#undef FUNC_NAME

static int
fair_cond_timedwait (fair_cond *c,
		     fair_mutex *m,
		     const struct timespec *waittime)
{
  int err;
  scm_i_plugin_mutex_lock (&c->lock);

  while (1)
    {
      enqueue (c->waiting, cur_thread);
      scm_i_plugin_mutex_unlock (&c->lock);
      fair_mutex_unlock (m); /*fixme* - not thread safe */
      if (waittime == NULL)
	err = block ();
      else
	err = timed_block (waittime);
      fair_mutex_lock (m);
      if (err)
	return err;
      /* XXX - check whether we have been signalled. */
      break;
    }
  return err;
}

static int
fair_cond_signal (fair_cond *c)
{
  SCM th;
  scm_i_plugin_mutex_lock (&c->lock);
  if (!SCM_FALSEP (th = dequeue (c->waiting)))
    unblock (SCM_THREAD_DATA (th));
  scm_i_plugin_mutex_unlock (&c->lock);
  return 0;
}

static int
fair_cond_broadcast (fair_cond *c)
{
  SCM th;
  scm_i_plugin_mutex_lock (&c->lock);
  while (!SCM_FALSEP (th = dequeue (c->waiting)))
    unblock (SCM_THREAD_DATA (th));
  scm_i_plugin_mutex_unlock (&c->lock);
  return 0;
}

/*** Mutexes */

SCM_DEFINE (scm_make_mutex, "make-mutex", 0, 0, 0,
	    (void),
	    "Create a new mutex object. ")
#define FUNC_NAME s_scm_make_mutex
{
  SCM mx = scm_make_smob (scm_tc16_mutex);
  scm_i_plugin_mutex_init (SCM_MUTEX_DATA (mx), 0);
  return mx;
}
#undef FUNC_NAME

/*fixme* change documentation */
SCM_DEFINE (scm_lock_mutex, "lock-mutex", 1, 0, 0,
	    (SCM mx),
"Lock @var{mutex}. If the mutex is already locked, the calling thread "
"blocks until the mutex becomes available. The function returns when "
"the calling thread owns the lock on @var{mutex}.  Locking a mutex that "
"a thread already owns will succeed right away and will not block the "
"thread.  That is, Guile's mutexes are @emph{recursive}. ")
#define FUNC_NAME s_scm_lock_mutex
{
  int err;
  SCM_VALIDATE_MUTEX (1, mx);
  
  if (SCM_TYP16 (mx) == scm_tc16_fair_mutex)
    err = fair_mutex_lock (SCM_MUTEX_DATA (mx));
  else
    {
      scm_t_mutex *m = SCM_MUTEX_DATA (mx);
      scm_thread *t = scm_i_leave_guile ();
      err = scm_i_plugin_mutex_lock (m);
      scm_i_enter_guile (t);
    }

  if (err)
    {
      errno = err;
      SCM_SYSERROR;
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
  int err;
  SCM_VALIDATE_MUTEX (1, mx);
  
  if (SCM_TYP16 (mx) == scm_tc16_fair_mutex)
    err = fair_mutex_trylock (SCM_MUTEX_DATA (mx));
  else
    {
      scm_t_mutex *m = SCM_MUTEX_DATA (mx);
      scm_thread *t = scm_i_leave_guile ();
      err = scm_i_plugin_mutex_trylock (m);
      scm_i_enter_guile (t);
    }

  if (err == EBUSY)
    return SCM_BOOL_F;
  
  if (err)
    {
      errno = err;
      SCM_SYSERROR;
    }
  
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
  int err;
  SCM_VALIDATE_MUTEX (1, mx);
  
  if (SCM_TYP16 (mx) == scm_tc16_fair_mutex)
    {
      err = fair_mutex_unlock (SCM_MUTEX_DATA (mx));
      if (err == EPERM)
	{
	  fair_mutex *m = SCM_MUTEX_DATA (mx);
	  if (m->owner != cur_thread)
	    {
	      if (m->owner == SCM_BOOL_F)
		SCM_MISC_ERROR ("mutex not locked", SCM_EOL);
	      else
		SCM_MISC_ERROR ("mutex not locked by this thread", SCM_EOL);
	    }
	}
    }
  else
    {
      scm_t_mutex *m = SCM_MUTEX_DATA (mx);
      err = scm_i_plugin_mutex_unlock (m);
    }

  if (err)
    {
      errno = err;
      SCM_SYSERROR;
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*** Condition variables */

SCM_DEFINE (scm_make_condition_variable, "make-condition-variable", 0, 0, 0,
	    (void),
	    "Make a new condition variable.")
#define FUNC_NAME s_scm_make_condition_variable
{
  SCM cv = scm_make_smob (scm_tc16_condvar);
  scm_i_plugin_cond_init (SCM_CONDVAR_DATA (cv), 0);
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
  struct timespec waittime;
  int err;

  SCM_VALIDATE_CONDVAR (1, cv);
  SCM_VALIDATE_MUTEX (2, mx);
  if (!((SCM_TYP16 (cv) == scm_tc16_condvar
	 && SCM_TYP16 (mx) == scm_tc16_mutex)
	|| (SCM_TYP16 (cv) == scm_tc16_fair_condvar
	    && SCM_TYP16 (mx) == scm_tc16_fair_mutex)))
    SCM_MISC_ERROR ("Condition variable and mutex are of different kinds.",
		    SCM_EOL);
  
  if (!SCM_UNBNDP (t))
    {
      if (SCM_CONSP (t))
	{
	  SCM_VALIDATE_UINT_COPY (3, SCM_CAR (t), waittime.tv_sec);
	  SCM_VALIDATE_UINT_COPY (3, SCM_CDR (t), waittime.tv_nsec);
	  waittime.tv_nsec *= 1000;
	}
      else
	{
	  SCM_VALIDATE_UINT_COPY (3, t, waittime.tv_sec);
	  waittime.tv_nsec = 0;
	}
    }

  if (SCM_TYP16 (cv) == scm_tc16_fair_condvar)
    err = fair_cond_timedwait (SCM_CONDVAR_DATA (cv),
			       SCM_MUTEX_DATA (mx),
			       SCM_UNBNDP (t) ? NULL : &waittime);
  else
    {
      scm_t_cond *c = SCM_CONDVAR_DATA (cv);
      scm_t_mutex *m = SCM_MUTEX_DATA (mx);
      scm_thread *t = scm_i_leave_guile ();
      err = scm_i_plugin_cond_wait (c, m);
      scm_i_enter_guile (t);
    }

  if (err)
    {
      errno = err;
      SCM_SYSERROR;
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_signal_condition_variable, "signal-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up one thread that is waiting for @var{cv}")
#define FUNC_NAME s_scm_signal_condition_variable
{
  SCM_VALIDATE_CONDVAR (1, cv);
  if (SCM_TYP16 (cv) == scm_tc16_fair_condvar)
    fair_cond_signal (SCM_CONDVAR_DATA (cv));
  else
    {
      scm_t_cond *c = SCM_CONDVAR_DATA (cv);
      scm_i_plugin_cond_signal (c);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_broadcast_condition_variable, "broadcast-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up all threads that are waiting for @var{cv}. ")
#define FUNC_NAME s_scm_broadcast_condition_variable
{
  SCM_VALIDATE_CONDVAR (1, cv);
  if (SCM_TYP16 (cv) == scm_tc16_fair_condvar)
    fair_cond_broadcast (SCM_CONDVAR_DATA (cv));
  else
    {
      scm_t_cond *c = SCM_CONDVAR_DATA (cv);
      scm_i_plugin_cond_broadcast (c);
    }
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
      if (!THREAD_INITIALIZED_P (t))
	{
	  /* Not fully initialized yet. */
	  continue;
	}
      if (t->top == NULL)
	{
	  long stack_len;
#ifdef SCM_DEBUG
	  if (t->thread != scm_thread_self ())
	    abort ();
#endif
	  /* Active thread */
	  /* stack_len is long rather than sizet in order to guarantee
	     that &stack_len is long aligned */
#ifdef STACK_GROWS_UP
	  stack_len = ((SCM_STACKITEM *) (&t) -
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
	  stack_len = ((SCM_STACKITEM *) t->base -
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
  scm_thread *c = scm_i_leave_guile ();
  res = scm_i_plugin_select (nfds, readfds, writefds, exceptfds, timeout);
  eno = errno;
  scm_i_enter_guile (c);
  SCM_ASYNC_TICK;
  errno = eno;
  return res;
}

/* Low-level C API */

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  return create_thread (body, body_data, handler, handler_data, SCM_BOOL_F);
}

int
scm_mutex_lock (scm_t_mutex *m)
{
  scm_thread *t = scm_i_leave_guile ();
  int res = scm_i_plugin_mutex_lock (m);
  scm_i_enter_guile (t);
  return res;
}

int
scm_cond_wait (scm_t_cond *c, scm_t_mutex *m)
{
  scm_thread *t = scm_i_leave_guile ();
  scm_i_plugin_cond_wait (c, m);
  scm_i_enter_guile (t);
  return 0;
}

int
scm_cond_timedwait (scm_t_cond *c, scm_t_mutex *m, const struct timespec *wt)
{
  scm_thread *t = scm_i_leave_guile ();
  int res = scm_i_plugin_cond_timedwait (c, m, wt);
  scm_i_enter_guile (t);
  return res;
}

void
scm_enter_guile ()
{
  scm_i_enter_guile (SCM_CURRENT_THREAD);
}

void
scm_leave_guile ()
{
  scm_i_leave_guile ();
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
#define FUNC_NAME s_scm_all_threads
{
  return all_threads;
}
#undef FUNC_NAME

scm_root_state *
scm_i_thread_root (SCM thread)
{
  return ((scm_thread *) SCM_THREAD_DATA (thread))->root;
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

static scm_t_cond wake_up_cond;
int scm_i_thread_go_to_sleep;
static scm_thread *gc_thread;
static scm_t_mutex gc_section_mutex;
static scm_thread *gc_section_owner;
static int gc_section_count = 0;
static int threads_initialized_p = 0;

void
scm_i_thread_put_to_sleep ()
{
  SCM_REC_CRITICAL_SECTION_START (gc_section);
  if (threads_initialized_p && gc_section_count == 1)
    {
      SCM threads = all_threads;
      /* Signal all threads to go to sleep */
      scm_i_thread_go_to_sleep = 1;
      for (; !SCM_NULLP (threads); threads = SCM_CDR (threads))
	if (SCM_CAR (threads) != cur_thread)
	  {
	    scm_thread *t = SCM_THREAD_DATA (SCM_CAR (threads));
	    t->clear_freelists_p = 1;
	    scm_i_plugin_mutex_lock (&t->heap_mutex);
	  }
      gc_thread = suspend ();
      scm_i_thread_go_to_sleep = 0;
    }
}

void
scm_i_thread_wake_up ()
{
  if (threads_initialized_p && gc_section_count == 1)
    {
      SCM threads = all_threads;
      resume (gc_thread);
      scm_i_plugin_cond_broadcast (&wake_up_cond);
      for (; !SCM_NULLP (threads); threads = SCM_CDR (threads))
	if (SCM_CAR (threads) != cur_thread)
	  {
	    scm_thread *t = SCM_THREAD_DATA (SCM_CAR (threads));
	    scm_i_plugin_mutex_unlock (&t->heap_mutex);
	  }
    }
  SCM_REC_CRITICAL_SECTION_END (gc_section);
}

void
scm_i_thread_sleep_for_gc ()
{
  scm_thread *t;
  t = suspend ();
  *SCM_FREELIST_LOC (scm_i_freelist) = SCM_EOL;
  *SCM_FREELIST_LOC (scm_i_freelist2) = SCM_EOL;
  scm_i_plugin_cond_wait (&wake_up_cond, &t->heap_mutex);
  t->clear_freelists_p = 0;
  t->top = NULL; /* resume (t); but don't clear freelists */
}

/* The mother of all recursive critical sections */
scm_t_mutex scm_i_section_mutex;

scm_t_mutex scm_i_critical_section_mutex;
scm_t_mutex scm_i_defer_mutex;
int scm_i_defer_count = 0;
scm_thread *scm_i_defer_owner = 0;

/*** Initialization */

void
scm_threads_prehistory ()
{
  scm_thread *t;
  scm_i_plugin_mutex_init (&thread_admin_mutex, 0);
  scm_i_plugin_mutex_init (&gc_section_mutex, 0);
  scm_i_plugin_cond_init (&wake_up_cond, 0);
  scm_i_plugin_mutex_init (&scm_i_critical_section_mutex, 0);
  thread_count = 1;
  scm_i_plugin_key_create (&scm_i_thread_key, 0);
  scm_i_plugin_key_create (&scm_i_root_state_key, 0);
  scm_i_plugin_mutex_init (&scm_i_defer_mutex, 0);
  scm_i_plugin_mutex_init (&scm_i_section_mutex, 0);
  /* Allocate a fake thread object to be used during bootup. */
  t = malloc (sizeof (scm_thread));
  t->base = NULL;
  t->clear_freelists_p = 0;
  scm_setspecific (scm_i_thread_key, t);
}

scm_t_bits scm_tc16_thread;
scm_t_bits scm_tc16_mutex;
scm_t_bits scm_tc16_fair_mutex;
scm_t_bits scm_tc16_condvar;
scm_t_bits scm_tc16_fair_condvar;

void
scm_init_threads (SCM_STACKITEM *base)
{
  SCM thread;
  scm_tc16_thread = scm_make_smob_type ("thread", sizeof (scm_thread));
  scm_tc16_mutex = scm_make_smob_type ("mutex", sizeof (scm_t_mutex));
  scm_tc16_fair_mutex = scm_make_smob_type ("fair-mutex",
					    sizeof (fair_mutex));
  scm_tc16_condvar = scm_make_smob_type ("condition-variable",
					 sizeof (scm_t_cond));
  scm_tc16_fair_condvar = scm_make_smob_type ("fair-condition-variable",
					      sizeof (fair_cond));

  thread = make_thread (SCM_BOOL_F);
  /* Replace initial fake thread with a real thread object */
  free (SCM_CURRENT_THREAD);
  scm_setspecific (scm_i_thread_key, SCM_THREAD_DATA (thread));
  scm_i_enter_guile (SCM_CURRENT_THREAD);

  /* root is set later from init.c */
  init_thread_creatant (thread, base);
  thread_count = 1;
  scm_gc_register_root (&all_threads);
  all_threads = scm_cons (thread, SCM_EOL);

  scm_set_smob_mark (scm_tc16_thread, thread_mark);
  scm_set_smob_print (scm_tc16_thread, thread_print);
  scm_set_smob_free (scm_tc16_thread, thread_free);

  scm_set_smob_mark (scm_tc16_fair_mutex, fair_mutex_mark);

  scm_set_smob_mark (scm_tc16_fair_condvar, fair_cond_mark);

  threads_initialized_p = 1;
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

/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */




#include "libguile/_scm.h"

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#include <assert.h>
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "libguile/validate.h"
#include "libguile/root.h"
#include "libguile/eval.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/threads.h"
#include "libguile/dynwind.h"
#include "libguile/iselect.h"
#include "libguile/fluids.h"
#include "libguile/continuations.h"
#include "libguile/init.h"

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
  if (scm_is_null (SCM_CDR (q)))
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
  for (p = SCM_CDR (q); !scm_is_null (p); p = SCM_CDR (p))
    {
      if (scm_is_eq (p, c))
	{
	  if (scm_is_eq (c, SCM_CAR (q)))
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
  if (scm_is_null (c))
    return SCM_BOOL_F;
  else
    {
      SCM_SETCDR (q, SCM_CDR (c));
      if (scm_is_null (SCM_CDR (q)))
	SCM_SETCAR (q, SCM_EOL);
      return SCM_CAR (c);
    }
}

/*** Threads */

static SCM
thread_mark (SCM obj)
{
  scm_thread *t = SCM_THREAD_DATA (obj);
  scm_gc_mark (t->result);
  return t->root;
}

static int
thread_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_thread *t = SCM_THREAD_DATA (exp);
  scm_puts ("#<thread ", port);
  scm_uintprint ((size_t)t->pthread, 10, port);
  scm_puts (" (", port);
  scm_uintprint ((scm_t_bits)t, 16, port);
  scm_puts (")>", port);
  return 1;
}

static size_t
thread_free (SCM obj)
{
  scm_thread *t = SCM_THREAD_DATA (obj);
  assert (t->exited);
  scm_gc_free (t, sizeof (*t), "thread");
  return 0;
}

/*** Scheduling */

#define cur_thread (SCM_CURRENT_THREAD->handle)
pthread_key_t scm_i_thread_key;

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

static void
scm_i_enter_guile (scm_thread *t)
{
  pthread_mutex_lock (&t->heap_mutex);
  resume (t);
}

static scm_thread *
suspend ()
{
  scm_thread *c = SCM_CURRENT_THREAD;

  /* record top of stack for the GC */
  c->top = SCM_STACK_PTR (&c);
  /* save registers. */
  SCM_FLUSH_REGISTER_WINDOWS;
  setjmp (c->regs);

  return c;
}

static scm_thread *
scm_i_leave_guile ()
{
  scm_thread *t = suspend ();
  pthread_mutex_unlock (&t->heap_mutex);
  return t;
}

/* Put the current thread to sleep until it is explicitely unblocked.
 */
static int
block ()
{
  int err;
  scm_thread *t = suspend ();
  err = pthread_cond_wait (&t->sleep_cond, &t->heap_mutex);
  resume (t);
  return err;
}

/* Put the current thread to sleep until it is explicitely unblocked
   or until a signal arrives or until time AT (absolute time) is
   reached.  Return 0 when it has been unblocked; errno otherwise.
 */
static int
timed_block (const scm_t_timespec *at)
{
  int err;
  scm_thread *t = suspend ();
  err = pthread_cond_timedwait (&t->sleep_cond, &t->heap_mutex, at);
  resume (t);
  return err;
}

/* Unblock a sleeping thread.
 */
static void
unblock (scm_thread *t)
{
  pthread_cond_signal (&t->sleep_cond);
}

/* Getting into and out of guile mode.
 */

static pthread_mutex_t thread_admin_mutex = PTHREAD_MUTEX_INITIALIZER;
static scm_thread *all_threads = NULL;
static int thread_count;

static void
restart_stack (void *base)
{
  scm_dynwinds = SCM_EOL;
  SCM_DYNENV (scm_rootcont) = SCM_EOL;
  SCM_THROW_VALUE (scm_rootcont) = SCM_EOL;
  SCM_DFRAME (scm_rootcont) = scm_last_debug_frame = 0;
  SCM_BASE (scm_rootcont) = base;
}

static void
start_stack (void *base)
{
  scm_stack_base = base;
  scm_root->fluids = scm_i_make_initial_fluids ();

  /* Create an object to hold the root continuation.
   */
  {
    scm_t_contregs *contregs = scm_gc_malloc (sizeof (scm_t_contregs),
					      "continuation");
    contregs->num_stack_items = 0;
    contregs->seq = 0;
    SCM_NEWSMOB (scm_rootcont, scm_tc16_continuation, contregs);
  }

  /* The remainder of stack initialization is factored out to another
   * function so that if this stack is ever exitted, it can be
   * re-entered using restart_stack.  */
  restart_stack (base);
}

static SCM scm_i_root_root;

static void
guilify_self_1 (SCM_STACKITEM *base)
{
  scm_thread *t = malloc (sizeof (scm_thread));

  t->pthread = pthread_self ();
  t->handle = SCM_BOOL_F;
  t->root = SCM_BOOL_F;
  t->result = SCM_BOOL_F;
  t->base = base;
  pthread_cond_init (&t->sleep_cond, NULL);
  pthread_mutex_init (&t->heap_mutex, NULL);
  t->clear_freelists_p = 0;
  t->exited = 0;

  t->freelist = SCM_EOL;
  t->freelist2 = SCM_EOL;
  SCM_SET_FREELIST_LOC (scm_i_freelist, &t->freelist);
  SCM_SET_FREELIST_LOC (scm_i_freelist2, &t->freelist2);

  pthread_setspecific (scm_i_thread_key, t);

  pthread_mutex_lock (&t->heap_mutex);

  pthread_mutex_lock (&thread_admin_mutex);
  t->next_thread = all_threads;
  all_threads = t;
  thread_count++;
  pthread_mutex_unlock (&thread_admin_mutex);
}

static void
guilify_self_2 (SCM parent)
{
  scm_thread *t = SCM_CURRENT_THREAD;

  SCM_NEWSMOB (t->handle, scm_tc16_thread, t);
  scm_gc_register_collectable_memory (t, sizeof (scm_thread), "thread");
  t->root = scm_make_root (SCM_BOOL_F);
  scm_set_root (SCM_ROOT_STATE (t->root));
  start_stack (t->base);

  if (SCM_ROOTP (parent))
    {
      scm_root_state *thread_root = SCM_ROOT_STATE (t->root);
      scm_root_state *parent_root = SCM_ROOT_STATE (parent);

      thread_root->cur_inp = parent_root->cur_inp;
      thread_root->cur_outp = parent_root->cur_outp;
      thread_root->cur_errp = parent_root->cur_errp;
      thread_root->fluids = parent_root->fluids;
      scm_i_copy_fluids (thread_root);
    }
}

static void
on_thread_exit (void *v)
{
  scm_thread *t = (scm_thread *)v, **tp;

  pthread_mutex_lock (&thread_admin_mutex);
  t->exited = 1;
  for (tp = &all_threads; *tp; tp = &(*tp)->next_thread)
    if (*tp == t)
      {
	*tp = t->next_thread;
	break;
      }
  thread_count--;
  pthread_mutex_unlock (&thread_admin_mutex);
}

static void
scm_i_init_thread_for_guile (SCM_STACKITEM *base, SCM parent)
{
  scm_thread *t;

  pthread_mutex_lock (&scm_i_init_mutex);
  if (scm_initialized_p == 0)
    {
      /* First thread ever to enter Guile.  Run the full
	 initialization.
      */
      scm_i_init_guile (base);
    }
  else if ((t = SCM_CURRENT_THREAD) == NULL)
    {
      /* Guile is already initialized, but this thread enters it for
	 the first time.  Only initialize this thread.
      */
      guilify_self_1 (base);
      guilify_self_2 (parent);
    }
  else
    {
      /* This thread is already guilified, just resume it.
       */
      scm_i_enter_guile (t);
    }
  pthread_mutex_unlock (&scm_i_init_mutex);
}

extern void *__libc_stack_end;

static SCM_STACKITEM *
get_thread_stack_base ()
{
  pthread_attr_t attr;
  void *start, *end;
  size_t size;
  int res;

  /* XXX - pthread_getattr_np does not seem to work for the main
     thread, but we can use __libc_stack_end in that case.
  */

  pthread_getattr_np (pthread_self (), &attr);
  pthread_attr_getstack (&attr, &start, &size);
  end = (char *)start + size;

  if ((void *)&attr < start || (void *)&attr >= end)
    return __libc_stack_end;
  else
    {
#if SCM_STACK_GROWS_UP
      return start;
#else
      return end;
#endif
    }
}

void
scm_init_guile ()
{
  scm_i_init_thread_for_guile (get_thread_stack_base (), scm_i_root_root);
}

void
scm_enter_guile ()
{
  SCM_STACKITEM base_item;
  scm_i_init_thread_for_guile (&base_item, scm_i_root_root);
}

void
scm_leave_guile ()
{
  scm_i_leave_guile ();
}

void *
scm_with_guile (void *(*func)(void *), void *data)
{
  return scm_i_with_guile_and_parent (func, data, scm_i_root_root);
}

void *
scm_i_with_guile_and_parent (void *(*func)(void *), void *data,
			     SCM parent)
{
  void *res;
  SCM_STACKITEM base_item;
  scm_i_init_thread_for_guile (&base_item, parent);
  res = func (data);
  scm_i_leave_guile ();
  return res;
}

void *
scm_without_guile (void *(*func)(void *), void *data)
{
  void *res;
  scm_thread *t;
  t = scm_i_leave_guile ();
  res = func (data);
  scm_i_enter_guile (t);
  return res;
}

/*** Thread creation */

typedef struct {
  SCM parent;
  SCM thunk;
  SCM handler;
  SCM thread;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
} launch_data;

static void *
really_launch (void *d)
{
  launch_data *data = (launch_data *)d;
  SCM thunk = data->thunk, handler = data->handler;
  scm_thread *t;

  t = SCM_CURRENT_THREAD;

  pthread_mutex_lock (&data->mutex);
  data->thread = scm_current_thread ();
  pthread_cond_signal (&data->cond);
  pthread_mutex_unlock (&data->mutex);

  t->result = scm_catch (SCM_BOOL_T, thunk, handler);

  t->exited = 1;
  pthread_detach (t->pthread);

  return 0;
}

static void *
launch_thread (void *d)
{
  launch_data *data = (launch_data *)d;
  return scm_i_with_guile_and_parent (really_launch, d, data->parent);
}

SCM_DEFINE (scm_call_with_new_thread, "call-with-new-thread", 2, 0, 0,
	    (SCM thunk, SCM handler),
"Evaluate @code{(@var{thunk})} in a new thread, and new dynamic context, "
"returning a new thread object representing the thread. "
"If an error occurs during evaluation, call error-thunk, passing it an "
"error code describing the condition. "
"If this happens, the error-thunk is called outside the scope of the new "
"root -- it is called in the same dynamic context in which "
"with-new-thread was evaluated, but not in the callers thread. "
"All the evaluation rules for dynamic roots apply to threads.")
#define FUNC_NAME s_scm_call_with_new_thread
{
  launch_data data;
  pthread_t id;

  SCM_ASSERT (scm_is_true (scm_thunk_p (thunk)), thunk, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_true (scm_procedure_p (handler)), handler, SCM_ARG2,
	      FUNC_NAME);

  data.parent = scm_root->handle;
  data.thunk = thunk;
  data.handler = handler;
  data.thread = SCM_BOOL_F;
  pthread_mutex_init (&data.mutex, NULL);
  pthread_cond_init (&data.cond, NULL);

  pthread_mutex_lock (&data.mutex);
  if (pthread_create (&id, NULL, launch_thread, &data))
    {
      pthread_mutex_unlock (&data.mutex);
      SCM_SYSERROR;
    }
  pthread_cond_wait (&data.cond, &data.mutex);
  pthread_mutex_unlock (&data.mutex);
  
  return data.thread;
}
#undef FUNC_NAME

typedef struct {
  SCM parent;
  scm_t_catch_body body;
  void *body_data;
  scm_t_catch_handler handler;
  void *handler_data;
  SCM thread;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
} spawn_data;

static void *
really_spawn (void *d)
{
  spawn_data *data = (spawn_data *)d;
  scm_t_catch_body body = data->body;
  void *body_data = data->body_data;
  scm_t_catch_handler handler = data->handler;
  void *handler_data = data->handler_data;
  scm_thread *t = SCM_CURRENT_THREAD;

  pthread_mutex_lock (&data->mutex);
  data->thread = scm_current_thread ();
  pthread_cond_signal (&data->cond);
  pthread_mutex_unlock (&data->mutex);

  t->result = scm_internal_catch (SCM_BOOL_T,
				  body, body_data,
				  handler, handler_data);

  t->exited = 1;
  pthread_detach (t->pthread);

  return 0;
}

static void *
spawn_thread (void *d)
{
  spawn_data *data = (spawn_data *)d;
  return scm_i_with_guile_and_parent (really_spawn, d, data->parent);
}

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  spawn_data data;
  pthread_t id;

  data.parent = scm_root->handle;
  data.body = body;
  data.body_data = body_data;
  data.handler = handler;
  data.handler_data = handler_data;
  data.thread = SCM_BOOL_F;
  pthread_mutex_init (&data.mutex, NULL);
  pthread_cond_init (&data.cond, NULL);

  pthread_mutex_lock (&data.mutex);
  if (pthread_create (&id, NULL, spawn_thread, &data))
    {
      pthread_mutex_unlock (&data.mutex);
      scm_syserror (NULL);
    }
  pthread_cond_wait (&data.cond, &data.mutex);
  pthread_mutex_unlock (&data.mutex);
  
  return data.thread;
}

SCM_DEFINE (scm_yield, "yield", 0, 0, 0,
	    (),
"Move the calling thread to the end of the scheduling queue.")
#define FUNC_NAME s_scm_yield
{
  return scm_from_bool (sched_yield ());
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
  if (scm_is_eq (cur_thread, thread))
    SCM_MISC_ERROR ("can not join the current thread", SCM_EOL);

  t = SCM_THREAD_DATA (thread);
  if (!t->exited)
    {
      scm_thread *c;
      c = scm_i_leave_guile ();
      pthread_join (t->pthread, 0);
      scm_i_enter_guile (c);
    }
  res = t->result;
  t->result = SCM_BOOL_F;
  return res;
}
#undef FUNC_NAME

/*** Fat mutexes */

/* We implement our own mutex type since we want them to be 'fair', we
   want to do fancy things while waiting for them (like running
   asyncs) and we want to support waiting on many things at once.
   Also, we might add things that are nice for debugging.
*/

typedef struct {
  pthread_mutex_t lock;
  SCM owner;
  int level;      /* how much the owner owns us.  
		     < 0 for non-recursive mutexes */
  SCM waiting;    /* the threads waiting for this mutex. */
} fat_mutex;

static SCM
fat_mutex_mark (SCM mx)
{
  fat_mutex *m = SCM_MUTEX_DATA (mx);
  scm_gc_mark (m->owner);
  return m->waiting;
}

static size_t
fat_mutex_free (SCM mx)
{
  fat_mutex *m = SCM_MUTEX_DATA (mx);
  pthread_mutex_destroy (&m->lock);
  scm_gc_free (m, sizeof (fat_mutex), "mutex");
  return 0;
}

static int
fat_mutex_print (SCM mx, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  fat_mutex *m = SCM_MUTEX_DATA (mx);
  scm_puts ("#<mutex ", port);
  scm_uintprint ((scm_t_bits)m, 16, port);
  scm_puts (">", port);
  return 1;
}
 
static SCM
make_fat_mutex (int recursive)
{
  fat_mutex *m;
  SCM mx;

  m = scm_gc_malloc (sizeof (fat_mutex), "mutex");
  pthread_mutex_init (&m->lock, NULL);
  m->owner = SCM_BOOL_F;
  m->level = recursive? 0 : -1;
  m->waiting = SCM_EOL;
  SCM_NEWSMOB (mx, scm_tc16_mutex, (scm_t_bits) m);
  m->waiting = make_queue ();
  return mx;
}

SCM_DEFINE (scm_make_mutex, "make-mutex", 0, 0, 0,
	    (void),
	    "Create a new mutex. ")
#define FUNC_NAME s_scm_make_mutex
{
  return make_fat_mutex (0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_recursive_mutex, "make-recursive-mutex", 0, 0, 0,
	    (void),
	    "Create a new recursive mutex. ")
#define FUNC_NAME s_scm_make_recursive_mutex
{
  return make_fat_mutex (1);
}
#undef FUNC_NAME

static void
fat_mutex_lock (fat_mutex *m)
{
  pthread_mutex_lock (&m->lock);
  
  if (scm_is_false (m->owner))
    m->owner = cur_thread;
  else if (scm_is_eq (m->owner, cur_thread))
    {
      if (m->level >= 0)
	m->level++;
      else
	{
	  pthread_mutex_unlock (&m->lock);
	  scm_misc_error (NULL, "mutex already locked by current thread",
			  SCM_EOL);
	}
    }
  else
    {
      while (1)
	{
	  SCM c = enqueue (m->waiting, cur_thread);
	  int err;
	  /* Note: It's important that m->lock is never locked for
	     any longer amount of time since that could prevent GC */
	  pthread_mutex_unlock (&m->lock);
	  err = block ();
	  if (scm_is_eq (m->owner, cur_thread))
	    return;
	  pthread_mutex_lock (&m->lock);
	  remqueue (m->waiting, c);
	  pthread_mutex_unlock (&m->lock);
	  if (err)
	    {
	      errno = err;
	      scm_syserror (NULL);
	    }
	  SCM_ASYNC_TICK;
	  pthread_mutex_lock (&m->lock);
	}
    }
  pthread_mutex_unlock (&m->lock);
}

SCM_DEFINE (scm_lock_mutex, "lock-mutex", 1, 0, 0,
	    (SCM mx),
"Lock @var{mutex}. If the mutex is already locked, the calling thread "
"blocks until the mutex becomes available. The function returns when "
"the calling thread owns the lock on @var{mutex}.  Locking a mutex that "
"a thread already owns will succeed right away and will not block the "
"thread.  That is, Guile's mutexes are @emph{recursive}. ")
#define FUNC_NAME s_scm_lock_mutex
{
  SCM_VALIDATE_MUTEX (1, mx);
  
  fat_mutex_lock (SCM_MUTEX_DATA (mx));
  return SCM_BOOL_T;
}
#undef FUNC_NAME

static int
fat_mutex_trylock (fat_mutex *m)
{
  pthread_mutex_lock (&m->lock);
  if (scm_is_false (m->owner))
    m->owner = cur_thread;
  else if (scm_is_eq (m->owner, cur_thread))
    {
      if (m->level >= 0)
	m->level++;
      else
	{
	  pthread_mutex_unlock (&m->lock);
	  scm_misc_error (NULL, "mutex already locked by current thread",
			  SCM_EOL);
	}
    }
  else
    {
      pthread_mutex_unlock (&m->lock);
      return 0;
    }
  pthread_mutex_unlock (&m->lock);
  return 1;
}

SCM_DEFINE (scm_try_mutex, "try-mutex", 1, 0, 0,
	    (SCM mx),
"Try to lock @var{mutex}. If the mutex is already locked by someone "
"else, return @code{#f}.  Else lock the mutex and return @code{#t}. ")
#define FUNC_NAME s_scm_try_mutex
{
  SCM_VALIDATE_MUTEX (1, mx);
  
  return scm_from_bool (fat_mutex_trylock (SCM_MUTEX_DATA (mx)));
}
#undef FUNC_NAME

static void
fat_mutex_unlock (fat_mutex *m)
{
  pthread_mutex_lock (&m->lock);
  if (!scm_is_eq (m->owner, cur_thread))
    {
      const char *msg;
      if (scm_is_false (m->owner))
	msg = "mutex not locked";
      else
	msg = "mutex not locked by current thread";

      pthread_mutex_unlock (&m->lock);
      scm_misc_error (NULL, msg, SCM_EOL);
    }
  else if (m->level > 0)
    m->level--;
  else
    {
      SCM next = dequeue (m->waiting);
      if (scm_is_true (next))
	{
	  m->owner = next;
	  unblock (SCM_THREAD_DATA (next));
	}
      else
	m->owner = SCM_BOOL_F;
    }
  pthread_mutex_unlock (&m->lock);
}

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
  SCM_VALIDATE_MUTEX (1, mx);
  
  fat_mutex_unlock (SCM_MUTEX_DATA (mx));
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*** Fat condition variables */

/* Like mutexes, we implement our own condition variables using the
   primitives above.
*/

typedef struct {
  pthread_mutex_t lock;
  SCM waiting;               /* the threads waiting for this condition. */
} fat_cond;

static SCM
fat_cond_mark (SCM cv)
{
  fat_cond *c = SCM_CONDVAR_DATA (cv);
  return c->waiting;
}

static size_t
fat_cond_free (SCM mx)
{
  fat_cond *c = SCM_CONDVAR_DATA (mx);
  pthread_mutex_destroy (&c->lock);
  scm_gc_free (c, sizeof (fat_cond), "condition-variable");
  return 0;
}

static int
fat_cond_print (SCM cv, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  fat_cond *c = SCM_CONDVAR_DATA (cv);
  scm_puts ("#<condition-variable ", port);
  scm_uintprint ((scm_t_bits)c, 16, port);
  scm_puts (">", port);
  return 1;
}

SCM_DEFINE (scm_make_condition_variable, "make-condition-variable", 0, 0, 0,
	    (void),
	    "Make a new condition variable.")
#define FUNC_NAME s_scm_make_condition_variable
{
  fat_cond *c;
  SCM cv;

  c = scm_gc_malloc (sizeof (fat_cond), "condition variable");
  pthread_mutex_init (&c->lock, 0);
  c->waiting = SCM_EOL;
  SCM_NEWSMOB (cv, scm_tc16_condvar, (scm_t_bits) c);
  c->waiting = make_queue ();
  return cv;
}
#undef FUNC_NAME

static void
fat_cond_timedwait (fat_cond *c,
		    fat_mutex *m,
		    const scm_t_timespec *waittime)
{
  int err;
  pthread_mutex_lock (&c->lock);

  while (1)
    {
      enqueue (c->waiting, cur_thread);
      pthread_mutex_unlock (&c->lock);
      fat_mutex_unlock (m); /*fixme* - not thread safe */
      if (waittime == NULL)
	err = block ();
      else
	err = timed_block (waittime);
      fat_mutex_lock (m);
      if (err)
	{
	  errno = err;
	  scm_syserror (NULL);
	}
      /* XXX - check whether we have been signalled. */
      break;
    }
}

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
  scm_t_timespec waittime;

  SCM_VALIDATE_CONDVAR (1, cv);
  SCM_VALIDATE_MUTEX (2, mx);
  
  if (!SCM_UNBNDP (t))
    {
      if (scm_is_pair (t))
	{
	  waittime.tv_sec = scm_to_ulong (SCM_CAR (t));
	  waittime.tv_nsec = scm_to_ulong (SCM_CAR (t)) * 1000;
	}
      else
	{
	  waittime.tv_sec = scm_to_ulong (t);
	  waittime.tv_nsec = 0;
	}
    }

  fat_cond_timedwait (SCM_CONDVAR_DATA (cv),
		      SCM_MUTEX_DATA (mx),
		      SCM_UNBNDP (t) ? NULL : &waittime);
  return SCM_BOOL_T;
}
#undef FUNC_NAME

static int
fat_cond_signal (fat_cond *c)
{
  SCM th;
  pthread_mutex_lock (&c->lock);
  if (scm_is_true (th = dequeue (c->waiting)))
    unblock (SCM_THREAD_DATA (th));
  pthread_mutex_unlock (&c->lock);
  return 0;
}

SCM_DEFINE (scm_signal_condition_variable, "signal-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up one thread that is waiting for @var{cv}")
#define FUNC_NAME s_scm_signal_condition_variable
{
  SCM_VALIDATE_CONDVAR (1, cv);
  fat_cond_signal (SCM_CONDVAR_DATA (cv));
  return SCM_BOOL_T;
}
#undef FUNC_NAME

static int
fat_cond_broadcast (fat_cond *c)
{
  SCM th;
  pthread_mutex_lock (&c->lock);
  while (scm_is_true (th = dequeue (c->waiting)))
    unblock (SCM_THREAD_DATA (th));
  pthread_mutex_unlock (&c->lock);
  return 0;
}

SCM_DEFINE (scm_broadcast_condition_variable, "broadcast-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up all threads that are waiting for @var{cv}. ")
#define FUNC_NAME s_scm_broadcast_condition_variable
{
  SCM_VALIDATE_CONDVAR (1, cv);
  fat_cond_broadcast (SCM_CONDVAR_DATA (cv));
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
  scm_thread *t;
  for (t = all_threads; t; t = t->next_thread)
    {
      /* Check that thread has indeed been suspended.
       */
      assert (t->top);

      scm_gc_mark (t->handle);

#if SCM_STACK_GROWS_UP
      scm_mark_locations (t->base, t->top - t->base);
#else
      scm_mark_locations (t->top, t->base - t->top);
#endif
      scm_mark_locations ((SCM_STACKITEM *) t->regs,
			  ((size_t) sizeof(t->regs)
			   / sizeof (SCM_STACKITEM)));
    }

  SCM_MARK_BACKING_STORE ();
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
  res = select (nfds, readfds, writefds, exceptfds, timeout);
  eno = errno;
  scm_i_enter_guile (c);
  SCM_ASYNC_TICK;
  errno = eno;
  return res;
}

/* Convenience API */

int
scm_pthread_mutex_lock (pthread_mutex_t *mutex)
{
  scm_thread *t = scm_i_leave_guile ();
  int res = pthread_mutex_lock (mutex);
  scm_i_enter_guile (t);
  return res;
}

static void
unlock (void *data)
{
  pthread_mutex_unlock ((pthread_mutex_t *)data);
}

void
scm_frame_pthread_mutex_lock (pthread_mutex_t *mutex)
{
  scm_pthread_mutex_lock (mutex);
  scm_frame_unwind_handler (unlock, mutex, SCM_F_WIND_EXPLICITLY);
}

int
scm_pthread_cond_wait (pthread_cond_t *cond, pthread_mutex_t *mutex)
{
  scm_thread *t = scm_i_leave_guile ();
  int res = pthread_cond_wait (cond, mutex);
  scm_i_enter_guile (t);
  return res;
}

int
scm_pthread_cond_timedwait (pthread_cond_t *cond,
			    pthread_mutex_t *mutex,
			    const scm_t_timespec *wt)
{
  scm_thread *t = scm_i_leave_guile ();
  int res = pthread_cond_timedwait (cond, mutex, wt);
  scm_i_enter_guile (t);
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

static SCM
scm_c_make_list (size_t n, SCM fill)
{
  SCM res = SCM_EOL;
  while (n-- > 0)
    res = scm_cons (fill, res);
  return res;
}

SCM_DEFINE (scm_all_threads, "all-threads", 0, 0, 0,
	    (void),
	    "Return a list of all threads.")
#define FUNC_NAME s_scm_all_threads
{
  /* We can not allocate while holding the thread_admin_mutex because
     of the way GC is done.
  */
  int n = thread_count;
  scm_thread *t;
  SCM list = scm_c_make_list (n, SCM_UNSPECIFIED), *l;

  pthread_mutex_lock (&thread_admin_mutex);
  l = &list;
  for (t = all_threads; t && n > 0; t = t->next_thread)
    {
      SCM_SETCAR (*l, t->handle);
      l = SCM_CDRLOC (*l);
      n--;
    }
  *l = SCM_EOL;
  pthread_mutex_unlock (&thread_admin_mutex);
  return list;
}
#undef FUNC_NAME

scm_root_state *
scm_i_thread_root (SCM thread)
{
  return SCM_ROOT_STATE ((SCM_CURRENT_THREAD)->root);
}

SCM_DEFINE (scm_thread_exited_p, "thread-exited?", 1, 0, 0,
	    (SCM thread),
	    "Return @code{#t} iff @var{thread} has exited.\n")
#define FUNC_NAME s_scm_thread_exited_p
{
  return scm_from_bool (scm_c_thread_exited_p (thread));
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

static pthread_cond_t wake_up_cond;
int scm_i_thread_go_to_sleep;
static int threads_initialized_p = 0;

void
scm_i_thread_put_to_sleep ()
{
  if (threads_initialized_p)
    {
      scm_thread *t;

      /* We leave Guile completely before locking the
	 thread_admin_mutex.  This ensures that other threads can put
	 us to sleep while we block on that mutex.
      */
      scm_i_leave_guile ();
      pthread_mutex_lock (&thread_admin_mutex);
      /* Signal all threads to go to sleep */
      scm_i_thread_go_to_sleep = 1;
      for (t = all_threads; t; t = t->next_thread)
	pthread_mutex_lock (&t->heap_mutex);
      scm_i_thread_go_to_sleep = 0;
    }
}

void
scm_i_thread_invalidate_freelists ()
{
  /* thread_admin_mutex is already locked. */

  scm_thread *t;
  for (t = all_threads; t; t = t->next_thread)
    if (t != SCM_CURRENT_THREAD)
      t->clear_freelists_p = 1;
}

void
scm_i_thread_wake_up ()
{
  if (threads_initialized_p)
    {
      scm_thread *t;
      pthread_cond_broadcast (&wake_up_cond);
      for (t = all_threads; t; t = t->next_thread)
	pthread_mutex_unlock (&t->heap_mutex);
      pthread_mutex_unlock (&thread_admin_mutex);
      scm_i_enter_guile (SCM_CURRENT_THREAD);
    }
}

void
scm_i_thread_sleep_for_gc ()
{
  scm_thread *t;
  t = suspend ();
  pthread_cond_wait (&wake_up_cond, &t->heap_mutex);
  resume (t);
}

pthread_mutex_t scm_i_critical_section_mutex = PTHREAD_MUTEX_INITIALIZER;

/*** Initialization */

pthread_key_t scm_i_freelist, scm_i_freelist2;
pthread_mutex_t scm_i_misc_mutex;

void
scm_threads_prehistory (SCM_STACKITEM *base)
{
  pthread_mutex_init (&thread_admin_mutex, NULL);
  pthread_mutex_init (&scm_i_misc_mutex, NULL);
  pthread_cond_init (&wake_up_cond, NULL);
  pthread_mutex_init (&scm_i_critical_section_mutex, NULL);
  pthread_key_create (&scm_i_thread_key, on_thread_exit);
  pthread_key_create (&scm_i_root_key, NULL);
  pthread_key_create (&scm_i_freelist, NULL);
  pthread_key_create (&scm_i_freelist2, NULL);
  
  guilify_self_1 (base);
}

scm_t_bits scm_tc16_thread;
scm_t_bits scm_tc16_mutex;
scm_t_bits scm_tc16_condvar;

void
scm_init_threads ()
{
  scm_tc16_thread = scm_make_smob_type ("thread", sizeof (scm_thread));
  scm_set_smob_mark (scm_tc16_thread, thread_mark);
  scm_set_smob_print (scm_tc16_thread, thread_print);
  scm_set_smob_free (scm_tc16_thread, thread_free);

  scm_tc16_mutex = scm_make_smob_type ("mutex", sizeof (fat_mutex));
  scm_set_smob_mark (scm_tc16_mutex, fat_mutex_mark);
  scm_set_smob_print (scm_tc16_mutex, fat_mutex_print);
  scm_set_smob_free (scm_tc16_mutex, fat_mutex_free);

  scm_tc16_condvar = scm_make_smob_type ("condition-variable",
					 sizeof (fat_cond));
  scm_set_smob_mark (scm_tc16_condvar, fat_cond_mark);
  scm_set_smob_print (scm_tc16_condvar, fat_cond_print);
  scm_set_smob_free (scm_tc16_condvar, fat_cond_free);

  scm_i_root_root = SCM_BOOL_F;
  guilify_self_2 (SCM_BOOL_F);
  threads_initialized_p = 1;
}

void
scm_init_threads_root_root ()
{
  scm_root_state *rr;

  scm_i_root_root = scm_permanent_object (scm_make_root (SCM_BOOL_F));
  rr = SCM_ROOT_STATE (scm_i_root_root);
  rr->cur_inp = scm_cur_inp;
  rr->cur_outp = scm_cur_outp;
  rr->cur_errp = scm_cur_errp;
  rr->fluids = scm_root->fluids;
  scm_i_copy_fluids (rr);
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

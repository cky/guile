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




#include <unistd.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>

#include "libguile/validate.h"
#include "libguile/coop-pthreads.h"
#include "libguile/root.h"
#include "libguile/eval.h"
#include "libguile/async.h"
#include "libguile/ports.h"

#undef DEBUG

/*** Queues */

static SCM
make_queue ()
{
  return scm_cons (SCM_EOL, SCM_EOL);
}

static void
enqueue (SCM q, SCM t)
{
  SCM c = scm_cons (t, SCM_EOL);
  if (SCM_NULLP (SCM_CAR (q)))
    SCM_SETCAR (q, c);
  else
    SCM_SETCDR (SCM_CDR (q), c);
  SCM_SETCDR (q, c);
}

static SCM
dequeue (SCM q)
{
  SCM c = SCM_CAR (q);
  if (SCM_NULLP (c))
    return SCM_BOOL_F;
  else
    {
      SCM_SETCAR (q, SCM_CDR (c));
      if (SCM_NULLP (SCM_CAR (q)))
	SCM_SETCDR (q, SCM_EOL);
      return SCM_CAR (c);
    }
}


/*** Threads */

typedef struct scm_copt_thread {
  
  /* A condition variable for sleeping on.
   */
  pthread_cond_t sleep_cond;

  /* A link for waiting queues.
   */
  struct scm_copt_thread *next_waiting;

  scm_root_state *root;
  SCM handle;
  pthread_t pthread;
  SCM result;

  SCM joining_threads;

  /* For keeping track of the stack and registers. */
  SCM_STACKITEM *base;
  SCM_STACKITEM *top;
  jmp_buf regs;

} scm_copt_thread;

static SCM
make_thread (SCM creation_protects)
{
  SCM z;
  scm_copt_thread *t = scm_gc_malloc (sizeof(*t), "thread");
  z = scm_cell (scm_tc16_thread, (scm_t_bits)t);
  t->handle = z;
  t->result = creation_protects;
  t->base = NULL;
  t->joining_threads = make_queue ();
  pthread_cond_init (&t->sleep_cond, NULL);
  return z;
}

static void
init_thread_creator (SCM thread, pthread_t th, scm_root_state *r)
{
  scm_copt_thread *t = SCM_THREAD_DATA(thread);
  t->root = r;
  t->pthread = th;
#ifdef DEBUG
  // fprintf (stderr, "%ld created %ld\n", pthread_self (), th);
#endif
}

static void
init_thread_creatant (SCM thread, SCM_STACKITEM *base)
{
  scm_copt_thread *t = SCM_THREAD_DATA(thread);
  t->base = base;
  t->top = NULL;
}

static SCM
thread_mark (SCM obj)
{
  scm_copt_thread *t = SCM_THREAD_DATA (obj);
  scm_gc_mark (t->result);
  scm_gc_mark (t->joining_threads);
  return t->root->handle;
}

static int
thread_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_copt_thread *t = SCM_THREAD_DATA (exp);
  scm_puts ("#<thread ", port);
  scm_intprint ((unsigned long)t, 16, port);
  if (t->pthread != -1)
    {
      scm_putc (' ', port);
      scm_intprint (t->pthread, 10, port);
    }
  else
    scm_puts (" (exited)", port);
  scm_putc ('>', port);
  return 1;
}

static size_t
thread_free (SCM obj)
{
  scm_copt_thread *t = SCM_THREAD_DATA (obj);
  if (t->pthread != -1)
    abort ();
  scm_gc_free (t, sizeof (*t), "thread");
  return 0;
}

/*** Fair mutexes */

/* POSIX mutexes are not necessarily fair but since we'd like to use a
   mutex for scheduling, we build a fair one on top of POSIX.
*/

typedef struct fair_mutex {
  pthread_mutex_t lock;
  scm_copt_thread *owner;
  scm_copt_thread *next_waiting, *last_waiting;
} fair_mutex;

static void
fair_mutex_init (fair_mutex *m)
{
  pthread_mutex_init (&m->lock, NULL);
  m->owner = NULL;
  m->next_waiting = NULL;
  m->last_waiting = NULL;
}

static void
fair_mutex_lock_1 (fair_mutex *m, scm_copt_thread *t)
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
	  pthread_cond_wait (&t->sleep_cond, &m->lock);
	}
      while (m->owner != t);
      assert (m->next_waiting == t);
      m->next_waiting = t->next_waiting;
      if (m->next_waiting == NULL)
	m->last_waiting = NULL;
    }
  pthread_mutex_unlock (&m->lock);
}

static void
fair_mutex_lock (fair_mutex *m, scm_copt_thread *t)
{
  pthread_mutex_lock (&m->lock);
  fair_mutex_lock_1 (m, t);
}

static void
fair_mutex_unlock_1 (fair_mutex *m)
{
  scm_copt_thread *t;
  pthread_mutex_lock (&m->lock);
  // fprintf (stderr, "%ld unlocking\n", m->owner->pthread);
  if ((t = m->next_waiting) != NULL)
    {
      m->owner = t;
      pthread_cond_signal (&t->sleep_cond);
    }
  else
    m->owner = NULL;
  // fprintf (stderr, "%ld unlocked\n", pthread_self ());
}

static void
fair_mutex_unlock (fair_mutex *m)
{
  fair_mutex_unlock_1 (m);
  pthread_mutex_unlock (&m->lock);
}

/*  Temporarily give up the mutex.  This function makes sure that we
    are on the wait queue before starting the next thread.  Otherwise
    the next thread might preempt us and we will have a hard time
    getting on the wait queue.
*/
#if 0
static void
fair_mutex_yield (fair_mutex *m)
{
  scm_copt_thread *self, *next;

  pthread_mutex_lock (&m->lock);

  /* get next thread
   */
  if ((next = m->next_waiting) == NULL)
    {
      /* No use giving it up. */
      pthread_mutex_unlock (&m->lock);
      return;
    }

  /* put us on queue
   */
  self = m->owner;
  self->next_waiting = NULL;
  if (m->last_waiting)
    m->last_waiting->next_waiting = self;
  else
    m->next_waiting = self;
  m->last_waiting = self;

  /* wake up next thread
   */

  m->owner = next;
  pthread_cond_signal (&next->sleep_cond);

  /* wait for mutex
   */
  do
    {
      pthread_cond_wait (&self->sleep_cond, &m->lock);
    }
  while (m->owner != self);
  assert (m->next_waiting == self);
  m->next_waiting = self->next_waiting;
  if (m->next_waiting == NULL)
    m->last_waiting = NULL;

  pthread_mutex_unlock (&m->lock);
}
#else
static void
fair_mutex_yield (fair_mutex *m)
{
  scm_copt_thread *self = m->owner;
  fair_mutex_unlock_1 (m);
  fair_mutex_lock_1 (m, self);
}
#endif

static void
fair_cond_wait (pthread_cond_t *c, fair_mutex *m)
{
  scm_copt_thread *t = m->owner;
  fair_mutex_unlock_1 (m);
  pthread_cond_wait (c, &m->lock);
  fair_mutex_lock_1 (m, t);
}

/* Return 1 when the mutex was signalled and 0 when not. */
static int
fair_cond_timedwait (pthread_cond_t *c, fair_mutex *m, struct timespec *at)
{
  int res;
  scm_copt_thread *t = m->owner;
  fair_mutex_unlock_1 (m);
  res = pthread_cond_timedwait (c, &m->lock, at);  /* XXX - signals? */
  fair_mutex_lock_1 (m, t);
  return res == 0;
}

/*** Scheduling */

/* When a thread wants to execute Guile functions, it locks the
   guile_mutex.
*/

static fair_mutex guile_mutex;

static SCM cur_thread;
void *scm_i_copt_thread_data;

void
scm_i_copt_set_thread_data (void *data)
{
  scm_copt_thread *t = SCM_THREAD_DATA (cur_thread);
  scm_i_copt_thread_data = data;
  t->root = (scm_root_state *)data;
}
  
static void
resume (scm_copt_thread *t)
{
  cur_thread = t->handle;
  scm_i_copt_thread_data = t->root;
  t->top = NULL;
}

static void
enter_guile (scm_copt_thread *t)
{
  fair_mutex_lock (&guile_mutex, t);
  resume (t);
}

static scm_copt_thread *
suspend ()
{
  SCM cur = cur_thread;
  scm_copt_thread *c = SCM_THREAD_DATA (cur);

  /* record top of stack for the GC */
  c->top = (SCM_STACKITEM *)&c;
  /* save registers. */
  SCM_FLUSH_REGISTER_WINDOWS;
  setjmp (c->regs);

  return c;
}

static scm_copt_thread *
leave_guile ()
{
  scm_copt_thread *c = suspend ();
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
      scm_copt_thread *t = suspend ();
      fair_mutex_yield (&guile_mutex);
      resume (t);
    }
  return SCM_BOOL_T;
}

/* Put the current thread to sleep until it is explicitely unblocked.
 */
static void
block ()
{
  scm_copt_thread *t = suspend ();
  fair_cond_wait (&t->sleep_cond, &guile_mutex);
  resume (t);
}

/* Put the current thread to sleep until it is explicitely unblocked
   or until a signal arrives or until time AT (absolute time) is
   reached.  Return 1 when it has been unblocked; 0 otherwise.
 */
static int
timed_block (struct timespec *at)
{
  int res;
  scm_copt_thread *t = suspend ();
  res = fair_cond_timedwait (&t->sleep_cond, &guile_mutex, at);
  resume (t);
  return res;
}

/* Unblock a sleeping thread.
 */
static void
unblock (scm_copt_thread *t)
{
  pthread_cond_signal (&t->sleep_cond);
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
  // return scm_call_0 (data->body);
}

static SCM
handler_bootstrip (launch_data* data, SCM tag, SCM throw_args)
{
  scm_root->rootcont = data->rootcont;
  return (data->handler) (data->handler_data, tag, throw_args);
  // return scm_apply_1 (data->handler, tag, throw_args);
}

static void
really_launch (SCM_STACKITEM *base, launch_data *data)
{
  SCM thread = data->thread;
  scm_copt_thread *t = SCM_THREAD_DATA (thread);
  init_thread_creatant (thread, base);
  enter_guile (t);

  data->rootcont = SCM_BOOL_F;
  t->result =
    scm_internal_cwdr ((scm_t_catch_body) body_bootstrip,
		       data,
		       (scm_t_catch_handler) handler_bootstrip,
		       data, base);
  free (data);

  pthread_detach (t->pthread);
  all_threads = scm_delq (thread, all_threads);
  t->pthread = -1;
  thread_count--;
  leave_guile ();
}

static void *
launch_thread (void *p)
{
  really_launch ((SCM_STACKITEM *)&p, (launch_data *)p);
  return NULL;
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
    pthread_t th;
    SCM root, old_winds;
    launch_data *data;

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
    pthread_create (&th, NULL, launch_thread, (void *) data);
    init_thread_creator (thread, th, SCM_ROOT_STATE (root));
    all_threads = scm_cons (thread, all_threads);
    thread_count++;

    /* Return to old dynamic context. */
    scm_dowinds (old_winds, - scm_ilength (old_winds));
  }

  return thread;
}

SCM
scm_call_with_new_thread (SCM argl)
#define FUNC_NAME s_call_with_new_thread
{
  SCM thunk, handler;

  /* Check arguments. */
  {
    register SCM args = argl;
    if (!SCM_CONSP (args))
      SCM_WRONG_NUM_ARGS ();
    thunk = SCM_CAR (args);
    SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (thunk)),
		thunk,
		SCM_ARG1,
		s_call_with_new_thread);
    args = SCM_CDR (args);
    if (!SCM_CONSP (args))
      SCM_WRONG_NUM_ARGS ();
    handler = SCM_CAR (args);
    SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (handler)),
		handler,
		SCM_ARG2,
		s_call_with_new_thread);
    if (!SCM_NULLP (SCM_CDR (args)))
      SCM_WRONG_NUM_ARGS ();
  }

  return create_thread ((scm_t_catch_body) scm_call_0, thunk,
			(scm_t_catch_handler) scm_apply_1, handler,
			argl);
}
#undef FUNC_NAME

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  return create_thread (body, body_data, handler, handler_data, SCM_BOOL_F);
}

/*** Mutexes */

/* We implement our own mutex type since we want them to be 'fair', we
   want to do fancy things while waiting for them (like running
   asyncs) and we want to support waiting on many things at once.
   Also, we might add things that are nice for debugging.
*/

typedef struct scm_copt_mutex {
  /* the thread currently owning the mutex, or SCM_BOOL_F. */
  SCM owner;
  /* how much the owner owns us. */
  int level;
  /* the threads waiting for this mutex. */
  SCM waiting;
} scm_copt_mutex;

static SCM
mutex_mark (SCM mx)
{
  scm_copt_mutex *m = SCM_MUTEX_DATA (mx);
  scm_gc_mark (m->owner);
  return m->waiting;
}

SCM
scm_make_mutex ()
{
  SCM mx = scm_make_smob (scm_tc16_mutex);
  scm_copt_mutex *m = SCM_MUTEX_DATA (mx);
  m->owner = SCM_BOOL_F;
  m->level = 0;
  m->waiting = make_queue ();
  return mx;
}

SCM
scm_lock_mutex (SCM mx)
#define FUNC_NAME s_lock_mutex
{
  scm_copt_mutex *m;
  SCM_ASSERT (SCM_MUTEXP (mx), mx, SCM_ARG1, FUNC_NAME);
  m = SCM_MUTEX_DATA (mx);

  if (m->owner == SCM_BOOL_F)
    m->owner = cur_thread;
  else if (m->owner == cur_thread)
    m->level++;
  else
    {
      while (m->owner != cur_thread)
	{
	  enqueue (m->waiting, cur_thread);
	  block ();
	  SCM_ASYNC_TICK;
	}
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM
scm_try_mutex (SCM mx)
#define FUNC_NAME s_try_mutex
{
  scm_copt_mutex *m;
  SCM_ASSERT (SCM_MUTEXP (mx), mx, SCM_ARG1, FUNC_NAME);
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

SCM
scm_unlock_mutex (SCM mx)
#define FUNC_NAME s_unlock_mutex
{
  scm_copt_mutex *m;
  SCM_ASSERT (SCM_MUTEXP (mx), mx, SCM_ARG1, FUNC_NAME);
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

typedef struct scm_copt_cond {
  /* the threads waiting for this condition. */
  SCM waiting;
} scm_copt_cond;

static SCM
cond_mark (SCM cv)
{
  scm_copt_cond *c = SCM_CONDVAR_DATA (cv);
  return c->waiting;
}

SCM
scm_make_condition_variable (void)
{
  SCM cv = scm_make_smob (scm_tc16_condvar);
  scm_copt_cond *c = SCM_CONDVAR_DATA (cv);
  c->waiting = make_queue ();
  return cv;
}

SCM
scm_timed_wait_condition_variable (SCM cv, SCM mx, SCM t)
#define FUNC_NAME s_wait_condition_variable
{
  scm_copt_cond *c;
  struct timespec waittime;
  int res;

  SCM_ASSERT (SCM_CONDVARP (cv),
	      cv,
	      SCM_ARG1,
	      s_wait_condition_variable);
  SCM_ASSERT (SCM_MUTEXP (mx),
	      mx,
	      SCM_ARG2,
	      s_wait_condition_variable);
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

  enqueue (c->waiting, cur_thread);
  scm_unlock_mutex (mx);
  if (SCM_UNBNDP (t))
    {
      block ();
      res = 1;
    }
  else
    res = timed_block (&waittime);
  scm_lock_mutex (mx);
  return SCM_BOOL (res);
}
#undef FUNC_NAME

SCM
scm_signal_condition_variable (SCM cv)
#define FUNC_NAME s_signal_condition_variable
{
  SCM th;
  scm_copt_cond *c;
  SCM_ASSERT (SCM_CONDVARP (cv),
	      cv,
	      SCM_ARG1,
	      s_signal_condition_variable);
  c = SCM_CONDVAR_DATA (cv);
  if (!SCM_FALSEP (th = dequeue (c->waiting)))
    unblock (SCM_THREAD_DATA (th));
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM
scm_broadcast_condition_variable (SCM cv)
#define FUNC_NAME s_broadcast_condition_variable
{
  SCM th;
  scm_copt_cond *c;
  SCM_ASSERT (SCM_CONDVARP (cv),
	      cv,
	      SCM_ARG1,
	      s_signal_condition_variable);
  c = SCM_CONDVAR_DATA (cv);
  while (!SCM_FALSEP (th = dequeue (c->waiting)))
    unblock (SCM_THREAD_DATA (th));
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*** Initialization */

void
scm_threads_init (SCM_STACKITEM *base)
{
  scm_tc16_thread = scm_make_smob_type ("thread", 0);
  scm_tc16_mutex = scm_make_smob_type ("mutex", sizeof (scm_copt_mutex));
  scm_tc16_condvar = scm_make_smob_type ("condition-variable",
					 sizeof (scm_copt_cond));

  scm_i_switch_counter = SCM_I_THREAD_SWITCH_COUNT;

  fair_mutex_init (&guile_mutex);

  cur_thread = make_thread (SCM_BOOL_F);
  enter_guile (SCM_THREAD_DATA (cur_thread));
  /* root is set later from init.c */
  init_thread_creator (cur_thread, pthread_self(), NULL);
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
      scm_copt_thread *t = SCM_THREAD_DATA (SCM_CAR (c));
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
  scm_copt_thread *c = leave_guile ();
  res = select (nfds, readfds, writefds, exceptfds, timeout);
  eno = errno;
  enter_guile (c);
  SCM_ASYNC_TICK;
  errno = eno;
  return res;
}

void
scm_init_iselect ()
{
}

unsigned long
scm_thread_usleep (unsigned long usec)
{
  scm_copt_thread *c = leave_guile ();
  usleep (usec);
  enter_guile (c);
  return 0;
}

unsigned long
scm_thread_sleep (unsigned long sec)
{
  unsigned long res;
  scm_copt_thread *c = leave_guile ();
  res = sleep (sec);
  enter_guile (c);
  return res;
}

/*** Misc */

SCM
scm_current_thread (void)
{
  return cur_thread;
}

SCM
scm_all_threads (void)
{
  return all_threads;
}

scm_root_state *
scm_i_thread_root (SCM thread)
{
  if (thread == cur_thread)
    return scm_i_copt_thread_data;
  else
    return ((scm_copt_thread *)SCM_THREAD_DATA (thread))->root;
}

SCM
scm_join_thread (SCM thread)
#define FUNC_NAME s_join_thread
{
  scm_copt_thread *t;
  SCM res;

  SCM_VALIDATE_THREAD (1, thread);

  t = SCM_THREAD_DATA (thread);
  if (t->pthread != -1)
    {
      scm_copt_thread *c = leave_guile ();
      pthread_join (t->pthread, NULL);
      enter_guile (c);
    }
  res = t->result;
  t->result = SCM_BOOL_F;
  return res;
}
#undef FUNC_NAME

int
scm_c_thread_exited_p (SCM thread)
#define FUNC_NAME s_scm_thread_exited_p
{
  scm_copt_thread *t;
  SCM_VALIDATE_THREAD (1, thread);
  t = SCM_THREAD_DATA (thread);
  return t->pthread == -1;
}
#undef FUNC_NAME

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/


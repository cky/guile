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

#include "libguile/validate.h"
#include "libguile/coop-pthreads.h"
#include "libguile/root.h"
#include "libguile/eval.h"
#include "libguile/async.h"
#include "libguile/ports.h"

#undef DEBUG

/* This thread implementation uses POSIX threads but allows only
   thread to really execute at any one time.

   XXX - more overview here.
*/

/* All data is protected by a single mutex: guile_mutex. */

static pthread_mutex_t guile_mutex = PTHREAD_MUTEX_INITIALIZER;

/*** Threads */

typedef struct scm_copt_thread {
  
  /* A condition variable for sleeping on.
   */
  pthread_cond_t sleep_cond;

  /* A link for the ready queue.
   */
  struct scm_copt_thread *next_ready;

  scm_root_state *root;
  SCM handle;
  pthread_t pthread;
  SCM result;

  /* For keeping track of the stack and registers. */
  SCM_STACKITEM *base;
  SCM_STACKITEM *top;
  jmp_buf regs;

} scm_copt_thread;

static SCM
make_thread (SCM args)
{
  SCM z;
  scm_copt_thread *t = scm_gc_malloc (sizeof(*t), "thread");
  z = scm_cell (scm_tc16_thread, (scm_t_bits)t);
  t->handle = z;
  t->result = args;
  return z;
}

static void
init_thread_creator (SCM thread, pthread_t th, scm_root_state *r)
{
  scm_copt_thread *t = SCM_THREAD_DATA(thread);
  t->root = r;
  t->pthread = th;
  pthread_cond_init (&t->sleep_cond, NULL);
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

/*** Ready queue */

/* Normally, queues are implemented with the procedures above, but the
   ready queue is special.  We need to put threads on it from the
   'outside', i.e., when we don't hold the guile_mutex.  That's why
   the ready queue has its own mutex and isn't implemented with SCM
   objects.
*/

static pthread_mutex_t ready_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
static scm_copt_thread *next_ready = NULL;
static scm_copt_thread *last_ready = NULL;

static void
get_ready (scm_copt_thread *t)
{
  pthread_mutex_lock (&ready_queue_mutex);
  t->next_ready = NULL;
  if (last_ready)
    last_ready->next_ready = t;
  else
    next_ready = t;
  last_ready = t;
  pthread_mutex_unlock (&ready_queue_mutex);
}

static scm_copt_thread *
get_next_ready ()
{
  scm_copt_thread *t;
  pthread_mutex_lock (&ready_queue_mutex);
  t = next_ready;
  if (t)
    {
      next_ready = t->next_ready;
      if (next_ready == NULL)
	last_ready = NULL;
    }
  return t;
  pthread_mutex_unlock (&ready_queue_mutex);
}

/*** Running and sleeping */

static SCM cur_thread;

/* Kick the next runnable thread if there is one.
 */
static void
kick_next ()
{
  scm_copt_thread *next = get_next_ready (ready_queue);
  if (next)
    pthread_cond_signal (&next->sleep_cond);
}
  
static SCM
suspend ()
{
  SCM cur = cur_thread;
  scm_copt_thread *c = SCM_THREAD_DATA (cur);

  /* record top of stack for the GC */
  c->top = (SCM_STACKITEM *)&c;
  /* save registers. */
  SCM_FLUSH_REGISTER_WINDOWS;
  setjmp (c->regs);

  return cur;
}

static void
release ()
{
  pthread_mutex_unlock (&guile_mutex);
}

static void
acquire ()
{
  please = 1;
  pthread_mutex_lock (&guile_mutex);
}

static void
resume (SCM cur)
{
  scm_copt_thread *c = SCM_THREAD_DATA (cur);
  cur_thread = cur;
  c->top = NULL;
}

static void
block ()
{
  SCM cur = suspend ();
  scm_copt_thread *c = SCM_THREAD_DATA (cur);
  pthread_cond_wait (&c->sleep_cond, &guile_mutex);
  resume (cur);
}

/* Yielding consists of getting the next thread from the ready_queue
   and if there is one, putting ourselves on the ready queue and
   block. 
*/
SCM
scm_yield ()
{
  scm_copt_thread *next = get_next_ready ();
  if (next)
    {
      pthread_cond_signal (&next->sleep_cond);
      get_ready (SCM_THREAD_DATA (cur_thread));
      block ();
    }
  return SCM_BOOL_T;
}

int scm_switch_counter;

/*** Thread creation */

static SCM all_threads;
static int thread_count;

typedef struct scheme_launch_data {
  SCM rootcont;
  SCM body;
  SCM handler;
} scheme_launch_data;

static SCM
scheme_body_bootstrip (scheme_launch_data* data)
{
  /* First save the new root continuation */
  data->rootcont = scm_root->rootcont;
  return scm_call_0 (data->body);
}

static SCM
scheme_handler_bootstrip (scheme_launch_data* data, SCM tag, SCM throw_args)
{
  scm_root->rootcont = data->rootcont;
  return scm_apply_1 (data->handler, tag, throw_args);
}

static void
really_launch (SCM_STACKITEM *base, SCM thread)
{
  scm_copt_thread *t = SCM_THREAD_DATA (thread);
  scheme_launch_data data;
  init_thread_creatant (thread, base);
  resume (thread);

  /* Ok, we bullied our way in, now be nice and stand in queue. 
   */
  scm_yield ();

  data.rootcont = SCM_BOOL_F;
  data.body = SCM_CAR (t->result);
  data.handler = SCM_CADR (t->result);
  t->result =
    scm_internal_cwdr ((scm_t_catch_body) scheme_body_bootstrip,
		       &data,
		       (scm_t_catch_handler) scheme_handler_bootstrip,
		       &data, base);
  pthread_detach (t->pthread);

  {
    SCM next = dequeue (ready_queue);
    if (!SCM_FALSEP (next))
      {
	scm_copt_thread *n = SCM_THREAD_DATA (next);
	pthread_cond_signal (&n->sleep_cond);
      }
  }

  all_threads = scm_delq (thread, all_threads);
  t->pthread = -1;
  thread_count--;
  suspend ();
}

static void *
scheme_launch_thread (void *p)
{
  acquire ();
  really_launch ((SCM_STACKITEM *)&p, (SCM)p);
  release ();
  return NULL;
}


SCM
scm_call_with_new_thread (SCM argl)
#define FUNC_NAME s_call_with_new_thread
{
  SCM thread;

  /* Check arguments. */
  {
    register SCM args = argl;
    SCM thunk, handler;
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

  /* Make new thread.  The first thing the new thread will do is to
     lock guile_mutex.  Thus, we can safely complete its
     initialization after creating it.  While the new thread starts,
     all its data is protected via all_threads.
   */

  {
    pthread_t th;
    SCM root, old_winds;
    
    /* Unwind wind chain. */
    old_winds = scm_dynwinds;
    scm_dowinds (SCM_EOL, scm_ilength (scm_root->dynwinds));

    /* Allocate thread locals. */
    root = scm_make_root (scm_root->handle);
    /* Make thread. */
    thread = make_thread (argl);
    SCM_DEFER_INTS;
    pthread_create (&th, NULL, scheme_launch_thread, (void *) thread);
    init_thread_creator (thread, th, SCM_ROOT_STATE (root));
    all_threads = scm_cons (thread, all_threads);
    thread_count++;
#ifdef DEBUG
    fprintf (stderr, "thread %ld created\n", th);
#endif
    SCM_ALLOW_INTS;

    /* Return to old dynamic context. */
    scm_dowinds (old_winds, - scm_ilength (old_winds));
  }
  
  return thread;
}
#undef FUNC_NAME

/*** Mutexes */

/* We implement our own mutex type since we want them to be 'fair', 
   we want to do fancy things while waiting for them (like running
   asyncs) and we want to support waiting on many things at once.
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
	  kick_next ();
	  block ();
	  SCM_ASYNC_TICK;
	}
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM
scm_unlock_mutex (SCM mx)
#define FUNC_NAME s_lock_mutex
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
	  enqueue (ready_queue, next);
	  scm_yield ();
	}
      else
	m->owner = SCM_BOOL_F;
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*** Initialization */

void
scm_threads_init (SCM_STACKITEM *base)
{
  scm_tc16_thread = scm_make_smob_type ("thread", 0);
  scm_tc16_mutex = scm_make_smob_type ("mutex", sizeof (scm_copt_mutex));
  scm_tc16_condvar = scm_make_smob_type ("condition-variable", 0);

  scm_switch_counter = SCM_THREAD_SWITCH_COUNT;

  acquire ();
  cur_thread = make_thread (SCM_BOOL_F);
  /* root is set later from init.c */
  init_thread_creator (cur_thread, pthread_self(), NULL);
  init_thread_creatant (cur_thread, base);
  resume (cur_thread);
  thread_count = 1;
  scm_gc_register_root (&all_threads);
  all_threads = scm_cons (cur_thread, SCM_EOL);

  scm_set_smob_mark (scm_tc16_thread, thread_mark);
  scm_set_smob_print (scm_tc16_thread, thread_print);
  scm_set_smob_free (scm_tc16_thread, thread_free);

  scm_set_smob_mark (scm_tc16_mutex, mutex_mark);

  ready_queue = scm_permanent_object (make_queue ());
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

#include "libguile/iselect.h"

int
scm_internal_select (int nfds,
		     SELECT_TYPE *readfds,
		     SELECT_TYPE *writefds,
		     SELECT_TYPE *exceptfds,
		     struct timeval *timeout)
{
  int res;
  SCM cur = suspend ();
  release ();
  res = select (nfds, readfds, writefds, exceptfds, timeout);
  acquire ();
  resume (cur);
  SCM_ASYNC_TICK;
  return res;
}

void
scm_init_iselect ()
{
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
  return ((scm_copt_thread *)SCM_THREAD_DATA (thread))->root;
}

void *
scm_copt_thread_data (void)
{
  scm_copt_thread *t = SCM_THREAD_DATA (cur_thread);
  return t->root;
}

void
scm_copt_set_thread_data (void *d)
{
  scm_copt_thread *t = SCM_THREAD_DATA (cur_thread);
  t->root = d;
}

/* XXX from here to end */

#if 0
/* NOTE: There are TWO mechanisms for starting a thread: The first one
   is used when spawning a thread from Scheme, while the second one is
   used from C.

   It might be argued that the first should be implemented in terms of
   the second.  The reason it isn't is that that would require an
   extra unnecessary malloc (the thread_args structure).  By providing
   one pair of extra functions (c_launch_thread, scm_spawn_thread) the
   Scheme threads are started more efficiently.  */

/* This is the first thread spawning mechanism: threads from Scheme */



/* This is the second thread spawning mechanism: threads from C */

typedef struct c_launch_data {
  union {
    SCM thread;
    SCM rootcont;
  } u;
  scm_t_catch_body body;
  void *body_data;
  scm_t_catch_handler handler;
  void *handler_data;
} c_launch_data;

static SCM
c_body_bootstrip (c_launch_data* data)
{
  /* First save the new root continuation */
  data->u.rootcont = scm_root->rootcont;
  return (data->body) (data->body_data);
}

static SCM
c_handler_bootstrip (c_launch_data* data, SCM tag, SCM throw_args)
{
  scm_root->rootcont = data->u.rootcont;
  return (data->handler) (data->handler_data, tag, throw_args);
}

static void
c_really_launch (void *p)
{
  SCM result;
  c_launch_data *data = (c_launch_data *) p;
  /* The thread object will be GC protected by being on this stack */
  SCM thread = data->u.thread;
  scm_copt_thread *t = SCM_THREAD_DATA (thread);
  /* We must use the address of `thread', otherwise the compiler will
     optimize it away.  This is OK since the longest SCM_STACKITEM
     also is a long.  */
  result = scm_internal_cwdr ((scm_t_catch_body) c_body_bootstrip,
			      data,
			      (scm_t_catch_handler) c_handler_bootstrip,
			      data,
			      (SCM_STACKITEM *) &thread);
  all_threads = scm_delq (thread, all_threads);
  t->result = result;
  pthread_detach (t->pthread);
  t->pthread = -1;
  thread_count--;
  free ((char *) data);
}

static void *
c_launch_thread (void *p)
{
  ticket t;
  enter_guile ((SCM_STACKITEM *)&t, &t);
  c_really_launch (p);
  leave_guile (&t);
  return NULL;
}

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  SCM thread;
  pthread_t th;
  SCM root, old_winds;
  c_launch_data *data = (c_launch_data *) scm_malloc (sizeof (*data));
  
  /* Unwind wind chain. */
  old_winds = scm_dynwinds;
  scm_dowinds (SCM_EOL, scm_ilength (scm_root->dynwinds));

  /* Allocate thread locals. */
  root = scm_make_root (scm_root->handle);
  /* Make thread. */
  thread = make_thread ();
  SCM_DEFER_INTS;

  data->u.thread = thread;
  data->body = body;
  data->body_data = body_data;
  data->handler = handler;
  data->handler_data = handler_data;
  
  pthread_create (&th, NULL, c_launch_thread, (void *) data);
  init_thread_creator (thread, th, SCM_ROOT_STATE (root));
  all_threads = scm_cons (thread, all_threads);
  thread_count++;
  SCM_ALLOW_INTS;

  /* Return to old dynamic context. */
  scm_dowinds (old_winds, - scm_ilength (old_winds));
  
  return thread;
}
#endif

SCM
scm_join_thread (SCM thread)
#define FUNC_NAME s_join_thread
{
#if 0
  scm_copt_thread *t;
  SCM res;

  SCM_VALIDATE_THREAD (1, thread);

  t = SCM_THREAD_DATA (thread);
  if (t->pthread != -1)
    {
      ticket *tt = suspend_guile ();
      pthread_join (t->pthread, NULL);
      resume_guile (tt);
    }
  res = t->result;
  t->result = SCM_BOOL_F;
  return res;
#endif
  return SCM_BOOL_F;
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

SCM
scm_make_condition_variable (void)
{
  abort ();
}

SCM
scm_timed_wait_condition_variable (SCM c, SCM m, SCM t)
{
  abort ();
}

SCM
scm_signal_condition_variable (SCM c)
{
  abort ();
}

unsigned long
scm_thread_usleep (unsigned long usec)
{
  return usleep (usec);
}

unsigned long
scm_thread_sleep (unsigned long sec)
{
  return sleep (sec);
}


SCM
scm_try_mutex (SCM mx)
{
  abort ();
}

SCM
scm_broadcast_condition_variable (SCM cv)
{
  abort ();
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/


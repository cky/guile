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

static pthread_mutex_t guile_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t guile_thread;

static SCM all_threads;
static int thread_count;

static pthread_key_t handle_key;

/* The following functions are for managing a threads visit in Guile
   land.  A thread can be in one of three states: outside, active, or
   suspended.  "Outside" means that the thread can not call any Guile
   related functions and can not store SCM values in local variables.
   "Active" or "suspended" means that a thread can use Guile and can
   store SCM values in its stack.  An "active" thread is currently
   executing while a "suspended" one is waiting to become active
   again.  There can only be one "active" thread at any one time.
*/

typedef struct ticket {
  struct ticket *next;
  struct ticket **prevp;
  SCM_STACKITEM *base;
  SCM_STACKITEM *top;
  jmp_buf regs;
} ticket;

static ticket *tickets = NULL;
static pthread_key_t ticket_key;

/* Enter Guile land.  While in Guile land, the stack between BASE and
   the current stack pointer will be scanned for SCM references.  Only
   one thread can be in Guile land at any one time.  When you try to
   enter it while another one is already there, you will be put to
   sleep until it leaves or suspends.
 */
static void
enter_guile (SCM_STACKITEM *base, ticket *t)
{
  pthread_mutex_lock (&guile_mutex);
  guile_thread = pthread_self ();

#ifdef DEBUG
  fprintf (stderr, "thread %ld entered\n", pthread_self ());
#endif

  /* Ok, we are in.  Stamp our ticket. */
  t->next = tickets;
  if (tickets)
    tickets->prevp = &t->next;
  tickets = t;
  t->prevp = &tickets;

  t->base = base;
  t->top = NULL;

  pthread_setspecific (ticket_key, (void *)t);
}

/* Leave Guile land so that the next thread can enter.  This function
   must be called from the same stack frame as the corresponding
   enter_guile.  The stack of this thread will no longer be scanned.
*/
static void
leave_guile (ticket *t)
{
  /* Remove ticket... */
  *t->prevp = t->next;
  if (t->next)
    t->next->prevp = t->prevp;

#ifdef DEBUG
  fprintf (stderr, "thread %ld left\n", pthread_self ());
#endif

  /* ...and leave. */
  guile_thread = 0;
  pthread_mutex_unlock (&guile_mutex);
}

/* Suspend the visit in Guile land so that other threads can resume
   their visit or enter.  While a thread is suspended, its stack is
   scanned between BASE (as given to enter_guile) and TOP (as given
   here).
*/
static ticket *
suspend_guile ()
{
  ticket *t = pthread_getspecific (ticket_key);

  if (t == NULL)
    abort ();

  /* Record top of stack...*/
  t->top = (SCM_STACKITEM *)&t;
  /* ...save registers for the GC...*/
  SCM_FLUSH_REGISTER_WINDOWS;
  setjmp (t->regs);

#ifdef DEBUG
  fprintf (stderr, "thread %ld suspended\n", pthread_self ());
#endif

  /* ... and leave temporarily. */
  guile_thread = 0;
  pthread_mutex_unlock (&guile_mutex);
  return t;
}

/* Resume the visit.  This must be called from the same frame as the
   corresponding suspend_guile.
*/
static void
resume_guile (ticket *t)
{
  pthread_mutex_lock (&guile_mutex);
  guile_thread = pthread_self ();
#ifdef DEBUG
  fprintf (stderr, "thread %ld resumed\n", pthread_self ());
#endif
  t->top = NULL;
}

static ticket main_ticket;

int scm_switch_counter;

static void
init_queue (scm_copt_queue *q)
{
  q->first = NULL;
  q->lastp = &q->first;
}

static void
enqueue (scm_copt_queue *q, scm_copt_thread *t)
{
  t->prevp = q->lastp;
  t->next = NULL;
  *t->prevp = t;
  q->lastp = &t->next;
}

static scm_copt_thread *
dequeue (scm_copt_queue *q)
{
  scm_copt_thread *t = q->first;
  if (t)
    {
      *t->prevp = t->next;
      if (t->next)
	t->next->prevp = t->prevp;
      else
	q->lastp = t->prevp;
    }
  return t;
}

static SCM
make_thread ()
{
  SCM z;
  scm_copt_thread *t = scm_gc_malloc (sizeof(*t), "thread");
  z = scm_cell (scm_tc16_thread, (scm_t_bits)t);
  t->handle = z;
  return z;
}

static void
init_thread_creator (SCM thread, pthread_t th, scm_root_state *r)
{
  scm_copt_thread *t = SCM_THREAD_DATA(thread);
  t->root = r;
  t->pthread = th;
  t->result = SCM_BOOL_F;
  pthread_cond_init (&t->block, NULL);
}

static void
init_thread_creatant (SCM thread)
{
  scm_copt_thread *t = SCM_THREAD_DATA(thread);
  pthread_setspecific (handle_key, t);
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

static scm_copt_queue yield_queue;

void
scm_threads_init (SCM_STACKITEM *base)
{
  SCM main_thread;
  pthread_key_create (&handle_key, NULL);
  pthread_key_create (&ticket_key, NULL);
  scm_switch_counter = SCM_THREAD_SWITCH_COUNT;
  enter_guile (base, &main_ticket);
  main_thread = make_thread ();
  /* root is set later from init.c */
  init_thread_creator (main_thread, pthread_self(), NULL);
  init_thread_creatant (main_thread);
  scm_gc_register_root (&all_threads);
  all_threads = scm_cons (main_thread, SCM_EOL);
  thread_count = 1;
  scm_set_smob_mark (scm_tc16_thread, thread_mark);
  scm_set_smob_print (scm_tc16_thread, thread_print);
  scm_set_smob_free (scm_tc16_thread, thread_free);
  init_queue (&yield_queue);
}

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
  ticket *t;

  for (t = tickets; t; t = t->next)
    {
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

/* NOTE: There are TWO mechanisms for starting a thread: The first one
   is used when spawning a thread from Scheme, while the second one is
   used from C.

   It might be argued that the first should be implemented in terms of
   the second.  The reason it isn't is that that would require an
   extra unnecessary malloc (the thread_args structure).  By providing
   one pair of extra functions (c_launch_thread, scm_spawn_thread) the
   Scheme threads are started more efficiently.  */

/* This is the first thread spawning mechanism: threads from Scheme */

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
really_launch (void *p)
{
  SCM argl = (SCM) p;
  SCM thread = SCM_CAR (argl);
  scm_copt_thread *t = SCM_THREAD_DATA (thread);
  SCM result;
  scheme_launch_data data;
  init_thread_creatant (thread);
  data.rootcont = SCM_BOOL_F;
  data.body = SCM_CADR (argl);
  data.handler = SCM_CADDR (argl);
  result = scm_internal_cwdr ((scm_t_catch_body) scheme_body_bootstrip,
			      &data,
			      (scm_t_catch_handler) scheme_handler_bootstrip,
			      &data,
			      (SCM_STACKITEM *) &thread);
  all_threads = scm_delq (thread, all_threads);
  t->result = result;
  pthread_detach (t->pthread);
  t->pthread = -1;
  thread_count--;
}

static void *
scheme_launch_thread (void *p)
{
  ticket t;
  enter_guile ((SCM_STACKITEM *)&t, &t);
  really_launch (p);
  leave_guile (&t);
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
     call enter_guile.  Thus, we can safely complete its
     initialization after creating it. */
  {
    pthread_t th;
    SCM root, old_winds;
    
    /* Unwind wind chain. */
    old_winds = scm_dynwinds;
    scm_dowinds (SCM_EOL, scm_ilength (scm_root->dynwinds));

    /* Allocate thread locals. */
    root = scm_make_root (scm_root->handle);
    /* Make thread. */
    thread = make_thread ();
    SCM_DEFER_INTS;
    argl = scm_cons (thread, argl);
    /* Note that we couldn't pass a pointer to argl as data since the
       argl variable may not exist in memory when the thread starts.  */
    pthread_create (&th, NULL, scheme_launch_thread, (void *) argl);
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

SCM
scm_current_thread (void)
{
  scm_copt_thread *t = pthread_getspecific (handle_key);
  if (t == NULL)
    abort ();   /* XXX - should ceate a new handle. */
  else
    return t->handle;
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
      ticket *tt = suspend_guile ();
      pthread_join (t->pthread, NULL);
      resume_guile (tt);
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

SCM
scm_yield (void)
{
  fprintf (stderr, "yield\n");
  if (yield_queue.first)
    {
      ticket *t;
      scm_copt_thread *me = pthread_getspecific (handle_key);
      scm_copt_thread *next = dequeue (&yield_queue);
      enqueue (&yield_queue, me);
      pthread_cond_signal (&next->block);
      t = suspend_guile_2 ();
      pthread_cond_wait (&me->block, &guile_mutex);
      resume_guile_2 (t);
    }
  return SCM_BOOL_T;
}

void
scm_copt_mutex_init (scm_copt_mutex *m)
{
  pthread_mutex_init (&m->mutex, NULL);
  m->owner = NULL;
  m->level = 0;
  init_queue (&m->waiting);
}

void
scm_copt_mutex_destroy (scm_copt_mutex *m)
{
  pthread_mutex_destroy (&m->mutex);
}

void
scm_copt_mutex_lock (scm_copt_mutex *m)
{
  scm_copt_thread *t = pthread_getspecific (handle_key);
  pthread_mutex_lock (&m->mutex);
  if (m->owner == t)
    m->level++;
  else if (m->owner == NULL)
    {
      m->owner = t;
    }
  else
    {
      enqueue (&m->waiting, t);
      do
	{
	  ticket *tt = suspend_guile ();
	  pthread_cond_wait (&t->block, &m->mutex);
	  resume_guile (tt);
	  SCM_ASYNC_TICK;
	}
      while (m->owner != t);
    }
  pthread_mutex_unlock (&m->mutex);
}

void
scm_copt_mutex_unlock (scm_copt_mutex *m)
{
  pthread_mutex_lock (&m->mutex);
  if (m->level == 0)
    {
      scm_copt_thread *t = dequeue (&m->waiting);
      m->owner = t;
      if (t)
	pthread_cond_signal (&t->block);
    }
  else
    m->level--;
  pthread_mutex_unlock (&m->mutex);
}

SCM
scm_make_mutex (void)
{
  SCM m = scm_make_smob (scm_tc16_mutex);
  scm_copt_mutex_init (SCM_MUTEX_DATA (m));
  return m;
}

SCM
scm_lock_mutex (SCM m)
{
  ticket *t;
  SCM_ASSERT (SCM_MUTEXP (m), m, SCM_ARG1, s_lock_mutex);
  t = suspend_guile ();
  scm_copt_mutex_lock (SCM_MUTEX_DATA (m));
  resume_guile (t);
  return SCM_BOOL_T;
}

SCM
scm_unlock_mutex (SCM m)
{
  SCM_ASSERT (SCM_MUTEXP (m), m, SCM_ARG1, s_unlock_mutex);
  scm_copt_mutex_unlock(SCM_MUTEX_DATA (m));
  return SCM_BOOL_T;
}

SCM
scm_make_condition_variable (void)
{
  SCM c = scm_make_smob (scm_tc16_condvar);
  pthread_cond_init (SCM_CONDVAR_DATA (c), NULL);
  return c;
}

SCM
scm_wait_condition_variable (SCM c, SCM m)
{
  ticket *t;
  SCM_ASSERT (SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_wait_condition_variable);
  SCM_ASSERT (SCM_MUTEXP (m),
	      m,
	      SCM_ARG2,
	      s_wait_condition_variable);
  t = suspend_guile ();
  pthread_cond_wait (SCM_CONDVAR_DATA (c), SCM_MUTEX_DATA (m));
  resume_guile (t);
  return SCM_BOOL_T;
}

SCM
scm_signal_condition_variable (SCM c)
{
  SCM_ASSERT (SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_signal_condition_variable);
  pthread_cond_signal (SCM_CONDVAR_DATA (c));
  return SCM_BOOL_T;
}

void *
scm_copt_thread_data (void)
{
  scm_copt_thread *t = pthread_getspecific (handle_key);
  if (t == NULL)
    abort ();
  else
    return t->root;
}

void
scm_copt_set_thread_data (void *d)
{
  scm_copt_thread *t = pthread_getspecific (handle_key);
  if (t == NULL)
    abort ();
  else
    t->root = d;
}

unsigned long
scm_thread_usleep (unsigned long usec)
{
  ticket *t;
  unsigned long ret;
  t = suspend_guile ();
  ret = usleep (usec);
  resume_guile (t);
  return ret;
}

unsigned long
scm_thread_sleep (unsigned long sec)
{
  ticket *t;
  unsigned long ret;
  t = suspend_guile ();
  ret = sleep (sec);
  resume_guile (t);
  return ret;
}

#include "libguile/iselect.h"

int
scm_internal_select (int nfds,
		     SELECT_TYPE *readfds,
		     SELECT_TYPE *writefds,
		     SELECT_TYPE *exceptfds,
		     struct timeval *timeout)
{
  ticket *t;
  int res;
  t = suspend_guile ();
  res = select (nfds, readfds, writefds, exceptfds, timeout);
  resume_guile (t);
  SCM_ASYNC_TICK;
  return res;
}

void
scm_init_iselect ()
{
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/


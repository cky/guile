/* Copyright (C) 1995,1996,1997,1998,2000,2001 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include "libguile/validate.h"
#include "libguile/coop-threads.h"
#include "libguile/root.h"

/* A counter of the current number of threads */
size_t scm_thread_count = 0;

/* This is included rather than compiled separately in order
   to simplify the configuration mechanism. */
#include "libguile/coop.c"

/* A count-down counter used to determine when to switch
   contexts */
size_t scm_switch_counter = SCM_THREAD_SWITCH_COUNT;

coop_m scm_critical_section_mutex;

void
scm_threads_init (SCM_STACKITEM *i)
{
  coop_init();

  scm_thread_count = 1;

#ifndef GUILE_PTHREAD_COMPAT
  coop_global_main.sto = i;
#endif
  coop_global_main.base = i;
  coop_global_curr = &coop_global_main;
  coop_all_qput (&coop_global_allq, coop_global_curr);

  coop_mutex_init (&scm_critical_section_mutex);

  coop_global_main.data = 0; /* Initialized in init.c */
}

void
scm_threads_mark_stacks (void)
{
  coop_t *thread;
  
  for (thread = coop_global_allq.t.all_next; 
       thread != NULL; thread = thread->all_next)
    {
      if (thread == coop_global_curr)
	{
	  /* Active thread */
	  /* stack_len is long rather than sizet in order to guarantee
	     that &stack_len is long aligned */
#ifdef STACK_GROWS_UP
	  long stack_len = ((SCM_STACKITEM *) (&thread) -
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
	  
	  scm_mark_locations (((size_t) thread->base,
			       (sizet) stack_len));
#else
	  long stack_len = ((SCM_STACKITEM *) thread->base -
			    (SCM_STACKITEM *) (&thread));
	  
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
	  
	  scm_mark_locations ((SCM_STACKITEM *) &thread,
			      stack_len);
#endif
	}
      else
	{
	  /* Suspended thread */
#ifdef STACK_GROWS_UP
	  long stack_len = ((SCM_STACKITEM *) (thread->sp) -
			    (SCM_STACKITEM *) thread->base);

	  scm_mark_locations ((size_t)thread->base,
			      (sizet) stack_len);
#else
	  long stack_len = ((SCM_STACKITEM *) thread->base -
			    (SCM_STACKITEM *) (thread->sp));
	  
	  /* Registers are already on the stack. No need to mark. */
	  
	  scm_mark_locations ((SCM_STACKITEM *) (size_t)thread->sp,
			      stack_len);
#endif
	}

      /* Mark this thread's root */
      scm_gc_mark (((scm_root_state *) thread->data) -> handle);
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

extern SCM scm_apply (SCM, SCM, SCM);

static SCM
scheme_body_bootstrip (scheme_launch_data* data)
{
  /* First save the new root continuation */
  data->rootcont = scm_root->rootcont;
  return scm_apply (data->body, SCM_EOL, SCM_EOL);
}

static SCM
scheme_handler_bootstrip (scheme_launch_data* data, SCM tag, SCM throw_args)
{
  scm_root->rootcont = data->rootcont;
  return scm_apply (data->handler, scm_cons (tag, throw_args), SCM_EOL);
}

static void
scheme_launch_thread (void *p)
{
  /* The thread object will be GC protected by being a member of the
     list given as argument to launch_thread.  It will be marked
     during the conservative sweep of the stack. */
  register SCM argl = (SCM) p;
  SCM thread = SCM_CAR (argl);
  scheme_launch_data data;
  data.rootcont = SCM_BOOL_F;
  data.body = SCM_CADR (argl);
  data.handler = SCM_CADDR (argl);
  scm_internal_cwdr ((scm_t_catch_body) scheme_body_bootstrip,
		     &data,
		     (scm_t_catch_handler) scheme_handler_bootstrip,
		     &data,
		     (SCM_STACKITEM *) &thread);
  SCM_SET_CELL_WORD_1 (thread, 0);
  scm_thread_count--;
  SCM_DEFER_INTS;
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

  /* Make new thread. */
  {
    coop_t *t;
    SCM root, old_winds;
    
    /* Unwind wind chain. */
    old_winds = scm_dynwinds;
    scm_dowinds (SCM_EOL, scm_ilength (scm_root->dynwinds));

    /* Allocate thread locals. */
    root = scm_make_root (scm_root->handle);
    /* Make thread. */
    SCM_NEWCELL (thread);
    SCM_DEFER_INTS;
    SCM_SETCAR (thread, scm_tc16_thread);
    argl = scm_cons (thread, argl);
    /* Note that we couldn't pass a pointer to argl as data since the
       argl variable may not exist in memory when the thread starts.  */
    t = coop_create (scheme_launch_thread, (void *) argl);
    t->data = SCM_ROOT_STATE (root);
    SCM_SET_CELL_WORD_1 (thread, (scm_t_bits) t);
    scm_thread_count++;
    /* Note that the following statement also could cause coop_yield.*/
    SCM_ALLOW_INTS;

    /* We're now ready for the thread to begin. */
    coop_yield();

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
c_launch_thread (void *p)
{
  register c_launch_data *data = (c_launch_data *) p;
  /* The thread object will be GC protected by being on this stack */
  SCM thread = data->u.thread;
  /* We must use the address of `thread', otherwise the compiler will
     optimize it away.  This is OK since the longest SCM_STACKITEM
     also is a long.  */
  scm_internal_cwdr ((scm_t_catch_body) c_body_bootstrip,
		     data,
		     (scm_t_catch_handler) c_handler_bootstrip,
		     data,
		     (SCM_STACKITEM *) &thread);
  scm_thread_count--;
  scm_must_free ((char *) data);
}

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  SCM thread;
  coop_t *t;
  SCM root, old_winds;
  c_launch_data *data = (c_launch_data *) scm_must_malloc (sizeof (*data),
							   "scm_spawn_thread");
  
  /* Unwind wind chain. */
  old_winds = scm_dynwinds;
  scm_dowinds (SCM_EOL, scm_ilength (scm_root->dynwinds));

  /* Allocate thread locals. */
  root = scm_make_root (scm_root->handle);
  /* Make thread. */
  SCM_NEWCELL (thread);
  SCM_DEFER_INTS;
  SCM_SETCAR (thread, scm_tc16_thread);

  data->u.thread = thread;
  data->body = body;
  data->body_data = body_data;
  data->handler = handler;
  data->handler_data = handler_data;
  
  t = coop_create (c_launch_thread, (void *) data);
  
  t->data = SCM_ROOT_STATE (root);
  SCM_SET_CELL_WORD_1 (thread, (scm_t_bits) t);
  scm_thread_count++;
  /* Note that the following statement also could cause coop_yield.*/
  SCM_ALLOW_INTS;

  /* We're now ready for the thread to begin. */
  coop_yield();

  /* Return to old dynamic context. */
  scm_dowinds (old_winds, - scm_ilength (old_winds));
  
  return thread;
}

SCM
scm_join_thread (SCM thread)
#define FUNC_NAME s_join_thread
{
  coop_t *thread_data;
  SCM_VALIDATE_THREAD (1, thread);
  /* Dirk:FIXME:: SCM_THREAD_DATA is a handle for a thread.  It may be that a
   * certain thread implementation uses a value of 0 as a valid thread handle.
   * With the following code, this thread would always be considered finished.
   */
  /* Dirk:FIXME:: With preemptive threading, a thread may finish immediately
   * after SCM_THREAD_DATA is read.  Thus, it must be guaranteed that the
   * handle remains valid until the thread-object is garbage collected, or
   * a mutex has to be used for reading and modifying SCM_THREAD_DATA.
   */
  thread_data = SCM_THREAD_DATA (thread);
  if (thread_data)
    /* The thread is still alive */
    coop_join (thread_data);
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM
scm_yield (void)
{
  /* Yield early */
  scm_switch_counter = SCM_THREAD_SWITCH_COUNT;
  coop_yield();

  return SCM_BOOL_T;
}

SCM
scm_single_thread_p (void)
{
  return (coop_global_runq.tail == &coop_global_runq.t
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM
scm_make_mutex (void)
{
  SCM m;
  coop_m *data = (coop_m *) scm_must_malloc (sizeof (coop_m), "mutex");

  SCM_NEWSMOB (m, scm_tc16_mutex, (scm_t_bits) data);
  coop_mutex_init (data);
  return m;
}

SCM
scm_lock_mutex (SCM m)
{
  SCM_ASSERT (SCM_MUTEXP (m), m, SCM_ARG1, s_lock_mutex);
  coop_mutex_lock (SCM_MUTEX_DATA (m));
  return SCM_BOOL_T;
}

SCM
scm_unlock_mutex (SCM m)
{
  SCM_ASSERT (SCM_MUTEXP (m), m, SCM_ARG1, s_unlock_mutex);
  coop_mutex_unlock(SCM_MUTEX_DATA (m));

  /* Yield early */
  scm_switch_counter = SCM_THREAD_SWITCH_COUNT;
  coop_yield();

  return SCM_BOOL_T;
}

SCM
scm_make_condition_variable (void)
{
  SCM c;
  coop_c *data = (coop_c *) scm_must_malloc (sizeof (coop_c), "condvar");
  SCM_NEWSMOB (c, scm_tc16_condvar, (scm_t_bits) data);
  coop_condition_variable_init (SCM_CONDVAR_DATA (c));
  return c;
}

SCM
scm_wait_condition_variable (SCM c, SCM m)
{
  SCM_ASSERT (SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_wait_condition_variable);
  SCM_ASSERT (SCM_MUTEXP (m),
	      m,
	      SCM_ARG2,
	      s_wait_condition_variable);
  coop_condition_variable_wait_mutex (SCM_CONDVAR_DATA (c),
				      SCM_MUTEX_DATA (m));
  return SCM_BOOL_T;
}

SCM
scm_signal_condition_variable (SCM c)
{
  SCM_ASSERT (SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_signal_condition_variable);
  coop_condition_variable_signal (SCM_CONDVAR_DATA (c));
  return SCM_BOOL_T;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

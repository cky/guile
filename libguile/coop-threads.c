/*	Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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


#include "coop-threads.h"

/* A counter of the current number of threads */
size_t scm_thread_count = 0;

/* This is included rather than compiled separately in order
   to simplify the configuration mechanism. */
#include "coop.c"

/* A count-down counter used to determine when to switch
   contexts */
size_t scm_switch_counter = SCM_THREAD_SWITCH_COUNT;

coop_m scm_critical_section_mutex;

#ifdef __STDC__
size_t
scm_threads_free_thread (SCM t)
#else
size_t
scm_threads_free_thread (t)
     SCM t;
#endif
{
  scm_must_free (SCM_THREAD_DATA (t));
  return sizeof (coop_t);
}

#ifdef __STDC__
size_t
scm_threads_free_mutex (SCM m)
#else
size_t
scm_threads_free_mutex (m)
     SCM m;
#endif
{
  scm_must_free (SCM_MUTEX_DATA (m));
  return sizeof (coop_m);
}

#ifdef __STDC__
size_t
scm_threads_free_condvar (SCM c)
#else
size_t
scm_threads_free_condvar (c)
     SCM c;
#endif
{
  scm_must_free (SCM_CONDVAR_DATA (c));
  return sizeof (coop_c);
}

#ifdef __STDC__
void
scm_threads_init (SCM_STACKITEM *i)
#else
void
scm_threads_init (i)
     SCM_STACKITEM *i;
#endif
{
  coop_init();

  scm_thread_count = 1;

  coop_global_main.sto = i;
  coop_global_main.base = i;
  coop_global_curr = &coop_global_main;
  coop_all_qput (&coop_global_allq, coop_global_curr);

  coop_mutex_init (&scm_critical_section_mutex);

  coop_global_main.data = 0; /* Initialized in init.c */
}

#ifdef __STDC__
void
scm_threads_mark_stacks ()
#else
void
scm_threads_mark_stacks ()
#endif
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
	   * for which the values from SCM_LENGTH and SCM_CHARS must remain
	   * usable.   This requirement is stricter than a liveness
	   * requirement -- in particular, it constrains the implementation
	   * of scm_resizuve.
	   */
	  SCM_FLUSH_REGISTER_WINDOWS;
	  /* This assumes that all registers are saved into the jmp_buf */
	  setjmp (scm_save_regs_gc_mark);
	  scm_mark_locations ((SCM_STACKITEM *) scm_save_regs_gc_mark,
			      ((scm_sizet) sizeof scm_save_regs_gc_mark
			       / sizeof (SCM_STACKITEM)));
	  
	  scm_mark_locations (((size_t) thread->base,
			       (sizet) stack_len));
#else
	  long stack_len = ((SCM_STACKITEM *) thread->base -
			    (SCM_STACKITEM *) (&thread));
	  
	  /* Protect from the C stack.  This must be the first marking
	   * done because it provides information about what objects
	   * are "in-use" by the C code.   "in-use" objects are  those
	   * for which the values from SCM_LENGTH and SCM_CHARS must remain
	   * usable.   This requirement is stricter than a liveness
	   * requirement -- in particular, it constrains the implementation
	   * of scm_resizuve.
	   */
	  SCM_FLUSH_REGISTER_WINDOWS;
	  /* This assumes that all registers are saved into the jmp_buf */
	  setjmp (scm_save_regs_gc_mark);
	  scm_mark_locations ((SCM_STACKITEM *) scm_save_regs_gc_mark,
			      ((scm_sizet) sizeof scm_save_regs_gc_mark
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

static void
scheme_launch_thread (void *p)
{
  /* The thread object will be GC protected by being a member of the
     list given as argument to launch_thread.  It will be marked
     during the conservative sweep of the stack. */
  SCM args = (SCM) p;
  scm_call_with_dynamic_root (SCM_CADR (args), SCM_CADDR (args));
  scm_thread_count--;
}

#ifdef __STDC__
SCM
scm_call_with_new_thread (SCM argl)
#else
SCM
scm_call_with_new_thread (argl)
     SCM argl;
#endif
{
  SCM thread;

  /* Check arguments. */
  {
    register SCM args = argl;
    SCM thunk, handler;
    SCM_ASSERT (SCM_NIMP (args),
		scm_makfrom0str (s_call_with_new_thread),
		SCM_WNA, NULL);
    thunk = SCM_CAR (args);
    SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (thunk)),
		thunk,
		SCM_ARG1,
		s_call_with_new_thread);
    args = SCM_CDR (args);
    SCM_ASSERT (SCM_NIMP (args),
		scm_makfrom0str (s_call_with_new_thread),
		SCM_WNA, NULL);
    handler = SCM_CAR (args);
    SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (handler)),
		handler,
		SCM_ARG2,
		s_call_with_new_thread);
    SCM_ASSERT (SCM_NULLP (SCM_CDR (args)),
		scm_makfrom0str (s_call_with_new_thread),
		SCM_WNA, NULL);
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
    t = coop_create (scheme_launch_thread, (void *) argl);
    t->data = SCM_ROOT_STATE (root);
    SCM_SETCDR (thread, t);
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

/* This is the second thread spawning mechanism: threads from C */

struct thread_args {
  SCM thread;
  scm_catch_body_t body;
  void *body_data;
  scm_catch_handler_t handler;
  void *handler_data;
};

static void
c_launch_thread (void *p)
{
  struct thread_args *args = (struct thread_args *) p;
  /* The thread object will be GC protected by being on this stack */
  SCM thread = args->thread;
  /* We must use the address of `thread', otherwise the compiler will
     optimize it away.  This is OK since the longest SCM_STACKITEM
     also is a long.  */
  scm_internal_cwdr (args->body,
		     args->body_data,
		     args->handler,
		     args->handler_data,
		     &thread);
  scm_thread_count--;
  scm_must_free ((char *) args);
}

SCM
scm_spawn_thread (scm_catch_body_t body, void *body_data,
		  scm_catch_handler_t handler, void *handler_data)
{
  SCM thread;
  coop_t *t;
  SCM root, old_winds;
  struct thread_args *args =
    (struct thread_args *) scm_must_malloc (sizeof (*args),
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

  args->thread = thread;
  args->body = body;
  args->body_data = body_data;
  args->handler = handler;
  args->handler_data = handler_data;
  
  t = coop_create (c_launch_thread, (void *) args);
  
  t->data = SCM_ROOT_STATE (root);
  SCM_SETCDR (thread, t);
  scm_thread_count++;
  /* Note that the following statement also could cause coop_yield.*/
  SCM_ALLOW_INTS;

  /* We're now ready for the thread to begin. */
  coop_yield();

  /* Return to old dynamic context. */
  scm_dowinds (old_winds, - scm_ilength (old_winds));
  
  return thread;
}

#ifdef __STDC__
SCM
scm_join_thread (SCM t)
#else
SCM
scm_join_thread (t)
     SCM t;
#endif
{
  SCM_ASSERT (SCM_NIMP (t) && SCM_THREADP (t), t, SCM_ARG1, s_join_thread);
  coop_join (SCM_THREAD_DATA (t));
  return SCM_BOOL_T;
}

#ifdef __STDC__
SCM
scm_yield ()
#else
SCM
scm_yield ()
#endif
{
  /* Yield early */
  scm_switch_counter = SCM_THREAD_SWITCH_COUNT;
  coop_yield();

  return SCM_BOOL_T;
}

#ifdef __STDC__
SCM
scm_single_thread_p ()
#else
SCM
scm_single_thread_p ()
#endif
{
  return (coop_global_runq.tail == &coop_global_runq.t
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

#ifdef __STDC__
SCM
scm_make_mutex ()
#else
SCM
scm_make_mutex ()
#endif
{
  SCM m;
  coop_m *data = (coop_m *) scm_must_malloc (sizeof (coop_m), "mutex");
  SCM_NEWCELL (m);
  SCM_DEFER_INTS;
  SCM_SETCAR (m, scm_tc16_mutex);
  SCM_SETCDR (m, data);
  SCM_ALLOW_INTS;
  coop_mutex_init (data);
  return m;
}

#ifdef __STDC__
SCM
scm_lock_mutex (SCM m)
#else
SCM
scm_lock_mutex (m)
     SCM m;
#endif
{
  SCM_ASSERT (SCM_NIMP (m) && SCM_MUTEXP (m), m, SCM_ARG1, s_lock_mutex);
  coop_mutex_lock (SCM_MUTEX_DATA (m));
  return SCM_BOOL_T;
}

#ifdef __STDC__
SCM
scm_unlock_mutex (SCM m)
#else
SCM
scm_unlock_mutex (m)
     SCM m;
#endif
{
  SCM_ASSERT (SCM_NIMP (m) && SCM_MUTEXP (m), m, SCM_ARG1, s_unlock_mutex);
  coop_mutex_unlock(SCM_MUTEX_DATA (m));

  /* Yield early */
  scm_switch_counter = SCM_THREAD_SWITCH_COUNT;
  coop_yield();

  return SCM_BOOL_T;
}

#ifdef __STDC__
SCM
scm_make_condition_variable ()
#else
SCM
scm_make_condition_variable ()
#endif
{
  SCM c;
  coop_c *data = (coop_c *) scm_must_malloc (sizeof (coop_c), "condvar");
  SCM_NEWCELL (c);
  SCM_DEFER_INTS;
  SCM_SETCAR (c, scm_tc16_condvar);
  SCM_SETCDR (c, data);
  SCM_ALLOW_INTS;
  coop_condition_variable_init (SCM_CONDVAR_DATA (c));
  return c;
}

#ifdef __STDC__
SCM
scm_wait_condition_variable (SCM c, SCM m)
#else
SCM
scm_wait_condition_variable (c, m)
     SCM c;
     SCM m;
#endif
{
  SCM_ASSERT (SCM_NIMP (c) && SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_wait_condition_variable);
  SCM_ASSERT (SCM_NIMP (m) && SCM_MUTEXP (m),
	      m,
	      SCM_ARG2,
	      s_wait_condition_variable);
  coop_mutex_unlock (SCM_MUTEX_DATA (m));
  coop_condition_variable_wait (SCM_CONDVAR_DATA (c));
  coop_mutex_lock (SCM_MUTEX_DATA (m));
  return SCM_BOOL_T;
}

#ifdef __STDC__
SCM
scm_signal_condition_variable (SCM c)
#else
SCM
scm_signal_condition_variable (c)
     SCM c;
#endif
{
  SCM_ASSERT (SCM_NIMP (c) && SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_signal_condition_variable);
  coop_condition_variable_signal (SCM_CONDVAR_DATA (c));
  return SCM_BOOL_T;
}

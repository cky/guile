/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/bdw-gc.h"
#include "libguile/_scm.h"

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>   /* for memset used by FD_ZERO on Solaris 10 */
#endif

#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include <assert.h>

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
#include "libguile/gc.h"
#include "libguile/init.h"
#include "libguile/scmsigs.h"
#include "libguile/strings.h"
#include "libguile/weaks.h"

#ifdef __MINGW32__
#ifndef ETIMEDOUT
# define ETIMEDOUT       WSAETIMEDOUT
#endif
# include <fcntl.h>
# include <process.h>
# define pipe(fd) _pipe (fd, 256, O_BINARY)
#endif /* __MINGW32__ */

#include <full-read.h>




/* First some libgc shims. */

/* Make sure GC_fn_type is defined; it is missing from the public
   headers of GC 7.1 and earlier.  */
#ifndef HAVE_GC_FN_TYPE
typedef void * (* GC_fn_type) (void *);
#endif


#ifndef GC_SUCCESS
#define GC_SUCCESS 0
#endif

#ifndef GC_UNIMPLEMENTED
#define GC_UNIMPLEMENTED 3
#endif

/* Likewise struct GC_stack_base is missing before 7.1.  */
#ifndef HAVE_GC_STACK_BASE
struct GC_stack_base {
  void * mem_base; /* Base of memory stack. */
#ifdef __ia64__
  void * reg_base; /* Base of separate register stack. */
#endif
};

static int
GC_register_my_thread (struct GC_stack_base *stack_base)
{
  return GC_UNIMPLEMENTED;
}

static void
GC_unregister_my_thread ()
{
}

#if !SCM_USE_PTHREAD_THREADS
/* No threads; we can just use GC_stackbottom.  */
static void *
get_thread_stack_base ()
{
  return GC_stackbottom;
}

#elif defined HAVE_PTHREAD_ATTR_GETSTACK && defined HAVE_PTHREAD_GETATTR_NP \
  && defined PTHREAD_ATTR_GETSTACK_WORKS
/* This method for GNU/Linux and perhaps some other systems.
   It's not for MacOS X or Solaris 10, since pthread_getattr_np is not
   available on them.  */
static void *
get_thread_stack_base ()
{
  pthread_attr_t attr;
  void *start, *end;
  size_t size;

  pthread_getattr_np (pthread_self (), &attr);
  pthread_attr_getstack (&attr, &start, &size);
  end = (char *)start + size;

#if SCM_STACK_GROWS_UP
  return start;
#else
  return end;
#endif
}

#elif defined HAVE_PTHREAD_GET_STACKADDR_NP
/* This method for MacOS X.
   It'd be nice if there was some documentation on pthread_get_stackaddr_np,
   but as of 2006 there's nothing obvious at apple.com.  */
static void *
get_thread_stack_base ()
{
  return pthread_get_stackaddr_np (pthread_self ());
}

#else 
#error Threads enabled with old BDW-GC, but missing get_thread_stack_base impl.  Please upgrade to libgc >= 7.1.
#endif

static int
GC_get_stack_base (struct GC_stack_base *stack_base)
{
  stack_base->mem_base = get_thread_stack_base ();
#ifdef __ia64__
  /* Calculate and store off the base of this thread's register
     backing store (RBS).  Unfortunately our implementation(s) of
     scm_ia64_register_backing_store_base are only reliable for the
     main thread.  For other threads, therefore, find out the current
     top of the RBS, and use that as a maximum. */
  stack_base->reg_base = scm_ia64_register_backing_store_base ();
  {
    ucontext_t ctx;
    void *bsp;
    getcontext (&ctx);
    bsp = scm_ia64_ar_bsp (&ctx);
    if (stack_base->reg_base > bsp)
      stack_base->reg_base = bsp;
  }
#endif
  return GC_SUCCESS;
}

static void *
GC_call_with_stack_base(void * (*fn) (struct GC_stack_base*, void*), void *arg)
{
  struct GC_stack_base stack_base;

  stack_base.mem_base = (void*)&stack_base;
#ifdef __ia64__
  /* FIXME: Untested.  */
  {
    ucontext_t ctx;
    getcontext (&ctx);
    stack_base.reg_base = scm_ia64_ar_bsp (&ctx);
  }
#endif

  return fn (&stack_base, arg);
}
#endif /* HAVE_GC_STACK_BASE */


/* Now define with_gc_active and with_gc_inactive.  */

#if (defined(HAVE_GC_DO_BLOCKING) && defined (HAVE_DECL_GC_DO_BLOCKING) && defined (HAVE_GC_CALL_WITH_GC_ACTIVE))

/* We have a sufficiently new libgc (7.2 or newer).  */

static void*
with_gc_inactive (GC_fn_type func, void *data)
{
  return GC_do_blocking (func, data);
}

static void*
with_gc_active (GC_fn_type func, void *data)
{
  return GC_call_with_gc_active (func, data);
}

#else

/* libgc not new enough, so never actually deactivate GC.

   Note that though GC 7.1 does have a GC_do_blocking, it doesn't have
   GC_call_with_gc_active.  */

static void*
with_gc_inactive (GC_fn_type func, void *data)
{
  return func (data);
}

static void*
with_gc_active (GC_fn_type func, void *data)
{
  return func (data);
}

#endif /* HAVE_GC_DO_BLOCKING */



static void
to_timespec (SCM t, scm_t_timespec *waittime)
{
  if (scm_is_pair (t))
    {
      waittime->tv_sec = scm_to_ulong (SCM_CAR (t));
      waittime->tv_nsec = scm_to_ulong (SCM_CDR (t)) * 1000;
    }
  else
    {
      double time = scm_to_double (t);
      double sec = scm_c_truncate (time);

      waittime->tv_sec = (long) sec;
      waittime->tv_nsec = (long) ((time - sec) * 1000000000);
    }
}


/*** Queues */

/* Note: We annotate with "GC-robust" assignments whose purpose is to avoid
   the risk of false references leading to unbounded retained space as
   described in "Bounding Space Usage of Conservative Garbage Collectors",
   H.J. Boehm, 2001.  */

/* Make an empty queue data structure.
 */
static SCM
make_queue ()
{
  return scm_cons (SCM_EOL, SCM_EOL);
}

/* Put T at the back of Q and return a handle that can be used with
   remqueue to remove T from Q again.
 */
static SCM
enqueue (SCM q, SCM t)
{
  SCM c = scm_cons (t, SCM_EOL);
  SCM_CRITICAL_SECTION_START;
  if (scm_is_null (SCM_CDR (q)))
    SCM_SETCDR (q, c);
  else
    SCM_SETCDR (SCM_CAR (q), c);
  SCM_SETCAR (q, c);
  SCM_CRITICAL_SECTION_END;
  return c;
}

/* Remove the element that the handle C refers to from the queue Q.  C
   must have been returned from a call to enqueue.  The return value
   is zero when the element referred to by C has already been removed.
   Otherwise, 1 is returned.
*/
static int
remqueue (SCM q, SCM c)
{
  SCM p, prev = q;
  SCM_CRITICAL_SECTION_START;
  for (p = SCM_CDR (q); !scm_is_null (p); p = SCM_CDR (p))
    {
      if (scm_is_eq (p, c))
	{
	  if (scm_is_eq (c, SCM_CAR (q)))
	    SCM_SETCAR (q, SCM_CDR (c));
	  SCM_SETCDR (prev, SCM_CDR (c));

	  /* GC-robust */
	  SCM_SETCDR (c, SCM_EOL);

	  SCM_CRITICAL_SECTION_END;
	  return 1;
	}
      prev = p;
    }
  SCM_CRITICAL_SECTION_END;
  return 0;
}

/* Remove the front-most element from the queue Q and return it.
   Return SCM_BOOL_F when Q is empty.
*/
static SCM
dequeue (SCM q)
{
  SCM c;
  SCM_CRITICAL_SECTION_START;
  c = SCM_CDR (q);
  if (scm_is_null (c))
    {
      SCM_CRITICAL_SECTION_END;
      return SCM_BOOL_F;
    }
  else
    {
      SCM_SETCDR (q, SCM_CDR (c));
      if (scm_is_null (SCM_CDR (q)))
	SCM_SETCAR (q, SCM_EOL);
      SCM_CRITICAL_SECTION_END;

      /* GC-robust */
      SCM_SETCDR (c, SCM_EOL);

      return SCM_CAR (c);
    }
}

/*** Thread smob routines */


static int
thread_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  /* On a Gnu system pthread_t is an unsigned long, but on mingw it's a
     struct.  A cast like "(unsigned long) t->pthread" is a syntax error in
     the struct case, hence we go via a union, and extract according to the
     size of pthread_t.  */
  union {
    scm_i_pthread_t p;
    unsigned short us;
    unsigned int   ui;
    unsigned long  ul;
    scm_t_uintmax  um;
  } u;
  scm_i_thread *t = SCM_I_THREAD_DATA (exp);
  scm_i_pthread_t p = t->pthread;
  scm_t_uintmax id;
  u.p = p;
  if (sizeof (p) == sizeof (unsigned short))
    id = u.us;
  else if (sizeof (p) == sizeof (unsigned int))
    id = u.ui;
  else if (sizeof (p) == sizeof (unsigned long))
    id = u.ul;
  else
    id = u.um;

  scm_puts ("#<thread ", port);
  scm_uintprint (id, 10, port);
  scm_puts (" (", port);
  scm_uintprint ((scm_t_bits)t, 16, port);
  scm_puts (")>", port);
  return 1;
}


/*** Blocking on queues. */

/* See also scm_i_queue_async_cell for how such a block is
   interrputed.
*/

/* Put the current thread on QUEUE and go to sleep, waiting for it to
   be woken up by a call to 'unblock_from_queue', or to be
   interrupted.  Upon return of this function, the current thread is
   no longer on QUEUE, even when the sleep has been interrupted.

   The caller of block_self must hold MUTEX.  It will be atomically
   unlocked while sleeping, just as with scm_i_pthread_cond_wait.

   SLEEP_OBJECT is an arbitrary SCM value that is kept alive as long
   as MUTEX is needed.

   When WAITTIME is not NULL, the sleep will be aborted at that time.

   The return value of block_self is an errno value.  It will be zero
   when the sleep has been successfully completed by a call to
   unblock_from_queue, EINTR when it has been interrupted by the
   delivery of a system async, and ETIMEDOUT when the timeout has
   expired.

   The system asyncs themselves are not executed by block_self.
*/
static int
block_self (SCM queue, SCM sleep_object, scm_i_pthread_mutex_t *mutex,
	    const scm_t_timespec *waittime)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  SCM q_handle;
  int err;

  if (scm_i_setup_sleep (t, sleep_object, mutex, -1))
    err = EINTR;
  else
    {
      t->block_asyncs++;
      q_handle = enqueue (queue, t->handle);
      if (waittime == NULL)
	err = scm_i_scm_pthread_cond_wait (&t->sleep_cond, mutex);
      else
	err = scm_i_scm_pthread_cond_timedwait (&t->sleep_cond, mutex, waittime);

      /* When we are still on QUEUE, we have been interrupted.  We
	 report this only when no other error (such as a timeout) has
	 happened above.
      */
      if (remqueue (queue, q_handle) && err == 0)
	err = EINTR;
      t->block_asyncs--;
      scm_i_reset_sleep (t);
    }

  return err;
}

/* Wake up the first thread on QUEUE, if any.  The awoken thread is
   returned, or #f if the queue was empty.
 */
static SCM
unblock_from_queue (SCM queue)
{
  SCM thread = dequeue (queue);
  if (scm_is_true (thread))
    scm_i_pthread_cond_signal (&SCM_I_THREAD_DATA(thread)->sleep_cond);
  return thread;
}


/* Getting into and out of guile mode.
 */

/* Key used to attach a cleanup handler to a given thread.  Also, if
   thread-local storage is unavailable, this key is used to retrieve the
   current thread with `pthread_getspecific ()'.  */
scm_i_pthread_key_t scm_i_thread_key;


#ifdef SCM_HAVE_THREAD_STORAGE_CLASS

/* When thread-local storage (TLS) is available, a pointer to the
   current-thread object is kept in TLS.  Note that storing the thread-object
   itself in TLS (rather than a pointer to some malloc'd memory) is not
   possible since thread objects may live longer than the actual thread they
   represent.  */
SCM_THREAD_LOCAL scm_i_thread *scm_i_current_thread = NULL;

#endif /* SCM_HAVE_THREAD_STORAGE_CLASS */


static scm_i_pthread_mutex_t thread_admin_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;
static scm_i_thread *all_threads = NULL;
static int thread_count;

static SCM scm_i_default_dynamic_state;

/* Perform first stage of thread initialisation, in non-guile mode.
 */
static void
guilify_self_1 (struct GC_stack_base *base)
{
  scm_i_thread t;

  /* We must arrange for SCM_I_CURRENT_THREAD to point to a valid value
     before allocating anything in this thread, because allocation could
     cause GC to run, and GC could cause finalizers, which could invoke
     Scheme functions, which need the current thread to be set.  */

  t.pthread = scm_i_pthread_self ();
  t.handle = SCM_BOOL_F;
  t.result = SCM_BOOL_F;
  t.cleanup_handler = SCM_BOOL_F;
  t.mutexes = SCM_EOL;
  t.held_mutex = NULL;
  t.join_queue = SCM_EOL;
  t.dynamic_state = SCM_BOOL_F;
  t.dynwinds = SCM_EOL;
  t.active_asyncs = SCM_EOL;
  t.block_asyncs = 1;
  t.pending_asyncs = 1;
  t.critical_section_level = 0;
  t.base = base->mem_base;
#ifdef __ia64__
  t.register_backing_store_base = base->reg-base;
#endif
  t.continuation_root = SCM_EOL;
  t.continuation_base = t.base;
  scm_i_pthread_cond_init (&t.sleep_cond, NULL);
  t.sleep_mutex = NULL;
  t.sleep_object = SCM_BOOL_F;
  t.sleep_fd = -1;

  if (pipe (t.sleep_pipe) != 0)
    /* FIXME: Error conditions during the initialization phase are handled
       gracelessly since public functions such as `scm_init_guile ()'
       currently have type `void'.  */
    abort ();

  scm_i_pthread_mutex_init (&t.admin_mutex, NULL);
  t.current_mark_stack_ptr = NULL;
  t.current_mark_stack_limit = NULL;
  t.canceled = 0;
  t.exited = 0;
  t.guile_mode = 0;

  /* The switcheroo.  */
  {
    scm_i_thread *t_ptr = &t;
    
    GC_disable ();
    t_ptr = GC_malloc (sizeof (scm_i_thread));
    memcpy (t_ptr, &t, sizeof t);

    scm_i_pthread_setspecific (scm_i_thread_key, t_ptr);

#ifdef SCM_HAVE_THREAD_STORAGE_CLASS
    /* Cache the current thread in TLS for faster lookup.  */
    scm_i_current_thread = t_ptr;
#endif

    scm_i_pthread_mutex_lock (&thread_admin_mutex);
    t_ptr->next_thread = all_threads;
    all_threads = t_ptr;
    thread_count++;
    scm_i_pthread_mutex_unlock (&thread_admin_mutex);

    GC_enable ();
  }
}

/* Perform second stage of thread initialisation, in guile mode.
 */
static void
guilify_self_2 (SCM parent)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  t->guile_mode = 1;

  SCM_NEWSMOB (t->handle, scm_tc16_thread, t);

  t->continuation_root = scm_cons (t->handle, SCM_EOL);
  t->continuation_base = t->base;
  t->vm = SCM_BOOL_F;

  if (scm_is_true (parent))
    t->dynamic_state = scm_make_dynamic_state (parent);
  else
    t->dynamic_state = scm_i_make_initial_dynamic_state ();

  t->join_queue = make_queue ();
  t->block_asyncs = 0;
}


/*** Fat mutexes */

/* We implement our own mutex type since we want them to be 'fair', we
   want to do fancy things while waiting for them (like running
   asyncs) and we might want to add things that are nice for
   debugging.
*/

typedef struct {
  scm_i_pthread_mutex_t lock;
  SCM owner;
  int level; /* how much the owner owns us.  <= 1 for non-recursive mutexes */

  int recursive; /* allow recursive locking? */
  int unchecked_unlock; /* is it an error to unlock an unlocked mutex? */
  int allow_external_unlock; /* is it an error to unlock a mutex that is not
				owned by the current thread? */

  SCM waiting;    /* the threads waiting for this mutex. */
} fat_mutex;

#define SCM_MUTEXP(x)         SCM_SMOB_PREDICATE (scm_tc16_mutex, x)
#define SCM_MUTEX_DATA(x)     ((fat_mutex *) SCM_SMOB_DATA (x))

/* Perform thread tear-down, in guile mode.
 */
static void *
do_thread_exit (void *v)
{
  scm_i_thread *t = (scm_i_thread *) v;

  if (!scm_is_false (t->cleanup_handler))
    {
      SCM ptr = t->cleanup_handler;

      t->cleanup_handler = SCM_BOOL_F;
      t->result = scm_internal_catch (SCM_BOOL_T,
				      (scm_t_catch_body) scm_call_0, ptr,
				      scm_handle_by_message_noexit, NULL);
    }

  scm_i_scm_pthread_mutex_lock (&t->admin_mutex);

  t->exited = 1;
  close (t->sleep_pipe[0]);
  close (t->sleep_pipe[1]);
  while (scm_is_true (unblock_from_queue (t->join_queue)))
    ;

  while (!scm_is_null (t->mutexes))
    {
      SCM mutex = SCM_WEAK_PAIR_CAR (t->mutexes);

      if (!SCM_UNBNDP (mutex))
	{
	  fat_mutex *m  = SCM_MUTEX_DATA (mutex);

	  scm_i_pthread_mutex_lock (&m->lock);

	  /* Since MUTEX is in `t->mutexes', T must be its owner.  */
	  assert (scm_is_eq (m->owner, t->handle));

	  unblock_from_queue (m->waiting);

	  scm_i_pthread_mutex_unlock (&m->lock);
	}

      t->mutexes = SCM_WEAK_PAIR_CDR (t->mutexes);
    }

  scm_i_pthread_mutex_unlock (&t->admin_mutex);

  return NULL;
}

static void *
do_thread_exit_trampoline (struct GC_stack_base *sb, void *v)
{
  /* Won't hurt if we are already registered.  */
#if SCM_USE_PTHREAD_THREADS
  GC_register_my_thread (sb);
#endif

  return scm_with_guile (do_thread_exit, v);
}

static void
on_thread_exit (void *v)
{
  /* This handler is executed in non-guile mode.  */
  scm_i_thread *t = (scm_i_thread *) v, **tp;

  /* If this thread was cancelled while doing a cond wait, it will
     still have a mutex locked, so we unlock it here. */
  if (t->held_mutex)
    {
      scm_i_pthread_mutex_unlock (t->held_mutex);
      t->held_mutex = NULL;
    }

  /* Reinstate the current thread for purposes of scm_with_guile
     guile-mode cleanup handlers.  Only really needed in the non-TLS
     case but it doesn't hurt to be consistent.  */
  scm_i_pthread_setspecific (scm_i_thread_key, t);

  /* Ensure the signal handling thread has been launched, because we might be
     shutting it down.  */
  scm_i_ensure_signal_delivery_thread ();

  /* Scheme-level thread finalizers and other cleanup needs to happen in
     guile mode.  */
  GC_call_with_stack_base (do_thread_exit_trampoline, t);

  /* Removing ourself from the list of all threads needs to happen in
     non-guile mode since all SCM values on our stack become
     unprotected once we are no longer in the list.  */
  scm_i_pthread_mutex_lock (&thread_admin_mutex);
  for (tp = &all_threads; *tp; tp = &(*tp)->next_thread)
    if (*tp == t)
      {
	*tp = t->next_thread;

	/* GC-robust */
	t->next_thread = NULL;

	break;
      }
  thread_count--;

  /* If there's only one other thread, it could be the signal delivery
     thread, so we need to notify it to shut down by closing its read pipe.
     If it's not the signal delivery thread, then closing the read pipe isn't
     going to hurt.  */
  if (thread_count <= 1)
    scm_i_close_signal_pipe ();

  scm_i_pthread_mutex_unlock (&thread_admin_mutex);

  scm_i_pthread_setspecific (scm_i_thread_key, NULL);

#if SCM_USE_PTHREAD_THREADS
  GC_unregister_my_thread ();
#endif
}

static scm_i_pthread_once_t init_thread_key_once = SCM_I_PTHREAD_ONCE_INIT;

static void
init_thread_key (void)
{
  scm_i_pthread_key_create (&scm_i_thread_key, on_thread_exit);
}

/* Perform any initializations necessary to make the current thread
   known to Guile (via SCM_I_CURRENT_THREAD), initializing Guile itself,
   if necessary.

   BASE is the stack base to use with GC.

   PARENT is the dynamic state to use as the parent, ot SCM_BOOL_F in
   which case the default dynamic state is used.

   Returns zero when the thread was known to guile already; otherwise
   return 1.

   Note that it could be the case that the thread was known
   to Guile, but not in guile mode (because we are within a
   scm_without_guile call).   Check SCM_I_CURRENT_THREAD->guile_mode to
   be sure.  New threads are put into guile mode implicitly.  */

static int
scm_i_init_thread_for_guile (struct GC_stack_base *base, SCM parent)
{
  scm_i_pthread_once (&init_thread_key_once, init_thread_key);

  if (SCM_I_CURRENT_THREAD)
    {
      /* Thread is already known to Guile.
      */
      return 0;
    }
  else
    {
      /* This thread has not been guilified yet.
       */

      scm_i_pthread_mutex_lock (&scm_i_init_mutex);
      if (scm_initialized_p == 0)
	{
	  /* First thread ever to enter Guile.  Run the full
	     initialization.
	  */
	  scm_i_init_guile (base);

#if defined (HAVE_GC_ALLOW_REGISTER_THREADS) && SCM_USE_PTHREAD_THREADS
          /* Allow other threads to come in later.  */
          GC_allow_register_threads ();
#endif

	  scm_i_pthread_mutex_unlock (&scm_i_init_mutex);
	}
      else
	{
	  /* Guile is already initialized, but this thread enters it for
	     the first time.  Only initialize this thread.
	  */
	  scm_i_pthread_mutex_unlock (&scm_i_init_mutex);

          /* Register this thread with libgc.  */
#if SCM_USE_PTHREAD_THREADS
          GC_register_my_thread (base);
#endif

	  guilify_self_1 (base);
	  guilify_self_2 (parent);
	}
      return 1;
    }
}

void
scm_init_guile ()
{
  struct GC_stack_base stack_base;
  
  if (GC_get_stack_base (&stack_base) == GC_SUCCESS)
    scm_i_init_thread_for_guile (&stack_base,
                                 scm_i_default_dynamic_state);
  else
    {
      fprintf (stderr, "Failed to get stack base for current thread.\n");
      exit (1);
    }
}

SCM_UNUSED static void
scm_leave_guile_cleanup (void *x)
{
  on_thread_exit (SCM_I_CURRENT_THREAD);
}

struct with_guile_args
{
  GC_fn_type func;
  void *data;
  SCM parent;
};

static void *
with_guile_trampoline (void *data)
{
  struct with_guile_args *args = data;

  return scm_c_with_continuation_barrier (args->func, args->data);
}
  
static void *
with_guile_and_parent (struct GC_stack_base *base, void *data)
{
  void *res;
  int new_thread;
  scm_i_thread *t;
  struct with_guile_args *args = data;

  new_thread = scm_i_init_thread_for_guile (base, args->parent);
  t = SCM_I_CURRENT_THREAD;
  if (new_thread)
    {
      /* We are in Guile mode.  */
      assert (t->guile_mode);

      res = scm_c_with_continuation_barrier (args->func, args->data);

      /* Leave Guile mode.  */
      t->guile_mode = 0;
    }
  else if (t->guile_mode)
    {
      /* Already in Guile mode.  */
      res = scm_c_with_continuation_barrier (args->func, args->data);
    }
  else
    {
      /* We are not in Guile mode, either because we are not within a
         scm_with_guile, or because we are within a scm_without_guile.

         This call to scm_with_guile() could happen from anywhere on the
         stack, and in particular lower on the stack than when it was
         when this thread was first guilified.  Thus, `base' must be
         updated.  */
#if SCM_STACK_GROWS_UP
      if (SCM_STACK_PTR (base->mem_base) < t->base)
        t->base = SCM_STACK_PTR (base->mem_base);
#else
      if (SCM_STACK_PTR (base->mem_base) > t->base)
        t->base = SCM_STACK_PTR (base->mem_base);
#endif

      t->guile_mode = 1;
      res = with_gc_active (with_guile_trampoline, args);
      t->guile_mode = 0;
    }
  return res;
}

static void *
scm_i_with_guile_and_parent (void *(*func)(void *), void *data, SCM parent)
{
  struct with_guile_args args;

  args.func = func;
  args.data = data;
  args.parent = parent;
  
  return GC_call_with_stack_base (with_guile_and_parent, &args);
}

void *
scm_with_guile (void *(*func)(void *), void *data)
{
  return scm_i_with_guile_and_parent (func, data,
				      scm_i_default_dynamic_state);
}

void *
scm_without_guile (void *(*func)(void *), void *data)
{
  void *result;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (t->guile_mode)
    {
      SCM_I_CURRENT_THREAD->guile_mode = 0;
      result = with_gc_inactive (func, data);
      SCM_I_CURRENT_THREAD->guile_mode = 1;
    }
  else
    /* Otherwise we're not in guile mode, so nothing to do.  */
    result = func (data);

  return result;
}


/*** Thread creation */

typedef struct {
  SCM parent;
  SCM thunk;
  SCM handler;
  SCM thread;
  scm_i_pthread_mutex_t mutex;
  scm_i_pthread_cond_t cond;
} launch_data;

static void *
really_launch (void *d)
{
  launch_data *data = (launch_data *)d;
  SCM thunk = data->thunk, handler = data->handler;
  scm_i_thread *t;

  t = SCM_I_CURRENT_THREAD;

  scm_i_scm_pthread_mutex_lock (&data->mutex);
  data->thread = scm_current_thread ();
  scm_i_pthread_cond_signal (&data->cond);
  scm_i_pthread_mutex_unlock (&data->mutex);

  if (SCM_UNBNDP (handler))
    t->result = scm_call_0 (thunk);
  else
    t->result = scm_catch (SCM_BOOL_T, thunk, handler);

  return 0;
}

static void *
launch_thread (void *d)
{
  launch_data *data = (launch_data *)d;
  scm_i_pthread_detach (scm_i_pthread_self ());
  scm_i_with_guile_and_parent (really_launch, d, data->parent);
  return NULL;
}

SCM_DEFINE (scm_call_with_new_thread, "call-with-new-thread", 1, 1, 0,
	    (SCM thunk, SCM handler),
	    "Call @code{thunk} in a new thread and with a new dynamic state,\n"
	    "returning a new thread object representing the thread.  The procedure\n"
	    "@var{thunk} is called via @code{with-continuation-barrier}.\n"
	    "\n"
	    "When @var{handler} is specified, then @var{thunk} is called from\n"
	    "within a @code{catch} with tag @code{#t} that has @var{handler} as its\n"
	    "handler.  This catch is established inside the continuation barrier.\n"
	    "\n"
	    "Once @var{thunk} or @var{handler} returns, the return value is made\n"
	    "the @emph{exit value} of the thread and the thread is terminated.")
#define FUNC_NAME s_scm_call_with_new_thread
{
  launch_data data;
  scm_i_pthread_t id;
  int err;

  SCM_ASSERT (scm_is_true (scm_thunk_p (thunk)), thunk, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_UNBNDP (handler) || scm_is_true (scm_procedure_p (handler)),
	      handler, SCM_ARG2, FUNC_NAME);

  data.parent = scm_current_dynamic_state ();
  data.thunk = thunk;
  data.handler = handler;
  data.thread = SCM_BOOL_F;
  scm_i_pthread_mutex_init (&data.mutex, NULL);
  scm_i_pthread_cond_init (&data.cond, NULL);

  scm_i_scm_pthread_mutex_lock (&data.mutex);
  err = scm_i_pthread_create (&id, NULL, launch_thread, &data);
  if (err)
    {
      scm_i_pthread_mutex_unlock (&data.mutex);
      errno = err;
      scm_syserror (NULL);
    }
  scm_i_scm_pthread_cond_wait (&data.cond, &data.mutex);
  scm_i_pthread_mutex_unlock (&data.mutex);

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
  scm_i_pthread_mutex_t mutex;
  scm_i_pthread_cond_t cond;
} spawn_data;

static void *
really_spawn (void *d)
{
  spawn_data *data = (spawn_data *)d;
  scm_t_catch_body body = data->body;
  void *body_data = data->body_data;
  scm_t_catch_handler handler = data->handler;
  void *handler_data = data->handler_data;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  scm_i_scm_pthread_mutex_lock (&data->mutex);
  data->thread = scm_current_thread ();
  scm_i_pthread_cond_signal (&data->cond);
  scm_i_pthread_mutex_unlock (&data->mutex);

  if (handler == NULL)
    t->result = body (body_data);
  else
    t->result = scm_internal_catch (SCM_BOOL_T,
				    body, body_data,
				    handler, handler_data);

  return 0;
}

static void *
spawn_thread (void *d)
{
  spawn_data *data = (spawn_data *)d;
  scm_i_pthread_detach (scm_i_pthread_self ());
  scm_i_with_guile_and_parent (really_spawn, d, data->parent);
  return NULL;
}

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  spawn_data data;
  scm_i_pthread_t id;
  int err;

  data.parent = scm_current_dynamic_state ();
  data.body = body;
  data.body_data = body_data;
  data.handler = handler;
  data.handler_data = handler_data;
  data.thread = SCM_BOOL_F;
  scm_i_pthread_mutex_init (&data.mutex, NULL);
  scm_i_pthread_cond_init (&data.cond, NULL);

  scm_i_scm_pthread_mutex_lock (&data.mutex);
  err = scm_i_pthread_create (&id, NULL, spawn_thread, &data);
  if (err)
    {
      scm_i_pthread_mutex_unlock (&data.mutex);
      errno = err;
      scm_syserror (NULL);
    }
  scm_i_scm_pthread_cond_wait (&data.cond, &data.mutex);
  scm_i_pthread_mutex_unlock (&data.mutex);

  return data.thread;
}

SCM_DEFINE (scm_yield, "yield", 0, 0, 0,
	    (),
"Move the calling thread to the end of the scheduling queue.")
#define FUNC_NAME s_scm_yield
{
  return scm_from_bool (scm_i_sched_yield ());
}
#undef FUNC_NAME

SCM_DEFINE (scm_cancel_thread, "cancel-thread", 1, 0, 0,
	    (SCM thread),
"Asynchronously force the target @var{thread} to terminate. @var{thread} "
"cannot be the current thread, and if @var{thread} has already terminated or "
"been signaled to terminate, this function is a no-op.")
#define FUNC_NAME s_scm_cancel_thread
{
  scm_i_thread *t = NULL;

  SCM_VALIDATE_THREAD (1, thread);
  t = SCM_I_THREAD_DATA (thread);
  scm_i_scm_pthread_mutex_lock (&t->admin_mutex);
  if (!t->canceled)
    {
      t->canceled = 1;
      scm_i_pthread_mutex_unlock (&t->admin_mutex);
      scm_i_pthread_cancel (t->pthread);
    }
  else
    scm_i_pthread_mutex_unlock (&t->admin_mutex);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_thread_cleanup_x, "set-thread-cleanup!", 2, 0, 0,
	    (SCM thread, SCM proc),
"Set the thunk @var{proc} as the cleanup handler for the thread @var{thread}. "
"This handler will be called when the thread exits.")
#define FUNC_NAME s_scm_set_thread_cleanup_x
{
  scm_i_thread *t;

  SCM_VALIDATE_THREAD (1, thread);
  if (!scm_is_false (proc))
    SCM_VALIDATE_THUNK (2, proc);

  t = SCM_I_THREAD_DATA (thread);
  scm_i_pthread_mutex_lock (&t->admin_mutex);

  if (!(t->exited || t->canceled))
    t->cleanup_handler = proc;

  scm_i_pthread_mutex_unlock (&t->admin_mutex);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_thread_cleanup, "thread-cleanup", 1, 0, 0,
	    (SCM thread),
"Return the cleanup handler installed for the thread @var{thread}.")
#define FUNC_NAME s_scm_thread_cleanup
{
  scm_i_thread *t;
  SCM ret;

  SCM_VALIDATE_THREAD (1, thread);

  t = SCM_I_THREAD_DATA (thread);
  scm_i_pthread_mutex_lock (&t->admin_mutex);
  ret = (t->exited || t->canceled) ? SCM_BOOL_F : t->cleanup_handler;
  scm_i_pthread_mutex_unlock (&t->admin_mutex);

  return ret;
}
#undef FUNC_NAME

SCM scm_join_thread (SCM thread)
{
  return scm_join_thread_timed (thread, SCM_UNDEFINED, SCM_UNDEFINED);
}

SCM_DEFINE (scm_join_thread_timed, "join-thread", 1, 2, 0,
	    (SCM thread, SCM timeout, SCM timeoutval),
"Suspend execution of the calling thread until the target @var{thread} "
"terminates, unless the target @var{thread} has already terminated. ")
#define FUNC_NAME s_scm_join_thread_timed
{
  scm_i_thread *t;
  scm_t_timespec ctimeout, *timeout_ptr = NULL;
  SCM res = SCM_BOOL_F;

  if (! (SCM_UNBNDP (timeoutval)))
    res = timeoutval;

  SCM_VALIDATE_THREAD (1, thread);
  if (scm_is_eq (scm_current_thread (), thread))
    SCM_MISC_ERROR ("cannot join the current thread", SCM_EOL);

  t = SCM_I_THREAD_DATA (thread);
  scm_i_scm_pthread_mutex_lock (&t->admin_mutex);

  if (! SCM_UNBNDP (timeout))
    {
      to_timespec (timeout, &ctimeout);
      timeout_ptr = &ctimeout;
    }

  if (t->exited)
    res = t->result;
  else
    {
      while (1)
	{
	  int err = block_self (t->join_queue, thread, &t->admin_mutex,
				timeout_ptr);
	  if (err == 0)
	    {
	      if (t->exited)
		{
		  res = t->result;
		  break;
		}
	    }
	  else if (err == ETIMEDOUT)
	    break;

	  scm_i_pthread_mutex_unlock (&t->admin_mutex);
	  SCM_TICK;
	  scm_i_scm_pthread_mutex_lock (&t->admin_mutex);

	  /* Check for exit again, since we just released and
	     reacquired the admin mutex, before the next block_self
	     call (which would block forever if t has already
	     exited). */
	  if (t->exited)
	    {
	      res = t->result;
	      break;
	    }
	}
    }

  scm_i_pthread_mutex_unlock (&t->admin_mutex);

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_thread_p, "thread?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a thread.")
#define FUNC_NAME s_scm_thread_p
{
  return SCM_I_IS_THREAD(obj) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


static size_t
fat_mutex_free (SCM mx)
{
  fat_mutex *m = SCM_MUTEX_DATA (mx);
  scm_i_pthread_mutex_destroy (&m->lock);
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
make_fat_mutex (int recursive, int unchecked_unlock, int external_unlock)
{
  fat_mutex *m;
  SCM mx;

  m = scm_gc_malloc (sizeof (fat_mutex), "mutex");
  scm_i_pthread_mutex_init (&m->lock, NULL);
  m->owner = SCM_BOOL_F;
  m->level = 0;

  m->recursive = recursive;
  m->unchecked_unlock = unchecked_unlock;
  m->allow_external_unlock = external_unlock;

  m->waiting = SCM_EOL;
  SCM_NEWSMOB (mx, scm_tc16_mutex, (scm_t_bits) m);
  m->waiting = make_queue ();
  return mx;
}

SCM scm_make_mutex (void)
{
  return scm_make_mutex_with_flags (SCM_EOL);
}

SCM_SYMBOL (unchecked_unlock_sym, "unchecked-unlock");
SCM_SYMBOL (allow_external_unlock_sym, "allow-external-unlock");
SCM_SYMBOL (recursive_sym, "recursive");

SCM_DEFINE (scm_make_mutex_with_flags, "make-mutex", 0, 0, 1,
	    (SCM flags),
	    "Create a new mutex. ")
#define FUNC_NAME s_scm_make_mutex_with_flags
{
  int unchecked_unlock = 0, external_unlock = 0, recursive = 0;

  SCM ptr = flags;
  while (! scm_is_null (ptr))
    {
      SCM flag = SCM_CAR (ptr);
      if (scm_is_eq (flag, unchecked_unlock_sym))
	unchecked_unlock = 1;
      else if (scm_is_eq (flag, allow_external_unlock_sym))
	external_unlock = 1;
      else if (scm_is_eq (flag, recursive_sym))
	recursive = 1;
      else
	SCM_MISC_ERROR ("unsupported mutex option: ~a", scm_list_1 (flag));
      ptr = SCM_CDR (ptr);
    }
  return make_fat_mutex (recursive, unchecked_unlock, external_unlock);
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_recursive_mutex, "make-recursive-mutex", 0, 0, 0,
	    (void),
	    "Create a new recursive mutex. ")
#define FUNC_NAME s_scm_make_recursive_mutex
{
  return make_fat_mutex (1, 0, 0);
}
#undef FUNC_NAME

SCM_SYMBOL (scm_abandoned_mutex_error_key, "abandoned-mutex-error");

static SCM
fat_mutex_lock (SCM mutex, scm_t_timespec *timeout, SCM owner, int *ret)
{
  fat_mutex *m = SCM_MUTEX_DATA (mutex);

  SCM new_owner = SCM_UNBNDP (owner) ? scm_current_thread() : owner;
  SCM err = SCM_BOOL_F;

  struct timeval current_time;

  scm_i_scm_pthread_mutex_lock (&m->lock);

  while (1)
    {
      if (m->level == 0)
	{
	  m->owner = new_owner;
	  m->level++;

	  if (SCM_I_IS_THREAD (new_owner))
	    {
	      scm_i_thread *t = SCM_I_THREAD_DATA (new_owner);
	      scm_i_pthread_mutex_lock (&t->admin_mutex);

	      /* Only keep a weak reference to MUTEX so that it's not
		 retained when not referenced elsewhere (bug #27450).
		 The weak pair itself is eventually removed when MUTEX
		 is unlocked.  Note that `t->mutexes' lists mutexes
		 currently held by T, so it should be small.  */
	      t->mutexes = scm_weak_car_pair (mutex, t->mutexes);

	      scm_i_pthread_mutex_unlock (&t->admin_mutex);
	    }
	  *ret = 1;
	  break;
	}
      else if (SCM_I_IS_THREAD (m->owner) && scm_c_thread_exited_p (m->owner))
	{
	  m->owner = new_owner;
	  err = scm_cons (scm_abandoned_mutex_error_key,
			  scm_from_locale_string ("lock obtained on abandoned "
						  "mutex"));
	  *ret = 1;
	  break;
	}
      else if (scm_is_eq (m->owner, new_owner))
	{
	  if (m->recursive)
	    {
	      m->level++;
	      *ret = 1;
	    }
	  else
	    {
	      err = scm_cons (scm_misc_error_key,
			      scm_from_locale_string ("mutex already locked "
						      "by thread"));
	      *ret = 0;
	    }
	  break;
	}
      else
	{
	  if (timeout != NULL)
	    {
	      gettimeofday (&current_time, NULL);
	      if (current_time.tv_sec > timeout->tv_sec ||
		  (current_time.tv_sec == timeout->tv_sec &&
		   current_time.tv_usec * 1000 > timeout->tv_nsec))
		{
		  *ret = 0;
		  break;
		}
	    }
	  block_self (m->waiting, mutex, &m->lock, timeout);
	  scm_i_pthread_mutex_unlock (&m->lock);
	  SCM_TICK;
	  scm_i_scm_pthread_mutex_lock (&m->lock);
	}
    }
  scm_i_pthread_mutex_unlock (&m->lock);
  return err;
}

SCM scm_lock_mutex (SCM mx)
{
  return scm_lock_mutex_timed (mx, SCM_UNDEFINED, SCM_UNDEFINED);
}

SCM_DEFINE (scm_lock_mutex_timed, "lock-mutex", 1, 2, 0,
	    (SCM m, SCM timeout, SCM owner),
"Lock @var{mutex}. If the mutex is already locked, the calling thread "
"blocks until the mutex becomes available. The function returns when "
"the calling thread owns the lock on @var{mutex}.  Locking a mutex that "
"a thread already owns will succeed right away and will not block the "
"thread.  That is, Guile's mutexes are @emph{recursive}. ")
#define FUNC_NAME s_scm_lock_mutex_timed
{
  SCM exception;
  int ret = 0;
  scm_t_timespec cwaittime, *waittime = NULL;

  SCM_VALIDATE_MUTEX (1, m);

  if (! SCM_UNBNDP (timeout) && ! scm_is_false (timeout))
    {
      to_timespec (timeout, &cwaittime);
      waittime = &cwaittime;
    }

  exception = fat_mutex_lock (m, waittime, owner, &ret);
  if (!scm_is_false (exception))
    scm_ithrow (SCM_CAR (exception), scm_list_1 (SCM_CDR (exception)), 1);
  return ret ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

void
scm_dynwind_lock_mutex (SCM mutex)
{
  scm_dynwind_unwind_handler_with_scm ((void(*)(SCM))scm_unlock_mutex, mutex,
				       SCM_F_WIND_EXPLICITLY);
  scm_dynwind_rewind_handler_with_scm ((void(*)(SCM))scm_lock_mutex, mutex,
				       SCM_F_WIND_EXPLICITLY);
}

SCM_DEFINE (scm_try_mutex, "try-mutex", 1, 0, 0,
	    (SCM mutex),
"Try to lock @var{mutex}. If the mutex is already locked by someone "
"else, return @code{#f}.  Else lock the mutex and return @code{#t}. ")
#define FUNC_NAME s_scm_try_mutex
{
  SCM exception;
  int ret = 0;
  scm_t_timespec cwaittime, *waittime = NULL;

  SCM_VALIDATE_MUTEX (1, mutex);

  to_timespec (scm_from_int(0), &cwaittime);
  waittime = &cwaittime;

  exception = fat_mutex_lock (mutex, waittime, SCM_UNDEFINED, &ret);
  if (!scm_is_false (exception))
    scm_ithrow (SCM_CAR (exception), scm_list_1 (SCM_CDR (exception)), 1);
  return ret ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

/*** Fat condition variables */

typedef struct {
  scm_i_pthread_mutex_t lock;
  SCM waiting;               /* the threads waiting for this condition. */
} fat_cond;

#define SCM_CONDVARP(x)       SCM_SMOB_PREDICATE (scm_tc16_condvar, x)
#define SCM_CONDVAR_DATA(x)   ((fat_cond *) SCM_SMOB_DATA (x))

static int
fat_mutex_unlock (SCM mutex, SCM cond,
		  const scm_t_timespec *waittime, int relock)
{
  SCM owner;
  fat_mutex *m = SCM_MUTEX_DATA (mutex);
  fat_cond *c = NULL;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  int err = 0, ret = 0;

  scm_i_scm_pthread_mutex_lock (&m->lock);

  owner = m->owner;

  if (!scm_is_eq (owner, t->handle))
    {
      if (m->level == 0)
	{
	  if (!m->unchecked_unlock)
	    {
	      scm_i_pthread_mutex_unlock (&m->lock);
	      scm_misc_error (NULL, "mutex not locked", SCM_EOL);
	    }
	  owner = t->handle;
	}
      else if (!m->allow_external_unlock)
	{
	  scm_i_pthread_mutex_unlock (&m->lock);
	  scm_misc_error (NULL, "mutex not locked by current thread", SCM_EOL);
	}
    }

  if (! (SCM_UNBNDP (cond)))
    {
      c = SCM_CONDVAR_DATA (cond);
      while (1)
	{
	  int brk = 0;

	  if (m->level > 0)
	    m->level--;
	  if (m->level == 0)
	    {
	      /* Change the owner of MUTEX.  */
	      t->mutexes = scm_delq_x (mutex, t->mutexes);
	      m->owner = unblock_from_queue (m->waiting);
	    }

	  t->block_asyncs++;

	  err = block_self (c->waiting, cond, &m->lock, waittime);
	  scm_i_pthread_mutex_unlock (&m->lock);

	  if (err == 0)
	    {
	      ret = 1;
	      brk = 1;
	    }
	  else if (err == ETIMEDOUT)
	    {
	      ret = 0;
	      brk = 1;
	    }
	  else if (err != EINTR)
	    {
	      errno = err;
	      scm_syserror (NULL);
	    }

	  if (brk)
	    {
	      if (relock)
		scm_lock_mutex_timed (mutex, SCM_UNDEFINED, owner);
	      t->block_asyncs--;
	      break;
	    }

	  t->block_asyncs--;
	  scm_async_click ();

	  scm_remember_upto_here_2 (cond, mutex);

	  scm_i_scm_pthread_mutex_lock (&m->lock);
	}
    }
  else
    {
      if (m->level > 0)
	m->level--;
      if (m->level == 0)
	{
	  /* Change the owner of MUTEX.  */
	  t->mutexes = scm_delq_x (mutex, t->mutexes);
	  m->owner = unblock_from_queue (m->waiting);
	}

      scm_i_pthread_mutex_unlock (&m->lock);
      ret = 1;
    }

  return ret;
}

SCM scm_unlock_mutex (SCM mx)
{
  return scm_unlock_mutex_timed (mx, SCM_UNDEFINED, SCM_UNDEFINED);
}

SCM_DEFINE (scm_unlock_mutex_timed, "unlock-mutex", 1, 2, 0,
	    (SCM mx, SCM cond, SCM timeout),
"Unlocks @var{mutex} if the calling thread owns the lock on "
"@var{mutex}.  Calling unlock-mutex on a mutex not owned by the current "
"thread results in undefined behaviour. Once a mutex has been unlocked, "
"one thread blocked on @var{mutex} is awakened and grabs the mutex "
"lock.  Every call to @code{lock-mutex} by this thread must be matched "
"with a call to @code{unlock-mutex}.  Only the last call to "
"@code{unlock-mutex} will actually unlock the mutex. ")
#define FUNC_NAME s_scm_unlock_mutex_timed
{
  scm_t_timespec cwaittime, *waittime = NULL;

  SCM_VALIDATE_MUTEX (1, mx);
  if (! (SCM_UNBNDP (cond)))
    {
      SCM_VALIDATE_CONDVAR (2, cond);

      if (! (SCM_UNBNDP (timeout)))
	{
	  to_timespec (timeout, &cwaittime);
	  waittime = &cwaittime;
	}
    }

  return fat_mutex_unlock (mx, cond, waittime, 0) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_mutex_p, "mutex?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a mutex.")
#define FUNC_NAME s_scm_mutex_p
{
  return SCM_MUTEXP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_mutex_owner, "mutex-owner", 1, 0, 0,
	    (SCM mx),
	    "Return the thread owning @var{mx}, or @code{#f}.")
#define FUNC_NAME s_scm_mutex_owner
{
  SCM owner;
  fat_mutex *m = NULL;

  SCM_VALIDATE_MUTEX (1, mx);
  m = SCM_MUTEX_DATA (mx);
  scm_i_pthread_mutex_lock (&m->lock);
  owner = m->owner;
  scm_i_pthread_mutex_unlock (&m->lock);

  return owner;
}
#undef FUNC_NAME

SCM_DEFINE (scm_mutex_level, "mutex-level", 1, 0, 0,
	    (SCM mx),
	    "Return the lock level of mutex @var{mx}.")
#define FUNC_NAME s_scm_mutex_level
{
  SCM_VALIDATE_MUTEX (1, mx);
  return scm_from_int (SCM_MUTEX_DATA(mx)->level);
}
#undef FUNC_NAME

SCM_DEFINE (scm_mutex_locked_p, "mutex-locked?", 1, 0, 0,
	    (SCM mx),
	    "Returns @code{#t} if the mutex @var{mx} is locked.")
#define FUNC_NAME s_scm_mutex_locked_p
{
  SCM_VALIDATE_MUTEX (1, mx);
  return SCM_MUTEX_DATA (mx)->level > 0 ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

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
  c->waiting = SCM_EOL;
  SCM_NEWSMOB (cv, scm_tc16_condvar, (scm_t_bits) c);
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
  scm_t_timespec waittime, *waitptr = NULL;

  SCM_VALIDATE_CONDVAR (1, cv);
  SCM_VALIDATE_MUTEX (2, mx);

  if (!SCM_UNBNDP (t))
    {
      to_timespec (t, &waittime);
      waitptr = &waittime;
    }

  return fat_mutex_unlock (mx, cv, waitptr, 1) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

static void
fat_cond_signal (fat_cond *c)
{
  unblock_from_queue (c->waiting);
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

static void
fat_cond_broadcast (fat_cond *c)
{
  while (scm_is_true (unblock_from_queue (c->waiting)))
    ;
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

SCM_DEFINE (scm_condition_variable_p, "condition-variable?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a condition variable.")
#define FUNC_NAME s_scm_condition_variable_p
{
  return SCM_CONDVARP(obj) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



/*** Select */

struct select_args
{
  int             nfds;
  SELECT_TYPE    *read_fds;
  SELECT_TYPE    *write_fds;
  SELECT_TYPE    *except_fds;
  struct timeval *timeout;

  int             result;
  int             errno_value;
};

static void *
do_std_select (void *args)
{
  struct select_args *select_args;

  select_args = (struct select_args *) args;

  select_args->result =
    select (select_args->nfds,
	    select_args->read_fds, select_args->write_fds,
	    select_args->except_fds, select_args->timeout);
  select_args->errno_value = errno;

  return NULL;
}

int
scm_std_select (int nfds,
		SELECT_TYPE *readfds,
		SELECT_TYPE *writefds,
		SELECT_TYPE *exceptfds,
		struct timeval *timeout)
{
  fd_set my_readfds;
  int res, eno, wakeup_fd;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  struct select_args args;

  if (readfds == NULL)
    {
      FD_ZERO (&my_readfds);
      readfds = &my_readfds;
    }

  while (scm_i_setup_sleep (t, SCM_BOOL_F, NULL, t->sleep_pipe[1]))
    SCM_TICK;

  wakeup_fd = t->sleep_pipe[0];
  FD_SET (wakeup_fd, readfds);
  if (wakeup_fd >= nfds)
    nfds = wakeup_fd+1;

  args.nfds = nfds;
  args.read_fds = readfds;
  args.write_fds = writefds;
  args.except_fds = exceptfds;
  args.timeout = timeout;

  /* Explicitly cooperate with the GC.  */
  scm_without_guile (do_std_select, &args);

  res = args.result;
  eno = args.errno_value;

  t->sleep_fd = -1;
  scm_i_reset_sleep (t);

  if (res > 0 && FD_ISSET (wakeup_fd, readfds))
    {
      char dummy;
      full_read (wakeup_fd, &dummy, 1);

      FD_CLR (wakeup_fd, readfds);
      res -= 1;
      if (res == 0)
	{
	  eno = EINTR;
	  res = -1;
	}
    }
  errno = eno;
  return res;
}

/* Convenience API for blocking while in guile mode. */

#if SCM_USE_PTHREAD_THREADS

/* It seems reasonable to not run procedures related to mutex and condition
   variables within `GC_do_blocking ()' since, (i) the GC can operate even
   without it, and (ii) the only potential gain would be GC latency.  See
   http://thread.gmane.org/gmane.comp.programming.garbage-collection.boehmgc/2245/focus=2251
   for a discussion of the pros and cons.  */

int
scm_pthread_mutex_lock (scm_i_pthread_mutex_t *mutex)
{
  int res = scm_i_pthread_mutex_lock (mutex);
  return res;
}

static void
do_unlock (void *data)
{
  scm_i_pthread_mutex_unlock ((scm_i_pthread_mutex_t *)data);
}

void
scm_dynwind_pthread_mutex_lock (scm_i_pthread_mutex_t *mutex)
{
  scm_i_scm_pthread_mutex_lock (mutex);
  scm_dynwind_unwind_handler (do_unlock, mutex, SCM_F_WIND_EXPLICITLY);
}

int
scm_pthread_cond_wait (scm_i_pthread_cond_t *cond, scm_i_pthread_mutex_t *mutex)
{
  int res;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  t->held_mutex = mutex;
  res = scm_i_pthread_cond_wait (cond, mutex);
  t->held_mutex = NULL;

  return res;
}

int
scm_pthread_cond_timedwait (scm_i_pthread_cond_t *cond,
			    scm_i_pthread_mutex_t *mutex,
			    const scm_t_timespec *wt)
{
  int res;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  t->held_mutex = mutex;
  res = scm_i_pthread_cond_timedwait (cond, mutex, wt);
  t->held_mutex = NULL;

  return res;
}

#endif

unsigned long
scm_std_usleep (unsigned long usecs)
{
  struct timeval tv;
  tv.tv_usec = usecs % 1000000;
  tv.tv_sec = usecs / 1000000;
  scm_std_select (0, NULL, NULL, NULL, &tv);
  return tv.tv_sec * 1000000 + tv.tv_usec;
}

unsigned int
scm_std_sleep (unsigned int secs)
{
  struct timeval tv;
  tv.tv_usec = 0;
  tv.tv_sec = secs;
  scm_std_select (0, NULL, NULL, NULL, &tv);
  return tv.tv_sec;
}

/*** Misc */

SCM_DEFINE (scm_current_thread, "current-thread", 0, 0, 0,
	    (void),
	    "Return the thread that called this function.")
#define FUNC_NAME s_scm_current_thread
{
  return SCM_I_CURRENT_THREAD->handle;
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
  scm_i_thread *t;
  SCM list = scm_c_make_list (n, SCM_UNSPECIFIED), *l;

  scm_i_pthread_mutex_lock (&thread_admin_mutex);
  l = &list;
  for (t = all_threads; t && n > 0; t = t->next_thread)
    {
      if (t != scm_i_signal_delivery_thread)
	{
	  SCM_SETCAR (*l, t->handle);
	  l = SCM_CDRLOC (*l);
	}
      n--;
    }
  *l = SCM_EOL;
  scm_i_pthread_mutex_unlock (&thread_admin_mutex);
  return list;
}
#undef FUNC_NAME

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
  scm_i_thread *t;
  SCM_VALIDATE_THREAD (1, thread);
  t = SCM_I_THREAD_DATA (thread);
  return t->exited;
}
#undef FUNC_NAME

static scm_i_pthread_cond_t wake_up_cond;
static int threads_initialized_p = 0;


/* This mutex is used by SCM_CRITICAL_SECTION_START/END.
 */
scm_i_pthread_mutex_t scm_i_critical_section_mutex;

static SCM dynwind_critical_section_mutex;

void
scm_dynwind_critical_section (SCM mutex)
{
  if (scm_is_false (mutex))
    mutex = dynwind_critical_section_mutex;
  scm_dynwind_lock_mutex (mutex);
  scm_dynwind_block_asyncs ();
}

/*** Initialization */

scm_i_pthread_mutex_t scm_i_misc_mutex;

#if SCM_USE_PTHREAD_THREADS
pthread_mutexattr_t scm_i_pthread_mutexattr_recursive[1];
#endif

void
scm_threads_prehistory (void *base)
{
#if SCM_USE_PTHREAD_THREADS
  pthread_mutexattr_init (scm_i_pthread_mutexattr_recursive);
  pthread_mutexattr_settype (scm_i_pthread_mutexattr_recursive,
			     PTHREAD_MUTEX_RECURSIVE);
#endif

  scm_i_pthread_mutex_init (&scm_i_critical_section_mutex,
			    scm_i_pthread_mutexattr_recursive);
  scm_i_pthread_mutex_init (&scm_i_misc_mutex, NULL);
  scm_i_pthread_cond_init (&wake_up_cond, NULL);

  guilify_self_1 ((struct GC_stack_base *) base);
}

scm_t_bits scm_tc16_thread;
scm_t_bits scm_tc16_mutex;
scm_t_bits scm_tc16_condvar;

void
scm_init_threads ()
{
  scm_tc16_thread = scm_make_smob_type ("thread", sizeof (scm_i_thread));
  scm_set_smob_print (scm_tc16_thread, thread_print);

  scm_tc16_mutex = scm_make_smob_type ("mutex", sizeof (fat_mutex));
  scm_set_smob_print (scm_tc16_mutex, fat_mutex_print);
  scm_set_smob_free (scm_tc16_mutex, fat_mutex_free);

  scm_tc16_condvar = scm_make_smob_type ("condition-variable",
					 sizeof (fat_cond));
  scm_set_smob_print (scm_tc16_condvar, fat_cond_print);

  scm_i_default_dynamic_state = SCM_BOOL_F;
  guilify_self_2 (SCM_BOOL_F);
  threads_initialized_p = 1;

  dynwind_critical_section_mutex = scm_make_recursive_mutex ();
}

void
scm_init_threads_default_dynamic_state ()
{
  SCM state = scm_make_dynamic_state (scm_current_dynamic_state ());
  scm_i_default_dynamic_state = state;
}

void
scm_init_thread_procs ()
{
#include "libguile/threads.x"
}


/* IA64-specific things.  */

#ifdef __ia64__
# ifdef __hpux
#  include <sys/param.h>
#  include <sys/pstat.h>
void *
scm_ia64_register_backing_store_base (void)
{
  struct pst_vm_status vm_status;
  int i = 0;
  while (pstat_getprocvm (&vm_status, sizeof (vm_status), 0, i++) == 1)
    if (vm_status.pst_type == PS_RSESTACK)
      return (void *) vm_status.pst_vaddr;
  abort ();
}
void *
scm_ia64_ar_bsp (const void *ctx)
{
  uint64_t bsp;
  __uc_get_ar_bsp (ctx, &bsp);
  return (void *) bsp;
}
# endif /* hpux */
# ifdef linux
#  include <ucontext.h>
void *
scm_ia64_register_backing_store_base (void)
{
  extern void *__libc_ia64_register_backing_store_base;
  return __libc_ia64_register_backing_store_base;
}
void *
scm_ia64_ar_bsp (const void *opaque)
{
  const ucontext_t *ctx = opaque;
  return (void *) ctx->uc_mcontext.sc_ar_bsp;
}
# endif /* linux */
#endif /* __ia64__ */


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

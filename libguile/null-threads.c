/* Copyright (C) 2002 Free Software Foundation, Inc.
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




#include "libguile/validate.h"
#include "libguile/root.h"
#include "libguile/stackchk.h"
#include "libguile/async.h"
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <stdio.h>

void *scm_null_threads_data;

static SCM main_thread;

typedef struct {
  int level;
} scm_null_mutex;

typedef struct {
  int signalled;
} scm_null_cond;

void
scm_threads_init (SCM_STACKITEM *i)
{
  scm_tc16_thread = scm_make_smob_type ("thread", 0);
  scm_tc16_mutex = scm_make_smob_type ("mutex", sizeof (scm_null_mutex));
  scm_tc16_condvar = scm_make_smob_type ("condition-variable",
					 sizeof (scm_null_cond));

  main_thread = scm_permanent_object (scm_cell (scm_tc16_thread, 0));
  scm_null_threads_data = NULL;
}

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
  /* Mark objects on the C stack. */
  SCM_FLUSH_REGISTER_WINDOWS;
  /* This assumes that all registers are saved into the jmp_buf */
  setjmp (scm_save_regs_gc_mark);
  scm_mark_locations ((SCM_STACKITEM *) scm_save_regs_gc_mark,
		      (   (size_t) (sizeof (SCM_STACKITEM) - 1 +
				       sizeof scm_save_regs_gc_mark)
			  / sizeof (SCM_STACKITEM)));

  {
    unsigned long stack_len = scm_stack_size (scm_stack_base);
#ifdef SCM_STACK_GROWS_UP
    scm_mark_locations (scm_stack_base, stack_len);
#else
    scm_mark_locations (scm_stack_base - stack_len, stack_len);
#endif
  }
  SCM_MARK_BACKING_STORE();
}

SCM
scm_call_with_new_thread (SCM argl)
#define FUNC_NAME s_call_with_new_thread
{
  SCM_MISC_ERROR ("threads are not supported in this version of Guile",
		  SCM_EOL);
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  scm_misc_error ("scm_spawn_thread", 
		  "threads are not supported in this version of Guile",
		  SCM_EOL);
  return SCM_BOOL_F;
}

SCM
scm_current_thread (void)
{
  return main_thread;
}

SCM
scm_all_threads (void)
{
  return scm_list_1 (main_thread);
}

scm_root_state *
scm_i_thread_root (SCM thread)
{
  return (scm_root_state *)scm_null_threads_data;
}

SCM
scm_join_thread (SCM thread)
#define FUNC_NAME s_join_thread
{
  SCM_MISC_ERROR ("threads are not supported in this version of Guile",
		  SCM_EOL);
  return SCM_BOOL_F;
}
#undef FUNC_NAME

int
scm_c_thread_exited_p (SCM thread)
#define FUNC_NAME s_scm_thread_exited_p
{
  return 0;
}
#undef FUNC_NAME

SCM
scm_yield (void)
{
  return SCM_BOOL_T;
}

SCM
scm_make_mutex (void)
{
  SCM m = scm_make_smob (scm_tc16_mutex);
  scm_null_mutex *mx = SCM_MUTEX_DATA(m);
  mx->level = 0;
  return m;
}

SCM
scm_lock_mutex (SCM m)
{
  scm_null_mutex *mx;
  SCM_ASSERT (SCM_MUTEXP (m), m, SCM_ARG1, s_lock_mutex);
  mx = SCM_MUTEX_DATA(m);
  mx->level++;
  return SCM_BOOL_T;
}

SCM
scm_try_mutex (SCM m)
{
  return scm_lock_mutex (m); /* will always succeed right away. */
}

SCM
scm_unlock_mutex (SCM m)
{
  scm_null_mutex *mx;
  SCM_ASSERT (SCM_MUTEXP (m), m, SCM_ARG1, s_unlock_mutex);
  mx = SCM_MUTEX_DATA(m);
  if (mx->level == 0)
    scm_misc_error (s_unlock_mutex, "mutex is not locked", SCM_EOL);
  mx->level--;
  return SCM_BOOL_T;
}

SCM
scm_make_condition_variable (void)
{
  scm_null_cond *cv;
  SCM c = scm_make_smob (scm_tc16_condvar);
  cv = SCM_CONDVAR_DATA (c);
  cv->signalled = 0;
  return c;
}

/* Subtract the `struct timeval' values X and Y,
   storing the result in RESULT.  Might modify Y.
   Return 1 if the difference is negative or zero, otherwise 0.  */

static int
timeval_subtract (result, x, y)
     struct timeval *result, *x, *y;
{
  /* Perform the carry for the later subtraction by updating Y. */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }
  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (x->tv_usec - y->tv_usec) / 1000000;
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }
  
  /* Compute the time remaining to wait.
     `tv_usec' is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;
  
  /* Return 1 if result is negative or zero. */
  return x->tv_sec < y->tv_sec
    || (result->tv_sec == 0 && result->tv_usec == 0);
}

SCM
scm_timed_wait_condition_variable (SCM c, SCM m, SCM t)
#define FUNC_NAME s_wait_condition_variable
{
  scm_null_cond *cv;
  struct timeval waittime;

  SCM_ASSERT (SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_wait_condition_variable);
  SCM_ASSERT (SCM_MUTEXP (m),
	      m,
	      SCM_ARG2,
	      s_wait_condition_variable);
  if (!SCM_UNBNDP (t))
    {
      if (SCM_CONSP (t))
	{
	  SCM_VALIDATE_UINT_COPY (3, SCM_CAR(t), waittime.tv_sec);
	  SCM_VALIDATE_UINT_COPY (3, SCM_CDR(t), waittime.tv_usec);
	}
      else
	{
	  SCM_VALIDATE_UINT_COPY (3, t, waittime.tv_sec);
	  waittime.tv_usec = 0;
	}
    }

  cv = SCM_CONDVAR_DATA (c);

  scm_unlock_mutex (m);
  while (!cv->signalled)
    {
      if (SCM_UNBNDP (t))
	select (0, NULL, NULL, NULL, NULL);
      else
	{
	  struct timeval now, then, diff;
	  then = waittime;
	  gettimeofday (&now, NULL);
	  if (timeval_subtract (&diff, &then, &now))
	    break;
	  select (0, NULL, NULL, NULL, &diff);
	}
      SCM_ASYNC_TICK;
    }
  scm_lock_mutex (m);
  if (cv->signalled)
    {
      cv->signalled = 0;
      return SCM_BOOL_T;
    }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM
scm_signal_condition_variable (SCM c)
{
  scm_null_cond *cv;
  SCM_ASSERT (SCM_CONDVARP (c),
	      c,
	      SCM_ARG1,
	      s_signal_condition_variable);
  cv = SCM_CONDVAR_DATA (c);
  cv->signalled = 1;
  return SCM_BOOL_T;
}

SCM
scm_broadcast_condition_variable (SCM c)
{
  return scm_signal_condition_variable (c); /* only one thread anyway. */
}

unsigned long 
scm_thread_usleep (unsigned long usec)
{
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = usec;
  select (0, NULL, NULL, NULL, &timeout);
  return 0;  /* Maybe we should calculate actual time slept,
		but this is faster... :) */
}

unsigned long
scm_thread_sleep (unsigned long sec)
{
  time_t now = time (NULL);
  struct timeval timeout;
  unsigned long slept;
  timeout.tv_sec = sec;
  timeout.tv_usec = 0;
  select (0, NULL, NULL, NULL, &timeout);
  slept = time (NULL) - now;
  return slept > sec ? 0 : sec - slept;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

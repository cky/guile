/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2002 Free Software Foundation, Inc.
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




#include <signal.h>
#include <errno.h>

#include "libguile/_scm.h"

#include "libguile/async.h"
#include "libguile/eval.h"
#include "libguile/root.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/scmsigs.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

/* The thread system has its own sleep and usleep functions.  */
#ifndef USE_THREADS

#if defined(MISSING_SLEEP_DECL)
int sleep ();
#endif

#if defined(HAVE_USLEEP) && defined(MISSING_USLEEP_DECL)
int usleep ();
#endif

#endif

#ifdef __MINGW32__
#include <windows.h>
#define alarm(sec) (0)
/* This weird comma expression is because Sleep is void under Windows. */
#define sleep(sec) (Sleep ((sec) * 1000), 0)
#define usleep(usec) (Sleep ((usec) / 1000), 0)
#define kill(pid, sig) raise (sig)
#endif



/* SIGRETTYPE is the type that signal handlers return.  See <signal.h> */

#ifdef RETSIGTYPE
# define SIGRETTYPE RETSIGTYPE
#else
# ifdef STDC_HEADERS
#  define SIGRETTYPE void
# else
#  define SIGRETTYPE int
# endif
#endif



/* take_signal is installed as the C signal handler whenever a Scheme
   handler is set.  when a signal arrives, take_signal will queue the
   Scheme handler procedure for its thread.  */


/* Scheme vectors with information about a signal.  signal_handlers
   contains the handler procedure or #f, signal_handler_cells contains
   preallocated cells for queuing the handler in take_signal since we
   can't allocate during signal delivery, signal_handler_threads
   points to the thread that a signal should be delivered to.
*/
static SCM *signal_handlers;
static SCM signal_handler_cells;
static SCM signal_handler_threads;

/* saves the original C handlers, when a new handler is installed.
   set to SIG_ERR if the original handler is installed.  */
#ifdef HAVE_SIGACTION
static struct sigaction orig_handlers[NSIG];
#else
static SIGRETTYPE (*orig_handlers[NSIG])(int);
#endif

static SIGRETTYPE
take_signal (int signum)
{
  if (signum >= 0 && signum < NSIG)
    {
#ifdef USE_THREADS
      SCM thread = SCM_VECTOR_REF (signal_handler_threads, signum);
      scm_i_queue_async_cell (SCM_VECTOR_REF(signal_handler_cells, signum),
			      scm_i_thread_root (thread));
#else
      scm_i_queue_async_cell (SCM_VECTOR_REF(signal_handler_cells, signum),
			      scm_root);
#endif
    }
#ifndef HAVE_SIGACTION
  signal (signum, take_signal);
#endif
}

SCM
scm_sigaction (SCM signum, SCM handler, SCM flags)
{
  return scm_sigaction_for_thread (signum, handler, flags, SCM_UNDEFINED);
}

static SCM
close_1 (SCM proc, SCM arg)
{
  return scm_primitive_eval_x (scm_list_3 (scm_sym_lambda, SCM_EOL,
					   scm_list_2 (proc, arg)));
}

/* user interface for installation of signal handlers.  */
SCM_DEFINE (scm_sigaction_for_thread, "sigaction", 1, 3, 0,
           (SCM signum, SCM handler, SCM flags, SCM thread),
	    "Install or report the signal handler for a specified signal.\n\n"
	    "@var{signum} is the signal number, which can be specified using the value\n"
	    "of variables such as @code{SIGINT}.\n\n"
	    "If @var{handler} is omitted, @code{sigaction} returns a pair: the\n"
	    "CAR is the current\n"
	    "signal hander, which will be either an integer with the value @code{SIG_DFL}\n"
	    "(default action) or @code{SIG_IGN} (ignore), or the Scheme procedure which\n"
	    "handles the signal, or @code{#f} if a non-Scheme procedure handles the\n"
	    "signal.  The CDR contains the current @code{sigaction} flags for the handler.\n\n"
	    "If @var{handler} is provided, it is installed as the new handler for\n"
	    "@var{signum}.  @var{handler} can be a Scheme procedure taking one\n"
	    "argument, or the value of @code{SIG_DFL} (default action) or\n"
	    "@code{SIG_IGN} (ignore), or @code{#f} to restore whatever signal handler\n"
	    "was installed before @code{sigaction} was first used.  When\n"
	    "a scheme procedure has been specified, that procedure will run\n"
	    "in the given @var{thread}.   When no thread has been given, the\n"
	    "thread that made this call to @code{sigaction} is used.\n"
	    "Flags can "
	    "optionally be specified for the new handler (@code{SA_RESTART} will\n"
	    "always be added if it's available and the system is using restartable\n"
	    "system calls.)  The return value is a pair with information about the\n"
	    "old handler as described above.\n\n"
	    "This interface does not provide access to the \"signal blocking\"\n"
	    "facility.  Maybe this is not needed, since the thread support may\n"
	    "provide solutions to the problem of consistent access to data\n"
	    "structures.")
#define FUNC_NAME s_scm_sigaction_for_thread
{
  int csig;
#ifdef HAVE_SIGACTION
  struct sigaction action;
  struct sigaction old_action;
#else
  SIGRETTYPE (* chandler) (int) = SIG_DFL;
  SIGRETTYPE (* old_chandler) (int);
#endif
  int query_only = 0;
  int save_handler = 0;
      
  SCM old_handler;

  SCM_VALIDATE_INUM_COPY (1, signum, csig);
  if (csig < 0 || csig > NSIG)
    SCM_OUT_OF_RANGE (1, signum);
#if defined(HAVE_SIGACTION)
#if defined(SA_RESTART) && defined(HAVE_RESTARTABLE_SYSCALLS)
  /* don't allow SA_RESTART to be omitted if HAVE_RESTARTABLE_SYSCALLS
     is defined, since libguile would be likely to produce spurious
     EINTR errors.  */
  action.sa_flags = SA_RESTART;
#else
  action.sa_flags = 0;
#endif
  if (!SCM_UNBNDP (flags))
    {
      SCM_VALIDATE_INUM (3, flags);
      action.sa_flags |= SCM_INUM (flags);
    }
  sigemptyset (&action.sa_mask);
#endif

#ifdef USE_THREADS
  if (SCM_UNBNDP (thread))
    thread = scm_current_thread ();
  else
    SCM_VALIDATE_THREAD (4, thread);
#else
  thread = SCM_BOOL_F;
#endif

  SCM_DEFER_INTS;
  old_handler = SCM_VECTOR_REF(*signal_handlers, csig);
  if (SCM_UNBNDP (handler))
    query_only = 1;
  else if (SCM_EQ_P (scm_integer_p (handler), SCM_BOOL_T))
    {
      if (SCM_NUM2LONG (2, handler) == (long) SIG_DFL
	  || SCM_NUM2LONG (2, handler) == (long) SIG_IGN)
	{
#ifdef HAVE_SIGACTION
	  action.sa_handler = (SIGRETTYPE (*) (int)) SCM_INUM (handler);
#else
	  chandler = (SIGRETTYPE (*) (int)) SCM_INUM (handler);
#endif
	  SCM_VECTOR_SET (*signal_handlers, csig, SCM_BOOL_F);
	}
      else
	SCM_OUT_OF_RANGE (2, handler);
    }
  else if (SCM_FALSEP (handler))
    {
      /* restore the default handler.  */
#ifdef HAVE_SIGACTION
      if (orig_handlers[csig].sa_handler == SIG_ERR)
	query_only = 1;
      else
	{
	  action = orig_handlers[csig];
	  orig_handlers[csig].sa_handler = SIG_ERR;
	  SCM_VECTOR_SET (*signal_handlers, csig, SCM_BOOL_F);

	}
#else
      if (orig_handlers[csig] == SIG_ERR)
	query_only = 1;
      else
	{
	  chandler = orig_handlers[csig];
	  orig_handlers[csig] = SIG_ERR;
	  SCM_VECTOR_SET (*signal_handlers, csig, SCM_BOOL_F);
	}
#endif
    }
  else
    {
      SCM_VALIDATE_NIM (2, handler);
#ifdef HAVE_SIGACTION
      action.sa_handler = take_signal;
      if (orig_handlers[csig].sa_handler == SIG_ERR)
	save_handler = 1;
#else
      chandler = take_signal;
      if (orig_handlers[csig] == SIG_ERR)
	save_handler = 1;
#endif
      SCM_VECTOR_SET (*signal_handlers, csig, handler);
      SCM_VECTOR_SET (signal_handler_cells, csig,
		      scm_cons (close_1 (handler, signum), SCM_BOOL_F));
      SCM_VECTOR_SET (signal_handler_threads, csig, thread);
    }

  /* XXX - Silently ignore setting handlers for `program error signals'
     because they can't currently be handled by Scheme code.
  */

  switch (csig)
    {
      /* This list of program error signals is from the GNU Libc
         Reference Manual */
    case SIGFPE:
    case SIGILL:
    case SIGSEGV:
#ifdef SIGBUS
    case SIGBUS:
#endif
    case SIGABRT:
#if defined(SIGIOT) && (SIGIOT != SIGABRT)
    case SIGIOT:
#endif
#ifdef SIGTRAP
    case SIGTRAP:
#endif
#ifdef SIGEMT
    case SIGEMT:
#endif
#ifdef SIGSYS
    case SIGSYS:
#endif
      query_only = 1;
    }

#ifdef HAVE_SIGACTION
  if (query_only)
    {
      if (sigaction (csig, 0, &old_action) == -1)
	SCM_SYSERROR;
    }
  else
    {
      if (sigaction (csig, &action , &old_action) == -1)
	SCM_SYSERROR;
      if (save_handler)
	orig_handlers[csig] = old_action;
    }
  if (old_action.sa_handler == SIG_DFL || old_action.sa_handler == SIG_IGN)
    old_handler = scm_long2num ((long) old_action.sa_handler);
  SCM_ALLOW_INTS;
  return scm_cons (old_handler, SCM_MAKINUM (old_action.sa_flags));
#else
  if (query_only)
    {
      if ((old_chandler = signal (csig, SIG_IGN)) == SIG_ERR)
	SCM_SYSERROR;
      if (signal (csig, old_chandler) == SIG_ERR)
	SCM_SYSERROR;
    }
  else
    {
      if ((old_chandler = signal (csig, chandler)) == SIG_ERR)
	SCM_SYSERROR;
      if (save_handler)
	orig_handlers[csig] = old_chandler;
    }
  if (old_chandler == SIG_DFL || old_chandler == SIG_IGN)
    old_handler = scm_long2num ((long) old_chandler);
  SCM_ALLOW_INTS;
  return scm_cons (old_handler, SCM_MAKINUM (0));
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_restore_signals, "restore-signals", 0, 0, 0,
            (void),
	    "Return all signal handlers to the values they had before any call to\n"
	    "@code{sigaction} was made.  The return value is unspecified.")
#define FUNC_NAME s_scm_restore_signals
{
  int i;
  for (i = 0; i < NSIG; i++)
    {
#ifdef HAVE_SIGACTION
      if (orig_handlers[i].sa_handler != SIG_ERR)
	{
	  if (sigaction (i, &orig_handlers[i], NULL) == -1)
	    SCM_SYSERROR;
	  orig_handlers[i].sa_handler = SIG_ERR;
	  SCM_VECTOR_SET (*signal_handlers, i, SCM_BOOL_F);
	}
#else
      if (orig_handlers[i] != SIG_ERR)
	{
	  if (signal (i, orig_handlers[i]) == SIG_ERR)
	    SCM_SYSERROR;
	  orig_handlers[i] = SIG_ERR;
	  SCM_VECTOR_SET (*signal_handlers, i, SCM_BOOL_F);	  
	}
#endif
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_alarm, "alarm", 1, 0, 0,
           (SCM i),
	    "Set a timer to raise a @code{SIGALRM} signal after the specified\n"
	    "number of seconds (an integer).  It's advisable to install a signal\n"
	    "handler for\n"
	    "@code{SIGALRM} beforehand, since the default action is to terminate\n"
	    "the process.\n\n"
	    "The return value indicates the time remaining for the previous alarm,\n"
	    "if any.  The new value replaces the previous alarm.  If there was\n"
	    "no previous alarm, the return value is zero.")
#define FUNC_NAME s_scm_alarm
{
  unsigned int j;
  SCM_VALIDATE_INUM (1, i);
  j = alarm (SCM_INUM (i));
  return SCM_MAKINUM (j);
}
#undef FUNC_NAME

#ifdef HAVE_SETITIMER
SCM_DEFINE (scm_setitimer, "setitimer", 5, 0, 0,
           (SCM which_timer,
            SCM interval_seconds, SCM interval_microseconds,
            SCM value_seconds, SCM value_microseconds),
            "Set the timer specified by @var{which_timer} according to the given\n"
            "@var{interval_seconds}, @var{interval_microseconds},\n"
            "@var{value_seconds}, and @var{value_microseconds} values.\n"
            "\n"
            "Return information about the timer's previous setting."
            "\n"
            "Errors are handled as described in the guile info pages under ``POSIX\n"
            "Interface Conventions''.\n"
            "\n"
            "The timers available are: @code{ITIMER_REAL}, @code{ITIMER_VIRTUAL},\n"
            "and @code{ITIMER_PROF}.\n"
            "\n"
            "The return value will be a list of two cons pairs representing the\n"
            "current state of the given timer.  The first pair is the seconds and\n"
            "microseconds of the timer @code{it_interval}, and the second pair is\n"
            "the seconds and microseconds of the timer @code{it_value}.")
#define FUNC_NAME s_scm_setitimer
{
  int rv;
  int c_which_timer;
  struct itimerval new_timer;
  struct itimerval old_timer;

  c_which_timer = SCM_NUM2INT(1, which_timer);
  new_timer.it_interval.tv_sec = SCM_NUM2LONG(2, interval_seconds);
  new_timer.it_interval.tv_usec = SCM_NUM2LONG(3, interval_microseconds);
  new_timer.it_value.tv_sec = SCM_NUM2LONG(4, value_seconds);
  new_timer.it_value.tv_usec = SCM_NUM2LONG(5, value_microseconds);

  SCM_SYSCALL(rv = setitimer(c_which_timer, &new_timer, &old_timer));
  
  if(rv != 0)
    SCM_SYSERROR;

  return scm_list_2(scm_cons(scm_long2num(old_timer.it_interval.tv_sec),
                             scm_long2num(old_timer.it_interval.tv_usec)),
                    scm_cons(scm_long2num(old_timer.it_value.tv_sec),
                             scm_long2num(old_timer.it_value.tv_usec)));
}
#undef FUNC_NAME
#endif /* HAVE_SETITIMER */

#ifdef HAVE_GETITIMER
SCM_DEFINE (scm_getitimer, "getitimer", 1, 0, 0,
  (SCM which_timer),
            "Return information about the timer specified by @var{which_timer}"
            "\n"
            "Errors are handled as described in the guile info pages under ``POSIX\n"
            "Interface Conventions''.\n"
            "\n"
            "The timers available are: @code{ITIMER_REAL}, @code{ITIMER_VIRTUAL},\n"
            "and @code{ITIMER_PROF}.\n"
            "\n"
            "The return value will be a list of two cons pairs representing the\n"
            "current state of the given timer.  The first pair is the seconds and\n"
            "microseconds of the timer @code{it_interval}, and the second pair is\n"
            "the seconds and microseconds of the timer @code{it_value}.")
#define FUNC_NAME s_scm_getitimer
{
  int rv;
  int c_which_timer;
  struct itimerval old_timer;

  c_which_timer = SCM_NUM2INT(1, which_timer);

  SCM_SYSCALL(rv = getitimer(c_which_timer, &old_timer));
  
  if(rv != 0)
    SCM_SYSERROR;
  
  return scm_list_2(scm_cons(scm_long2num(old_timer.it_interval.tv_sec),
                             scm_long2num(old_timer.it_interval.tv_usec)),
                    scm_cons(scm_long2num(old_timer.it_value.tv_sec),
                             scm_long2num(old_timer.it_value.tv_usec)));
}
#undef FUNC_NAME
#endif /* HAVE_GETITIMER */

#ifdef HAVE_PAUSE
SCM_DEFINE (scm_pause, "pause", 0, 0, 0,
           (),
	    "Pause the current process (thread?) until a signal arrives whose\n"
	    "action is to either terminate the current process or invoke a\n"
	    "handler procedure.  The return value is unspecified.")
#define FUNC_NAME s_scm_pause
{
  pause ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

SCM_DEFINE (scm_sleep, "sleep", 1, 0, 0,
           (SCM i),
	    "Wait for the given number of seconds (an integer) or until a signal\n"
	    "arrives.  The return value is zero if the time elapses or the number\n"
	    "of seconds remaining otherwise.")
#define FUNC_NAME s_scm_sleep
{
  unsigned long j;
  SCM_VALIDATE_INUM_MIN (1, i,0);
#ifdef USE_THREADS
  j = scm_thread_sleep (SCM_INUM(i));
#else
  j = sleep (SCM_INUM(i));
#endif
  return scm_ulong2num (j);
}
#undef FUNC_NAME

#if defined(USE_THREADS) || defined(HAVE_USLEEP) || defined(__MINGW32__)
SCM_DEFINE (scm_usleep, "usleep", 1, 0, 0,
           (SCM i),
	    "Sleep for I microseconds.  @code{usleep} is not available on\n"
	    "all platforms.")
#define FUNC_NAME s_scm_usleep
{
  SCM_VALIDATE_INUM_MIN (1, i,0);

#ifdef USE_THREADS
  /* If we have threads, we use the thread system's sleep function.  */
  {
    unsigned long j = scm_thread_usleep (SCM_INUM (i));
    return scm_ulong2num (j);
  }
#else
#ifdef USLEEP_RETURNS_VOID
  usleep (SCM_INUM (i));
  return SCM_INUM0;
#else
  {
    int j = usleep (SCM_INUM (i));
    return SCM_MAKINUM (j);
  }
#endif
#endif
}
#undef FUNC_NAME
#endif /* USE_THREADS || HAVE_USLEEP || __MINGW32__ */

SCM_DEFINE (scm_raise, "raise", 1, 0, 0,
           (SCM sig),
	    "Sends a specified signal @var{sig} to the current process, where\n"
	    "@var{sig} is as described for the kill procedure.")
#define FUNC_NAME s_scm_raise
{
  SCM_VALIDATE_INUM (1, sig);
  SCM_DEFER_INTS;
  if (kill (getpid (), (int) SCM_INUM (sig)) != 0)
    SCM_SYSERROR;
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_init_scmsigs ()
{
  int i;

  signal_handlers =
    SCM_VARIABLE_LOC (scm_c_define ("signal-handlers",
				  scm_c_make_vector (NSIG, SCM_BOOL_F)));
  signal_handler_cells =
    scm_permanent_object (scm_c_make_vector (NSIG, SCM_BOOL_F));
  signal_handler_threads =
    scm_permanent_object (scm_c_make_vector (NSIG, SCM_BOOL_F));

  for (i = 0; i < NSIG; i++)
    {
#ifdef HAVE_SIGACTION
      orig_handlers[i].sa_handler = SIG_ERR;

#else
      orig_handlers[i] = SIG_ERR;
#endif

#ifdef HAVE_RESTARTABLE_SYSCALLS
      /* If HAVE_RESTARTABLE_SYSCALLS is defined, it's important that
	 signals really are restartable.  don't rely on the same
	 run-time that configure got: reset the default for every signal.
      */
#ifdef HAVE_SIGINTERRUPT
      siginterrupt (i, 0);
#elif defined(SA_RESTART)
      {
	struct sigaction action;

	sigaction (i, NULL, &action);
	if (!(action.sa_flags & SA_RESTART))
	  {
	    action.sa_flags |= SA_RESTART;
	    sigaction (i, &action, NULL);
	  }
      }
#endif
      /* if neither siginterrupt nor SA_RESTART are available we may
	 as well assume that signals are always restartable.  */
#endif
    }

  scm_c_define ("NSIG", scm_long2num (NSIG));
  scm_c_define ("SIG_IGN", scm_long2num ((long) SIG_IGN));
  scm_c_define ("SIG_DFL", scm_long2num ((long) SIG_DFL));
#ifdef SA_NOCLDSTOP
  scm_c_define ("SA_NOCLDSTOP", scm_long2num (SA_NOCLDSTOP));
#endif
#ifdef SA_RESTART
  scm_c_define ("SA_RESTART", scm_long2num (SA_RESTART));
#endif

#if defined(HAVE_SETITIMER) || defined(HAVE_GETITIMER)
  /* Stuff needed by setitimer and getitimer. */
  scm_c_define ("ITIMER_REAL", SCM_MAKINUM (ITIMER_REAL));
  scm_c_define ("ITIMER_VIRTUAL", SCM_MAKINUM (ITIMER_VIRTUAL));
  scm_c_define ("ITIMER_PROF", SCM_MAKINUM (ITIMER_PROF));
#endif /* defined(HAVE_SETITIMER) || defined(HAVE_GETITIMER) */

#include "libguile/scmsigs.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

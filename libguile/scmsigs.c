/* Copyright (C) 1995, 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
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


#include <stdio.h>
#include <signal.h>
#include "_scm.h"

#include "async.h"
#include "eval.h"
#include "scmsigs.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
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



#ifdef USE_MIT_PTHREADS
#undef signal
#define signal pthread_signal
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
   handler is set.  when a signal arrives, take_signal marks the corresponding
   element of got_signal and marks signal_async.  the thunk in signal_async
   (sys_deliver_signals) will be run at the next opportunity, outside a
   critical section. sys_deliver_signals runs each Scheme handler for
   which got_signal is set.  */

static SCM signal_async;

static char got_signal[NSIG];

/* a Scheme vector of handler procedures.  */
static SCM *signal_handlers;

/* saves the original C handlers, when a new handler is installed.
   set to SIG_ERR if the original handler is installed.  */
#ifdef HAVE_SIGACTION
static struct sigaction orig_handlers[NSIG];
#else
static SIGRETTYPE (*orig_handlers)(int)[NSIG];
#endif

static SIGRETTYPE
take_signal (int signum)
{
  int saved_errno = errno;
  SCM ignored;
  if (!scm_ints_disabled)
    {
      /* For reasons of speed, the SCM_NEWCELL macro doesn't defer
	 interrupts.  Instead, it first sets its argument to point to
	 the first cell in the list, and then advances the freelist
	 pointer to the next cell.  Now, if this procedure is
	 interrupted, the only anomalous state possible is to have
	 both SCM_NEWCELL's argument and scm_freelist pointing to the
	 same cell.  To deal with this case, we always throw away the
	 first cell in scm_freelist here.

	 At least, that's the theory.  I'm not convinced that that's
	 the only anomalous path we need to worry about.  */
      SCM_NEWCELL (ignored);
    }
  got_signal[signum] = 1;
#if HAVE_SIGACTION
  /* unblock the signal before the scheme handler gets to run, since
     it may use longjmp to escape (i.e., throw an exception).  */
  {
    sigset_t set;
    sigemptyset (&set);
    sigaddset (&set, signum);
    sigprocmask (SIG_UNBLOCK, &set, NULL);
  }
#endif
  scm_system_async_mark (signal_async);
  errno = saved_errno;
}

static SCM
sys_deliver_signals (void)
{
  int i;

  for (i = 0; i < NSIG; i++)
    {
      if (got_signal[i])
	{
	  /* The flag is reset before calling the handler in case the
	     handler doesn't return.  If the handler doesn't return
	     but leaves other signals flagged, they their handlers
	     will be applied some time later when the async is checked
	     again.  It would probably be better to reset the flags
	     after doing a longjmp.  */
	  got_signal[i] = 0;
#ifndef HAVE_SIGACTION
	  signal (i, take_signal);
#endif
	  scm_apply (SCM_VELTS (*signal_handlers)[i], 
		     scm_listify (SCM_MAKINUM (i), SCM_UNDEFINED),
		     SCM_EOL);
	}
    }
  return SCM_UNSPECIFIED;
}

/* user interface for installation of signal handlers.  */
SCM_PROC(s_sigaction, "sigaction", 1, 2, 0, scm_sigaction);
SCM
scm_sigaction (SCM signum, SCM handler, SCM flags)
{
  int csig;
#ifdef HAVE_SIGACTION
  struct sigaction action;
  struct sigaction old_action;
#else
  SIGRETTYPE (* chandler) (int);
  SIGRETTYPE (* old_chandler) (int);
#endif
  int query_only = 0;
  int save_handler = 0;
  SCM *scheme_handlers = SCM_VELTS (*signal_handlers);
  SCM old_handler;

  SCM_ASSERT (SCM_INUMP (signum), signum, SCM_ARG1, s_sigaction);
  csig = SCM_INUM (signum);
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
      SCM_ASSERT (SCM_INUMP (flags), flags, SCM_ARG3, s_sigaction);
      action.sa_flags |= SCM_INUM (flags);
    }
  sigemptyset (&action.sa_mask);
#endif
  SCM_DEFER_INTS;
  old_handler = scheme_handlers[csig];
  if (SCM_UNBNDP (handler))
    query_only = 1;
  else if (scm_integer_p (handler) == SCM_BOOL_T)
    {
      if (scm_num2long (handler, (char *) SCM_ARG2, s_sigaction)
	  == (long) SIG_DFL
	  || scm_num2long (handler, (char *) SCM_ARG2, s_sigaction) 
	  == (long) SIG_IGN)
	{
#ifdef HAVE_SIGACTION
	  action.sa_handler = (SIGRETTYPE (*) (int)) SCM_INUM (handler);
#else
	  chandler = (SIGRETTYPE (*) (int)) SCM_INUM (handler);
#endif
	  scheme_handlers[csig] = SCM_BOOL_F;
	}
      else
	scm_out_of_range (s_sigaction, handler);
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
	  scheme_handlers[csig] = SCM_BOOL_F;
	}
#else
      if (orig_handlers[csig] == SIG_ERR)
	query_only = 1;
      else
	{
	  chandler = orig_handlers[csig];
	  orig_handlers[csig] = SIG_ERR;
	  scheme_handlers[csig] = SCM_BOOL_F;
	}
#endif
    } 
  else
    {
      SCM_ASSERT (SCM_NIMP (handler), handler, SCM_ARG2, s_sigaction);
#ifdef HAVE_SIGACTION
      action.sa_handler = take_signal;
      if (orig_handlers[csig].sa_handler == SIG_ERR)
	save_handler = 1;
#else
      chandler = take_signal;
      if (orig_handlers[csig] == SIG_ERR)
	save_handler = 1;
#endif
      scheme_handlers[csig] = handler;
    }
#ifdef HAVE_SIGACTION
  if (query_only)
    {
      if (sigaction (csig, 0, &old_action) == -1)
	scm_syserror (s_sigaction);
    }
  else
    {
      if (sigaction (csig, &action , &old_action) == -1)
	scm_syserror (s_sigaction);
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
	scm_syserror (s_sigaction);
      if (signal (csig, old_chandler) == SIG_ERR)
	scm_syserror (s_sigaction);
    }
  else
    {
      if ((old_chandler = signal (csig, chandler)) == SIG_ERR)
	scm_syserror (s_sigaction);
      if (save_handler)
	orig_handlers[csig] = old_chandler;
    }
  if (old_chandler == SIG_DFL || old_chandler == SIG_IGN)
    old_handler = scm_long2num (old_chandler);
  SCM_ALLOW_INTS;
  return scm_cons (old_handler, SCM_MAKINUM (0));
#endif
}

SCM_PROC (s_restore_signals, "restore-signals", 0, 0, 0, scm_restore_signals);
SCM
scm_restore_signals (void)
{
  int i;
  SCM *scheme_handlers = SCM_VELTS (*signal_handlers);  

  for (i = 0; i < NSIG; i++)
    {
#ifdef HAVE_SIGACTION
      if (orig_handlers[i].sa_handler != SIG_ERR)
	{
	  if (sigaction (i, &orig_handlers[i], NULL) == -1)
	    scm_syserror (s_restore_signals);
	  orig_handlers[i].sa_handler = SIG_ERR;
	  scheme_handlers[i] = SCM_BOOL_F;
	}
#else
      if (orig_handlers[i] != SIG_ERR)
	{
	  if (signal (i, orig_handlers[i]) == SIG_ERR)
	    scm_syserror (s_restore_signals);
	  orig_handlers[i] = SIG_ERR;
	  scheme_handlers[i] = SCM_BOOL_F;
	}
#endif
    }
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_alarm, "alarm", 1, 0, 0, scm_alarm);

SCM 
scm_alarm (i)
     SCM i;
{
  unsigned int j;
  SCM_ASSERT (SCM_INUMP (i) && (SCM_INUM (i) >= 0), i, SCM_ARG1, s_alarm);
  j = alarm (SCM_INUM (i));
  return SCM_MAKINUM (j);
}

#ifdef HAVE_PAUSE
SCM_PROC(s_pause, "pause", 0, 0, 0, scm_pause);

SCM 
scm_pause ()
{
  pause ();
  return SCM_UNSPECIFIED;
}
#endif

SCM_PROC(s_sleep, "sleep", 1, 0, 0, scm_sleep);

SCM 
scm_sleep (i)
     SCM i;
{
  unsigned long j;
  SCM_ASSERT (SCM_INUMP (i) && (SCM_INUM (i) >= 0), i, SCM_ARG1, s_sleep);
#ifdef USE_THREADS
  j = scm_thread_sleep (SCM_INUM(i));
#else
  j = sleep (SCM_INUM(i));
#endif
  return scm_ulong2num (j);
}

#if defined(USE_THREADS) || defined(HAVE_USLEEP)
SCM_PROC(s_usleep, "usleep", 1, 0, 0, scm_usleep);

SCM 
scm_usleep (i)
     SCM i;
{
  SCM_ASSERT (SCM_INUMP (i) && (SCM_INUM (i) >= 0), i, SCM_ARG1, s_usleep);

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
#endif /* GUILE_ISELECT || HAVE_USLEEP */

SCM_PROC(s_raise, "raise", 1, 0, 0, scm_raise);

SCM
scm_raise(sig)
     SCM sig;
{
  SCM_ASSERT(SCM_INUMP(sig), sig, SCM_ARG1, s_raise);
  SCM_DEFER_INTS;
  if (kill (getpid (), (int) SCM_INUM (sig)) != 0)
    scm_syserror (s_raise);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}



void
scm_init_scmsigs ()
{
  SCM thunk;
  int i;

  signal_handlers =
    SCM_CDRLOC (scm_sysintern ("signal-handlers",
			       scm_make_vector (SCM_MAKINUM (NSIG),
						SCM_BOOL_F)));
  thunk = scm_make_gsubr ("%deliver-signals", 0, 0, 0,
			  sys_deliver_signals);
  signal_async = scm_system_async (thunk);

  for (i = 0; i < NSIG; i++)
    {
      got_signal[i] = 0;
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
	    action.sa_flags &= SA_RESTART;
	    sigaction (i, &action, NULL);
	  }
      }
#endif
      /* if neither siginterrupt nor SA_RESTART are available we may
	 as well assume that signals are always restartable.  */
#endif
    }

  scm_sysintern ("NSIG", scm_long2num (NSIG));
  scm_sysintern ("SIG_IGN", scm_long2num ((long) SIG_IGN));
  scm_sysintern ("SIG_DFL", scm_long2num ((long) SIG_DFL));
#ifdef SA_NOCLDSTOP
  scm_sysintern ("SA_NOCLDSTOP", scm_long2num (SA_NOCLDSTOP));
#endif
#ifdef SA_RESTART
  scm_sysintern ("SA_RESTART", scm_long2num (SA_RESTART));
#endif

#include "scmsigs.x"
}


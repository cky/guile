/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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

#include "scmsigs.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif




#if (__TURBOC__==1)
#define signal ssignal		/* Needed for TURBOC V1.0 */
#endif

#ifdef USE_MIT_PTHREADS
#undef signal
#define signal pthread_signal
#endif



/* SIGRETTYPE is the type that signal handlers return.  See <signal.h>*/

#ifdef RETSIGTYPE
#define SIGRETTYPE RETSIGTYPE
#else
#ifdef STDC_HEADERS
#if (__TURBOC__==1)
#define SIGRETTYPE int
#else
#define SIGRETTYPE void
#endif
#else
#ifdef linux
#define SIGRETTYPE void
#else
#define SIGRETTYPE int
#endif
#endif
#endif

#ifdef vms
#ifdef __GNUC__
#define SIGRETTYPE int
#endif
#endif



#define SIGFN(NAME, SCM_NAME, SIGNAL) \
static SIGRETTYPE \
NAME (sig) \
     int sig; \
{ \
  signal (SIGNAL, NAME); \
  scm_take_signal (SCM_NAME); \
}

#ifdef SIGHUP
SIGFN(scm_hup_signal, SCM_HUP_SIGNAL, SIGHUP)
#endif

#ifdef SIGINT
SIGFN(scm_int_signal, SCM_INT_SIGNAL, SIGINT)
#endif

#ifdef SIGFPE
SIGFN(scm_fpe_signal, SCM_FPE_SIGNAL, SIGFPE)
#endif

#ifdef SIGBUS
SIGFN(scm_bus_signal, SCM_BUS_SIGNAL, SIGBUS)
#endif

#ifdef SIGSEGV
SIGFN(scm_segv_signal, SCM_SEGV_SIGNAL, SIGSEGV)
#endif

#ifdef SIGALRM
SIGFN(scm_alrm_signal, SCM_ALRM_SIGNAL, SIGALRM)
#endif

#define FAKESIGFN(NAME, SCM_NAME) \
static SIGRETTYPE \
NAME (sig) \
     int sig; \
{ \
  scm_take_signal (SCM_NAME); \
}

#if 0
/* !!! */
FAKESIGFN(scm_gc_signal, SCM_GC_SIGNAL)
FAKESIGFN(scm_tick_signal, SCM_TICK_SIGNAL)
#endif


SCM_PROC(s_alarm, "alarm", 1, 0, 0, scm_alarm);

SCM 
scm_alarm (i)
     SCM i;
{
  unsigned int j;
  SCM_ASSERT (SCM_INUMP (i) && (SCM_INUM (i) >= 0), i, SCM_ARG1, s_alarm);
  SCM_SYSCALL (j = alarm (SCM_INUM (i)));
  return SCM_MAKINUM (j);
}


SCM_PROC(s_pause, "pause", 0, 0, 0, scm_pause);

SCM 
scm_pause ()
{
  pause ();
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_sleep, "sleep", 1, 0, 0, scm_sleep);

SCM 
scm_sleep (i)
     SCM i;
{
  unsigned int j;
  SCM_ASSERT (SCM_INUMP (i) && (SCM_INUM (i) >= 0), i, SCM_ARG1, s_sleep);
#ifdef __HIGHC__
  SCM_SYSCALL(j = 0; sleep(SCM_INUM(i)););
#else
  SCM_SYSCALL(j = sleep(SCM_INUM(i)););
#endif
  return SCM_MAKINUM (j);
}

SCM_PROC(s_raise, "raise", 1, 0, 0, scm_raise);

SCM
scm_raise(sig)
     SCM sig;
{
  SCM_ASSERT(SCM_INUMP(sig), sig, SCM_ARG1, s_raise);
# ifdef vms
  return SCM_MAKINUM(gsignal((int)SCM_INUM(sig)));
# else
  return kill (getpid(), (int)SCM_INUM(sig)) ? SCM_BOOL_F : SCM_BOOL_T;
# endif
}


#ifdef SIGHUP
static SIGRETTYPE (*oldhup) ();
#endif

#ifdef SIGINT
static SIGRETTYPE (*oldint) ();
#endif

#ifdef SIGFPE
static SIGRETTYPE (*oldfpe) ();
#endif

#ifdef SIGBUS
static SIGRETTYPE (*oldbus) ();
#endif

#ifdef SIGSEGV			/* AMIGA lacks! */
static SIGRETTYPE (*oldsegv) ();
#endif

#ifdef SIGALRM
static SIGRETTYPE (*oldalrm) ();
#endif

#ifdef SIGPIPE
static SIGRETTYPE (*oldpipe) ();
#endif



void 
scm_init_signals ()
{
#ifdef SIGINT
  oldint = signal (SIGINT, scm_int_signal);
#endif
#ifdef SIGHUP
  oldhup = signal (SIGHUP, scm_hup_signal);
#endif
#ifdef SIGFPE
  oldfpe = signal (SIGFPE, scm_fpe_signal);
#endif
#ifdef SIGBUS
  oldbus = signal (SIGBUS, scm_bus_signal);
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
  oldsegv = signal (SIGSEGV, scm_segv_signal);
#endif
#ifdef SIGALRM
  alarm (0);			/* kill any pending ALRM interrupts */
  oldalrm = signal (SIGALRM, scm_alrm_signal);
#endif
#ifdef SIGPIPE
  oldpipe = signal (SIGPIPE, SIG_IGN);
#endif
#ifdef ultrix
  siginterrupt (SIGINT, 1);
  siginterrupt (SIGALRM, 1);
  siginterrupt (SIGHUP, 1);
  siginterrupt (SIGPIPE, 1);
#endif /* ultrix */
}

/* This is used in preparation for a possible fork().  Ignore all
   signals before the fork so that child will catch only if it
   establishes a handler */

void 
scm_ignore_signals ()
{
#ifdef ultrix
  siginterrupt (SIGINT, 0);
  siginterrupt (SIGALRM, 0);
  siginterrupt (SIGHUP, 0);
  siginterrupt (SIGPIPE, 0);
#endif /* ultrix */
  signal (SIGINT, SIG_IGN);
#ifdef SIGHUP
  signal (SIGHUP, SIG_DFL);
#endif
#ifdef SCM_FLOATS
  signal (SIGFPE, SIG_DFL);
#endif
#ifdef SIGBUS
  signal (SIGBUS, SIG_DFL);
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
  signal (SIGSEGV, SIG_DFL);
#endif
  /* Some documentation claims that ALRMs are cleared accross forks.
     If this is not always true then the value returned by alarm(0)
     will have to be saved and scm_unignore_signals() will have to
     reinstate it. */
  /* This code should be neccessary only if the forked process calls
     alarm() without establishing a handler:
     #ifdef SIGALRM
     oldalrm = signal(SIGALRM, SIG_DFL);
     #endif */
  /* These flushes are per warning in man page on fork(). */
  fflush (stdout);
  fflush (stderr);
}


void 
scm_unignore_signals ()
{
  signal (SIGINT, scm_int_signal);
#ifdef SIGHUP
  signal (SIGHUP, scm_hup_signal);
#endif
#ifdef SCM_FLOATS
  signal (SIGFPE, scm_fpe_signal);
#endif
#ifdef SIGBUS
  signal (SIGBUS, scm_bus_signal);
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
  signal (SIGSEGV, scm_segv_signal);
#endif
#ifdef SIGALRM
  signal (SIGALRM, scm_alrm_signal);
#endif
#ifdef ultrix
  siginterrupt (SIGINT, 1);
  siginterrupt (SIGALRM, 1);
  siginterrupt (SIGHUP, 1);
  siginterrupt (SIGPIPE, 1);
#endif /* ultrix */
}

SCM_PROC (s_restore_signals, "restore-signals", 0, 0, 0, scm_restore_signals);

SCM
scm_restore_signals ()
{
#ifdef ultrix
  siginterrupt (SIGINT, 0);
  siginterrupt (SIGALRM, 0);
  siginterrupt (SIGHUP, 0);
  siginterrupt (SIGPIPE, 0);
#endif /* ultrix */
  signal (SIGINT, oldint);
#ifdef SIGHUP
  signal (SIGHUP, oldhup);
#endif
#ifdef SCM_FLOATS
  signal (SIGFPE, oldfpe);
#endif
#ifdef SIGBUS
  signal (SIGBUS, oldbus);
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
  signal (SIGSEGV, oldsegv);
#endif
#ifdef SIGPIPE
  signal (SIGPIPE, oldpipe);
#endif
#ifdef SIGALRM
  alarm (0);			/* kill any pending ALRM interrupts */
  signal (SIGALRM, oldalrm);
#endif
  return SCM_UNSPECIFIED;
}



void
scm_init_scmsigs ()
{
#include "scmsigs.x"
}


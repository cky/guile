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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include <stdio.h>
#include "_scm.h"
#include "genio.h"
#include "smob.h"
#include "alist.h"
#include "eval.h"
#include "dynwind.h"
#ifdef DEBUG_EXTENSIONS
#include "debug.h"
#endif
#include "continuations.h"
#include "stackchk.h"

#include "throw.h"


/* {Catch and Throw} 
 */
static int scm_tc16_jmpbuffer;

#define SCM_JMPBUFP(O) (SCM_TYP16(O) == scm_tc16_jmpbuffer)
#define JBACTIVE(O) (SCM_CAR (O) & (1L << 16L))
#define ACTIVATEJB(O)  (SCM_SETOR_CAR (O, (1L << 16L)))
#define DEACTIVATEJB(O)  (SCM_SETAND_CAR (O, ~(1L << 16L)))

#ifndef DEBUG_EXTENSIONS
#define JBJMPBUF(O) ((jmp_buf*)SCM_CDR (O) )
#define SETJBJMPBUF SCM_SETCDR
#else
#define SCM_JBDFRAME(O) ((scm_debug_frame*)SCM_CAR (SCM_CDR (O)) )
#define JBJMPBUF(O) ((jmp_buf*)SCM_CDR (SCM_CDR (O)) )
#define SCM_SETJBDFRAME(O,X) SCM_SETCAR (SCM_CDR (O), (SCM)(X))
#define SETJBJMPBUF(O,X) SCM_SETCDR(SCM_CDR (O), X)

static scm_sizet freejb SCM_P ((SCM jbsmob));

static scm_sizet
freejb (jbsmob)
     SCM jbsmob;
{
  scm_must_free ((char *) SCM_CDR (jbsmob));
  return sizeof (scm_cell);
}
#endif

static int printjb SCM_P ((SCM exp, SCM port, scm_print_state *pstate));
static int
printjb (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  scm_gen_puts (scm_regular_string, "#<jmpbuffer ", port);
  scm_gen_puts (scm_regular_string, JBACTIVE(exp) ? "(active) " : "(inactive) ", port);
  scm_intprint((SCM) JBJMPBUF(exp), 16, port);
  scm_gen_putc ('>', port);
  return 1 ;
}

static scm_smobfuns jbsmob = {
  scm_mark0,
#ifdef DEBUG_EXTENSIONS
  freejb,
#else
  scm_free0,
#endif
  printjb,
  0
};

static SCM make_jmpbuf SCM_P ((void));
static SCM
make_jmpbuf ()
{
  SCM answer;
  SCM_NEWCELL (answer);
  SCM_REDEFER_INTS;
  {
#ifdef DEBUG_EXTENSIONS
    char *mem = scm_must_malloc (sizeof (scm_cell), "jb");
    SCM_SETCDR (answer, (SCM) mem);
#endif
    SCM_SETCAR (answer, scm_tc16_jmpbuffer);
    SETJBJMPBUF(answer, (jmp_buf *)0);
    DEACTIVATEJB(answer);
  }
  SCM_REALLOW_INTS;
  return answer;
}

struct jmp_buf_and_retval	/* use only on the stack, in scm_catch */
{
  jmp_buf buf;			/* must be first */
  SCM throw_tag;
  SCM retval;
};

SCM
scm_catch_apply (tag, proc, a1, args, handler)
     SCM tag;
     SCM proc;
     SCM a1;
     SCM args;
     SCM handler;
{
  struct jmp_buf_and_retval jbr;
  SCM jmpbuf;
  SCM answer;

  jmpbuf = make_jmpbuf ();
  answer = SCM_EOL;
  scm_dynwinds = scm_acons (tag, jmpbuf, scm_dynwinds);
  SETJBJMPBUF(jmpbuf, &jbr.buf);
#ifdef DEBUG_EXTENSIONS
  SCM_SETJBDFRAME(jmpbuf, scm_last_debug_frame);
#endif
  if (setjmp (jbr.buf))
    {
      SCM throw_tag;
      SCM throw_args;

#ifdef STACK_CHECKING
      scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
#endif
      SCM_REDEFER_INTS;
      DEACTIVATEJB (jmpbuf);
      scm_dynwinds = SCM_CDR (scm_dynwinds);
      SCM_REALLOW_INTS;
      throw_args = jbr.retval;
      throw_tag = jbr.throw_tag;
      jbr.throw_tag = SCM_EOL;
      jbr.retval = SCM_EOL;
      answer = scm_apply (handler, scm_cons (throw_tag, throw_args), SCM_EOL);
    }
  else
    {
      ACTIVATEJB (jmpbuf);
      if (tag == SCM_BOOL_F)
	answer = scm_apply (proc,
			    SCM_NULLP (a1)
			    ? scm_cons (jmpbuf, SCM_EOL)
			    : scm_cons2 (jmpbuf, a1, args),
			    SCM_EOL);
      else
	answer = scm_apply (proc, a1, args);
      SCM_REDEFER_INTS;
      DEACTIVATEJB (jmpbuf);
      scm_dynwinds = SCM_CDR (scm_dynwinds);
      SCM_REALLOW_INTS;
    }
  return answer;
}

SCM_PROC(s_catch, "catch", 3, 0, 0, scm_catch);
SCM
scm_catch (tag, thunk, handler)
     SCM tag;
     SCM thunk;
     SCM handler;
{
  SCM_ASSERT ((tag == SCM_BOOL_F)
	      || (SCM_NIMP(tag) && SCM_SYMBOLP(tag))
	      || (tag == SCM_BOOL_T),
	      tag, SCM_ARG1, s_catch);
  return scm_catch_apply (tag, thunk, SCM_EOL, SCM_EOL, handler);
}

SCM_PROC(s_lazy_catch, "lazy-catch", 3, 0, 0, scm_lazy_catch);
SCM
scm_lazy_catch (tag, thunk, handler)
     SCM tag;
     SCM thunk;
     SCM handler;
{
  SCM answer;
  SCM_ASSERT ((SCM_NIMP(tag) && SCM_SYMBOLP(tag))
	      || (tag == SCM_BOOL_T),
	      tag, SCM_ARG1, s_lazy_catch);
  SCM_REDEFER_INTS;
  scm_dynwinds = scm_acons (tag, handler, scm_dynwinds);
  SCM_REALLOW_INTS;
  answer = scm_apply (thunk, SCM_EOL, SCM_EOL);
  SCM_REDEFER_INTS;
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  SCM_REALLOW_INTS;
  return answer;
}

/* The user has thrown to an uncaught key --- print a message and die. 
   1) If the user wants something different, they can use (catch #t
   ...) to do what they like.
   2) Outside the context of a read-eval-print loop, there isn't
   anything else good to do; libguile should not assume the existence
   of a read-eval-print loop.
   3) Given that we shouldn't do anything complex, it's much more
   robust to do it in C code.  */
static SCM uncaught_throw SCM_P ((SCM key, SCM args));
static SCM
uncaught_throw (key, args)
     SCM key;
     SCM args;
{
  SCM p = scm_def_errp; 
  scm_gen_puts (scm_regular_string, "guile: uncaught throw to ", p);
  scm_prin1 (key, p, 0);
  scm_gen_puts (scm_regular_string, ": ", p);
  scm_prin1 (args, p, 1);
  scm_gen_putc ('\n', p);
  
  exit (2);
}


static char s_throw[];
SCM
scm_ithrow (key, args, noreturn)
     SCM key;
     SCM args;
     int noreturn;
{
  SCM jmpbuf;
  SCM wind_goal;

  if (SCM_NIMP (key) && SCM_JMPBUFP (key))
    {
      jmpbuf = key;
      if (noreturn)
	{
	  SCM_ASSERT (JBACTIVE (jmpbuf), jmpbuf,
		  "throw to dynamically inactive catch",
		  s_throw);
	}
      else if (!JBACTIVE (jmpbuf))
	return SCM_UNSPECIFIED;
    }
  else
    {
      SCM dynpair;
      SCM winds;

      if (noreturn)
	{
	  SCM_ASSERT (SCM_NIMP (key) && SCM_SYMBOLP (key), key, SCM_ARG1,
		      s_throw);
	}
      else if (!(SCM_NIMP (key) && SCM_SYMBOLP (key)))
	return SCM_UNSPECIFIED;

      /* Search the wind list for an appropriate catch.
	 "Waiter, please bring us the wind list." */
      for (winds = scm_dynwinds; SCM_NIMP (winds); winds = SCM_CDR (winds))
	{
	  if (! SCM_CONSP (winds))
	    abort ();

	  dynpair = SCM_CAR (winds);
	  if (SCM_NIMP (dynpair) && SCM_CONSP (dynpair))
	    {
	      SCM this_key = SCM_CAR (dynpair);

	      if (this_key == SCM_BOOL_T || this_key == key)
		break;
	    }
	}

      /* If we didn't find anything, print a message and exit Guile.  */
      if (winds == SCM_EOL)
	uncaught_throw (key, args);

      if (SCM_IMP (winds) || SCM_NCONSP (winds))
	abort ();
      
      if (dynpair != SCM_BOOL_F)
	jmpbuf = SCM_CDR (dynpair);
      else
	{
	  if (!noreturn)
	    return SCM_UNSPECIFIED;
	  else
	    {
	      scm_exitval = scm_cons (key, args);
	      scm_dowinds (SCM_EOL, scm_ilength (scm_dynwinds));
#ifdef DEBUG_EXTENSIONS
	      scm_last_debug_frame = SCM_DFRAME (scm_rootcont);
#endif
	      longjmp (SCM_JMPBUF (scm_rootcont), 1);
	    }
	}
    }
  for (wind_goal = scm_dynwinds;
       SCM_CDAR (wind_goal) != jmpbuf;
       wind_goal = SCM_CDR (wind_goal))
    ;
  if (!SCM_JMPBUFP (jmpbuf))
    {
      SCM oldwinds = scm_dynwinds;
      SCM handle, answer;
      scm_dowinds (wind_goal, scm_ilength (scm_dynwinds) - scm_ilength (wind_goal));
      SCM_REDEFER_INTS;
      handle = scm_dynwinds;
      scm_dynwinds = SCM_CDR (scm_dynwinds);
      SCM_REALLOW_INTS;
      answer = scm_apply (jmpbuf, scm_cons (key, args), SCM_EOL);
      SCM_REDEFER_INTS;
      SCM_SETCDR (handle, scm_dynwinds);
      scm_dynwinds = handle;
      SCM_REALLOW_INTS;
      scm_dowinds (oldwinds, scm_ilength (scm_dynwinds) - scm_ilength (oldwinds));
      return answer;
    }
  else
    {
      struct jmp_buf_and_retval * jbr;
      scm_dowinds (wind_goal, scm_ilength (scm_dynwinds) - scm_ilength (wind_goal));
      jbr = (struct jmp_buf_and_retval *)JBJMPBUF (jmpbuf);
      jbr->throw_tag = key;
      jbr->retval = args;
    }
#ifdef DEBUG_EXTENSIONS
  scm_last_debug_frame = SCM_JBDFRAME (jmpbuf);
#endif
  longjmp (*JBJMPBUF (jmpbuf), 1);
}


SCM_PROC(s_throw, "throw", 1, 0, 1, scm_throw);
SCM
scm_throw (key, args)
     SCM key;
     SCM args;
{
  /* May return if handled by lazy catch. */
  return scm_ithrow (key, args, 1);
}


void
scm_init_throw ()
{
  scm_tc16_jmpbuffer = scm_newsmob (&jbsmob);
#include "throw.x"
}

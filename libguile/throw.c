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




/* {Catch and Throw} 
 */
static int scm_tc16_jmpbuffer;

SCM scm_bad_throw_vcell;

#define SCM_JMPBUFP(O) (SCM_TYP16(O) == scm_tc16_jmpbuffer)
#define JBACTIVE(O) (SCM_CAR (O) & (1L << 16L))
#define ACTIVATEJB(O)  (SCM_CAR (O) |= (1L << 16L))
#define DEACTIVATEJB(O)  (SCM_CAR (O) &= ~(1L << 16L))

#ifdef DEBUG_EXTENSIONS
#define JBSCM_DFRAME(O) ((debug_frame*)SCM_CAR (SCM_CDR (O)) )
#define JBJMPBUF(O) ((jmp_buf*)SCM_CDR (SCM_CDR (O)) )
#define SETJBSCM_DFRAME(O,X) SCM_CAR(SCM_CDR (O)) = (SCM)(X)
#define SETJBJMPBUF(O,X) SCM_SETCDR(SCM_CDR (O), X)
#else
#define JBJMPBUF(O) ((jmp_buf*)SCM_CDR (O) )
#define SETJBJMPBUF SCM_SETCDR
#endif

#ifdef __STDC__
static int
printjb (SCM exp, SCM port, int writing)
#else
static int
printjb (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
{
  scm_gen_puts (scm_regular_string, "#<jmpbuffer ", port);
  scm_gen_puts (scm_regular_string, JBACTIVE(exp) ? "(active) " : "(inactive) ", port);
  scm_intprint((SCM) JBJMPBUF(exp), 16, port);
  scm_gen_putc ('>', port);
  return 1 ;
}

/* !!! The mark function needs to be different for
 * debugging support.		A. Green
 */
static scm_smobfuns jbsmob = {scm_mark0, scm_free0, printjb, 0};

#ifdef __STDC__
static SCM
make_jmpbuf (void)
#else
static SCM
make_jmpbuf ()
#endif
{
  SCM answer;
  SCM_NEWCELL (answer);
#ifdef DEBUG_EXTENSIONS
  SCM_NEWCELL (SCM_CDR (answer));
#endif
  SCM_DEFER_INTS;
  {
    SCM_CAR(answer) = scm_tc16_jmpbuffer;
    SETJBJMPBUF(answer, (jmp_buf *)0);
    DEACTIVATEJB(answer);
  }
  SCM_ALLOW_INTS;
  return answer;
}


struct jmp_buf_and_retval	/* use only on the stack, in scm_catch */
{
  jmp_buf buf;			/* must be first */
  SCM throw_tag;
  SCM retval;
};

SCM_PROC(s_catch, "catch", 3, 0, 0, scm_catch);
#ifdef __STDC__
SCM
scm_catch (SCM tag, SCM thunk, SCM handler)
#else
SCM
scm_catch (tag, thunk, handler)
     SCM tag;
     SCM thunk;
     SCM handler;
#endif
{
  struct jmp_buf_and_retval jbr;
  SCM jmpbuf;
  SCM answer;

  SCM_ASSERT ((tag == SCM_BOOL_F) || (SCM_NIMP(tag) && SCM_SYMBOLP(tag)) || (tag == SCM_BOOL_T),
	  tag, SCM_ARG1, s_catch);
  jmpbuf = make_jmpbuf ();
  answer = SCM_EOL;
  scm_dynwinds = scm_acons (tag, jmpbuf, scm_dynwinds);
  SETJBJMPBUF(jmpbuf, &jbr.buf);
#ifdef DEBUG_EXTENSIONS
  SETJBSCM_DFRAME(jmpbuf, last_debug_info_frame);
#endif
  if (setjmp (jbr.buf))
    {
      SCM throw_tag;
      SCM throw_args;

      SCM_DEFER_INTS;
      DEACTIVATEJB (jmpbuf);
      scm_dynwinds = SCM_CDR (scm_dynwinds);
      SCM_ALLOW_INTS;
      throw_args = jbr.retval;
      throw_tag = jbr.throw_tag;
      jbr.throw_tag = SCM_EOL;
      jbr.retval = SCM_EOL;
      answer = scm_apply (handler, scm_cons (throw_tag, throw_args), SCM_EOL);
    }
  else
    {
      ACTIVATEJB (jmpbuf);
      answer = scm_apply (thunk,
			  ((tag == SCM_BOOL_F) ? scm_cons (jmpbuf, SCM_EOL) : SCM_EOL),
			  SCM_EOL);
      SCM_DEFER_INTS;
      DEACTIVATEJB (jmpbuf);
      scm_dynwinds = SCM_CDR (scm_dynwinds);
      SCM_ALLOW_INTS;
    }
  return answer;
}


static char s_throw[];
#ifdef __STDC__
SCM
scm_ithrow (SCM key, SCM args, int noreturn)
#else
SCM
scm_ithrow (key, args, noreturn)
     SCM key;
     SCM args;
     int noreturn;
#endif
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
      SCM hook;

      if (noreturn)
	{
	  SCM_ASSERT (SCM_NIMP (key) && SCM_SYMBOLP (key), key, SCM_ARG1, s_throw);
	}
      else if (!(SCM_NIMP (key) && SCM_SYMBOLP (key)))
	return SCM_UNSPECIFIED;

      dynpair = scm_sloppy_assq (key, scm_dynwinds);

      if (dynpair == SCM_BOOL_F)
	dynpair = scm_sloppy_assq (SCM_BOOL_T, scm_dynwinds);

      hook = SCM_CDR (scm_bad_throw_vcell);
      if ((dynpair == SCM_BOOL_F)
	  && (SCM_BOOL_T == scm_procedure_p (hook)))
	{
	  SCM answer;
	  answer = scm_apply (hook, scm_cons (key, args), SCM_EOL);
	}
      
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
	      longjmp (SCM_JMPBUF (scm_rootcont), 1);
	    }
	}
    }
#ifdef DEBUG_EXTENSIONS
  last_debug_info_frame = JBSCM_DFRAME (jmpbuf);
#endif
  for (wind_goal = scm_dynwinds;
       SCM_CDAR (wind_goal) != jmpbuf;
       wind_goal = SCM_CDR (wind_goal))
    ;
  {
    struct jmp_buf_and_retval * jbr;
    jbr = (struct jmp_buf_and_retval *)JBJMPBUF (jmpbuf);
    jbr->throw_tag = key;
    jbr->retval = args;
  }
  scm_dowinds (wind_goal, scm_ilength (scm_dynwinds) - scm_ilength (wind_goal));
  longjmp (*JBJMPBUF (jmpbuf), 1);
}


SCM_PROC(s_throw, "throw", 1, 0, 1, scm_throw);
#ifdef __STDC__
SCM
scm_throw (SCM key, SCM args)
#else
SCM
scm_throw (key, args)
     SCM key;
     SCM args;
#endif
{
  scm_ithrow (key, args, 1);
  return SCM_BOOL_F;  /* never really returns */
}


#ifdef __STDC__
void
scm_init_throw (void)
#else
void
scm_init_throw ()
#endif
{
  scm_tc16_jmpbuffer = scm_newsmob (&jbsmob);
  scm_bad_throw_vcell = scm_sysintern ("%%bad-throw", SCM_BOOL_F);
#include "throw.x"
}


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
#include "eq.h"
#include "dynwind.h"
#include "backtrace.h"
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


/* scm_internal_catch is the guts of catch.  It handles all the
   mechanics of setting up a catch target, invoking the catch body,
   and perhaps invoking the handler if the body does a throw.

   The function is designed to be usable from C code, but is general
   enough to implement all the semantics Guile Scheme expects from
   throw.

   TAG is the catch tag.  Typically, this is a symbol, but this
   function doesn't actually care about that.

   BODY is a pointer to a C function which runs the body of the catch;
   this is the code you can throw from.  We call it like this:
      BODY (BODY_DATA, JMPBUF)
   where:
      BODY_DATA is just the BODY_DATA argument we received; we pass it
	 through to BODY as its first argument.  The caller can make
	 BODY_DATA point to anything useful that BODY might need.
      JMPBUF is the Scheme jmpbuf object corresponding to this catch,
         which we have just created and initialized.

   HANDLER is a pointer to a C function to deal with a throw to TAG,
   should one occur.  We call it like this:
      HANDLER (HANDLER_DATA, TAG, THROW_ARGS)
   where
      HANDLER_DATA is the HANDLER_DATA argument we recevied; it's the
         same idea as BODY_DATA above.
      TAG is the tag that the user threw to; usually this is TAG, but
         it could be something else if TAG was #t (i.e., a catch-all),
         or the user threw to a jmpbuf.
      THROW_ARGS is the list of arguments the user passed to the THROW
         function.

   BODY_DATA is just a pointer we pass through to BODY.  HANDLER_DATA
   is just a pointer we pass through to HANDLER.  We don't actually
   use either of those pointers otherwise ourselves.  The idea is
   that, if our caller wants to communicate something to BODY or
   HANDLER, it can pass a pointer to it as MUMBLE_DATA, which BODY and
   HANDLER can then use.  Think of it as a way to make BODY and
   HANDLER closures, not just functions; MUMBLE_DATA points to the
   enclosed variables.

   Of course, it's up to the caller to make sure that any data a
   MUMBLE_DATA needs is protected from GC.  A common way to do this is
   to make MUMBLE_DATA a pointer to data stored in an automatic
   structure variable; since the collector must scan the stack for
   references anyway, this assures that any references in MUMBLE_DATA
   will be found.  */

SCM
scm_internal_catch (tag, body, body_data, handler, handler_data)
     SCM tag;
     scm_catch_body_t body;
     void *body_data;
     scm_catch_handler_t handler;
     void *handler_data;
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
      answer = handler (handler_data, throw_tag, throw_args);
    }
  else
    {
      ACTIVATEJB (jmpbuf);
      answer = body (body_data, jmpbuf);
      SCM_REDEFER_INTS;
      DEACTIVATEJB (jmpbuf);
      scm_dynwinds = SCM_CDR (scm_dynwinds);
      SCM_REALLOW_INTS;
    }
  return answer;
}


/* This is a body function you can pass to scm_internal_catch if you
   want the body to be like Scheme's `catch' --- a thunk, or a
   function of one argument if the tag is #f.

   DATA contains the Scheme procedure to invoke.  If the tag being
   caught is #f, then we pass JMPBUF to the body procedure; otherwise,
   it gets no arguments.  */

SCM
scm_body_thunk (body_data, jmpbuf)
     void *body_data;
     SCM jmpbuf;
{
  struct scm_body_thunk_data *c = (struct scm_body_thunk_data *) body_data;

  if (c->tag == SCM_BOOL_F)
    return scm_apply (c->body_proc, scm_cons (jmpbuf, SCM_EOL), SCM_EOL);
  else
    return scm_apply (c->body_proc, SCM_EOL, SCM_EOL);
}


/* If the user does a throw to this catch, this function runs a
   handler procedure written in Scheme.  HANDLER_DATA is a pointer to
   an SCM variable holding the Scheme procedure object to invoke.  It
   ought to be a pointer to an automatic, or the procedure object
   should be otherwise protected from GC.  */
SCM
scm_handle_by_proc (handler_data, tag, throw_args)
     void *handler_data;
     SCM tag;
     SCM throw_args;
{
  SCM *handler_proc_p = (SCM *) handler_data;

  return scm_apply (*handler_proc_p, scm_cons (tag, throw_args), SCM_EOL);
}


SCM_PROC(s_catch, "catch", 3, 0, 0, scm_catch);
SCM
scm_catch (tag, thunk, handler)
     SCM tag;
     SCM thunk;
     SCM handler;
{
  struct scm_body_thunk_data c;

  SCM_ASSERT ((tag == SCM_BOOL_F)
	      || (SCM_NIMP(tag) && SCM_SYMBOLP(tag))
	      || (tag == SCM_BOOL_T),
	      tag, SCM_ARG1, s_catch);

  c.tag = tag;
  c.body_proc = thunk;

  /* scm_internal_catch takes care of all the mechanics of setting up
     a catch tag; we tell it to call scm_body_thunk to run the body,
     and scm_handle_by_proc to deal with any throws to this catch.
     The former receives a pointer to c, telling it how to behave.
     The latter receives a pointer to HANDLER, so it knows who to call.  */
  return scm_internal_catch (tag,
			     scm_body_thunk, &c, 
			     scm_handle_by_proc, &handler);
}


/* The smob tag for lazy_catch smobs.  */
static long tc16_lazy_catch;

/* This is the structure we put on the wind list for a lazy catch.  It
   stores the handler function to call, and the data pointer to pass
   through to it.  It's not a Scheme closure, but it is a function
   with data, so the term "closure" is appropriate in its broader
   sense.

   (We don't need anything like this in the "eager" catch code,
   because the same C frame runs both the body and the handler.)  */
struct lazy_catch {
  scm_catch_handler_t handler;
  void *handler_data;
};

/* Strictly speaking, we could just pass a zero for our print
   function, because we don't need to print them.  They should never
   appear in normal data structures, only in the wind list.  However,
   it might be nice for debugging someday... */
static int
print_lazy_catch (SCM closure, SCM port, scm_print_state *pstate)
{
  struct lazy_catch *c = (struct lazy_catch *) SCM_CDR (closure);
  char buf[200];

  sprintf (buf, "#<lazy-catch 0x%lx 0x%lx>",
	   (long) c->handler, (long) c->handler_data);
  scm_gen_puts (scm_regular_string, buf, port);

  return 1;
}

static scm_smobfuns lazy_catch_funs = {
  scm_mark0, scm_free0, print_lazy_catch, 0
};


/* Given a pointer to a lazy catch structure, return a smob for it,
   suitable for inclusion in the wind list.  ("Ah yes, a Château
   Gollombiere '72, no?").  */
static SCM
make_lazy_catch (struct lazy_catch *c)
{
  SCM smob;

  SCM_NEWCELL (smob);
  SCM_SETCDR (smob, c);
  SCM_SETCAR (smob, tc16_lazy_catch);

  return smob;
}

#define SCM_LAZY_CATCH_P(obj) \
  (SCM_NIMP (obj) && (SCM_CAR (obj) == tc16_lazy_catch))


/* Exactly like scm_internal_catch, except:
   - It does not unwind the stack (this is the major difference).
   - If handler returns, its value is returned from the throw.
   - BODY always receives #f as its JMPBUF argument (since there's no
     jmpbuf associated with a lazy catch, because we don't unwind the
     stack.)  */
SCM
scm_internal_lazy_catch (tag, body, body_data, handler, handler_data)
     SCM tag;
     scm_catch_body_t body;
     void *body_data;
     scm_catch_handler_t handler;
     void *handler_data;
{
  SCM lazy_catch, answer;
  struct lazy_catch c;

  c.handler = handler;
  c.handler_data = handler_data;
  lazy_catch = make_lazy_catch (&c);

  SCM_REDEFER_INTS;
  scm_dynwinds = scm_acons (tag, lazy_catch, scm_dynwinds);
  SCM_REALLOW_INTS;

  answer = (*body) (body_data, SCM_BOOL_F);

  SCM_REDEFER_INTS;
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  SCM_REALLOW_INTS;

  return answer;
}


SCM_PROC(s_lazy_catch, "lazy-catch", 3, 0, 0, scm_lazy_catch);
SCM
scm_lazy_catch (tag, thunk, handler)
     SCM tag;
     SCM thunk;
     SCM handler;
{
  struct scm_body_thunk_data c;

  SCM_ASSERT ((SCM_NIMP(tag) && SCM_SYMBOLP(tag))
	      || (tag == SCM_BOOL_T),
	      tag, SCM_ARG1, s_lazy_catch);

  c.tag = tag;
  c.body_proc = thunk;

  /* scm_internal_lazy_catch takes care of all the mechanics of
     setting up a lazy catch tag; we tell it to call scm_body_thunk to
     run the body, and scm_handle_by_proc to deal with any throws to
     this catch.  The former receives a pointer to c, telling it how
     to behave.  The latter receives a pointer to HANDLER, so it knows
     who to call.  */
  return scm_internal_lazy_catch (tag,
				  scm_body_thunk, &c, 
				  scm_handle_by_proc, &handler);
}


/* The user has thrown to an uncaught key --- print a message and die.
   At boot time, we establish a catch-all that uses this as its handler.
   1) If the user wants something different, they can use (catch #t
   ...) to do what they like.
   2) Outside the context of a read-eval-print loop, there isn't
   anything else good to do; libguile should not assume the existence
   of a read-eval-print loop.
   3) Given that we shouldn't do anything complex, it's much more
   robust to do it in C code.

   HANDLER_DATA, if non-zero, is assumed to be a char * pointing to a
   message header to print; if zero, we use "guile" instead.  That
   text is followed by a colon, then the message described by ARGS.  */

SCM
scm_handle_by_message (handler_data, tag, args)
     void *handler_data;
     SCM tag;
     SCM args;
{
  char *prog_name = (char *) handler_data;
  SCM p = scm_def_errp;

  if (SCM_NFALSEP (scm_eq_p (tag, SCM_CAR (scm_intern0 ("quit")))))
    exit (scm_exit_status (args));

  if (! prog_name)
    prog_name = "guile";

  scm_gen_puts (scm_regular_string, prog_name, p);
  scm_gen_puts (scm_regular_string, ": ", p);

  if (scm_ilength (args) >= 3)
    {
      SCM message = SCM_CADR (args);
      SCM parts = SCM_CADDR (args);

      scm_display_error_message (message, parts, p);
    }
  else
    {
      scm_gen_puts (scm_regular_string, "uncaught throw to ", p);
      scm_prin1 (tag, p, 0);
      scm_gen_puts (scm_regular_string, ": ", p);
      scm_prin1 (args, p, 1);
      scm_gen_putc ('\n', p);
    }

  exit (2);
}

/* Derive the an exit status from the arguments to (quit ...).  */
int
scm_exit_status (args)
  SCM args;
{
  if (SCM_NNULLP (args))
    {
      SCM cqa = SCM_CAR (args);
      
      if (SCM_INUMP (cqa))
	return (SCM_INUM (cqa));
      else if (SCM_FALSEP (cqa))
	return 1;
    }
  return 0;
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
      SCM dynpair = SCM_UNDEFINED;
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

      /* If we didn't find anything, abort.  scm_boot_guile should
         have established a catch-all, but obviously things are
         thoroughly screwed up.  */
      if (winds == SCM_EOL)
	abort ();

      /* If the wind list is malformed, bail.  */
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

  /* Is a lazy catch?  In wind list entries for lazy catches, the key
     is bound to a lazy_catch smob, not a jmpbuf.  */
  if (SCM_LAZY_CATCH_P (jmpbuf))
    {
      struct lazy_catch *c = (struct lazy_catch *) SCM_CDR (jmpbuf);
      SCM oldwinds = scm_dynwinds;
      SCM handle, answer;
      scm_dowinds (wind_goal, (scm_ilength (scm_dynwinds)
			       - scm_ilength (wind_goal)));
      SCM_REDEFER_INTS;
      handle = scm_dynwinds;
      scm_dynwinds = SCM_CDR (scm_dynwinds);
      SCM_REALLOW_INTS;
      answer = (c->handler) (c->handler_data, key, args);
      SCM_REDEFER_INTS;
      SCM_SETCDR (handle, scm_dynwinds);
      scm_dynwinds = handle;
      SCM_REALLOW_INTS;
      scm_dowinds (oldwinds, (scm_ilength (scm_dynwinds)
			      - scm_ilength (oldwinds)));
      return answer;
    }

  /* Otherwise, it's a normal catch.  */
  else if (SCM_JMPBUFP (jmpbuf))
    {
      struct jmp_buf_and_retval * jbr;
      scm_dowinds (wind_goal, (scm_ilength (scm_dynwinds)
			       - scm_ilength (wind_goal)));
      jbr = (struct jmp_buf_and_retval *)JBJMPBUF (jmpbuf);
      jbr->throw_tag = key;
      jbr->retval = args;
    }

  /* Otherwise, it's some random piece of junk.  */
  else
    abort ();

#ifdef DEBUG_EXTENSIONS
  scm_last_debug_frame = SCM_JBDFRAME (jmpbuf);
#endif
  longjmp (*JBJMPBUF (jmpbuf), 1);
}


void
scm_init_throw ()
{
  scm_tc16_jmpbuffer = scm_newsmob (&jbsmob);
  tc16_lazy_catch = scm_newsmob (&lazy_catch_funs);
#include "throw.x"
}

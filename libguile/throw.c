/*	Copyright (C) 1995, 1996, 1997, 1998, 2000 Free Software Foundation, Inc.
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



#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/smob.h"
#include "libguile/alist.h"
#include "libguile/eval.h"
#include "libguile/eq.h"
#include "libguile/dynwind.h"
#include "libguile/backtrace.h"
#ifdef DEBUG_EXTENSIONS
#include "libguile/debug.h"
#endif
#include "libguile/continuations.h"
#include "libguile/stackchk.h"
#include "libguile/stacks.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"

#include "libguile/validate.h"
#include "libguile/throw.h"


/* the jump buffer data structure */
static int scm_tc16_jmpbuffer;

#define SCM_JMPBUFP(OBJ) (SCM_NIMP(OBJ) && (SCM_TYP16(OBJ) == scm_tc16_jmpbuffer))

#define JBACTIVE(OBJ) (SCM_CELL_WORD_0 (OBJ) & (1L << 16L))
#define ACTIVATEJB(OBJ)  (SCM_SETOR_CAR (OBJ, (1L << 16L)))
#define DEACTIVATEJB(OBJ)  (SCM_SETAND_CAR (OBJ, ~(1L << 16L)))

#define JBJMPBUF(OBJ)           ((jmp_buf *) SCM_CELL_WORD_1 (OBJ))
#define SETJBJMPBUF(x,v)        (SCM_SET_CELL_WORD_1 ((x), (v)))
#ifdef DEBUG_EXTENSIONS
#define SCM_JBDFRAME(x)         ((scm_debug_frame *) SCM_CELL_WORD_2 (x))
#define SCM_SETJBDFRAME(x,v)    (SCM_SET_CELL_WORD_2 ((x), (v)))
#endif

static int
printjb (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<jmpbuffer ", port);
  scm_puts (JBACTIVE(exp) ? "(active) " : "(inactive) ", port);
  scm_intprint((long) JBJMPBUF (exp), 16, port);

  scm_putc ('>', port);
  return 1 ;
}


static SCM
make_jmpbuf (void)
{
  SCM answer;
  SCM_REDEFER_INTS;
  {
#ifdef DEBUG_EXTENSIONS
    SCM_NEWSMOB2 (answer, scm_tc16_jmpbuffer, 0, 0);
#else
    SCM_NEWSMOB (answer, scm_tc16_jmpbuffer, 0);
#endif
    SETJBJMPBUF(answer, (jmp_buf *)0);
    DEACTIVATEJB(answer);
  }
  SCM_REALLOW_INTS;
  return answer;
}


/* scm_internal_catch (the guts of catch) */

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
      BODY (BODY_DATA)
   where:
      BODY_DATA is just the BODY_DATA argument we received; we pass it
	 through to BODY as its first argument.  The caller can make
	 BODY_DATA point to anything useful that BODY might need.

   HANDLER is a pointer to a C function to deal with a throw to TAG,
   should one occur.  We call it like this:
      HANDLER (HANDLER_DATA, THROWN_TAG, THROW_ARGS)
   where
      HANDLER_DATA is the HANDLER_DATA argument we recevied; it's the
         same idea as BODY_DATA above.
      THROWN_TAG is the tag that the user threw to; usually this is
         TAG, but it could be something else if TAG was #t (i.e., a
         catch-all), or the user threw to a jmpbuf.
      THROW_ARGS is the list of arguments the user passed to the THROW
         function, after the tag.

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
scm_internal_catch (SCM tag, scm_catch_body_t body, void *body_data, scm_catch_handler_t handler, void *handler_data)
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
      answer = body (body_data);
      SCM_REDEFER_INTS;
      DEACTIVATEJB (jmpbuf);
      scm_dynwinds = SCM_CDR (scm_dynwinds);
      SCM_REALLOW_INTS;
    }
  return answer;
}



/* scm_internal_lazy_catch (the guts of lazy catching) */

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
  struct lazy_catch *c = (struct lazy_catch *) SCM_CELL_WORD_1 (closure);
  char buf[200];

  sprintf (buf, "#<lazy-catch 0x%lx 0x%lx>",
	   (long) c->handler, (long) c->handler_data);
  scm_puts (buf, port);

  return 1;
}


/* Given a pointer to a lazy catch structure, return a smob for it,
   suitable for inclusion in the wind list.  ("Ah yes, a Château
   Gollombiere '72, non?").  */
static SCM
make_lazy_catch (struct lazy_catch *c)
{
  SCM_RETURN_NEWSMOB (tc16_lazy_catch, c);
}

#define SCM_LAZY_CATCH_P(obj) (SCM_SMOB_PREDICATE (tc16_lazy_catch, obj))


/* Exactly like scm_internal_catch, except:
   - It does not unwind the stack (this is the major difference).
   - If handler returns, its value is returned from the throw.  */
SCM
scm_internal_lazy_catch (SCM tag, scm_catch_body_t body, void *body_data, scm_catch_handler_t handler, void *handler_data)
{
  SCM lazy_catch, answer;
  struct lazy_catch c;

  c.handler = handler;
  c.handler_data = handler_data;
  lazy_catch = make_lazy_catch (&c);

  SCM_REDEFER_INTS;
  scm_dynwinds = scm_acons (tag, lazy_catch, scm_dynwinds);
  SCM_REALLOW_INTS;

  answer = (*body) (body_data);

  SCM_REDEFER_INTS;
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  SCM_REALLOW_INTS;

  return answer;
}


/* scm_internal_stack_catch
   Use this one if you want debugging information to be stored in
   scm_the_last_stack_fluid on error. */

static SCM
ss_handler (void *data, SCM tag, SCM throw_args)
{
  /* Save the stack */
  scm_fluid_set_x (SCM_CDR (scm_the_last_stack_fluid),
		   scm_make_stack (SCM_BOOL_T, SCM_EOL));
  /* Throw the error */
  return scm_throw (tag, throw_args);
}

struct cwss_data
{
  SCM tag;
  scm_catch_body_t body;
  void *data;
};

static SCM
cwss_body (void *data)
{
  struct cwss_data *d = data;
  return scm_internal_lazy_catch (d->tag, d->body, d->data, ss_handler, NULL);
}

SCM
scm_internal_stack_catch (SCM tag,
			  scm_catch_body_t body,
			  void *body_data,
			  scm_catch_handler_t handler,
			  void *handler_data)
{
  struct cwss_data d;
  d.tag = tag;
  d.body = body;
  d.data = body_data;
  return scm_internal_catch (tag, cwss_body, &d, handler, handler_data);
}



/* body and handler functions for use with any of the above catch variants */

/* This is a body function you can pass to scm_internal_catch if you
   want the body to be like Scheme's `catch' --- a thunk.

   BODY_DATA is a pointer to a scm_body_thunk_data structure, which
   contains the Scheme procedure to invoke as the body, and the tag
   we're catching.  */

SCM
scm_body_thunk (void *body_data)
{
  struct scm_body_thunk_data *c = (struct scm_body_thunk_data *) body_data;

  return scm_apply (c->body_proc, SCM_EOL, SCM_EOL);
}


/* This is a handler function you can pass to scm_internal_catch if
   you want the handler to act like Scheme's catch: (throw TAG ARGS ...)
   applies a handler procedure to (TAG ARGS ...).

   If the user does a throw to this catch, this function runs a
   handler procedure written in Scheme.  HANDLER_DATA is a pointer to
   an SCM variable holding the Scheme procedure object to invoke.  It
   ought to be a pointer to an automatic variable (i.e., one living on
   the stack), or the procedure object should be otherwise protected
   from GC.  */
SCM
scm_handle_by_proc (void *handler_data, SCM tag, SCM throw_args)
{
  SCM *handler_proc_p = (SCM *) handler_data;

  return scm_apply (*handler_proc_p, scm_cons (tag, throw_args), SCM_EOL);
}

/* SCM_HANDLE_BY_PROC_CATCHING_ALL is like SCM_HANDLE_BY_PROC but
   catches all throws that the handler might emit itself.  The handler
   used for these `secondary' throws is SCM_HANDLE_BY_MESSAGE_NO_EXIT.  */

struct hbpca_data {
  SCM proc;
  SCM args;
};

static SCM
hbpca_body (void *body_data)
{
  struct hbpca_data *data = (struct hbpca_data *)body_data;
  return scm_apply (data->proc, data->args, SCM_EOL);
}

SCM
scm_handle_by_proc_catching_all (void *handler_data, SCM tag, SCM throw_args)
{
  SCM *handler_proc_p = (SCM *) handler_data;
  struct hbpca_data data;
  data.proc = *handler_proc_p;
  data.args = scm_cons (tag, throw_args);

  return scm_internal_catch (SCM_BOOL_T,
			     hbpca_body, &data,
			     scm_handle_by_message_noexit, NULL);
}

/* Derive the an exit status from the arguments to (quit ...).  */
int
scm_exit_status (SCM args)
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
	

static void
handler_message (void *handler_data, SCM tag, SCM args)
{
  char *prog_name = (char *) handler_data;
  SCM p = scm_cur_errp;

  if (scm_ilength (args) >= 3)
    {
      SCM stack   = scm_make_stack (SCM_BOOL_T, SCM_EOL);
      SCM subr    = SCM_CAR (args);
      SCM message = SCM_CADR (args);
      SCM parts   = SCM_CADDR (args);
      SCM rest    = SCM_CDDDR (args);

      if (SCM_BACKTRACE_P && SCM_NFALSEP (stack))
	{
	  scm_puts ("Backtrace:\n", p);
	  scm_display_backtrace (stack, p, SCM_UNDEFINED, SCM_UNDEFINED);
	  scm_newline (p);
	}
      scm_display_error (stack, p, subr, message, parts, rest);
    }
  else
    {
      if (! prog_name)
	prog_name = "guile";

      scm_puts (prog_name, p);
      scm_puts (": ", p);

      scm_puts ("uncaught throw to ", p);
      scm_prin1 (tag, p, 0);
      scm_puts (": ", p);
      scm_prin1 (args, p, 1);
      scm_putc ('\n', p);
    }
}


/* This is a handler function to use if you want scheme to print a
   message and die.  Useful for dealing with throws to uncaught keys
   at the top level.

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

/* Dirk:FIXME:: The name of the function should make clear that the
 * application gets terminated.
 */

SCM
scm_handle_by_message (void *handler_data, SCM tag, SCM args)
{
  if (SCM_NFALSEP (scm_eq_p (tag, scm_str2symbol ("quit"))))
    {
      exit (scm_exit_status (args));
    }

  handler_message (handler_data, tag, args);
  exit (2);
}


/* This is just like scm_handle_by_message, but it doesn't exit; it
   just returns #f.  It's useful in cases where you don't really know
   enough about the body to handle things in a better way, but don't
   want to let throws fall off the bottom of the wind list.  */
SCM
scm_handle_by_message_noexit (void *handler_data, SCM tag, SCM args)
{
  handler_message (handler_data, tag, args);

  return SCM_BOOL_F;
}


SCM
scm_handle_by_throw (void *handler_data, SCM tag, SCM args)
{
  scm_ithrow (tag, args, 1);
  return SCM_UNSPECIFIED;  /* never returns */
}



/* the Scheme-visible CATCH and LAZY-CATCH functions */

SCM_DEFINE (scm_catch, "catch", 3, 0, 0,
           (SCM tag, SCM thunk, SCM handler),
	    "Invoke @var{thunk} in the dynamic context of @var{handler} for\n"
	    "exceptions matching @var{key}.  If thunk throws to the symbol @var{key},\n"
	    "then @var{handler} is invoked this way:\n\n"
	    "@example\n"
	    "(handler key args ...)\n"
	    "@end example\n\n"
	    "@var{key} is a symbol or #t.\n\n"
	    "@var{thunk} takes no arguments.  If @var{thunk} returns normally, that\n"
	    "is the return value of @code{catch}.\n\n"
	    "Handler is invoked outside the scope of its own @code{catch}.  If\n"
	    "@var{handler} again throws to the same key, a new handler from further\n"
	    "up the call chain is invoked.\n\n"
	    "If the key is @code{#t}, then a throw to @emph{any} symbol will match\n"
	    "this call to @code{catch}.")
#define FUNC_NAME s_scm_catch
{
  struct scm_body_thunk_data c;

  SCM_ASSERT (SCM_SYMBOLP (tag) || SCM_EQ_P (tag, SCM_BOOL_T),
	      tag, SCM_ARG1, FUNC_NAME);

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
#undef FUNC_NAME


SCM_DEFINE (scm_lazy_catch, "lazy-catch", 3, 0, 0,
           (SCM tag, SCM thunk, SCM handler),
	    "")
#define FUNC_NAME s_scm_lazy_catch
{
  struct scm_body_thunk_data c;

  SCM_ASSERT (SCM_SYMBOLP (tag) || SCM_EQ_P (tag, SCM_BOOL_T),
	      tag, SCM_ARG1, FUNC_NAME);

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
#undef FUNC_NAME



/* throwing */

SCM_DEFINE (scm_throw, "throw", 1, 0, 1,
           (SCM key, SCM args),
	    "Invoke the catch form matching @var{key}, passing @var{args} to the\n"
	    "@var{handler}.  \n\n"
	    "@var{key} is a symbol.  It will match catches of the same symbol or of\n"
	    "#t.\n\n"
	    "If there is no handler at all, an error is signaled.")
#define FUNC_NAME s_scm_throw
{
  SCM_VALIDATE_SYMBOL (1,key);
  /* May return if handled by lazy catch. */
  return scm_ithrow (key, args, 1);
}
#undef FUNC_NAME

SCM
scm_ithrow (SCM key, SCM args, int noreturn)
{
  SCM jmpbuf = SCM_UNDEFINED;
  SCM wind_goal;

  SCM dynpair = SCM_UNDEFINED;
  SCM winds;

  /* Search the wind list for an appropriate catch.
     "Waiter, please bring us the wind list." */
  for (winds = scm_dynwinds; SCM_CONSP (winds); winds = SCM_CDR (winds))
    {
      dynpair = SCM_CAR (winds);
      if (SCM_CONSP (dynpair))
	{
	  SCM this_key = SCM_CAR (dynpair);

	  if (SCM_EQ_P (this_key, SCM_BOOL_T) || SCM_EQ_P (this_key, key))
	    break;
	}
    }

#ifdef __GNUC__
  /* Dirk:FIXME:: This bugfix should be removed some time. */
  /* GCC 2.95.2 has a bug in its optimizer that makes it generate
     incorrect code sometimes.  This barrier stops it from being too
     clever. */
  asm volatile ("" : "=g" (winds));
#endif

  /* If we didn't find anything, print a message and abort the process
     right here.  If you don't want this, establish a catch-all around
     any code that might throw up. */
  if (SCM_NULLP (winds))
    {
      scm_handle_by_message (NULL, key, args);
      abort ();
    }

  /* If the wind list is malformed, bail.  */
  if (!SCM_CONSP (winds))
    abort ();
      
  jmpbuf = SCM_CDR (dynpair);
  
  for (wind_goal = scm_dynwinds;
       !SCM_EQ_P (SCM_CDAR (wind_goal), jmpbuf);
       wind_goal = SCM_CDR (wind_goal))
    ;

  /* Is a lazy catch?  In wind list entries for lazy catches, the key
     is bound to a lazy_catch smob, not a jmpbuf.  */
  if (SCM_LAZY_CATCH_P (jmpbuf))
    {
      struct lazy_catch *c = (struct lazy_catch *) SCM_CELL_WORD_1 (jmpbuf);
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
  scm_tc16_jmpbuffer = scm_make_smob_type_mfpe ("jmpbuffer",
						0,
						NULL, /* mark */
						NULL,
						printjb,
						NULL);

  tc16_lazy_catch = scm_make_smob_type_mfpe ("lazy-catch", 0,
					     NULL,
					     NULL,
					     print_lazy_catch,
					     NULL);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/throw.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

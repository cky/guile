/*	Copyright (C) 1995,1996,1997,1998, 1999, 2000 Free Software Foundation, Inc.
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
#include "libguile/stackchk.h"
#include "libguile/dynwind.h"
#include "libguile/eval.h"
#include "libguile/smob.h"
#include "libguile/pairs.h"
#include "libguile/throw.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"

#include "libguile/root.h"


/* Define this if you want to try out the stack allocation of cwdr's
   jumpbuf.  It works for me but I'm still worried that the dynwinds
   might be able to make a mess. */

#undef USE_STACKJMPBUF

SCM scm_sys_protects[SCM_NUM_PROTECTS];

long scm_tc16_root;

#ifndef USE_THREADS
struct scm_root_state *scm_root;
#endif



static SCM
mark_root (SCM root)
{
  scm_root_state *s = SCM_ROOT_STATE (root);

  scm_gc_mark (s->rootcont);
  scm_gc_mark (s->dynwinds);
  scm_gc_mark (s->continuation_stack);
  scm_gc_mark (s->continuation_stack_ptr);
  scm_gc_mark (s->progargs);
  scm_gc_mark (s->exitval);
  scm_gc_mark (s->cur_inp);
  scm_gc_mark (s->cur_outp);
  scm_gc_mark (s->cur_errp);
  scm_gc_mark (s->def_inp);
  scm_gc_mark (s->def_outp);
  scm_gc_mark (s->def_errp);
  /* No need to gc mark def_loadp */
  scm_gc_mark (s->fluids);
  return SCM_ROOT_STATE (root) -> parent;
}


static int
print_root (SCM exp,SCM port,scm_print_state *pstate)
{
  scm_puts ("#<root ", port);
  scm_intprint(SCM_SEQ (SCM_ROOT_STATE (exp) -> rootcont), 16, port);
  scm_putc('>', port);
  return 1;
}




SCM
scm_make_root (SCM parent)
{
  SCM root;
  scm_root_state *root_state;

  root_state = (scm_root_state *) scm_must_malloc (sizeof (scm_root_state),
						   "scm_make_root");
  if (SCM_ROOTP (parent))
    {
      memcpy (root_state, SCM_ROOT_STATE (parent), sizeof (scm_root_state));
      scm_copy_fluids (root_state);
      root_state->parent = parent;
    }
  else
    {
      root_state->parent = SCM_BOOL_F;

      /* Initialize everything right now, in case a GC happens early.  */
      root_state->rootcont
	= root_state->dynwinds
	= root_state->continuation_stack
	= root_state->continuation_stack_ptr
	= root_state->progargs
	= root_state->exitval
	= root_state->cur_inp
	= root_state->cur_outp
	= root_state->cur_errp
	= root_state->def_inp
	= root_state->def_outp
	= root_state->def_errp
	= root_state->cur_loadp
	= root_state->fluids
	= root_state->system_transformer
	= root_state->top_level_lookup_closure_var
	= root_state->handle
	= root_state->parent
	= SCM_BOOL_F;
    }
  SCM_REDEFER_INTS;
  SCM_NEWSMOB (root, scm_tc16_root, root_state);
  root_state->handle = root;
  SCM_REALLOW_INTS;
  return root;
}

/* {call-with-dynamic-root}
 *
 * Suspending the current thread to evaluate a thunk on the
 * same C stack but under a new root.
 *
 * Calls to call-with-dynamic-root return exactly once (unless
 * the process is somehow exitted).  */

/* Some questions about cwdr:

   Couldn't the body just be a closure?  Do we really need to pass
   args through to it?

   The semantics are a lot like catch's; in fact, we call
   scm_internal_catch to take care of that part of things.  Wouldn't
   it be cleaner to say that uncaught throws just disappear into the
   ether (or print a message to stderr), and let the caller use catch
   themselves if they want to?

   -JimB */

#if 0
SCM scm_exitval;		/* INUM with return value */
#endif
static int n_dynamic_roots = 0;


/* cwdr fills out both of these structures, and then passes a pointer
   to them through scm_internal_catch to the cwdr_body and
   cwdr_handler functions, to tell them how to behave and to get
   information back from them.

   A cwdr is a lot like a catch, except there is no tag (all
   exceptions are caught), and the body procedure takes the arguments
   passed to cwdr as A1 and ARGS.  The handler is also special since
   it is not directly run from scm_internal_catch.  It is executed
   outside the new dynamic root. */

struct cwdr_body_data {
  /* Arguments to pass to the cwdr body function.  */
  SCM a1, args;

  /* Scheme procedure to use as body of cwdr.  */
  SCM body_proc;
};

struct cwdr_handler_data {
  /* Do we need to run the handler? */
  int run_handler;

  /* The tag and args to pass it. */
  SCM tag, args;
};


/* Invoke the body of a cwdr, assuming that the throw handler has
   already been set up.  DATA points to a struct set up by cwdr that
   says what proc to call, and what args to apply it to.

   With a little thought, we could replace this with scm_body_thunk,
   but I don't want to mess with that at the moment.  */
static SCM
cwdr_body (void *data)
{
  struct cwdr_body_data *c = (struct cwdr_body_data *) data;

  return scm_apply (c->body_proc, c->a1, c->args);
}

/* Record the fact that the body of the cwdr has thrown.  Record
   enough information to invoke the handler later when the dynamic
   root has been deestablished.  */

static SCM
cwdr_handler (void *data, SCM tag, SCM args)
{
  struct cwdr_handler_data *c = (struct cwdr_handler_data *) data;

  c->run_handler = 1;
  c->tag = tag;
  c->args = args;
  return SCM_UNSPECIFIED;
}

/* This is the basic code for new root creation.
 *
 * WARNING!  The order of actions in this routine is in many ways
 * critical.  E. g., it is essential that an error doesn't leave Guile
 * in a messed up state.  */

SCM 
scm_internal_cwdr (scm_catch_body_t body, void *body_data,
		   scm_catch_handler_t handler, void *handler_data,
		   SCM_STACKITEM *stack_start)
{
#ifdef USE_STACKJMPBUF
  scm_contregs static_contregs;
#endif
  int old_ints_disabled = scm_ints_disabled;
  SCM old_rootcont, old_winds;
  struct cwdr_handler_data my_handler_data;
  SCM answer;

  /* Create a fresh root continuation.  */
  {
    SCM new_rootcont;
    SCM_NEWCELL (new_rootcont);
    SCM_REDEFER_INTS;
#ifdef USE_STACKJMPBUF
    SCM_SET_CONTREGS (new_rootcont, &static_contregs);
#else
    SCM_SET_CONTREGS (new_rootcont,
		      scm_must_malloc (sizeof (scm_contregs),
				       "inferior root continuation"));
#endif
    SCM_SET_CELL_TYPE (new_rootcont, scm_tc7_contin);
    SCM_DYNENV (new_rootcont) = SCM_EOL;
    SCM_BASE (new_rootcont) = stack_start;
    SCM_SEQ (new_rootcont) = ++n_dynamic_roots;
#ifdef DEBUG_EXTENSIONS
    SCM_DFRAME (new_rootcont) = 0;
#endif
    old_rootcont = scm_rootcont;
    scm_rootcont = new_rootcont;
    SCM_REALLOW_INTS;
  }

  /* Exit caller's dynamic state.
   */
  old_winds = scm_dynwinds;
  scm_dowinds (SCM_EOL, scm_ilength (scm_dynwinds));
#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (old_rootcont) = scm_last_debug_frame;
  scm_last_debug_frame = 0;
#endif

  {
    my_handler_data.run_handler = 0;
    answer = scm_internal_catch (SCM_BOOL_T,
				 body, body_data,
				 cwdr_handler, &my_handler_data);
  }

  scm_dowinds (old_winds, - scm_ilength (old_winds));
  SCM_REDEFER_INTS;
#ifdef USE_STACKCJMPBUF
  SCM_SET_CONTREGS (scm_rootcont, NULL);
#endif
#ifdef DEBUG_EXTENSIONS
  scm_last_debug_frame = SCM_DFRAME (old_rootcont);
#endif
  scm_rootcont = old_rootcont;
  SCM_REALLOW_INTS;
  scm_ints_disabled = old_ints_disabled;

  /* Now run the real handler iff the body did a throw. */
  if (my_handler_data.run_handler)
    return handler (handler_data, my_handler_data.tag, my_handler_data.args);
  else
    return answer;
}

/* The original CWDR for invoking Scheme code with a Scheme handler. */

static SCM 
cwdr (SCM proc, SCM a1, SCM args, SCM handler, SCM_STACKITEM *stack_start)
{
  struct cwdr_body_data c;
  
  c.a1 = a1;
  c.args = args;
  c.body_proc = proc;

  return scm_internal_cwdr (cwdr_body, &c,
			    scm_handle_by_proc, &handler,
			    stack_start);
}

SCM_DEFINE (scm_call_with_dynamic_root, "call-with-dynamic-root", 2, 0, 0,
           (SCM thunk, SCM handler),
	    "Evaluate @var{(thunk)} in a new dynamic context, returning its value.\n\n"
	    "If an error occurs during evaluation, apply @var{handler} to the\n"
	    "arguments to the throw, just as @code{throw} would.  If this happens,\n"
	    "@var{handler} is called outside the scope of the new root -- it is\n"
	    "called in the same dynamic context in which\n"
	    "@code{call-with-dynamic-root} was evaluated.\n\n"
	    "If @var{thunk} captures a continuation, the continuation is rooted at\n"
	    "the call to @var{thunk}.  In particular, the call to\n"
	    "@code{call-with-dynamic-root} is not captured.  Therefore,\n"
	    "@code{call-with-dynamic-root} always returns at most one time.\n\n"
	    "Before calling @var{thunk}, the dynamic-wind chain is un-wound back to\n"
	    "the root and a new chain started for @var{thunk}.  Therefore, this call\n"
	    "may not do what you expect:\n\n"
	    "@example\n"
	    ";; Almost certainly a bug:\n"
	    "(with-output-to-port\n"
	    " some-port\n\n"
	    " (lambda ()\n"
	    "   (call-with-dynamic-root\n"
	    "    (lambda ()\n"
	    "      (display 'fnord)\n"
	    "      (newline))\n"
	    "    (lambda (errcode) errcode))))\n"
	    "@end example\n\n"
	    "The problem is, on what port will @samp{fnord} be displayed?  You\n"
	    "might expect that because of the @code{with-output-to-port} that\n"
	    "it will be displayed on the port bound to @code{some-port}.  But it\n"
	    "probably won't -- before evaluating the thunk, dynamic winds are\n"
	    "unwound, including those created by @code{with-output-to-port}.\n"
	    "So, the standard output port will have been re-set to its default value\n"
	    "before @code{display} is evaluated.\n\n"
	    "(This function was added to Guile mostly to help calls to functions in C\n"
	    "libraries that can not tolerate non-local exits or calls that return\n"
	    "multiple times.  If such functions call back to the interpreter, it should\n"
	    "be under a new dynamic root.)")
#define FUNC_NAME s_scm_call_with_dynamic_root
{
  SCM_STACKITEM stack_place;
  return cwdr (thunk, SCM_EOL, SCM_EOL, handler, &stack_place);
}
#undef FUNC_NAME

SCM_DEFINE (scm_dynamic_root, "dynamic-root", 0, 0, 0, 
           (),
	    "Return an object representing the current dynamic root.\n\n"
	    "These objects are only useful for comparison using @code{eq?}.\n"
	    "They are currently represented as numbers, but your code should\n"
	    "in no way depend on this.")
#define FUNC_NAME s_scm_dynamic_root
{
  return scm_ulong2num (SCM_SEQ (scm_root->rootcont));
}
#undef FUNC_NAME

SCM
scm_apply_with_dynamic_root (SCM proc, SCM a1, SCM args, SCM handler)
{
  SCM_STACKITEM stack_place;
  return cwdr (proc, a1, args, handler, &stack_place);
}



#if (SCM_DEBUG_DEPRECATED == 0)

/* Call thunk(closure) underneath a top-level error handler.
 * If an error occurs, pass the exitval through err_filter and return it.
 * If no error occurs, return the value of thunk.
 */

#ifdef _UNICOS
typedef int setjmp_type;
#else
typedef long setjmp_type;
#endif


SCM
scm_call_catching_errors (SCM (*thunk)(), SCM (*err_filter)(), void *closure)
{
  SCM answer;
  setjmp_type i;
#ifdef DEBUG_EXTENSIONS
  SCM_DFRAME (scm_rootcont) = scm_last_debug_frame;
#endif
  i = setjmp (SCM_JMPBUF (scm_rootcont));
  scm_stack_checking_enabled_p = SCM_STACK_CHECKING_P;
  if (!i)
    {
      scm_gc_heap_lock = 0;
      answer = thunk (closure);
    }
  else
    {
      scm_gc_heap_lock = 1;
      answer = err_filter (scm_exitval, closure);
    }
  return answer;
}

#endif  /* SCM_DEBUG_DEPRECATED == 0 */


void
scm_init_root ()
{
  scm_tc16_root = scm_make_smob_type ("root", sizeof (struct scm_root_state));
  scm_set_smob_mark (scm_tc16_root, mark_root);
  scm_set_smob_print (scm_tc16_root, print_root);

#include "libguile/root.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

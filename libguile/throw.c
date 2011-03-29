/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2003, 2004, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <unistdio.h>
#include "libguile/_scm.h"
#include "libguile/smob.h"
#include "libguile/eval.h"
#include "libguile/eq.h"
#include "libguile/control.h"
#include "libguile/deprecation.h"
#include "libguile/backtrace.h"
#include "libguile/debug.h"
#include "libguile/stackchk.h"
#include "libguile/stacks.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"
#include "libguile/validate.h"
#include "libguile/vm.h"
#include "libguile/throw.h"
#include "libguile/init.h"
#include "libguile/strings.h"

#include "libguile/private-options.h"


/* Pleasantly enough, the guts of catch are defined in Scheme, in terms of
   prompt, abort, and the %exception-handler fluid. This file just provides
   shims so that it's easy to have catch functionality from C.

   All of these function names and prototypes carry a fair bit of historical
   baggage. */


#define CACHE_VAR(var,name)                                             \
  static SCM var = SCM_BOOL_F;                                          \
  if (scm_is_false (var))                                               \
    {                                                                   \
      var = scm_module_variable (scm_the_root_module (),                \
                                 scm_from_latin1_symbol (name));        \
      if (scm_is_false (var))                                           \
        abort ();                                                       \
    }



SCM
scm_catch (SCM key, SCM thunk, SCM handler)
{
  CACHE_VAR (var, "catch");

  return scm_call_3 (scm_variable_ref (var), key, thunk, handler);
}

SCM
scm_catch_with_pre_unwind_handler (SCM key, SCM thunk, SCM handler,
                                   SCM pre_unwind_handler)
{
  if (SCM_UNBNDP (pre_unwind_handler))
    return scm_catch (key, thunk, handler);
  else
    {
      CACHE_VAR (var, "catch");
      
      return scm_call_4 (scm_variable_ref (var), key, thunk, handler,
                         pre_unwind_handler);
    }
}

SCM
scm_with_throw_handler (SCM key, SCM thunk, SCM handler)
{
  CACHE_VAR (var, "with-throw-handler");

  return scm_call_3 (scm_variable_ref (var), key, thunk, handler);
}

SCM
scm_throw (SCM key, SCM args)
{
  CACHE_VAR (var, "throw");

  return scm_apply_1 (scm_variable_ref (var), key, args);
}



/* Now some support for C bodies and catch handlers */

static scm_t_bits tc16_catch_closure;

enum {
  CATCH_CLOSURE_BODY,
  CATCH_CLOSURE_HANDLER
};

static SCM
make_catch_body_closure (scm_t_catch_body body, void *body_data)
{
  SCM ret;
  SCM_NEWSMOB2 (ret, tc16_catch_closure, body, body_data);
  SCM_SET_SMOB_FLAGS (ret, CATCH_CLOSURE_BODY);
  return ret;
}

static SCM
make_catch_handler_closure (scm_t_catch_handler handler, void *handler_data)
{
  SCM ret;
  SCM_NEWSMOB2 (ret, tc16_catch_closure, handler, handler_data);
  SCM_SET_SMOB_FLAGS (ret, CATCH_CLOSURE_HANDLER);
  return ret;
}

static SCM
apply_catch_closure (SCM clo, SCM args)
{
  void *data = (void*)SCM_SMOB_DATA_2 (clo);

  switch (SCM_SMOB_FLAGS (clo))
    {
    case CATCH_CLOSURE_BODY:
      {
        scm_t_catch_body body = (void*)SCM_SMOB_DATA (clo);
        return body (data);
      }
    case CATCH_CLOSURE_HANDLER:
      {
        scm_t_catch_handler handler = (void*)SCM_SMOB_DATA (clo);
        return handler (data, scm_car (args), scm_cdr (args));
      }
    default:
      abort ();
    }
}

/* TAG is the catch tag.  Typically, this is a symbol, but this
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
scm_c_catch (SCM tag,
	     scm_t_catch_body body, void *body_data,
	     scm_t_catch_handler handler, void *handler_data,
	     scm_t_catch_handler pre_unwind_handler, void *pre_unwind_handler_data)
{
  SCM sbody, shandler, spre_unwind_handler;
  
  sbody = make_catch_body_closure (body, body_data);
  shandler = make_catch_handler_closure (handler, handler_data);
  if (pre_unwind_handler)
    spre_unwind_handler = make_catch_handler_closure (pre_unwind_handler,
                                                      pre_unwind_handler_data);
  else
    spre_unwind_handler = SCM_UNDEFINED;
  
  return scm_catch_with_pre_unwind_handler (tag, sbody, shandler,
                                            spre_unwind_handler);
}

SCM
scm_internal_catch (SCM tag,
		    scm_t_catch_body body, void *body_data,
		    scm_t_catch_handler handler, void *handler_data)
{
  return scm_c_catch (tag,
                      body, body_data,
                      handler, handler_data,
                      NULL, NULL);
}


SCM
scm_c_with_throw_handler (SCM tag,
			  scm_t_catch_body body,
			  void *body_data,
			  scm_t_catch_handler handler,
			  void *handler_data,
			  int lazy_catch_p)
{
  SCM sbody, shandler;

  if (lazy_catch_p)
    scm_c_issue_deprecation_warning
      ("The LAZY_CATCH_P argument to `scm_c_with_throw_handler' is no longer.\n"
       "supported. Instead the handler will be invoked from within the dynamic\n"
       "context of the corresponding `throw'.\n"
       "\nTHIS COULD CHANGE YOUR PROGRAM'S BEHAVIOR.\n\n"
       "Please modify your program to pass 0 as the LAZY_CATCH_P argument,\n"
       "and adapt it (if necessary) to expect to be within the dynamic context\n"
       "of the throw.");

  sbody = make_catch_body_closure (body, body_data);
  shandler = make_catch_handler_closure (handler, handler_data);
  
  return scm_with_throw_handler (tag, sbody, shandler);
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

  return scm_call_0 (c->body_proc);
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

  return scm_apply_1 (*handler_proc_p, tag, throw_args);
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
  return scm_apply_0 (data->proc, data->args);
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
  if (!SCM_NULL_OR_NIL_P (args))
    {
      SCM cqa = SCM_CAR (args);
      
      if (scm_is_integer (cqa))
	return (scm_to_int (cqa));
      else if (scm_is_false (cqa))
	return 1;
    }
  return 0;
}
	

static int
should_print_backtrace (SCM tag, SCM stack)
{
  return SCM_BACKTRACE_P
    && scm_is_true (stack)
    && scm_initialized_p
    /* It's generally not useful to print backtraces for errors reading
       or expanding code in these fallback catch statements. */
    && !scm_is_eq (tag, scm_from_latin1_symbol ("read-error"))
    && !scm_is_eq (tag, scm_from_latin1_symbol ("syntax-error"));
}

static void
handler_message (void *handler_data, SCM tag, SCM args)
{
  SCM p, stack, frame;

  p = scm_current_error_port ();
  /* Usually we get here via a throw to a catch-all.  In that case
     there is the throw frame active, and the catch closure, so narrow by
     two frames.  It is possible for a user to invoke
     scm_handle_by_message directly, though, so it could be this
     narrows too much.  We'll have to see how this works out in
     practice.  */
  stack = scm_make_stack (SCM_BOOL_T, scm_list_1 (scm_from_int (2)));
  frame = scm_is_true (stack) ? scm_stack_ref (stack, SCM_INUM0) : SCM_BOOL_F;

  if (should_print_backtrace (tag, stack))
    {
      scm_puts ("Backtrace:\n", p);
      scm_display_backtrace_with_highlights (stack, p,
                                             SCM_BOOL_F, SCM_BOOL_F,
                                             SCM_EOL);
      scm_newline (p);
    }

  scm_print_exception (p, frame, tag, args);
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
  if (scm_is_true (scm_eq_p (tag, scm_from_latin1_symbol ("quit"))))
    exit (scm_exit_status (args));

  handler_message (handler_data, tag, args);
  scm_i_pthread_exit (NULL);

  /* this point not reached, but suppress gcc warning about no return value
     in case scm_i_pthread_exit isn't marked as "noreturn" (which seemed not
     to be the case on cygwin for instance) */
  return SCM_BOOL_F;
}


/* This is just like scm_handle_by_message, but it doesn't exit; it
   just returns #f.  It's useful in cases where you don't really know
   enough about the body to handle things in a better way, but don't
   want to let throws fall off the bottom of the wind list.  */
SCM
scm_handle_by_message_noexit (void *handler_data, SCM tag, SCM args)
{
  if (scm_is_true (scm_eq_p (tag, scm_from_latin1_symbol ("quit"))))
    exit (scm_exit_status (args));

  handler_message (handler_data, tag, args);

  return SCM_BOOL_F;
}


SCM
scm_handle_by_throw (void *handler_data SCM_UNUSED, SCM tag, SCM args)
{
  scm_ithrow (tag, args, 1);
  return SCM_UNSPECIFIED;  /* never returns */
}

SCM
scm_ithrow (SCM key, SCM args, int noreturn SCM_UNUSED)
{
  return scm_throw (key, args);
}

/* Unfortunately we have to support catch and throw before boot-9 has, um,
   booted. So here are lame versions, which will get replaced with their scheme
   equivalents. */

SCM_SYMBOL (sym_pre_init_catch_tag, "%pre-init-catch-tag");

static SCM
pre_init_catch (SCM tag, SCM thunk, SCM handler, SCM pre_unwind_handler)
{
  SCM vm, prompt, res;

  /* Only handle catch-alls without pre-unwind handlers */
  if (!SCM_UNBNDP (pre_unwind_handler))
    abort ();
  if (scm_is_false (scm_eqv_p (tag, SCM_BOOL_T)))
    abort ();

  vm = scm_the_vm ();
  prompt = scm_c_make_prompt (sym_pre_init_catch_tag,
                              SCM_VM_DATA (vm)->fp, SCM_VM_DATA (vm)->sp,
                              SCM_VM_DATA (vm)->ip, 1, -1, scm_i_dynwinds ());
  scm_i_set_dynwinds (scm_cons (prompt, SCM_PROMPT_DYNWINDS (prompt)));

  if (SCM_PROMPT_SETJMP (prompt))
    {
      /* nonlocal exit */
      SCM args = scm_i_prompt_pop_abort_args_x (vm);
      /* cdr past the continuation */
      return scm_apply_0 (handler, scm_cdr (args));
    }

  res = scm_call_0 (thunk);
  scm_i_set_dynwinds (scm_cdr (scm_i_dynwinds ()));

  return res;
}

static int
find_pre_init_catch (void)
{
  SCM winds;

  /* Search the wind list for an appropriate prompt.
     "Waiter, please bring us the wind list." */
  for (winds = scm_i_dynwinds (); scm_is_pair (winds); winds = SCM_CDR (winds))
    if (SCM_PROMPT_P (SCM_CAR (winds))
        && scm_is_eq (SCM_PROMPT_TAG (SCM_CAR (winds)), sym_pre_init_catch_tag))
      return 1;

  return 0;
}

static SCM
pre_init_throw (SCM k, SCM args)
{
  if (find_pre_init_catch ())
    return scm_at_abort (sym_pre_init_catch_tag, scm_cons (k, args));
  else
    { 
      static int error_printing_error = 0;
      static int error_printing_fallback = 0;
      
      if (error_printing_fallback)
        fprintf (stderr, "\nFailed to print exception.\n");
      else if (error_printing_error)
        {
          fprintf (stderr, "\nError while printing exception:\n");
          error_printing_fallback = 1;
          fprintf (stderr, "Key: ");
          scm_write (k, scm_current_error_port ());
          fprintf (stderr, ", args: ");
          scm_write (args, scm_current_error_port ());
          scm_newline (scm_current_error_port ());
        }
      else
        {
          fprintf (stderr, "Throw without catch before boot:\n");
          error_printing_error = 1;
          scm_handle_by_message_noexit (NULL, k, args);
        }

      fprintf (stderr, "Aborting.\n");
      abort ();
      return SCM_BOOL_F; /* not reached */
    }
}

void
scm_init_throw ()
{
  tc16_catch_closure = scm_make_smob_type ("catch-closure", 0);
  scm_set_smob_apply (tc16_catch_closure, apply_catch_closure, 0, 0, 1);

  scm_c_define ("catch", scm_c_make_gsubr ("catch", 3, 1, 0, pre_init_catch));
  scm_c_define ("throw", scm_c_make_gsubr ("throw", 1, 0, 1, pre_init_throw));

#include "libguile/throw.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

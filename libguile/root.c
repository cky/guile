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
#include "stackchk.h"
#include "dynwind.h"
#include "eval.h"
#include "genio.h"
#include "smob.h"
#include "pairs.h"
#include "throw.h"

#include "root.h"


SCM scm_sys_protects[SCM_NUM_PROTECTS];

long scm_tc16_root;

#ifndef USE_THREADS
struct scm_root_state *scm_root;
#endif



static SCM mark_root SCM_P ((SCM));

static SCM
mark_root (root)
     SCM root;
{
  scm_root_state *s = SCM_ROOT_STATE (root);
  SCM_SETGC8MARK (root);
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
  scm_gc_mark (s->top_level_lookup_closure_var);
  scm_gc_mark (s->system_transformer);
  return SCM_ROOT_STATE (root) -> parent;
}

static scm_sizet free_root SCM_P ((SCM));

static scm_sizet
free_root (root)
     SCM root;
{
  scm_must_free ((char *) SCM_ROOT_STATE (root));
  return sizeof (scm_root_state);
}

static int print_root SCM_P ((SCM exp, SCM port, scm_print_state *pstate));

static int
print_root (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  scm_gen_puts (scm_regular_string, "#<root ", port);
  scm_intprint(SCM_SEQ (SCM_ROOT_STATE (exp) -> rootcont), 16, port);
  scm_gen_putc('>', port);
  return 1;
}

static scm_smobfuns root_smob =
{
  mark_root,
  free_root,
  print_root,
  0
};



SCM
scm_make_root (parent)
     SCM parent;
{
  SCM root;
  scm_root_state *root_state;

  root_state = (scm_root_state *) scm_must_malloc (sizeof (scm_root_state),
						   "scm_make_root");
  if (SCM_NIMP (parent) && SCM_ROOTP (parent))
    {
      memcpy (root_state, SCM_ROOT_STATE (parent), sizeof (scm_root_state));
      root_state->parent = parent;
    }
  else
    {
      root_state->parent = SCM_BOOL_F;
    }
  SCM_NEWCELL (root);
  SCM_REDEFER_INTS;
  SCM_SETCAR (root, scm_tc16_root);
  SCM_SETCDR (root, root_state);
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
 * the process is somehow exitted).
 */

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


/* cwdr fills out one of these structures, and then passes a pointer
   to it through scm_internal_catch to the cwdr_body and cwdr_handler
   functions, to tell them how to behave.

   A cwdr is a lot like a catch, except there is no tag (all
   exceptions are caught), and the body procedure takes the arguments
   passed to cwdr as A1 and ARGS.  */

struct cwdr_body_data {

  /* Arguments to pass to the cwdr body function.  */
  SCM a1, args;

  /* Scheme procedure to use as body of cwdr.  */
  SCM body_proc;

  /* Scheme procedure to call if a throw occurs within the cwdr.  */
  SCM handler_proc;
};


/* Invoke the body of a cwdr, assuming that the throw handler has
   already been set up.  DATA points to a struct set up by cwdr that
   says what proc to call, and what args to apply it to.  */
static SCM cwdr_body SCM_P ((void *, SCM));

static SCM
cwdr_body (void *data, SCM jmpbuf)
{
  struct cwdr_body_data *c = (struct cwdr_body_data *) data;

  return scm_apply (c->body_proc, c->a1, c->args);
}


/* Invoke the handler of a cwdr.  DATA points to a struct set up by
   cwdr that says what proc to call to handle the throw.  */
static SCM cwdr_handler SCM_P ((void *, SCM, SCM));

static SCM
cwdr_handler (void *data, SCM tag, SCM throw_args)
{
  struct cwdr_body_data *c = (struct cwdr_body_data *) data;

  return scm_apply (c->handler_proc, scm_cons (tag, throw_args), SCM_EOL);
}


static SCM cwdr SCM_P ((SCM thunk, SCM a1, SCM args, SCM handler, SCM_STACKITEM *stack_start));

/* This is the basic code for new root creation.
 *
 * WARNING!  The order of actions in this routine is in many ways
 * critical.  E. g., it is essential that an error doesn't leave Guile
 * in a messed up state.  */

static SCM 
cwdr (proc, a1, args, handler, stack_start)
     SCM proc;
     SCM a1;
     SCM args;
     SCM handler;
     SCM_STACKITEM *stack_start;
{
  int old_ints_disabled = scm_ints_disabled;
  SCM old_rootcont, old_winds;
  SCM answer;

  /* Create a fresh root continuation.
   */
  {
    SCM new_rootcont;
    SCM_NEWCELL (new_rootcont);
    SCM_REDEFER_INTS;
    SCM_SETJMPBUF (new_rootcont,
		   scm_must_malloc ((long) sizeof (scm_contregs),
				    "inferior root continuation"));
    SCM_SETCAR (new_rootcont, scm_tc7_contin);
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
  
  /* Catch all errors. */
  {
    struct cwdr_body_data c;

    c.a1 = a1;
    c.args = args;
    c.body_proc = proc;
    c.handler_proc = handler;

    answer = scm_internal_catch (SCM_BOOL_T, cwdr_body, cwdr_handler, &c);
  }
  
  scm_dowinds (old_winds, - scm_ilength (old_winds));
  SCM_REDEFER_INTS;
#ifdef DEBUG_EXTENSIONS
  scm_last_debug_frame = SCM_DFRAME (old_rootcont);
#endif
  scm_rootcont = old_rootcont;
  SCM_REALLOW_INTS;
  scm_ints_disabled = old_ints_disabled;
  return answer;
}


SCM_PROC(s_call_with_dynamic_root, "call-with-dynamic-root", 2, 0, 0, scm_call_with_dynamic_root);
SCM
scm_call_with_dynamic_root (thunk, handler)
     SCM thunk;
     SCM handler;
{
  SCM_STACKITEM stack_place;

  return cwdr (thunk, SCM_EOL, SCM_EOL, handler, &stack_place);
}

SCM_PROC(s_dynamic_root, "dynamic-root", 0, 0, 0, scm_dynamic_root);
SCM
scm_dynamic_root ()
{
  return scm_ulong2num (SCM_SEQ (scm_root->rootcont));
}

SCM
scm_apply_with_dynamic_root (proc, a1, args, handler)
     SCM proc;
     SCM a1;
     SCM args;
     SCM handler;
{
  SCM_STACKITEM stack_place;
  return cwdr (proc, a1, args, handler, &stack_place);
}



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
scm_call_catching_errors (thunk, err_filter, closure)
     SCM (*thunk)();
     SCM (*err_filter)();
     void *closure;
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

void
scm_init_root ()
{
  scm_tc16_root = scm_newsmob (&root_smob);
#include "root.x"
}

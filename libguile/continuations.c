/*	Copyright (C) 1995,1996,1998, 2000 Free Software Foundation, Inc.
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
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/root.h"
#include "libguile/stackchk.h"
#include "libguile/smob.h"
#include "libguile/ports.h"
#include "libguile/dynwind.h"
#include "libguile/values.h"

#ifdef DEBUG_EXTENSIONS
#include "libguile/debug.h"
#endif

#include "libguile/continuations.h"



/* {Continuations}
 */

scm_bits_t scm_tc16_continuation;

static SCM
continuation_mark (SCM obj)
{
  scm_contregs *continuation = SCM_CONTREGS (obj);

  scm_gc_mark (continuation->throw_value);
  scm_mark_locations (continuation->stack, continuation->num_stack_items);
  return continuation->dynenv;
}

static scm_sizet
continuation_free (SCM obj)
{
  scm_contregs *continuation = SCM_CONTREGS (obj);
  /* stack array size is 1 if num_stack_items is 0 (rootcont).  */
  scm_sizet extra_items = (continuation->num_stack_items > 0)
    ? (continuation->num_stack_items - 1)
    : 0;
  scm_sizet bytes_free = sizeof (scm_contregs)
    + extra_items * sizeof (SCM_STACKITEM);
  
  scm_must_free (continuation);
  return bytes_free;
}

static int
continuation_print (SCM obj, SCM port, scm_print_state *state)
{
  scm_contregs *continuation = SCM_CONTREGS (obj);

  scm_puts ("#<continuation ", port);
  scm_intprint (continuation->num_stack_items, 10, port);
  scm_puts (" @ ", port);
  scm_intprint (SCM_CELL_WORD_1 (obj), 16, port);
  scm_putc ('>', port);
  return 1;
}

/* this may return more than once: the first time with the escape
   procedure, then subsequently with the value to be passed to the
   continuation.  */
#define FUNC_NAME "scm_make_continuation"
SCM 
scm_make_continuation (int *first)
{
  volatile SCM cont;
  scm_contregs *continuation;
  scm_contregs *rootcont = SCM_CONTREGS (scm_rootcont);
  long stack_size;
  SCM_STACKITEM * src;

  SCM_ENTER_A_SECTION;
  SCM_FLUSH_REGISTER_WINDOWS;
  stack_size = scm_stack_size (rootcont->base);
  continuation = scm_must_malloc (sizeof (scm_contregs)
				  + (stack_size - 1) * sizeof (SCM_STACKITEM),
				  FUNC_NAME);
  continuation->num_stack_items = stack_size;
  continuation->dynenv = scm_dynwinds;
  continuation->throw_value = SCM_EOL;
  continuation->base = src = rootcont->base;
  continuation->seq = rootcont->seq;
#ifdef DEBUG_EXTENSIONS
  continuation->dframe = scm_last_debug_frame;
#endif
  SCM_NEWSMOB (cont, scm_tc16_continuation, continuation);
  SCM_EXIT_A_SECTION;

#ifndef SCM_STACK_GROWS_UP
  src -= stack_size;
#endif
  memcpy (continuation->stack, src, sizeof (SCM_STACKITEM) * stack_size);

  if (setjmp (continuation->jmpbuf))
    {
      *first = 0;
      return continuation->throw_value;
    }
  else
    {
      *first = 1;
      return cont;
    }
}
#undef FUNC_NAME

static void scm_dynthrow (SCM, SCM);

/* Grow the stack by a fixed amount to provide space to copy in the
 * continuation.  Possibly this function has to be called several times
 * recursively before enough space is available.  Make sure the compiler does
 * not optimize the growth array away by storing it's address into a global
 * variable.
 */

scm_bits_t scm_i_dummy;

static void 
grow_stack (SCM cont, SCM val)
{
  scm_bits_t growth[100];

  scm_i_dummy = (scm_bits_t) growth;
  scm_dynthrow (cont, val);
}


/* Copy the continuation stack into the current stack.  Calling functions from
 * within this function is safe, since only stack frames below this function's
 * own frame are overwritten.  Thus, memcpy can be used for best performance.
 */
static void
copy_stack_and_call (scm_contregs *continuation, SCM val, 
		     SCM_STACKITEM * dst)
{
  memcpy (dst, continuation->stack,
	  sizeof (SCM_STACKITEM) * continuation->num_stack_items);

#ifdef DEBUG_EXTENSIONS
  scm_last_debug_frame = continuation->dframe;
#endif

  continuation->throw_value = val;
  longjmp (continuation->jmpbuf, 1);
}


/* Call grow_stack until the stack space is large enough, then, as the current
 * stack frame might get overwritten, let copy_stack_and_call perform the
 * actual copying and continuation calling.
 */
static void 
scm_dynthrow (SCM cont, SCM val)
{
  scm_contregs *continuation = SCM_CONTREGS (cont);
  SCM_STACKITEM * dst = SCM_BASE (scm_rootcont);
  SCM_STACKITEM stack_top_element;

#ifdef SCM_STACK_GROWS_UP
  if (SCM_PTR_GE (dst + continuation->num_stack_items, &stack_top_element))
    grow_stack (cont, val);
#else
  dst -= continuation->num_stack_items;
  if (SCM_PTR_LE (dst, &stack_top_element))
    grow_stack (cont, val);
#endif /* def SCM_STACK_GROWS_UP */

  SCM_FLUSH_REGISTER_WINDOWS;
  copy_stack_and_call (continuation, val, dst);
}

#define FUNC_NAME "continuation_apply"
static SCM continuation_apply (SCM cont, SCM args)
{
  scm_contregs *continuation = SCM_CONTREGS (cont);
  scm_contregs *rootcont = SCM_CONTREGS (scm_rootcont);

  if (continuation->seq != rootcont->seq
      /* this base comparison isn't needed */
      || continuation->base != rootcont->base)
    {
      scm_wta (cont, "continuation from wrong top level", FUNC_NAME);
    }
  
  scm_dowinds (continuation->dynenv,
	       scm_ilength (scm_dynwinds)
	       - scm_ilength (continuation->dynenv));
  
  scm_dynthrow (cont, scm_values (args));
  return SCM_UNSPECIFIED; /* not reached */
}
#undef FUNC_NAME

void
scm_init_continuations ()
{
  scm_tc16_continuation = scm_make_smob_type ("continuation", 0);
  scm_set_smob_mark (scm_tc16_continuation, continuation_mark);
  scm_set_smob_free (scm_tc16_continuation, continuation_free);
  scm_set_smob_print (scm_tc16_continuation, continuation_print);
  scm_set_smob_apply (scm_tc16_continuation, continuation_apply, 0, 0, 1);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/continuations.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* Copyright (C) 1995,1996,1998,2000,2001,2004 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */




#include "libguile/_scm.h"

#include <string.h>

#include "libguile/debug.h"
#include "libguile/root.h"
#include "libguile/stackchk.h"
#include "libguile/smob.h"
#include "libguile/ports.h"
#include "libguile/dynwind.h"
#include "libguile/values.h"

#include "libguile/validate.h"
#include "libguile/continuations.h"



/* {Continuations}
 */

scm_t_bits scm_tc16_continuation;

static SCM
continuation_mark (SCM obj)
{
  scm_t_contregs *continuation = SCM_CONTREGS (obj);

  scm_gc_mark (continuation->throw_value);
  scm_mark_locations (continuation->stack, continuation->num_stack_items);
#ifdef __ia64__
  if (continuation->backing_store)
    scm_mark_locations (continuation->backing_store, 
                        continuation->backing_store_size / 
                        sizeof (SCM_STACKITEM));
#endif /* __ia64__ */
  return continuation->dynenv;
}

static size_t
continuation_free (SCM obj)
{
  scm_t_contregs *continuation = SCM_CONTREGS (obj);
  /* stack array size is 1 if num_stack_items is 0 (rootcont).  */
  size_t extra_items = (continuation->num_stack_items > 0)
    ? (continuation->num_stack_items - 1)
    : 0;
  size_t bytes_free = sizeof (scm_t_contregs)
    + extra_items * sizeof (SCM_STACKITEM);

#ifdef __ia64__
  scm_gc_free (continuation->backing_store, continuation->backing_store_size,
	       "continuation backing store");
#endif /* __ia64__ */ 
  scm_gc_free (continuation, bytes_free, "continuation");
  return 0;
}

static int
continuation_print (SCM obj, SCM port, scm_print_state *state SCM_UNUSED)
{
  scm_t_contregs *continuation = SCM_CONTREGS (obj);

  scm_puts ("#<continuation ", port);
  scm_intprint (continuation->num_stack_items, 10, port);
  scm_puts (" @ ", port);
  scm_uintprint (SCM_CELL_WORD_1 (obj), 16, port);
  scm_putc ('>', port);
  return 1;
}

#ifdef __ia64__
/* Extern declaration of getcontext()/setcontext() in order to redefine
   getcontext() since on ia64-linux the second return value indicates whether
   it returned from getcontext() itself or by running setcontext(). */
struct rv
{
  long retval;
  long first_return;
};
extern struct rv ia64_getcontext (ucontext_t *) __asm__ ("getcontext");
#endif /* __ia64__ */

/* this may return more than once: the first time with the escape
   procedure, then subsequently with the value to be passed to the
   continuation.  */
#define FUNC_NAME "scm_make_continuation"
SCM 
scm_make_continuation (int *first)
{
  volatile SCM cont;
  scm_t_contregs *continuation;
  scm_t_contregs *rootcont = SCM_CONTREGS (scm_rootcont);
  long stack_size;
  SCM_STACKITEM * src;
#ifdef __ia64__
  struct rv rv;
#endif /* __ia64__ */

  SCM_ENTER_A_SECTION;
  SCM_FLUSH_REGISTER_WINDOWS;
  stack_size = scm_stack_size (rootcont->base);
  continuation = scm_gc_malloc (sizeof (scm_t_contregs)
				+ (stack_size - 1) * sizeof (SCM_STACKITEM),
				"continuation");
  continuation->num_stack_items = stack_size;
  continuation->dynenv = scm_dynwinds;
  continuation->throw_value = SCM_EOL;
  continuation->base = src = rootcont->base;
  continuation->seq = rootcont->seq;
  continuation->dframe = scm_last_debug_frame;
  SCM_NEWSMOB (cont, scm_tc16_continuation, continuation);
  SCM_EXIT_A_SECTION;

#if ! SCM_STACK_GROWS_UP
  src -= stack_size;
#endif
  memcpy (continuation->stack, src, sizeof (SCM_STACKITEM) * stack_size);

#ifdef __ia64__
  rv = ia64_getcontext (&continuation->ctx);
  if (rv.first_return)
    {
      continuation->backing_store_size = 
        continuation->ctx.uc_mcontext.sc_ar_bsp - 
        (unsigned long) __libc_ia64_register_backing_store_base;
      continuation->backing_store = NULL;
      continuation->backing_store = 
        scm_gc_malloc (continuation->backing_store_size,
		       "continuation backing store");
      memcpy (continuation->backing_store, 
              (void *) __libc_ia64_register_backing_store_base, 
              continuation->backing_store_size);
      *first = 1;
      return cont;
    }
  else
    {
      SCM ret = continuation->throw_value;
      *first = 0;
      continuation->throw_value = SCM_BOOL_F;
      return ret;
    }
#else /* !__ia64__ */
  if (setjmp (continuation->jmpbuf))
    {
      SCM ret = continuation->throw_value;
      *first = 0;
      continuation->throw_value = SCM_BOOL_F;
      return ret;
    }
  else
    {
      *first = 1;
      return cont;
    }
#endif /* !__ia64__ */
}
#undef FUNC_NAME


/* Invoking a continuation proceeds as follows:
 *
 * - the stack is made large enough for the called continuation
 * - the old windchain is unwound down to the branching point
 * - the continuation stack is copied into place
 * - the windchain is rewound up to the continuation's context
 * - the continuation is invoked via longjmp (or setcontext)
 *
 * This order is important so that unwind and rewind handlers are run
 * with their correct stack.
 */

static void scm_dynthrow (SCM, SCM);

/* Grow the stack by a fixed amount to provide space to copy in the
 * continuation.  Possibly this function has to be called several times
 * recursively before enough space is available.  Make sure the compiler does
 * not optimize the growth array away by storing it's address into a global
 * variable.
 */

scm_t_bits scm_i_dummy;

static void 
grow_stack (SCM cont, SCM val)
{
  scm_t_bits growth[100];

  scm_i_dummy = (scm_t_bits) growth;
  scm_dynthrow (cont, val);
}


/* Copy the continuation stack into the current stack.  Calling functions from
 * within this function is safe, since only stack frames below this function's
 * own frame are overwritten.  Thus, memcpy can be used for best performance.
 */

typedef struct {
  scm_t_contregs *continuation;
  SCM_STACKITEM *dst;
} copy_stack_data;

static void
copy_stack (void *data)
{
  copy_stack_data *d = (copy_stack_data *)data;
  memcpy (d->dst, d->continuation->stack,
	  sizeof (SCM_STACKITEM) * d->continuation->num_stack_items);
}

static void
copy_stack_and_call (scm_t_contregs *continuation, SCM val,
		     SCM_STACKITEM * dst)
{
  long delta;
  copy_stack_data data;

  delta = scm_ilength (scm_dynwinds) - scm_ilength (continuation->dynenv);
  data.continuation = continuation;
  data.dst = dst;
  scm_i_dowinds (continuation->dynenv, delta, copy_stack, &data);

  scm_last_debug_frame = continuation->dframe;

  continuation->throw_value = val;
#ifdef __ia64__
  memcpy ((void *) __libc_ia64_register_backing_store_base,
          continuation->backing_store,
          continuation->backing_store_size);
  setcontext (&continuation->ctx);
#else
  longjmp (continuation->jmpbuf, 1);
#endif
}

/* Call grow_stack until the stack space is large enough, then, as the current
 * stack frame might get overwritten, let copy_stack_and_call perform the
 * actual copying and continuation calling.
 */
static void 
scm_dynthrow (SCM cont, SCM val)
{
  scm_t_contregs *continuation = SCM_CONTREGS (cont);
  SCM_STACKITEM * dst = SCM_BASE (scm_rootcont);
  SCM_STACKITEM stack_top_element;

#if SCM_STACK_GROWS_UP
  if (dst + continuation->num_stack_items >= &stack_top_element)
    grow_stack (cont, val);
#else
  dst -= continuation->num_stack_items;
  if (dst <= &stack_top_element)
    grow_stack (cont, val);
#endif /* def SCM_STACK_GROWS_UP */

  SCM_FLUSH_REGISTER_WINDOWS;
  copy_stack_and_call (continuation, val, dst);
}


static SCM
continuation_apply (SCM cont, SCM args)
#define FUNC_NAME "continuation_apply"
{
  scm_t_contregs *continuation = SCM_CONTREGS (cont);
  scm_t_contregs *rootcont = SCM_CONTREGS (scm_rootcont);

  if (continuation->seq != rootcont->seq
      /* this base comparison isn't needed */
      || continuation->base != rootcont->base)
    {
      SCM_MISC_ERROR ("continuation from wrong top level: ~S", 
		      scm_list_1 (cont));
    }
  
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
#include "libguile/continuations.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

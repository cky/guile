/* Copyright (C) 1995,1996,1998,2000,2001,2004, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"

#include <string.h>
#include <stdio.h>

#include "libguile/async.h"
#include "libguile/debug.h"
#include "libguile/root.h"
#include "libguile/stackchk.h"
#include "libguile/smob.h"
#include "libguile/ports.h"
#include "libguile/dynwind.h"
#include "libguile/values.h"
#include "libguile/eval.h"
#include "libguile/vm.h"
#include "libguile/instructions.h"

#include "libguile/validate.h"
#include "libguile/continuations.h"



static scm_t_bits tc16_continuation;
#define SCM_CONTREGSP(x)	SCM_TYP16_PREDICATE (tc16_continuation, x)

#define SCM_CONTREGS(x)		((scm_t_contregs *) SCM_SMOB_DATA_1 (x))

#define SCM_CONTINUATION_LENGTH(x) (SCM_CONTREGS (x)->num_stack_items)
#define SCM_SET_CONTINUATION_LENGTH(x, n)\
   (SCM_CONTREGS (x)->num_stack_items = (n))
#define SCM_JMPBUF(x)		 ((SCM_CONTREGS (x))->jmpbuf)
#define SCM_DYNENV(x)		 ((SCM_CONTREGS (x))->dynenv)
#define SCM_THROW_VALUE(x)	 ((SCM_CONTREGS (x))->throw_value)
#define SCM_CONTINUATION_ROOT(x) ((SCM_CONTREGS (x))->root)   
#define SCM_DFRAME(x)		 ((SCM_CONTREGS (x))->dframe)



/* scm_i_make_continuation will return a procedure whose objcode contains an
   instruction to reinstate the continuation. Here, as in gsubr.c and smob.c, we
   define the form of that trampoline function.
 */

#ifdef WORDS_BIGENDIAN
#define OBJCODE_HEADER(main,meta) 0, 0, 0, main, 0, 0, 0, meta+8
#define META_HEADER(meta)         0, 0, 0, meta, 0, 0, 0, 0
#else
#define OBJCODE_HEADER(main,meta) main, 0, 0, 0, meta+8, 0, 0, 0
#define META_HEADER(meta)         meta, 0, 0, 0, 0,      0, 0, 0
#endif

#define ROUND_UP(len,align) (((len-1)|(align-1))+1)
#define ALIGN_PTR(type,p,align) (type*)(ROUND_UP (((scm_t_bits)p), align))

#ifdef SCM_ALIGNED
#define SCM_DECLARE_STATIC_ALIGNED_ARRAY(type, sym)\
static const type sym[]
#define SCM_STATIC_ALIGNED_ARRAY(alignment, type, sym)\
static SCM_ALIGNED (alignment) const type sym[]
#else
#define SCM_DECLARE_STATIC_ALIGNED_ARRAY(type, sym)\
static type *sym
#define SCM_STATIC_ALIGNED_ARRAY(alignment, type, sym)                  \
SCM_SNARF_INIT(sym = scm_malloc (sizeof(sym##__unaligned) + alignment - 1); \
               sym = ALIGN_PTR (type, sym, alignment);                  \
               memcpy (sym, sym##__unaligned, sizeof(sym##__unaligned));) \
static type *sym = NULL;                                                \
static const type sym##__unaligned[]
#endif

#define STATIC_OBJCODE_TAG                                      \
  SCM_PACK (scm_tc7_objcode | (SCM_F_OBJCODE_IS_STATIC << 8))

#define SCM_STATIC_OBJCODE(sym)                                         \
  SCM_DECLARE_STATIC_ALIGNED_ARRAY (scm_t_uint8, sym##__bytecode);      \
  SCM_STATIC_ALIGNED_ARRAY (8, scm_t_cell, sym##__cells) = {            \
    { STATIC_OBJCODE_TAG, SCM_PACK (sym##__bytecode) },                 \
    { SCM_BOOL_F, SCM_PACK (0) }                                        \
  };                                                                    \
  static const SCM sym = SCM_PACK (sym##__cells);                       \
  SCM_STATIC_ALIGNED_ARRAY (8, scm_t_uint8, sym##__bytecode)

  
SCM_STATIC_OBJCODE (cont_objcode) = {
  /* This code is the same as in gsubr.c, except we use smob_call instead of
     struct_call. */
  OBJCODE_HEADER (8, 19),
  /* leave args on the stack */
  /* 0 */ scm_op_object_ref, 0, /* push scm_t_contregs smob */
  /* 2 */ scm_op_continuation_call, /* and longjmp (whee) */
  /* 3 */ scm_op_nop, /* pad to 8 bytes */
  /* 4 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,
  /* 8 */

  /* We could put some meta-info to say that this proc is a continuation. Not sure
     how to do that, though. */
  META_HEADER (19),
  /* 0 */ scm_op_make_eol, /* bindings */
  /* 1 */ scm_op_make_eol, /* sources */
  /* 2 */ scm_op_make_int8, 0, scm_op_make_int8, 3, /* arity: from ip 0 to ip 3 */
  /* 6 */ scm_op_make_int8_0, /* the arity is 0 required args */
  /* 7 */ scm_op_make_int8_0, /* 0 optionals */
  /* 8 */ scm_op_make_true, /* and a rest arg */
  /* 9 */ scm_op_list, 0, 5, /* make a list of those 5 vals */
  /* 12 */ scm_op_list, 0, 1, /* and the arities will be a list of that one list */
  /* 15 */ scm_op_list, 0, 3, /* pack bindings, sources, and arities into list */
  /* 18 */ scm_op_return /* and return */
  /* 19 */
};


SCM_STATIC_OBJCODE (call_cc_objcode) = {
  /* Before Scheme's call/cc is compiled, eval.c will use this hand-coded
     call/cc. */
  OBJCODE_HEADER (8, 17),
  /* 0 */ scm_op_assert_nargs_ee, 0, 1, /* assert that nargs==1 */
  /* 3 */ scm_op_local_ref, 0, /* push the proc */
  /* 5 */ scm_op_tail_call_cc, /* and call/cc */
  /* 6 */ scm_op_nop, scm_op_nop, /* pad to 8 bytes */
  /* 8 */

  META_HEADER (17),
  /* 0 */ scm_op_make_eol, /* bindings */
  /* 1 */ scm_op_make_eol, /* sources */
  /* 2 */ scm_op_make_int8, 3, scm_op_make_int8, 6, /* arity: from ip 0 to ip 6 */
  /* 6 */ scm_op_make_int8_1, /* the arity is 0 required args */
  /* 7 */ scm_op_list, 0, 3, /* make a list of those 5 vals */
  /* 10 */ scm_op_list, 0, 1, /* and the arities will be a list of that one list */
  /* 13 */ scm_op_list, 0, 3, /* pack bindings, sources, and arities into list */
  /* 16 */ scm_op_return /* and return */
  /* 17 */
};


static SCM
make_continuation_trampoline (SCM contregs)
{
  SCM ret = scm_make_program (cont_objcode,
                              scm_c_make_vector (1, contregs),
                              SCM_BOOL_F);
  SCM_SET_CELL_WORD_0 (ret,
                       SCM_CELL_WORD_0 (ret) | SCM_F_PROGRAM_IS_CONTINUATION);

  return ret;
}
  

/* {Continuations}
 */


static int
continuation_print (SCM obj, SCM port, scm_print_state *state SCM_UNUSED)
{
  scm_t_contregs *continuation = SCM_CONTREGS (obj);

  scm_puts ("#<continuation ", port);
  scm_intprint (continuation->num_stack_items, 10, port);
  scm_puts (" @ ", port);
  scm_uintprint (SCM_SMOB_DATA_1 (obj), 16, port);
  scm_putc ('>', port);
  return 1;
}

/* this may return more than once: the first time with the escape
   procedure, then subsequently with the value to be passed to the
   continuation.  */
#define FUNC_NAME "scm_i_make_continuation"
SCM 
scm_i_make_continuation (int *first)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  SCM cont;
  scm_t_contregs *continuation;
  long stack_size;
  SCM_STACKITEM * src;

  SCM_FLUSH_REGISTER_WINDOWS;
  stack_size = scm_stack_size (thread->continuation_base);
  continuation = scm_gc_malloc (sizeof (scm_t_contregs)
				+ (stack_size - 1) * sizeof (SCM_STACKITEM),
				"continuation");
  continuation->num_stack_items = stack_size;
  continuation->dynenv = scm_i_dynwinds ();
  continuation->throw_value = SCM_EOL;
  continuation->root = thread->continuation_root;
  src = thread->continuation_base;
#if ! SCM_STACK_GROWS_UP
  src -= stack_size;
#endif
  continuation->offset = continuation->stack - src;
  memcpy (continuation->stack, src, sizeof (SCM_STACKITEM) * stack_size);
  continuation->vm_conts = scm_vm_capture_continuations ();

  SCM_NEWSMOB (cont, tc16_continuation, continuation);

  *first = !SCM_I_SETJMP (continuation->jmpbuf);
  if (*first)
    {
#ifdef __ia64__
      continuation->backing_store_size =
	(char *) scm_ia64_ar_bsp(&continuation->jmpbuf.ctx)
	-
	(char *) thread->register_backing_store_base;
      continuation->backing_store = NULL;
      continuation->backing_store = 
        scm_gc_malloc (continuation->backing_store_size,
		       "continuation backing store");
      memcpy (continuation->backing_store, 
              (void *) thread->register_backing_store_base, 
              continuation->backing_store_size);
#endif /* __ia64__ */
      return make_continuation_trampoline (cont);
    }
  else
    {
      SCM ret = continuation->throw_value;
      continuation->throw_value = SCM_BOOL_F;
      return ret;
    }
}
#undef FUNC_NAME

SCM
scm_i_call_with_current_continuation (SCM proc)
{
  static SCM call_cc = SCM_BOOL_F;

  if (scm_is_false (call_cc))
    call_cc = scm_make_program (call_cc_objcode, SCM_BOOL_F, SCM_BOOL_F);
  
  return scm_call_1 (call_cc, proc);
}

SCM
scm_i_continuation_to_frame (SCM continuation)
{
  SCM contregs;
  scm_t_contregs *cont;

  contregs = scm_c_vector_ref (scm_program_objects (continuation), 0);
  cont = SCM_CONTREGS (contregs);

  if (!scm_is_null (cont->vm_conts))
    {
      SCM vm_cont;
      struct scm_vm_cont *data;
      vm_cont = scm_cdr (scm_car (cont->vm_conts));
      data = SCM_VM_CONT_DATA (vm_cont);
      return scm_c_make_frame (vm_cont,
                               data->fp + data->reloc,
                               data->sp + data->reloc,
                               data->ip,
                               data->reloc);
    }
  else
    return SCM_BOOL_F;
}


/* {Apply}
 */

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
  scm_vm_reinstate_continuations (d->continuation->vm_conts);
#ifdef __ia64__
  SCM_I_CURRENT_THREAD->pending_rbs_continuation = d->continuation;
#endif
}

static void
copy_stack_and_call (scm_t_contregs *continuation, SCM val,
		     SCM_STACKITEM * dst)
{
  long delta;
  copy_stack_data data;

  delta = scm_ilength (scm_i_dynwinds ()) - scm_ilength (continuation->dynenv);
  data.continuation = continuation;
  data.dst = dst;
  scm_i_dowinds (continuation->dynenv, delta, copy_stack, &data);

  continuation->throw_value = val;
  SCM_I_LONGJMP (continuation->jmpbuf, 1);
}

#ifdef __ia64__
void
scm_ia64_longjmp (scm_i_jmp_buf *JB, int VAL)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (t->pending_rbs_continuation)
    {
      memcpy (t->register_backing_store_base,
	      t->pending_rbs_continuation->backing_store,
	      t->pending_rbs_continuation->backing_store_size);
      t->pending_rbs_continuation = NULL;
    }
  setcontext (&JB->ctx);
}
#endif

/* Call grow_stack until the stack space is large enough, then, as the current
 * stack frame might get overwritten, let copy_stack_and_call perform the
 * actual copying and continuation calling.
 */
static void 
scm_dynthrow (SCM cont, SCM val)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_t_contregs *continuation = SCM_CONTREGS (cont);
  SCM_STACKITEM *dst = thread->continuation_base;
  SCM_STACKITEM stack_top_element;

  if (thread->critical_section_level)
    {
      fprintf (stderr, "continuation invoked from within critical section.\n");
      abort ();
    }

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


void
scm_i_continuation_call (SCM cont, size_t n, SCM *argv)
{
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  scm_t_contregs *continuation = SCM_CONTREGS (cont);
  SCM args = SCM_EOL;
  
  /* FIXME: shuffle args on VM stack instead of heap-allocating */
  while (n--)
    args = scm_cons (argv[n], args);

  if (continuation->root != thread->continuation_root)
    scm_misc_error
      ("%continuation-call", 
       "invoking continuation would cross continuation barrier: ~A",
       scm_list_1 (cont));
  
  scm_dynthrow (cont, scm_values (args));
}

SCM
scm_i_with_continuation_barrier (scm_t_catch_body body,
				 void *body_data,
				 scm_t_catch_handler handler,
				 void *handler_data,
				 scm_t_catch_handler pre_unwind_handler,
				 void *pre_unwind_handler_data)
{
  SCM_STACKITEM stack_item;
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;
  SCM old_controot;
  SCM_STACKITEM *old_contbase;
  SCM result;

  /* Establish a fresh continuation root.  
   */
  old_controot = thread->continuation_root;
  old_contbase = thread->continuation_base;
  thread->continuation_root = scm_cons (thread->handle, old_controot);
  thread->continuation_base = &stack_item;

  /* Call FUNC inside a catch all.  This is now guaranteed to return
     directly and exactly once.
  */
  result = scm_c_catch (SCM_BOOL_T,
			body, body_data,
			handler, handler_data,
			pre_unwind_handler, pre_unwind_handler_data);

  /* Return to old continuation root.
   */
  thread->continuation_base = old_contbase;
  thread->continuation_root = old_controot;

  return result;
}

struct c_data {
  void *(*func) (void *);
  void *data;
  void *result;
};

static SCM
c_body (void *d)
{
  struct c_data *data = (struct c_data *)d;
  data->result = data->func (data->data);
  return SCM_UNSPECIFIED;
}

static SCM
c_handler (void *d, SCM tag, SCM args)
{
  struct c_data *data = (struct c_data *)d;
  data->result = NULL;
  return SCM_UNSPECIFIED;
}

void *
scm_c_with_continuation_barrier (void *(*func) (void *), void *data)
{
  struct c_data c_data;
  c_data.func = func;
  c_data.data = data;
  scm_i_with_continuation_barrier (c_body, &c_data,
				   c_handler, &c_data,
				   scm_handle_by_message_noexit, NULL);
  return c_data.result;
}

struct scm_data {
  SCM proc;
};

static SCM
scm_body (void *d)
{
  struct scm_data *data = (struct scm_data *)d;
  return scm_call_0 (data->proc);
}

static SCM
scm_handler (void *d, SCM tag, SCM args)
{
  return SCM_BOOL_F;
}

SCM_DEFINE (scm_with_continuation_barrier, "with-continuation-barrier", 1,0,0,
	    (SCM proc),
"Call @var{proc} and return its result.  Do not allow the invocation of\n"
"continuations that would leave or enter the dynamic extent of the call\n"
"to @code{with-continuation-barrier}.  Such an attempt causes an error\n"
"to be signaled.\n"
"\n"
"Throws (such as errors) that are not caught from within @var{proc} are\n"
"caught by @code{with-continuation-barrier}.  In that case, a short\n"
"message is printed to the current error port and @code{#f} is returned.\n"
"\n"
"Thus, @code{with-continuation-barrier} returns exactly once.\n")
#define FUNC_NAME s_scm_with_continuation_barrier
{
  struct scm_data scm_data;
  scm_data.proc = proc;
  return scm_i_with_continuation_barrier (scm_body, &scm_data,
					  scm_handler, &scm_data,
					  scm_handle_by_message_noexit, NULL);
}
#undef FUNC_NAME

void
scm_init_continuations ()
{
  tc16_continuation = scm_make_smob_type ("continuation", 0);
  scm_set_smob_print (tc16_continuation, continuation_print);
#include "libguile/continuations.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

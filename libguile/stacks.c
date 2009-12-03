/* Representation of stack frame debug information
 * Copyright (C) 1996,1997,2000,2001, 2006, 2007, 2008, 2009 Free Software Foundation
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
#include "libguile/eval.h"
#include "libguile/debug.h"
#include "libguile/continuations.h"
#include "libguile/struct.h"
#include "libguile/macros.h"
#include "libguile/procprop.h"
#include "libguile/modules.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vm.h" /* to capture vm stacks */
#include "libguile/frames.h" /* vm frames */

#include "libguile/validate.h"
#include "libguile/stacks.h"
#include "libguile/private-options.h"



/* {Frames and stacks}
 *
 * The stack is represented as a struct with an id slot and a tail
 * array of scm_t_info_frame structs.
 *
 * A frame is represented as a pair where the car contains a stack and
 * the cdr an inum.  The inum is an index to the first SCM value of
 * the scm_t_info_frame struct.
 *
 * Stacks
 *   Constructor
 *     make-stack
 *   Selectors
 *     stack-id
 *     stack-ref
 *   Inspector
 *     stack-length
 *
 * Frames
 *   Constructor
 *     last-stack-frame
 *   Selectors
 *     frame-number
 *     frame-source
 *     frame-procedure
 *     frame-arguments
 *     frame-previous
 *     frame-next
 *   Predicates
 *     frame-real?
 *     frame-procedure?
 *     frame-evaluating-args?
 *     frame-overflow?  */



static SCM stack_id_with_fp (SCM vmframe, SCM **fp);

/* Count number of debug info frames on a stack, beginning with VMFRAME.
 */
static long
stack_depth (SCM vmframe, SCM *fp)
{
  long n;
  /* count vmframes, skipping boot frames */
  for (; scm_is_true (vmframe) && SCM_VM_FRAME_FP (vmframe) > fp;
       vmframe = scm_c_vm_frame_prev (vmframe))
    if (!SCM_PROGRAM_IS_BOOT (scm_vm_frame_program (vmframe)))
      ++n;
  return n;
}

/* Fill the scm_t_info_frame vector IFRAME with data from N stack frames
 * starting with the first stack frame represented by VMFRAME.
 */

static scm_t_bits
read_frames (SCM vmframe, long n, scm_t_info_frame *iframes)
{
  scm_t_info_frame *iframe = iframes;

  for (; scm_is_true (vmframe);
       vmframe = scm_c_vm_frame_prev (vmframe))
    {
      if (SCM_PROGRAM_IS_BOOT (scm_vm_frame_program (vmframe)))
        /* skip boot frame */
        continue;
      else 
        {
          /* Oh dear, oh dear, oh dear. */
          iframe->flags = SCM_UNPACK (SCM_INUM0) | SCM_FRAMEF_PROC;
          iframe->source = scm_vm_frame_source (vmframe);
          iframe->proc = scm_vm_frame_program (vmframe);
          iframe->args = scm_vm_frame_arguments (vmframe);
          ++iframe;
          if (--n == 0)
            break;
        }
    }
  return iframe - iframes;  /* Number of frames actually read */
}

/* Narrow STACK by cutting away stackframes (mutatingly).
 *
 * Inner frames (most recent) are cut by advancing the frames pointer.
 * Outer frames are cut by decreasing the recorded length.
 *
 * Cut maximally INNER inner frames and OUTER outer frames using
 * the keys INNER_KEY and OUTER_KEY.
 *
 * Frames are cut away starting at the end points and moving towards
 * the center of the stack.  The key is normally compared to the
 * operator in application frames.  Frames up to and including the key
 * are cut.
 *
 * If INNER_KEY is #t a different scheme is used for inner frames:
 *
 * Frames up to but excluding the first source frame originating from
 * a user module are cut, except for possible application frames
 * between the user frame and the last system frame previously
 * encountered.
 */

static void
narrow_stack (SCM stack, long inner, SCM inner_key, long outer, SCM outer_key)
{
  scm_t_stack *s = SCM_STACK (stack);
  unsigned long int i;
  long n = s->length;
  
  /* Cut inner part. */
  if (scm_is_eq (inner_key, SCM_BOOL_T))
    {
      /* Cut all frames up to user module code */
      for (i = 0; inner; ++i, --inner)
        ;
    }
  else
    /* Use standard cutting procedure. */
    {
      for (i = 0; inner; --inner)
	if (scm_is_eq (s->frames[i++].proc, inner_key))
	  break;
    }
  s->frames = &s->frames[i];
  n -= i;

  /* Cut outer part. */
  for (; n && outer; --outer)
    if (scm_is_eq (s->frames[--n].proc, outer_key))
      break;

  s->length = n;
}



/* Stacks
 */

SCM scm_stack_type;

SCM_DEFINE (scm_stack_p, "stack?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a calling stack.")
#define FUNC_NAME s_scm_stack_p
{
  return scm_from_bool(SCM_STACKP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_stack, "make-stack", 1, 0, 1, 
            (SCM obj, SCM args),
	    "Create a new stack. If @var{obj} is @code{#t}, the current\n"
	    "evaluation stack is used for creating the stack frames,\n"
	    "otherwise the frames are taken from @var{obj} (which must be\n"
	    "either a debug object or a continuation).\n\n"
	    "@var{args} should be a list containing any combination of\n"
	    "integer, procedure and @code{#t} values.\n\n"
	    "These values specify various ways of cutting away uninteresting\n"
	    "stack frames from the top and bottom of the stack that\n"
	    "@code{make-stack} returns.  They come in pairs like this:\n"
	    "@code{(@var{inner_cut_1} @var{outer_cut_1} @var{inner_cut_2}\n"
	    "@var{outer_cut_2} @dots{})}.\n\n"
	    "Each @var{inner_cut_N} can be @code{#t}, an integer, or a\n"
	    "procedure.  @code{#t} means to cut away all frames up to but\n"
	    "excluding the first user module frame.  An integer means to cut\n"
	    "away exactly that number of frames.  A procedure means to cut\n"
	    "away all frames up to but excluding the application frame whose\n"
	    "procedure matches the specified one.\n\n"
	    "Each @var{outer_cut_N} can be an integer or a procedure.  An\n"
	    "integer means to cut away that number of frames.  A procedure\n"
	    "means to cut away frames down to but excluding the application\n"
	    "frame whose procedure matches the specified one.\n\n"
	    "If the @var{outer_cut_N} of the last pair is missing, it is\n"
	    "taken as 0.")
#define FUNC_NAME s_scm_make_stack
{
  long n, size;
  int maxp;
  scm_t_info_frame *iframe;
  SCM vmframe;
  SCM stack;
  SCM id, *id_fp;
  SCM inner_cut, outer_cut;

  /* Extract a pointer to the innermost frame of whatever object
     scm_make_stack was given.  */
  if (scm_is_eq (obj, SCM_BOOL_T))
    {
      struct scm_vm *vp = SCM_VM_DATA (scm_the_vm ());
      vmframe = scm_c_make_vm_frame (scm_the_vm (), vp->fp, vp->sp, vp->ip, 0);
    }
  else if (SCM_VM_FRAME_P (obj))
    vmframe = obj;
  else if (SCM_CONTINUATIONP (obj))
    {
      scm_t_contregs *cont = SCM_CONTREGS (obj);
      if (!scm_is_null (cont->vm_conts))
        { SCM vm_cont;
          struct scm_vm_cont *data;
          vm_cont = scm_cdr (scm_car (cont->vm_conts));
          data = SCM_VM_CONT_DATA (vm_cont);
          vmframe = scm_c_make_vm_frame (vm_cont,
                                         data->fp + data->reloc,
                                         data->sp + data->reloc,
                                         data->ip,
                                         data->reloc);
        } else 
          vmframe = SCM_BOOL_F;
    }
  else
    {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, obj);
      /* not reached */
    }

  if (scm_is_false (vmframe))
    return SCM_BOOL_F;

  /* Get ID of the stack corresponding to the given frame. */
  id = stack_id_with_fp (vmframe, &id_fp);

  /* Count number of frames.  Also get stack id tag and check whether
     there are more stackframes than we want to record
     (SCM_BACKTRACE_MAXDEPTH). */
  id = SCM_BOOL_F;
  maxp = 0;
  n = stack_depth (vmframe, id_fp);
  /* FIXME: redo maxp? */
  size = n * SCM_FRAME_N_SLOTS;

  /* Make the stack object. */
  stack = scm_make_struct (scm_stack_type, scm_from_long (size), SCM_EOL);
  SCM_STACK (stack) -> id = id;
  iframe = &SCM_STACK (stack) -> tail[0];
  SCM_STACK (stack) -> frames = iframe;
  SCM_STACK (stack) -> length = n;

  /* Translate the current chain of stack frames into debugging information. */
  n = read_frames (vmframe, n, iframe);
  if (n != SCM_STACK (stack)->length)
    {
      scm_puts ("warning: stack count incorrect!\n", scm_current_error_port ());
      SCM_STACK (stack)->length = n;
    }

  /* Narrow the stack according to the arguments given to scm_make_stack. */
  SCM_VALIDATE_REST_ARGUMENT (args);
  while (n > 0 && !scm_is_null (args))
    {
      inner_cut = SCM_CAR (args);
      args = SCM_CDR (args);
      if (scm_is_null (args)) 
	{
	  outer_cut = SCM_INUM0;
	} 
      else
	{
	  outer_cut = SCM_CAR (args);
	  args = SCM_CDR (args);
	}
      
      narrow_stack (stack,
		    scm_is_integer (inner_cut) ? scm_to_int (inner_cut) : n,
		    scm_is_integer (inner_cut) ? 0 : inner_cut,
		    scm_is_integer (outer_cut) ? scm_to_int (outer_cut) : n,
		    scm_is_integer (outer_cut) ? 0 : outer_cut);

      n = SCM_STACK (stack) -> length;
    }
  
  if (n > 0 && maxp)
    iframe[n - 1].flags |= SCM_FRAMEF_OVERFLOW;

  if (n > 0)
    return stack;
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_stack_id, "stack-id", 1, 0, 0, 
            (SCM stack),
	    "Return the identifier given to @var{stack} by @code{start-stack}.")
#define FUNC_NAME s_scm_stack_id
{
  SCM vmframe, *id_fp;
  
  if (scm_is_eq (stack, SCM_BOOL_T))
    {
      struct scm_vm *vp = SCM_VM_DATA (scm_the_vm ());
      vmframe = scm_c_make_vm_frame (scm_the_vm (), vp->fp, vp->sp, vp->ip, 0);
    }
  else if (SCM_VM_FRAME_P (stack))
    vmframe = stack;
  else if (SCM_CONTINUATIONP (stack))
    {
      scm_t_contregs *cont = SCM_CONTREGS (stack);
      if (!scm_is_null (cont->vm_conts))
        { SCM vm_cont;
          struct scm_vm_cont *data;
          vm_cont = scm_cdr (scm_car (cont->vm_conts));
          data = SCM_VM_CONT_DATA (vm_cont);
          vmframe = scm_c_make_vm_frame (vm_cont,
                                         data->fp + data->reloc,
                                         data->sp + data->reloc,
                                         data->ip,
                                         data->reloc);
        } else 
          vmframe = SCM_BOOL_F;
    }
  else
    {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, stack);
      /* not reached */
    }

  return stack_id_with_fp (vmframe, &id_fp);
}
#undef FUNC_NAME

static SCM
stack_id_with_fp (SCM vmframe, SCM **fp)
{
  SCM holder = SCM_VM_FRAME_STACK_HOLDER (vmframe);

  if (SCM_VM_CONT_P (holder))
    {
      *fp = NULL;
      return SCM_BOOL_F;
    }
  else
    {
      *fp = NULL;
      return SCM_BOOL_F;
    }
}

SCM_DEFINE (scm_stack_ref, "stack-ref", 2, 0, 0,
            (SCM stack, SCM index),
	    "Return the @var{index}'th frame from @var{stack}.")
#define FUNC_NAME s_scm_stack_ref
{
  unsigned long int c_index;

  SCM_VALIDATE_STACK (1, stack);
  c_index = scm_to_unsigned_integer (index, 0, SCM_STACK_LENGTH(stack)-1);
  return scm_cons (stack, index);
}
#undef FUNC_NAME

SCM_DEFINE (scm_stack_length, "stack-length", 1, 0, 0, 
	    (SCM stack),
	    "Return the length of @var{stack}.")
#define FUNC_NAME s_scm_stack_length
{
  SCM_VALIDATE_STACK (1, stack);
  return scm_from_int (SCM_STACK_LENGTH (stack));
}
#undef FUNC_NAME

/* Frames
 */

SCM_DEFINE (scm_frame_p, "frame?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a stack frame.")
#define FUNC_NAME s_scm_frame_p
{
  return scm_from_bool(SCM_FRAMEP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_number, "frame-number", 1, 0, 0, 
	    (SCM frame),
	    "Return the frame number of @var{frame}.")
#define FUNC_NAME s_scm_frame_number
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_int (SCM_FRAME_NUMBER (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_source, "frame-source", 1, 0, 0, 
	    (SCM frame),
	    "Return the source of @var{frame}.")
#define FUNC_NAME s_scm_frame_source
{
  SCM_VALIDATE_FRAME (1, frame);
  return SCM_FRAME_SOURCE (frame);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_procedure, "frame-procedure", 1, 0, 0, 
	    (SCM frame),
	    "Return the procedure for @var{frame}, or @code{#f} if no\n"
	    "procedure is associated with @var{frame}.")
#define FUNC_NAME s_scm_frame_procedure
{
  SCM_VALIDATE_FRAME (1, frame);
  return (SCM_FRAME_PROC_P (frame)
	  ? SCM_FRAME_PROC (frame)
	  : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_arguments, "frame-arguments", 1, 0, 0, 
	    (SCM frame),
	    "Return the arguments of @var{frame}.")
#define FUNC_NAME s_scm_frame_arguments
{
  SCM_VALIDATE_FRAME (1, frame);
  return SCM_FRAME_ARGS (frame);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_previous, "frame-previous", 1, 0, 0, 
	    (SCM frame),
	    "Return the previous frame of @var{frame}, or @code{#f} if\n"
	    "@var{frame} is the first frame in its stack.")
#define FUNC_NAME s_scm_frame_previous
{
  unsigned long int n;
  SCM_VALIDATE_FRAME (1, frame);
  n = scm_to_ulong (SCM_CDR (frame)) + 1;
  if (n >= SCM_STACK_LENGTH (SCM_CAR (frame)))
    return SCM_BOOL_F;
  else
    return scm_cons (SCM_CAR (frame), scm_from_ulong (n));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_next, "frame-next", 1, 0, 0, 
           (SCM frame),
	    "Return the next frame of @var{frame}, or @code{#f} if\n"
	    "@var{frame} is the last frame in its stack.")
#define FUNC_NAME s_scm_frame_next
{
  unsigned long int n;
  SCM_VALIDATE_FRAME (1, frame);
  n = scm_to_ulong (SCM_CDR (frame));
  if (n == 0)
    return SCM_BOOL_F;
  else
    return scm_cons (SCM_CAR (frame), scm_from_ulong (n - 1));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_real_p, "frame-real?", 1, 0, 0, 
	    (SCM frame),
	    "Return @code{#t} if @var{frame} is a real frame.")
#define FUNC_NAME s_scm_frame_real_p
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_bool(SCM_FRAME_REAL_P (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_procedure_p, "frame-procedure?", 1, 0, 0, 
	    (SCM frame),
	    "Return @code{#t} if a procedure is associated with @var{frame}.")
#define FUNC_NAME s_scm_frame_procedure_p
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_bool(SCM_FRAME_PROC_P (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_evaluating_args_p, "frame-evaluating-args?", 1, 0, 0, 
	    (SCM frame),
	    "Return @code{#t} if @var{frame} contains evaluated arguments.")
#define FUNC_NAME s_scm_frame_evaluating_args_p
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_bool(SCM_FRAME_EVAL_ARGS_P (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_overflow_p, "frame-overflow?", 1, 0, 0, 
	    (SCM frame),
	    "Return @code{#t} if @var{frame} is an overflow frame.")
#define FUNC_NAME s_scm_frame_overflow_p
{
  SCM_VALIDATE_FRAME (1, frame);
  return scm_from_bool(SCM_FRAME_OVERFLOW_P (frame));
}
#undef FUNC_NAME



void
scm_init_stacks ()
{
  scm_stack_type =
    scm_permanent_object
    (scm_make_vtable (scm_from_locale_string (SCM_STACK_LAYOUT),
                      SCM_UNDEFINED));
  scm_set_struct_vtable_name_x (scm_stack_type,
				scm_from_locale_symbol ("stack"));
#include "libguile/stacks.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

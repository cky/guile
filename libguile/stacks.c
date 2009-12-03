/* A stack holds a frame chain
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



/* {Stacks}
 *
 * The stack is represented as a struct that holds a frame. The frame itself is
 * linked to the next frame, or #f.
 *
 * Stacks
 *   Constructor
 *     make-stack
 *   Selectors
 *     stack-id
 *     stack-ref
 *   Inspector
 *     stack-length
 */



static SCM stack_id_with_fp (SCM frame, SCM **fp);

/* Count number of debug info frames on a stack, beginning with FRAME.
 */
static long
stack_depth (SCM frame, SCM *fp)
{
  long n;
  /* count frames, skipping boot frames */
  for (; scm_is_true (frame) && SCM_VM_FRAME_FP (frame) > fp;
       frame = scm_c_frame_prev (frame))
    if (!SCM_PROGRAM_IS_BOOT (scm_frame_procedure (frame)))
      ++n;
  return n;
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
  unsigned long int len;
  SCM frame;
  
  len = SCM_STACK_LENGTH (stack);
  frame = SCM_STACK_FRAME (stack);

  /* Cut inner part. */
  if (scm_is_eq (inner_key, SCM_BOOL_T))
    {
      /* Cut specified number of frames. */
      for (; inner && len; --inner)
        {
          len--;
          frame = scm_c_frame_prev (frame);
        }
    }
  else
    {
      /* Cut until the given procedure is seen. */
      for (; inner && len ; --inner)
        {
          SCM proc = scm_frame_procedure (frame);
          len--;
          frame = scm_c_frame_prev (frame);
          if (scm_is_eq (proc, inner_key))
            break;
        }
    }

  SCM_SET_STACK_LENGTH (stack, len);
  SCM_SET_STACK_FRAME (stack, frame);

  /* Cut outer part. */
  for (; outer && len ; --outer)
    {
      frame = scm_stack_ref (stack, scm_from_long (len - 1));
      len--;
      if (scm_is_eq (scm_frame_procedure (frame), outer_key))
        break;
    }

  SCM_SET_STACK_LENGTH (stack, len);
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
  long n;
  int maxp;
  SCM frame;
  SCM stack;
  SCM id, *id_fp;
  SCM inner_cut, outer_cut;

  /* Extract a pointer to the innermost frame of whatever object
     scm_make_stack was given.  */
  if (scm_is_eq (obj, SCM_BOOL_T))
    {
      SCM cont;
      struct scm_vm_cont *c;

      cont = scm_cdar (scm_vm_capture_continuations ());
      c = SCM_VM_CONT_DATA (cont);

      frame = scm_c_make_frame (cont, c->fp + c->reloc,
                                c->sp + c->reloc, c->ip,
                                c->reloc);
    }
  else if (SCM_VM_FRAME_P (obj))
    frame = obj;
  else if (SCM_CONTINUATIONP (obj))
    {
      scm_t_contregs *cont = SCM_CONTREGS (obj);
      if (!scm_is_null (cont->vm_conts))
        { SCM vm_cont;
          struct scm_vm_cont *data;
          vm_cont = scm_cdr (scm_car (cont->vm_conts));
          data = SCM_VM_CONT_DATA (vm_cont);
          frame = scm_c_make_frame (vm_cont,
                                    data->fp + data->reloc,
                                    data->sp + data->reloc,
                                    data->ip,
                                    data->reloc);
        } else 
        frame = SCM_BOOL_F;
    }
  else
    {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, obj);
      /* not reached */
    }

  if (scm_is_false (frame))
    return SCM_BOOL_F;

  /* Get ID of the stack corresponding to the given frame. */
  id = stack_id_with_fp (frame, &id_fp);

  /* Count number of frames.  Also get stack id tag and check whether
     there are more stackframes than we want to record
     (SCM_BACKTRACE_MAXDEPTH). */
  id = SCM_BOOL_F;
  maxp = 0;
  n = stack_depth (frame, id_fp);

  /* Make the stack object. */
  stack = scm_make_struct (scm_stack_type, SCM_INUM0, SCM_EOL);
  SCM_SET_STACK_LENGTH (stack, n);
  SCM_SET_STACK_ID (stack, id);
  SCM_SET_STACK_FRAME (stack, frame);
  
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

      n = SCM_STACK_LENGTH (stack);
    }
  
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
  SCM frame, *id_fp;
  
  if (scm_is_eq (stack, SCM_BOOL_T))
    {
      struct scm_vm *vp = SCM_VM_DATA (scm_the_vm ());
      frame = scm_c_make_frame (scm_the_vm (), vp->fp, vp->sp, vp->ip, 0);
    }
  else if (SCM_VM_FRAME_P (stack))
    frame = stack;
  else if (SCM_CONTINUATIONP (stack))
    {
      scm_t_contregs *cont = SCM_CONTREGS (stack);
      if (!scm_is_null (cont->vm_conts))
        { SCM vm_cont;
          struct scm_vm_cont *data;
          vm_cont = scm_cdr (scm_car (cont->vm_conts));
          data = SCM_VM_CONT_DATA (vm_cont);
          frame = scm_c_make_frame (vm_cont,
                                    data->fp + data->reloc,
                                    data->sp + data->reloc,
                                    data->ip,
                                    data->reloc);
        } else 
        frame = SCM_BOOL_F;
    }
  else
    {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, stack);
      /* not reached */
    }

  return stack_id_with_fp (frame, &id_fp);
}
#undef FUNC_NAME

static SCM
stack_id_with_fp (SCM frame, SCM **fp)
{
  SCM holder = SCM_VM_FRAME_STACK_HOLDER (frame);

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
  SCM frame;

  SCM_VALIDATE_STACK (1, stack);
  c_index = scm_to_unsigned_integer (index, 0, SCM_STACK_LENGTH(stack)-1);
  frame = SCM_STACK_FRAME (stack);
  while (c_index--)
    {
      frame = scm_c_frame_prev (frame);
      while (SCM_PROGRAM_IS_BOOT (scm_frame_procedure (frame)))
        frame = scm_c_frame_prev (frame);
    }
  return frame;
}
#undef FUNC_NAME

SCM_DEFINE (scm_stack_length, "stack-length", 1, 0, 0, 
	    (SCM stack),
	    "Return the length of @var{stack}.")
#define FUNC_NAME s_scm_stack_length
{
  SCM_VALIDATE_STACK (1, stack);
  return scm_from_long (SCM_STACK_LENGTH (stack));
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

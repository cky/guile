/* A stack holds a frame chain
 * Copyright (C) 1996,1997,2000,2001, 2006, 2007, 2008, 2009, 2010, 2011 Free Software Foundation
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
#include "libguile/control.h"
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


static SCM scm_sys_stacks;


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



/* Count number of debug info frames on a stack, beginning with FRAME.
 */
static long
stack_depth (SCM frame)
{
  long n = 0;
  /* count frames, skipping boot frames */
  for (; scm_is_true (frame); frame = scm_frame_previous (frame))
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

static SCM
find_prompt (SCM key)
{
  SCM winds;
  for (winds = scm_i_dynwinds (); scm_is_pair (winds); winds = scm_cdr (winds))
    {
      SCM elt = scm_car (winds);
      if (SCM_PROMPT_P (elt) && SCM_PROMPT_TAG (elt) == key)
        return elt;
    }
  scm_misc_error ("make-stack", "Prompt tag not found while narrowing stack",
                  scm_list_1 (key));
  return SCM_BOOL_F; /* not reached */
}

static void
narrow_stack (SCM stack, long inner, SCM inner_key, long outer, SCM outer_key)
{
  unsigned long int len;
  SCM frame;
  
  len = SCM_STACK_LENGTH (stack);
  frame = SCM_STACK_FRAME (stack);

  /* Cut inner part. */
  if (scm_is_true (scm_procedure_p (inner_key)))
    {
      /* Cut until the given procedure is seen. */
      for (; inner && len ; --inner)
        {
          SCM proc = scm_frame_procedure (frame);
          len--;
          frame = scm_frame_previous (frame);
          if (scm_is_eq (proc, inner_key))
            break;
        }
    }
  else if (scm_is_symbol (inner_key))
    {
      /* Cut until the given prompt tag is seen. FIXME, assumes prompt tags are
         symbols. */
      SCM prompt = find_prompt (inner_key);
      for (; len; len--, frame = scm_frame_previous (frame))
        if (SCM_PROMPT_REGISTERS (prompt)->fp
            == SCM_VM_FRAME_FP (frame) - SCM_VM_FRAME_OFFSET (frame))
          break;
    }
  else
    {
      /* Cut specified number of frames. */
      for (; inner && len; --inner)
        {
          len--;
          frame = scm_frame_previous (frame);
        }
    }

  SCM_SET_STACK_LENGTH (stack, len);
  SCM_SET_STACK_FRAME (stack, frame);

  /* Cut outer part. */
  if (scm_is_true (scm_procedure_p (outer_key)))
    {
      /* Cut until the given procedure is seen. */
      for (; outer && len ; --outer)
        {
          frame = scm_stack_ref (stack, scm_from_long (len - 1));
          len--;
          if (scm_is_eq (scm_frame_procedure (frame), outer_key))
            break;
        }
    }
  else if (scm_is_symbol (outer_key))
    {
      /* Cut until the given prompt tag is seen. FIXME, assumes prompt tags are
         symbols. */
      SCM prompt = find_prompt (outer_key);
      while (len)
        {
          frame = scm_stack_ref (stack, scm_from_long (len - 1));
          len--;
          if (SCM_PROMPT_REGISTERS (prompt)->fp
              == SCM_VM_FRAME_FP (frame) - SCM_VM_FRAME_OFFSET (frame))
            break;
        }
    }
  else
    {
      /* Cut specified number of frames. */
      for (; outer && len ; --outer)
        {
          frame = scm_stack_ref (stack, scm_from_long (len - 1));
          len--;
        }
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
	    "a continuation or a frame object).\n"
            "\n"
	    "@var{args} should be a list containing any combination of\n"
	    "integer, procedure, prompt tag and @code{#t} values.\n"
            "\n"
	    "These values specify various ways of cutting away uninteresting\n"
	    "stack frames from the top and bottom of the stack that\n"
	    "@code{make-stack} returns.  They come in pairs like this:\n"
	    "@code{(@var{inner_cut_1} @var{outer_cut_1} @var{inner_cut_2}\n"
	    "@var{outer_cut_2} @dots{})}.\n"
            "\n"
	    "Each @var{inner_cut_N} can be @code{#t}, an integer, a prompt\n"
            "tag, or a procedure.  @code{#t} means to cut away all frames up\n"
            "to but excluding the first user module frame.  An integer means\n"
            "to cut away exactly that number of frames.  A prompt tag means\n"
            "to cut away all frames that are inside a prompt with the given\n"
            "tag. A procedure means to cut away all frames up to but\n"
            "excluding the application frame whose procedure matches the\n"
            "specified one.\n"
            "\n"
	    "Each @var{outer_cut_N} can be an integer, a prompt tag, or a\n"
            "procedure.  An integer means to cut away that number of frames.\n"
            "A prompt tag means to cut away all frames that are outside a\n"
            "prompt with the given tag. A procedure means to cut away\n"
            "frames down to but excluding the application frame whose\n"
            "procedure matches the specified one.\n"
            "\n"
	    "If the @var{outer_cut_N} of the last pair is missing, it is\n"
	    "taken as 0.")
#define FUNC_NAME s_scm_make_stack
{
  long n;
  SCM frame;
  SCM stack;
  SCM inner_cut, outer_cut;

  /* Extract a pointer to the innermost frame of whatever object
     scm_make_stack was given.  */
  if (scm_is_eq (obj, SCM_BOOL_T))
    {
      SCM cont;
      struct scm_vm_cont *c;

      cont = scm_i_vm_capture_continuation (scm_the_vm ());
      c = SCM_VM_CONT_DATA (cont);

      frame = scm_c_make_frame (cont, c->fp + c->reloc,
                                c->sp + c->reloc, c->ra,
                                c->reloc);
    }
  else if (SCM_VM_FRAME_P (obj))
    frame = obj;
  else if (SCM_CONTINUATIONP (obj))
    /* FIXME: Narrowing to prompt tags should narrow with respect to the prompts
       that were in place when the continuation was captured. */
    frame = scm_i_continuation_to_frame (obj);
  else
    {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, obj);
      /* not reached */
    }

  /* FIXME: is this even possible? */
  if (scm_is_true (frame)
      && SCM_PROGRAM_IS_BOOT (scm_frame_procedure (frame)))
    frame = scm_frame_previous (frame);
  
  if (scm_is_false (frame))
    return SCM_BOOL_F;

  /* Count number of frames.  Also get stack id tag and check whether
     there are more stackframes than we want to record
     (SCM_BACKTRACE_MAXDEPTH). */
  n = stack_depth (frame);

  /* Make the stack object. */
  stack = scm_make_struct (scm_stack_type, SCM_INUM0, SCM_EOL);
  SCM_SET_STACK_LENGTH (stack, n);
  SCM_SET_STACK_ID (stack, scm_stack_id (obj));
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
		    scm_is_integer (inner_cut) ? SCM_BOOL_T : inner_cut,
		    scm_is_integer (outer_cut) ? scm_to_int (outer_cut) : n,
		    scm_is_integer (outer_cut) ? SCM_BOOL_T : outer_cut);

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
  if (scm_is_eq (stack, SCM_BOOL_T)
      /* FIXME: frame case assumes frame still live on the stack, and no
         intervening start-stack. Hmm... */
      || SCM_VM_FRAME_P (stack))
    {
      /* Fetch most recent start-stack tag. */
      SCM stacks = scm_fluid_ref (scm_sys_stacks);
      return scm_is_pair (stacks) ? scm_caar (stacks) : SCM_BOOL_F;
    }
  else if (SCM_CONTINUATIONP (stack))
    /* FIXME: implement me */
    return SCM_BOOL_F;
  else
    {
      SCM_WRONG_TYPE_ARG (SCM_ARG1, stack);
      /* not reached */
    }
}
#undef FUNC_NAME

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
    frame = scm_frame_previous (frame);
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
  scm_sys_stacks = scm_make_fluid ();
  scm_c_define ("%stacks", scm_sys_stacks);
  
  scm_stack_type = scm_make_vtable (scm_from_locale_string (SCM_STACK_LAYOUT),
                                    SCM_UNDEFINED);
  scm_set_struct_vtable_name_x (scm_stack_type,
				scm_from_latin1_symbol ("stack"));
#include "libguile/stacks.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

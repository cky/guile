/* Representation of stack frame debug information
 * Copyright (C) 1996 Mikael Djurfeldt
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
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
 */


#include <stdio.h>
#include "_scm.h"
#include "debug.h"
#include "continuations.h"
#include "struct.h"

#include "stacks.h"


/* {Frames and stacks}
 *
 * The debugging evaluator creates debug frames on the stack.  These
 * are linked from the innermost frame and outwards.  The last frame
 * created can always be accessed as SCM_LAST_DEBUG_FRAME.
 * Continuations contain a pointer to the innermost debug frame on the
 * continuation stack.
 *
 * Each debug frame contains a set of flags and information about one
 * or more stack frames.  The case of multiple frames occurs due to
 * tail recursion.  The maximal number of stack frames which can be
 * recorded in one debug frame can be set dynamically with the debug
 * option FRAMES.
 *
 * Stack frame information is of two types: eval information (the
 * expression being evaluated and its environment) and apply
 * information (the procedure being applied and its arguments).  A
 * stack frame normally corresponds to an eval/apply pair, but macros
 * and special forms (which are implemented as macros in Guile) only
 * have eval information and apply calls leads to apply only frames.
 *
 * Since we want to record the total stack information and later
 * manipulate this data at the scheme level in the debugger, we need
 * to transform it into a new representation.  In the following code
 * section you'll find the functions implementing this data type.
 *
 * Representation:
 *
 * The stack is represented as a struct with an id slot and a tail
 * array of scm_info_frame structs.
 *
 * A frame is represented as a pair where the car contains a stack and
 * the cdr an inum.  The inum is an index to the first SCM value of
 * the scm_info_frame struct.
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



/* Some auxiliary functions for reading debug frames off the stack.
 */

/* Count number of debug info frames on a stack, beginning with
 * DFRAME.  OFFSET is used for relocation of pointers when the stack
 * is read from a continuation.
 */
static int stack_depth SCM_P ((scm_debug_frame *dframe, long offset, SCM *id, int *maxp));
static int
stack_depth (dframe, offset, id, maxp)
     scm_debug_frame *dframe;
     long offset;
     SCM *id;
     int *maxp;
{
  int n, size;
  int max_depth = SCM_BACKTRACE_MAXDEPTH;
  scm_debug_info *info;
  for (n = 0;
       dframe && !SCM_VOIDFRAMEP (*dframe) && n < max_depth;
       dframe = (scm_debug_frame *) ((SCM_STACKITEM *) dframe->prev + offset))
    {
      if (SCM_EVALFRAMEP (*dframe))
	{
	  size = dframe->status & SCM_MAX_FRAME_SIZE;
	  info = (scm_debug_info *) (*((SCM_STACKITEM **) &dframe->vect[size])
				     + offset);
	  n += (info - dframe->vect) / 2 + 1;
	  /* Data in the apply part of an eval info frame comes from previous
	     stack frame if the scm_debug_info vector is overflowed. */
	  if ((((info - dframe->vect) & 1) == 0)
	      && SCM_OVERFLOWP (*dframe)
	      && !SCM_UNBNDP (info[1].a.proc))
	    ++n;
	}
      else
	++n;
    }
  if (dframe && SCM_VOIDFRAMEP (*dframe))
    *id = dframe->vect[0].id;
  else if (dframe)
    *maxp = 1;
  return n;
}

/* Read debug info from DFRAME into IFRAME.
 */
static void read_frame SCM_P ((scm_debug_frame *dframe, long offset, scm_info_frame *iframe));
static void
read_frame (dframe, offset, iframe)
     scm_debug_frame *dframe;
     long offset;
     scm_info_frame *iframe;
{
  SCM flags = SCM_INUM0;
  int size;
  scm_debug_info *info;
  if (SCM_EVALFRAMEP (*dframe))
    {
      size = dframe->status & SCM_MAX_FRAME_SIZE;
      info = (scm_debug_info *) (*((SCM_STACKITEM **) &dframe->vect[size])
				 + offset);
      if ((info - dframe->vect) & 1)
	{
	  /* Debug.vect ends with apply info. */
	  --info;
	  if (info[1].a.proc != SCM_UNDEFINED)
	    {
	      flags |= SCM_FRAMEF_PROC;
	      iframe->proc = info[1].a.proc;
	      iframe->args = info[1].a.args;
	      if (!SCM_ARGS_READY_P (*dframe))
		flags |= SCM_FRAMEF_EVAL_ARGS;
	    }
	}
      iframe->source = scm_make_memoized (info[0].e.exp, info[0].e.env);
    }
  else
    {
      flags |= SCM_FRAMEF_PROC;
      iframe->proc = dframe->vect[0].a.proc;
      iframe->args = dframe->vect[0].a.args;
    }
  iframe->flags = flags;
}

/* Fill the scm_info_frame vector IFRAME with data from N stack frames
 * starting with the first stack frame represented by debug frame
 * DFRAME.
 */

#define NEXT_FRAME(iframe, n, quit) \
{ \
  ++iframe; \
  if (--n == 0) \
    goto quit; \
} \


static void read_frames SCM_P ((scm_debug_frame *dframe, long offset, int nframes, scm_info_frame *iframes));
static void
read_frames (dframe, offset, n, iframes)
     scm_debug_frame *dframe;
     long offset;
     int n;
     scm_info_frame *iframes;
{
  int size;
  scm_info_frame *iframe = iframes;
  scm_debug_info *info;
  
  for (;
       dframe && !SCM_VOIDFRAMEP (*dframe) && n > 0;
       dframe = (scm_debug_frame *) ((SCM_STACKITEM *) dframe->prev + offset))
    {
      read_frame (dframe, offset, iframe);
      if (SCM_EVALFRAMEP (*dframe))
	{
	  size = dframe->status & SCM_MAX_FRAME_SIZE;
	  info = (scm_debug_info *) (*((SCM_STACKITEM **) &dframe->vect[size])
				     + offset);
	  if ((info - dframe->vect) & 1)
	    --info;
	  /* Data in the apply part of an eval info frame comes from
	     previous stack frame if the scm_debug_info vector is overflowed. */
	  else if (SCM_OVERFLOWP (*dframe)
		   && !SCM_UNBNDP (info[1].a.proc))
	    {
	      NEXT_FRAME (iframe, n, quit);
	      iframe->flags = SCM_INUM0 | SCM_FRAMEF_PROC;
	      iframe->proc = info[1].a.proc;
	      iframe->args = info[1].a.args;
	    }
	  if (SCM_OVERFLOWP (*dframe))
	    iframe->flags |= SCM_FRAMEF_OVERFLOW;
	  info -= 2;
	  NEXT_FRAME (iframe, n, quit);
	  while (info >= dframe->vect)
	    {
	      if (!SCM_UNBNDP (info[1].a.proc))
		{
		  iframe->flags = SCM_INUM0 | SCM_FRAMEF_PROC;
		  iframe->proc = info[1].a.proc;
		  iframe->args = info[1].a.args;
		}
	      else
		iframe->flags = SCM_INUM0;
	      iframe->source = scm_make_memoized (info[0].e.exp,
						  info[0].e.env);
	      info -= 2;
	      NEXT_FRAME (iframe, n, quit);
	    }
	}
      else
	{
	  NEXT_FRAME (iframe, n, quit);
	}
    quit:
      if (iframe > iframes)
	(iframe - 1) -> flags |= SCM_FRAMEF_REAL;
    }
}

static void narrow_stack SCM_P ((SCM stack, int inner, SCM inner_key, int outer, SCM outer_key));

static void
narrow_stack (stack, inner, inner_key, outer, outer_key)
     SCM stack;
     int inner;
     SCM inner_key;
     int outer;
     SCM outer_key;
{
  scm_stack *s = SCM_STACK (stack);
  int i;
  int n = s->length;
  
  /* Cut inner part. */
  for (i = 0; inner; --inner)
    if (s->frames[i++].proc == inner_key)
      break;
  s->frames = &s->frames[i];
  n -= i;

  /* Cut outer part. */
  for (; n && outer; --outer)
    if (s->frames[--n].proc == outer_key)
      break;

  s->length = n;
}



/* Stacks
 */

SCM scm_stack_type;

SCM_PROC (s_stack_p, "stack?", 1, 0, 0, scm_stack_p);
SCM
scm_stack_p (obj)
     SCM obj;
{
  return SCM_NIMP (obj) && SCM_STACKP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC (s_make_stack, "make-stack", 0, 0, 1, scm_make_stack);
SCM
scm_make_stack (args)
     SCM args;
{
  int n, maxp, size;
  scm_debug_frame *dframe;
  scm_info_frame *iframe;
  long offset = 0;
  SCM stack, id;
  SCM obj, inner_cut, outer_cut;

  SCM_ASSERT (SCM_NIMP (args) && SCM_CONSP (args), SCM_WNA, args, s_make_stack);
  obj = SCM_CAR (args);
  args = SCM_CDR (args);

  /* Extract a pointer to the innermost frame of whatever object
     scm_make_stack was given.  */
  if (obj == SCM_BOOL_T)
    dframe = scm_last_debug_frame;
  else
    {
      SCM_ASSERT (SCM_NIMP (obj), obj, SCM_ARG1, s_make_stack);
      if (SCM_DEBUGOBJP (obj))
	dframe = (scm_debug_frame *) SCM_DEBUGOBJ_FRAME (obj);
      else if (scm_tc7_contin == SCM_TYP7 (obj))
	{
	  offset = ((SCM_STACKITEM *) (SCM_CHARS (obj) + sizeof (scm_contregs))
		    - SCM_BASE (obj));
#ifndef STACK_GROWS_UP
	  offset += SCM_LENGTH (obj);
#endif
	  dframe = (scm_debug_frame *) ((SCM_STACKITEM *) SCM_DFRAME (obj)
				       + offset);
	}
      else scm_wta (obj, (char *) SCM_ARG1, s_make_stack);
    }

  /* Count number of frames.  Also get stack id tag and check whether
     there are more stackframes than we want to record
     (SCM_BACKTRACE_MAXDEPTH). */
  id = SCM_BOOL_F;
  maxp = 0;
  n = stack_depth (dframe, offset, &id, &maxp);
  size = n * SCM_FRAME_N_SLOTS;

  /* Make the stack object. */
  stack = scm_make_struct (scm_stack_type, SCM_MAKINUM (size), SCM_EOL);
  SCM_STACK (stack) -> id = id;
  SCM_STACK (stack) -> length = n;
  iframe = &SCM_STACK (stack) -> tail[0];
  SCM_STACK (stack) -> frames = iframe;

  /* Translate the current chain of stack frames into debugging information. */
  read_frames ((scm_debug_frame *) ((SCM_STACKITEM *) dframe + offset),
	       offset, n, iframe);

  /* Narrow the stack according to the arguments given to scm_make_stack. */
  while (n > 0 && SCM_NIMP (args) && SCM_CONSP (args))
    {
      inner_cut = SCM_CAR (args);
      args = SCM_CDR (args);
      if (SCM_NIMP (args) && SCM_CONSP (args))
	{
	  outer_cut = SCM_CAR (args);
	  args = SCM_CDR (args);
	}
      else
	outer_cut = SCM_INUM0;
      
      narrow_stack (stack,
		    SCM_INUMP (inner_cut) ? SCM_INUM (inner_cut) : n,
		    SCM_INUMP (inner_cut) ? 0 : inner_cut,
		    SCM_INUMP (outer_cut) ? SCM_INUM (outer_cut) : n,
		    SCM_INUMP (outer_cut) ? 0 : outer_cut);

      n = SCM_STACK (stack) -> length;
    }
  
  if (n > 0)
    {
      if (maxp)
	iframe[n - 1].flags |= SCM_FRAMEF_OVERFLOW;
      return stack;
    }
  else
    return SCM_BOOL_F;
}

SCM_PROC (s_stack_id, "stack-id", 1, 0, 0, scm_stack_id);
SCM
scm_stack_id (stack)
     SCM stack;
{
  scm_debug_frame *dframe;
  long offset = 0;
  if (stack == SCM_BOOL_T)
    dframe = scm_last_debug_frame;
  else
    {
      SCM_ASSERT (SCM_NIMP (stack), stack, SCM_ARG1, s_make_stack);
      if (SCM_DEBUGOBJP (stack))
	dframe = (scm_debug_frame *) SCM_DEBUGOBJ_FRAME (stack);
      else if (scm_tc7_contin == SCM_TYP7 (stack))
	{
	  offset = ((SCM_STACKITEM *) (SCM_CHARS (stack) + sizeof (scm_contregs))
		    - SCM_BASE (stack));
#ifndef STACK_GROWS_UP
	  offset += SCM_LENGTH (stack);
#endif
	  dframe = (scm_debug_frame *) ((SCM_STACKITEM *) SCM_DFRAME (stack)
				       + offset);
	}
      else if (SCM_STACKP (stack))
	return SCM_STACK (stack) -> id;
      else scm_wrong_type_arg (s_stack_id, SCM_ARG1, stack);
    }
  while (dframe && !SCM_VOIDFRAMEP (*dframe))
    dframe = (scm_debug_frame *) ((SCM_STACKITEM *) dframe->prev + offset);
  if (dframe && SCM_VOIDFRAMEP (*dframe))
    return dframe->vect[0].id;
  return SCM_BOOL_F;
}

SCM_PROC (s_stack_ref, "stack-ref", 2, 0, 0, scm_stack_ref);
SCM
scm_stack_ref (stack, i)
     SCM stack;
     SCM i;
{
  SCM_ASSERT (SCM_NIMP (stack)
	      && SCM_STACKP (stack),
	      stack,
	      SCM_ARG1,
	      s_stack_ref);
  SCM_ASSERT (SCM_INUMP (i), i, SCM_ARG2, s_stack_ref);
  SCM_ASSERT (SCM_INUM (i) >= 0
	      && SCM_INUM (i) < SCM_STACK_LENGTH (stack),
	      i,
	      SCM_OUTOFRANGE,
	      s_stack_ref);
  return scm_cons (stack, i);
}

SCM_PROC(s_stack_length, "stack-length", 1, 0, 0, scm_stack_length);
SCM
scm_stack_length (stack)
     SCM stack;
{
  SCM_ASSERT (SCM_NIMP (stack)
	      && SCM_STACKP (stack),
	      stack,
	      SCM_ARG1,
	      s_stack_length);
  return SCM_MAKINUM (SCM_STACK_LENGTH (stack));
}

/* Frames
 */

SCM_PROC (s_frame_p, "frame?", 1, 0, 0, scm_frame_p);
SCM
scm_frame_p (obj)
     SCM obj;
{
  return SCM_NIMP (obj) && SCM_FRAMEP (obj);
}

SCM_PROC(s_last_stack_frame, "last-stack-frame", 1, 0, 0, scm_last_stack_frame);
SCM
scm_last_stack_frame (obj)
     SCM obj;
{
  scm_debug_frame *dframe;
  long offset = 0;
  SCM stack;
  
  SCM_ASSERT (SCM_NIMP (obj), obj, SCM_ARG1, s_last_stack_frame);
  if (SCM_DEBUGOBJP (obj))
    dframe = (scm_debug_frame *) SCM_DEBUGOBJ_FRAME (obj);
  else if (scm_tc7_contin == SCM_TYP7 (obj))
    {
      offset = ((SCM_STACKITEM *) (SCM_CHARS (obj) + sizeof (scm_contregs))
		- SCM_BASE (obj));
#ifndef STACK_GROWS_UP
      offset += SCM_LENGTH (obj);
#endif
      dframe = (scm_debug_frame *) ((SCM_STACKITEM *) SCM_DFRAME (obj) + offset);
    }
  else scm_wta (obj, (char *) SCM_ARG1, s_last_stack_frame);
  
  if (!dframe || SCM_VOIDFRAMEP (*dframe))
    return SCM_BOOL_F;

  stack = scm_make_struct (scm_stack_type, SCM_MAKINUM (SCM_FRAME_N_SLOTS), SCM_EOL);
  SCM_STACK (stack) -> length = 1;
  SCM_STACK (stack) -> frames = &SCM_STACK (stack) -> tail[0];
  read_frame (dframe, offset, (scm_info_frame *) &SCM_STACK (stack) -> frames[0]);
  
  return scm_cons (stack, SCM_INUM0);;
}

SCM_PROC(s_frame_number, "frame-number", 1, 0, 0, scm_frame_number);
SCM
scm_frame_number (frame)
     SCM frame;
{
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_number);
  return SCM_MAKINUM (SCM_FRAME_NUMBER (frame));
}

SCM_PROC(s_frame_source, "frame-source", 1, 0, 0, scm_frame_source);
SCM
scm_frame_source (frame)
     SCM frame;
{
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_source);
  return SCM_FRAME_SOURCE (frame);
}

SCM_PROC(s_frame_procedure, "frame-procedure", 1, 0, 0, scm_frame_procedure);
SCM
scm_frame_procedure (frame)
     SCM frame;
{
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_procedure);
  return (SCM_FRAME_PROC_P (frame)
	  ? SCM_BOOL_F
	  : SCM_FRAME_PROC (frame));
}

SCM_PROC(s_frame_arguments, "frame-arguments", 1, 0, 0, scm_frame_arguments);
SCM
scm_frame_arguments (frame)
     SCM frame;
{
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_arguments);
  return SCM_FRAME_ARGS (frame);
}

SCM_PROC(s_frame_previous, "frame-previous", 1, 0, 0, scm_frame_previous);
SCM
scm_frame_previous (frame)
     SCM frame;
{
  int n;
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_previous);
  n = SCM_INUM (SCM_CDR (frame)) + 1;
  if (n >= SCM_STACK_LENGTH (SCM_CAR (frame)))
    return SCM_BOOL_F;
  else
    return scm_cons (SCM_CAR (frame), SCM_MAKINUM (n));
}

SCM_PROC(s_frame_next, "frame-next", 1, 0, 0, scm_frame_next);
SCM
scm_frame_next (frame)
     SCM frame;
{
  int n;
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_next);
  n = SCM_INUM (SCM_CDR (frame)) - 1;
  if (n < 0)
    return SCM_BOOL_F;
  else
    return scm_cons (SCM_CAR (frame), SCM_MAKINUM (n));
}

SCM_PROC(s_frame_real_p, "frame-real?", 1, 0, 0, scm_frame_real_p);
SCM
scm_frame_real_p (frame)
     SCM frame;
{
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_real_p);
  return SCM_FRAME_REAL_P (frame) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_frame_procedure_p, "frame-procedure?", 1, 0, 0, scm_frame_procedure_p);
SCM
scm_frame_procedure_p (frame)
     SCM frame;
{
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_procedure_p);
  return SCM_FRAME_PROC_P (frame) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_frame_evaluating_args_p, "frame-evaluating-args?", 1, 0, 0, scm_frame_evaluating_args_p);
SCM
scm_frame_evaluating_args_p (frame)
     SCM frame;
{
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_evaluating_args_p);
  return SCM_FRAME_EVAL_ARGS_P (frame) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_frame_overflow_p, "frame-overflow?", 1, 0, 0, scm_frame_overflow_p);
SCM
scm_frame_overflow_p (frame)
     SCM frame;
{
  SCM_ASSERT (SCM_NIMP (frame) && SCM_FRAMEP (frame),
	      frame,
	      SCM_ARG1,
	      s_frame_overflow_p);
  return SCM_FRAME_OVERFLOW_P (frame) ? SCM_BOOL_T : SCM_BOOL_F;
}



void
scm_init_stacks ()
{
  SCM vtable;
  SCM vtable_layout = scm_make_struct_layout (scm_nullstr);
  SCM stack_layout = scm_make_struct_layout (scm_makfrom0str (SCM_STACK_LAYOUT));
  vtable = scm_make_vtable_vtable (vtable_layout, SCM_INUM0, SCM_EOL);
  scm_stack_type = scm_permanent_object (scm_make_struct (vtable,
							  SCM_INUM0,
							  scm_cons (stack_layout, SCM_EOL)));
#include "stacks.x"
}

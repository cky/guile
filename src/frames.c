/* Copyright (C) 2001 Free Software Foundation, Inc.
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

#include <string.h>
#include "frames.h"


scm_bits_t scm_tc16_heap_frame;

SCM
scm_c_make_heap_frame (SCM *fp)
{
  struct scm_heap_frame *p =
    scm_must_malloc (sizeof (struct scm_heap_frame), "make_heap_frame");
  p->fp            = fp;
  p->program       = SCM_UNDEFINED;
  p->variables     = SCM_UNDEFINED;
  p->dynamic_link  = SCM_UNDEFINED;
  p->external_link = SCM_UNDEFINED;
  SCM_RETURN_NEWSMOB (scm_tc16_heap_frame, p);
}

static SCM
heap_frame_mark (SCM obj)
{
  struct scm_heap_frame *p = SCM_HEAP_FRAME_DATA (obj);
  scm_gc_mark (p->program);
  scm_gc_mark (p->variables);
  scm_gc_mark (p->dynamic_link);
  return p->external_link;
}

/* Scheme interface */

SCM_DEFINE (scm_frame_p, "frame?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_frame_p
{
  return SCM_BOOL (SCM_HEAP_FRAME_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_program, "frame-program", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_program
{
  SCM_VALIDATE_HEAP_FRAME (1, frame);
  return SCM_STACK_FRAME_PROGRAM (SCM_HEAP_FRAME_DATA (frame)->fp);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_local_variables, "frame-local-variables", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_local_variables
{
  struct scm_heap_frame *p;

  SCM_VALIDATE_HEAP_FRAME (1, frame);
  p = SCM_HEAP_FRAME_DATA (frame);

  if (SCM_UNBNDP (p->variables))
    {
      SCM prog = scm_frame_program (frame);
      struct scm_program *pp = SCM_PROGRAM_DATA (prog);
      int i, size = pp->nargs + pp->nlocs;
      p->variables = scm_make_vector (SCM_MAKINUM (size), SCM_BOOL_F);
      for (i = 0; i < size; i++)
	SCM_VELTS (p->variables)[i] = SCM_STACK_FRAME_VARIABLE (p->fp, i);
    }
  return p->variables;
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_return_address, "frame-return-address", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_return_address
{
  SCM_VALIDATE_HEAP_FRAME (1, frame);

  return scm_long2num ((long) SCM_VM_BYTE_ADDRESS
		       (SCM_STACK_FRAME_RETURN_ADDRESS
			(SCM_HEAP_FRAME_DATA (frame)->fp)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_dynamic_link, "frame-dynamic-link", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_dynamic_link
{
  struct scm_heap_frame *p;

  SCM_VALIDATE_HEAP_FRAME (1, frame);
  p = SCM_HEAP_FRAME_DATA (frame);

  if (SCM_UNBNDP (p->dynamic_link))
    {
      SCM *fp = SCM_VM_STACK_ADDRESS (SCM_STACK_FRAME_DYNAMIC_LINK (p->fp));
      if (fp)
	p->dynamic_link = scm_c_make_heap_frame (fp);
      else
	p->dynamic_link = SCM_BOOL_F;
    }

  return p->dynamic_link;
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_external_link, "frame-external-link", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_external_link
{
  struct scm_heap_frame *p;

  SCM_VALIDATE_HEAP_FRAME (1, frame);
  p = SCM_HEAP_FRAME_DATA (frame);

  if (SCM_UNBNDP (p->external_link))
    p->external_link = SCM_STACK_FRAME_EXTERNAL_LINK (p->fp);

  return p->external_link;
}
#undef FUNC_NAME


void
scm_init_frames (void)
{
  scm_tc16_heap_frame = scm_make_smob_type ("heap_frame", 0);
  scm_set_smob_mark (scm_tc16_heap_frame, heap_frame_mark);

#ifndef SCM_MAGIC_SNARFER
#include "frames.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

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
  SCM frame;
  SCM *lower = SCM_FRAME_LOWER_ADDRESS (fp);
  SCM *upper = SCM_FRAME_UPPER_ADDRESS (fp);
  size_t size = sizeof (SCM) * (upper - lower + 1);
  SCM *p = scm_must_malloc (size, "scm_c_make_heap_frame");
  SCM_NEWSMOB (frame, scm_tc16_heap_frame, p);
  p[0] = frame; /* self link */
  memcpy (p + 1, lower, size - sizeof (SCM));
  return frame;
}

static SCM
heap_frame_mark (SCM obj)
{
  SCM *sp;
  SCM *fp = SCM_HEAP_FRAME_POINTER (obj);
  SCM *limit = &SCM_FRAME_HEAP_LINK (fp);

  for (sp = SCM_FRAME_LOWER_ADDRESS (fp); sp <= limit; sp++)
    if (SCM_NIMP (*sp))
      scm_gc_mark (*sp);

  return SCM_BOOL_F;
}

static scm_sizet
heap_frame_free (SCM obj)
{
  SCM *fp = SCM_HEAP_FRAME_POINTER (obj);
  SCM *lower = SCM_FRAME_LOWER_ADDRESS (fp);
  SCM *upper = SCM_FRAME_UPPER_ADDRESS (fp);
  size_t size = sizeof (SCM) * (upper - lower + 1);
  scm_must_free (SCM_HEAP_FRAME_DATA (obj));
  return size;
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
  return SCM_FRAME_PROGRAM (SCM_HEAP_FRAME_POINTER (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_local_ref, "frame-local-ref", 2, 0, 0,
	    (SCM frame, SCM index),
	    "")
#define FUNC_NAME s_scm_frame_local_ref
{
  SCM_VALIDATE_HEAP_FRAME (1, frame);
  SCM_VALIDATE_INUM (2, index); /* FIXME: Check the range! */
  return SCM_FRAME_VARIABLE (SCM_HEAP_FRAME_POINTER (frame),
			     SCM_INUM (index));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_local_set_x, "frame-local-set!", 3, 0, 0,
	    (SCM frame, SCM index, SCM val),
	    "")
#define FUNC_NAME s_scm_frame_local_set_x
{
  SCM_VALIDATE_HEAP_FRAME (1, frame);
  SCM_VALIDATE_INUM (2, index); /* FIXME: Check the range! */
  SCM_FRAME_VARIABLE (SCM_HEAP_FRAME_POINTER (frame), SCM_INUM (index)) = val;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_return_address, "frame-return-address", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_return_address
{
  SCM_VALIDATE_HEAP_FRAME (1, frame);
  return scm_ulong2num ((unsigned long) (SCM_FRAME_RETURN_ADDRESS
					 (SCM_HEAP_FRAME_POINTER (frame))));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_dynamic_link, "frame-dynamic-link", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_dynamic_link
{
  SCM_VALIDATE_HEAP_FRAME (1, frame);
  return SCM_FRAME_HEAP_LINK (SCM_HEAP_FRAME_POINTER (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_external_link, "frame-external-link", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_external_link
{
  SCM_VALIDATE_HEAP_FRAME (1, frame);
  return SCM_FRAME_EXTERNAL_LINK (SCM_HEAP_FRAME_POINTER (frame));
}
#undef FUNC_NAME


void
scm_init_frames (void)
{
  scm_tc16_heap_frame = scm_make_smob_type ("frame", 0);
  scm_set_smob_mark (scm_tc16_heap_frame, heap_frame_mark);
  scm_set_smob_free (scm_tc16_heap_frame, heap_frame_free);

#ifndef SCM_MAGIC_SNARFER
#include "frames.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

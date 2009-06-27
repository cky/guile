/* Copyright (C) 2001 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>
#include "_scm.h"
#include "vm-bootstrap.h"
#include "frames.h"


scm_t_bits scm_tc16_vm_frame;

#define RELOC(frame, val) (val + SCM_VM_FRAME_OFFSET (frame))

SCM
scm_c_make_vm_frame (SCM stack_holder, SCM *fp, SCM *sp,
                     scm_byte_t *ip, scm_t_ptrdiff offset)
{
  struct scm_vm_frame *p = scm_gc_malloc (sizeof (struct scm_vm_frame),
                                          "vmframe");
  p->stack_holder = stack_holder;
  p->fp = fp;
  p->sp = sp;
  p->ip = ip;
  p->offset = offset;
  SCM_RETURN_NEWSMOB (scm_tc16_vm_frame, p);
}

static int
vm_frame_print (SCM frame, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<vm-frame ", port);
  scm_uintprint (SCM_UNPACK (frame), 16, port);
  scm_putc (' ', port);
  scm_write (scm_vm_frame_program (frame), port);
  /* don't write args, they can get us into trouble. */
  scm_puts (">", port);

  return 1;
}

static SCM
vm_frame_mark (SCM obj)
{
  return SCM_VM_FRAME_STACK_HOLDER (obj);
}

static size_t
vm_frame_free (SCM obj)
{
  struct scm_vm_frame *p = SCM_VM_FRAME_DATA (obj);
  scm_gc_free (p, sizeof(struct scm_vm_frame), "vmframe");
  return 0;
}

/* Scheme interface */

SCM_DEFINE (scm_vm_frame_p, "vm-frame?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_vm_frame_p
{
  return SCM_BOOL (SCM_VM_FRAME_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_program, "vm-frame-program", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_vm_frame_program
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return SCM_FRAME_PROGRAM (SCM_VM_FRAME_FP (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_arguments, "vm-frame-arguments", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_vm_frame_arguments
{
  SCM *fp;
  int i;
  struct scm_objcode *bp;
  SCM ret;
  
  SCM_VALIDATE_VM_FRAME (1, frame);

  fp = SCM_VM_FRAME_FP (frame);
  bp = SCM_PROGRAM_DATA (SCM_FRAME_PROGRAM (fp));

  if (!bp->nargs)
    return SCM_EOL;
  else if (bp->nrest)
    ret = fp[bp->nargs - 1];
  else
    ret = scm_cons (fp[bp->nargs - 1], SCM_EOL);
  
  for (i = bp->nargs - 2; i >= 0; i--)
    ret = scm_cons (fp[i], ret);
  
  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_source, "vm-frame-source", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_vm_frame_source
{
  SCM *fp;
  struct scm_objcode *bp;
  
  SCM_VALIDATE_VM_FRAME (1, frame);

  fp = SCM_VM_FRAME_FP (frame);
  bp = SCM_PROGRAM_DATA (SCM_FRAME_PROGRAM (fp));

  return scm_c_program_source (SCM_FRAME_PROGRAM (fp),
                               SCM_VM_FRAME_IP (frame) - bp->base);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_local_ref, "vm-frame-local-ref", 2, 0, 0,
	    (SCM frame, SCM index),
	    "")
#define FUNC_NAME s_scm_vm_frame_local_ref
{
  SCM *fp;
  unsigned int i;
  struct scm_objcode *bp;
  
  SCM_VALIDATE_VM_FRAME (1, frame);

  fp = SCM_VM_FRAME_FP (frame);
  bp = SCM_PROGRAM_DATA (SCM_FRAME_PROGRAM (fp));

  SCM_VALIDATE_UINT_COPY (2, index, i);
  SCM_ASSERT_RANGE (2, index, i < bp->nargs + bp->nlocs);

  return SCM_FRAME_VARIABLE (fp, i);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_local_set_x, "vm-frame-local-set!", 3, 0, 0,
	    (SCM frame, SCM index, SCM val),
	    "")
#define FUNC_NAME s_scm_vm_frame_local_set_x
{
  SCM *fp;
  unsigned int i;
  struct scm_objcode *bp;
  
  SCM_VALIDATE_VM_FRAME (1, frame);

  fp = SCM_VM_FRAME_FP (frame);
  bp = SCM_PROGRAM_DATA (SCM_FRAME_PROGRAM (fp));

  SCM_VALIDATE_UINT_COPY (2, index, i);
  SCM_ASSERT_RANGE (2, index, i < bp->nargs + bp->nlocs);

  SCM_FRAME_VARIABLE (fp, i) = val;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_return_address, "vm-frame-return-address", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_vm_frame_return_address
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return scm_from_ulong ((unsigned long)
			 (SCM_FRAME_RETURN_ADDRESS
			  (SCM_VM_FRAME_FP (frame))));
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_mv_return_address, "vm-frame-mv-return-address", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_vm_frame_mv_return_address
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return scm_from_ulong ((unsigned long)
			 (SCM_FRAME_MV_RETURN_ADDRESS
			  (SCM_VM_FRAME_FP (frame))));
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_dynamic_link, "vm-frame-dynamic-link", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_vm_frame_dynamic_link
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  /* fixme: munge fp if holder is a continuation */
  return scm_from_ulong
    ((unsigned long)
     RELOC (frame,
            SCM_FRAME_DYNAMIC_LINK (SCM_VM_FRAME_FP (frame))));
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_external_link, "vm-frame-external-link", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_vm_frame_external_link
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return SCM_FRAME_EXTERNAL_LINK (SCM_VM_FRAME_FP (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_vm_frame_stack, "vm-frame-stack", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_vm_frame_stack
{
  SCM *top, *bottom, ret = SCM_EOL;

  SCM_VALIDATE_VM_FRAME (1, frame);

  top = SCM_VM_FRAME_SP (frame);
  bottom = SCM_FRAME_UPPER_ADDRESS (SCM_VM_FRAME_FP (frame));
  while (bottom <= top)
    ret = scm_cons (*bottom++, ret);

  return ret;
}
#undef FUNC_NAME

extern SCM
scm_c_vm_frame_prev (SCM frame)
{
  SCM *this_fp, *new_fp, *new_sp;
  this_fp = SCM_VM_FRAME_FP (frame);
  new_fp = SCM_FRAME_DYNAMIC_LINK (this_fp);
  if (new_fp) 
    { new_fp = RELOC (frame, new_fp);
      new_sp = SCM_FRAME_LOWER_ADDRESS (this_fp) - 1;
      return scm_c_make_vm_frame (SCM_VM_FRAME_STACK_HOLDER (frame),
                                  new_fp, new_sp,
                                  SCM_FRAME_RETURN_ADDRESS (this_fp),
                                  SCM_VM_FRAME_OFFSET (frame));
    }
  else
    return SCM_BOOL_F;
}


void
scm_bootstrap_frames (void)
{
  scm_tc16_vm_frame = scm_make_smob_type ("vm-frame", 0);
  scm_set_smob_mark (scm_tc16_vm_frame, vm_frame_mark);
  scm_set_smob_free (scm_tc16_vm_frame, vm_frame_free);
  scm_set_smob_print (scm_tc16_vm_frame, vm_frame_print);
  scm_c_register_extension ("libguile", "scm_init_frames",
                            (scm_t_extension_init_func)scm_init_frames, NULL);
}

void
scm_init_frames (void)
{
  scm_bootstrap_vm ();

#ifndef SCM_MAGIC_SNARFER
#include "libguile/frames.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

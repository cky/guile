/* Copyright (C) 2001, 2009 Free Software Foundation, Inc.
 * * 
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

#ifndef _SCM_FRAMES_H_
#define _SCM_FRAMES_H_

#include <libguile.h>
#include "programs.h"


/*
 * VM frames
 */

/* VM Frame Layout
   ---------------

   |                  | <- fp + bp->nargs + bp->nlocs + 3
   +------------------+    = SCM_FRAME_UPPER_ADDRESS (fp)
   | Return address   |
   | MV return address|
   | Dynamic link     | <- fp + bp->nargs + bp->blocs
   | Local variable 1 |    = SCM_FRAME_DATA_ADDRESS (fp)
   | Local variable 0 | <- fp + bp->nargs
   | Argument 1       |
   | Argument 0       | <- fp
   | Program          | <- fp - 1
   +------------------+    = SCM_FRAME_LOWER_ADDRESS (fp)
   |                  |

   As can be inferred from this drawing, it is assumed that
   `sizeof (SCM *) == sizeof (SCM)', since pointers (the `link' parts) are
   assumed to be as long as SCM objects.  */

#define SCM_FRAME_DATA_ADDRESS(fp)				\
  (fp + SCM_PROGRAM_DATA (SCM_FRAME_PROGRAM (fp))->nargs	\
      + SCM_PROGRAM_DATA (SCM_FRAME_PROGRAM (fp))->nlocs)
#define SCM_FRAME_UPPER_ADDRESS(fp)	(SCM_FRAME_DATA_ADDRESS (fp) + 3)
#define SCM_FRAME_LOWER_ADDRESS(fp)	(fp - 1)

#define SCM_FRAME_BYTE_CAST(x)		((scm_byte_t *) SCM_UNPACK (x))
#define SCM_FRAME_STACK_CAST(x)		((SCM *) SCM_UNPACK (x))

#define SCM_FRAME_RETURN_ADDRESS(fp)				\
  (SCM_FRAME_BYTE_CAST (SCM_FRAME_DATA_ADDRESS (fp)[2]))
#define SCM_FRAME_MV_RETURN_ADDRESS(fp)				\
  (SCM_FRAME_BYTE_CAST (SCM_FRAME_DATA_ADDRESS (fp)[1]))
#define SCM_FRAME_DYNAMIC_LINK(fp)				\
  (SCM_FRAME_STACK_CAST (SCM_FRAME_DATA_ADDRESS (fp)[0]))
#define SCM_FRAME_SET_DYNAMIC_LINK(fp, dl)		\
  ((SCM_FRAME_DATA_ADDRESS (fp)[1])) = (SCM)(dl);
#define SCM_FRAME_VARIABLE(fp,i)	fp[i]
#define SCM_FRAME_PROGRAM(fp)		fp[-1]


/*
 * Heap frames
 */

SCM_API scm_t_bits scm_tc16_vm_frame;

struct scm_vm_frame 
{
  SCM stack_holder;
  SCM *fp;
  SCM *sp;
  scm_byte_t *ip;
  scm_t_ptrdiff offset;
};

#define SCM_VM_FRAME_P(x)	SCM_SMOB_PREDICATE (scm_tc16_vm_frame, x)
#define SCM_VM_FRAME_DATA(x)	((struct scm_vm_frame*)SCM_SMOB_DATA (x))
#define SCM_VM_FRAME_STACK_HOLDER(f)	SCM_VM_FRAME_DATA(f)->stack_holder
#define SCM_VM_FRAME_FP(f)	SCM_VM_FRAME_DATA(f)->fp
#define SCM_VM_FRAME_SP(f)	SCM_VM_FRAME_DATA(f)->sp
#define SCM_VM_FRAME_IP(f)	SCM_VM_FRAME_DATA(f)->ip
#define SCM_VM_FRAME_OFFSET(f)	SCM_VM_FRAME_DATA(f)->offset
#define SCM_VALIDATE_VM_FRAME(p,x)	SCM_MAKE_VALIDATE (p, x, VM_FRAME_P)

/* FIXME rename scm_byte_t */
SCM_API SCM scm_c_make_vm_frame (SCM stack_holder, SCM *fp, SCM *sp,
                                scm_byte_t *ip, scm_t_ptrdiff offset);
SCM_API SCM scm_vm_frame_p (SCM obj);
SCM_API SCM scm_vm_frame_program (SCM frame);
SCM_API SCM scm_vm_frame_arguments (SCM frame);
SCM_API SCM scm_vm_frame_source (SCM frame);
SCM_API SCM scm_vm_frame_local_ref (SCM frame, SCM index);
SCM_API SCM scm_vm_frame_local_set_x (SCM frame, SCM index, SCM val);
SCM_API SCM scm_vm_frame_return_address (SCM frame);
SCM_API SCM scm_vm_frame_mv_return_address (SCM frame);
SCM_API SCM scm_vm_frame_dynamic_link (SCM frame);
SCM_API SCM scm_vm_frame_stack (SCM frame);

SCM_API SCM scm_c_vm_frame_prev (SCM frame);

SCM_INTERNAL void scm_bootstrap_frames (void);
SCM_INTERNAL void scm_init_frames (void);

#endif /* _SCM_FRAMES_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* Copyright (C) 2001, 2009, 2010, 2011 Free Software Foundation, Inc.
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

/*
 * It's a little confusing, but there are two representations of frames in this
 * file: frame pointers and Scheme objects wrapping those frame pointers. The
 * former uses the SCM_FRAME_... macro prefix, the latter SCM_VM_FRAME_..
 * prefix.
 *
 * The confusing thing is that only Scheme frame objects have functions that use
 * them, and they use the scm_frame_.. prefix. Hysterical raisins.
 */

/* VM Frame Layout
   ---------------

   | ...              |
   | Intermed. val. 0 | <- fp + nargs + nlocs
   +------------------+    
   | Local variable 1 |
   | Local variable 0 | <- fp + nargs
   | Argument 1       |
   | Argument 0       | <- fp = SCM_FRAME_STACK_ADDRESS (fp)
   | Program          | <- fp - 1
   +==================+
   | Return address   | <- SCM_FRAME_UPPER_ADDRESS (fp)
   | MV return address|
   | Dynamic link     | <- fp - 4 = SCM_FRAME_DATA_ADDRESS (fp) = SCM_FRAME_LOWER_ADDRESS (fp)
   +==================+
   |                  |

   As can be inferred from this drawing, it is assumed that
   `sizeof (SCM *) == sizeof (SCM)', since pointers (the `link' parts) are
   assumed to be as long as SCM objects.  */

/* This structure maps to the contents of a VM stack frame.  It can
   alias a frame directly.  */
struct scm_vm_frame
{
  SCM *dynamic_link;
  scm_t_uint8 *mv_return_address;
  scm_t_uint8 *return_address;
  SCM program;
  SCM stack[1]; /* Variable-length */
};

#define SCM_FRAME_STRUCT(fp)				\
  ((struct scm_vm_frame *) SCM_FRAME_DATA_ADDRESS (fp))

#define SCM_FRAME_DATA_ADDRESS(fp)	(((SCM *) (fp)) - 4)
#define SCM_FRAME_STACK_ADDRESS(fp)	(SCM_FRAME_STRUCT (fp)->stack)
#define SCM_FRAME_UPPER_ADDRESS(fp)	((SCM*)&SCM_FRAME_STRUCT (fp)->return_address)
#define SCM_FRAME_LOWER_ADDRESS(fp)	((SCM*)SCM_FRAME_STRUCT (fp))

#define SCM_FRAME_BYTE_CAST(x)		((scm_t_uint8 *) SCM_UNPACK (x))
#define SCM_FRAME_STACK_CAST(x)		((SCM *) SCM_UNPACK (x))

#define SCM_FRAME_RETURN_ADDRESS(fp)            \
  (SCM_FRAME_STRUCT (fp)->return_address)
#define SCM_FRAME_SET_RETURN_ADDRESS(fp, ra)    \
  SCM_FRAME_STRUCT (fp)->return_address = (ra)
#define SCM_FRAME_MV_RETURN_ADDRESS(fp)         \
  (SCM_FRAME_STRUCT (fp)->mv_return_address)
#define SCM_FRAME_SET_MV_RETURN_ADDRESS(fp, mvra)       \
  SCM_FRAME_STRUCT (fp)->mv_return_address = (mvra)
#define SCM_FRAME_DYNAMIC_LINK(fp)              \
  (SCM_FRAME_STRUCT (fp)->dynamic_link)
#define SCM_FRAME_SET_DYNAMIC_LINK(fp, dl)      \
  SCM_FRAME_DYNAMIC_LINK (fp) = (dl)
#define SCM_FRAME_VARIABLE(fp,i)                \
  (SCM_FRAME_STRUCT (fp)->stack[i])
#define SCM_FRAME_PROGRAM(fp)                   \
  (SCM_FRAME_STRUCT (fp)->program)


/*
 * Heap frames
 */

struct scm_frame 
{
  SCM stack_holder;
  SCM *fp;
  SCM *sp;
  scm_t_uint8 *ip;
  scm_t_ptrdiff offset;
};

#define SCM_VM_FRAME_P(x)	(SCM_NIMP (x) && SCM_TYP7 (x) == scm_tc7_frame)
#define SCM_VM_FRAME_DATA(x)	((struct scm_frame*)SCM_CELL_WORD_1 (x))
#define SCM_VM_FRAME_STACK_HOLDER(f)	SCM_VM_FRAME_DATA(f)->stack_holder
#define SCM_VM_FRAME_FP(f)	SCM_VM_FRAME_DATA(f)->fp
#define SCM_VM_FRAME_SP(f)	SCM_VM_FRAME_DATA(f)->sp
#define SCM_VM_FRAME_IP(f)	SCM_VM_FRAME_DATA(f)->ip
#define SCM_VM_FRAME_OFFSET(f)	SCM_VM_FRAME_DATA(f)->offset
#define SCM_VALIDATE_VM_FRAME(p,x)	SCM_MAKE_VALIDATE (p, x, VM_FRAME_P)

SCM_API SCM scm_c_make_frame (SCM stack_holder, SCM *fp, SCM *sp,
                              scm_t_uint8 *ip, scm_t_ptrdiff offset);
SCM_API SCM scm_frame_p (SCM obj);
SCM_API SCM scm_frame_procedure (SCM frame);
SCM_API SCM scm_frame_arguments (SCM frame);
SCM_API SCM scm_frame_source (SCM frame);
SCM_API SCM scm_frame_num_locals (SCM frame);
SCM_API SCM scm_frame_local_ref (SCM frame, SCM index);
SCM_API SCM scm_frame_local_set_x (SCM frame, SCM index, SCM val);
SCM_API SCM scm_frame_address (SCM frame);
SCM_API SCM scm_frame_stack_pointer (SCM frame);
SCM_API SCM scm_frame_instruction_pointer (SCM frame);
SCM_API SCM scm_frame_return_address (SCM frame);
SCM_API SCM scm_frame_mv_return_address (SCM frame);
SCM_API SCM scm_frame_dynamic_link (SCM frame);
SCM_API SCM scm_frame_previous (SCM frame);

SCM_INTERNAL void scm_i_frame_print (SCM frame, SCM port,
                                     scm_print_state *pstate);
SCM_INTERNAL void scm_init_frames (void);

#endif /* _SCM_FRAMES_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

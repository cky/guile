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

#ifndef _SCM_FRAMES_H_
#define _SCM_FRAMES_H_

#include <libguile.h>
#include "config.h"
#include "programs.h"

/*
 * VM Address
 */

#define SCM_VM_MAKE_STACK_ADDRESS(ptr)	SCM_PACK (ptr)
#define SCM_VM_STACK_ADDRESS(addr)	((SCM *) SCM_UNPACK (addr))

#define SCM_VM_MAKE_BYTE_ADDRESS(ptr)	SCM_PACK (ptr)
#define SCM_VM_BYTE_ADDRESS(addr)	((scm_byte_t *) SCM_UNPACK (addr))


/*
 * VM Stack frames
 */

/* Stack frames are allocated on the VM stack as follows:

   |                  | <- fp + bp->nargs + bp->nlocs + 3
   +------------------+    = SCM_STACK_FRAME_UPPER_ADDRESS (fp)
   | Return address   |
   | Dynamic link     |
   | External link    | <- fp + bp->nargs + bp->nlocs
   | Local varialbe 1 |    = SCM_STACK_FRAME_DATA_ADDRESS (fp)
   | Local variable 0 | <- fp + bp->nargs
   | Argument 1       |
   | Argument 0       | <- fp
   | Program          | <- fp - 1
   +------------------+    = SCM_STACK_FRAME_LOWER_ADDRESS (fp)
   |                  |
*/

#define SCM_STACK_FRAME_DATA_ADDRESS(fp)			\
  (fp + SCM_PROGRAM_DATA (SCM_STACK_FRAME_PROGRAM (fp))->nargs	\
      + SCM_PROGRAM_DATA (SCM_STACK_FRAME_PROGRAM (fp))->nlocs)
#define SCM_STACK_FRAME_UPPER_ADDRESS(fp)			\
  (SCM_STACK_FRAME_DATA_ADDRESS (fp) + 3)
#define SCM_STACK_FRAME_LOWER_ADDRESS(fp)	(fp - 1)

#define SCM_STACK_FRAME_RETURN_ADDRESS(fp) SCM_STACK_FRAME_DATA_ADDRESS (fp)[2]
#define SCM_STACK_FRAME_DYNAMIC_LINK(fp)   SCM_STACK_FRAME_DATA_ADDRESS (fp)[1]
#define SCM_STACK_FRAME_EXTERNAL_LINK(fp)  SCM_STACK_FRAME_DATA_ADDRESS (fp)[0]
#define SCM_STACK_FRAME_VARIABLE(fp,i)	   fp[i]
#define SCM_STACK_FRAME_PROGRAM(fp)	   fp[-1]


/*
 * VM Heap frames
 */

struct scm_heap_frame {
  SCM *fp;
  SCM program;
  SCM variables;
  SCM dynamic_link;
  SCM external_link;
};

extern scm_bits_t scm_tc16_heap_frame;

#define SCM_HEAP_FRAME_P(x)	SCM_SMOB_PREDICATE (scm_tc16_heap_frame, x)
#define SCM_HEAP_FRAME_DATA(f) ((struct scm_heap_frame *) SCM_SMOB_DATA (f))
#define SCM_VALIDATE_HEAP_FRAME(p,x)	SCM_MAKE_VALIDATE (p, x, HEAP_FRAME_P)

#define SCM_HEAP_FRAME_PROGRAM(f)	SCM_HEAP_FRAME_DATA (f)->program
#define SCM_HEAP_FRAME_VARIABLES(f)	SCM_HEAP_FRAME_DATA (f)->variables
#define SCM_HEAP_FRAME_DYNAMIC_LINK(f)	SCM_HEAP_FRAME_DATA (f)->dynamic_link
#define SCM_HEAP_FRAME_EXTERNAL_LINK(f) SCM_HEAP_FRAME_DATA (f)->external_link

extern SCM scm_c_make_heap_frame (SCM *fp);
extern void scm_init_frames (void);

#endif /* _SCM_FRAMES_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

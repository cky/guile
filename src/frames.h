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
 * VM frames
 */

/* 
   |                  | <- fp + bp->nargs + bp->nlocs + 4
   +------------------+    = SCM_FRAME_UPPER_ADDRESS (fp)
   | Return address   |
   | Dynamic link     |
   | Heap link        |
   | External link    | <- fp + bp->nargs + bp->nlocs
   | Local varialbe 1 |    = SCM_FRAME_DATA_ADDRESS (fp)
   | Local variable 0 | <- fp + bp->nargs
   | Argument 1       |
   | Argument 0       | <- fp
   | Program          | <- fp - 1
   +------------------+    = SCM_FRAME_LOWER_ADDRESS (fp)
   |                  |
*/

#define SCM_FRAME_DATA_ADDRESS(fp)				\
  (fp + SCM_PROGRAM_DATA (SCM_FRAME_PROGRAM (fp))->nargs	\
      + SCM_PROGRAM_DATA (SCM_FRAME_PROGRAM (fp))->nlocs)
#define SCM_FRAME_UPPER_ADDRESS(fp)	(SCM_FRAME_DATA_ADDRESS (fp) + 4)
#define SCM_FRAME_LOWER_ADDRESS(fp)	(fp - 1)

#define SCM_FRAME_BYTE_CAST(x)		((scm_byte_t *) SCM_UNPACK (x))
#define SCM_FRAME_STACK_CAST(x)		((SCM *) SCM_UNPACK (x))

#define SCM_FRAME_RETURN_ADDRESS(fp)	SCM_FRAME_BYTE_CAST (SCM_FRAME_DATA_ADDRESS (fp)[3])
#define SCM_FRAME_DYNAMIC_LINK(fp)	SCM_FRAME_STACK_CAST (SCM_FRAME_DATA_ADDRESS (fp)[2])
#define SCM_FRAME_HEAP_LINK(fp)		SCM_FRAME_DATA_ADDRESS (fp)[1]
#define SCM_FRAME_EXTERNAL_LINK(fp)	SCM_FRAME_DATA_ADDRESS (fp)[0]
#define SCM_FRAME_VARIABLE(fp,i)	fp[i]
#define SCM_FRAME_PROGRAM(fp)		fp[-1]


/*
 * Heap frames
 */

extern scm_bits_t scm_tc16_heap_frame;

#define SCM_HEAP_FRAME_P(x)	SCM_SMOB_PREDICATE (scm_tc16_heap_frame, x)
#define SCM_HEAP_FRAME_DATA(f)		((SCM *) SCM_SMOB_DATA (f))
#define SCM_HEAP_FRAME_SELF(f)		(SCM_HEAP_FRAME_DATA (f)[0])
#define SCM_HEAP_FRAME_POINTER(f)	(SCM_HEAP_FRAME_DATA (f) + 2)
#define SCM_VALIDATE_HEAP_FRAME(p,x)	SCM_MAKE_VALIDATE (p, x, HEAP_FRAME_P)

extern SCM scm_c_make_heap_frame (SCM *fp);
extern void scm_init_frames (void);

#endif /* _SCM_FRAMES_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

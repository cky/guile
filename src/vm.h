/* Copyright (C) 2000 Free Software Foundation, Inc.
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

#ifndef _VM_H_
#define _VM_H_

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
 * VM Stack frame
 */

/*
   |                  | <- fp + bp->nargs + bp->nlocs + 2
   +------------------+    = SCM_VM_FRAME_UPPER_ADDRESS (fp)
   | Dynamic link     |
   | Return address   | <- fp + bp->nargs + bp->nlocs
   | Local varialbe 1 |    = SCM_VM_FRAME_DATA_ADDRESS (fp)
   | Local variable 0 | <- fp + bp->nargs
   | Argument 1       |
   | Argument 0       | <- fp
   | Program          | <- fp - 1
   +------------------+    = SCM_VM_FRAME_LOWER_ADDRESS (fp)
   |                  |
*/

#define SCM_VM_FRAME_LOWER_ADDRESS(fp)	(fp - 1)
#define SCM_VM_FRAME_DATA_ADDRESS(fp)			\
  (fp + SCM_PROGRAM_NARGS (SCM_VM_FRAME_PROGRAM (fp))	\
      + SCM_PROGRAM_NLOCS (SCM_VM_FRAME_PROGRAM (fp)))
#define SCM_VM_FRAME_UPPER_ADDRESS(fp)			\
  (SCM_VM_FRAME_DATA_ADDRESS (fp) + 2)

#define SCM_VM_FRAME_DYNAMIC_LINK(fp)	SCM_VM_FRAME_DATA_ADDRESS (fp)[1]
#define SCM_VM_FRAME_RETURN_ADDRESS(fp)	SCM_VM_FRAME_DATA_ADDRESS (fp)[0]
#define SCM_VM_FRAME_VARIABLE(fp,i)	fp[i]
#define SCM_VM_FRAME_PROGRAM(fp)	fp[-1]

/*
 * VM Heap frame
 */

struct scm_vm_heap_frame {
  SCM *fp;
  SCM program;
  SCM variables;
  SCM dynamic_link;
};

extern scm_bits_t scm_tc16_vm_heap_frame;

#define SCM_VM_HEAP_FRAME_P(x)	SCM_SMOB_PREDICATE (scm_tc16_vm_heap_frame, x)
#define SCM_VM_HEAP_FRAME_DATA(f) ((struct scm_vm_heap_frame *) SCM_SMOB_DATA (f))
#define SCM_VALIDATE_VM_HEAP_FRAME(p,x)	SCM_MAKE_VALIDATE (p, x, VM_HEAP_FRAME_P)

#define SCM_VM_HEAP_FRAME_PROGRAM(f)	SCM_VM_HEAP_FRAME_DATA (f)->program
#define SCM_VM_HEAP_FRAME_VARIABLES(f)	SCM_VM_HEAP_FRAME_DATA (f)->variables
#define SCM_VM_HEAP_FRAME_DYNAMIC_LINK(f) SCM_VM_HEAP_FRAME_DATA (f)->dynamic_link

/*
 * VM
 */

#define SCM_VM_BOOT_HOOK	0
#define SCM_VM_HALT_HOOK	1
#define SCM_VM_NEXT_HOOK	2
#define SCM_VM_ENTER_HOOK	3
#define SCM_VM_APPLY_HOOK	4
#define SCM_VM_EXIT_HOOK	5
#define SCM_VM_RETURN_HOOK	6
#define SCM_VM_NUM_HOOKS	7

struct scm_vm {
  scm_byte_t *ip;		/* instruction pointer */
  SCM *sp;			/* stack pointer */
  SCM *fp;			/* frame pointer */
  size_t stack_size;		/* stack size */
  SCM *stack_base;		/* stack base address */
  SCM *stack_limit;		/* stack limit address */
  SCM hooks[SCM_VM_NUM_HOOKS];	/* hooks */
  SCM options;			/* options */
  unsigned long time;		/* time spent */
  unsigned long clock;		/* bogos clock */
};

#define SCM_VM_P(x)		SCM_SMOB_PREDICATE (scm_tc16_vm, x)
#define SCM_VM_DATA(vm)		((struct scm_vm *) SCM_SMOB_DATA (vm))
#define SCM_VALIDATE_VM(pos,x)	SCM_MAKE_VALIDATE (pos, x, VM_P)

extern SCM scm_the_vm ();
extern SCM scm_make_vm (void);
extern SCM scm_vm_apply (SCM vm, SCM program, SCM args);
extern SCM scm_vm_option_ref (SCM vm, SCM key);
extern SCM scm_vm_option_set_x (SCM vm, SCM key, SCM val);

extern SCM scm_vm_current_frame (SCM vm);

extern void scm_init_vm (void);

#endif /* _VM_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

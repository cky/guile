/* classes: h_files */

#ifndef SCM_CONTINUATIONS_H
#define SCM_CONTINUATIONS_H

/* Copyright (C) 1995,1996,2000,2001, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"

#ifdef __ia64__
#include <signal.h>
#include <ucontext.h>
#endif /* __ia64__ */


#define SCM_CONTINUATIONP(x) \
  (SCM_PROGRAM_P (x) && SCM_PROGRAM_IS_CONTINUATION (x))

/* a continuation SCM is a non-immediate pointing to a heap cell with:
   word 0: bits 0-15: smob type tag: scm_tc16_continuation.
           bits 16-31: unused.
   word 1: malloc block containing an scm_t_contregs structure with a
           tail array of SCM_STACKITEM.  the size of the array is stored
	   in the num_stack_items field of the structure.
*/

typedef struct 
{
  scm_i_jmp_buf jmpbuf;
  SCM dynenv;
#ifdef __ia64__
  void *backing_store;
  unsigned long backing_store_size;
#endif /* __ia64__ */
  size_t num_stack_items;   /* size of the saved stack.  */
  SCM root;                 /* continuation root identifier.  */
  SCM vm;                   /* vm */
  SCM vm_cont;              /* vm's stack and regs */

  /* The offset from the live stack location to this copy.  This is
     used to adjust pointers from within the copied stack to the stack
     itself.

     Thus, when you read a pointer from the copied stack that points
     into the live stack, you need to add OFFSET so that it points
     into the copy.
  */
  scm_t_ptrdiff offset;

  SCM_STACKITEM stack[1];    /* copied stack of size num_stack_items.  */ 
} scm_t_contregs;




SCM_INTERNAL SCM scm_i_make_continuation (int *first, SCM vm, SCM vm_cont);
SCM_INTERNAL void scm_i_check_continuation (SCM cont);
SCM_INTERNAL void scm_i_reinstate_continuation (SCM cont);

SCM_INTERNAL SCM scm_i_call_with_current_continuation (SCM proc);

SCM_INTERNAL SCM scm_i_continuation_to_frame (SCM cont);
SCM_INTERNAL SCM scm_i_contregs_vm (SCM contregs);
SCM_INTERNAL SCM scm_i_contregs_vm_cont (SCM contregs);

SCM_API void *scm_c_with_continuation_barrier (void *(*func)(void*), void *);
SCM_API SCM scm_with_continuation_barrier (SCM proc);

SCM_INTERNAL SCM
scm_i_with_continuation_barrier (scm_t_catch_body body,
				 void *body_data,
				 scm_t_catch_handler handler,
				 void *handler_data,
				 scm_t_catch_handler pre_unwind_handler,
				 void *pre_unwind_handler_data);

SCM_INTERNAL void scm_init_continuations (void);

#endif  /* SCM_CONTINUATIONS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

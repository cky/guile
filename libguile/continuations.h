/* classes: h_files */

#ifndef SCM_CONTINUATIONS_H
#define SCM_CONTINUATIONS_H

/* Copyright (C) 1995,1996,2000,2001 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */



#include "libguile/__scm.h"

#ifdef __ia64__
#include <signal.h>
#include <ucontext.h>
extern unsigned long * __libc_ia64_register_backing_store_base;
#endif /* __ia64__ */


/* a continuation SCM is a non-immediate pointing to a heap cell with:
   word 0: bits 0-15: smob type tag: scm_tc16_continuation.
           bits 16-31: unused.
   word 1: malloc block containing an scm_t_contregs structure with a
           tail array of SCM_STACKITEM.  the size of the array is stored
	   in the num_stack_items field of the structure.
*/

SCM_API scm_t_bits scm_tc16_continuation;

typedef struct 
{
  SCM throw_value;
  jmp_buf jmpbuf;
  SCM dynenv;
#ifdef __ia64__
  ucontext_t ctx;
  void *backing_store;
  unsigned long backing_store_size;
#endif /* __ia64__ */
  SCM_STACKITEM *base;      /* base of the live stack, before it was saved.  */
  size_t num_stack_items;   /* size of the saved stack.  */
  unsigned long seq;        /* dynamic root identifier.  */

  /* The offset from the live stack location and this copy.  This is
     used to adjust pointers from within the copied stack to the stack
     itself.

     Thus, when you read a pointer from the copied stack that points
     into the live stack, you need to add OFFSET so that it points
     into the copy.
  */
  scm_t_ptrdiff offset;

  /* The most recently created debug frame on the live stack, before
     it was saved.  This need to be adjusted with OFFSET, above.
  */
  struct scm_t_debug_frame *dframe;

  SCM_STACKITEM stack[1];    /* copied stack of size num_stack_items.  */ 
} scm_t_contregs;

#define SCM_CONTINUATIONP(x)	SCM_TYP16_PREDICATE (scm_tc16_continuation, x)

#define SCM_CONTREGS(x)		((scm_t_contregs *) SCM_CELL_WORD_1 (x))

#define SCM_CONTINUATION_LENGTH(x) (SCM_CONTREGS (x)->num_stack_items)
#define SCM_SET_CONTINUATION_LENGTH(x, n)\
   (SCM_CONTREGS (x)->num_stack_items = (n))
#define SCM_JMPBUF(x)		((SCM_CONTREGS (x))->jmpbuf)
#define SCM_DYNENV(x)		((SCM_CONTREGS (x))->dynenv)
#define SCM_THROW_VALUE(x)	((SCM_CONTREGS (x))->throw_value)
#define SCM_BASE(x)		((SCM_CONTREGS (x))->base) 
#define SCM_SEQ(x)		((SCM_CONTREGS (x))->seq)   
#define SCM_DFRAME(x)		((SCM_CONTREGS (x))->dframe)



SCM_API SCM scm_make_continuation (int *first);
SCM_API void scm_init_continuations (void);

#endif  /* SCM_CONTINUATIONS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

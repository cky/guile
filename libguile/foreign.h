#ifndef SCM_FOREIGN_H
#define SCM_FOREIGN_H

/* Copyright (C) 2010  Free Software Foundation, Inc.
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

/* A foreign value is some value that exists outside of Guile. It is represented
   by a cell whose second word is a pointer. The first word has the
   scm_tc7_foreign typecode and type of the aliased (pointed-to) value in its
   lower 16 bits.

   The basic idea is that we can help the programmer to avoid cutting herself,
   but we won't take away her knives.  */

enum scm_t_foreign_type
  {
    SCM_FOREIGN_TYPE_VOID,
    SCM_FOREIGN_TYPE_FLOAT,
    SCM_FOREIGN_TYPE_DOUBLE,
    SCM_FOREIGN_TYPE_UINT8,
    SCM_FOREIGN_TYPE_INT8,
    SCM_FOREIGN_TYPE_UINT16,
    SCM_FOREIGN_TYPE_INT16,
    SCM_FOREIGN_TYPE_UINT32,
    SCM_FOREIGN_TYPE_INT32,
    SCM_FOREIGN_TYPE_UINT64,
    SCM_FOREIGN_TYPE_INT64,
    SCM_FOREIGN_TYPE_LAST = SCM_FOREIGN_TYPE_INT64
  };

typedef enum scm_t_foreign_type scm_t_foreign_type;

typedef void (*scm_t_foreign_finalizer) (void *);

#define SCM_FOREIGN_P(x)                                                \
  (!SCM_IMP (x) && SCM_TYP7(x) == scm_tc7_foreign)
#define SCM_VALIDATE_FOREIGN(pos, x)		\
  SCM_MAKE_VALIDATE (pos, x, FOREIGN_P)
#define SCM_FOREIGN_POINTER(x)			\
  ((void *) SCM_CELL_WORD_1 (x))
#define SCM_FOREIGN_HAS_FINALIZER(x)		\
  ((SCM_CELL_WORD_0 (x) >> 16) & 0x1)

SCM_API SCM scm_take_foreign_pointer (void *, scm_t_foreign_finalizer);

SCM_API SCM scm_alignof (SCM type);
SCM_API SCM scm_sizeof (SCM type);
SCM_API SCM scm_foreign_address (SCM foreign);
SCM_API SCM scm_foreign_to_bytevector (SCM foreign, SCM type,
                                       SCM offset, SCM len);
SCM_API SCM scm_foreign_set_finalizer_x (SCM foreign, SCM finalizer);
SCM_API SCM scm_bytevector_to_foreign (SCM bv, SCM offset, SCM len);

SCM_INTERNAL SCM scm_make_pointer (SCM address, SCM finalizer);
SCM_INTERNAL void scm_i_foreign_print (SCM foreign, SCM port,
                                       scm_print_state *pstate);



/* Foreign functions */

/* The goal is to make it so that calling a foreign function doesn't cause any
   heap allocation. That means we need native Scheme formats for all kinds of
   arguments.

   For "value" types like s64 or f32, we just use native Scheme value types.
   (Note that in both these cases, allocation is possible / likely, as the
   value might need to be boxed, but perhaps we won't worry about that. Hmm.)

   For everything else, we use foreign pointers. This includes arrays, pointer
   arguments and return vals, struct args and return vals, and out and in/out
   arguments.
 */

SCM_API SCM scm_make_foreign_function (SCM return_type, SCM func_ptr,
                                       SCM arg_types);
SCM_INTERNAL SCM scm_i_foreign_call (SCM foreign, const SCM *argv);



SCM_INTERNAL void scm_register_foreign (void);


#endif /* SCM_FOREIGN_H */

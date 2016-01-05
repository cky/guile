#ifndef SCM_FOREIGN_H
#define SCM_FOREIGN_H

/* Copyright (C) 2010, 2011, 2012, 2016  Free Software Foundation, Inc.
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

/* A "foreign pointer" is a wrapped C pointer.  It is represented by a
   cell whose second word is a pointer.  The first word has the
   `scm_tc7_pointer' type code.

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

typedef void (*scm_t_pointer_finalizer) (void *);

#define SCM_POINTER_P(x)                                                \
  (!SCM_IMP (x) && SCM_TYP7(x) == scm_tc7_pointer)
#define SCM_VALIDATE_POINTER(pos, x)		\
  SCM_MAKE_VALIDATE (pos, x, POINTER_P)
#define SCM_POINTER_VALUE(x)			\
  ((void *) SCM_CELL_WORD_1 (x))

SCM_API void *scm_to_pointer (SCM pointer);
SCM_API SCM scm_from_pointer (void *, scm_t_pointer_finalizer);

SCM_API SCM scm_alignof (SCM type);
SCM_API SCM scm_sizeof (SCM type);
SCM_API SCM scm_pointer_address (SCM pointer);
SCM_API SCM scm_pointer_to_bytevector (SCM pointer, SCM type,
                                       SCM offset, SCM len);
SCM_API SCM scm_set_pointer_finalizer_x (SCM pointer, SCM finalizer);
SCM_API SCM scm_bytevector_to_pointer (SCM bv, SCM offset);

SCM_INTERNAL SCM scm_pointer_p (SCM obj);
SCM_INTERNAL SCM scm_make_pointer (SCM address, SCM finalizer);
SCM_INTERNAL void scm_i_pointer_print (SCM pointer, SCM port,
                                       scm_print_state *pstate);

SCM_INTERNAL SCM scm_dereference_pointer (SCM pointer);
SCM_INTERNAL SCM scm_string_to_pointer (SCM string, SCM encoding);
SCM_INTERNAL SCM scm_pointer_to_string (SCM pointer, SCM length, SCM encoding);



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

SCM_API SCM scm_pointer_to_procedure (SCM return_type, SCM func_ptr,
				      SCM arg_types);
SCM_API SCM scm_pointer_to_procedure_with_errno (SCM return_type, SCM func_ptr,
                                                 SCM arg_types);
SCM_API SCM scm_procedure_to_pointer (SCM return_type, SCM func_ptr,
				      SCM arg_types);
SCM_INTERNAL SCM scm_i_foreign_call (SCM foreign, const SCM *argv);



SCM_INTERNAL void scm_register_foreign (void);


#endif /* SCM_FOREIGN_H */

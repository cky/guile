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

#ifndef SCM_FOREIGN_H
#define SCM_FOREIGN_H



/* A subset of libffi's types. */
typedef enum
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
    SCM_FOREIGN_TYPE_STRUCT,
    SCM_FOREIGN_TYPE_POINTER
  } scm_t_foreign_type;


typedef void (*scm_t_foreign_finalizer) (void *);

#define SCM_FOREIGN_P(x)                                                \
  (!SCM_IMP (x) && SCM_TYP7(x) == scm_tc7_foreign)
#define SCM_VALIDATE_FOREIGN(pos, x)					\
  SCM_MAKE_VALIDATE (pos, x, FOREIGN_P)
#define SCM_FOREIGN_TYPE(x)                                             \
  ((scm_t_foreign_type)((SCM_CELL_WORD_0 (x) >> 8)&0xff))
#define SCM_FOREIGN_OBJECT(x, ctype)                                    \
  ((ctype*)SCM_CELL_OBJECT_1 (x))
#define SCM_FOREIGN_OBJECT_REF(x, ctype)                                \
  (*SCM_FOREIGN_OBJECT (x, ctype))
#define SCM_FOREIGN_OBJECT_SET(x, ctype, val)                           \
  (*SCM_FOREIGN_OBJECT (x, ctype) = (val))

#define SCM_FOREIGN_TYPED_P(x, type)					\
  (SCM_FOREIGN_P (x) && SCM_FOREIGN_TYPE (x) == SCM_FOREIGN_TYPE_##type)
#define SCM_VALIDATE_FOREIGN_TYPED(pos, x, type)                        \
  do {                                                                  \
    SCM_ASSERT_TYPE (SCM_FOREIGN_TYPED_P (x, type), x, pos, FUNC_NAME,  \
                     "FOREIGN_"#type"_P");                              \
  } while (0)

#define SCM_FOREIGN_SIMPLE_P(x)                                         \
  (SCM_FOREIGN_P (x)                                                    \
   && SCM_FOREIGN_TYPE (x) != SCM_FOREIGN_TYPE_VOID                     \
   && SCM_FOREIGN_TYPE (x) != SCM_FOREIGN_TYPE_STRUCT                   \
   && SCM_FOREIGN_TYPE (x) != SCM_FOREIGN_TYPE_POINTER)
#define SCM_VALIDATE_FOREIGN_SIMPLE(pos, x)				\
  SCM_MAKE_VALIDATE (pos, x, FOREIGN_SIMPLE_P)

SCM_API SCM scm_c_from_foreign (scm_t_foreign_type type, void *val, size_t size,
                                scm_t_foreign_finalizer finalizer);
SCM_API SCM scm_c_take_foreign (scm_t_foreign_type type, void *val,
                                scm_t_foreign_finalizer finalizer);

SCM_API SCM scm_foreign_ref (SCM foreign);
SCM_API SCM scm_foreign_set_x (SCM foreign, SCM val);

SCM_INTERNAL void scm_i_foreign_print (SCM foreign, SCM port,
                                       scm_print_state *pstate);
SCM_INTERNAL void scm_init_foreign (void);


#endif /* SCM_FOREIGN_H */

#ifndef SCM_FOREIGN_OBJECT_H
#define SCM_FOREIGN_OBJECT_H

/* Copyright (C) 2014 Free Software Foundation, Inc.
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
#include "libguile/print.h"




SCM_API SCM scm_make_foreign_object_type (SCM name, SCM slot_names,
                                          scm_t_struct_finalize finalizer);

SCM_API void scm_assert_foreign_object_type (SCM type, SCM val);

/* All objects of a given foreign object type have the same number of
   fields.  When constructing a foreign object, you don't have to pass
   initializers for all of the fields; it is always OK to call
   scm_make_foreign_object_0 and initialize the fields by hand with
   scm_foreign_object_set_x or other setters.  The initial value of
   fields that haven't been explicitly given a value is 0.  */
SCM_API SCM scm_make_foreign_object_0 (SCM type);
SCM_API SCM scm_make_foreign_object_1 (SCM type, void *val0);
SCM_API SCM scm_make_foreign_object_2 (SCM type, void *val0, void *val1);
SCM_API SCM scm_make_foreign_object_3 (SCM type, void *val0, void *val1,
                                       void *val2);
SCM_API SCM scm_make_foreign_object_n (SCM type, size_t n, void *vals[]);

SCM_API void* scm_foreign_object_ref (SCM obj, size_t n);
SCM_API void scm_foreign_object_set_x (SCM obj, size_t n, void *val);

SCM_API scm_t_bits scm_foreign_object_unsigned_ref (SCM obj, size_t n);
SCM_API void scm_foreign_object_unsigned_set_x (SCM obj, size_t n,
                                                scm_t_bits val);

SCM_API scm_t_signed_bits scm_foreign_object_signed_ref (SCM obj, size_t n);
SCM_API void scm_foreign_object_signed_set_x (SCM obj, size_t n,
                                              scm_t_signed_bits val);

SCM_INTERNAL void scm_register_foreign_object (void);


#endif  /* SCM_FOREIGN_OBJECT_H */

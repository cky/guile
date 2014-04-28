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



#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/goops.h"
#include "libguile/foreign-object.h"




static SCM make_fobj_type_var;

static void
init_make_fobj_type_var (void)
{
  make_fobj_type_var = scm_c_private_lookup ("system foreign-object",
                                             "make-foreign-object-type");
}

SCM
scm_make_foreign_object_type (SCM name, SCM slot_names,
                              scm_t_struct_finalize finalizer)
{
  SCM type;

  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_make_fobj_type_var);

  type = scm_call_2 (scm_variable_ref (make_fobj_type_var), name, slot_names);

  if (finalizer)
    SCM_SET_VTABLE_INSTANCE_FINALIZER (type, finalizer);

  return type;
}

void
scm_assert_foreign_object_type (SCM type, SCM val)
{
  if (!SCM_IS_A_P (val, type))
    scm_error (scm_arg_type_key, NULL, "Wrong type (expecting ~A): ~S",
               scm_list_2 (scm_class_name (type), val), scm_list_1 (val));
}

SCM
scm_make_foreign_object_0 (SCM type)
{
  return scm_make_foreign_object_n (type, 0, NULL);
}

SCM
scm_make_foreign_object_1 (SCM type, void *val0)
{
  return scm_make_foreign_object_n (type, 1, &val0);
}

SCM
scm_make_foreign_object_2 (SCM type, void *val0, void *val1)
{
  void *vals[2];

  vals[0] = val0;
  vals[1] = val1;

  return scm_make_foreign_object_n (type, 2, vals);
}

SCM
scm_make_foreign_object_3 (SCM type, void *val0, void *val1, void *val2)
{
  void *vals[3];

  vals[0] = val0;
  vals[1] = val1;
  vals[2] = val2;

  return scm_make_foreign_object_n (type, 3, vals);
}

SCM
scm_make_foreign_object_n (SCM type, size_t n, void *vals[])
#define FUNC_NAME "make-foreign-object"
{
  SCM obj;
  SCM layout;
  size_t i;
  const char *layout_chars;

  SCM_VALIDATE_VTABLE (SCM_ARG1, type);

  layout = SCM_VTABLE_LAYOUT (type);

  if (scm_i_symbol_length (layout) / 2 < n)
    scm_out_of_range (FUNC_NAME, scm_from_size_t (n));

  layout_chars = scm_i_symbol_chars (layout);
  for (i = 0; i < n; i++)
    if (layout_chars[i * 2] != 'u')
      scm_wrong_type_arg_msg (FUNC_NAME, 0, layout, "'u' field");

  obj = scm_c_make_structv (type, 0, 0, NULL);

  for (i = 0; i < n; i++)
    SCM_STRUCT_DATA_SET (obj, i, (scm_t_bits) vals[i]);

  return obj;
}
#undef FUNC_NAME

scm_t_bits
scm_foreign_object_unsigned_ref (SCM obj, size_t n)
#define FUNC_NAME "foreign-object-ref"
{
  SCM layout;

  SCM_VALIDATE_STRUCT (SCM_ARG1, obj);
  
  layout = SCM_STRUCT_LAYOUT (obj);
  if (scm_i_symbol_length (layout) / 2 < n)
    scm_out_of_range (FUNC_NAME, scm_from_size_t (n));

  if (scm_i_symbol_ref (layout, n * 2) != 'u')
    scm_wrong_type_arg_msg (FUNC_NAME, 0, layout, "'u' field");

  return SCM_STRUCT_DATA_REF (obj, n);
}
#undef FUNC_NAME

void
scm_foreign_object_unsigned_set_x (SCM obj, size_t n, scm_t_bits val)
#define FUNC_NAME "foreign-object-set!"
{
  SCM layout;

  SCM_VALIDATE_STRUCT (SCM_ARG1, obj);
  
  layout = SCM_STRUCT_LAYOUT (obj);
  if (scm_i_symbol_length (layout) / 2 < n)
    scm_out_of_range (FUNC_NAME, scm_from_size_t (n));

  if (scm_i_symbol_ref (layout, n * 2) != 'u')
    scm_wrong_type_arg_msg (FUNC_NAME, 0, layout, "'u' field");

  SCM_STRUCT_DATA_SET (obj, n, val);
}
#undef FUNC_NAME

scm_t_signed_bits
scm_foreign_object_signed_ref (SCM obj, size_t n)
{
  scm_t_bits bits = scm_foreign_object_unsigned_ref (obj, n);
  return (scm_t_signed_bits) bits;
}

void
scm_foreign_object_signed_set_x (SCM obj, size_t n, scm_t_signed_bits val)
{
  scm_t_bits bits = (scm_t_bits) val;
  scm_foreign_object_unsigned_set_x (obj, n, bits);
}

void*
scm_foreign_object_ref (SCM obj, size_t n)
{
  scm_t_bits bits = scm_foreign_object_unsigned_ref (obj, n);
  return (void *) bits;
}

void
scm_foreign_object_set_x (SCM obj, size_t n, void *val)
{
  scm_t_bits bits = (scm_t_bits) val;
  scm_foreign_object_unsigned_set_x (obj, n, bits);
}
  
static void
invoke_finalizer (void *obj, void *data)
{
  scm_call_1 (PTR2SCM (data), PTR2SCM (obj));
}

static SCM
sys_add_finalizer_x (SCM obj, SCM finalizer)
#define FUNC_NAME "%add-finalizer!"
{
  SCM_VALIDATE_PROC (SCM_ARG2, finalizer);

  scm_i_add_finalizer (SCM2PTR (obj), invoke_finalizer, SCM2PTR (finalizer));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
scm_init_foreign_object (void)
{
  scm_c_define_gsubr ("%add-finalizer!", 2, 0, 0,
                      (scm_t_subr) sys_add_finalizer_x);
}

void
scm_register_foreign_object (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_foreign_object",
                            (scm_t_extension_init_func)scm_init_foreign_object,
                            NULL);
}

/* srfi-4.c --- Uniform numeric vector datatypes.
 *
 * 	Copyright (C) 2001, 2004, 2006, 2009, 2010, 2014 Free Software Foundation, Inc.
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
#include "libguile/__scm.h"
#include "libguile/bdw-gc.h"
#include "libguile/srfi-4.h"
#include "libguile/bytevectors.h"
#include "libguile/error.h"
#include "libguile/eval.h"
#include "libguile/extensions.h"
#include "libguile/uniform.h"
#include "libguile/generalized-vectors.h"
#include "libguile/validate.h"


#define DEFINE_SCHEME_PROXY100(cname, modname, scmname)                   \
  SCM cname (SCM arg1)                                                  \
  {                                                                     \
    static SCM var = SCM_BOOL_F;                                        \
    if (scm_is_false (var))                                             \
      var = scm_c_module_lookup (scm_c_resolve_module (modname), scmname); \
    return scm_call_1 (SCM_VARIABLE_REF (var), arg1);                   \
  }

#define DEFINE_SCHEME_PROXY001(cname, modname, scmname)                 \
  SCM cname (SCM args)                                                  \
  {                                                                     \
    static SCM var = SCM_BOOL_F;                                        \
    if (scm_is_false (var))                                             \
      var = scm_c_module_lookup (scm_c_resolve_module (modname), scmname); \
    return scm_apply_0 (SCM_VARIABLE_REF (var), args);                  \
  }

#define DEFINE_SCHEME_PROXY110(cname, modname, scmname)                   \
  SCM cname (SCM arg1, SCM opt1)                                        \
  {                                                                     \
    static SCM var = SCM_BOOL_F;                                        \
    if (scm_is_false (var))                                             \
      var = scm_c_module_lookup (scm_c_resolve_module (modname), scmname); \
    if (SCM_UNBNDP (opt1))                                              \
      return scm_call_1 (SCM_VARIABLE_REF (var), arg1);                 \
    else                                                                \
      return scm_call_2 (SCM_VARIABLE_REF (var), arg1, opt1);           \
  }

#define DEFINE_SCHEME_PROXY200(cname, modname, scmname)                   \
  SCM cname (SCM arg1, SCM arg2)                                        \
  {                                                                     \
    static SCM var = SCM_BOOL_F;                                        \
    if (scm_is_false (var))                                             \
      var = scm_c_module_lookup (scm_c_resolve_module (modname), scmname); \
    return scm_call_2 (SCM_VARIABLE_REF (var), arg1, arg2);             \
  }

#define DEFINE_SCHEME_PROXY300(cname, modname, scmname)                   \
  SCM cname (SCM arg1, SCM arg2, SCM arg3)                              \
  {                                                                     \
    static SCM var = SCM_BOOL_F;                                        \
    if (scm_is_false (var))                                             \
      var = scm_c_module_lookup (scm_c_resolve_module (modname), scmname); \
    return scm_call_3 (SCM_VARIABLE_REF (var), arg1, arg2, arg3);       \
  }

#define DEFPROXY100(cname, scmname)               \
  DEFINE_SCHEME_PROXY100 (cname, MOD, scmname)
#define DEFPROXY110(cname, scmname)               \
  DEFINE_SCHEME_PROXY110 (cname, MOD, scmname)
#define DEFPROXY001(cname, scmname)               \
  DEFINE_SCHEME_PROXY001 (cname, MOD, scmname)
#define DEFPROXY200(cname, scmname)               \
  DEFINE_SCHEME_PROXY200 (cname, MOD, scmname)
#define DEFPROXY300(cname, scmname)               \
  DEFINE_SCHEME_PROXY300 (cname, MOD, scmname)

#define DEFVECT(sym, str, func)\

#define DEFINE_SRFI_4_PROXIES(tag)                                      \
  DEFPROXY100 (scm_##tag##vector_p, #tag "vector?");                    \
  DEFPROXY110 (scm_make_##tag##vector, "make-" #tag "vector");          \
  DEFPROXY001 (scm_##tag##vector, #tag "vector");                       \
  DEFPROXY100 (scm_##tag##vector_length, #tag "vector-length");         \
  DEFPROXY200 (scm_##tag##vector_ref, #tag "vector-ref");               \
  DEFPROXY300 (scm_##tag##vector_set_x, #tag "vector-set!");            \
  DEFPROXY100 (scm_list_to_##tag##vector, "list->"#tag "vector");       \
  DEFPROXY100 (scm_##tag##vector_to_list, #tag "vector->list");         \
  
  
#define ETYPE(TAG) \
  SCM_ARRAY_ELEMENT_TYPE_##TAG

#define DEFINE_SRFI_4_C_FUNCS(TAG, tag, ctype, width)                   \
  SCM scm_take_##tag##vector (ctype *data, size_t n)                    \
  {                                                                     \
    return scm_c_take_typed_bytevector ((scm_t_int8*)data, n, ETYPE (TAG));   \
  }                                                                     \
  const ctype* scm_array_handle_##tag##_elements (scm_t_array_handle *h) \
  {                                                                     \
    if (h->element_type != ETYPE (TAG))                                 \
      scm_wrong_type_arg_msg (NULL, 0, h->array, #tag "vector");        \
    return ((const ctype*) h->elements) + h->base*width;                \
  }                                                                     \
  ctype* scm_array_handle_##tag##_writable_elements (scm_t_array_handle *h) \
  {                                                                     \
    if (h->element_type != ETYPE (TAG))                                 \
      scm_wrong_type_arg_msg (NULL, 0, h->array, #tag "vector");        \
    return ((ctype*) h->writable_elements) + h->base*width;             \
  }                                                                     \
  const ctype *scm_##tag##vector_elements (SCM uvec,                    \
                                           scm_t_array_handle *h,       \
                                           size_t *lenp, ssize_t *incp) \
  {                                                                     \
    return scm_##tag##vector_writable_elements (uvec, h, lenp, incp);   \
  }                                                                     \
  ctype *scm_##tag##vector_writable_elements (SCM uvec,                 \
                                              scm_t_array_handle *h,    \
                                              size_t *lenp, ssize_t *incp) \
  {                                                                     \
    size_t byte_width = width * sizeof (ctype);                         \
    if (!scm_is_bytevector (uvec)                                       \
        || (scm_c_bytevector_length (uvec) % byte_width))               \
      scm_wrong_type_arg_msg (NULL, 0, uvec, #tag "vector");            \
    scm_array_get_handle (uvec, h);                                     \
    if (lenp)                                                           \
      *lenp = scm_c_bytevector_length (uvec) / byte_width;              \
    if (incp)                                                           \
      *incp = 1;                                                        \
    return ((ctype *)h->writable_elements);                             \
  }


#define MOD "srfi srfi-4"

DEFINE_SRFI_4_PROXIES (u8);
DEFINE_SRFI_4_C_FUNCS (U8, u8, scm_t_uint8, 1);

DEFINE_SRFI_4_PROXIES (s8);
DEFINE_SRFI_4_C_FUNCS (S8, s8, scm_t_int8, 1);

DEFINE_SRFI_4_PROXIES (u16);
DEFINE_SRFI_4_C_FUNCS (U16, u16, scm_t_uint16, 1);

DEFINE_SRFI_4_PROXIES (s16);
DEFINE_SRFI_4_C_FUNCS (S16, s16, scm_t_int16, 1);

DEFINE_SRFI_4_PROXIES (u32);
DEFINE_SRFI_4_C_FUNCS (U32, u32, scm_t_uint32, 1);

DEFINE_SRFI_4_PROXIES (s32);
DEFINE_SRFI_4_C_FUNCS (S32, s32, scm_t_int32, 1);

DEFINE_SRFI_4_PROXIES (u64);
DEFINE_SRFI_4_C_FUNCS (U64, u64, scm_t_uint64, 1);

DEFINE_SRFI_4_PROXIES (s64);
DEFINE_SRFI_4_C_FUNCS (S64, s64, scm_t_int64, 1);

DEFINE_SRFI_4_PROXIES (f32);
DEFINE_SRFI_4_C_FUNCS (F32, f32, float, 1);

DEFINE_SRFI_4_PROXIES (f64);
DEFINE_SRFI_4_C_FUNCS (F64, f64, double, 1);

#undef MOD
#define MOD "srfi srfi-4 gnu"

DEFINE_SRFI_4_PROXIES (c32);
DEFINE_SRFI_4_C_FUNCS (C32, c32, float, 2);

DEFINE_SRFI_4_PROXIES (c64);
DEFINE_SRFI_4_C_FUNCS (C64, c64, double, 2);

#define DEFINE_SRFI_4_GNU_PROXIES(tag)                              \
  DEFPROXY100 (scm_any_to_##tag##vector, "any->" #tag "vector")

#undef MOD
#define MOD "srfi srfi-4 gnu"
DEFINE_SRFI_4_GNU_PROXIES (u8);
DEFINE_SRFI_4_GNU_PROXIES (s8);
DEFINE_SRFI_4_GNU_PROXIES (u16);
DEFINE_SRFI_4_GNU_PROXIES (s16);
DEFINE_SRFI_4_GNU_PROXIES (u32);
DEFINE_SRFI_4_GNU_PROXIES (s32);
DEFINE_SRFI_4_GNU_PROXIES (u64);
DEFINE_SRFI_4_GNU_PROXIES (s64);
DEFINE_SRFI_4_GNU_PROXIES (f32);
DEFINE_SRFI_4_GNU_PROXIES (f64);
DEFINE_SRFI_4_GNU_PROXIES (c32);
DEFINE_SRFI_4_GNU_PROXIES (c64);


SCM_DEFINE (scm_make_srfi_4_vector, "make-srfi-4-vector", 2, 1, 0,
            (SCM type, SCM len, SCM fill),
            "Make a srfi-4 vector")
#define FUNC_NAME s_scm_make_srfi_4_vector
{
  int c_type;
  size_t c_len;

  for (c_type = 0; c_type <= SCM_ARRAY_ELEMENT_TYPE_LAST; c_type++)
    if (scm_is_eq (type, scm_i_array_element_types[c_type]))
      break;
  if (c_type > SCM_ARRAY_ELEMENT_TYPE_LAST)
    scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARG1, type, "vector type");
  switch (c_type)
    {
    case SCM_ARRAY_ELEMENT_TYPE_U8:
    case SCM_ARRAY_ELEMENT_TYPE_S8:
    case SCM_ARRAY_ELEMENT_TYPE_U16:
    case SCM_ARRAY_ELEMENT_TYPE_S16:
    case SCM_ARRAY_ELEMENT_TYPE_U32:
    case SCM_ARRAY_ELEMENT_TYPE_S32:
    case SCM_ARRAY_ELEMENT_TYPE_U64:
    case SCM_ARRAY_ELEMENT_TYPE_S64:
    case SCM_ARRAY_ELEMENT_TYPE_F32:
    case SCM_ARRAY_ELEMENT_TYPE_F64:
    case SCM_ARRAY_ELEMENT_TYPE_C32:
    case SCM_ARRAY_ELEMENT_TYPE_C64:
      {
        SCM ret;

        c_len = scm_to_size_t (len);
        ret = scm_i_make_typed_bytevector (c_len, c_type);

        if (SCM_UNBNDP (fill) || scm_is_eq (len, SCM_INUM0))
          ; /* pass */
        else if (scm_is_true (scm_zero_p (fill)))
          memset (SCM_BYTEVECTOR_CONTENTS (ret), 0,
                  SCM_BYTEVECTOR_LENGTH (ret));
        else
          {
            scm_t_array_handle h;
            size_t i;

            scm_array_get_handle (ret, &h);
            for (i = 0; i < c_len; i++)
              scm_array_handle_set (&h, i, fill);
            scm_array_handle_release (&h);
          }
        return ret;
      }
    default:
      scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARG1, type, "uniform vector type");
      return SCM_BOOL_F; /* not reached */
    }
}
#undef FUNC_NAME

void
scm_init_srfi_4 (void)
{
#define REGISTER(tag, TAG)                                       \
  scm_i_register_vector_constructor                              \
    (scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_##TAG],    \
     scm_make_##tag##vector)

  REGISTER (u8, U8); 
  REGISTER (s8, S8); 
  REGISTER (u16, U16);
  REGISTER (s16, S16);
  REGISTER (u32, U32);
  REGISTER (s32, S32);
  REGISTER (u64, U64);
  REGISTER (s64, S64);
  REGISTER (f32, F32);
  REGISTER (f64, F64);
  REGISTER (c32, C32);
  REGISTER (c64, C64);

#include "libguile/srfi-4.x"
}

/* End of srfi-4.c.  */

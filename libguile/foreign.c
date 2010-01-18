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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include "libguile/_scm.h"
#include "libguile/bytevectors.h"
#include "libguile/foreign.h"



SCM_SYMBOL (sym_void, "void");
SCM_SYMBOL (sym_float, "float");
SCM_SYMBOL (sym_double, "double");
SCM_SYMBOL (sym_uint8, "uint8");
SCM_SYMBOL (sym_int8, "int8");
SCM_SYMBOL (sym_uint16, "uint16");
SCM_SYMBOL (sym_int16, "int16");
SCM_SYMBOL (sym_uint32, "uint32");
SCM_SYMBOL (sym_int32, "int32");
SCM_SYMBOL (sym_uint64, "uint64");
SCM_SYMBOL (sym_int64, "int64");

static SCM foreign_weak_refs = SCM_BOOL_F;

static void
register_weak_reference (SCM from, SCM to)
{
  scm_hashq_set_x (foreign_weak_refs, from, to);
}
    
static void
foreign_finalizer_trampoline (GC_PTR ptr, GC_PTR data)
{
  scm_t_foreign_finalizer finalizer = data;
  finalizer (SCM_FOREIGN_POINTER (PTR2SCM (ptr), void));
}

SCM
scm_take_foreign_pointer (scm_t_foreign_type type, void *ptr, size_t len,
                          scm_t_foreign_finalizer finalizer)
{
  SCM ret;
  scm_t_bits word0;
    
  word0 = (scm_t_bits)(scm_tc7_foreign | (type<<8)
                       | (finalizer ? (1<<16) : 0) | (len<<17));
  if (SCM_UNLIKELY ((word0 >> 16) != len))
    scm_out_of_range ("scm_take_foreign_pointer", scm_from_size_t (len));
    
  ret = PTR2SCM (scm_gc_malloc_pointerless (sizeof (scm_t_bits) * 2,
                                            "foreign"));
  SCM_SET_CELL_WORD_0 (ret, word0);
  SCM_SET_CELL_WORD_1 (ret, (scm_t_bits)ptr);

  if (finalizer)
    {
      /* Register a finalizer for the newly created instance.  */
      GC_finalization_proc prev_finalizer;
      GC_PTR prev_finalizer_data;
      GC_REGISTER_FINALIZER_NO_ORDER (SCM2PTR (ret),
                                      foreign_finalizer_trampoline,
                                      finalizer,
                                      &prev_finalizer,
                                      &prev_finalizer_data);
    }

  return ret;
}

SCM_DEFINE (scm_foreign_ref, "foreign-ref", 1, 0, 0,
	    (SCM foreign),
	    "Reference the foreign value wrapped by @var{foreign}.\n\n"
            "The value will be referenced according to its type.")
#define FUNC_NAME s_scm_foreign_ref
{
  scm_t_foreign_type ftype;
  scm_t_uint8 *ptr;

  SCM_VALIDATE_FOREIGN (1, foreign);
  ptr = SCM_FOREIGN_POINTER (foreign, scm_t_uint8);
  ftype = SCM_FOREIGN_TYPE (foreign);
  
  /* FIXME: is there a window in which we can see ptr but not foreign? */
  /* FIXME: accessing unaligned pointers */
  switch (ftype)
    {
    case SCM_FOREIGN_TYPE_VOID:
      return scm_from_ulong ((unsigned long)ptr);
    case SCM_FOREIGN_TYPE_FLOAT:
      return scm_from_double (*(float*)ptr);
    case SCM_FOREIGN_TYPE_DOUBLE:
      return scm_from_double (*(double*)ptr);
    case SCM_FOREIGN_TYPE_UINT8:
      return scm_from_uint8 (*(scm_t_uint8*)ptr);
    case SCM_FOREIGN_TYPE_INT8:
      return scm_from_int8 (*(scm_t_int8*)ptr);
    case SCM_FOREIGN_TYPE_UINT16:
      return scm_from_uint16 (*(scm_t_uint16*)ptr);
    case SCM_FOREIGN_TYPE_INT16:
      return scm_from_int16 (*(scm_t_int16*)ptr);
    case SCM_FOREIGN_TYPE_UINT32:
      return scm_from_uint32 (*(scm_t_uint32*)ptr);
    case SCM_FOREIGN_TYPE_INT32:
      return scm_from_int32 (*(scm_t_int32*)ptr);
    case SCM_FOREIGN_TYPE_UINT64:
      return scm_from_uint64 (*(scm_t_uint64*)ptr);
    case SCM_FOREIGN_TYPE_INT64:
      return scm_from_int64 (*(scm_t_int64*)ptr);
    default:
      abort ();
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_foreign_set_x, "foreign-set!", 2, 0, 0,
	    (SCM foreign, SCM val),
	    "Set the foreign value wrapped by @var{foreign}.\n\n"
            "The value will be set according to its type.")
#define FUNC_NAME s_scm_foreign_set_x
{
  scm_t_foreign_type ftype;
  scm_t_uint8 *ptr;

  SCM_VALIDATE_FOREIGN (1, foreign);
  ptr = SCM_FOREIGN_POINTER (foreign, scm_t_uint8);
  ftype = SCM_FOREIGN_TYPE (foreign);

  /* FIXME: is there a window in which we can see ptr but not foreign? */
  /* FIXME: unaligned access */
  switch (ftype)
    {
    case SCM_FOREIGN_TYPE_VOID:
      SCM_SET_CELL_WORD_1 (foreign, scm_to_ulong (val));
      break;
    case SCM_FOREIGN_TYPE_FLOAT:
      *(float*)ptr = scm_to_double (val);
      break;
    case SCM_FOREIGN_TYPE_DOUBLE:
      *(double*)ptr = scm_to_double (val);
      break;
    case SCM_FOREIGN_TYPE_UINT8:
      *(scm_t_uint8*)ptr = scm_to_uint8 (val);
      break;
    case SCM_FOREIGN_TYPE_INT8:
      *(scm_t_int8*)ptr = scm_to_int8 (val);
      break;
    case SCM_FOREIGN_TYPE_UINT16:
      *(scm_t_uint16*)ptr = scm_to_uint16 (val);
      break;
    case SCM_FOREIGN_TYPE_INT16:
      *(scm_t_int16*)ptr = scm_to_int16 (val);
      break;
    case SCM_FOREIGN_TYPE_UINT32:
      *(scm_t_uint32*)ptr = scm_to_uint32 (val);
      break;
    case SCM_FOREIGN_TYPE_INT32:
      *(scm_t_int32*)ptr = scm_to_int32 (val);
      break;
    case SCM_FOREIGN_TYPE_UINT64:
      *(scm_t_uint64*)ptr = scm_to_uint64 (val);
      break;
    case SCM_FOREIGN_TYPE_INT64:
      *(scm_t_int64*)ptr = scm_to_int64 (val);
      break;
    default:
      abort ();
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_foreign_to_bytevector, "foreign->bytevector", 1, 3, 0,
	    (SCM foreign, SCM uvec_type, SCM offset, SCM len),
	    "Return a bytevector aliasing the memory pointed to by\n"
            "@var{foreign}.\n\n"
            "@var{foreign} must be a void pointer, a foreign whose type is\n"
            "@var{void}. By default, the resulting bytevector will alias\n"
            "all of the memory pointed to by @var{foreign}, from beginning\n"
            "to end, treated as a @code{vu8} array.\n\n"
            "The user may specify an alternate default interpretation for\n"
            "the memory by passing the @var{uvec_type} argument, to indicate\n"
            "that the memory is an array of elements of that type.\n"
            "@var{uvec_type} should be something that\n"
            "@code{uniform-vector-element-type} would return, like @code{f32}\n"
            "or @code{s16}.\n\n"
            "Users may also specify that the bytevector should only alias a\n"
            "subset of the memory, by specifying @var{offset} and @var{len}\n"
            "arguments.")
#define FUNC_NAME s_scm_foreign_to_bytevector
{
  SCM ret;
  scm_t_int8 *ptr;
  size_t boffset, blen;
  scm_t_array_element_type btype;

  SCM_VALIDATE_FOREIGN_TYPED (1, foreign, VOID);
  ptr = SCM_FOREIGN_POINTER (foreign, scm_t_int8);
  
  if (SCM_UNBNDP (uvec_type))
    btype = SCM_ARRAY_ELEMENT_TYPE_VU8;
  else
    {
      int i;
      for (i = 0; i <= SCM_ARRAY_ELEMENT_TYPE_LAST; i++)
        if (scm_is_eq (uvec_type, scm_i_array_element_types[i]))
          break;
      switch (i)
        {
        case SCM_ARRAY_ELEMENT_TYPE_VU8:
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
          btype = i;
          break;
        default:
          scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARG1, uvec_type,
                                  "uniform vector type");
        }
    }
  
  if (SCM_UNBNDP (offset))
    boffset = 0;
  else if (SCM_FOREIGN_LEN (foreign))
    boffset = scm_to_unsigned_integer (offset, 0,
                                       SCM_FOREIGN_LEN (foreign) - 1);
  else
    boffset = scm_to_size_t (offset);

  if (SCM_UNBNDP (len))
    {
      if (SCM_FOREIGN_LEN (foreign))
        blen = SCM_FOREIGN_LEN (foreign) - boffset;
      else
        scm_misc_error (FUNC_NAME,
                        "length needed to convert foreign pointer to bytevector",
                        SCM_EOL);
    }
  else
    {
      if (SCM_FOREIGN_LEN (foreign))
        blen = scm_to_unsigned_integer (len, 0,
                                        SCM_FOREIGN_LEN (foreign) - boffset);
      else
        blen = scm_to_size_t (len);
    }

  ret = scm_c_take_typed_bytevector (ptr + boffset, blen, btype);
  register_weak_reference (ret, foreign);
  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_to_foreign, "bytevector->foreign", 1, 2, 0,
	    (SCM bv, SCM offset, SCM len),
	    "Return a foreign pointer aliasing the memory pointed to by\n"
            "@var{bv}.\n\n"
            "The resulting foreign will be a void pointer, a foreign whose\n"
            "type is @code{void}. By default it will alias all of the\n"
            "memory pointed to by @var{bv}, from beginning to end.\n\n"
            "Users may explicily specify that the foreign should only alias a\n"
            "subset of the memory, by specifying @var{offset} and @var{len}\n"
            "arguments.")
#define FUNC_NAME s_scm_bytevector_to_foreign
{
  SCM ret;
  scm_t_int8 *ptr;
  size_t boffset, blen;

  SCM_VALIDATE_BYTEVECTOR (1, bv);
  ptr = SCM_BYTEVECTOR_CONTENTS (bv);
  
  if (SCM_UNBNDP (offset))
    boffset = 0;
  else
    boffset = scm_to_unsigned_integer (offset, 0,
                                       SCM_BYTEVECTOR_LENGTH (bv) - 1);

  if (SCM_UNBNDP (len))
    blen = SCM_BYTEVECTOR_LENGTH (bv) - boffset;
  else
    blen = scm_to_unsigned_integer (len, 0,
                                    SCM_BYTEVECTOR_LENGTH (bv) - boffset);

  ret = scm_take_foreign_pointer (SCM_FOREIGN_TYPE_VOID, ptr + boffset, blen,
                                  NULL);
  register_weak_reference (ret, bv);
  return ret;
}
#undef FUNC_NAME

void
scm_i_foreign_print (SCM foreign, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<foreign ", port);
  switch (SCM_FOREIGN_TYPE (foreign))
    {
    case SCM_FOREIGN_TYPE_FLOAT:
      scm_puts ("float ", port);
      break;
    case SCM_FOREIGN_TYPE_DOUBLE:
      scm_puts ("double ", port);
      break;
    case SCM_FOREIGN_TYPE_UINT8:
      scm_puts ("uint8 ", port);
      break;
    case SCM_FOREIGN_TYPE_INT8:
      scm_puts ("int8 ", port);
      break;
    case SCM_FOREIGN_TYPE_UINT16:
      scm_puts ("uint16 ", port);
      break;
    case SCM_FOREIGN_TYPE_INT16:
      scm_puts ("int16 ", port);
      break;
    case SCM_FOREIGN_TYPE_UINT32:
      scm_puts ("uint32 ", port);
      break;
    case SCM_FOREIGN_TYPE_INT32:
      scm_puts ("int32 ", port);
      break;
    case SCM_FOREIGN_TYPE_UINT64:
      scm_puts ("uint64 ", port);
      break;
    case SCM_FOREIGN_TYPE_INT64:
      scm_puts ("int64 ", port);
      break;
    case SCM_FOREIGN_TYPE_VOID:
      scm_puts ("pointer ", port);
      break;
    default:
      abort ();
    }
  scm_display (scm_foreign_ref (foreign), port);
  scm_putc ('>', port);
}



static void
scm_init_foreign (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/foreign.x"
#endif
  scm_define (sym_void, scm_from_uint8 (SCM_FOREIGN_TYPE_VOID));
  scm_define (sym_float, scm_from_uint8 (SCM_FOREIGN_TYPE_FLOAT));
  scm_define (sym_double, scm_from_uint8 (SCM_FOREIGN_TYPE_DOUBLE));
  scm_define (sym_uint8, scm_from_uint8 (SCM_FOREIGN_TYPE_UINT8));
  scm_define (sym_int8, scm_from_uint8 (SCM_FOREIGN_TYPE_INT8));
  scm_define (sym_uint16, scm_from_uint8 (SCM_FOREIGN_TYPE_UINT16));
  scm_define (sym_int16, scm_from_uint8 (SCM_FOREIGN_TYPE_INT16));
  scm_define (sym_uint32, scm_from_uint8 (SCM_FOREIGN_TYPE_UINT32));
  scm_define (sym_int32, scm_from_uint8 (SCM_FOREIGN_TYPE_INT32));
  scm_define (sym_uint64, scm_from_uint8 (SCM_FOREIGN_TYPE_UINT64));
  scm_define (sym_int64, scm_from_uint8 (SCM_FOREIGN_TYPE_INT64));
}

void
scm_register_foreign (void)
{
  scm_c_register_extension ("libguile", "scm_init_foreign",
                            (scm_t_extension_init_func)scm_init_foreign,
                            NULL);
  foreign_weak_refs = scm_make_weak_key_hash_table (SCM_UNDEFINED);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

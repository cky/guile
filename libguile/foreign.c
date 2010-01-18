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

static void
keepalive (GC_PTR obj, GC_PTR data)
{
}

SCM_DEFINE (scm_foreign_ref, "foreign-ref", 1, 3, 0,
	    (SCM foreign, SCM type, SCM offset, SCM len),
	    "Reference the foreign value wrapped by @var{foreign}.\n\n"
            "The value will be referenced according to its type.\n"
            "If and only if the type of the foreign value is @code{void},\n"
            "this function accepts optional @var{type} and @var{offset}\n"
            "arguments, indicating that the pointer wrapped by\n"
            "@var{foreign} should be incremented by @var{offset} bytes,\n"
            "and treated as a pointer to a value of the given @var{type}.\n"
            "@var{offset} defaults to 0.\n\n"
            "If @var{type} itself is @code{void}, @var{len} will be used\n"
            "to specify the size of the resulting @code{void} pointer.")
#define FUNC_NAME s_scm_foreign_ref
{
  scm_t_foreign_type ftype;
  scm_t_uint8 *ptr;

  SCM_VALIDATE_FOREIGN (1, foreign);
  ptr = SCM_FOREIGN_POINTER (foreign, scm_t_uint8);

  ftype = SCM_FOREIGN_TYPE (foreign);
  if (ftype == SCM_FOREIGN_TYPE_VOID)
    {
      if (SCM_UNBNDP (type))
        scm_error_num_args_subr (FUNC_NAME);
      ftype = scm_to_unsigned_integer (type, 0, SCM_FOREIGN_TYPE_LAST);
      if (!SCM_UNBNDP (offset))
        ptr += scm_to_ssize_t (offset);
    }
  else
    {
      if (!SCM_UNBNDP (type))
        scm_error_num_args_subr (FUNC_NAME);
    }
  
  /* FIXME: is there a window in which we can see ptr but not foreign? */
  switch (ftype)
    {
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
    case SCM_FOREIGN_TYPE_VOID:
      /* seems we're making a new pointer, woo */
      {
        GC_finalization_proc prev_finalizer;
        GC_PTR prev_finalizer_data;
        SCM ret = scm_take_foreign_pointer
          (ftype, ptr, SCM_UNBNDP (len) ? 0 : scm_to_size_t (len), NULL);
        /* while the kid is alive, keep the parent alive */
        if (SCM_FOREIGN_HAS_FINALIZER (foreign))
          GC_REGISTER_FINALIZER_NO_ORDER (SCM2PTR (ret), keepalive, foreign,
                                          &prev_finalizer, &prev_finalizer_data);
        return ret;
      }
    default:
      abort ();
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_foreign_set_x, "foreign-set!", 2, 2, 0,
	    (SCM foreign, SCM val, SCM type, SCM offset),
	    "Set the foreign value wrapped by @var{foreign}.\n\n"
            "The value will be set according to its type.\n"
            "If and only if the type of the foreign value is @code{void},\n"
            "this function accepts optional @var{type} and @var{offset}\n"
            "arguments, indicating that the pointer wrapped by\n"
            "@var{foreign} should be incremented by @var{offset} bytes,\n"
            "and treated as a pointer to a value of the given @var{type}.\n"
            "@var{offset} defaults to 0.")
#define FUNC_NAME s_scm_foreign_set_x
{
  scm_t_foreign_type ftype;
  scm_t_uint8 *ptr;

  SCM_VALIDATE_FOREIGN (1, foreign);
  ptr = SCM_FOREIGN_POINTER (foreign, scm_t_uint8);

  ftype = SCM_FOREIGN_TYPE (foreign);
  if (ftype == SCM_FOREIGN_TYPE_VOID)
    {
      if (SCM_UNBNDP (type))
        scm_error_num_args_subr (FUNC_NAME);
      ftype = scm_to_unsigned_integer (type, 0, SCM_FOREIGN_TYPE_LAST);
      if (!SCM_UNBNDP (offset))
        ptr += scm_to_ssize_t (offset);
    }
  else
    {
      if (!SCM_UNBNDP (type))
        scm_error_num_args_subr (FUNC_NAME);
    }
  
  /* FIXME: is there a window in which we can see ptr but not foreign? */
  switch (ftype)
    {
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
    case SCM_FOREIGN_TYPE_VOID:
      SCM_VALIDATE_FOREIGN (2, val);
      if (SCM_FOREIGN_HAS_FINALIZER (val))
        /* setting a pointer inside one foreign value to the pointer of another?
           that is asking for trouble */
        scm_wrong_type_arg_msg (FUNC_NAME, 2, val,
                                "foreign value without finalizer");
      *(void**)ptr = SCM_FOREIGN_POINTER (val, void*);
      break;
    default:
      abort ();
    }

  return SCM_UNSPECIFIED;
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
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_DOUBLE:
      scm_puts ("double ", port);
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_UINT8:
      scm_puts ("uint8 ", port);
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_INT8:
      scm_puts ("int8 ", port);
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_UINT16:
      scm_puts ("uint16 ", port);
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_INT16:
      scm_puts ("int16 ", port);
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_UINT32:
      scm_puts ("uint32 ", port);
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_INT32:
      scm_puts ("int32 ", port);
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_UINT64:
      scm_puts ("uint64 ", port);
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_INT64:
      scm_puts ("int64 ", port);
      scm_display (scm_foreign_ref (foreign, SCM_UNDEFINED, SCM_UNDEFINED,
                                    SCM_UNDEFINED),
                   port);
      break;
    case SCM_FOREIGN_TYPE_VOID:
      scm_puts ("pointer 0x", port);
      scm_uintprint ((scm_t_bits)SCM_FOREIGN_POINTER (foreign, void), 16, port);
      break;
    default:
      abort ();
    }
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
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

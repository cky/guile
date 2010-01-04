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
#include "_scm.h"
#include "foreign.h"



static size_t
sizeof_type (scm_t_foreign_type type)
{
  switch (type)
    {
    case SCM_FOREIGN_TYPE_VOID:    abort ();
    case SCM_FOREIGN_TYPE_FLOAT:   return sizeof(float);
    case SCM_FOREIGN_TYPE_DOUBLE:  return sizeof(double);
    case SCM_FOREIGN_TYPE_UINT8:   return sizeof(scm_t_uint8);
    case SCM_FOREIGN_TYPE_INT8:    return sizeof(scm_t_int8);
    case SCM_FOREIGN_TYPE_UINT16:  return sizeof(scm_t_uint16);
    case SCM_FOREIGN_TYPE_INT16:   return sizeof(scm_t_int16);
    case SCM_FOREIGN_TYPE_UINT32:  return sizeof(scm_t_uint32);
    case SCM_FOREIGN_TYPE_INT32:   return sizeof(scm_t_int32);
    case SCM_FOREIGN_TYPE_UINT64:  return sizeof(scm_t_uint64);
    case SCM_FOREIGN_TYPE_INT64:   return sizeof(scm_t_int64);
    case SCM_FOREIGN_TYPE_STRUCT:  abort ();
    case SCM_FOREIGN_TYPE_POINTER: return sizeof(void*);
    default:                       abort ();
    }
}


static void
foreign_finalizer_trampoline (GC_PTR ptr, GC_PTR data)
{
  scm_t_foreign_finalizer finalizer = data;
  finalizer (SCM_FOREIGN_OBJECT (PTR2SCM (ptr), void*));
}

SCM
scm_c_from_foreign (scm_t_foreign_type type, void *val, size_t size,
                    scm_t_foreign_finalizer finalizer)
{
  void *ret;
  if (!size)
    size = sizeof_type (type);
    
  ret = scm_gc_malloc_pointerless (sizeof (scm_t_bits) * 2 + size, "foreign");
  SCM_SET_CELL_WORD_0 (PTR2SCM (ret), scm_tc7_foreign | (type<<8));

  /* set SCM_FOREIGN_OBJECT to point to the third word of the object, which will
     be 8-byte aligned. Then copy *val into that space. */
  SCM_SET_CELL_WORD_1 (PTR2SCM (ret),
                       (scm_t_bits)SCM_CELL_OBJECT_LOC (PTR2SCM (ret), 2));
  memcpy (SCM_FOREIGN_OBJECT (PTR2SCM (ret), void), val, size);

  if (finalizer)
    {
      /* Register a finalizer for the newly created instance.  */
      GC_finalization_proc prev_finalizer;
      GC_PTR prev_finalizer_data;
      GC_REGISTER_FINALIZER_NO_ORDER (ret,
                                      foreign_finalizer_trampoline,
                                      finalizer,
                                      &prev_finalizer,
                                      &prev_finalizer_data);
    }

  return PTR2SCM (ret);
}

SCM
scm_c_take_foreign (scm_t_foreign_type type, void *val,
                    scm_t_foreign_finalizer finalizer)
{
  void *ret;
    
  ret = scm_gc_malloc_pointerless (sizeof (scm_t_bits) * 2, "foreign");
  SCM_SET_CELL_WORD_0 (PTR2SCM (ret), scm_tc7_foreign | (type<<8));
  /* Set SCM_FOREIGN_OBJECT to the given pointer. */
  SCM_SET_CELL_WORD_1 (PTR2SCM (ret), (scm_t_bits)val);

  if (finalizer)
    {
      /* Register a finalizer for the newly created instance.  */
      GC_finalization_proc prev_finalizer;
      GC_PTR prev_finalizer_data;
      GC_REGISTER_FINALIZER_NO_ORDER (ret,
                                      foreign_finalizer_trampoline,
                                      finalizer,
                                      &prev_finalizer,
                                      &prev_finalizer_data);
    }

  return PTR2SCM (ret);
}

SCM_DEFINE (scm_foreign_ref, "foreign-ref", 1, 0, 0,
	    (SCM foreign),
	    "Reference the foreign value wrapped by @var{foreign}.\n\n"
            "Note that only \"simple\" types may be referenced by this\n"
            "function. See @code{foreign-struct-ref} or @code{foreign-pointer-ref}\n"
            "for structs or pointers, respectively.")
#define FUNC_NAME s_scm_foreign_ref
{
  SCM_VALIDATE_FOREIGN_SIMPLE (1, foreign);

  switch (SCM_FOREIGN_TYPE (foreign))
    {
    case SCM_FOREIGN_TYPE_FLOAT:
      return scm_from_double (SCM_FOREIGN_OBJECT_REF (foreign, float));
    case SCM_FOREIGN_TYPE_DOUBLE:
      return scm_from_double (SCM_FOREIGN_OBJECT_REF (foreign, double));
    case SCM_FOREIGN_TYPE_UINT8:
      return scm_from_uint8 (SCM_FOREIGN_OBJECT_REF (foreign, scm_t_uint8));
    case SCM_FOREIGN_TYPE_INT8:
      return scm_from_int8 (SCM_FOREIGN_OBJECT_REF (foreign, scm_t_int8));
    case SCM_FOREIGN_TYPE_UINT16:
      return scm_from_uint16 (SCM_FOREIGN_OBJECT_REF (foreign, scm_t_uint16));
    case SCM_FOREIGN_TYPE_INT16:
      return scm_from_int16 (SCM_FOREIGN_OBJECT_REF (foreign, scm_t_int16));
    case SCM_FOREIGN_TYPE_UINT32:
      return scm_from_uint32 (SCM_FOREIGN_OBJECT_REF (foreign, scm_t_uint32));
    case SCM_FOREIGN_TYPE_INT32:
      return scm_from_int32 (SCM_FOREIGN_OBJECT_REF (foreign, scm_t_int32));
    case SCM_FOREIGN_TYPE_UINT64:
      return scm_from_uint64 (SCM_FOREIGN_OBJECT_REF (foreign, scm_t_uint64));
    case SCM_FOREIGN_TYPE_INT64:
      return scm_from_int64 (SCM_FOREIGN_OBJECT_REF (foreign, scm_t_int64));
    case SCM_FOREIGN_TYPE_VOID:
    case SCM_FOREIGN_TYPE_STRUCT:
    case SCM_FOREIGN_TYPE_POINTER:
    default:
      /* other cases should have been caught by the FOREIGN_SIMPLE check */
      abort ();
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_foreign_set_x, "foreign-set!", 2, 0, 0,
	    (SCM foreign, SCM val),
	    "Set the foreign value wrapped by @var{foreign}.\n\n"
            "Note that only \"simple\" types may be set by this function.\n"
            "See @code{foreign-struct-ref} or @code{foreign-pointer-ref} for\n"
            "structs or pointers, respectively.")
#define FUNC_NAME s_scm_foreign_set_x
{
  SCM_VALIDATE_FOREIGN_SIMPLE (1, foreign);

  switch (SCM_FOREIGN_TYPE (foreign))
    {
    case SCM_FOREIGN_TYPE_FLOAT:
      SCM_FOREIGN_OBJECT_SET (foreign, float, scm_to_double (val));
      break;
    case SCM_FOREIGN_TYPE_DOUBLE:
      SCM_FOREIGN_OBJECT_SET (foreign, double, scm_to_double (val));
      break;
    case SCM_FOREIGN_TYPE_UINT8:
      SCM_FOREIGN_OBJECT_SET (foreign, scm_t_uint8, scm_to_uint8 (val));
      break;
    case SCM_FOREIGN_TYPE_INT8:
      SCM_FOREIGN_OBJECT_SET (foreign, scm_t_int8, scm_to_int8 (val));
      break;
    case SCM_FOREIGN_TYPE_UINT16:
      SCM_FOREIGN_OBJECT_SET (foreign, scm_t_uint16, scm_to_uint16 (val));
      break;
    case SCM_FOREIGN_TYPE_INT16:
      SCM_FOREIGN_OBJECT_SET (foreign, scm_t_int16, scm_to_int16 (val));
      break;
    case SCM_FOREIGN_TYPE_UINT32:
      SCM_FOREIGN_OBJECT_SET (foreign, scm_t_uint32, scm_to_uint32 (val));
      break;
    case SCM_FOREIGN_TYPE_INT32:
      SCM_FOREIGN_OBJECT_SET (foreign, scm_t_int32, scm_to_int32 (val));
      break;
    case SCM_FOREIGN_TYPE_UINT64:
      SCM_FOREIGN_OBJECT_SET (foreign, scm_t_uint64, scm_to_uint64 (val));
      break;
    case SCM_FOREIGN_TYPE_INT64:
      SCM_FOREIGN_OBJECT_SET (foreign, scm_t_int64, scm_to_int64 (val));
      break;
    case SCM_FOREIGN_TYPE_VOID:
    case SCM_FOREIGN_TYPE_STRUCT:
    case SCM_FOREIGN_TYPE_POINTER:
    default:
      /* other cases should have been caught by the FOREIGN_SIMPLE check */
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
    case SCM_FOREIGN_TYPE_VOID:
      abort ();
    case SCM_FOREIGN_TYPE_FLOAT:
      scm_puts ("float ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_DOUBLE:
      scm_puts ("double ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_UINT8:
      scm_puts ("uint8 ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_INT8:
      scm_puts ("int8 ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_UINT16:
      scm_puts ("uint16 ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_INT16:
      scm_puts ("int16 ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_UINT32:
      scm_puts ("uint32 ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_INT32:
      scm_puts ("int32 ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_UINT64:
      scm_puts ("uint64 ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_INT64:
      scm_puts ("int64 ", port);
      scm_display (scm_foreign_ref (foreign), port);
      break;
    case SCM_FOREIGN_TYPE_STRUCT:
      scm_puts ("struct at 0x", port);
      scm_uintprint (SCM_CELL_WORD_1 (foreign), 16, port);
      break;
    case SCM_FOREIGN_TYPE_POINTER:
      scm_puts ("pointer 0x", port);
      scm_uintprint (SCM_FOREIGN_OBJECT_REF (foreign, scm_t_bits), 16, port);
      break;
    default:
      abort ();
    }
  scm_putc ('>', port);
}



void
scm_init_foreign (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/foreign.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

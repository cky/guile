/* Copyright (C) 2001, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#ifndef _SCM_OBJCODES_H_
#define _SCM_OBJCODES_H_

#include <libguile.h>

/* Objcode data should be directly mappable to this C structure.  */
struct scm_objcode
{
  scm_t_uint32 len;             /* the maximum index of base[] */
  scm_t_uint32 metalen;         /* well, i lie. this many bytes at the end of
                                   base[] for metadata */
  /* In C99, we'd have:
     scm_t_uint8 base[];  */
};

/* Return a pointer to the base of objcode OBJ.  */
#define SCM_C_OBJCODE_BASE(obj)				\
  ((scm_t_uint8 *)(obj) + sizeof (struct scm_objcode))

#define SCM_OBJCODE_TYPE_MMAP       (0)
#define SCM_OBJCODE_TYPE_BYTEVECTOR (1)
#define SCM_OBJCODE_TYPE_SLICE      (2)
#define SCM_OBJCODE_TYPE_STATIC     (3)

#define SCM_OBJCODE_P(x)	(SCM_NIMP (x) && SCM_TYP7 (x) == scm_tc7_objcode)
#define SCM_OBJCODE_DATA(x)	((struct scm_objcode *) SCM_CELL_WORD_1 (x))
#define SCM_VALIDATE_OBJCODE(p,x) SCM_MAKE_VALIDATE (p, x, OBJCODE_P)

#define SCM_OBJCODE_LEN(x)	(SCM_OBJCODE_DATA (x)->len)
#define SCM_OBJCODE_META_LEN(x)	(SCM_OBJCODE_DATA (x)->metalen)
#define SCM_OBJCODE_TOTAL_LEN(x) (SCM_OBJCODE_LEN (x) + SCM_OBJCODE_META_LEN (x))
#define SCM_OBJCODE_BASE(x)	(SCM_C_OBJCODE_BASE (SCM_OBJCODE_DATA (x)))

#define SCM_MAKE_OBJCODE_TAG(type, flags) (scm_tc7_objcode | (type << 8) | (flags << 16))
#define SCM_OBJCODE_TYPE(x)	((SCM_CELL_WORD_0 (x) >> 8) & 0xff)
#define SCM_OBJCODE_FLAGS(x)	(SCM_CELL_WORD_0 (x) >> 16)
#define SCM_OBJCODE_IS_MMAP(x)	(SCM_OBJCODE_TYPE (x) == SCM_OBJCODE_TYPE_MMAP)
#define SCM_OBJCODE_IS_BYTEVECTOR(x) (SCM_OBJCODE_TYPE (x) == SCM_OBJCODE_TYPE_BYTEVECTOR)
#define SCM_OBJCODE_IS_SLICE(x) (SCM_OBJCODE_TYPE (x) == SCM_OBJCODE_TYPE_SLICE)
#define SCM_OBJCODE_IS_STATIC(x) (SCM_OBJCODE_TYPE (x) == SCM_OBJCODE_TYPE_STATIC)

#define SCM_OBJCODE_NATIVE_CODE(x) (SCM_CELL_WORD_3 (x))
#define SCM_SET_OBJCODE_NATIVE_CODE(x, code) (SCM_SET_CELL_WORD_3 (x, code))

SCM_API SCM scm_c_make_objcode_slice (SCM parent, const scm_t_uint8 *ptr);
SCM_API SCM scm_load_objcode (SCM file);
SCM_API SCM scm_objcode_p (SCM obj);
SCM_API SCM scm_objcode_meta (SCM objcode);
SCM_API SCM scm_bytecode_to_objcode (SCM bytecode);
SCM_INTERNAL SCM scm_bytecode_to_native_objcode (SCM bytecode);
SCM_API SCM scm_objcode_to_bytecode (SCM objcode);
SCM_API SCM scm_write_objcode (SCM objcode, SCM port);

SCM_INTERNAL void scm_i_objcode_print (SCM objcode, SCM port,
                                       scm_print_state *pstate);
SCM_INTERNAL void scm_bootstrap_objcodes (void);
SCM_INTERNAL void scm_init_objcodes (void);

#endif /* _SCM_OBJCODES_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

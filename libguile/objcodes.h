/* Copyright (C) 2001, 2009 Free Software Foundation, Inc.
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

/* objcode data should be directly mappable to this C structure. */
struct scm_objcode {
  scm_t_uint8 nargs;
  scm_t_uint8 nrest;
  scm_t_uint8 nlocs;
  scm_t_uint8 unused;
  scm_t_uint32 len;             /* the maximum index of base[] */
  scm_t_uint32 metalen;         /* well, i lie. this many bytes at the end of
                                   base[] for metadata */
  scm_t_uint8 base[0];
};

#define SCM_F_OBJCODE_IS_MMAP     (1<<0)
#define SCM_F_OBJCODE_IS_U8VECTOR (1<<1)
#define SCM_F_OBJCODE_IS_SLICE    (1<<2)

SCM_API scm_t_bits scm_tc16_objcode;

#define SCM_OBJCODE_P(x)	(SCM_SMOB_PREDICATE (scm_tc16_objcode, x))
#define SCM_OBJCODE_DATA(x)	((struct scm_objcode *) SCM_SMOB_DATA (x))
#define SCM_VALIDATE_OBJCODE(p,x) SCM_MAKE_VALIDATE (p, x, OBJCODE_P)

#define SCM_OBJCODE_LEN(x)	(SCM_OBJCODE_DATA (x)->len)
#define SCM_OBJCODE_META_LEN(x)	(SCM_OBJCODE_DATA (x)->metalen)
#define SCM_OBJCODE_TOTAL_LEN(x) (SCM_OBJCODE_LEN (x) + SCM_OBJCODE_META_LEN (x))
#define SCM_OBJCODE_NARGS(x)	(SCM_OBJCODE_DATA (x)->nargs)
#define SCM_OBJCODE_NREST(x)	(SCM_OBJCODE_DATA (x)->nrest)
#define SCM_OBJCODE_NLOCS(x)	(SCM_OBJCODE_DATA (x)->nlocs)
#define SCM_OBJCODE_BASE(x)	(SCM_OBJCODE_DATA (x)->base)

#define SCM_OBJCODE_IS_MMAP(x)	(SCM_SMOB_FLAGS (x) & SCM_F_OBJCODE_IS_MMAP)
#define SCM_OBJCODE_IS_U8VECTOR(x) (SCM_SMOB_FLAGS (x) & SCM_F_OBJCODE_IS_U8VECTOR)
#define SCM_OBJCODE_IS_SLICE(x) (SCM_SMOB_FLAGS (x) & SCM_F_OBJCODE_IS_SLICE)

SCM scm_c_make_objcode_slice (SCM parent, const scm_t_uint8 *ptr);
SCM_API SCM scm_load_objcode (SCM file);
SCM_API SCM scm_objcode_p (SCM obj);
SCM_API SCM scm_objcode_meta (SCM objcode);
SCM_API SCM scm_bytecode_to_objcode (SCM bytecode);
SCM_API SCM scm_objcode_to_bytecode (SCM objcode);
SCM_API SCM scm_write_objcode (SCM objcode, SCM port);

SCM_INTERNAL void scm_bootstrap_objcodes (void);
SCM_INTERNAL void scm_init_objcodes (void);

#endif /* _SCM_OBJCODES_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

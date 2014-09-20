/* classes: h_files */

#ifndef SCM_SMOB_H
#define SCM_SMOB_H

/* Copyright (C) 1995, 1996, 1998, 1999, 2000, 2001, 2004, 2006, 2009,
 *   2010, 2011, 2012 Free Software Foundation, Inc.
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



/* This is the internal representation of a smob type */

typedef struct scm_smob_descriptor
{
  char const *name;
  size_t size;
  SCM (*mark) (SCM);
  size_t (*free) (SCM);
  int (*print) (SCM exp, SCM port, scm_print_state *pstate);
  SCM (*equalp) (SCM, SCM);
  scm_t_subr apply;
  /* In 2.2 this field is renamed to "apply_trampoline".  */
  SCM apply_trampoline_objcode;
} scm_smob_descriptor;


#define SCM_SMOB_TYPE_MASK		0xffff
#define SCM_SMOB_TYPE_BITS(tc)		(tc)
#define SCM_TC2SMOBNUM(x)		(0x0ff & ((x) >> 8))
#define SCM_SMOBNUM(x)			(SCM_TC2SMOBNUM (SCM_CELL_TYPE (x)))
/* SCM_SMOBNAME can be 0 if name is missing */
#define SCM_SMOBNAME(smobnum)		(scm_smobs[smobnum].name)
#define SCM_SMOB_PREDICATE(tag, obj)	SCM_TYP16_PREDICATE (tag, obj)
#define SCM_SMOB_DESCRIPTOR(x)		(scm_smobs[SCM_SMOBNUM (x)])
#define SCM_SMOB_APPLICABLE_P(x)	(SCM_SMOB_DESCRIPTOR (x).apply)

/* Maximum number of SMOB types.  */
#define SCM_I_MAX_SMOB_TYPE_COUNT  256

SCM_API long scm_numsmob;
SCM_API scm_smob_descriptor scm_smobs[];




SCM_API SCM scm_i_new_smob (scm_t_bits tc, scm_t_bits);
SCM_API SCM scm_i_new_double_smob (scm_t_bits tc, scm_t_bits,
                                   scm_t_bits, scm_t_bits);


SCM_INLINE SCM scm_new_smob (scm_t_bits tc, scm_t_bits);
SCM_INLINE SCM scm_new_double_smob (scm_t_bits tc, scm_t_bits,
                                    scm_t_bits, scm_t_bits);

/* These two are internal details of the previous implementation of
   SCM_NEWSMOB and are no longer used.  They are still here to preserve
   ABI stability in the 2.0 series.  */
SCM_API void scm_i_finalize_smob (void *ptr, void *data);
SCM_API SCM scm_i_new_smob_with_mark_proc (scm_t_bits tc, scm_t_bits,
                                           scm_t_bits, scm_t_bits);


#if SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES
SCM_INLINE_IMPLEMENTATION SCM
scm_new_smob (scm_t_bits tc, scm_t_bits data)
{
  scm_t_bits smobnum = SCM_TC2SMOBNUM (tc);

  if (SCM_UNLIKELY (scm_smobs[smobnum].mark || scm_smobs[smobnum].free))
    return scm_i_new_smob (tc, data);
  else
    return scm_cell (tc, data);
}

SCM_INLINE_IMPLEMENTATION SCM
scm_new_double_smob (scm_t_bits tc, scm_t_bits data1,
                     scm_t_bits data2, scm_t_bits data3)
{
  scm_t_bits smobnum = SCM_TC2SMOBNUM (tc);

  if (SCM_UNLIKELY (scm_smobs[smobnum].mark || scm_smobs[smobnum].free))
    return scm_i_new_double_smob (tc, data1, data2, data3);
  else
    return scm_double_cell (tc, data1, data2, data3);
}
#endif

#define SCM_NEWSMOB(z, tc, data)                \
  z = scm_new_smob ((tc), (scm_t_bits)(data))
#define SCM_RETURN_NEWSMOB(tc, data)            \
  return scm_new_smob ((tc), (scm_t_bits)(data))

#define SCM_NEWSMOB2(z, tc, data1, data2)               \
  z = scm_new_double_smob ((tc), (scm_t_bits)(data1),   \
                           (scm_t_bits)(data2), 0)
#define SCM_RETURN_NEWSMOB2(tc, data1, data2)                   \
  return scm_new_double_smob ((tc), (scm_t_bits)(data1),        \
                              (scm_t_bits)(data2), 0)

#define SCM_NEWSMOB3(z, tc, data1, data2, data3)                        \
  z = scm_new_double_smob ((tc), (scm_t_bits)(data1),                   \
                           (scm_t_bits)(data2), (scm_t_bits)(data3))
#define SCM_RETURN_NEWSMOB3(tc, data1, data2, data3)                    \
  return scm_new_double_smob ((tc), (scm_t_bits)(data1),                \
                              (scm_t_bits)(data2), (scm_t_bits)(data3))



#define SCM_SMOB_DATA_N(x, n)		(SCM_CELL_WORD ((x), (n)))
#define SCM_SET_SMOB_DATA_N(x, n, data)	(SCM_SET_CELL_WORD ((x), (n), (data)))

#define SCM_SMOB_DATA_0(x)		(SCM_SMOB_DATA_N ((x), 0))
#define SCM_SMOB_DATA_1(x)		(SCM_SMOB_DATA_N ((x), 1))
#define SCM_SMOB_DATA_2(x)		(SCM_SMOB_DATA_N ((x), 2))
#define SCM_SMOB_DATA_3(x)		(SCM_SMOB_DATA_N ((x), 3))
#define SCM_SET_SMOB_DATA_0(x, data)	(SCM_SET_SMOB_DATA_N ((x), 0, (data)))
#define SCM_SET_SMOB_DATA_1(x, data)	(SCM_SET_SMOB_DATA_N ((x), 1, (data)))
#define SCM_SET_SMOB_DATA_2(x, data)	(SCM_SET_SMOB_DATA_N ((x), 2, (data)))
#define SCM_SET_SMOB_DATA_3(x, data)	(SCM_SET_SMOB_DATA_N ((x), 3, (data)))

#define SCM_SMOB_FLAGS(x)               (SCM_SMOB_DATA_0 (x) >> 16)
#define SCM_SMOB_DATA(x)		(SCM_SMOB_DATA_1 (x))
#define SCM_SET_SMOB_FLAGS(x, data)     (SCM_SET_SMOB_DATA_0 ((x), (SCM_CELL_TYPE (x)&0xffff)|((data)<<16)))
#define SCM_SET_SMOB_DATA(x, data)	(SCM_SET_SMOB_DATA_1 ((x), (data)))

#define SCM_SMOB_OBJECT_N(x,n)		(SCM_CELL_OBJECT ((x), (n)))
#define SCM_SET_SMOB_OBJECT_N(x,n,obj)	(SCM_SET_CELL_OBJECT ((x), (n), (obj)))
#define SCM_SMOB_OBJECT_N_LOC(x,n)	(SCM_CELL_OBJECT_LOC ((x), (n)))

/*#define SCM_SMOB_OBJECT_0(x)		(SCM_SMOB_OBJECT_N ((x), 0))*/
#define SCM_SMOB_OBJECT_1(x)		(SCM_SMOB_OBJECT_N ((x), 1))
#define SCM_SMOB_OBJECT_2(x)		(SCM_SMOB_OBJECT_N ((x), 2))
#define SCM_SMOB_OBJECT_3(x)		(SCM_SMOB_OBJECT_N ((x), 3))
/*#define SCM_SET_SMOB_OBJECT_0(x,obj)	(SCM_SET_SMOB_OBJECT_N ((x), 0, (obj)))*/
#define SCM_SET_SMOB_OBJECT_1(x,obj)	(SCM_SET_SMOB_OBJECT_N ((x), 1, (obj)))
#define SCM_SET_SMOB_OBJECT_2(x,obj)	(SCM_SET_SMOB_OBJECT_N ((x), 2, (obj)))
#define SCM_SET_SMOB_OBJECT_3(x,obj)	(SCM_SET_SMOB_OBJECT_N ((x), 3, (obj)))
#define SCM_SMOB_OBJECT_0_LOC(x)	(SCM_SMOB_OBJECT_N_LOC ((x), 0))
#define SCM_SMOB_OBJECT_1_LOC(x)	(SCM_SMOB_OBJECT_N_LOC ((x), 1))
#define SCM_SMOB_OBJECT_2_LOC(x)	(SCM_SMOB_OBJECT_N_LOC ((x), 2))
#define SCM_SMOB_OBJECT_3_LOC(x)	(SCM_SMOB_OBJECT_N_LOC ((x), 3))

#define SCM_SMOB_OBJECT(x)		(SCM_SMOB_OBJECT_1 (x))
#define SCM_SET_SMOB_OBJECT(x,obj)	(SCM_SET_SMOB_OBJECT_1 ((x), (obj)))
#define SCM_SMOB_OBJECT_LOC(x)		(SCM_SMOB_OBJECT_1_LOC (x))


#define SCM_SMOB_APPLY_0(x)		(scm_call_0 (x))
#define SCM_SMOB_APPLY_1(x, a1)		(scm_call_1 (x, a1))
#define SCM_SMOB_APPLY_2(x, a1, a2)	(scm_call_2 (x, a1, a2))
#define SCM_SMOB_APPLY_3(x, a1, a2, rst) (scm_call_3 (x, a1, a2, a3))



SCM_API SCM scm_mark0 (SCM ptr);
SCM_API SCM scm_markcdr (SCM ptr);
SCM_API size_t scm_free0 (SCM ptr);
SCM_API int scm_smob_print (SCM exp, SCM port, scm_print_state *pstate);

/* The following set of functions is the standard way to create new
 * SMOB types.
 *
 * Create a type tag using `scm_make_smob_type', accept default values
 * for mark, free, print and/or equalp functions, or set your own
 * values using `scm_set_smob_xxx'.
 */

SCM_API scm_t_bits scm_make_smob_type (char const *name, size_t size);

SCM_API void scm_set_smob_mark (scm_t_bits tc, SCM (*mark) (SCM));
SCM_API void scm_set_smob_free (scm_t_bits tc, size_t (*free) (SCM));
SCM_API void scm_set_smob_print (scm_t_bits tc,
				 int (*print) (SCM, SCM, scm_print_state*));
SCM_API void scm_set_smob_equalp (scm_t_bits tc, SCM (*equalp) (SCM, SCM));
SCM_API void scm_set_smob_apply (scm_t_bits tc,
				 scm_t_subr apply,
				 unsigned int req,
				 unsigned int opt,
				 unsigned int rst);

SCM_API void scm_assert_smob_type (scm_t_bits tag, SCM val);

/* Function for creating smobs */

SCM_API SCM scm_make_smob (scm_t_bits tc);

SCM_API void scm_smob_prehistory (void);

#endif  /* SCM_SMOB_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

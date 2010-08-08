/* classes: h_files */

#ifndef SCM_PAIRS_H
#define SCM_PAIRS_H

/* Copyright (C) 1995,1996,2000,2001, 2004, 2006, 2008, 2009, 2010 Free Software Foundation, Inc.
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



#if (SCM_DEBUG_PAIR_ACCESSES == 1)
# define SCM_VALIDATE_PAIR(cell, expr) \
    ((!scm_is_pair (cell) ? scm_error_pair_access (cell), 0 : 0), (expr))
#else
# define SCM_VALIDATE_PAIR(cell, expr) (expr)
#endif

/*
 * Use scm_is_null_and_not_nil if it's important (for correctness)
 * that #nil must NOT be considered null.
 */
#define scm_is_null_and_not_nil(x)     (scm_is_eq ((x), SCM_EOL))

/*
 * Use scm_is_null_assume_not_nil if 
#nil will never be tested,
 * for increased efficiency.
 */
#define scm_is_null_assume_not_nil(x)  (scm_is_eq ((x), SCM_EOL))

/*
 * See the comments preceeding the definitions of SCM_BOOL_F and
 * SCM_MATCHES_BITS_IN_COMMON in tags.h for more information on
 * how the following macro works.
 */
#define scm_is_null_or_nil(x)  \
  (SCM_MATCHES_BITS_IN_COMMON ((x), SCM_ELISP_NIL, SCM_EOL))




/* Older spellings for these null, nil, and pair predicates. */
#define SCM_NILP(x) (scm_is_eq ((x), SCM_ELISP_NIL))
#define SCM_NULL_OR_NIL_P(x) (scm_is_null_or_nil (x))
#define SCM_NULLP(x) (scm_is_null (x))
#define SCM_NNULLP(x) (!scm_is_null (x))
#define SCM_CONSP(x)            (scm_is_pair (x))
#define SCM_NCONSP(x)           (!SCM_CONSP (x))




/* #nil is null. */
#define scm_is_null(x)		(scm_is_null_or_nil(x))

#define SCM_CAR(x)		(SCM_VALIDATE_PAIR (x, SCM_CELL_OBJECT_0 (x)))
#define SCM_CDR(x)		(SCM_VALIDATE_PAIR (x, SCM_CELL_OBJECT_1 (x)))

#define SCM_SETCAR(x, v)	(SCM_VALIDATE_PAIR (x, SCM_SET_CELL_OBJECT_0 ((x), (v))))
#define SCM_SETCDR(x, v)	(SCM_VALIDATE_PAIR (x, SCM_SET_CELL_OBJECT_1 ((x), (v))))

#define SCM_CAAR(OBJ)		SCM_CAR (SCM_CAR (OBJ))
#define SCM_CDAR(OBJ)		SCM_CDR (SCM_CAR (OBJ))
#define SCM_CADR(OBJ)		SCM_CAR (SCM_CDR (OBJ))
#define SCM_CDDR(OBJ)		SCM_CDR (SCM_CDR (OBJ))

#define SCM_CAAAR(OBJ)		SCM_CAR (SCM_CAR (SCM_CAR (OBJ)))
#define SCM_CDAAR(OBJ)		SCM_CDR (SCM_CAR (SCM_CAR (OBJ)))
#define SCM_CADAR(OBJ)		SCM_CAR (SCM_CDR (SCM_CAR (OBJ)))
#define SCM_CDDAR(OBJ)		SCM_CDR (SCM_CDR (SCM_CAR (OBJ)))
#define SCM_CAADR(OBJ)		SCM_CAR (SCM_CAR (SCM_CDR (OBJ)))
#define SCM_CDADR(OBJ)		SCM_CDR (SCM_CAR (SCM_CDR (OBJ)))
#define SCM_CADDR(OBJ)		SCM_CAR (SCM_CDR (SCM_CDR (OBJ)))
#define SCM_CDDDR(OBJ)		SCM_CDR (SCM_CDR (SCM_CDR (OBJ)))

#define SCM_CAAAAR(OBJ)		SCM_CAR (SCM_CAR (SCM_CAR (SCM_CAR (OBJ))))
#define SCM_CDAAAR(OBJ)		SCM_CDR (SCM_CAR (SCM_CAR (SCM_CAR (OBJ))))
#define SCM_CADAAR(OBJ)		SCM_CAR (SCM_CDR (SCM_CAR (SCM_CAR (OBJ))))
#define SCM_CDDAAR(OBJ)		SCM_CDR (SCM_CDR (SCM_CAR (SCM_CAR (OBJ))))
#define SCM_CAADAR(OBJ)		SCM_CAR (SCM_CAR (SCM_CDR (SCM_CAR (OBJ))))
#define SCM_CDADAR(OBJ)		SCM_CDR (SCM_CAR (SCM_CDR (SCM_CAR (OBJ))))
#define SCM_CADDAR(OBJ)		SCM_CAR (SCM_CDR (SCM_CDR (SCM_CAR (OBJ))))
#define SCM_CDDDAR(OBJ)		SCM_CDR (SCM_CDR (SCM_CDR (SCM_CAR (OBJ))))
#define SCM_CAAADR(OBJ)		SCM_CAR (SCM_CAR (SCM_CAR (SCM_CDR (OBJ))))
#define SCM_CDAADR(OBJ)		SCM_CDR (SCM_CAR (SCM_CAR (SCM_CDR (OBJ))))
#define SCM_CADADR(OBJ)		SCM_CAR (SCM_CDR (SCM_CAR (SCM_CDR (OBJ))))
#define SCM_CDDADR(OBJ)		SCM_CDR (SCM_CDR (SCM_CAR (SCM_CDR (OBJ))))
#define SCM_CAADDR(OBJ)		SCM_CAR (SCM_CAR (SCM_CDR (SCM_CDR (OBJ))))
#define SCM_CDADDR(OBJ)		SCM_CDR (SCM_CAR (SCM_CDR (SCM_CDR (OBJ))))
#define SCM_CADDDR(OBJ)		SCM_CAR (SCM_CDR (SCM_CDR (SCM_CDR (OBJ))))
#define SCM_CDDDDR(OBJ)		SCM_CDR (SCM_CDR (SCM_CDR (SCM_CDR (OBJ))))



#if (SCM_DEBUG_PAIR_ACCESSES == 1)
SCM_API void scm_error_pair_access (SCM);
#endif

SCM_API SCM scm_cons (SCM x, SCM y);
SCM_API SCM scm_cons2 (SCM w, SCM x, SCM y);
SCM_API SCM scm_pair_p (SCM x);
SCM_API SCM scm_car (SCM x);
SCM_API SCM scm_cdr (SCM x);
SCM_API SCM scm_set_car_x (SCM pair, SCM value);
SCM_API SCM scm_set_cdr_x (SCM pair, SCM value);

SCM_API SCM scm_cddr (SCM x);
SCM_API SCM scm_cdar (SCM x);
SCM_API SCM scm_cadr (SCM x);
SCM_API SCM scm_caar (SCM x);
SCM_API SCM scm_cdddr (SCM x);
SCM_API SCM scm_cddar (SCM x);
SCM_API SCM scm_cdadr (SCM x);
SCM_API SCM scm_cdaar (SCM x);
SCM_API SCM scm_caddr (SCM x);
SCM_API SCM scm_cadar (SCM x);
SCM_API SCM scm_caadr (SCM x);
SCM_API SCM scm_caaar (SCM x);
SCM_API SCM scm_cddddr (SCM x);
SCM_API SCM scm_cdddar (SCM x);
SCM_API SCM scm_cddadr (SCM x);
SCM_API SCM scm_cddaar (SCM x);
SCM_API SCM scm_cdaddr (SCM x);
SCM_API SCM scm_cdadar (SCM x);
SCM_API SCM scm_cdaadr (SCM x);
SCM_API SCM scm_cdaaar (SCM x);
SCM_API SCM scm_cadddr (SCM x);
SCM_API SCM scm_caddar (SCM x);
SCM_API SCM scm_cadadr (SCM x);
SCM_API SCM scm_cadaar (SCM x);
SCM_API SCM scm_caaddr (SCM x);
SCM_API SCM scm_caadar (SCM x);
SCM_API SCM scm_caaadr (SCM x);
SCM_API SCM scm_caaaar (SCM x);

SCM_INTERNAL void scm_init_pairs (void);

#endif  /* SCM_PAIRS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

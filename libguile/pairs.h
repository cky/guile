/* classes: h_files */

#ifndef SCM_PAIRS_H
#define SCM_PAIRS_H

/* Copyright (C) 1995,1996,2000,2001 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */



#include "libguile/__scm.h"



#if (SCM_DEBUG_PAIR_ACCESSES == 1)
# define SCM_VALIDATE_PAIR(cell, expr) \
    ((!SCM_CONSP (cell) ? scm_error_pair_access (cell), 0 : 0), (expr))
#else
# define SCM_VALIDATE_PAIR(cell, expr) (expr)
#endif

#define SCM_NULLP(x)		(SCM_EQ_P ((x), SCM_EOL))
#define SCM_NNULLP(x)		(!SCM_NULLP (x))

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
SCM_API SCM scm_set_car_x (SCM pair, SCM value);
SCM_API SCM scm_set_cdr_x (SCM pair, SCM value);
SCM_API void scm_init_pairs (void);

#endif  /* SCM_PAIRS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

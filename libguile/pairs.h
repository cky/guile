/* classes: h_files */

#ifndef PAIRSH
#define PAIRSH
/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


#include "libguile/__scm.h"



typedef struct scm_cell
{
  SCM car;
  SCM cdr;
} scm_cell;

/* SCM_PTR_LT defines how to compare two SCM_CELLPTRs (which may not be in the
 * same scm_array).  SCM_CELLPTR is a pointer to a cons cell which may be
 * compared or differenced.  SCMPTR is used for stack bounds.
 */

#if !defined(__TURBOC__) || defined(__TOS__)

typedef scm_cell *SCM_CELLPTR;
typedef SCM  *SCMPTR;

# ifdef nosve
#  define SCM_PTR_MASK 0xffffffffffff
#  define SCM_PTR_LT(x, y) (((int)(x)&SCM_PTR_MASK) < ((int)(y)&SCM_PTR_MASK))
# else
#  define SCM_PTR_LT(x, y) ((x) < (y))
# endif /* def nosve */

#else /* defined(__TURBOC__) && !defined(__TOS__) */

# ifdef PROT386
typedef scm_cell *SCM_CELLPTR;
typedef SCM *SCMPTR;
#  define SCM_PTR_LT(x, y) (((long)(x)) < ((long)(y)))
# else
typedef scm_cell huge *SCM_CELLPTR;
typedef SCM  huge *SCMPTR;
#  define SCM_PTR_LT(x, y) ((x) < (y))
# endif /* def PROT386 */

#endif /*  defined(__TURBOC__) && !defined(__TOS__) */

#define SCM_PTR_GT(x, y) SCM_PTR_LT(y, x)
#define SCM_PTR_LE(x, y) (!SCM_PTR_GT(x, y))
#define SCM_PTR_GE(x, y) (!SCM_PTR_LT(x, y))

#define SCM_NULLP(x) 		(SCM_EOL == (x))
#define SCM_NNULLP(x) 		(SCM_EOL != (x))




/* Cons Pairs
 */

#define SCM_CAR(x) (((scm_cell *)(SCM2PTR(x)))->car)
#define SCM_CDR(x) (((scm_cell *)(SCM2PTR(x)))->cdr)
#define SCM_GCCDR(x) (~1L & SCM_CDR(x))
#define SCM_SETCAR(x, v) (SCM_CAR(x) = (SCM)(v))
#define SCM_SETCDR(x, v) (SCM_CDR(x) = (SCM)(v))

#define SCM_CARLOC(x) (&SCM_CAR (x))
#define SCM_CDRLOC(x) (&SCM_CDR (x))

#define SCM_SETAND_CAR(x, y) (SCM_CAR (x) &= (y))
#define SCM_SETAND_CDR(x, y) (SCM_CDR (x) &= (y))
#define SCM_SETOR_CAR(x, y)  (SCM_CAR (x) |= (y))
#define SCM_SETOR_CDR(x, y)  (SCM_CDR (x) |= (y))

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


#ifdef DEBUG_FREELIST
#define SCM_NEWCELL(_into) do { _into = scm_debug_newcell (); } while (0)
#else
#define SCM_NEWCELL(_into) \
	do { \
	  if (SCM_IMP(scm_freelist)) \
	     _into = scm_gc_for_newcell();\
	  else \
	    { \
	       _into = scm_freelist; \
	       scm_freelist = SCM_CDR(scm_freelist);\
	       ++scm_cells_allocated; \
	    } \
	} while(0)
#endif



extern SCM scm_cons SCM_P ((SCM x, SCM y));
extern SCM scm_cons2 SCM_P ((SCM w, SCM x, SCM y));
extern SCM scm_pair_p SCM_P ((SCM x));
extern SCM scm_set_car_x SCM_P ((SCM pair, SCM value));
extern SCM scm_set_cdr_x SCM_P ((SCM pair, SCM value));
extern void scm_init_pairs SCM_P ((void));

#endif  /* PAIRSH */

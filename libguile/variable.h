/* classes: h_files */

#ifndef SCM_VARIABLE_H
#define SCM_VARIABLE_H
/* Copyright (C) 1995,1996,2000,2001 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */

#include "libguile/__scm.h"
#include "libguile/smob.h"




/* Variables 
 */
extern scm_t_bits scm_tc16_variable;

#define SCM_VARIABLEP(X)   	SCM_SMOB_PREDICATE (scm_tc16_variable, X)

#if !SCM_ENABLE_VCELLS
#define SCM_VARIABLE_REF(V)   SCM_CELL_OBJECT_1(V)
#define SCM_VARIABLE_SET(V,X) SCM_SET_CELL_OBJECT_1 (V, X)
#define SCM_VARIABLE_LOC(V)   ((SCM *) SCM_CELL_WORD_LOC ((V), 1))
#else
#define SCM_VARVCELL(V)       SCM_CELL_OBJECT_1(V)
#define SCM_UDVARIABLEP(X)    (SCM_VARIABLEP(X) && SCM_UNBNDP (SCM_CDR (SCM_VARVCELL (X))))
#define SCM_DEFVARIABLEP(X)   (SCM_VARIABLEP(X) && !SCM_UNBNDP (SCM_CDR (SCM_VARVCELL (X))))

#define SCM_VARIABLE_REF(V)   SCM_CDR(SCM_VARVCELL(V))
#define SCM_VARIABLE_SET(V,X) SCM_SETCDR(SCM_VARVCELL(V),X)
#define SCM_VARIABLE_LOC(V)   SCM_CDRLOC(SCM_VARVCELL(V))
#endif



extern SCM scm_make_variable (SCM init);
extern SCM scm_make_undefined_variable (void);
extern SCM scm_variable_p (SCM obj);
extern SCM scm_variable_ref (SCM var);
extern SCM scm_variable_set_x (SCM var, SCM val);
extern SCM scm_variable_bound_p (SCM var);
extern SCM scm_variable_set_name_hint (SCM var, SCM hint);
#if SCM_ENABLE_VCELLS
extern SCM scm_builtin_variable (SCM name);
#endif

extern void scm_init_variable (void);

#endif  /* SCM_VARIABLE_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

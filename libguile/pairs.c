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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include <stdio.h>
#include "_scm.h"


#include "scm_validate.h"



/* {Pairs}
 */

GUILE_PROC(scm_cons, "cons", 2, 0, 0,
           (SCM x, SCM y),
"")
#define FUNC_NAME s_scm_cons
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_SETCAR (z, x);
  SCM_SETCDR (z, y);
  return z;
}
#undef FUNC_NAME


SCM 
scm_cons2 (SCM w, SCM x, SCM y)
{
  register SCM z;
  SCM_NEWCELL (z);
  SCM_SETCAR (z, x);
  SCM_SETCDR (z, y);
  x = z;
  SCM_NEWCELL (z);
  SCM_SETCAR (z, w);
  SCM_SETCDR (z, x);
  return z;
}


GUILE_PROC (scm_pair_p, "pair?", 1, 0, 0, 
            (SCM x),
"")
#define FUNC_NAME s_scm_pair_p
{
  if (SCM_IMP (x))
    return SCM_BOOL_F;
  return SCM_BOOL(SCM_CONSP (x));
}
#undef FUNC_NAME

GUILE_PROC (scm_set_car_x, "set-car!", 2, 0, 0,
            (SCM pair, SCM value),
"")
#define FUNC_NAME s_scm_set_car_x
{
  SCM_VALIDATE_NIMCONS(1,pair);
  SCM_SETCAR (pair, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

GUILE_PROC (scm_set_cdr_x, "set-cdr!", 2, 0, 0,
            (SCM pair, SCM value),
"")
#define FUNC_NAME s_scm_set_cdr_x
{
  SCM_VALIDATE_NIMCONS(1,pair);
  SCM_SETCDR (pair, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




static const scm_iproc cxrs[] = 
{
  {"car", 0},
  {"cdr", 0},
  {"caar", 0},
  {"cadr", 0},
  {"cdar", 0},
  {"cddr", 0},
  {"caaar", 0},
  {"caadr", 0},
  {"cadar", 0},
  {"caddr", 0},
  {"cdaar", 0},
  {"cdadr", 0},
  {"cddar", 0},
  {"cdddr", 0},
  {"caaaar", 0},
  {"caaadr", 0},
  {"caadar", 0},
  {"caaddr", 0},
  {"cadaar", 0},
  {"cadadr", 0},
  {"caddar", 0},
  {"cadddr", 0},
  {"cdaaar", 0},
  {"cdaadr", 0},
  {"cdadar", 0},
  {"cdaddr", 0},
  {"cddaar", 0},
  {"cddadr", 0},
  {"cdddar", 0},
  {"cddddr", 0},
  {0, 0}
};



void
scm_init_pairs ()
{
  scm_init_iprocs (cxrs, scm_tc7_cxr);
#include "pairs.x"
}


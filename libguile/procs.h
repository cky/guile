/* classes: h_files */

#ifndef PROCSH
#define PROCSH
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.  
 */


#include "libguile/__scm.h"




/* Subrs 
 */

typedef struct scm_subr
{
  long sname;
  SCM (*cproc) ();
} scm_subr;

typedef struct scm_iproc
{
  char *scm_string;
  SCM (*cproc) ();
} scm_iproc;

typedef struct scm_dsubr
{
  long sname;
  double (*dproc) ();
} scm_dsubr;

#define SCM_SNAME(x) ((SCM_CAR(x)>>8)?(SCM)(scm_heap_org+(SCM_CAR(x)>>8)):scm_nullstr)
#define SCM_SUBRF(x) (((scm_subr *)(SCM2PTR(x)))->cproc)
#define SCM_DSUBRF(x) (((scm_dsubr *)(SCM2PTR(x)))->dproc)
#define SCM_CCLO_SUBR(x) (SCM_VELTS(x)[0])

/* Closures
 */

#define SCM_CLOSUREP(x) (SCM_TYP3(x)==scm_tc3_closure)
#define SCM_CLOSCAR(x) (SCM_CAR(x)-scm_tc3_closure)
#define SCM_CODE(x) SCM_CAR(SCM_CLOSCAR (x))
#define SCM_PROCPROPS(x) SCM_CDR(SCM_CLOSCAR (x))
#define SCM_SETPROCPROPS(x, p) SCM_SETCDR(SCM_CLOSCAR (x), p)
#define SCM_SETCODE(x, e) (SCM_SETCAR (x, scm_cons ((e), SCM_EOL) + scm_tc3_closure))
#define SCM_ENV(x) SCM_CDR(x)
#define SCM_SETENV(x, e) SCM_SETCDR (x, e)
#define SCM_TOP_LEVEL(SCM_ENV)  (SCM_NULLP(SCM_ENV) || (SCM_BOOL_T == scm_procedure_p (SCM_CAR (SCM_ENV))))



extern SCM scm_make_subr SCM_P ((char *name, int type, SCM (*fcn) ()));
extern SCM scm_make_subr_opt SCM_P ((char *name, int type, SCM (*fcn) (),
				     int set));
extern SCM scm_makcclo SCM_P ((SCM proc, long len));
extern SCM scm_procedure_p SCM_P ((SCM obj));
extern SCM scm_thunk_p SCM_P ((SCM obj));
extern void scm_init_iprocs SCM_P ((scm_iproc *subra, int type));
extern void scm_init_procs SCM_P ((void));


#endif  /* PROCSH */

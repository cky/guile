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


#include <stdio.h>
#include "_scm.h"





/* {Procedures}
 */

#ifdef __STDC__
SCM 
scm_make_subr_opt (char *name, int type, SCM (*fcn) (), int set)
#else
SCM 
scm_make_subr_opt (name, type, fcn, set)
     char *name;
     int type;
     SCM (*fcn) ();
     int set;
#endif
{
  SCM symcell;
  long tmp;
  register SCM z;
  symcell = scm_sysintern (name, SCM_UNDEFINED);
  tmp = ((((SCM_CELLPTR) (SCM_CAR (symcell))) - scm_heap_org) << 8);
  if ((tmp >> 8) != ((SCM_CELLPTR) (SCM_CAR (symcell)) - scm_heap_org))
    tmp = 0;
  SCM_NEWCELL (z);
  SCM_SUBRF (z) = fcn;
  SCM_CAR (z) = tmp + type;
  if (set)
    SCM_CDR (symcell) = z;
  return z;
}


#ifdef __STDC__
SCM 
scm_make_subr (char *name, int type, SCM (*fcn) ())
#else
SCM 
scm_make_subr (name, type, fcn)
     char *name;
     int type;
     SCM (*fcn) ();
#endif
{
  return scm_make_subr_opt (name, type, fcn, 1);
}

#ifdef CCLO
#ifdef __STDC__
SCM 
scm_makcclo (SCM proc, long len)
#else
SCM 
scm_makcclo (proc, len)
     SCM proc;
     long len;
#endif
{
  SCM s;
  SCM_NEWCELL (s);
  SCM_DEFER_INTS;
  SCM_SETCHARS (s, scm_must_malloc (len * sizeof (SCM), "compiled-closure"));
  SCM_SETLENGTH (s, len, scm_tc7_cclo);
  while (--len)
    SCM_VELTS (s)[len] = SCM_UNSPECIFIED;
  SCM_CCLO_SUBR (s) = proc;
  SCM_ALLOW_INTS;
  return s;
}
#endif



SCM_PROC(s_procedure_p, "procedure?", 1, 0, 0, scm_procedure_p);
#ifdef __STDC__
SCM 
scm_procedure_p (SCM obj)
#else
SCM 
scm_procedure_p (obj)
     SCM obj;
#endif
{
  if (SCM_NIMP (obj))
    switch (SCM_TYP7 (obj))
      {
      case scm_tcs_closures:
      case scm_tc7_contin:
      case scm_tcs_subrs:
#ifdef CCLO
      case scm_tc7_cclo:
#endif
	return SCM_BOOL_T;
      default:
	return SCM_BOOL_F;
      }
  return SCM_BOOL_F;
}


#ifdef __STDC__
void
scm_init_iprocs(scm_iproc *subra, int type)
#else
void
scm_init_iprocs(subra, type)
     scm_iproc *subra;
     int type;
#endif
{
  for(;subra->scm_string; subra++)
    scm_make_subr(subra->scm_string,
		  type,
		  subra->cproc);
}




#ifdef __STDC__
void
scm_init_procs (void)
#else
void
scm_init_procs ()
#endif
{
#include "procs.x"
}


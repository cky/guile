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


#include <stdio.h>
#include "_scm.h"
#include "alist.h"
#include "eval.h"
#include "procs.h"
#include "gsubr.h"

#include "procprop.h"


SCM_GLOBAL_SYMBOL (scm_sym_arity, "arity");

static SCM
scm_i_procedure_arity (proc)
{
  int a = 0, o = 0, r = 0;
 loop:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_1o:
      o = 1;
    case scm_tc7_subr_0:
      break;
    case scm_tc7_subr_2o:
      o = 1;
    case scm_tc7_subr_1:
    case scm_tc7_cxr:
    case scm_tc7_contin:
      a += 1;
      break;
    case scm_tc7_subr_2:
      a += 2;
      break;
    case scm_tc7_subr_3:
      a += 3;
      break;
    case scm_tc7_asubr:
    case scm_tc7_rpsubr:
    case scm_tc7_lsubr:
      r = 1;
    case scm_tc7_lsubr_2:
      a += 2;
      break;
#ifdef CCLO
    case scm_tc7_cclo:
      if (SCM_CCLO_SUBR (proc) == scm_f_gsubr_apply)
	{
	  int type = SCM_INUM (SCM_GSUBR_TYPE (proc));
	  a = SCM_GSUBR_REQ (type);
	  o = SCM_GSUBR_OPT (type);
	  r = SCM_GSUBR_REST (type);
	  break;
	}
      proc = SCM_CCLO_SUBR (proc);
      a -= 1;
      goto loop;
#endif
    case scm_tcs_closures:
      proc = SCM_CAR (SCM_CODE (proc));
      if (SCM_IMP (proc))
	break;
      while (SCM_NIMP (proc) && SCM_CONSP (proc))
	{
	  ++a;
	  proc = SCM_CDR (proc);
	}
      if (SCM_NIMP (proc))
	r = 1;
      break;
    }
  return SCM_LIST3 (SCM_MAKINUM (a),
		    SCM_MAKINUM (o),
		    r ? SCM_BOOL_T : SCM_BOOL_F);
}

static SCM
scm_stand_in_scm_proc(proc)
     SCM proc;
{
  SCM answer;
  answer = scm_assoc (proc, scm_stand_in_procs);
  if (answer == SCM_BOOL_F)
    {
      answer = scm_closure (scm_listify (SCM_EOL, SCM_BOOL_F, SCM_UNDEFINED),
			    SCM_EOL);
      scm_stand_in_procs = scm_cons (scm_cons (proc, answer),
				     scm_stand_in_procs);
    }
  else
    answer = SCM_CDR (answer);
  return answer;
}

SCM_PROC(s_procedure_properties, "procedure-properties", 1, 0, 0, scm_procedure_properties);

SCM
scm_procedure_properties (proc)
     SCM proc;
{
  SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (proc)),
	      proc, SCM_ARG1, s_procedure_properties);
  return scm_acons (scm_sym_arity, scm_i_procedure_arity (proc),
		    SCM_PROCPROPS (SCM_NIMP (proc) && SCM_CLOSUREP (proc)
				   ? proc
				   : scm_stand_in_scm_proc (proc)));
}

SCM_PROC(s_set_procedure_properties_x, "set-procedure-properties!", 2, 0, 0, scm_set_procedure_properties_x);

SCM
scm_set_procedure_properties_x (proc, new_val)
     SCM proc;
     SCM new_val;
{
  if (!(SCM_NIMP (proc) && SCM_CLOSUREP (proc)))
    proc = scm_stand_in_scm_proc(proc);
  SCM_ASSERT (SCM_NIMP (proc) && SCM_CLOSUREP (proc), proc, SCM_ARG1, s_set_procedure_properties_x);
  SCM_SETPROCPROPS (proc, new_val);
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_procedure_property, "procedure-property", 2, 0, 0, scm_procedure_property);

SCM
scm_procedure_property (p, k)
     SCM p;
     SCM k;
{
  SCM assoc;
  SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (p)),
	      p, SCM_ARG1, s_procedure_property);
  if (k == scm_sym_arity)
    return scm_i_procedure_arity (p);
  assoc = scm_sloppy_assq (k,
			   SCM_PROCPROPS (SCM_NIMP (p) && SCM_CLOSUREP (p)
					  ? p
					  : scm_stand_in_scm_proc (p)));
  return (SCM_NIMP (assoc) ? SCM_CDR (assoc) : SCM_BOOL_F);
}

SCM_PROC(s_set_procedure_property_x, "set-procedure-property!", 3, 0, 0, scm_set_procedure_property_x);

SCM
scm_set_procedure_property_x (p, k, v)
     SCM p;
     SCM k;
     SCM v;
{
  SCM assoc;
  if (!(SCM_NIMP (p) && SCM_CLOSUREP (p)))
    p = scm_stand_in_scm_proc(p);
  SCM_ASSERT (SCM_NIMP (p) && SCM_CLOSUREP (p), p, SCM_ARG1, s_set_procedure_property_x);
  if (k == scm_sym_arity)
    scm_misc_error (s_set_procedure_property_x,
		    "arity is a read-only property",
		    SCM_EOL);
  assoc = scm_sloppy_assq (k, SCM_PROCPROPS (p));
  if (SCM_NIMP (assoc))
    SCM_SETCDR (assoc, v);
  else
    SCM_SETPROCPROPS (p, scm_acons (k, v, SCM_PROCPROPS (p)));
  return SCM_UNSPECIFIED;
}




void
scm_init_procprop ()
{
#include "procprop.x"
}


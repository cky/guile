/*	Copyright (C) 1995,1996,1997,1998, 1999, 2000 Free Software Foundation, Inc.
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
#include "libguile/_scm.h"
#include "libguile/procprop.h"
#include "libguile/root.h"

#include "libguile/gsubr.h"

/*
 * gsubr.c
 * Provide `gsubrs' -- subrs taking a prescribed number of required, optional,
 * and rest arguments.
 */

/* #define GSUBR_TEST */

SCM_GLOBAL_SYMBOL (scm_sym_name, "name");

SCM scm_f_gsubr_apply;

SCM
scm_make_gsubr(const char *name,int req,int opt,int rst,SCM (*fcn)())
{
  switch SCM_GSUBR_MAKTYPE(req, opt, rst) {
  case SCM_GSUBR_MAKTYPE(0, 0, 0): return scm_make_subr(name, scm_tc7_subr_0, fcn);
  case SCM_GSUBR_MAKTYPE(1, 0, 0): return scm_make_subr(name, scm_tc7_subr_1, fcn);
  case SCM_GSUBR_MAKTYPE(0, 1, 0): return scm_make_subr(name, scm_tc7_subr_1o, fcn);
  case SCM_GSUBR_MAKTYPE(1, 1, 0): return scm_make_subr(name, scm_tc7_subr_2o, fcn);
  case SCM_GSUBR_MAKTYPE(2, 0, 0): return scm_make_subr(name, scm_tc7_subr_2, fcn);
  case SCM_GSUBR_MAKTYPE(3, 0, 0): return scm_make_subr(name, scm_tc7_subr_3, fcn);
  case SCM_GSUBR_MAKTYPE(0, 0, 1): return scm_make_subr(name, scm_tc7_lsubr, fcn);
  case SCM_GSUBR_MAKTYPE(2, 0, 1): return scm_make_subr(name, scm_tc7_lsubr_2, fcn);
  default:
    {
      SCM sym = scm_str2symbol (name);
      SCM var = scm_sym2var (sym, scm_current_module_lookup_closure (),
			     SCM_BOOL_T);
      SCM cclo = scm_makcclo (scm_f_gsubr_apply, 3L);
      if (SCM_GSUBR_MAX < req + opt + rst) {
	fputs("ERROR in scm_make_gsubr: too many args\n", stderr);
	exit (1);
      }
      SCM_SET_GSUBR_PROC (cclo, scm_make_subr_opt (name, scm_tc7_subr_0, fcn, 0));
      SCM_SET_GSUBR_TYPE (cclo, SCM_MAKINUM (SCM_GSUBR_MAKTYPE (req, opt, rst)));
      SCM_VARIABLE_SET (var, cclo);
#ifdef DEBUG_EXTENSIONS
      if (SCM_REC_PROCNAMES_P)
	scm_set_procedure_property_x (cclo, scm_sym_name, sym);
#endif
      return cclo;
    }
  }
}

SCM
scm_make_gsubr_with_generic (const char *name,
			     int req,
			     int opt,
			     int rst,
			     SCM (*fcn)(),
			     SCM *gf)
{
  switch SCM_GSUBR_MAKTYPE(req, opt, rst) {
  case SCM_GSUBR_MAKTYPE(0, 0, 0):
    return scm_make_subr_with_generic(name, scm_tc7_subr_0, fcn, gf);
  case SCM_GSUBR_MAKTYPE(1, 0, 0):
    return scm_make_subr_with_generic(name, scm_tc7_subr_1, fcn, gf);
  case SCM_GSUBR_MAKTYPE(0, 1, 0):
    return scm_make_subr_with_generic(name, scm_tc7_subr_1o, fcn, gf);
  case SCM_GSUBR_MAKTYPE(1, 1, 0):
    return scm_make_subr_with_generic(name, scm_tc7_subr_2o, fcn, gf);
  case SCM_GSUBR_MAKTYPE(2, 0, 0):
    return scm_make_subr_with_generic(name, scm_tc7_subr_2, fcn, gf);
  case SCM_GSUBR_MAKTYPE(3, 0, 0):
    return scm_make_subr_with_generic(name, scm_tc7_subr_3, fcn, gf);
  case SCM_GSUBR_MAKTYPE(0, 0, 1):
    return scm_make_subr_with_generic(name, scm_tc7_lsubr, fcn, gf);
  case SCM_GSUBR_MAKTYPE(2, 0, 1):
    return scm_make_subr_with_generic(name, scm_tc7_lsubr_2, fcn, gf);
  default:
    ;
  }
  scm_misc_error ("scm_make_gsubr_with_generic",
		  "can't make primitive-generic with this arity",
		  SCM_EOL);
  return SCM_BOOL_F; /* never reached */
}


SCM
scm_gsubr_apply (SCM args)
#define FUNC_NAME "scm_gsubr_apply"
{
  SCM self = SCM_CAR(args);
  SCM (*fcn)() = SCM_SUBRF(SCM_GSUBR_PROC(self));
  SCM v[SCM_GSUBR_MAX];
  int typ = SCM_INUM(SCM_GSUBR_TYPE(self));
  int i, n = SCM_GSUBR_REQ(typ) + SCM_GSUBR_OPT(typ) + SCM_GSUBR_REST(typ);
#if 0
  if (n > SCM_GSUBR_MAX)
    scm_misc_error (FUNC_NAME, 
		    "Function ~S has illegal arity ~S.", 
		    SCM_LIST2 (self, SCM_MAKINUM (n)));
#endif
  args = SCM_CDR(args);
  for (i = 0; i < SCM_GSUBR_REQ(typ); i++) {
#ifndef SCM_RECKLESS
    if (SCM_NULLP (args))
      scm_wrong_num_args (SCM_SNAME (SCM_GSUBR_PROC (self)));
#endif
    v[i] = SCM_CAR(args);
    args = SCM_CDR(args);
  }
  for (; i < SCM_GSUBR_REQ(typ) + SCM_GSUBR_OPT(typ); i++) {
    if (SCM_NIMP(args)) {
      v[i] = SCM_CAR(args);
      args = SCM_CDR(args);
    }
    else
      v[i] = SCM_UNDEFINED;
  }
  if (SCM_GSUBR_REST(typ))
    v[i] = args;
  else if (!SCM_NULLP (args))
    scm_wrong_num_args (SCM_SNAME (SCM_GSUBR_PROC (self)));
  switch (n) {
  case 2: return (*fcn)(v[0], v[1]);
  case 3: return (*fcn)(v[0], v[1], v[2]);
  case 4: return (*fcn)(v[0], v[1], v[2], v[3]);
  case 5: return (*fcn)(v[0], v[1], v[2], v[3], v[4]);
  case 6: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5]);
  case 7: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6]);
  case 8: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]);
  case 9: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8]);
  case 10: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
  }
  return SCM_BOOL_F; /* Never reached. */
}
#undef FUNC_NAME


#ifdef GSUBR_TEST
/* A silly example, taking 2 required args, 1 optional, and
   a scm_list of rest args 
   */
SCM
gsubr_21l(SCM req1, SCM req2, SCM opt, SCM rst)
{
  scm_puts ("gsubr-2-1-l:\n req1: ", scm_cur_outp);
  scm_display(req1, scm_cur_outp);
  scm_puts ("\n req2: ", scm_cur_outp);
  scm_display(req2, scm_cur_outp);
  scm_puts ("\n opt: ", scm_cur_outp);
  scm_display(opt, scm_cur_outp);
  scm_puts ("\n rest: ", scm_cur_outp);
  scm_display(rst, scm_cur_outp);
  scm_newline(scm_cur_outp);
  return SCM_UNSPECIFIED;
}
#endif



void
scm_init_gsubr()
{
  scm_f_gsubr_apply = scm_make_subr_opt("gsubr-apply", scm_tc7_lsubr, scm_gsubr_apply, 0);

#ifdef GSUBR_TEST
  scm_make_gsubr("gsubr-2-1-l", 2, 1, 1, gsubr_21l); /* example */
#endif

#ifndef SCM_MAGIC_SNARFER
#include "libguile/gsubr.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

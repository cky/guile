/*	Copyright (C) 1998 Free Software Foundation, Inc.
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


#include "_scm.h"
#include "eval.h"
#include "macros.h"

#include "evalext.h"

SCM_PROC (s_definedp, "defined?", 1, 0, 0, scm_definedp);

SCM 
scm_definedp (sym)
     SCM sym;
{
  SCM vcell;

  if (SCM_ISYMP (sym))
    return SCM_BOOL_T;

  SCM_ASSERT (SCM_NIMP (sym) && SCM_SYMBOLP (sym), sym, SCM_ARG1, s_definedp);

  vcell = scm_sym2vcell(sym,
			SCM_CDR (scm_top_level_lookup_closure_var),
			SCM_BOOL_F);
  return (vcell == SCM_BOOL_F || SCM_UNBNDP(SCM_CDR(vcell))) ? 
      SCM_BOOL_F : SCM_BOOL_T;
}

static char s_undefine[] = "undefine";

SCM
scm_m_undefine (x, env)
     SCM x, env;
{
  SCM arg1 = x;
  x = SCM_CDR (x);
  SCM_ASSYNT (SCM_TOP_LEVEL (env), arg1, "bad placement ", s_undefine);
  SCM_ASSYNT (SCM_NIMP (x) && SCM_CONSP (x) && SCM_CDR (x) == SCM_EOL,
	      arg1, scm_s_expression, s_undefine);
  x = SCM_CAR (x);
  SCM_ASSYNT (SCM_NIMP (x) && SCM_SYMBOLP (x), arg1, scm_s_variable, s_undefine);
  arg1 = scm_sym2vcell (x, scm_env_top_level (env), SCM_BOOL_F);
  SCM_ASSYNT (SCM_NFALSEP (arg1) && !SCM_UNBNDP (SCM_CDR (arg1)),
	      x, "variable already unbound ", s_undefine);
#if 0
#ifndef SCM_RECKLESS
  if (SCM_NIMP (SCM_CDR (arg1)) && ((SCM) SCM_SNAME (SCM_CDR (arg1)) == x))
    scm_warn ("undefining built-in ", SCM_CHARS (x));
  else
#endif
    if (5 <= scm_verbose && SCM_UNDEFINED != SCM_CDR (arg1))
      scm_warn ("redefining ", SCM_CHARS (x));
#endif
  SCM_SETCDR (arg1, SCM_UNDEFINED);
#ifdef SICP
  return SCM_CAR (arg1);
#else
  return SCM_UNSPECIFIED;
#endif
}

static char s_sequence_to_list[] = "sequence->list";

SCM
scm_m_sequence_to_list (SCM x, SCM env)
{
  SCM res = SCM_EOL;
  SCM_ASSYNT (SCM_NULLP (SCM_CDR (x))
	      || (SCM_NIMP (SCM_CDR (x) && SCM_ECONSP (SCM_CDR (x)))),
	      x, scm_s_expression, s_sequence_to_list);
  while (SCM_NNULLP (x = SCM_CDR (x)))
    res = scm_cons (SCM_XEVALCAR (x, env), res);
  return res;
}

SCM_PROC (s_serial_map, "serial-map", 2, 0, 1, scm_map);

void 
scm_init_evalext ()
{
  scm_make_synt (s_undefine, scm_makacro, scm_m_undefine);
  scm_make_synt (s_sequence_to_list, scm_makacro, scm_m_sequence_to_list);
#include "evalext.x"
}

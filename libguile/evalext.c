/* Copyright (C) 1998,1999,2000,2001 Free Software Foundation, Inc.
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



#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/macros.h"
#include "libguile/modules.h"
#include "libguile/fluids.h"

#include "libguile/validate.h"
#include "libguile/evalext.h"

SCM_SYMBOL (scm_sym_setter, "setter");

SCM 
scm_m_generalized_set_x (SCM xorig, SCM env SCM_UNUSED)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (2 == scm_ilength (x), scm_s_expression, scm_s_set_x);
  if (SCM_SYMBOLP (SCM_CAR (x)))
    return scm_cons (SCM_IM_SET_X, x);
  else if (SCM_CONSP (SCM_CAR (x)))
    return scm_cons (scm_list_2 (scm_sym_setter, SCM_CAAR (x)),
		     scm_append (scm_list_2 (SCM_CDAR (x), SCM_CDR (x))));
  else
    scm_misc_error (scm_s_set_x, scm_s_variable, SCM_EOL);
}

SCM_DEFINE (scm_definedp, "defined?", 1, 1, 0,
            (SCM sym, SCM env),
	    "Return @code{#t} if @var{sym} is defined in the lexical "
	    "environment @var{env}.  When @var{env} is not specified, "
	    "look in the top-level environment as defined by the "
	    "current module.")
#define FUNC_NAME s_scm_definedp
{
  SCM var;

  SCM_VALIDATE_SYMBOL (1,sym);

  if (SCM_UNBNDP (env))
    var = scm_sym2var (sym, scm_current_module_lookup_closure (),
			 SCM_BOOL_F);
  else
    {
      SCM frames = env;
      register SCM b;
      for (; SCM_NIMP (frames); frames = SCM_CDR (frames))
	{
	  SCM_ASSERT (SCM_CONSP (frames), env, SCM_ARG2, FUNC_NAME);
	  b = SCM_CAR (frames);
	  if (SCM_NFALSEP (scm_procedure_p (b)))
	    break;
	  SCM_ASSERT (SCM_CONSP (b), env, SCM_ARG2, FUNC_NAME);
	  for (b = SCM_CAR (b); SCM_NIMP (b); b = SCM_CDR (b))
	    {
	      if (SCM_NCONSP (b))
		{
		  if (SCM_EQ_P (b, sym))
		    return SCM_BOOL_T;
		  else
		    break;
		}
	      if (SCM_EQ_P (SCM_CAR (b), sym))
		return SCM_BOOL_T;
	    }
	}
      var = scm_sym2var (sym,
			 SCM_NIMP (frames) ? SCM_CAR (frames) : SCM_BOOL_F,
			 SCM_BOOL_F);
    }
	      
  return (SCM_FALSEP (var) || SCM_UNBNDP (SCM_VARIABLE_REF (var))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}
#undef FUNC_NAME


SCM_SYNTAX (s_undefine, "undefine", scm_makacro, scm_m_undefine);

SCM
scm_m_undefine (SCM x, SCM env)
{
  SCM arg1 = x;
  x = SCM_CDR (x);
  SCM_ASSYNT (SCM_TOP_LEVEL (env), "bad placement ", s_undefine);
  SCM_ASSYNT (SCM_CONSP (x) && SCM_NULLP (SCM_CDR (x)),
	      scm_s_expression, s_undefine);
  x = SCM_CAR (x);
  SCM_ASSYNT (SCM_SYMBOLP (x), scm_s_variable, s_undefine);
  arg1 = scm_sym2var (x, scm_env_top_level (env), SCM_BOOL_F);
  SCM_ASSYNT (SCM_NFALSEP (arg1) && !SCM_UNBNDP (SCM_VARIABLE_REF (arg1)),
	      "variable already unbound ", s_undefine);
  SCM_VARIABLE_SET (arg1, SCM_UNDEFINED);
#ifdef SICP
  return x;
#else
  return SCM_UNSPECIFIED;
#endif
}

SCM_REGISTER_PROC (s_map_in_order, "map-in-order", 2, 0, 1, scm_map);

void 
scm_init_evalext ()
{
  scm_make_synt (scm_s_set_x, scm_makmmacro, scm_m_generalized_set_x);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/evalext.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/*	Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
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
#include "smob.h"

#include "macros.h"

long scm_tc16_macro;

SCM_PROC(s_makacro, "procedure->syntax", 1, 0, 0, scm_makacro);

SCM 
scm_makacro (code)
     SCM code;
{
  SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (code)),
	      code, SCM_ARG1, s_makacro);
  SCM_RETURN_NEWSMOB (scm_tc16_macro, code);
}


SCM_PROC(s_makmacro, "procedure->macro", 1, 0, 0, scm_makmacro);

SCM 
scm_makmacro (code)
     SCM code;
{
  SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (code)),
	      code, SCM_ARG1, s_makmacro);
  SCM_RETURN_NEWSMOB (scm_tc16_macro | (1L << 16), code);
}


SCM_PROC(s_makmmacro, "procedure->memoizing-macro", 1, 0, 0, scm_makmmacro);

SCM 
scm_makmmacro (code)
     SCM code;
{
  SCM_ASSERT (SCM_NFALSEP (scm_procedure_p (code)),
	      code, SCM_ARG1, s_makmmacro);
  SCM_RETURN_NEWSMOB (scm_tc16_macro | (2L << 16), code);
}


SCM_PROC (s_macro_p, "macro?", 1, 0, 0, scm_macro_p);

SCM
scm_macro_p (obj)
     SCM obj;
{
  return (SCM_NIMP (obj) && SCM_TYP16 (obj) == scm_tc16_macro
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_SYMBOL (scm_sym_syntax, "syntax");
SCM_SYMBOL (scm_sym_macro, "macro");
SCM_SYMBOL (scm_sym_mmacro, "macro!");

SCM_PROC (s_macro_type, "macro-type", 1, 0, 0, scm_macro_type);

SCM
scm_macro_type (m)
     SCM m;
{
  if (!(SCM_NIMP (m) && SCM_TYP16 (m) == scm_tc16_macro))
    return SCM_BOOL_F;
  switch ((int) (SCM_CAR (m) >> 16))
    {
    case 0: return scm_sym_syntax;
    case 1: return scm_sym_macro;
    case 2: return scm_sym_mmacro;
    default: scm_wrong_type_arg (s_macro_type, 1, m);
    }
}


SCM_PROC (s_macro_name, "macro-name", 1, 0, 0, scm_macro_name);

SCM
scm_macro_name (m)
     SCM m;
{
  SCM_ASSERT (SCM_NIMP (m) && SCM_TYP16 (m) == scm_tc16_macro,
	      m,
	      SCM_ARG1,
	      s_macro_name);
  return scm_procedure_name (SCM_CDR (m));
}


SCM_PROC (s_macro_transformer, "macro-transformer", 1, 0, 0, scm_macro_transformer);

SCM
scm_macro_transformer (m)
     SCM m;
{
  SCM_ASSERT (SCM_NIMP (m) && SCM_TYP16 (m) == scm_tc16_macro,
	      m,
	      SCM_ARG1,
	      s_macro_transformer);
  return SCM_CLOSUREP (SCM_CDR (m)) ? SCM_CDR (m) : SCM_BOOL_F;
}

SCM
scm_make_synt (name, macroizer, fcn)
     const char *name;
     SCM (*macroizer) ();
     SCM (*fcn) ();
{
  SCM symcell = scm_sysintern (name, SCM_UNDEFINED);
  long tmp = ((((SCM_CELLPTR) (SCM_CAR (symcell))) - scm_heap_org) << 8);
  register SCM z;
  if ((tmp >> 8) != ((SCM_CELLPTR) (SCM_CAR (symcell)) - scm_heap_org))
    tmp = 0;
  SCM_NEWCELL (z);
  SCM_SUBRF (z) = fcn;
  SCM_SETCAR (z, tmp + scm_tc7_subr_2);
  SCM_SETCDR (symcell, macroizer (z));
  return SCM_CAR (symcell);
}

void
scm_init_macros ()
{
  scm_tc16_macro = scm_make_smob_type_mfpe ("macro", 0,
                                           scm_markcdr, NULL, NULL, NULL);
#include "macros.x"
}

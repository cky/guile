/*	Copyright (C) 1995,1996,1997,1998, 2000 Free Software Foundation, Inc.
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
#include "libguile/root.h"
#include "libguile/smob.h"

#include "libguile/validate.h"
#include "libguile/macros.h"

long scm_tc16_macro;

SCM_DEFINE (scm_makacro, "procedure->syntax", 1, 0, 0,
            (SCM code),
	    "Returns a @dfn{macro} which, when a symbol defined to this value\n"
            "appears as the first symbol in an expression, returns the result\n"
            "of applying @var{code} to the expression and the environment.")
#define FUNC_NAME s_scm_makacro
{
  SCM_VALIDATE_PROC (1,code);
  SCM_RETURN_NEWSMOB (scm_tc16_macro, SCM_UNPACK (code));
}
#undef FUNC_NAME


SCM_DEFINE (scm_makmacro, "procedure->macro", 1, 0, 0, 
           (SCM code),
	    "Returns a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, evaluates the result\n"
	    "of applying @var{code} to the expression and the environment.\n"
	    "The value returned from @var{code} which has been passed to\n"
	    "@code{procedure->memoizing-macro} replaces the form passed to\n"
	    "@var{code}.  For example:\n\n"
	    "@example\n"
	    "(define trace\n"
	    "  (procedure->macro\n"
	    "   (lambda (x env) `(set! ,(cadr x) (tracef ,(cadr x) ',(cadr x))))))\n\n"
	    "(trace @i{foo}) @equiv{} (set! @i{foo} (tracef @i{foo} '@i{foo})).\n"
	    "@end example")
#define FUNC_NAME s_scm_makmacro
{
  SCM_VALIDATE_PROC (1,code);
  SCM_RETURN_NEWSMOB (scm_tc16_macro | (1L << 16), SCM_UNPACK (code));
}
#undef FUNC_NAME


SCM_DEFINE (scm_makmmacro, "procedure->memoizing-macro", 1, 0, 0, 
           (SCM code),
	    "Returns a @dfn{macro} which, when a symbol defined to this value\n"
            "appears as the first symbol in an expression, evaluates the result\n"
            "of applying @var{proc} to the expression and the environment.\n"
            "The value returned from @var{proc} which has been passed to\n"
            "@code{procedure->memoizing-macro} replaces the form passed to\n"
            "@var{proc}.  For example:\n\n"
            "@example\n"
            "(define trace\n"
            "  (procedure->macro\n"
            "   (lambda (x env) `(set! ,(cadr x) (tracef ,(cadr x) ',(cadr x))))))\n\n"
            "(trace @i{foo}) @equiv{} (set! @i{foo} (tracef @i{foo} '@i{foo})).\n"
            "@end example")
#define FUNC_NAME s_scm_makmmacro
{
  SCM_VALIDATE_PROC (1,code);
  SCM_RETURN_NEWSMOB (scm_tc16_macro | (2L << 16), SCM_UNPACK (code));
}
#undef FUNC_NAME


SCM_DEFINE (scm_macro_p, "macro?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a regular macro, a memoizing macro or a\n"
	    "syntax transformer.")
#define FUNC_NAME s_scm_macro_p
{
  return SCM_BOOL(SCM_NIMP (obj) && SCM_TYP16 (obj) == scm_tc16_macro);
}
#undef FUNC_NAME


SCM_SYMBOL (scm_sym_syntax, "syntax");
SCM_SYMBOL (scm_sym_macro, "macro");
SCM_SYMBOL (scm_sym_mmacro, "macro!");

SCM_DEFINE (scm_macro_type, "macro-type", 1, 0, 0, 
            (SCM m),
	    "Return one of the symbols @code{syntax}, @code{macro} or @code{macro!},\n"
	    "depending on whether @var{obj} is a syntax tranformer, a regular macro,\n"
	    "or a memoizing macro, respectively.  If @var{obj} is not a macro,\n"
	    "@code{#f} is returned.")
#define FUNC_NAME s_scm_macro_type
{
  if (!(SCM_NIMP (m) && SCM_TYP16 (m) == scm_tc16_macro))
    return SCM_BOOL_F;
  switch (SCM_CELL_WORD_0 (m) >> 16)
    {
    case 0: return scm_sym_syntax;
    case 1: return scm_sym_macro;
    case 2: return scm_sym_mmacro;
    default: scm_wrong_type_arg (FUNC_NAME, 1, m);
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_macro_name, "macro-name", 1, 0, 0, 
            (SCM m),
	    "")
#define FUNC_NAME s_scm_macro_name
{
  SCM_VALIDATE_SMOB (1,m,macro);
  return scm_procedure_name (SCM_CDR (m));
}
#undef FUNC_NAME


SCM_DEFINE (scm_macro_transformer, "macro-transformer", 1, 0, 0, 
            (SCM m),
	    "")
#define FUNC_NAME s_scm_macro_transformer
{
  SCM_VALIDATE_SMOB (1,m,macro);
  return SCM_CLOSUREP (SCM_CDR (m)) ? SCM_CDR (m) : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM
scm_make_synt (const char *name, SCM (*macroizer) (), SCM (*fcn)() )
{
  SCM symcell = scm_sysintern (name, SCM_UNDEFINED);
  SCM transformer = scm_make_subr_opt (name, scm_tc7_subr_2, fcn, 0);
  SCM_SETCDR (symcell, macroizer (transformer));
  return SCM_CAR (symcell);
}

void
scm_init_macros ()
{
  scm_tc16_macro = scm_make_smob_type_mfpe ("macro", 0,
                                           scm_markcdr, NULL, NULL, NULL);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/macros.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

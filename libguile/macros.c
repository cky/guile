/* Copyright (C) 1995,1996,1997,1998,2000,2001 Free Software Foundation, Inc.
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
#include "libguile/alist.h" /* for SCM_EXTEND_ENV (well...) */
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/root.h"
#include "libguile/smob.h"

#include "libguile/validate.h"
#include "libguile/macros.h"

scm_t_bits scm_tc16_macro;


static int
macro_print (SCM macro, SCM port, scm_print_state *pstate)
{
  SCM code = SCM_MACRO_CODE (macro);
  if (!SCM_CLOSUREP (code)
      || SCM_FALSEP (scm_procedure_p (SCM_PRINT_CLOSURE))
      || SCM_FALSEP (scm_printer_apply (SCM_PRINT_CLOSURE,
					macro, port, pstate)))
    {
      if (!SCM_CLOSUREP (code))
	scm_puts ("#<primitive-", port);
      else
	scm_puts ("#<", port);

      if (SCM_MACRO_TYPE (macro) == 0)
	scm_puts ("syntax", port);
      else if (SCM_MACRO_TYPE (macro) == 1)
	scm_puts ("macro", port);
      if (SCM_MACRO_TYPE (macro) == 2)
	scm_puts ("macro!", port);
      scm_putc (' ', port);
      scm_iprin1 (scm_macro_name (macro), port, pstate);

      if (SCM_CLOSUREP (code) && SCM_PRINT_SOURCE_P)
	{
	  SCM formals = SCM_CLOSURE_FORMALS (code);
	  SCM env = SCM_ENV (code);
	  SCM xenv = SCM_EXTEND_ENV (formals, SCM_EOL, env);
	  SCM src = scm_unmemocopy (SCM_CODE (code), xenv);
	  scm_putc (' ', port);
	  scm_iprin1 (src, port, pstate);
	}

      scm_putc ('>', port);
    }

  return 1;
}


SCM_DEFINE (scm_makacro, "procedure->syntax", 1, 0, 0,
            (SCM code),
	    "Return a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, returns the\n"
	    "result of applying @var{code} to the expression and the\n"
	    "environment.")
#define FUNC_NAME s_scm_makacro
{
  SCM_VALIDATE_PROC (1,code);
  SCM_RETURN_NEWSMOB (scm_tc16_macro, SCM_UNPACK (code));
}
#undef FUNC_NAME


SCM_DEFINE (scm_makmacro, "procedure->macro", 1, 0, 0, 
           (SCM code),
	    "Return a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, evaluates the\n"
	    "result of applying @var{code} to the expression and the\n"
	    "environment.  The value returned from @var{code} which has been\n"
	    "passed to @code{procedure->memoizing-macro} replaces the form\n"
	    "passed to @var{code}.  For example:\n"
	    "\n"
	    "@lisp\n"
	    "(define trace\n"
	    "  (procedure->macro\n"
	    "   (lambda (x env) `(set! ,(cadr x) (tracef ,(cadr x) ',(cadr x))))))\n\n"
	    "(trace @i{foo}) @equiv{} (set! @i{foo} (tracef @i{foo} '@i{foo})).\n"
	    "@end lisp")
#define FUNC_NAME s_scm_makmacro
{
  SCM_VALIDATE_PROC (1,code);
  SCM_RETURN_NEWSMOB (scm_tc16_macro | (1L << 16), SCM_UNPACK (code));
}
#undef FUNC_NAME


SCM_DEFINE (scm_makmmacro, "procedure->memoizing-macro", 1, 0, 0, 
           (SCM code),
	    "Return a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, evaluates the\n"
	    "result of applying @var{proc} to the expression and the\n"
	    "environment.  The value returned from @var{proc} which has been\n"
	    "passed to @code{procedure->memoizing-macro} replaces the form\n"
	    "passed to @var{proc}.  For example:\n"
	    "\n"
	    "@lisp\n"
	    "(define trace\n"
	    "  (procedure->macro\n"
	    "   (lambda (x env) `(set! ,(cadr x) (tracef ,(cadr x) ',(cadr x))))))\n\n"
	    "(trace @i{foo}) @equiv{} (set! @i{foo} (tracef @i{foo} '@i{foo})).\n"
	    "@end lisp")
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
  return SCM_BOOL (SCM_TYP16_PREDICATE (scm_tc16_macro, obj));
}
#undef FUNC_NAME


SCM_SYMBOL (scm_sym_syntax, "syntax");
SCM_SYMBOL (scm_sym_macro, "macro");
SCM_SYMBOL (scm_sym_mmacro, "macro!");

SCM_DEFINE (scm_macro_type, "macro-type", 1, 0, 0, 
            (SCM m),
	    "Return one of the symbols @code{syntax}, @code{macro} or\n"
	    "@code{macro!}, depending on whether @var{m} is a syntax\n"
	    "tranformer, a regular macro, or a memoizing macro,\n"
	    "respectively.  If @var{m} is not a macro, @code{#f} is\n"
	    "returned.")
#define FUNC_NAME s_scm_macro_type
{
  if (!SCM_TYP16_PREDICATE (scm_tc16_macro, m))
    return SCM_BOOL_F;
  switch (SCM_MACRO_TYPE (m))
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
	    "Return the name of the macro @var{m}.")
#define FUNC_NAME s_scm_macro_name
{
  SCM_VALIDATE_SMOB (1,m,macro);
  return scm_procedure_name (SCM_PACK (SCM_SMOB_DATA (m)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_macro_transformer, "macro-transformer", 1, 0, 0, 
            (SCM m),
	    "Return the transformer of the macro @var{m}.")
#define FUNC_NAME s_scm_macro_transformer
{
  SCM_VALIDATE_SMOB (1,m,macro);
  return ((SCM_CLOSUREP (SCM_PACK (SCM_SMOB_DATA (m)))) ?
	  SCM_PACK(SCM_SMOB_DATA (m)) : SCM_BOOL_F);
}
#undef FUNC_NAME

SCM
scm_make_synt (const char *name, SCM (*macroizer) (), SCM (*fcn)() )
{
  SCM var = scm_c_define (name, SCM_UNDEFINED);
  SCM transformer = scm_c_make_subr (name, scm_tc7_subr_2, fcn);
  SCM_VARIABLE_SET (var, macroizer (transformer));
  return SCM_UNSPECIFIED;
}

void
scm_init_macros ()
{
  scm_tc16_macro = scm_make_smob_type ("macro", 0);
  scm_set_smob_mark (scm_tc16_macro, scm_markcdr);
  scm_set_smob_print (scm_tc16_macro, macro_print);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/macros.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

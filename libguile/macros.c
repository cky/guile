/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003, 2006, 2008, 2009 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define SCM_BUILDING_DEPRECATED_CODE

#include "libguile/_scm.h"
#include "libguile/alist.h" /* for SCM_EXTEND_ENV (well...) */
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/programs.h"
#include "libguile/macros.h"

#include "libguile/private-options.h"

scm_t_bits scm_tc16_macro;


static int
macro_print (SCM macro, SCM port, scm_print_state *pstate)
{
  SCM code = SCM_MACRO_CODE (macro);

  scm_puts ("#<", port);

  if (SCM_MACRO_TYPE (macro) < 4 && SCM_MACRO_IS_EXTENDED (macro))
    scm_puts ("extended-", port);

  /* FIXME: doesn't catch boot closures; but do we care? */
  if (!SCM_PROGRAM_P (code))
    scm_puts ("primitive-", port);

  if (SCM_MACRO_TYPE (macro) == 0)
    scm_puts ("syntax", port);
#if SCM_ENABLE_DEPRECATED == 1
  if (SCM_MACRO_TYPE (macro) == 1)
    scm_puts ("macro", port);
#endif
  if (SCM_MACRO_TYPE (macro) == 2)
    scm_puts ("macro!", port);
  if (SCM_MACRO_TYPE (macro) == 3)
    scm_puts ("builtin-macro!", port);
  if (SCM_MACRO_TYPE (macro) == 4)
    scm_puts ("syncase-macro", port);

  scm_putc (' ', port);
  scm_iprin1 (scm_macro_name (macro), port, pstate);

  if (SCM_MACRO_IS_EXTENDED (macro))
    {
      scm_putc (' ', port);
      scm_write (SCM_SMOB_OBJECT_2 (macro), port);
      scm_putc (' ', port);
      scm_write (SCM_SMOB_OBJECT_3 (macro), port);
    }

  scm_putc ('>', port);

  return 1;
}

static SCM
makmac (SCM code, scm_t_bits flags)
{
  SCM z;
  SCM_NEWSMOB (z, scm_tc16_macro, SCM_UNPACK (code));
  SCM_SET_SMOB_FLAGS (z, flags);
  return z;
}

/* Return a mmacro that is known to be one of guile's built in macros. */
SCM
scm_i_makbimacro (SCM code)
#define FUNC_NAME "scm_i_makbimacro"
{
  SCM_VALIDATE_PROC (1, code);
  return makmac (code, 3);
}
#undef FUNC_NAME


SCM_DEFINE (scm_makmmacro, "procedure->memoizing-macro", 1, 0, 0, 
           (SCM code),
	    "Return a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, evaluates the\n"
	    "result of applying @var{code} to the expression and the\n"
	    "environment.\n\n"
	    "@code{procedure->memoizing-macro} is the same as\n"
	    "@code{procedure->macro}, except that the expression returned by\n"
	    "@var{code} replaces the original macro expression in the memoized\n"
	    "form of the containing code.")
#define FUNC_NAME s_scm_makmmacro
{
  SCM_VALIDATE_PROC (1, code);
  return makmac (code, 2);
}
#undef FUNC_NAME


SCM_DEFINE (scm_makacro, "procedure->syntax", 1, 0, 0,
            (SCM code),
	    "Return a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, returns the\n"
	    "result of applying @var{code} to the expression and the\n"
	    "environment.")
#define FUNC_NAME s_scm_makacro
{
  SCM_VALIDATE_PROC (1, code);
  return makmac (code, 0);
}
#undef FUNC_NAME


#if SCM_ENABLE_DEPRECATED == 1

SCM_DEFINE (scm_makmacro, "procedure->macro", 1, 0, 0, 
           (SCM code),
	    "Return a @dfn{macro} which, when a symbol defined to this value\n"
	    "appears as the first symbol in an expression, evaluates the\n"
	    "result of applying @var{code} to the expression and the\n"
	    "environment.  For example:\n"
	    "\n"
	    "@lisp\n"
	    "(define trace\n"
	    "  (procedure->macro\n"
	    "   (lambda (x env) `(set! ,(cadr x) (tracef ,(cadr x) ',(cadr x))))))\n\n"
	    "(trace @i{foo}) @equiv{} (set! @i{foo} (tracef @i{foo} '@i{foo})).\n"
	    "@end lisp")
#define FUNC_NAME s_scm_makmacro
{
  scm_c_issue_deprecation_warning
    ("The function procedure->macro is deprecated, and so are"
     " non-memoizing macros in general.  Use memoizing macros"
     " or r5rs macros instead.");

  SCM_VALIDATE_PROC (1, code);
  return makmac (code, 1);
}
#undef FUNC_NAME

#endif

SCM_DEFINE (scm_make_syncase_macro, "make-syncase-macro", 2, 0, 0,
            (SCM type, SCM binding),
	    "Return a @dfn{macro} that requires expansion by syntax-case.\n"
            "While users should not call this function, it is useful to know\n"
            "that syntax-case macros are represented as Guile primitive macros.")
#define FUNC_NAME s_scm_make_syncase_macro
{
  SCM z;
  SCM_VALIDATE_SYMBOL (1, type);

  SCM_NEWSMOB3 (z, scm_tc16_macro, SCM_UNPACK (binding), SCM_UNPACK (type),
                SCM_UNPACK (binding));
  SCM_SET_SMOB_FLAGS (z, 4 | SCM_F_MACRO_EXTENDED);
  return z;
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_extended_syncase_macro, "make-extended-syncase-macro", 3, 0, 0,
            (SCM m, SCM type, SCM binding),
	    "Extend a core macro @var{m} with a syntax-case binding.")
#define FUNC_NAME s_scm_make_extended_syncase_macro
{
  SCM z;
  SCM_VALIDATE_SMOB (1, m, macro);
  SCM_VALIDATE_SYMBOL (2, type);

  SCM_NEWSMOB3 (z, scm_tc16_macro, SCM_SMOB_DATA (m), SCM_UNPACK (type),
                SCM_UNPACK (binding));
  SCM_SET_SMOB_FLAGS (z, SCM_SMOB_FLAGS (m) | SCM_F_MACRO_EXTENDED);
  return z;
}
#undef FUNC_NAME



SCM_DEFINE (scm_macro_p, "macro?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a regular macro, a memoizing macro, a\n"
	    "syntax transformer, or a syntax-case macro.")
#define FUNC_NAME s_scm_macro_p
{
  return scm_from_bool (SCM_SMOB_PREDICATE (scm_tc16_macro, obj));
}
#undef FUNC_NAME


SCM_SYMBOL (scm_sym_syntax, "syntax");
#if SCM_ENABLE_DEPRECATED == 1
SCM_SYMBOL (scm_sym_macro, "macro");
#endif
SCM_SYMBOL (scm_sym_mmacro, "macro!");
SCM_SYMBOL (scm_sym_bimacro, "builtin-macro!");
SCM_SYMBOL (scm_sym_syncase_macro, "syncase-macro");

SCM_DEFINE (scm_macro_type, "macro-type", 1, 0, 0, 
            (SCM m),
	    "Return one of the symbols @code{syntax}, @code{macro},\n"
	    "@code{macro!}, or @code{syntax-case}, depending on whether\n"
            "@var{m} is a syntax transformer, a regular macro, a memoizing\n"
            "macro, or a syntax-case macro, respectively.  If @var{m} is\n"
            "not a macro, @code{#f} is returned.")
#define FUNC_NAME s_scm_macro_type
{
  if (!SCM_SMOB_PREDICATE (scm_tc16_macro, m))
    return SCM_BOOL_F;
  switch (SCM_MACRO_TYPE (m))
    {
    case 0: return scm_sym_syntax;
#if SCM_ENABLE_DEPRECATED == 1
    case 1: return scm_sym_macro;
#endif
    case 2: return scm_sym_mmacro;
    case 3: return scm_sym_bimacro;
    case 4: return scm_sym_syncase_macro;
    default: scm_wrong_type_arg (FUNC_NAME, 1, m);
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_macro_name, "macro-name", 1, 0, 0, 
            (SCM m),
	    "Return the name of the macro @var{m}.")
#define FUNC_NAME s_scm_macro_name
{
  SCM_VALIDATE_SMOB (1, m, macro);
  if (scm_is_true (scm_procedure_p (SCM_SMOB_OBJECT (m))))
    return scm_procedure_name (SCM_SMOB_OBJECT (m));
  return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_macro_transformer, "macro-transformer", 1, 0, 0, 
            (SCM m),
	    "Return the transformer of the macro @var{m}.")
#define FUNC_NAME s_scm_macro_transformer
{
  SCM data;

  SCM_VALIDATE_SMOB (1, m, macro);
  data = SCM_PACK (SCM_SMOB_DATA (m));
  
  if (scm_is_true (scm_procedure_p (data)))
    return data;
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_syncase_macro_type, "syncase-macro-type", 1, 0, 0, 
            (SCM m),
	    "Return the type of the macro @var{m}.")
#define FUNC_NAME s_scm_syncase_macro_type
{
  SCM_VALIDATE_SMOB (1, m, macro);

  if (SCM_MACRO_IS_EXTENDED (m))
    return SCM_SMOB_OBJECT_2 (m);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_syncase_macro_binding, "syncase-macro-binding", 1, 0, 0, 
            (SCM m),
	    "Return the binding of the macro @var{m}.")
#define FUNC_NAME s_scm_syncase_macro_binding
{
  SCM_VALIDATE_SMOB (1, m, macro);

  if (SCM_MACRO_IS_EXTENDED (m))
    return SCM_SMOB_OBJECT_3 (m);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM
scm_make_synt (const char *name, SCM (*macroizer) (), SCM (*fcn)() )
{
  SCM var = scm_c_define (name, SCM_UNDEFINED);
  SCM transformer = scm_c_make_gsubr (name, 2, 0, 0, fcn);
  SCM_VARIABLE_SET (var, macroizer (transformer));
  return SCM_UNSPECIFIED;
}

void
scm_init_macros ()
{
  scm_tc16_macro = scm_make_smob_type ("macro", 0);
  scm_set_smob_print (scm_tc16_macro, macro_print);
#include "libguile/macros.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

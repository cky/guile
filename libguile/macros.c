/* Copyright (C) 1995,1996,1997,1998,2000,2001,2002,2003, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/smob.h"
#include "libguile/validate.h"
#include "libguile/macros.h"

#include "libguile/private-options.h"


static scm_t_bits scm_tc16_macro;

#define SCM_MACROP(x) SCM_SMOB_PREDICATE (scm_tc16_macro, (x))
#define SCM_MACRO_PRIMITIVE(m) ((scm_t_macro_primitive)SCM_SMOB_DATA (m))
#define SCM_MACRO_NAME(m) (SCM_SMOB_OBJECT_2 (m))
#define SCM_MACRO_TYPE(m) (SCM_SMOB_OBJECT_3 (m))
#define SCM_MACRO_BINDING(m) (SCM_CELL_OBJECT ((m), 4))
#define SCM_VALIDATE_MACRO(p,v) SCM_MAKE_VALIDATE ((p), (v), MACROP)


SCM_API scm_t_bits scm_tc16_macro;


static int
macro_print (SCM macro, SCM port, scm_print_state *pstate)
{
  if (scm_is_false (SCM_MACRO_TYPE (macro)))
    scm_puts ("#<primitive-syntax-transformer ", port);
  else
    scm_puts ("#<syntax-transformer ", port);
  scm_iprin1 (scm_macro_name (macro), port, pstate);
  scm_putc ('>', port);

  return 1;
}

/* Return a mmacro that is known to be one of guile's built in macros. */
SCM
scm_i_make_primitive_macro (const char *name, scm_t_macro_primitive fn)
{
  SCM z = scm_words (scm_tc16_macro, 5);
  SCM_SET_SMOB_DATA_N (z, 1, (scm_t_bits)fn);
  SCM_SET_SMOB_OBJECT_N (z, 2, scm_from_locale_symbol (name));
  SCM_SET_SMOB_OBJECT_N (z, 3, SCM_BOOL_F);
  SCM_SET_SMOB_OBJECT_N (z, 4, SCM_BOOL_F);
  return z;
}

scm_t_macro_primitive
scm_i_macro_primitive (SCM macro)
{
  return SCM_MACRO_PRIMITIVE (macro);
}


SCM_DEFINE (scm_make_syntax_transformer, "make-syntax-transformer", 3, 0, 0,
            (SCM name, SCM type, SCM binding),
	    "Construct a @dfn{syntax transformer}.\n\n"
            "This function is part of Guile's low-level support for the psyntax\n"
            "syntax expander. Users should not call this function.")
#define FUNC_NAME s_scm_make_syntax_transformer
{
  SCM z;
  SCM (*prim)(SCM,SCM) = NULL;

  if (scm_is_true (name))
    {
      SCM existing_var;
      
      SCM_VALIDATE_SYMBOL (1, name);
      existing_var = scm_sym2var (name, scm_current_module_lookup_closure (),
                                  SCM_BOOL_F);
      if (scm_is_true (existing_var)
          && scm_is_true (scm_variable_bound_p (existing_var))
          && SCM_MACROP (SCM_VARIABLE_REF (existing_var)))
        prim = SCM_MACRO_PRIMITIVE (SCM_VARIABLE_REF (existing_var));
    }

  SCM_VALIDATE_SYMBOL (2, type);

  z = scm_words (scm_tc16_macro, 5);
  SCM_SET_SMOB_DATA_N (z, 1, prim);
  SCM_SET_SMOB_OBJECT_N (z, 2, name);
  SCM_SET_SMOB_OBJECT_N (z, 3, type);
  SCM_SET_SMOB_OBJECT_N (z, 4, binding);
  return z;
}
#undef FUNC_NAME

SCM_DEFINE (scm_macro_p, "macro?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a syntax transformer (an object that "
            "transforms Scheme expressions at expansion-time).\n\n"
            "Macros are actually just one kind of syntax transformer; this\n"
            "procedure has its name due to historical reasons.")
#define FUNC_NAME s_scm_macro_p
{
  return scm_from_bool (SCM_MACROP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_macro_type, "macro-type", 1, 0, 0, 
            (SCM m),
	    "Return the type of the syntax transformer @var{m}, as passed to\n"
            "@code{make-syntax-transformer}. If @var{m} is a primitive syntax\n"
            "transformer, @code{#f} will be returned.")
#define FUNC_NAME s_scm_macro_type
{
  SCM_VALIDATE_MACRO (1, m);
  return SCM_MACRO_TYPE (m);
}
#undef FUNC_NAME

SCM_DEFINE (scm_macro_name, "macro-name", 1, 0, 0, 
            (SCM m),
	    "Return the name of the syntax transformer @var{m}.")
#define FUNC_NAME s_scm_macro_name
{
  SCM_VALIDATE_MACRO (1, m);
  return SCM_MACRO_NAME (m);
}
#undef FUNC_NAME

SCM_DEFINE (scm_macro_transformer, "macro-transformer", 1, 0, 0, 
            (SCM m),
	    "Return the transformer procedure of the macro @var{m}.\n\n"
            "If @var{m} is a syntax transformer but not a macro, @code{#f}\n"
            "will be returned. (This can happen, for example, with primitive\n"
            "syntax transformers).")
#define FUNC_NAME s_scm_macro_transformer
{
  SCM_VALIDATE_MACRO (1, m);
  /* here we rely on knowledge of how psyntax represents macro bindings, but
     hey, there is code out there that calls this function, and expects to get
     a procedure in return... */
  if (scm_is_true (scm_procedure_p (SCM_MACRO_BINDING (m))))
    return SCM_MACRO_BINDING (m);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_macro_binding, "macro-binding", 1, 0, 0, 
            (SCM m),
	    "Return the binding of the syntax transformer @var{m}, as passed to\n"
            "@code{make-syntax-transformer}. If @var{m} is a primitive syntax\n"
            "transformer, @code{#f} will be returned.")
#define FUNC_NAME s_scm_macro_binding
{
  SCM_VALIDATE_MACRO (1, m);
  return SCM_MACRO_BINDING (m);
}
#undef FUNC_NAME


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

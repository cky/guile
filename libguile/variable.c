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




#include "libguile/_scm.h"
#include "libguile/eq.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/variable.h"


void
scm_i_variable_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<variable ", port);
  scm_intprint (SCM_UNPACK (exp), 16, port);
  scm_puts (" binding: ", port);
  scm_iprin1 (SCM_VARIABLE_REF (exp), port, pstate);
  scm_putc('>', port);
}



#if SCM_ENABLE_VCELLS
SCM_SYMBOL (sym_huh, "???");
#endif

static SCM
make_variable (SCM init)
{
#if !SCM_ENABLE_VCELLS
  {
    SCM z;
    SCM_NEWCELL (z);
    SCM_SET_CELL_WORD_1 (z, SCM_UNPACK (init));
    SCM_SET_CELL_TYPE (z, scm_tc7_variable);
    scm_remember_upto_here_1 (init);
    return z;
  }
#else
  {
    SCM z;
    SCM cell = scm_cons (sym_huh, init);
    SCM_NEWCELL (z);
    SCM_SET_CELL_WORD_1 (z, SCM_UNPACK (cell));
    SCM_SET_CELL_TYPE (z, scm_tc7_variable);
    scm_remember_upto_here_1 (cell);
    return z;
  }
#endif
}

SCM_DEFINE (scm_make_variable, "make-variable", 1, 0, 0, 
            (SCM init),
            "Return a variable initialized to value @var{init}.\n")
#define FUNC_NAME s_scm_make_variable
{
  return make_variable (init);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_undefined_variable, "make-undefined-variable", 0, 0, 0, 
            (),
            "Return a variable that is initially unbound.\n")
#define FUNC_NAME s_scm_make_undefined_variable
{
  return make_variable (SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM_DEFINE (scm_variable_p, "variable?", 1, 0, 0, 
            (SCM obj),
            "Return @code{#t} iff @var{obj} is a variable object, else\n"
	    "return @code{#f}\n")
#define FUNC_NAME s_scm_variable_p
{
  return SCM_BOOL (SCM_VARIABLEP (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_variable_ref, "variable-ref", 1, 0, 0, 
            (SCM var),
            "Dereference @var{var} and return its value.\n"
            "@var{var} must be a variable object; see @code{make-variable}\n"
	    "and @code{make-undefined-variable}.")
#define FUNC_NAME s_scm_variable_ref
{
  SCM val;
  SCM_VALIDATE_VARIABLE (1, var);
  val = SCM_VARIABLE_REF (var);
  if (val == SCM_UNDEFINED)
    SCM_MISC_ERROR ("variable is unbound: ~S", scm_list_1 (var));
  return val;
}
#undef FUNC_NAME

SCM_DEFINE (scm_variable_set_x, "variable-set!", 2, 0, 0,
            (SCM var, SCM val),
            "Set the value of the variable @var{var} to @var{val}.\n"
            "@var{var} must be a variable object, @var{val} can be any\n"
	    "value. Return an unspecified value.\n")
#define FUNC_NAME s_scm_variable_set_x
{
  SCM_VALIDATE_VARIABLE (1, var);
  SCM_VARIABLE_SET (var, val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_variable_bound_p, "variable-bound?", 1, 0, 0, 
            (SCM var),
            "Return @code{#t} iff @var{var} is bound to a value.\n"
            "Throws an error if @var{var} is not a variable object.\n")
#define FUNC_NAME s_scm_variable_bound_p
{
  SCM_VALIDATE_VARIABLE (1, var);
  return SCM_BOOL (SCM_VARIABLE_REF (var) != SCM_UNDEFINED);
}
#undef FUNC_NAME

SCM_DEFINE (scm_variable_set_name_hint, "variable-set-name-hint!", 2, 0, 0,
	    (SCM var, SCM hint),
	    "Do not use this function.")
#define FUNC_NAME s_scm_variable_set_name_hint
{
  SCM_VALIDATE_VARIABLE (1, var);
  SCM_VALIDATE_SYMBOL (2, hint);
#if SCM_ENABLE_VCELLS
  SCM_SETCAR (SCM_SMOB_DATA (var), hint);
#endif
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#if SCM_ENABLE_VCELLS

SCM_DEFINE (scm_builtin_variable, "builtin-variable", 1, 0, 0, 
            (SCM name),
            "Return the built-in variable with the name @var{name}.\n"
            "@var{name} must be a symbol (not a string).\n"
            "Then use @code{variable-ref} to access its value.\n")
#define FUNC_NAME s_scm_builtin_variable
{
  SCM_VALIDATE_SYMBOL (1,name);
  scm_c_issue_deprecation_warning ("`builtin-variable' is deprecated. "
				   "Use module system operations instead.");
  return scm_sym2var (name, SCM_BOOL_F, SCM_BOOL_T);
}
#undef FUNC_NAME

#endif /* SCM_ENABLE_VCELLS */

void
scm_init_variable ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/variable.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

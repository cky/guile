/*	Copyright (C) 1995, 1996, 1997, 1998, 2000 Free Software Foundation, Inc.
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
#include "libguile/eq.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/smob.h"

#include "libguile/validate.h"
#include "libguile/variable.h"

scm_bits_t scm_tc16_variable;

static int
variable_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<variable ", port);
  scm_intprint(SCM_UNPACK (exp), 16, port);
  {
    SCM val_cell;
    val_cell = SCM_CDR(exp);
    if (!SCM_UNBNDP (SCM_CAR (val_cell)))
      {
	scm_puts (" name: ", port);
	scm_iprin1 (SCM_CAR (val_cell), port, pstate);
      }
    scm_puts (" binding: ", port);
    scm_iprin1 (SCM_CDR (val_cell), port, pstate);
  }
  scm_putc('>', port);
  return 1;
}

static SCM
variable_equalp (SCM var1, SCM var2)
{
  return scm_equal_p (SCM_CDR (var1), SCM_CDR (var2));
}


static SCM anonymous_variable_sym;


static SCM
make_vcell_variable (SCM vcell)
{
  SCM_RETURN_NEWSMOB (scm_tc16_variable, SCM_UNPACK (vcell));
}

SCM_DEFINE (scm_make_variable, "make-variable", 1, 1, 0, 
            (SCM init, SCM name_hint),
            "Return a variable object initialized to value INIT.\n"
            "If given, uses NAME-HINT as its internal (debugging)\n"
            "name, otherwise just treat it as an anonymous variable.\n"
            "Remember, of course, that multiple bindings to the same\n"
            "variable may exist, so NAME-HINT is just that---a hint.\n")
#define FUNC_NAME s_scm_make_variable
{
  SCM val_cell;
  
  if (SCM_UNBNDP (name_hint))
    name_hint = anonymous_variable_sym;

  SCM_NEWCELL(val_cell);
  SCM_DEFER_INTS;
  SCM_SETCAR (val_cell, name_hint);
  SCM_SETCDR (val_cell, init);
  SCM_ALLOW_INTS;
  return make_vcell_variable (val_cell);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_undefined_variable, "make-undefined-variable", 0, 1, 0, 
            (SCM name_hint),
            "Return a variable object initialized to an undefined value.\n"
            "If given, uses NAME-HINT as its internal (debugging)\n"
            "name, otherwise just treat it as an anonymous variable.\n"
            "Remember, of course, that multiple bindings to the same\n"
            "variable may exist, so NAME-HINT is just that---a hint.\n")
#define FUNC_NAME s_scm_make_undefined_variable
{
  SCM vcell;

  if (SCM_UNBNDP (name_hint))
    name_hint = anonymous_variable_sym;

  SCM_NEWCELL (vcell);
  SCM_DEFER_INTS;
  SCM_SETCAR (vcell, name_hint);
  SCM_SETCDR (vcell, SCM_UNDEFINED);
  SCM_ALLOW_INTS;
  return make_vcell_variable (vcell);
}
#undef FUNC_NAME


SCM_DEFINE (scm_variable_p, "variable?", 1, 0, 0, 
            (SCM obj),
            "Return #t iff OBJ is a variable object, else return #f\n")
#define FUNC_NAME s_scm_variable_p
{
  return SCM_BOOL (SCM_VARIABLEP (obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_variable_ref, "variable-ref", 1, 0, 0, 
            (SCM var),
            "Dereference VAR and return its value.\n"
            "VAR must be a variable object;  see `make-variable' and\n"
            "`make-undefined-variable'")
#define FUNC_NAME s_scm_variable_ref
{
  SCM_VALIDATE_VARIABLE (1, var);
  return SCM_CDR (SCM_CDR (var));
}
#undef FUNC_NAME



SCM_DEFINE (scm_variable_set_x, "variable-set!", 2, 0, 0,
            (SCM var, SCM val),
            "Set the value of the variable VAR to VAL.\n"
            "VAR must be a variable object, VAL can be any value.\n"
            "Returns an unspecified value.\n")
#define FUNC_NAME s_scm_variable_set_x
{
  SCM_VALIDATE_VARIABLE (1,var);
  SCM_SETCDR (SCM_CDR (var), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_builtin_variable, "builtin-variable", 1, 0, 0, 
            (SCM name),
            "Return the built-in variable with the name NAME.\n"
            "NAME must be a symbol (not a string).\n"
            "Then use `variable-ref' to access its value.\n")
#define FUNC_NAME s_scm_builtin_variable
{
  SCM vcell;
  SCM var_slot;

  SCM_VALIDATE_SYMBOL (1,name);
  vcell = scm_sym2vcell (name, SCM_BOOL_F, SCM_BOOL_T);
  if (SCM_FALSEP (vcell))
    return SCM_BOOL_F;

  scm_intern_symbol (scm_symhash_vars, name);
  var_slot = scm_sym2ovcell (name, scm_symhash_vars);

  SCM_DEFER_INTS;
  if (SCM_IMP (SCM_CDR (var_slot))
      || !SCM_EQ_P (SCM_VARVCELL (var_slot), vcell))
    SCM_SETCDR (var_slot, make_vcell_variable (vcell));
  SCM_ALLOW_INTS;

  return SCM_CDR (var_slot);
}
#undef FUNC_NAME


SCM_DEFINE (scm_variable_bound_p, "variable-bound?", 1, 0, 0, 
            (SCM var),
            "Return #t iff VAR is bound to a value.\n"
            "Throws an error if VAR is not a variable object.\n")
#define FUNC_NAME s_scm_variable_bound_p
{
  SCM_VALIDATE_VARIABLE (1,var);
  return SCM_NEGATE_BOOL(SCM_UNBNDP (SCM_CDR (SCM_VARVCELL (var))));
}
#undef FUNC_NAME




void
scm_init_variable ()
{
  scm_tc16_variable = scm_make_smob_type ("variable", 0);
  scm_set_smob_mark (scm_tc16_variable, scm_markcdr);
  scm_set_smob_print (scm_tc16_variable, variable_print);
  scm_set_smob_equalp (scm_tc16_variable, variable_equalp);

  anonymous_variable_sym = SCM_CAR (scm_sysintern ("anonymous-variable", SCM_UNDEFINED));
#ifndef SCM_MAGIC_SNARFER
#include "libguile/variable.x"
#endif
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

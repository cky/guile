/*	Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
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


#include <stdio.h>
#include "_scm.h"
#include "eq.h"
#include "genio.h"
#include "smob.h"

#include "variable.h"


static scm_sizet free_var SCM_P ((SCM obj));

static scm_sizet
free_var (obj)
     SCM obj;
{
  return 0;
}



static int prin_var SCM_P ((SCM exp, SCM port, scm_print_state *pstate));

static int
prin_var (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  scm_puts ("#<variable ", port);
  scm_intprint(exp, 16, port);
  {
    SCM val_cell;
    val_cell = SCM_CDR(exp);
    if (SCM_CAR (val_cell) != SCM_UNDEFINED)
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


static SCM scm_markvar SCM_P ((SCM ptr));

static SCM 
scm_markvar (ptr)
     SCM ptr;
{
  return SCM_CDR (ptr);
}

static SCM var_equal SCM_P ((SCM var1, SCM var2));

static SCM
var_equal (var1, var2)
     SCM var1;
     SCM var2;
{
  return scm_equal_p (SCM_CDR (var1), SCM_CDR (var2));
}

int scm_tc16_variable;
static scm_smobfuns variable_smob = {scm_markvar, free_var, prin_var, var_equal};


static SCM anonymous_variable_sym;


static SCM make_vcell_variable SCM_P ((SCM vcell));

static SCM
make_vcell_variable (vcell)
     SCM vcell;
{
  SCM answer;
  SCM_NEWCELL(answer);
  SCM_REDEFER_INTS;
  SCM_SETCAR (answer, scm_tc16_variable);
  SCM_SETCDR (answer, vcell);
  SCM_REALLOW_INTS;
  return answer;
}

SCM_PROC(s_make_variable, "make-variable", 1, 1, 0, scm_make_variable);

SCM
scm_make_variable (init, name_hint)
     SCM init;
     SCM name_hint;
{
  SCM val_cell;
  
  if (name_hint == SCM_UNDEFINED)
    name_hint = anonymous_variable_sym;

  SCM_NEWCELL(val_cell);
  SCM_DEFER_INTS;
  SCM_SETCAR (val_cell, name_hint);
  SCM_SETCDR (val_cell, init);
  SCM_ALLOW_INTS;
  return make_vcell_variable (val_cell);
}


SCM_PROC(s_make_undefined_variable, "make-undefined-variable", 0, 1, 0, scm_make_undefined_variable);

SCM
scm_make_undefined_variable (name_hint)
     SCM name_hint;
{
  SCM vcell;

  if (name_hint == SCM_UNDEFINED)
    name_hint = anonymous_variable_sym;

  SCM_NEWCELL (vcell);
  SCM_DEFER_INTS;
  SCM_SETCAR (vcell, name_hint);
  SCM_SETCDR (vcell, SCM_UNDEFINED);
  SCM_ALLOW_INTS;
  return make_vcell_variable (vcell);
}


SCM_PROC(s_variable_p, "variable?", 1, 0, 0, scm_variable_p);

SCM
scm_variable_p (obj)
     SCM obj;
{
  return ( (SCM_NIMP(obj) && SCM_VARIABLEP (obj))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_PROC(s_variable_ref, "variable-ref", 1, 0, 0, scm_variable_ref);

SCM
scm_variable_ref (var)
     SCM var;
{
  SCM_ASSERT (SCM_NIMP(var) && SCM_VARIABLEP(var), var, SCM_ARG1, s_variable_ref);
  return SCM_CDR (SCM_CDR (var));
}



SCM_PROC(s_variable_set_x, "variable-set!", 2, 0, 0, scm_variable_set_x);

SCM
scm_variable_set_x (var, val)
     SCM var;
     SCM val;
{
  SCM_ASSERT (SCM_NIMP(var) && SCM_VARIABLEP (var), var, SCM_ARG1, s_variable_set_x);
  SCM_SETCDR (SCM_CDR (var), val);
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_builtin_variable, "builtin-variable", 1, 0, 0, scm_builtin_variable);

SCM
scm_builtin_variable (name)
     SCM name;
{
  SCM vcell;
  SCM var_slot;

  SCM_ASSERT (SCM_NIMP (name) && SCM_SYMBOLP (name), name, SCM_ARG1, s_builtin_variable);
  vcell = scm_sym2vcell (name, SCM_BOOL_F, SCM_BOOL_T);
  if (vcell == SCM_BOOL_F)
    return SCM_BOOL_F;

  scm_intern_symbol (scm_symhash_vars, name);
  var_slot = scm_sym2ovcell (name, scm_symhash_vars);

  SCM_DEFER_INTS;
  if (   SCM_IMP (SCM_CDR (var_slot))
      || (SCM_VARVCELL (var_slot) != vcell))
    SCM_SETCDR (var_slot, make_vcell_variable (vcell));
  SCM_ALLOW_INTS;

  return SCM_CDR (var_slot);
}


SCM_PROC(s_variable_bound_p, "variable-bound?", 1, 0, 0, scm_variable_bound_p);

SCM 
scm_variable_bound_p (var)
     SCM var;
{
  SCM_ASSERT (SCM_NIMP(var) && SCM_VARIABLEP (var), var, SCM_ARG1, s_variable_bound_p);
  return (SCM_UNBNDP (SCM_CDR (SCM_VARVCELL (var)))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}




void
scm_init_variable ()
{
  scm_tc16_variable = scm_newsmob (&variable_smob);
  anonymous_variable_sym = SCM_CAR (scm_sysintern ("anonymous-variable", SCM_UNDEFINED));
#include "variable.x"
}


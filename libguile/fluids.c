/* Copyright (C) 1996,1997,2000,2001 Free Software Foundation, Inc.
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
#include "libguile/print.h"
#include "libguile/smob.h"
#include "libguile/dynwind.h"
#include "libguile/fluids.h"
#include "libguile/alist.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/deprecation.h"
#include "libguile/lang.h"

#define INITIAL_FLUIDS 10
#include "libguile/validate.h"

static volatile long n_fluids;
scm_t_bits scm_tc16_fluid;

SCM
scm_make_initial_fluids ()
{
  return scm_c_make_vector (INITIAL_FLUIDS, SCM_BOOL_F);
}

static void
grow_fluids (scm_root_state *root_state, int new_length)
{
  SCM old_fluids, new_fluids;
  long old_length, i;

  old_fluids = root_state->fluids;
  old_length = SCM_VECTOR_LENGTH (old_fluids);
  new_fluids = scm_c_make_vector (new_length, SCM_BOOL_F);
  i = 0;
  while (i < old_length)
    {
      SCM_VELTS(new_fluids)[i] = SCM_VELTS(old_fluids)[i];
      i++;
    }
  while (i < new_length)
    {
      SCM_VELTS(new_fluids)[i] = SCM_BOOL_F;
      i++;
    }

  root_state->fluids = new_fluids;
}

void
scm_copy_fluids (scm_root_state *root_state)
{
  grow_fluids (root_state, SCM_VECTOR_LENGTH (root_state->fluids));
}

static int
fluid_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<fluid ", port);
  scm_intprint ((int) SCM_FLUID_NUM (exp), 10, port);
  scm_putc ('>', port);
  return 1;
}

static long
next_fluid_num ()
{
  long n;
  SCM_CRITICAL_SECTION_START;
  n = n_fluids++;
  SCM_CRITICAL_SECTION_END;
  return n;
}

SCM_DEFINE (scm_make_fluid, "make-fluid", 0, 0, 0, 
	    (),
	    "Return a newly created fluid.\n"
	    "Fluids are objects of a certain type (a smob) that can hold one SCM\n"
	    "value per dynamic root.  That is, modifications to this value are\n"
	    "only visible to code that executes within the same dynamic root as\n"
	    "the modifying code.  When a new dynamic root is constructed, it\n"
	    "inherits the values from its parent.  Because each thread executes\n"
	    "in its own dynamic root, you can use fluids for thread local storage.")
#define FUNC_NAME s_scm_make_fluid
{
  long n;

  n = next_fluid_num ();
  SCM_RETURN_NEWSMOB (scm_tc16_fluid, n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_p, "fluid?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} iff @var{obj} is a fluid; otherwise, return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_fluid_p
{
  return SCM_BOOL(SCM_FLUIDP (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_ref, "fluid-ref", 1, 0, 0, 
	    (SCM fluid),
	    "Return the value associated with @var{fluid} in the current\n"
	    "dynamic root.  If @var{fluid} has not been set, then return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_fluid_ref
{
  unsigned long int n;

  SCM_VALIDATE_FLUID (1, fluid);
  n = SCM_FLUID_NUM (fluid);

  if (SCM_VECTOR_LENGTH (scm_root->fluids) <= n)
    grow_fluids (scm_root, n+1);
  return SCM_VELTS (scm_root->fluids)[n];
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_set_x, "fluid-set!", 2, 0, 0,
	    (SCM fluid, SCM value),
	    "Set the value associated with @var{fluid} in the current dynamic root.")
#define FUNC_NAME s_scm_fluid_set_x
{
  unsigned long int n;

  SCM_VALIDATE_FLUID (1, fluid);
  n = SCM_FLUID_NUM (fluid);

  if (SCM_VECTOR_LENGTH (scm_root->fluids) <= n)
    grow_fluids (scm_root, n+1);
  SCM_VELTS (scm_root->fluids)[n] = value;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_swap_fluids (SCM fluids, SCM vals)
{
  while (!SCM_NULL_OR_NIL_P (fluids))
    {
      SCM fl = SCM_CAR (fluids);
      SCM old_val = scm_fluid_ref (fl);
      scm_fluid_set_x (fl, SCM_CAR (vals));
      SCM_SETCAR (vals, old_val);
      fluids = SCM_CDR (fluids);
      vals = SCM_CDR (vals);
    }
}

/* Swap the fluid values in reverse order.  This is important when the
same fluid appears multiple times in the fluids list. */

void
scm_swap_fluids_reverse (SCM fluids, SCM vals)
{
  if (!SCM_NULL_OR_NIL_P (fluids))
    {
      SCM fl, old_val;

      scm_swap_fluids_reverse (SCM_CDR (fluids), SCM_CDR (vals));
      fl = SCM_CAR (fluids);
      old_val = scm_fluid_ref (fl);
      scm_fluid_set_x (fl, SCM_CAR (vals));
      SCM_SETCAR (vals, old_val);
    }
}


static SCM
apply_thunk (void *thunk)
{
  return scm_call_0 (SCM_PACK (thunk));
}

SCM_DEFINE (scm_with_fluids, "with-fluids*", 3, 0, 0, 
	    (SCM fluids, SCM values, SCM thunk),
	    "Set @var{fluids} to @var{values} temporary, and call @var{thunk}.\n"
	    "@var{fluids} must be a list of fluids and @var{values} must be the same\n"
	    "number of their values to be applied.  Each substitution is done\n"
	    "one after another.  @var{thunk} must be a procedure with no argument.")
#define FUNC_NAME s_scm_with_fluids
{
  return scm_c_with_fluids (fluids, values, apply_thunk, (void *) SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM
scm_c_with_fluids (SCM fluids, SCM values, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluids"
{
  SCM ans;
  long flen, vlen;

  SCM_VALIDATE_LIST_COPYLEN (1, fluids, flen);
  SCM_VALIDATE_LIST_COPYLEN (2, values, vlen);
  if (flen != vlen)
    scm_out_of_range (s_scm_with_fluids, values);

  scm_swap_fluids (fluids, values);
  scm_dynwinds = scm_acons (fluids, values, scm_dynwinds);
  ans = cproc (cdata);
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  scm_swap_fluids_reverse (fluids, values);
  return ans;
}
#undef FUNC_NAME

SCM
scm_c_with_fluid (SCM fluid, SCM value, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluid"
{
  return scm_c_with_fluids (scm_list_1 (fluid), scm_list_1 (value),
			    cproc, cdata);
}
#undef FUNC_NAME

void
scm_init_fluids ()
{
  scm_tc16_fluid = scm_make_smob_type ("fluid", 0);
  scm_set_smob_print (scm_tc16_fluid, fluid_print);
#ifndef SCM_MAGIC_SNARFER
#include "libguile/fluids.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

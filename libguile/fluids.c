/*	Copyright (C) 1996, 1997 Free Software Foundation, Inc.
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


#include "_scm.h"
#include "print.h"
#include "smob.h"
#include "genio.h"
#include "dynwind.h"
#include "fluids.h"
#include "alist.h"
#include "eval.h"

#define INITIAL_FLUIDS 10
#include "scm_validate.h"

static volatile int n_fluids;
long scm_tc16_fluid;

SCM
scm_make_initial_fluids ()
{
  return scm_make_vector (SCM_MAKINUM (INITIAL_FLUIDS),
			  SCM_BOOL_F);
}

static void
grow_fluids (scm_root_state *root_state,int new_length)
{
  SCM old_fluids, new_fluids;
  int old_length, i;

  old_fluids = root_state->fluids;
  old_length = SCM_LENGTH (old_fluids);
  new_fluids = scm_make_vector (SCM_MAKINUM (new_length), SCM_BOOL_F);
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
  grow_fluids (root_state, SCM_LENGTH(root_state->fluids));
}

static int
print_fluid (SCM exp, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<fluid ", port);
    scm_intprint (SCM_FLUID_NUM (exp), 10, port);
    scm_putc ('>', port);
    return 1;
}

static
int next_fluid_num ()
{
  int n;
#ifdef USE_THREADS
  SCM_THREAD_CRITICAL_SECTION_START;
#endif
  n = n_fluids++;
#ifdef USE_THREADS
  SCM_THREAD_CRITICAL_SECTION_END;
#endif
  return n;
}

SCM_DEFINE (scm_make_fluid, "make-fluid", 0, 0, 0, 
            (),
"")
#define FUNC_NAME s_scm_make_fluid
{
  int n;

  SCM_DEFER_INTS;
  n = next_fluid_num ();
  SCM_RETURN_NEWSMOB (scm_tc16_fluid, n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_p, "fluid?", 1, 0, 0, 
            (SCM fl),
"")
#define FUNC_NAME s_scm_fluid_p
{
  return SCM_BOOL(SCM_FLUIDP (fl));
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_ref, "fluid-ref", 1, 0, 0, 
            (SCM fl),
"")
#define FUNC_NAME s_scm_fluid_ref
{
  int n;

  SCM_VALIDATE_FLUID(1,fl);

  n = SCM_FLUID_NUM (fl);

  if (SCM_LENGTH (scm_root->fluids) <= n)
    grow_fluids (scm_root, n+1);
  return SCM_VELTS(scm_root->fluids)[n];
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_set_x, "fluid-set!", 2, 0, 0,
            (SCM fl, SCM val),
"")
#define FUNC_NAME s_scm_fluid_set_x
{
  int n;

  SCM_VALIDATE_FLUID(1,fl);
  n = SCM_FLUID_NUM (fl);

  if (SCM_LENGTH (scm_root->fluids) <= n)
    grow_fluids (scm_root, n+1);
  SCM_VELTS(scm_root->fluids)[n] = val;
  return val;
}
#undef FUNC_NAME

void
scm_swap_fluids (SCM fluids, SCM vals)
{
  while (SCM_NIMP (fluids))
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
  if (SCM_NIMP (fluids))
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
  return scm_apply ((SCM) thunk, SCM_EOL, SCM_EOL);
}

SCM_DEFINE (scm_with_fluids, "with-fluids*", 3, 0, 0, 
            (SCM fluids, SCM vals, SCM thunk),
"")
#define FUNC_NAME s_scm_with_fluids
{
  return scm_internal_with_fluids (fluids, vals, apply_thunk, (void *)thunk);
}
#undef FUNC_NAME

SCM
scm_internal_with_fluids (SCM fluids, SCM vals, SCM (*cproc) (), void *cdata)
{
  SCM ans;

  int flen = scm_ilength (fluids);
  int vlen = scm_ilength (vals);
  SCM_ASSERT (flen >= 0, fluids, SCM_ARG1, s_scm_with_fluids);
  SCM_ASSERT (vlen >= 0, vals, SCM_ARG2, s_scm_with_fluids);
  if (flen != vlen)
    scm_out_of_range (s_scm_with_fluids, vals);

  scm_swap_fluids (fluids, vals);
  scm_dynwinds = scm_acons (fluids, vals, scm_dynwinds);
  ans = cproc (cdata);
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  scm_swap_fluids_reverse (fluids, vals);
  return ans;
}



void
scm_init_fluids ()
{
  scm_tc16_fluid = scm_make_smob_type_mfpe ("fluid", 0,
                                           NULL, NULL, print_fluid, NULL);
#include "fluids.x"
}

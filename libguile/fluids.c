/*	Copyright (C) 1996 Free Software Foundation, Inc.
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

#include <assert.h>

#include "_scm.h"
#include "print.h"
#include "smob.h"
#include "dynwind.h"
#include "fluids.h"
#include "alist.h"
#include "eval.h"

#define INITIAL_FLUIDS 10

static volatile int n_fluids;
long scm_tc16_fluid;

SCM
scm_make_initial_fluids ()
{
  return scm_make_vector (SCM_MAKINUM (INITIAL_FLUIDS),
			  SCM_BOOL_F, SCM_BOOL_F);
}

static void grow_fluids SCM_P ((scm_root_state *, int new_length));
static void
grow_fluids (root_state, new_length)
     scm_root_state *root_state;
     int new_length;
{
  SCM old_fluids, new_fluids;
  int old_length, i;

  old_fluids = root_state->fluids;
  assert (SCM_NIMP (old_fluids) && SCM_VECTORP (old_fluids));
  old_length = SCM_LENGTH (old_fluids);
  assert (old_length <= new_length);
  new_fluids = scm_make_vector (SCM_MAKINUM (new_length),
				SCM_BOOL_F, SCM_BOOL_F);
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
scm_copy_fluids (root_state)
     scm_root_state *root_state;
{
  grow_fluids (root_state, SCM_LENGTH(root_state->fluids));
}

static int print_fluid SCM_P ((SCM exp, SCM port, scm_print_state *pstate));
static int
print_fluid (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
    scm_gen_puts (scm_regular_string, "#<fluid ", port);
    scm_intprint (SCM_FLUID_NUM (exp), 10, port);
    scm_gen_putc ('>', port);
    return 1;
}

static scm_smobfuns fluid_smob = {
    scm_mark0,
    scm_free0,
    print_fluid
};

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

SCM_PROC (s_make_fluid, "make-fluid", 0, 0, 0, scm_make_fluid);

SCM
scm_make_fluid ()
{
  SCM z;
  int n;

  SCM_DEFER_INTS;
  n = next_fluid_num ();
  SCM_NEWCELL (z);
  SCM_SETCAR (z, scm_tc16_fluid);
  SCM_SETCDR (z, n);
  SCM_ALLOW_INTS;
  
  return z;
}

SCM_PROC (s_fluid_p, "fluid?", 1, 0, 0, scm_fluid_p);

SCM
scm_fluid_p (fl)
     SCM fl;
{
  return (SCM_NIMP (fl) && SCM_FLUIDP (fl))? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC (s_fluid_ref, "fluid-ref", 1, 0, 0, scm_fluid_ref);

SCM
scm_fluid_ref (fl)
     SCM fl;
{
  int n;

  SCM_ASSERT (SCM_NIMP (fl) && SCM_FLUIDP (fl), fl, SCM_ARG1, s_fluid_ref);

  n = SCM_FLUID_NUM (fl);
  assert (n >= 0 && n < n_fluids);

  if (SCM_LENGTH (scm_root->fluids) <= n)
    grow_fluids (scm_root, n+1);
  return SCM_VELTS(scm_root->fluids)[n];
}

SCM_PROC (s_fluid_set_x, "fluid-set!", 2, 0, 0, scm_fluid_set_x);

SCM
scm_fluid_set_x (fl, val)
     SCM fl;
     SCM val;
{
  int n;

  SCM_ASSERT (SCM_NIMP (fl) && SCM_FLUIDP (fl), fl, SCM_ARG1, s_fluid_set_x);

  n = SCM_FLUID_NUM (fl);
  assert (n >= 0 && n < n_fluids);

  if (SCM_LENGTH (scm_root->fluids) <= n)
    grow_fluids (scm_root, n+1);
  SCM_VELTS(scm_root->fluids)[n] = val;
  return val;
}

void
scm_swap_fluids (fluids, vals)
     SCM fluids, vals;
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
scm_swap_fluids_reverse (fluids, vals)
     SCM fluids, vals;
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

SCM_PROC (s_with_fluids, "with-fluids*", 3, 0, 0, scm_with_fluids);

SCM
scm_internal_with_fluids (fluids, vals, cproc, cdata)
     SCM fluids, vals;
     SCM (*cproc) ();
     void *cdata;
{
  SCM ans;

  int flen = scm_ilength (fluids);
  int vlen = scm_ilength (vals);
  SCM_ASSERT (flen >= 0, fluids, SCM_ARG1, s_with_fluids);
  SCM_ASSERT (vlen >= 0, vals, SCM_ARG2, s_with_fluids);
  if (flen != vlen)
    scm_out_of_range (s_with_fluids, vals);

  scm_swap_fluids (fluids, vals);
  scm_dynwinds = scm_acons (fluids, vals, scm_dynwinds);
  ans = cproc (cdata);
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  scm_swap_fluids_reverse (fluids, vals);
  return ans;
}

static SCM
apply_thunk (void *thunk)
{
  return scm_apply ((SCM) thunk, SCM_EOL, SCM_EOL);
}

SCM
scm_with_fluids (fluids, vals, thunk)
     SCM fluids, vals, thunk;
{
  return scm_internal_with_fluids (fluids, vals, apply_thunk, (void *)thunk);
}

void
scm_init_fluids ()
{
  scm_tc16_fluid = scm_newsmob(&fluid_smob);
#include "fluids.x"
}

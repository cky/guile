/* classes: h_files */

#ifndef SCM_FLUIDS_H
#define SCM_FLUIDS_H

/* Copyright (C) 1996,2000,2001 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */



#include "libguile/__scm.h"
#include "libguile/root.h"
#include "libguile/vectors.h"

/* Fluids.

   Fluids are objects of a certain type (a smob) that can hold one SCM
   value per dynamic root.  That is, modifications to this value are
   only visible to code that executes within the same dynamic root as
   the modifying code.  When a new dynamic root is constructed, it
   inherits the values from its parent.  Because each thread executes
   in its own dynamic root, you can use fluids for thread local
   storage.

   Each fluid is identified by a small integer.  This integer is used
   to index a vector that holds the values of all fluids.  Each root
   has its own vector.

   Currently, you can't get rid a certain fluid if you don't use it
   any longer.  The slot that has been allocated for it in the fluid
   vector will not be reused for other fluids.  Therefore, only use
   SCM_MAKE_FLUID or its Scheme variant `make-fluid' in initialization
   code that is only run once.  Nevertheless, it should be possible to
   implement a more lightweight version of fluids on top of this basic
   mechanism. */

SCM_API scm_t_bits scm_tc16_fluid;

#define SCM_FLUIDP(x)    (!SCM_IMP (x) && (SCM_CELL_TYPE (x) == scm_tc16_fluid))
#define SCM_FLUID_NUM(x) (SCM_CELL_WORD_1 (x))

/* The fastest way to acces/modify the value of a fluid.  These macros
do no error checking at all.  You should only use them when you know
that the relevant fluid already exists in the current dynamic root.
The easiest way to ensure this is to execute a SCM_FLUID_SET_X in the
topmost root, for example right after SCM_MAKE_FLUID in your
SCM_INIT_MUMBLE routine that gets called from SCM_BOOT_GUILE_1.  The
first argument is the index number of the fluid, obtained via
SCM_FLUID_NUM, not the fluid itself. */

#define SCM_FAST_FLUID_REF(n) (SCM_VELTS(scm_root->fluids)[n])
#define SCM_FAST_FLUID_SET_X(n, val) (SCM_VELTS(scm_root->fluids)[n] = val)

SCM_API SCM scm_make_fluid (void);
SCM_API SCM scm_fluid_p (SCM fl);
SCM_API SCM scm_fluid_ref (SCM fluid);
SCM_API SCM scm_fluid_set_x (SCM fluid, SCM value);

SCM_API SCM scm_c_with_fluids (SCM fluids, SCM vals,
			       SCM (*cproc)(void *), void *cdata);
SCM_API SCM scm_c_with_fluid (SCM fluid, SCM val,
			      SCM (*cproc)(void *), void *cdata);
SCM_API SCM scm_with_fluids (SCM fluids, SCM vals, SCM thunk);

SCM_API SCM scm_make_initial_fluids (void);
SCM_API void scm_copy_fluids (scm_root_state *);
SCM_API void scm_swap_fluids (SCM fluids, SCM vals);
SCM_API void scm_swap_fluids_reverse (SCM fluids, SCM vals);

SCM_API void scm_init_fluids (void);

#endif  /* SCM_FLUIDS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

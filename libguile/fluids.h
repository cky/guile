/* classes: h_files */

#ifndef SCM_FLUIDS_H
#define SCM_FLUIDS_H

/* Copyright (C) 1996,2000,2001, 2006, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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



#include "libguile/__scm.h"
#include "libguile/root.h"
#include "libguile/vectors.h"

/* These "with-fluids" objects live on the dynamic stack, and record previous
   values of fluids. Guile uses shallow binding, so the current fluid values are
   always in the same place for a given thread, in the dynamic-state vector.
 */

#define SCM_WITH_FLUIDS_P(x) (!SCM_IMP (x) && SCM_TYP7(x) == scm_tc7_with_fluids)
#define SCM_WITH_FLUIDS_LEN(x) (SCM_CELL_WORD ((x), 0) >> 8)
#define SCM_WITH_FLUIDS_NTH_FLUID(x,n) (SCM_CELL_OBJECT ((x), 1 + (n)*2))
#define SCM_WITH_FLUIDS_NTH_VAL(x,n) (SCM_CELL_OBJECT ((x), 2 + (n)*2))
#define SCM_WITH_FLUIDS_SET_NTH_VAL(x,n,v) (SCM_SET_CELL_OBJECT ((x), 2 + (n)*2, (v)))


/* Fluids.

   Fluids are objects of a certain type that can hold one SCM value per
   dynamic state. That is, modifications to this value are only visible
   to code that executes with the same dynamic state as the modifying
   code. When a new dynamic state is constructed, it inherits the
   values from its parent. Because each thread executes with its own
   dynamic state, you can use fluids for thread local storage.

   Each fluid is identified by a small integer. This integer is used to
   index a vector that holds the values of all fluids. A dynamic state
   consists of this vector, wrapped in an object so that the vector can
   grow.
 */

#define SCM_FLUID_P(x)          (!SCM_IMP (x) && SCM_TYP7 (x) == scm_tc7_fluid)
#ifdef BUILDING_LIBGUILE
#define SCM_I_FLUID_NUM(x)        ((size_t)(SCM_CELL_WORD_0 (x) >> 8))
#define SCM_I_FLUID_DEFAULT(x)    (SCM_CELL_OBJECT_1 (x))
#endif

SCM_API SCM scm_make_fluid (void);
SCM_API SCM scm_make_fluid_with_default (SCM dflt);
SCM_API SCM scm_make_unbound_fluid (void);
SCM_API int scm_is_fluid (SCM obj);
SCM_API SCM scm_fluid_p (SCM fl);
SCM_API SCM scm_fluid_ref (SCM fluid);
SCM_API SCM scm_fluid_set_x (SCM fluid, SCM value);
SCM_API SCM scm_fluid_unset_x (SCM fluid);
SCM_API SCM scm_fluid_bound_p (SCM fluid);

SCM_INTERNAL SCM scm_i_make_with_fluids (size_t n, SCM *fluids, SCM *vals);
SCM_INTERNAL void scm_i_swap_with_fluids (SCM with_fluids, SCM dynamic_state);

SCM_API SCM scm_c_with_fluids (SCM fluids, SCM vals,
			       SCM (*cproc)(void *), void *cdata);
SCM_API SCM scm_c_with_fluid (SCM fluid, SCM val,
			      SCM (*cproc)(void *), void *cdata);
SCM_API SCM scm_with_fluids (SCM fluids, SCM vals, SCM thunk);
SCM_API SCM scm_with_fluid (SCM fluid, SCM val, SCM thunk);

SCM_API void scm_dynwind_fluid (SCM fluid, SCM value);

#ifdef BUILDING_LIBGUILE
#define SCM_I_DYNAMIC_STATE_P(x) (!SCM_IMP (x) && SCM_TYP7 (x) == scm_tc7_dynamic_state)
#define SCM_I_DYNAMIC_STATE_FLUIDS(x)        SCM_PACK (SCM_CELL_WORD_1 (x))
#endif

SCM_API SCM scm_make_dynamic_state (SCM parent);
SCM_API SCM scm_dynamic_state_p (SCM obj);
SCM_API int scm_is_dynamic_state (SCM obj);
SCM_API SCM scm_current_dynamic_state (void);
SCM_API SCM scm_set_current_dynamic_state (SCM state);
SCM_API void scm_dynwind_current_dynamic_state (SCM state);
SCM_API void *scm_c_with_dynamic_state (SCM state, 
					void *(*func)(void *), void *data);
SCM_API SCM scm_with_dynamic_state (SCM state, SCM proc);

SCM_INTERNAL SCM scm_i_make_initial_dynamic_state (void);

SCM_INTERNAL void scm_i_fluid_print (SCM exp, SCM port, scm_print_state *pstate);
SCM_INTERNAL void scm_i_dynamic_state_print (SCM exp, SCM port, scm_print_state *pstate);
SCM_INTERNAL void scm_i_with_fluids_print (SCM exp, SCM port, scm_print_state *pstate);
SCM_INTERNAL void scm_init_fluids (void);

#endif  /* SCM_FLUIDS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

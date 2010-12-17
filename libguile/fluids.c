/* Copyright (C) 1996,1997,2000,2001, 2004, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
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

#include <alloca.h>
#include <stdio.h>
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/print.h"
#include "libguile/dynwind.h"
#include "libguile/fluids.h"
#include "libguile/alist.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/deprecation.h"
#include "libguile/validate.h"
#include "libguile/bdw-gc.h"

/* Number of additional slots to allocate when ALLOCATED_FLUIDS is full.  */
#define FLUID_GROW 128

/* Vector of allocated fluids indexed by fluid numbers.  Access is protected by
   FLUID_ADMIN_MUTEX.  */
static void **allocated_fluids = NULL;
static size_t allocated_fluids_len = 0;

static scm_i_pthread_mutex_t fluid_admin_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

#define IS_FLUID(x)         SCM_FLUID_P (x)
#define FLUID_NUM(x)        SCM_I_FLUID_NUM (x)

#define IS_DYNAMIC_STATE(x) SCM_I_DYNAMIC_STATE_P (x)
#define DYNAMIC_STATE_FLUIDS(x)        SCM_I_DYNAMIC_STATE_FLUIDS (x)
#define SET_DYNAMIC_STATE_FLUIDS(x, y) SCM_SET_CELL_WORD_1 ((x), (SCM_UNPACK (y)))



/* Grow STATE so that it can hold up to ALLOCATED_FLUIDS_LEN fluids.  This may
   be more than necessary since ALLOCATED_FLUIDS is sparse and the current
   thread may not access all the fluids anyway.  Memory usage could be improved
   by using a 2-level array as is done in glibc for pthread keys (TODO).  */
static void
grow_dynamic_state (SCM state)
{
  SCM new_fluids;
  SCM old_fluids = DYNAMIC_STATE_FLUIDS (state);
  size_t i, len, old_len = SCM_SIMPLE_VECTOR_LENGTH (old_fluids);

  /* Assume the assignment below is atomic.  */
  len = allocated_fluids_len;

  new_fluids = scm_c_make_vector (len, SCM_BOOL_F);

  for (i = 0; i < old_len; i++)
    SCM_SIMPLE_VECTOR_SET (new_fluids, i,
			   SCM_SIMPLE_VECTOR_REF (old_fluids, i));
  SET_DYNAMIC_STATE_FLUIDS (state, new_fluids);
}

void
scm_i_fluid_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<fluid ", port);
  scm_intprint ((int) FLUID_NUM (exp), 10, port);
  scm_putc ('>', port);
}

void
scm_i_dynamic_state_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<dynamic-state ", port);
  scm_intprint (SCM_UNPACK (exp), 16, port);
  scm_putc ('>', port);
}

void
scm_i_with_fluids_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<with-fluids ", port);
  scm_intprint (SCM_UNPACK (exp), 16, port);
  scm_putc ('>', port);
}


/* Return a new fluid.  */
static SCM
new_fluid ()
{
  SCM fluid;
  size_t trial, n;

  /* Fluids are pointerless cells: the first word is the type tag; the second
     word is the fluid number.  */
  fluid = PTR2SCM (scm_gc_malloc_pointerless (sizeof (scm_t_cell), "fluid"));
  SCM_SET_CELL_TYPE (fluid, scm_tc7_fluid);

  scm_dynwind_begin (0);
  scm_i_dynwind_pthread_mutex_lock (&fluid_admin_mutex);

  for (trial = 0; trial < 2; trial++)
    {
      /* Look for a free fluid number.  */
      for (n = 0; n < allocated_fluids_len; n++)
	/* TODO: Use `__sync_bool_compare_and_swap' where available.  */
	if (allocated_fluids[n] == NULL)
	  break;

      if (trial == 0 && n >= allocated_fluids_len)
	/* All fluid numbers are in use.  Run a GC and retry.  Explicitly
	   running the GC is costly and bad-style.  We only do this because
	   dynamic state fluid vectors would grow unreasonably if fluid numbers
	   weren't reused.  */
	scm_i_gc ("fluids");
    }

  if (n >= allocated_fluids_len)
    {
      /* Grow the vector of allocated fluids.  */
      void **new_allocated_fluids =
	scm_gc_malloc_pointerless ((allocated_fluids_len + FLUID_GROW)
				   * sizeof (*allocated_fluids),
				   "allocated fluids");

      /* Copy over old values and initialize rest.  GC can not run
	 during these two operations since there is no safe point in
	 them.  */
      memcpy (new_allocated_fluids, allocated_fluids,
	      allocated_fluids_len * sizeof (*allocated_fluids));
      memset (new_allocated_fluids + allocated_fluids_len, 0,
	      FLUID_GROW * sizeof (*allocated_fluids));
      n = allocated_fluids_len;

      /* Update the vector of allocated fluids.  Dynamic states will
	 eventually be lazily grown to accomodate the new value of
	 ALLOCATED_FLUIDS_LEN in `fluid-ref' and `fluid-set!'.  */
      allocated_fluids = new_allocated_fluids;
      allocated_fluids_len += FLUID_GROW;
    }

  allocated_fluids[n] = SCM2PTR (fluid);
  SCM_SET_CELL_WORD_1 (fluid, (scm_t_bits) n);

  GC_GENERAL_REGISTER_DISAPPEARING_LINK (&allocated_fluids[n],
					 SCM2PTR (fluid));

  scm_dynwind_end ();
  return fluid;
}

SCM_DEFINE (scm_make_fluid, "make-fluid", 0, 0, 0, 
	    (),
	    "Return a newly created fluid.\n"
	    "Fluids are objects that can hold one\n"
	    "value per dynamic state.  That is, modifications to this value are\n"
	    "only visible to code that executes with the same dynamic state as\n"
	    "the modifying code.  When a new dynamic state is constructed, it\n"
	    "inherits the values from its parent.  Because each thread normally executes\n"
	    "with its own dynamic state, you can use fluids for thread local storage.")
#define FUNC_NAME s_scm_make_fluid
{
  return new_fluid ();
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_unbound_fluid, "make-unbound-fluid", 0, 0, 0,
            (),
            "Make a fluid that is initially unbound.")
#define FUNC_NAME s_scm_make_unbound_fluid
{
  SCM f = new_fluid ();
  scm_fluid_set_x (f, SCM_UNDEFINED);
  return f;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_p, "fluid?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} iff @var{obj} is a fluid; otherwise, return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_fluid_p
{
  return scm_from_bool (IS_FLUID (obj));
}
#undef FUNC_NAME

int
scm_is_fluid (SCM obj)
{
  return IS_FLUID (obj);
}

/* Does not check type of `fluid'! */
static SCM
fluid_ref (SCM fluid)
{
  SCM fluids = DYNAMIC_STATE_FLUIDS (SCM_I_CURRENT_THREAD->dynamic_state);

  if (SCM_UNLIKELY (FLUID_NUM (fluid) >= SCM_SIMPLE_VECTOR_LENGTH (fluids)))
    {
      /* Lazily grow the current thread's dynamic state.  */
      grow_dynamic_state (SCM_I_CURRENT_THREAD->dynamic_state);

      fluids = DYNAMIC_STATE_FLUIDS (SCM_I_CURRENT_THREAD->dynamic_state);
    }

  return SCM_SIMPLE_VECTOR_REF (fluids, FLUID_NUM (fluid));
}

SCM_DEFINE (scm_fluid_ref, "fluid-ref", 1, 0, 0, 
	    (SCM fluid),
	    "Return the value associated with @var{fluid} in the current\n"
	    "dynamic root.  If @var{fluid} has not been set, then return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_fluid_ref
{
  SCM val;
  SCM_VALIDATE_FLUID (1, fluid);
  val = fluid_ref (fluid);
  if (SCM_UNBNDP (val))
    SCM_MISC_ERROR ("unbound fluid: ~S",
                    scm_list_1 (fluid));
  return val;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_set_x, "fluid-set!", 2, 0, 0,
	    (SCM fluid, SCM value),
	    "Set the value associated with @var{fluid} in the current dynamic root.")
#define FUNC_NAME s_scm_fluid_set_x
{
  SCM fluids = DYNAMIC_STATE_FLUIDS (SCM_I_CURRENT_THREAD->dynamic_state);

  SCM_VALIDATE_FLUID (1, fluid);

  if (SCM_UNLIKELY (FLUID_NUM (fluid) >= SCM_SIMPLE_VECTOR_LENGTH (fluids)))
    {
      /* Lazily grow the current thread's dynamic state.  */
      grow_dynamic_state (SCM_I_CURRENT_THREAD->dynamic_state);

      fluids = DYNAMIC_STATE_FLUIDS (SCM_I_CURRENT_THREAD->dynamic_state);
    }

  SCM_SIMPLE_VECTOR_SET (fluids, FLUID_NUM (fluid), value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_unset_x, "fluid-unset!", 1, 0, 0,
            (SCM fluid),
            "Unset the value associated with @var{fluid}.")
#define FUNC_NAME s_scm_fluid_unset_x
{
  return scm_fluid_set_x (fluid, SCM_UNDEFINED);
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_bound_p, "fluid-bound?", 1, 0, 0,
	    (SCM fluid),
	    "Return @code{#t} iff @var{fluid} is bound to a value.\n"
	    "Throw an error if @var{fluid} is not a fluid.")
#define FUNC_NAME s_scm_fluid_bound_p
{
  SCM val;
  SCM_VALIDATE_FLUID (1, fluid);
  val = fluid_ref (fluid);
  return scm_from_bool (! (SCM_UNBNDP (val)));
}
#undef FUNC_NAME

static SCM
apply_thunk (void *thunk)
{
  return scm_call_0 (SCM_PACK (thunk));
}

SCM
scm_i_make_with_fluids (size_t n, SCM *fluids, SCM *vals)
{
  SCM ret;

  /* Ensure that there are no duplicates in the fluids set -- an N^2 operation,
     but N will usually be small, so perhaps that's OK. */
  {
    size_t i, j = n;

    while (j--)
      for (i = 0; i < j; i++)
        if (fluids[i] == fluids[j])
          {
            vals[i] = vals[j]; /* later bindings win */
            n--;
            break;
          }
  }
        
  ret = scm_words (scm_tc7_with_fluids | (n << 8), 1 + n*2);
  SCM_SET_CELL_WORD_1 (ret, n);

  while (n--)
    {
      if (SCM_UNLIKELY (!IS_FLUID (fluids[n])))
        scm_wrong_type_arg ("with-fluids", 0, fluids[n]);
      SCM_SET_CELL_OBJECT (ret, 1 + n * 2, fluids[n]);
      SCM_SET_CELL_OBJECT (ret, 2 + n * 2, vals[n]);
    }

  return ret;
}
  
void
scm_i_swap_with_fluids (SCM wf, SCM dynstate)
{
  SCM fluids;
  size_t i, max = 0;

  fluids = DYNAMIC_STATE_FLUIDS (dynstate);

  /* We could cache the max in the with-fluids, but that would take more mem,
     and we're touching all the fluids anyway, so this per-swap traversal should
     be OK. */
  for (i = 0; i < SCM_WITH_FLUIDS_LEN (wf); i++)
    {
      size_t num = FLUID_NUM (SCM_WITH_FLUIDS_NTH_FLUID (wf, i));
      max = (max > num) ? max : num;
    }

  if (SCM_UNLIKELY (max >= SCM_SIMPLE_VECTOR_LENGTH (fluids)))
    {
      /* Lazily grow the current thread's dynamic state.  */
      grow_dynamic_state (dynstate);

      fluids = DYNAMIC_STATE_FLUIDS (dynstate);
    }

  /* Bind the fluids. Order doesn't matter, as all fluids are distinct. */
  for (i = 0; i < SCM_WITH_FLUIDS_LEN (wf); i++)
    {
      size_t fluid_num;
      SCM x;
      
      fluid_num = FLUID_NUM (SCM_WITH_FLUIDS_NTH_FLUID (wf, i));
      x = SCM_SIMPLE_VECTOR_REF (fluids, fluid_num);
      SCM_SIMPLE_VECTOR_SET (fluids, fluid_num,
                             SCM_WITH_FLUIDS_NTH_VAL (wf, i));
      SCM_WITH_FLUIDS_SET_NTH_VAL (wf, i, x);
    }
}
  
SCM_DEFINE (scm_with_fluids, "with-fluids*", 3, 0, 0, 
	    (SCM fluids, SCM values, SCM thunk),
	    "Set @var{fluids} to @var{values} temporary, and call @var{thunk}.\n"
	    "@var{fluids} must be a list of fluids and @var{values} must be the same\n"
	    "number of their values to be applied.  Each substitution is done\n"
	    "one after another.  @var{thunk} must be a procedure with no argument.")
#define FUNC_NAME s_scm_with_fluids
{
  return scm_c_with_fluids (fluids, values,
			    apply_thunk, (void *) SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM
scm_c_with_fluids (SCM fluids, SCM values, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluids"
{
  SCM wf, ans;
  long flen, vlen, i;
  SCM *fluidsv, *valuesv;

  SCM_VALIDATE_LIST_COPYLEN (1, fluids, flen);
  SCM_VALIDATE_LIST_COPYLEN (2, values, vlen);
  if (flen != vlen)
    scm_out_of_range (s_scm_with_fluids, values);

  if (SCM_UNLIKELY (flen == 0))
    return cproc (cdata);

  fluidsv = alloca (sizeof(SCM)*flen);
  valuesv = alloca (sizeof(SCM)*flen);
  
  for (i = 0; i < flen; i++)
    {
      fluidsv[i] = SCM_CAR (fluids);
      fluids = SCM_CDR (fluids);
      valuesv[i] = SCM_CAR (values);
      values = SCM_CDR (values);
    }

  wf = scm_i_make_with_fluids (flen, fluidsv, valuesv);
  scm_i_swap_with_fluids (wf, SCM_I_CURRENT_THREAD->dynamic_state);
  scm_i_set_dynwinds (scm_cons (wf, scm_i_dynwinds ()));
  ans = cproc (cdata);
  scm_i_swap_with_fluids (wf, SCM_I_CURRENT_THREAD->dynamic_state);
  scm_i_set_dynwinds (scm_cdr (scm_i_dynwinds ()));

  return ans;
}
#undef FUNC_NAME

SCM_DEFINE (scm_with_fluid, "with-fluid*", 3, 0, 0, 
	    (SCM fluid, SCM value, SCM thunk),
	    "Set @var{fluid} to @var{value} temporarily, and call @var{thunk}.\n"
	    "@var{thunk} must be a procedure with no argument.")
#define FUNC_NAME s_scm_with_fluid
{
  return scm_c_with_fluid (fluid, value,
			   apply_thunk, (void *) SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM
scm_c_with_fluid (SCM fluid, SCM value, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluid"
{
  SCM ans, wf;

  wf = scm_i_make_with_fluids (1, &fluid, &value);
  scm_i_swap_with_fluids (wf, SCM_I_CURRENT_THREAD->dynamic_state);
  scm_i_set_dynwinds (scm_cons (wf, scm_i_dynwinds ()));
  ans = cproc (cdata);
  scm_i_swap_with_fluids (wf, SCM_I_CURRENT_THREAD->dynamic_state);
  scm_i_set_dynwinds (scm_cdr (scm_i_dynwinds ()));

  return ans;
}
#undef FUNC_NAME

static void
swap_fluid (SCM data)
{
  SCM f = SCM_CAR (data);
  SCM t = fluid_ref (f);
  scm_fluid_set_x (f, SCM_CDR (data));
  SCM_SETCDR (data, t);
}

void
scm_dynwind_fluid (SCM fluid, SCM value)
{
  SCM data = scm_cons (fluid, value);
  scm_dynwind_rewind_handler_with_scm (swap_fluid, data, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler_with_scm (swap_fluid, data, SCM_F_WIND_EXPLICITLY);
}

SCM
scm_i_make_initial_dynamic_state ()
{
  SCM fluids = scm_c_make_vector (allocated_fluids_len, SCM_BOOL_F);
  return scm_cell (scm_tc7_dynamic_state, SCM_UNPACK (fluids));
}

SCM_DEFINE (scm_make_dynamic_state, "make-dynamic-state", 0, 1, 0,
	    (SCM parent),
	    "Return a copy of the dynamic state object @var{parent}\n"
	    "or of the current dynamic state when @var{parent} is omitted.")
#define FUNC_NAME s_scm_make_dynamic_state
{
  SCM fluids;

  if (SCM_UNBNDP (parent))
    parent = scm_current_dynamic_state ();

  SCM_ASSERT (IS_DYNAMIC_STATE (parent), parent, SCM_ARG1, FUNC_NAME);
  fluids = scm_vector_copy (DYNAMIC_STATE_FLUIDS (parent));
  return scm_cell (scm_tc7_dynamic_state, SCM_UNPACK (fluids));
}
#undef FUNC_NAME

SCM_DEFINE (scm_dynamic_state_p, "dynamic-state?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a dynamic state object;\n"
	    "return @code{#f} otherwise")
#define FUNC_NAME s_scm_dynamic_state_p
{
  return scm_from_bool (IS_DYNAMIC_STATE (obj));
}
#undef FUNC_NAME

int
scm_is_dynamic_state (SCM obj)
{
  return IS_DYNAMIC_STATE (obj);
}

SCM_DEFINE (scm_current_dynamic_state, "current-dynamic-state", 0, 0, 0,
	    (),
	    "Return the current dynamic state object.")
#define FUNC_NAME s_scm_current_dynamic_state
{
  return SCM_I_CURRENT_THREAD->dynamic_state;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_current_dynamic_state, "set-current-dynamic-state", 1,0,0,
	    (SCM state),
	    "Set the current dynamic state object to @var{state}\n"
	    "and return the previous current dynamic state object.")
#define FUNC_NAME s_scm_set_current_dynamic_state
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  SCM old = t->dynamic_state;
  SCM_ASSERT (IS_DYNAMIC_STATE (state), state, SCM_ARG1, FUNC_NAME);
  t->dynamic_state = state;
  return old;
}
#undef FUNC_NAME

static void
swap_dynamic_state (SCM loc)
{
  SCM_SETCAR (loc, scm_set_current_dynamic_state (SCM_CAR (loc)));
}

void
scm_dynwind_current_dynamic_state (SCM state)
{
  SCM loc = scm_cons (state, SCM_EOL);
  SCM_ASSERT (IS_DYNAMIC_STATE (state), state, SCM_ARG1, NULL);
  scm_dynwind_rewind_handler_with_scm (swap_dynamic_state, loc,
				     SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler_with_scm (swap_dynamic_state, loc,
				     SCM_F_WIND_EXPLICITLY);
}

void *
scm_c_with_dynamic_state (SCM state, void *(*func)(void *), void *data)
{
  void *result;
  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_current_dynamic_state (state);
  result = func (data);
  scm_dynwind_end ();
  return result;
}

SCM_DEFINE (scm_with_dynamic_state, "with-dynamic-state", 2, 0, 0,
	    (SCM state, SCM proc),
	    "Call @var{proc} while @var{state} is the current dynamic\n"
	    "state object.")
#define FUNC_NAME s_scm_with_dynamic_state
{
  SCM result;
  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_current_dynamic_state (state);
  result = scm_call_0 (proc);
  scm_dynwind_end ();
  return result;
}
#undef FUNC_NAME


void
scm_init_fluids ()
{
#include "libguile/fluids.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

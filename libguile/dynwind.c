/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2003, 2004, 2006, 2008, 2010, 2011 Free Software Foundation, Inc.
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

#include <assert.h>

#include "libguile/_scm.h"
#include "libguile/control.h"
#include "libguile/eval.h"
#include "libguile/alist.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"
#include "libguile/smob.h"

#include "libguile/dynwind.h"


/* {Dynamic wind}
 
   Things that can be on the wind list:

   #<frame>
   #<winder>
   #<with-fluids>
   #<prompt>
   (enter-proc . leave-proc)     dynamic-wind

*/



SCM
scm_dynamic_wind (SCM in_guard, SCM thunk, SCM out_guard)
#define FUNC_NAME "dynamic-wind"
{
  SCM ans, old_winds;
  SCM_ASSERT (scm_is_true (scm_thunk_p (out_guard)),
	      out_guard,
	      SCM_ARG3, FUNC_NAME);
  scm_call_0 (in_guard);
  old_winds = scm_i_dynwinds ();
  scm_i_set_dynwinds (scm_acons (in_guard, out_guard, old_winds));
  ans = scm_call_0 (thunk);
  scm_i_set_dynwinds (old_winds);
  scm_call_0 (out_guard);
  return ans;
}
#undef FUNC_NAME

/* Frames and winders. */

static scm_t_bits tc16_frame;
#define FRAME_P(f)     SCM_SMOB_PREDICATE (tc16_frame, (f))

#define FRAME_F_REWINDABLE    (1 << 0)
#define FRAME_REWINDABLE_P(f) (SCM_SMOB_FLAGS(f) & FRAME_F_REWINDABLE)

static scm_t_bits tc16_winder;
#define WINDER_P(w)     SCM_SMOB_PREDICATE (tc16_winder, (w))
#define WINDER_PROC(w)  ((void (*)(void *))SCM_SMOB_DATA (w))
#define WINDER_DATA(w)  ((void *)SCM_SMOB_DATA_2 (w))

#define WINDER_F_EXPLICIT    (1 << 0)
#define WINDER_F_REWIND      (1 << 1)
#define WINDER_F_MARK        (1 << 2)
#define WINDER_EXPLICIT_P(w) (SCM_SMOB_FLAGS(w) & WINDER_F_EXPLICIT)
#define WINDER_REWIND_P(w)   (SCM_SMOB_FLAGS(w) & WINDER_F_REWIND)
#define WINDER_MARK_P(w)     (SCM_SMOB_FLAGS(w) & WINDER_F_MARK)

void
scm_dynwind_begin (scm_t_dynwind_flags flags)
{
  SCM f;
  SCM_NEWSMOB (f, tc16_frame, 0);
  if (flags & SCM_F_DYNWIND_REWINDABLE)
    SCM_SET_SMOB_FLAGS (f, FRAME_F_REWINDABLE);
  scm_i_set_dynwinds (scm_cons (f, scm_i_dynwinds ()));
}

void
scm_dynwind_end (void)
{
  SCM winds;

  /* Unwind upto and including the next frame entry.  We can only
     encounter #<winder> entries on the way.
   */

  winds = scm_i_dynwinds ();
  while (scm_is_pair (winds))
    {
      SCM entry = SCM_CAR (winds);
      winds = SCM_CDR (winds);

      scm_i_set_dynwinds (winds);

      if (FRAME_P (entry))
	return;

      assert (WINDER_P (entry));
      if (!WINDER_REWIND_P (entry) && WINDER_EXPLICIT_P (entry))
	WINDER_PROC(entry) (WINDER_DATA (entry));
    }

  assert (0);
}

void
scm_dynwind_unwind_handler (void (*proc) (void *), void *data,
			    scm_t_wind_flags flags)
{
  SCM w;
  SCM_NEWSMOB2 (w, tc16_winder,	(scm_t_bits) proc, (scm_t_bits) data);
  if (flags & SCM_F_WIND_EXPLICITLY)
    SCM_SET_SMOB_FLAGS (w, WINDER_F_EXPLICIT);
  scm_i_set_dynwinds (scm_cons (w, scm_i_dynwinds ()));
}

void
scm_dynwind_rewind_handler (void (*proc) (void *), void *data,
			    scm_t_wind_flags flags)
{
  SCM w;
  SCM_NEWSMOB2 (w, tc16_winder,	(scm_t_bits) proc, (scm_t_bits) data);
  SCM_SET_SMOB_FLAGS (w, WINDER_F_REWIND);
  scm_i_set_dynwinds (scm_cons (w, scm_i_dynwinds ()));
  if (flags & SCM_F_WIND_EXPLICITLY)
    proc (data);
}

void
scm_dynwind_unwind_handler_with_scm (void (*proc) (SCM), SCM data,
				     scm_t_wind_flags flags)
{
  SCM w;
  scm_t_bits fl = ((flags&SCM_F_WIND_EXPLICITLY)? WINDER_F_EXPLICIT : 0);
  SCM_NEWSMOB2 (w, tc16_winder,	(scm_t_bits) proc, SCM_UNPACK (data));
  SCM_SET_SMOB_FLAGS (w, fl | WINDER_F_MARK);
  scm_i_set_dynwinds (scm_cons (w, scm_i_dynwinds ()));
}

void
scm_dynwind_rewind_handler_with_scm (void (*proc) (SCM), SCM data,
				     scm_t_wind_flags flags)
{
  SCM w;
  SCM_NEWSMOB2 (w, tc16_winder, (scm_t_bits) proc, SCM_UNPACK (data));
  SCM_SET_SMOB_FLAGS (w, WINDER_F_REWIND | WINDER_F_MARK);
  scm_i_set_dynwinds (scm_cons (w, scm_i_dynwinds ()));
  if (flags & SCM_F_WIND_EXPLICITLY)
    proc (data);
}

void
scm_dynwind_free (void *mem)
{
  scm_dynwind_unwind_handler (free, mem, SCM_F_WIND_EXPLICITLY);
}

#ifdef GUILE_DEBUG
SCM_DEFINE (scm_wind_chain, "wind-chain", 0, 0, 0, 
            (),
	    "Return the current wind chain. The wind chain contains all\n"
	    "information required by @code{dynamic-wind} to call its\n"
	    "argument thunks when entering/exiting its scope.")
#define FUNC_NAME s_scm_wind_chain
{
  return scm_i_dynwinds ();
}
#undef FUNC_NAME
#endif

void
scm_swap_bindings (SCM vars, SCM vals)
{
  SCM tmp;
  while (SCM_NIMP (vals))
    {
      tmp = SCM_VARIABLE_REF (SCM_CAR (vars));
      SCM_VARIABLE_SET (SCM_CAR (vars), SCM_CAR (vals));
      SCM_SETCAR (vals, tmp);
      vars = SCM_CDR (vars);
      vals = SCM_CDR (vals);
    }
}

void
scm_dowinds (SCM to, long delta)
{
  scm_i_dowinds (to, delta, NULL, NULL);
}

void 
scm_i_dowinds (SCM to, long delta, void (*turn_func) (void *), void *data)
{
 tail:
  if (scm_is_eq (to, scm_i_dynwinds ()))
    {
      if (turn_func)
	turn_func (data);
    }
  else if (delta < 0)
    {
      SCM wind_elt;

      scm_i_dowinds (SCM_CDR (to), 1 + delta, turn_func, data);
      wind_elt = SCM_CAR (to);

      if (FRAME_P (wind_elt))
	{
	  if (!FRAME_REWINDABLE_P (wind_elt))
	    scm_misc_error ("dowinds", 
			    "cannot invoke continuation from this context",
			    SCM_EOL);
	}
      else if (WINDER_P (wind_elt))
	{
	  if (WINDER_REWIND_P (wind_elt))
	    WINDER_PROC (wind_elt) (WINDER_DATA (wind_elt));
	}
      else if (SCM_WITH_FLUIDS_P (wind_elt))
	{
          scm_i_swap_with_fluids (wind_elt,
                                  SCM_I_CURRENT_THREAD->dynamic_state);
	}
      else if (SCM_PROMPT_P (wind_elt))
        ; /* pass -- see vm_reinstate_partial_continuation */
      else if (scm_is_pair (wind_elt))
        scm_call_0 (SCM_CAR (wind_elt));
      else
        /* trash on the wind list */
        abort ();

      scm_i_set_dynwinds (to);
    }
  else
    {
      SCM wind;
      SCM wind_elt;

      wind = scm_i_dynwinds ();
      wind_elt = SCM_CAR (wind);
      scm_i_set_dynwinds (SCM_CDR (wind));

      if (FRAME_P (wind_elt))
	{
	  /* Nothing to do. */
	}
      else if (WINDER_P (wind_elt))
	{
	  if (!WINDER_REWIND_P (wind_elt))
	    WINDER_PROC (wind_elt) (WINDER_DATA (wind_elt));
	}
      else if (SCM_WITH_FLUIDS_P (wind_elt))
	{
          scm_i_swap_with_fluids (wind_elt,
                                  SCM_I_CURRENT_THREAD->dynamic_state);
	}
      else if (SCM_PROMPT_P (wind_elt))
        ; /* pass -- though we could invalidate the prompt */
      else if (scm_is_pair (wind_elt))
        scm_call_0 (SCM_CDR (wind_elt));
      else
        /* trash on the wind list */
        abort ();

      delta--;
      goto tail;		/* scm_dowinds(to, delta-1); */
    }
}

void
scm_init_dynwind ()
{
  tc16_frame = scm_make_smob_type ("frame", 0);

  tc16_winder = scm_make_smob_type ("winder", 0);

#include "libguile/dynwind.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2003, 2004 Free Software Foundation, Inc.
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




#include <assert.h>

#include "libguile/_scm.h"
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
   (enter-proc . leave-proc)     dynamic-wind
   (tag . jmpbuf)                catch
   (tag . lazy-catch)            lazy-catch
     tag is either a symbol or a boolean

   ((fluid ...) . (value ...))   with-fluids

*/



SCM_DEFINE (scm_dynamic_wind, "dynamic-wind", 3, 0, 0,
           (SCM in_guard, SCM thunk, SCM out_guard),
	    "All three arguments must be 0-argument procedures.\n"
	    "@var{in_guard} is called, then @var{thunk}, then\n"
	    "@var{out_guard}.\n"
	    "\n"
	    "If, any time during the execution of @var{thunk}, the\n"
	    "continuation of the @code{dynamic_wind} expression is escaped\n"
	    "non-locally, @var{out_guard} is called.  If the continuation of\n"
	    "the dynamic-wind is re-entered, @var{in_guard} is called.  Thus\n"
	    "@var{in_guard} and @var{out_guard} may be called any number of\n"
	    "times.\n"
	    "@lisp\n"
	    "(define x 'normal-binding)\n"
	    "@result{} x\n"
	    "(define a-cont  (call-with-current-continuation\n"
	    "		  (lambda (escape)\n"
	    "		     (let ((old-x x))\n"
	    "		       (dynamic-wind\n"
	    "			  ;; in-guard:\n"
	    "			  ;;\n"
	    "			  (lambda () (set! x 'special-binding))\n"
	    "\n"
	    "			  ;; thunk\n"
	    "			  ;;\n"
	    "		 	  (lambda () (display x) (newline)\n"
	    "				     (call-with-current-continuation escape)\n"
	    "				     (display x) (newline)\n"
	    "				     x)\n"
	    "\n"
	    "			  ;; out-guard:\n"
	    "			  ;;\n"
	    "			  (lambda () (set! x old-x)))))))\n"
	    "\n"
	    ";; Prints:\n"
	    "special-binding\n"
	    ";; Evaluates to:\n"
	    "@result{} a-cont\n"
	    "x\n"
	    "@result{} normal-binding\n"
	    "(a-cont #f)\n"
	    ";; Prints:\n"
	    "special-binding\n"
	    ";; Evaluates to:\n"
	    "@result{} a-cont  ;; the value of the (define a-cont...)\n"
	    "x\n"
	    "@result{} normal-binding\n"
	    "a-cont\n"
	    "@result{} special-binding\n"
	    "@end lisp")
#define FUNC_NAME s_scm_dynamic_wind
{
  SCM ans;
  SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (out_guard)),
	      out_guard,
	      SCM_ARG3, FUNC_NAME);
  scm_call_0 (in_guard);
  scm_dynwinds = scm_acons (in_guard, out_guard, scm_dynwinds);
  ans = scm_call_0 (thunk);
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  scm_call_0 (out_guard);
  return ans;
}
#undef FUNC_NAME

SCM
scm_internal_dynamic_wind (scm_t_guard before,
			   scm_t_inner inner,
			   scm_t_guard after,
			   void *inner_data,
			   void *guard_data)
{
  SCM ans;

  scm_begin_frame (SCM_F_FRAME_REWINDABLE);
  scm_on_rewind (before, guard_data, SCM_F_WIND_EXPLICITELY);
  scm_on_unwind (after, guard_data, SCM_F_WIND_EXPLICITELY);
  ans = inner (inner_data);
  scm_end_frame ();
  return ans;
}

/* Frames and winders. */

static scm_t_bits tc16_frame;
#define FRAME_P(f)     SCM_SMOB_PREDICATE (tc16_frame, (f))

#define FRAME_F_REWINDABLE    (1 << 16)
#define FRAME_REWINDABLE_P(f) (SCM_CELL_WORD_0(f) & FRAME_F_REWINDABLE)

static scm_t_bits tc16_winder;
#define WINDER_P(w)    SCM_SMOB_PREDICATE (tc16_winder, (w))
#define WINDER_PROC(w) ((void (*)(void *))SCM_CELL_WORD_1 (w))
#define WINDER_DATA(w) ((void *)SCM_CELL_WORD_2 (w))

#define WINDER_F_EXPLICIT    (1 << 16)
#define WINDER_F_REWIND      (1 << 17)
#define WINDER_EXPLICIT_P(w) (SCM_CELL_WORD_0(w) & WINDER_F_EXPLICIT)
#define WINDER_REWIND_P(w)   (SCM_CELL_WORD_0(w) & WINDER_F_REWIND)

static int
frame_print (SCM obj, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<frame>", port);
  return 1;
}

void
scm_begin_frame (scm_t_frame_flags flags)
{
  SCM f;
  scm_t_bits fl = ((flags&SCM_F_FRAME_REWINDABLE)? FRAME_F_REWINDABLE : 0);
  SCM_NEWSMOB (f, tc16_frame | fl, 0);
  scm_dynwinds = scm_cons (f, scm_dynwinds);
}

void
scm_end_frame (void)
{
  long delta;
  SCM to;
  
  /* Unwind upto and including the next frame entry.
   */

  for (to = scm_dynwinds, delta = 1;
       SCM_CONSP (to);
       to = SCM_CDR (to), delta++)
    {
      if (FRAME_P (SCM_CAR (to)))
	{
	  scm_i_dowinds (SCM_CDR (to), delta, 1, NULL, NULL);
	  return;
	}
    }

  assert (0);
}

void
scm_on_unwind (void (*proc) (void *), void *data,
	       scm_t_wind_flags flags)
{
  SCM w;
  scm_t_bits fl = ((flags&SCM_F_WIND_EXPLICITELY)? WINDER_F_EXPLICIT : 0);
  SCM_NEWSMOB2 (w, tc16_winder | fl,
		(scm_t_bits) proc, (scm_t_bits) data);
  scm_dynwinds = scm_cons (w, scm_dynwinds);
}

void
scm_on_rewind (void (*proc) (void *), void *data,
	       scm_t_wind_flags flags)
{
  SCM w;
  SCM_NEWSMOB2 (w, tc16_winder | WINDER_F_REWIND,
		(scm_t_bits) proc, (scm_t_bits) data);
  scm_dynwinds = scm_cons (w, scm_dynwinds);
  if (flags & SCM_F_WIND_EXPLICITELY)
    proc (data);
}

#ifdef GUILE_DEBUG
SCM_DEFINE (scm_wind_chain, "wind-chain", 0, 0, 0, 
            (),
	    "Return the current wind chain. The wind chain contains all\n"
	    "information required by @code{dynamic-wind} to call its\n"
	    "argument thunks when entering/exiting its scope.")
#define FUNC_NAME s_scm_wind_chain
{
  return scm_dynwinds;
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
  scm_i_dowinds (to, delta, 0, NULL, NULL);
}

void 
scm_i_dowinds (SCM to, long delta, int explicit,
	       void (*turn_func) (void *), void *data)
{
 tail:
  if (SCM_EQ_P (to, scm_dynwinds))
    {
      if (turn_func)
	turn_func (data);
    }
  else if (delta < 0)
    {
      SCM wind_elt;
      SCM wind_key;

      scm_i_dowinds (SCM_CDR (to), 1 + delta, explicit,
		     turn_func, data);
      wind_elt = SCM_CAR (to);

#if 0
      if (SCM_INUMP (wind_elt))
	{
	  scm_cross_dynwind_binding_scope (wind_elt, 0);
	}
      else
#endif
	{
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
		{
		  void (*proc) (void *) = WINDER_PROC (wind_elt);
		  void *data = WINDER_DATA (wind_elt);
		  proc (data);
		}
	    }
	  else
	    {
	      wind_key = SCM_CAR (wind_elt);
	      /* key = #t | symbol | thunk | list of variables | list of fluids */
	      if (SCM_NIMP (wind_key))
		{
		  if (SCM_CONSP (wind_key))
		    {
		      if (SCM_VARIABLEP (SCM_CAR (wind_key)))
			scm_swap_bindings (wind_key, SCM_CDR (wind_elt));
		      else if (SCM_FLUIDP (SCM_CAR (wind_key)))
			scm_swap_fluids (wind_key, SCM_CDR (wind_elt));
		    }
		  else if (SCM_TYP3 (wind_key) == scm_tc3_closure)
		    scm_call_0 (wind_key);
		}
	    }
	}
      scm_dynwinds = to;
    }
  else
    {
      SCM wind_elt;
      SCM wind_key;

      wind_elt = SCM_CAR (scm_dynwinds);
      scm_dynwinds = SCM_CDR (scm_dynwinds);

#if 0
      if (SCM_INUMP (wind_elt))
	{
	  scm_cross_dynwind_binding_scope (wind_elt, 0);
	}
      else
#endif
	{
	  if (FRAME_P (wind_elt))
	    {
	      /* Nothing to do. */
	    }
	  else if (WINDER_P (wind_elt))
	    {
	      if (!WINDER_REWIND_P (wind_elt)
		  && (!explicit || WINDER_EXPLICIT_P (wind_elt)))
		{
		  void (*proc) (void *) = WINDER_PROC (wind_elt);
		  void *data = WINDER_DATA (wind_elt);
		  proc (data);
		}
	    }
	  else
	    {
	      wind_key = SCM_CAR (wind_elt);
	      if (SCM_NIMP (wind_key))
		{
		  if (SCM_CONSP (wind_key))
		    {
		      if (SCM_VARIABLEP (SCM_CAR (wind_key)))
			scm_swap_bindings (wind_key, SCM_CDR (wind_elt));
		      else if (SCM_FLUIDP (SCM_CAR (wind_key)))
			scm_swap_fluids_reverse (wind_key, SCM_CDR (wind_elt));
		    }
		  else if (SCM_TYP3 (wind_key) == scm_tc3_closure)
		    scm_call_0 (SCM_CDR (wind_elt));
		}
	    }
	}
      delta--;
      goto tail;		/* scm_dowinds(to, delta-1); */
    }
}

void
scm_init_dynwind ()
{
  tc16_frame = scm_make_smob_type ("frame", 0);
  scm_set_smob_print (tc16_frame, frame_print);

  tc16_winder = scm_make_smob_type ("winder", 0);

#include "libguile/dynwind.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

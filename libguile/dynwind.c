/*	Copyright (C) 1995, 1996, 1998, 1999 Free Software Foundation, Inc.
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
#include "_scm.h"
#include "eval.h"
#include "alist.h"
#include "fluids.h"
#include "genio.h"
#include "smob.h"

#include "dynwind.h"


/* {Dynamic wind}
 
   Things that can be on the wind list:

   (enter-proc . leave-proc)     dynamic-wind
   (tag . jmpbuf)                catch
   (tag . lazy-catch)            lazy-catch
     tag is either a symbol or a boolean

   ((fluid ...) . (value ...))   with-fluids

*/



GUILE_PROC(scm_dynamic_wind, "dynamic-wind", 3, 0, 0,
           (SCM thunk1, SCM thunk2, SCM thunk3),
"")
#define FUNC_NAME s_scm_dynamic_wind
{
  SCM ans;
  SCM_ASSERT (SCM_NFALSEP (scm_thunk_p (thunk3)),
	      thunk3,
	      SCM_ARG3, FUNC_NAME);
  scm_apply (thunk1, SCM_EOL, SCM_EOL);
  scm_dynwinds = scm_acons (thunk1, thunk3, scm_dynwinds);
  ans = scm_apply (thunk2, SCM_EOL, SCM_EOL);
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  scm_apply (thunk3, SCM_EOL, SCM_EOL);
  return ans;
}
#undef FUNC_NAME

/* The implementation of a C-callable dynamic-wind,
 * scm_internal_dynamic_wind, requires packaging of C pointers in a
 * smob.  Objects of this type are pushed onto the dynwind chain.
 */

typedef struct guardsmem {
  scm_guard_t before;
  scm_guard_t after;
  void *data;
} guardsmem;

#define SCM_GUARDSMEM(obj) ((guardsmem *) SCM_CDR (obj))
#define SCM_BEFORE_GUARD(obj) (SCM_GUARDSMEM (obj)->before)
#define SCM_AFTER_GUARD(obj) (SCM_GUARDSMEM (obj)->after)
#define SCM_GUARD_DATA(obj) (SCM_GUARDSMEM (obj)->data)
#define SCM_GUARDSP(obj) (SCM_CAR (obj) == tc16_guards)

static long tc16_guards;

static scm_sizet
freeguards (SCM guards)
{
  scm_must_free ((char *) SCM_CDR (guards));
  return sizeof (guardsmem);
}

static int
printguards (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<guards ", port);
  scm_intprint (SCM_CDR (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}

SCM
scm_internal_dynamic_wind (scm_guard_t before,
			   scm_inner_t inner,
			   scm_guard_t after,
			   void *inner_data,
			   void *guard_data)
{
  SCM guards, ans;
  guardsmem *g;
  before (guard_data);
  g = (guardsmem *) scm_must_malloc (sizeof (*g), "guards");
  g->before = before;
  g->after = after;
  g->data = guard_data;
  SCM_NEWSMOB (guards, tc16_guards, g);
  scm_dynwinds = scm_acons (guards, SCM_BOOL_F, scm_dynwinds);
  ans = inner (inner_data);
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  after (guard_data);
  return ans;
}

#ifdef GUILE_DEBUG
GUILE_PROC (scm_wind_chain, "wind-chain", 0, 0, 0, 
            (),
"")
#define FUNC_NAME s_scm_wind_chain
{
  return scm_dynwinds;
}
#undef FUNC_NAME
#endif

static void
scm_swap_bindings (SCM glocs, SCM vals)
{
  SCM tmp;
  while (SCM_NIMP (vals))
    {
      tmp = SCM_GLOC_VAL (SCM_CAR (glocs));
      SCM_SETCDR (SCM_CAR (glocs) - 1L, SCM_CAR (vals));
      SCM_SETCAR (vals, tmp);
      glocs = SCM_CDR (glocs);
      vals = SCM_CDR (vals);
    }
}

void 
scm_dowinds (to, delta)
     SCM to;
     long delta;
{
 tail:
  if (scm_dynwinds == to);
  else if (0 > delta)
    {
      SCM wind_elt;
      SCM wind_key;

      scm_dowinds (SCM_CDR (to), 1 + delta);
      wind_elt = SCM_CAR (to);
#if 0
      if (SCM_INUMP (wind_elt))
	{
	  scm_cross_dynwind_binding_scope (wind_elt, 0);
	}
      else
#endif
	{
	  wind_key = SCM_CAR (wind_elt);
	  /* key = #t | symbol | thunk | list of glocs | list of fluids */
	  if (SCM_NIMP (wind_key))
	    {
	      if (SCM_TYP3 (wind_key) == scm_tc3_cons_gloc)
		scm_swap_bindings (wind_key, SCM_CDR (wind_elt));
	      else if (SCM_TYP3 (wind_key) == scm_tc3_cons)
		scm_swap_fluids (wind_key, SCM_CDR (wind_elt));
	      else if (SCM_GUARDSP (wind_key))
		SCM_BEFORE_GUARD (wind_key) (SCM_GUARD_DATA (wind_key));
	      else if (SCM_TYP3 (wind_key) == scm_tc3_closure)
		scm_apply (wind_key, SCM_EOL, SCM_EOL);
	    }
	}
      scm_dynwinds = to;
    }
  else
    {
      SCM from;
      SCM wind_elt;
      SCM wind_key;

      from = SCM_CDR (SCM_CAR (scm_dynwinds));
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
	  wind_key = SCM_CAR (wind_elt);
	  if (SCM_NIMP (wind_key))
	    {
	      if (SCM_TYP3 (wind_key) == scm_tc3_cons_gloc)
		scm_swap_bindings (wind_key, from);
	      else if (SCM_TYP3 (wind_key) == scm_tc3_cons)
		scm_swap_fluids_reverse (wind_key, from);
	      else if (SCM_GUARDSP (wind_key))
		SCM_AFTER_GUARD (wind_key) (SCM_GUARD_DATA (wind_key));
	      else if (SCM_TYP3 (wind_key) == scm_tc3_closure)
		scm_apply (from, SCM_EOL, SCM_EOL);
	    }
	}
      delta--;
      goto tail;		/* scm_dowinds(to, delta-1); */
    }
}



void
scm_init_dynwind ()
{
  tc16_guards = scm_make_smob_type_mfpe ("guards", sizeof (struct guardsmem),
                                        NULL, freeguards, printguards, NULL);
#include "dynwind.x"
}

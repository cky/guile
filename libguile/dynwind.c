/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
#include "eval.h"
#include "alist.h"

#include "dynwind.h"


/* {Dynamic wind}
 */



SCM_PROC(s_dynamic_wind, "dynamic-wind", 3, 0, 0, scm_dynamic_wind);

SCM 
scm_dynamic_wind (thunk1, thunk2, thunk3)
     SCM thunk1;
     SCM thunk2;
     SCM thunk3;
{
  SCM ans;
  scm_apply (thunk1, SCM_EOL, SCM_EOL);
  scm_dynwinds = scm_acons (thunk1, thunk3, scm_dynwinds);
  ans = scm_apply (thunk2, SCM_EOL, SCM_EOL);
  scm_dynwinds = SCM_CDR (scm_dynwinds);
  scm_apply (thunk3, SCM_EOL, SCM_EOL);
  return ans;
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
	  if (   !(SCM_NIMP (wind_key) && SCM_SYMBOLP (wind_key))
	      && (wind_key != SCM_BOOL_F)
	      && (wind_key != SCM_BOOL_T))
	    scm_apply (wind_key, SCM_EOL, SCM_EOL);
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
	  if (   !(SCM_NIMP (wind_key) && SCM_SYMBOLP (wind_key))
	      && (wind_key != SCM_BOOL_F)
	      && (wind_key != SCM_BOOL_T))
	    scm_apply (from, SCM_EOL, SCM_EOL);
	}
      delta--;
      goto tail;		/* scm_dowinds(to, delta-1); */
    }
}



void
scm_init_dynwind ()
{
#include "dynwind.x"
}


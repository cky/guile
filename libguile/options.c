/*	Copyright (C) 1995,1996 Mikael Djurfeldt
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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
 * If you do not wish that, delete this exception notice.
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN
 */


#include <stdio.h>
#include "_scm.h"



/* {Run-time options}
 *
 * This is the basic interface for low-level configuration of the
 * Guile library.  It is used for configuring the reader, evaluator,
 * printer and debugger.
 */

#ifdef __STDC__
SCM
scm_change_options (SCM new_mode, scm_option options[], int n, char *s)
#else
SCM
scm_change_options (new_mode, options, n, s)
     SCM new_mode;
     scm_option options[];
     int n;
     char *s;
#endif
{
  int i;
  SCM old = SCM_EOL;
  for (i = 0; i < n; ++i)
    switch (options[i].type)
      {
      case SCM_OPTION_BOOLEAN:
	if (options[i].val)
	  old = scm_cons (options[i].sym, old);
	break;
      case SCM_OPTION_INTEGER:
	old = scm_cons2 (options[i].sym,
			 SCM_MAKINUM (options[i].val),
			 old);
      }
  if (!SCM_UNBNDP (new_mode))
    {
      int *flags;
      flags = (int *) scm_must_malloc (n * sizeof (int), "mode buffer");
      for (i = 0; i < n; ++i)
	if (options[i].type == SCM_OPTION_BOOLEAN)
	  flags[i] = 0;
	else
	  flags[i] = options[i].val;
      while (SCM_NNULLP (new_mode))
	{
	  SCM_ASSERT (SCM_NIMP (new_mode) && SCM_CONSP (new_mode),
		      new_mode,
		      SCM_ARG1,
		      s);
	  for (i = 0; i < n; ++i)
	    if (SCM_CAR (new_mode) == options[i].sym)
	      switch (options[i].type)
		{
		case SCM_OPTION_BOOLEAN:
		  flags[i] = 1;
		  goto cont;
		case SCM_OPTION_INTEGER:
		  new_mode = SCM_CDR (new_mode);
		  SCM_ASSERT (SCM_NIMP (new_mode)
			      && SCM_CONSP (new_mode)
			      && SCM_INUMP (SCM_CAR (new_mode)),
			      new_mode,
			      SCM_ARG1,
			      s);
		  flags[i] = SCM_INUM (SCM_CAR (new_mode));
		  goto cont;
		}
#ifndef RECKLESS
	  scm_must_free ((char *) flags);
	  scm_wta (SCM_CAR (new_mode), "Unknown mode flag", s);
#endif
	cont:
	  new_mode = SCM_CDR (new_mode);
	}
      for (i = 0; i < n; ++i) options[i].val = flags[i];
      scm_must_free ((char *) flags);
    }
  return old;
}

#ifdef __STDC__
void
scm_init_opts (SCM (*func) (SCM), scm_option options[], int n)
#else
void
scm_init_opts (func, options, n)
     SCM (*func) (SCM);
     scm_option options[];
     int n;
#endif
{
  int i;

  for (i = 0; i < n; ++i)
    options[i].sym = SCM_CAR (scm_sysintern (options[i].name, SCM_UNDEFINED));
  func (SCM_UNDEFINED);
}

#ifdef __STDC__
void
scm_init_options (void)
#else
void
scm_init_options ()
#endif
{
#include "options.x"
}

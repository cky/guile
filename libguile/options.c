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

#include "options.h"


/* {Run-time options}
 *
 * This is the basic interface for low-level configuration of the
 * Guile library.  It is used for configuring the reader, evaluator,
 * printer and debugger.
 *
 * Motivation:
 *
 * 1. Altering option settings can have side effects.
 * 2. Option values can be stored in native format.
 *    (Important for efficiency in, e. g., the evaluator.)
 * 3. Doesn't use up name space.
 * 4. Options can be naturally grouped => ease of use.
 */

/* scm_options is the core of all options interface procedures.
 *
 * Some definitions:
 *
 * Run time options in Guile are arranged in groups.  Each group
 * affects a certain aspect of the behaviour of the library.
 *
 * An "options interface procedure" manages one group of options.  It
 * can be used to check or set options, or to get documentation for
 * all options of a group.  The options interface procedure is not
 * intended to be called directly by the user.  The user should
 * instead call
 *
 *   (<group>-options)
 *   (<group>-options 'help)
 *   (<group>-options 'full)
 *
 * to display current option settings (The second version also
 * displays documentation.  The third version also displays
 * information about programmer's options.), and
 *
 *   (<group>-enable  '<option-symbol>)
 *   (<group>-disable '<option-symbol>)
 *   (<group>-set! <option-symbol> <value>)
 *   (<group>-options <option setting>)
 *
 * to alter the state of an option  (The last version sets all
 * options according to <option setting>.) where <group> is the name
 * of the option group.
 *
 * An "option setting" represents the state of all low-level options
 * managed by one options interface procedure.  It is a list of
 * single symbols and symbols followed by a value.
 *
 * For boolean options, the presence of the symbol of that option in
 * the option setting indicates a true value.  If the symbol isn't a
 * member of the option setting list, this represents a false value.
 *
 * Other options are represented by a symbol followed by that options
 * value.
 *
 * If scm_options is called without arguments, the current option
 * setting is returned.  If the argument is an option setting, options
 * are altered and the old setting is returned.  If the argument isn't
 * a list, a list of sublists is returned, where each sublist contains
 * option name, value and documentation string.
 */

SCM_SYMBOL (scm_yes_sym, "yes");
SCM_SYMBOL (scm_no_sym, "no");

#ifdef __STDC__
SCM
scm_options (SCM new_mode, scm_option options[], int n, char *s)
#else
SCM
scm_options (new_mode, options, n, s)
     SCM new_mode;
     scm_option options[];
     int n;
     char *s;
#endif
{
  int i, docp = (!SCM_UNBNDP (new_mode)
		 && (SCM_IMP (new_mode) || SCM_NCONSP (new_mode)));
  SCM ans = SCM_EOL, ls;
  for (i = 0; i < n; ++i)
    {
      ls = docp ? scm_cons ((SCM) options[i].doc, SCM_EOL) : ans;
      switch (options[i].type)
	{
	case SCM_OPTION_BOOLEAN:
	  if (docp)
	    ls = scm_cons ((int) options[i].val
			   ? scm_yes_sym
			   : scm_no_sym,
			   ls);
	  break;
	case SCM_OPTION_INTEGER:
	  ls = scm_cons (SCM_MAKINUM ((int) options[i].val), ls);
	  break;
	case SCM_OPTION_SCM:
	  ls = scm_cons ((SCM) options[i].val, ls);
	}
      if (!((options[i].type == SCM_OPTION_BOOLEAN)
	    && !docp
	    && ! (int) options[i].val))
	ls = scm_cons ((SCM) options[i].name, ls);
      ans = docp ? scm_cons (ls, ans) : ls;
    }
  if (!(SCM_UNBNDP (new_mode) || docp))
    {
      unsigned long *flags;
      flags = (unsigned long *) scm_must_malloc (n * sizeof (unsigned long),
						 "mode buffer");
      for (i = 0; i < n; ++i)
	if (options[i].type == SCM_OPTION_BOOLEAN)
	  flags[i] = 0;
	else
	  flags[i] = (unsigned long) options[i].val;
      while (SCM_NNULLP (new_mode))
	{
	  SCM_ASSERT (SCM_NIMP (new_mode) && SCM_CONSP (new_mode),
		      new_mode,
		      SCM_ARG1,
		      s);
	  for (i = 0; i < n; ++i)
	    if (SCM_CAR (new_mode) == (SCM) options[i].name)
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
		  flags[i] = (unsigned long) SCM_INUM (SCM_CAR (new_mode));
		  goto cont;
		case SCM_OPTION_SCM:
		  new_mode = SCM_CDR (new_mode);
		  flags[i] = SCM_CAR (new_mode);
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
  return ans;
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
    {
      options[i].name =	(char *) SCM_CAR (scm_sysintern (options[i].name,
							 SCM_UNDEFINED));
      options[i].doc = (char *) scm_permanent_object (scm_take0str
						      (options[i].doc));
    }
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

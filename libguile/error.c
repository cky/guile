/*	Copyright (C) 1995,1996,1997,1998, 2000 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"
#include "libguile/pairs.h"
#include "libguile/strings.h"
#include "libguile/throw.h"

#include "libguile/validate.h"
#include "libguile/error.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif



/* {Errors and Exceptional Conditions}
 */


extern int errno;

/* All errors should pass through here.  */
void
scm_error (SCM key, const char *subr, const char *message, SCM args, SCM rest)
{
  SCM arg_list;
  if (scm_gc_running_p)
    {
      /* The error occured during GC --- abort */
      fprintf (stderr, "Error in %s during GC: %s\n",
	       subr ? subr : "unknown function",
	       message ? message : "<empty message>");
      abort ();
    }
  arg_list = scm_listify (subr ? scm_makfrom0str (subr) : SCM_BOOL_F,
			  message ? scm_makfrom0str (message) : SCM_BOOL_F,
			  args,
			  rest,
			  SCM_UNDEFINED);
  scm_ithrow (key, arg_list, 1);
  
  /* No return, but just in case: */
  {
    const char msg[] = "guile:scm_error:scm_ithrow returned!\n";

    write (2, msg, (sizeof msg) - 1);
  }
  exit (1);
}

/* Scheme interface to scm_error.  */
SCM_DEFINE (scm_error_scm, "scm-error", 5, 0, 0, 
           (SCM key, SCM subr, SCM message, SCM args, SCM rest),
	    "Raise an error with key @var{key}.  @var{subr} can be a string naming\n"
	    "the procedure associated with the error, or @code{#f}.  @var{message}\n"
	    "is the error message string, possibly containing @code{~S} and @code{~A}\n"
	    "escapes.  When an error is reported, these are replaced by formating the\n"
	    "corresponding members of @var{args}: @code{~A} (was @code{%s}) formats using @code{display}\n"
	    "and @code{~S} (was @code{%S}) formats using @code{write}.  @var{data} is a\n"
	    "list or @code{#f} depending on @var{key}: if @var{key} is\n"
	    "@code{system-error} then it should be a list\n"
	    "containing the Unix @code{errno} value;  If @var{key} is @code{signal} then\n"
	    "it should be a list containing the Unix signal number; otherwise it\n"
	    "will usually be @code{#f}.")
#define FUNC_NAME s_scm_error_scm
{
  char *szSubr;
  char *szMessage;

  SCM_VALIDATE_SYMBOL (1, key);

  if (SCM_FALSEP (subr))
    {
      szSubr = NULL;
    }
  else if (SCM_SYMBOLP (subr))
    {
      szSubr = SCM_SYMBOL_CHARS (subr);
    }
  else
    {
      SCM_VALIDATE_STRING (2, subr);
      SCM_STRING_COERCE_0TERMINATION_X (subr);
      szSubr = SCM_STRING_CHARS (subr);
    }

  if (SCM_FALSEP (message))
    {
      szMessage = NULL;
    }
  else
    {
      SCM_VALIDATE_STRING (2, message);
      SCM_STRING_COERCE_0TERMINATION_X (message);
      szMessage = SCM_STRING_CHARS (message);
    }

  scm_error (key, szSubr, szMessage, args, rest);
  /* not reached.  */
}
#undef FUNC_NAME

SCM_DEFINE (scm_strerror, "strerror", 1, 0, 0, 
            (SCM err),
	    "Returns the Unix error message corresponding to @var{err}, an integer.")
#define FUNC_NAME s_scm_strerror
{
  SCM_VALIDATE_INUM (1,err);
  return scm_makfrom0str (strerror (SCM_INUM (err)));
}
#undef FUNC_NAME

SCM_SYMBOL (scm_system_error_key, "system-error");
void
scm_syserror (const char *subr)
{
  int save_errno = errno;
  
  scm_error (scm_system_error_key,
	     subr,
	     "~A",
	     scm_cons (scm_makfrom0str (strerror (save_errno)), SCM_EOL),
	     scm_cons (SCM_MAKINUM (save_errno), SCM_EOL));
}

void
scm_syserror_msg (const char *subr, const char *message, SCM args, int eno)
{
  scm_error (scm_system_error_key,
	     subr,
	     message,
	     args,
	     scm_cons (SCM_MAKINUM (eno), SCM_EOL));
}


#if (SCM_DEBUG_DEPRECATED == 0)

/* scm_sysmissing is no longer used in libguile.  it can probably be
   removed after a release or two.  there's a comment in NEWS about it
   (2000-01-09).  */
void
scm_sysmissing (const char *subr)
{
#ifdef ENOSYS
  scm_error (scm_system_error_key,
	     subr,
	     "~A",
	     scm_cons (scm_makfrom0str (strerror (ENOSYS)), SCM_EOL),
	     scm_cons (SCM_MAKINUM (ENOSYS), SCM_EOL));
#else
  scm_error (scm_system_error_key,
	     subr,
	     "Missing function",
	     SCM_BOOL_F,
	     scm_cons (SCM_MAKINUM (0), SCM_EOL));
#endif
}

#endif  /* SCM_DEBUG_DEPRECATED == 0 */


SCM_SYMBOL (scm_num_overflow_key, "numerical-overflow");
void
scm_num_overflow (const char *subr)
{
  scm_error (scm_num_overflow_key,
	     subr,
	     "Numerical overflow",
	     SCM_BOOL_F,
	     SCM_BOOL_F);
}

SCM_SYMBOL (scm_out_of_range_key, "out-of-range");
void
scm_out_of_range (const char *subr, SCM bad_value)
{
  scm_error (scm_out_of_range_key,
	     subr,
	     "Argument out of range: ~S",
             SCM_LIST1(bad_value),
	     SCM_BOOL_F);
}

void
scm_out_of_range_pos (const char *subr, SCM bad_value, SCM pos)
{
  scm_error (scm_out_of_range_key,
	     subr,
	     "Argument ~S out of range: ~S",
             SCM_LIST2(pos,bad_value),
	     SCM_BOOL_F);
}


SCM_SYMBOL (scm_args_number_key, "wrong-number-of-args");
void
scm_wrong_num_args (SCM proc)
{
  scm_error (scm_args_number_key,
	     NULL,
	     "Wrong number of arguments to ~A",
	     SCM_LIST1(proc),
	     SCM_BOOL_F);
}

SCM_SYMBOL (scm_arg_type_key, "wrong-type-arg");
void
scm_wrong_type_arg (const char *subr, int pos, SCM bad_value)
{
  scm_error (scm_arg_type_key,
	     subr,
	     (pos == 0) ? "Wrong type argument: ~S"
	     : "Wrong type argument in position ~A: ~S",
	     (pos == 0) ? SCM_LIST1(bad_value)
	     : SCM_LIST2(SCM_MAKINUM(pos), bad_value),
	     SCM_BOOL_F);
}

void
scm_wrong_type_arg_msg (const char *subr, int pos, SCM bad_value, const char *szMessage)
{
  SCM msg = scm_makfrom0str(szMessage);
  if (pos == 0) {
    scm_error (scm_arg_type_key,
               subr, "Wrong type argument (expecting ~A): ~S",
               SCM_LIST2(msg,bad_value),
               SCM_BOOL_F);
  } else {
    scm_error (scm_arg_type_key,
               subr,
               "Wrong type argument in position ~A (expecting ~A): ~S",
               SCM_LIST3(SCM_MAKINUM(pos),msg,bad_value),
               SCM_BOOL_F);
  }
}


SCM_SYMBOL (scm_memory_alloc_key, "memory-allocation-error");
void
scm_memory_error (const char *subr)
{
  scm_error (scm_memory_alloc_key,
	     subr,
	     "Memory allocation error",
	     SCM_BOOL_F,
	     SCM_BOOL_F);
}

SCM_SYMBOL (scm_misc_error_key, "misc-error");
void
scm_misc_error (const char *subr, const char *message, SCM args)
{
  scm_error (scm_misc_error_key, subr, message, args, SCM_BOOL_F);
}

/* implements the SCM_ASSERT interface.  */  
SCM
scm_wta (SCM arg, const char *pos, const char *s_subr)
{
  if (!s_subr || !*s_subr)
    s_subr = NULL;
  if ((~0x1fL) & (long) pos)
    {
      /* error string supplied.  */
      scm_misc_error (s_subr, pos, SCM_LIST1 (arg));
    }
  else
    {
      /* numerical error code.  */
      int error = (long) pos;

      switch (error)
	{
	case SCM_ARGn:
	  scm_wrong_type_arg (s_subr, 0, arg);
	case SCM_ARG1:
	  scm_wrong_type_arg (s_subr, 1, arg);
	case SCM_ARG2:
	  scm_wrong_type_arg (s_subr, 2, arg);
	case SCM_ARG3:
	  scm_wrong_type_arg (s_subr, 3, arg);
	case SCM_ARG4:
	  scm_wrong_type_arg (s_subr, 4, arg);
	case SCM_ARG5:
	  scm_wrong_type_arg (s_subr, 5, arg);
	case SCM_ARG6:
	  scm_wrong_type_arg (s_subr, 6, arg);
	case SCM_ARG7:
	  scm_wrong_type_arg (s_subr, 7, arg);
	case SCM_WNA:
	  scm_wrong_num_args (arg);

#if (SCM_DEBUG_DEPRECATED == 0)

	case SCM_OUTOFRANGE:
	  scm_out_of_range (s_subr, arg);
	case SCM_NALLOC:
	  scm_memory_error (s_subr);

#endif  /* SCM_DEBUG_DEPRECATED == 0 */

	default:
	  /* this shouldn't happen.  */
	  scm_misc_error (s_subr, "Unknown error", SCM_EOL);
	}
    }
  return SCM_UNSPECIFIED;
}

void
scm_init_error ()
{
#include "libguile/cpp_err_symbols.c"
#include "libguile/error.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

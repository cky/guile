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
 */


#include <stdio.h>
#include "_scm.h"
#include "pairs.h"
#include "genio.h"
#include "throw.h"

#include "error.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif



/* {Errors and Exceptional Conditions}
 */


/* True between SCM_DEFER_INTS and SCM_ALLOW_INTS, and
 * when the interpreter is not running at all.
 */
int scm_ints_disabled = 1;

extern int errno;

static void 
err_head (str)
     char *str;
{
  int oerrno = errno;
  if (SCM_NIMP (scm_cur_outp))
    scm_fflush (scm_cur_outp);
  scm_gen_putc ('\n', scm_cur_errp);
#if 0
  if (SCM_BOOL_F != *scm_loc_loadpath)
    {
      scm_prin1 (*scm_loc_loadpath, scm_cur_errp, 1);
      scm_gen_puts (scm_regular_string, ", line ", scm_cur_errp);
      scm_intprint ((long) scm_linum, 10, scm_cur_errp);
      scm_gen_puts (scm_regular_string, ": ", scm_cur_errp);
    }
#endif
  scm_fflush (scm_cur_errp);
  errno = oerrno;
  if (scm_cur_errp == scm_def_errp)
    {
      if (errno > 0)
	perror (str);
      fflush (stderr);
      return;
    }
}


SCM_PROC(s_errno, "errno", 0, 1, 0, scm_errno);
SCM
scm_errno (arg)
     SCM arg;
{
  int old = errno;
  if (!SCM_UNBNDP (arg))
    {
      if (SCM_FALSEP (arg))
	errno = 0;
      else
	errno = SCM_INUM (arg);
    }
  return SCM_MAKINUM (old);
}

SCM_PROC(s_perror, "perror", 1, 0, 0, scm_perror);
SCM 
scm_perror (arg)
     SCM arg;
{
  SCM_ASSERT (SCM_NIMP (arg) && SCM_STRINGP (arg), arg, SCM_ARG1, s_perror);
  err_head (SCM_CHARS (arg));
  return SCM_UNSPECIFIED;
}

void (*scm_error_callback) () = 0;

/* all errors thrown from C should pass through here.  */
/* also known as scm_error.  */
void
scm_error (key, subr, message, args, rest)
     SCM key;
     char *subr;
     char *message;
     SCM args;
     SCM rest;
{
  SCM arg_list;
  if (scm_error_callback)
    (*scm_error_callback) (key, subr, message, args, rest);

  arg_list = scm_listify (subr ? scm_makfrom0str (subr) : SCM_BOOL_F,
			  message ? scm_makfrom0str (message) : SCM_BOOL_F,
			  args,
			  rest,
			  SCM_UNDEFINED);
  scm_ithrow (key, arg_list, 1);
  
  /* No return, but just in case: */

  write (2, "unhandled system error", sizeof ("unhandled system error") - 1);
  exit (1);
}

/* error keys: defined here, initialized below, prototyped in error.h,
   associated with handler procedures in boot-9.scm.  */
SCM scm_system_error_key;
SCM scm_num_overflow_key;
SCM scm_out_of_range_key;
SCM scm_arg_type_key;
SCM scm_args_number_key;
SCM scm_memory_alloc_key;
SCM scm_stack_overflow_key;
SCM scm_misc_error_key;

void
scm_syserror (subr)
     char *subr;
{
  scm_error (scm_system_error_key,
	     subr,
	     "%s",
	     scm_listify (scm_makfrom0str (strerror (errno)),
			  SCM_UNDEFINED),
	     scm_listify (SCM_MAKINUM (errno), SCM_UNDEFINED));
}

void
scm_syserror_msg (subr, message, args)
     char *subr;
     char *message;
     SCM args;
{
  scm_error (scm_system_error_key,
	     subr,
	     message,
	     args,
	     scm_listify (SCM_MAKINUM (errno), SCM_UNDEFINED));
}

void
scm_sysmissing (subr)
     char *subr;
{
#ifdef ENOSYS
  scm_error (scm_system_error_key,
	     subr,
	     "%s",
	     scm_listify (scm_makfrom0str (strerror (ENOSYS)), SCM_UNDEFINED),
	     scm_listify (SCM_MAKINUM (ENOSYS), SCM_UNDEFINED));
#else
  scm_error (scm_system_error_key,
	     subr,
	     "Missing function",
	     SCM_BOOL_F,
	     scm_listify (SCM_MAKINUM (0), SCM_UNDEFINED));
#endif
}

void
scm_num_overflow (subr)
  char *subr;
{
  scm_error (scm_num_overflow_key,
	     subr,
	     "Numerical overflow",
	     SCM_BOOL_F,
	     SCM_BOOL_F);
}

void
scm_out_of_range (subr, bad_value)
     char *subr;
     SCM bad_value;
{
  scm_error (scm_out_of_range_key,
	     subr,
	     "Argument out of range: %S",
	     scm_listify (bad_value, SCM_UNDEFINED),
	     SCM_BOOL_F);
}

void
scm_wrong_num_args (proc)
     SCM proc;
{
  scm_error (scm_args_number_key,
	     NULL,
	     "Wrong number of arguments to %s",
	     scm_listify (proc, SCM_UNDEFINED),
	     SCM_BOOL_F);
}

void
scm_wrong_type_arg (subr, pos, bad_value)
     char *subr;
     int pos;
     SCM bad_value;
{
  scm_error (scm_arg_type_key,
	     subr,
	     (pos == 0) ? "Wrong type argument: %S"
	     : "Wrong type argument in position %s: %S",
	     (pos == 0) ? scm_listify (bad_value, SCM_UNDEFINED)
	     : scm_listify (SCM_MAKINUM (pos), bad_value, SCM_UNDEFINED),
	     SCM_BOOL_F);
}

void
scm_memory_error (subr)
     char *subr;
{
  scm_error (scm_memory_alloc_key,
	     subr,
	     "Memory allocation error",
	     SCM_BOOL_F,
	     SCM_BOOL_F);
}

/* implements the SCM_ASSERT interface.  */  
SCM
scm_wta (arg, pos, s_subr)
     SCM arg;
     char *pos;
     char *s_subr;
{
  if (!s_subr || !*s_subr)
    s_subr = NULL;
  if ((~0x1fL) & (long) pos)
    {
      /* error string supplied.  */
      scm_error (scm_misc_error_key,
		 s_subr,
		 pos,
		 SCM_BOOL_F,
		 SCM_BOOL_F);
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
	case SCM_WNA:
	  scm_wrong_num_args (arg);
	case SCM_OUTOFRANGE:
	  scm_out_of_range (s_subr, arg);
	case SCM_NALLOC:
	  scm_memory_error (s_subr);
	default:
	  /* this shouldn't happen.  */
	  scm_error (scm_misc_error_key,
		     s_subr,
		     "Unknown error",
		     SCM_BOOL_F,
		     SCM_BOOL_F);
	}
    }
  return SCM_UNSPECIFIED;
}

/*  obsolete interface: scm_everr (exp, env, arg, pos, s_subr)
    was equivalent to scm_wta (arg, pos, s_subr)  */

void
scm_init_error ()
{
  scm_system_error_key
    = scm_permanent_object (SCM_CAR (scm_intern0 ("system-error")));
  scm_num_overflow_key
    = scm_permanent_object (SCM_CAR (scm_intern0 ("numerical-overflow")));
  scm_out_of_range_key
    = scm_permanent_object (SCM_CAR (scm_intern0 ("out-of-range")));
  scm_arg_type_key
    = scm_permanent_object (SCM_CAR (scm_intern0 ("wrong-type-arg")));
  scm_args_number_key
    = scm_permanent_object (SCM_CAR (scm_intern0 ("wrong-number-of-args")));
  scm_memory_alloc_key
    = scm_permanent_object (SCM_CAR (scm_intern0 ("memory-allocation-error")));
  scm_stack_overflow_key
    = scm_permanent_object (SCM_CAR (scm_intern0 ("stack-overflow")));
  scm_misc_error_key
    = scm_permanent_object (SCM_CAR (scm_intern0 ("misc-error")));
#include "error.x"
}


/* Printing of backtraces and error messages
 * Copyright (C) 1996,1997 Free Software Foundation
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
#include "stacks.h"
#include "srcprop.h"
#include "genio.h"
#include "struct.h"
#include "strports.h"

#include "backtrace.h"

/* {Error reporting and backtraces}
 * (A first approximation.)
 *
 * Note that these functions shouldn't generate errors themselves.
 */

#ifndef SCM_RECKLESS
#undef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr) \
	if (!(_cond)) \
          return SCM_BOOL_F;
#endif

static void display_header SCM_P ((SCM source, SCM port));
static void
display_header (source, port)
     SCM source;
     SCM port;
{
  SCM fname = (SCM_NIMP (source) && SCM_MEMOIZEDP (source)
	       ? scm_source_property (source, scm_i_filename)
	       : SCM_BOOL_F);
  if (SCM_NIMP (fname) && SCM_STRINGP (fname))
    {
      scm_prin1 (fname, port, 0);
      scm_gen_putc (':', port);
      scm_prin1 (scm_source_property (source, scm_i_line), port, 0);
      scm_gen_putc (':', port);
      scm_prin1 (scm_source_property (source, scm_i_column), port, 0);
    }
  else
    scm_gen_puts (scm_regular_string, "ERROR", port);
  scm_gen_puts (scm_regular_string, ": ", port);
}


void
scm_display_error_message (message, args, port)
     SCM message;
     SCM args;
     SCM port;
{
  int writingp;
  char *start;
  char *p;
  
  if (SCM_IMP (message) || !SCM_ROSTRINGP (message) || SCM_IMP (args)
      || !scm_list_p (args))
    {
      scm_prin1 (message, port, 0);
      scm_gen_putc ('\n', port);
      return;
    }

  SCM_COERCE_SUBSTR (message);
  start = SCM_ROCHARS (message);
  for (p = start; *p != '\0'; ++p)
    if (*p == '%')
      {
	if (SCM_IMP (args) || SCM_NCONSP (args))
	  continue;
	
	++p;
	if (*p == 's')
	  writingp = 0;
	else if (*p == 'S')
	  writingp = 1;
	else
	  continue;

	scm_gen_write (scm_regular_string, start, p - start - 1, port);
	scm_prin1 (SCM_CAR (args), port, writingp);
	args = SCM_CDR (args);
	start = p + 1;
      }
  scm_gen_write (scm_regular_string, start, p - start, port);
  scm_gen_putc ('\n', port);
}

static void display_expression SCM_P ((SCM frame, SCM pname, SCM source, SCM port));
static void
display_expression (frame, pname, source, port)
     SCM frame;
     SCM pname;
     SCM source;
     SCM port;
{
  SCM print_state = scm_make_print_state ();
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  pstate->writingp = 0;
  pstate->fancyp = 1;
  pstate->level = 2;
  pstate->length = 3;
  if (SCM_NIMP (pname) && SCM_ROSTRINGP (pname))
    {
      if (SCM_NIMP (frame)
	  && SCM_FRAMEP (frame)
	  && SCM_FRAME_EVAL_ARGS_P (frame))
	scm_gen_puts (scm_regular_string, "While evaluating arguments to ", port);
      else
	scm_gen_puts (scm_regular_string, "In procedure ", port);
      scm_iprin1 (pname, port, pstate);
      if (SCM_NIMP (source) && SCM_MEMOIZEDP (source))
	{
	  scm_gen_puts (scm_regular_string, " in expression ", port);
	  pstate->writingp = 1;
	  scm_iprin1 (scm_unmemoize (source), port, pstate);
	}
    }
  else if (SCM_NIMP (source))
    {
      scm_gen_puts (scm_regular_string, "In expression ", port);
      pstate->writingp = 1;
      scm_iprin1 (scm_unmemoize (source), port, pstate);
    }
  scm_gen_puts (scm_regular_string, ":\n", port);
  scm_free_print_state (print_state);
}

SCM_PROC(s_display_error, "display-error", 6, 0, 0, scm_display_error);
SCM
scm_display_error (stack, port, subr, message, args, rest)
     SCM stack;
     SCM port;
     SCM subr;
     SCM message;
     SCM args;
     SCM rest;
{
  SCM current_frame = SCM_BOOL_F;
  SCM source = SCM_BOOL_F;
  SCM pname = SCM_BOOL_F;
  if (SCM_DEBUGGINGP
      && SCM_NIMP (stack)
      && SCM_STACKP (stack)
      && SCM_STACK_LENGTH (stack) > 0)
    {
      current_frame = scm_stack_ref (stack, SCM_INUM0);
      source = SCM_FRAME_SOURCE (current_frame);
      if (!(SCM_NIMP (source) && SCM_MEMOIZEDP (source)))
	source = SCM_FRAME_SOURCE (SCM_FRAME_PREV (current_frame));
      if (SCM_FRAME_PROC_P (current_frame)
	  && scm_procedure_p (SCM_FRAME_PROC (current_frame)) == SCM_BOOL_T)
	pname = scm_procedure_name (SCM_FRAME_PROC (current_frame));
    }
  if (!(SCM_NIMP (pname) && SCM_ROSTRINGP (pname)))
    pname = subr;
  if ((SCM_NIMP (source) && SCM_MEMOIZEDP (source))
      || (SCM_NIMP (pname) && SCM_ROSTRINGP (pname)))
    {
      display_header (source, port);
      display_expression (current_frame, pname, source, port);
    }
  display_header (source, port);
  scm_display_error_message (message, args, port);
  return SCM_UNSPECIFIED;
}

static void indent SCM_P ((int n, SCM port));
static void
indent (n, port)
     int n;
     SCM port;
{
  int i;
  for (i = 0; i < n; ++i)
    scm_gen_putc (' ', port);
}

static void display_frame_expr SCM_P ((char *hdr, SCM exp, char *tlr, int indentation, SCM sport, SCM port, scm_print_state *pstate));
static void
display_frame_expr (hdr, exp, tlr, indentation, sport, port, pstate)
     char *hdr;
     SCM exp;
     char *tlr;
     int indentation;
     SCM sport;
     SCM port;
     scm_print_state *pstate;
{
  if (SCM_NIMP (exp) && SCM_CONSP (exp))
    {
      scm_iprlist (hdr, exp, tlr[0], port, pstate);
      scm_gen_puts (scm_regular_string, &tlr[1], port);
    }
  else
    scm_iprin1 (exp, port, pstate);
  scm_gen_putc ('\n', port);
}

static void display_application SCM_P ((SCM frame, int indentation, SCM sport, SCM port, scm_print_state *pstate));
static void
display_application (frame, indentation, sport, port, pstate)
     SCM frame;
     int indentation;
     SCM sport;
     SCM port;
     scm_print_state *pstate;
{
  SCM proc = SCM_FRAME_PROC (frame);
  SCM name = (SCM_NFALSEP (scm_procedure_p (proc))
	      ? scm_procedure_name (proc)
	      : SCM_BOOL_F);
  display_frame_expr ("[",
		      scm_cons (SCM_NFALSEP (name) ? name : proc,
				SCM_FRAME_ARGS (frame)),
		      SCM_FRAME_EVAL_ARGS_P (frame) ? " ..." : "]",
		      indentation,
		      sport,
		      port,
		      pstate);
}

SCM_PROC(s_display_application, "display-application", 1, 1, 0, scm_display_application);

SCM
scm_display_application (SCM frame, SCM port)
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  if (SCM_FRAME_PROC_P (frame))
    /* Display an application. */
    {
      SCM print_state;
      scm_print_state *pstate;
      
      /* Create a print state for printing of frames. */
      print_state = scm_make_print_state ();
      pstate = SCM_PRINT_STATE (print_state);
      pstate->writingp = 1;
      pstate->fancyp = 1;
      pstate->level = 2;
      pstate->length = 9;
      
      display_application (frame, 0, SCM_BOOL_F, port, pstate); /*fixme*/
      return SCM_BOOL_T;
    }
  else
    return SCM_BOOL_F;
}

static void display_frame SCM_P ((SCM frame, int nfield, int indentation, SCM sport, SCM port, scm_print_state *pstate));
static void
display_frame (frame, nfield, indentation, sport, port, pstate)
     SCM frame;
     int nfield;
     int indentation;
     SCM sport;
     SCM port;
     scm_print_state *pstate;
{
  int n, i, j;

  /* Announce missing frames? */
  if (!SCM_BACKWARDS_P && SCM_FRAME_OVERFLOW_P (frame))
    {
      indent (nfield + 1 + indentation, port);
      scm_gen_puts (scm_regular_string, "...\n", port);
    }

  /* Check size of frame number. */
  n = SCM_FRAME_NUMBER (frame);
  for (i = 0, j = n; j > 0; ++i) j /= 10;

  /* Number indentation. */
  indent (nfield - (i ? i : 1), port);

  /* Frame number. */
  scm_iprin1 (SCM_MAKINUM (n), port, pstate);

  /* Real frame marker */
  scm_gen_putc (SCM_FRAME_REAL_P (frame) ? '*' : ' ', port);

  /* Indentation. */
  indent (indentation, port);

  if (SCM_FRAME_PROC_P (frame))
    /* Display an application. */
    display_application (frame, nfield + 1 + indentation, sport, port, pstate);
  else
    /* Display a special form. */
    {
      SCM source = SCM_FRAME_SOURCE (frame);
      SCM copy = scm_source_property (source, scm_i_copy);
      display_frame_expr ("(",
			  SCM_NIMP (copy) && SCM_CONSP (copy)
			  ? copy
			  : scm_unmemoize (source),
			  ")",
			  nfield + 1 + indentation,
			  sport,
			  port,
			  pstate);
    }

  /* Announce missing frames? */
  if (SCM_BACKWARDS_P && SCM_FRAME_OVERFLOW_P (frame))
    {
      indent (nfield + 1 + indentation, port);
      scm_gen_puts (scm_regular_string, "...\n", port);
    }
}

SCM_PROC(s_display_backtrace, "display-backtrace", 2, 2, 0, scm_display_backtrace);
SCM
scm_display_backtrace (stack, port, first, depth)
     SCM stack;
     SCM port;
     SCM first;
     SCM depth;
{
  int n_frames, beg, end, n, i, j;
  int nfield, indent_p, indentation;
  SCM frame, sport, print_state;
  scm_print_state *pstate;

  /* Argument checking and extraction. */
  SCM_ASSERT (SCM_NIMP (stack) && SCM_STACKP (stack),
	      stack,
	      SCM_ARG1,
	      s_display_backtrace);
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port),
	      port,
	      SCM_ARG2,
	      s_display_backtrace);
  n_frames = SCM_INUM (scm_stack_length (stack));
  n = SCM_INUMP (depth) ? SCM_INUM (depth) : SCM_BACKTRACE_DEPTH;
  if (SCM_BACKWARDS_P)
    {
      beg = SCM_INUMP (first) ? SCM_INUM (first) : 0;
      end = beg + n - 1;
      if (end >= n_frames)
	end = n_frames - 1;
      n = end - beg + 1;
    }
  else
    {
      if (SCM_INUMP (first))
	{
	  beg = SCM_INUM (first);
	  end = beg - n + 1;
	  if (end < 0)
	    end = 0;
	}
      else
	{
	  beg = n - 1;
	  end = 0;
	  if (beg >= n_frames)
	    beg = n_frames - 1;
	}
      n = beg - end + 1;
    }
  SCM_ASSERT (beg >= 0 && beg < n_frames, first, SCM_ARG3, s_display_backtrace);
  SCM_ASSERT (n > 0, depth, SCM_ARG4, s_display_backtrace);

  /* Create a string port used for adaptation of printing parameters. */
  sport = scm_mkstrport (SCM_INUM0,
			 scm_make_string (SCM_MAKINUM (240), SCM_UNDEFINED),
			 SCM_OPN | SCM_WRTNG,
			 s_display_backtrace);

  /* Create a print state for printing of frames. */
  print_state = scm_make_print_state ();
  pstate = SCM_PRINT_STATE (print_state);
  pstate->writingp = 1;
  pstate->fancyp = 1;
  pstate->level = 2;
  pstate->length = 3;

  /* First find out if it's reasonable to do indentation. */
  if (SCM_BACKWARDS_P)
    indent_p = 0;
  else
    {
      indent_p = 1;
      frame = scm_stack_ref (stack, SCM_MAKINUM (beg));
      for (i = 0, j = 0; i < n; ++i)
	{
	  if (SCM_FRAME_REAL_P (frame))
	    ++j;
	  if (j > SCM_BACKTRACE_INDENT)
	    {
	      indent_p = 0;
	      break;
	    }
	  frame = (SCM_BACKWARDS_P
		   ? SCM_FRAME_PREV (frame)
		   : SCM_FRAME_NEXT (frame));
	}
    }
  
  /* Determine size of frame number field. */
  j = SCM_FRAME_NUMBER (scm_stack_ref (stack, SCM_MAKINUM (end)));
  for (i = 0; j > 0; ++i) j /= 10;
  nfield = i ? i : 1;
  
  scm_gen_puts (scm_regular_string, "Backtrace:\n", port);

  /* Print frames. */
  frame = scm_stack_ref (stack, SCM_MAKINUM (beg));
  indentation = 1;
  display_frame (frame, nfield, indentation, sport, port, pstate);
  for (i = 1; i < n; ++i)
    {
      if (indent_p && SCM_FRAME_EVAL_ARGS_P (frame))
	++indentation;
      frame = SCM_BACKWARDS_P ? SCM_FRAME_PREV (frame) : SCM_FRAME_NEXT (frame);
      display_frame (frame, nfield, indentation, sport, port, pstate);
    }
  
  return SCM_UNSPECIFIED;
}

SCM_VCELL (scm_has_shown_backtrace_hint_p_var, "has-shown-backtrace-hint?");

SCM_PROC(s_backtrace, "backtrace", 0, 0, 0, scm_backtrace);
SCM
scm_backtrace ()
{
  if (SCM_NFALSEP (SCM_CDR (scm_the_last_stack_var)))
    {
      scm_newline (scm_cur_outp);
      scm_display_backtrace (SCM_CDR (scm_the_last_stack_var),
			     scm_cur_outp,
			     SCM_UNDEFINED,
			     SCM_UNDEFINED);
      scm_newline (scm_cur_outp);
      if (SCM_FALSEP (SCM_CDR (scm_has_shown_backtrace_hint_p_var))
	  && !SCM_BACKTRACE_P)
	{
	  scm_gen_puts (scm_regular_string,
			"Type \"(debug-enable 'backtrace)\" if you would like "
			"a backtrace\n"
			"automatically if an error occurs in the future.\n",
			scm_cur_outp);
	  SCM_SETCDR (scm_has_shown_backtrace_hint_p_var, SCM_BOOL_T);
	}
    }
  else
    {
      scm_gen_puts (scm_regular_string,
		    "No backtrace available.\n",
		    scm_cur_outp);
    }
  return SCM_UNSPECIFIED;
}



void
scm_init_backtrace ()
{
  scm_the_last_stack_var = scm_sysintern ("the-last-stack", SCM_BOOL_F);

#include "backtrace.x"
}

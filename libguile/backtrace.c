/* Printing of backtraces and error messages
 * Copyright (C) 1996,1997,1998,1999,2000,2001 Free Software Foundation
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
 * If you do not wish that, delete this exception notice.
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN */



#include <stdio.h>
#include <ctype.h>

#include "libguile/_scm.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif

#include "libguile/stacks.h"
#include "libguile/srcprop.h"
#include "libguile/struct.h"
#include "libguile/strports.h"
#include "libguile/throw.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/backtrace.h"
#include "libguile/filesys.h"

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

SCM scm_the_last_stack_fluid_var;

static void
display_header (SCM source, SCM port)
{
  if (SCM_MEMOIZEDP (source))
    {
      SCM fname = scm_source_property (source, scm_sym_filename);
      SCM line = scm_source_property (source, scm_sym_line);
      SCM col = scm_source_property (source, scm_sym_column);

      /* Dirk:FIXME:: Maybe we should store the _port_ rather than the
       * filename with the source properties?  Then we could in case of
       * non-file ports give at least some more details than just
       * "<unnamed port>". */
      if (SCM_STRINGP (fname))
	scm_prin1 (fname, port, 0);
      else
	scm_puts ("<unnamed port>", port);

      if (!SCM_FALSEP (line) && !SCM_FALSEP (col))
	{
	  scm_putc (':', port);
	  scm_intprint (SCM_INUM (line) + 1, 10, port);
	  scm_putc (':', port);
	  scm_intprint (SCM_INUM (col) + 1, 10, port);
	}
    }
  else
    scm_puts ("ERROR", port);
  scm_puts (": ", port);
}


void
scm_display_error_message (SCM message, SCM args, SCM port)
{
  if (SCM_STRINGP (message) && !SCM_FALSEP (scm_list_p (args)))
    {
      scm_simple_format (port, message, args);
      scm_newline (port);
    }
  else
    {
      scm_display (message, port);
      scm_newline (port);
    }
}

static void
display_expression (SCM frame,SCM pname,SCM source,SCM port)
{
  SCM print_state = scm_make_print_state ();
  scm_print_state *pstate = SCM_PRINT_STATE (print_state);
  pstate->writingp = 0;
  pstate->fancyp = 1;
  pstate->level = 2;
  pstate->length = 3;
  if (SCM_SYMBOLP (pname) || SCM_STRINGP (pname))
    {
      if (SCM_FRAMEP (frame)
	  && SCM_FRAME_EVAL_ARGS_P (frame))
	scm_puts ("While evaluating arguments to ", port);
      else
	scm_puts ("In procedure ", port);
      scm_iprin1 (pname, port, pstate);
      if (SCM_MEMOIZEDP (source))
	{
	  scm_puts (" in expression ", port);
	  pstate->writingp = 1;
	  scm_iprin1 (scm_unmemoize (source), port, pstate);
	}
    }
  else if (SCM_MEMOIZEDP (source))
    {
      scm_puts ("In expression ", port);
      pstate->writingp = 1;
      scm_iprin1 (scm_unmemoize (source), port, pstate);
    }
  scm_puts (":\n", port);
  scm_free_print_state (print_state);
}

struct display_error_args {
  SCM stack;
  SCM port;
  SCM subr;
  SCM message;
  SCM args;
  SCM rest;
};

static SCM
display_error_body (struct display_error_args *a)
{
  SCM current_frame = SCM_BOOL_F;
  SCM source = SCM_BOOL_F;
  SCM prev_frame = SCM_BOOL_F;
  SCM pname = a->subr;

  if (SCM_DEBUGGINGP
      && SCM_STACKP (a->stack)
      && SCM_STACK_LENGTH (a->stack) > 0)
    {
      current_frame = scm_stack_ref (a->stack, SCM_INUM0);
      source = SCM_FRAME_SOURCE (current_frame);
      prev_frame = SCM_FRAME_PREV (current_frame);
      if (!SCM_MEMOIZEDP (source) && !SCM_FALSEP (prev_frame))
	source = SCM_FRAME_SOURCE (prev_frame);
      if (!SCM_SYMBOLP (pname) && !SCM_STRINGP (pname) && SCM_FRAME_PROC_P (current_frame)
	  && SCM_EQ_P (scm_procedure_p (SCM_FRAME_PROC (current_frame)), SCM_BOOL_T))
	pname = scm_procedure_name (SCM_FRAME_PROC (current_frame));
    }
  if (SCM_SYMBOLP (pname) || SCM_STRINGP (pname) || SCM_MEMOIZEDP (source))
    {
      display_header (source, a->port);
      display_expression (current_frame, pname, source, a->port);
    }
  display_header (source, a->port);
  scm_display_error_message (a->message, a->args, a->port);
  return SCM_UNSPECIFIED;
}

struct display_error_handler_data {
  char *mode;
  SCM port;
};

/* This is the exception handler for error reporting routines.
   Note that it is very important that this handler *doesn't* try to
   print more than the error tag, since the error very probably is
   caused by an erroneous print call-back routine.  If we would
   try to print all objects, we would enter an infinite loop. */
static SCM
display_error_handler (struct display_error_handler_data *data,
		       SCM tag, SCM args SCM_UNUSED)
{
  SCM print_state = scm_make_print_state ();
  scm_puts ("\nException during displaying of ", data->port);
  scm_puts (data->mode, data->port);
  scm_puts (": ", data->port);
  scm_iprin1 (tag, data->port, SCM_PRINT_STATE (print_state));
  scm_putc ('\n', data->port);
  return SCM_UNSPECIFIED;
}


/* The function scm_i_display_error prints out a detailed error message.  This
 * function will be called directly within libguile to signal error messages.
 * No parameter checks will be performed by scm_i_display_error.  Thus, User
 * code should rather use the function scm_display_error.
 */
void
scm_i_display_error (SCM stack, SCM port, SCM subr, SCM message, SCM args, SCM rest)
{
  struct display_error_args a;
  struct display_error_handler_data data;
  a.stack = stack;
  a.port  = port;
  a.subr  = subr;
  a.message = message;
  a.args  = args;
  a.rest  = rest;
  data.mode = "error";
  data.port = port;
  scm_internal_catch (SCM_BOOL_T,
		      (scm_t_catch_body) display_error_body, &a,
		      (scm_t_catch_handler) display_error_handler, &data);
}


SCM_DEFINE (scm_display_error, "display-error", 6, 0, 0,
	    (SCM stack, SCM port, SCM subr, SCM message, SCM args, SCM rest),
	    "Display an error message to the output port @var{port}.\n"
	    "@var{stack} is the saved stack for the error, @var{subr} is\n"
	    "the name of the procedure in which the error occured and\n"
	    "@var{message} is the actual error message, which may contain\n"
	    "formatting instructions. These will format the arguments in\n"
	    "the list @var{args} accordingly.  @var{rest} is currently\n"
	    "ignored.")
#define FUNC_NAME s_scm_display_error
{
  SCM_VALIDATE_OUTPUT_PORT (2, port);

  scm_i_display_error (stack, port, subr, message, args, rest);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


typedef struct {
  int level;
  int length;
} print_params_t;

static int n_print_params = 9;
static print_params_t default_print_params[] = {
  { 4, 9 }, { 4, 3 },
  { 3, 4 }, { 3, 3 },
  { 2, 4 }, { 2, 3 },
  { 1, 4 }, { 1, 3 }, { 1, 2 }
};
static print_params_t *print_params = default_print_params;

#ifdef GUILE_DEBUG
SCM_DEFINE (scm_set_print_params_x, "set-print-params!", 1, 0, 0,
           (SCM params),
	    "Set the print parameters to the values from @var{params}.\n"
	    "@var{params} must be a list of two-element lists which must\n"
	    "hold two integer values.")
#define FUNC_NAME s_scm_set_print_params_x
{
  int i;
  int n;
  SCM ls;
  print_params_t *new_params;

  SCM_VALIDATE_NONEMPTYLIST_COPYLEN (2, params, n);
  for (ls = params; !SCM_NULLP (ls); ls = SCM_CDR (ls))
    SCM_ASSERT (scm_ilength (SCM_CAR (params)) == 2
		&& SCM_INUMP (SCM_CAAR (ls))
		&& SCM_INUM (SCM_CAAR (ls)) >= 0
		&& SCM_INUMP (SCM_CADAR (ls))
		&& SCM_INUM (SCM_CADAR (ls)) >= 0,
		params,
		SCM_ARG2,
		s_scm_set_print_params_x);
  new_params = scm_must_malloc (n * sizeof (print_params_t),
				FUNC_NAME);
  if (print_params != default_print_params)
    scm_must_free (print_params);
  print_params = new_params;
  for (i = 0; i < n; ++i)
    {
      print_params[i].level = SCM_INUM (SCM_CAAR (params));
      print_params[i].length = SCM_INUM (SCM_CADAR (params));
      params = SCM_CDR (params);
    }
  n_print_params = n;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

static void
indent (int n, SCM port)
{
  int i;
  for (i = 0; i < n; ++i)
    scm_putc (' ', port);
}

static void
display_frame_expr (char *hdr,SCM exp,char *tlr,int indentation,SCM sport,SCM port,scm_print_state *pstate)
{
  SCM string;
  int i = 0, n;
  scm_t_ptob_descriptor *ptob = scm_ptobs + SCM_PTOBNUM (sport);
  do
    {
      pstate->length = print_params[i].length;
      ptob->seek (sport, 0, SEEK_SET);
      if (SCM_CONSP (exp))
	{
	  pstate->level = print_params[i].level - 1;
	  scm_iprlist (hdr, exp, tlr[0], sport, pstate);
	  scm_puts (&tlr[1], sport);
	}
      else
	{
	  pstate->level = print_params[i].level;
	  scm_iprin1 (exp, sport, pstate);
	}
      ptob->flush (sport);
      n = ptob->seek (sport, 0, SEEK_CUR);
      ++i;
    }
  while (indentation + n > SCM_BACKTRACE_WIDTH && i < n_print_params);
  ptob->truncate (sport, n);
  string = scm_strport_to_string (sport);
  /* Remove control characters */
  for (i = 0; i < n; ++i)
    if (iscntrl (SCM_STRING_CHARS (string)[i]))
      SCM_STRING_CHARS (string)[i] = ' ';
  /* Truncate */
  if (indentation + n > SCM_BACKTRACE_WIDTH)
    {
      n = SCM_BACKTRACE_WIDTH - indentation;
      SCM_STRING_CHARS (string)[n - 1] = '$';
    }
      
  scm_lfwrite (SCM_STRING_CHARS (string), n, port);
}

static void
display_application (SCM frame,int indentation,SCM sport,SCM port,scm_print_state *pstate)
{
  SCM proc = SCM_FRAME_PROC (frame);
  SCM name = (!SCM_FALSEP (scm_procedure_p (proc))
	      ? scm_procedure_name (proc)
	      : SCM_BOOL_F);
  display_frame_expr ("[",
		      scm_cons (!SCM_FALSEP (name) ? name : proc,
				SCM_FRAME_ARGS (frame)),
		      SCM_FRAME_EVAL_ARGS_P (frame) ? " ..." : "]",
		      indentation,
		      sport,
		      port,
		      pstate);
}

SCM_DEFINE (scm_display_application, "display-application", 1, 2, 0, 
           (SCM frame, SCM port, SCM indent),
	    "Display a procedure application @var{frame} to the output port\n"
	    "@var{port}. @var{indent} specifies the indentation of the\n"
	    "output.")
#define FUNC_NAME s_scm_display_application
{
  SCM_VALIDATE_FRAME (1,frame);
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_VALIDATE_OPOUTPORT (2,port);
  if (SCM_UNBNDP (indent))
    indent = SCM_INUM0;
  else
    SCM_VALIDATE_INUM (3,indent);
  
  if (SCM_FRAME_PROC_P (frame))
    /* Display an application. */
    {
      SCM sport, print_state;
      scm_print_state *pstate;
      
      /* Create a string port used for adaptation of printing parameters. */
      sport = scm_mkstrport (SCM_INUM0,
			     scm_make_string (SCM_MAKINUM (240),
					      SCM_UNDEFINED),
			     SCM_OPN | SCM_WRTNG,
			     FUNC_NAME);

      /* Create a print state for printing of frames. */
      print_state = scm_make_print_state ();
      pstate = SCM_PRINT_STATE (print_state);
      pstate->writingp = 1;
      pstate->fancyp = 1;
      
      display_application (frame, SCM_INUM (indent), sport, port, pstate);
      return SCM_BOOL_T;
    }
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_SYMBOL (sym_base, "base");

static void
display_backtrace_get_file_line (SCM frame, SCM *file, SCM *line)
{
  SCM source = SCM_FRAME_SOURCE (frame);
  *file = SCM_MEMOIZEDP (source) ? scm_source_property (source, scm_sym_filename) : SCM_BOOL_F;
  *line = (SCM_MEMOIZEDP (source)) ? scm_source_property (source, scm_sym_line) : SCM_BOOL_F;
}

static void
display_backtrace_file (frame, last_file, port, pstate)
     SCM frame;
     SCM *last_file;
     SCM port;
     scm_print_state *pstate;
{
  SCM file, line;

  display_backtrace_get_file_line (frame, &file, &line);

  if (file == *last_file)
    return;

  *last_file = file;

  scm_puts ("In ", port);
  if (file == SCM_BOOL_F)
    if (line == SCM_BOOL_F)
      scm_puts ("unknown file", port);
    else
      scm_puts ("current input", port);
  else
    {
      pstate->writingp = 0;
      scm_iprin1 (file, port, pstate);
      pstate->writingp = 1;
    }
  scm_puts (":\n", port);
}

static void
display_backtrace_file_and_line (SCM frame, SCM port, scm_print_state *pstate)
{
  SCM file, line;

  display_backtrace_get_file_line (frame, &file, &line);

  if (SCM_EQ_P (SCM_SHOW_FILE_NAME, sym_base))
    {
      if (file == SCM_BOOL_F)
	{
	  if (line == SCM_BOOL_F)
	    scm_putc ('?', port);
	  else
	    scm_puts ("<stdin>", port);
	}
      else
	{
	  pstate -> writingp = 0;
	  scm_iprin1 (SCM_STRINGP (file) ? scm_basename (file, SCM_UNDEFINED) : file,
		      port, pstate);
	  pstate -> writingp = 1;
	}

      scm_putc (':', port);
    }
  else if (line != SCM_BOOL_F)
    {
      int i, j=0;
      for (i = SCM_INUM (line)+1; i > 0; i = i/10, j++)
	;
      indent (4-j, port);
    }

  if (line == SCM_BOOL_F)
    scm_puts ("   ?", port);
  else
    scm_intprint (SCM_INUM (line) + 1, 10, port);
  scm_puts (": ", port);
}

static void
display_frame (SCM frame,int nfield,int indentation,SCM sport,SCM port,scm_print_state *pstate)
{
  int n, i, j;

  /* Announce missing frames? */
  if (!SCM_BACKWARDS_P && SCM_FRAME_OVERFLOW_P (frame))
    {
      indent (nfield + 1 + indentation, port);
      scm_puts ("...\n", port);
    }

  /* display file name and line number */
  if (!SCM_FALSEP (SCM_SHOW_FILE_NAME))
    display_backtrace_file_and_line (frame, port, pstate);

  /* Check size of frame number. */
  n = SCM_FRAME_NUMBER (frame);
  for (i = 0, j = n; j > 0; ++i) j /= 10;

  /* Number indentation. */
  indent (nfield - (i ? i : 1), port);

  /* Frame number. */
  scm_iprin1 (SCM_MAKINUM (n), port, pstate);

  /* Real frame marker */
  scm_putc (SCM_FRAME_REAL_P (frame) ? '*' : ' ', port);

  /* Indentation. */
  indent (indentation, port);

  if (SCM_FRAME_PROC_P (frame))
    /* Display an application. */
    display_application (frame, nfield + 1 + indentation, sport, port, pstate);
  else
    /* Display a special form. */
    {
      SCM source = SCM_FRAME_SOURCE (frame);
      SCM copy = (SCM_CONSP (source) 
		  ? scm_source_property (source, scm_sym_copy)
		  : SCM_BOOL_F);
      SCM umcopy = (SCM_MEMOIZEDP (source)
		    ? scm_unmemoize (source)
		    : SCM_BOOL_F);
      display_frame_expr ("(",
			  SCM_CONSP (copy) ? copy : umcopy,
			  ")",
			  nfield + 1 + indentation,
			  sport,
			  port,
			  pstate);
    }
  scm_putc ('\n', port);

  /* Announce missing frames? */
  if (SCM_BACKWARDS_P && SCM_FRAME_OVERFLOW_P (frame))
    {
      indent (nfield + 1 + indentation, port);
      scm_puts ("...\n", port);
    }
}

struct display_backtrace_args {
  SCM stack;
  SCM port;
  SCM first;
  SCM depth;
};

static SCM
display_backtrace_body (struct display_backtrace_args *a)
#define FUNC_NAME "display_backtrace_body"
{
  int n_frames, beg, end, n, i, j;
  int nfield, indent_p, indentation;
  SCM frame, sport, print_state;
  SCM last_file;
  scm_print_state *pstate;

  a->port = SCM_COERCE_OUTPORT (a->port);

  /* Argument checking and extraction. */
  SCM_VALIDATE_STACK (1, a->stack);
  SCM_VALIDATE_OPOUTPORT (2, a->port);
  n_frames = SCM_INUM (scm_stack_length (a->stack));
  n = SCM_INUMP (a->depth) ? SCM_INUM (a->depth) : SCM_BACKTRACE_DEPTH;
  if (SCM_BACKWARDS_P)
    {
      beg = SCM_INUMP (a->first) ? SCM_INUM (a->first) : 0;
      end = beg + n - 1;
      if (end >= n_frames)
	end = n_frames - 1;
      n = end - beg + 1;
    }
  else
    {
      if (SCM_INUMP (a->first))
	{
	  beg = SCM_INUM (a->first);
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
  SCM_ASSERT (beg >= 0 && beg < n_frames, a->first, SCM_ARG3, s_display_backtrace);
  SCM_ASSERT (n > 0, a->depth, SCM_ARG4, s_display_backtrace);

  /* Create a string port used for adaptation of printing parameters. */
  sport = scm_mkstrport (SCM_INUM0,
			 scm_make_string (SCM_MAKINUM (240), SCM_UNDEFINED),
			 SCM_OPN | SCM_WRTNG,
			 FUNC_NAME);

  /* Create a print state for printing of frames. */
  print_state = scm_make_print_state ();
  pstate = SCM_PRINT_STATE (print_state);
  pstate->writingp = 1;
  pstate->fancyp = 1;

  /* First find out if it's reasonable to do indentation. */
  if (SCM_BACKWARDS_P)
    indent_p = 0;
  else
    {
      unsigned int j;

      indent_p = 1;
      frame = scm_stack_ref (a->stack, SCM_MAKINUM (beg));
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
  j = SCM_FRAME_NUMBER (scm_stack_ref (a->stack, SCM_MAKINUM (end)));
  for (i = 0; j > 0; ++i) j /= 10;
  nfield = i ? i : 1;
  
  /* Print frames. */
  frame = scm_stack_ref (a->stack, SCM_MAKINUM (beg));
  indentation = 1;
  last_file = SCM_UNDEFINED;
  for (i = 0; i < n; ++i)
    {
      if (!SCM_EQ_P (SCM_SHOW_FILE_NAME, sym_base))
	display_backtrace_file (frame, &last_file, a->port, pstate);

      display_frame (frame, nfield, indentation, sport, a->port, pstate);
      if (indent_p && SCM_FRAME_EVAL_ARGS_P (frame))
	++indentation;
      frame = (SCM_BACKWARDS_P ? 
	       SCM_FRAME_PREV (frame) : SCM_FRAME_NEXT (frame));
    }

  scm_remember_upto_here_1 (print_state);
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_display_backtrace, "display-backtrace", 2, 2, 0, 
	    (SCM stack, SCM port, SCM first, SCM depth),
	    "Display a backtrace to the output port @var{port}. @var{stack}\n"
	    "is the stack to take the backtrace from, @var{first} specifies\n"
	    "where in the stack to start and @var{depth} how much frames\n"
	    "to display. Both @var{first} and @var{depth} can be @code{#f},\n"
	    "which means that default values will be used.")
#define FUNC_NAME s_scm_display_backtrace
{
  struct display_backtrace_args a;
  struct display_error_handler_data data;
  a.stack = stack;
  a.port  = port;
  a.first = first;
  a.depth = depth;
  data.mode = "backtrace";
  data.port = port;
  scm_internal_catch (SCM_BOOL_T,
		      (scm_t_catch_body) display_backtrace_body, &a,
		      (scm_t_catch_handler) display_error_handler, &data);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_VARIABLE (scm_has_shown_backtrace_hint_p_var, "has-shown-backtrace-hint?");

SCM_DEFINE (scm_backtrace, "backtrace", 0, 0, 0, 
	    (),
	    "Display a backtrace of the stack saved by the last error\n"
	    "to the current output port.")
#define FUNC_NAME s_scm_backtrace
{
  SCM the_last_stack =
    scm_fluid_ref (SCM_VARIABLE_REF (scm_the_last_stack_fluid_var));
  if (!SCM_FALSEP (the_last_stack))
    {
      scm_newline (scm_cur_outp);
      scm_puts ("Backtrace:\n", scm_cur_outp);
      scm_display_backtrace (the_last_stack,
			     scm_cur_outp,
			     SCM_UNDEFINED,
			     SCM_UNDEFINED);
      scm_newline (scm_cur_outp);
      if (SCM_FALSEP (SCM_VARIABLE_REF (scm_has_shown_backtrace_hint_p_var))
	  && !SCM_BACKTRACE_P)
	{
	  scm_puts ("Type \"(debug-enable 'backtrace)\" if you would like "
		    "a backtrace\n"
		    "automatically if an error occurs in the future.\n",
		    scm_cur_outp);
	  SCM_VARIABLE_SET (scm_has_shown_backtrace_hint_p_var, SCM_BOOL_T);
	}
    }
  else
    {
      scm_puts ("No backtrace available.\n", scm_cur_outp);
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_init_backtrace ()
{
  SCM f = scm_make_fluid ();
  scm_the_last_stack_fluid_var = scm_c_define ("the-last-stack", f);

#ifndef SCM_MAGIC_SNARFER
#include "libguile/backtrace.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

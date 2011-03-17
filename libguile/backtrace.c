/* Printing of backtraces and error messages
 * Copyright (C) 1996,1997,1998,1999,2000,2001, 2003, 2004, 2006, 2009, 2010, 2011 Free Software Foundation
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
#  include <config.h>
#endif

#include <stdio.h>
#include <ctype.h>

#include "libguile/_scm.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif

#include "libguile/deprecation.h"
#include "libguile/stacks.h"
#include "libguile/srcprop.h"
#include "libguile/struct.h"
#include "libguile/strports.h"
#include "libguile/throw.h"
#include "libguile/fluids.h"
#include "libguile/ports.h"
#include "libguile/strings.h"
#include "libguile/dynwind.h"
#include "libguile/frames.h"

#include "libguile/validate.h"
#include "libguile/backtrace.h"
#include "libguile/filesys.h"
#include "libguile/private-options.h"

/* {Error reporting and backtraces}
 *
 * Note that these functions shouldn't generate errors themselves.
 */

static SCM
boot_print_exception (SCM port, SCM frame, SCM key, SCM args)
#define FUNC_NAME "boot-print-exception"
{
  scm_puts ("Throw to key ", port);
  scm_write (key, port);
  scm_puts (" with args ", port);
  scm_write (args, port);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_print_exception (SCM port, SCM frame, SCM key, SCM args)
#define FUNC_NAME "print-exception"
{
  static SCM print_exception = SCM_BOOL_F;

  SCM_VALIDATE_OPOUTPORT (1, port);
  if (scm_is_true (frame))
    SCM_VALIDATE_FRAME (2, frame);
  SCM_VALIDATE_SYMBOL (3, key);
  SCM_VALIDATE_LIST (4, args);
  
  if (scm_is_false (print_exception))
    print_exception =
      scm_module_variable (scm_the_root_module (),
                           scm_from_latin1_symbol ("print-exception"));

  return scm_call_4 (scm_variable_ref (print_exception),
                     port, frame, key, args);
}
#undef FUNC_NAME




/* Print parameters for error messages. */

#define DISPLAY_ERROR_MESSAGE_MAX_LEVEL   7
#define DISPLAY_ERROR_MESSAGE_MAX_LENGTH 10

/* Print parameters for failing expressions in error messages.
 * (See also `print_params' below for backtrace print parameters.)
 */

#define DISPLAY_EXPRESSION_MAX_LEVEL      2
#define DISPLAY_EXPRESSION_MAX_LENGTH     3

#undef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr) \
	if (!(_cond)) \
          return SCM_BOOL_F;


void
scm_display_error_message (SCM message, SCM args, SCM port)
{
  scm_print_exception (port, SCM_BOOL_F, scm_misc_error_key,
                       scm_list_3 (SCM_BOOL_F, message, args));
}


/* The function scm_i_display_error prints out a detailed error message.  This
 * function will be called directly within libguile to signal error messages.
 * No parameter checks will be performed by scm_i_display_error.  Thus, User
 * code should rather use the function scm_display_error.
 */
void
scm_i_display_error (SCM frame, SCM port, SCM subr, SCM message, SCM args, SCM rest)
{
  scm_print_exception (port, frame, scm_misc_error_key,
                       scm_list_3 (subr, message, args));
}


SCM_DEFINE (scm_display_error, "display-error", 6, 0, 0,
	    (SCM frame, SCM port, SCM subr, SCM message, SCM args, SCM rest),
	    "Display an error message to the output port @var{port}.\n"
	    "@var{frame} is the frame in which the error occurred, @var{subr} is\n"
	    "the name of the procedure in which the error occurred and\n"
	    "@var{message} is the actual error message, which may contain\n"
	    "formatting instructions. These will format the arguments in\n"
	    "the list @var{args} accordingly.  @var{rest} is currently\n"
	    "ignored.")
#define FUNC_NAME s_scm_display_error
{
  SCM_VALIDATE_OUTPUT_PORT (2, port);

  scm_i_display_error (frame, port, subr, message, args, rest);

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
  for (ls = params; !SCM_NULL_OR_NIL_P (ls); ls = SCM_CDR (ls))
    SCM_ASSERT (scm_ilength (SCM_CAR (params)) == 2
		&& scm_is_unsigned_integer (SCM_CAAR (ls), 0, INT_MAX)
		&& scm_is_unsigned_integer (SCM_CADAR (ls), 0, INT_MAX),
		params,
		SCM_ARG2,
		s_scm_set_print_params_x);
  new_params = scm_malloc (n * sizeof (print_params_t));
  if (print_params != default_print_params)
    free (print_params);
  print_params = new_params;
  for (i = 0; i < n; ++i)
    {
      print_params[i].level = scm_to_int (SCM_CAAR (params));
      print_params[i].length = scm_to_int (SCM_CADAR (params));
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
display_frame_expr (char *hdr, SCM exp, char *tlr, int indentation, SCM sport, SCM port, scm_print_state *pstate)
{
  int i = 0, n;
  scm_t_ptob_descriptor *ptob = scm_ptobs + SCM_PTOBNUM (sport);
  do
    {
      pstate->length = print_params[i].length;
      ptob->seek (sport, 0, SEEK_SET);
      if (scm_is_pair (exp))
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
      
  scm_display (scm_strport_to_string (sport), port);
}

static void
display_application (SCM frame, int indentation, SCM sport, SCM port, scm_print_state *pstate)
{
  SCM proc = scm_frame_procedure (frame);
  SCM name = (scm_is_true (scm_procedure_p (proc))
	      ? scm_procedure_name (proc)
	      : SCM_BOOL_F);
  display_frame_expr ("[",
		      scm_cons (scm_is_true (name) ? name : proc,
				scm_frame_arguments (frame)),
		      "]",
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
  SCM_VALIDATE_FRAME (1, frame);
  if (SCM_UNBNDP (port))
    port = scm_current_output_port ();
  else
    SCM_VALIDATE_OPOUTPORT (2, port);
  if (SCM_UNBNDP (indent))
    indent = SCM_INUM0;
  
  /* Display an application. */
  {
    SCM sport, print_state;
    scm_print_state *pstate;
      
    /* Create a string port used for adaptation of printing parameters. */
    sport = scm_mkstrport (SCM_INUM0, SCM_BOOL_F,
                           SCM_OPN | SCM_WRTNG,
                           FUNC_NAME);

    /* Create a print state for printing of frames. */
    print_state = scm_make_print_state ();
    pstate = SCM_PRINT_STATE (print_state);
    pstate->writingp = 1;
    pstate->fancyp = 1;
      
    display_application (frame, scm_to_int (indent), sport, port, pstate);
    return SCM_BOOL_T;
  }
}
#undef FUNC_NAME

SCM_SYMBOL (sym_base, "base");

static void
display_backtrace_get_file_line (SCM frame, SCM *file, SCM *line)
{
  SCM source = scm_frame_source (frame);
  *file = *line = SCM_BOOL_F;
  if (scm_is_pair (source)
      && scm_is_pair (scm_cdr (source))
      && scm_is_pair (scm_cddr (source))
      && !scm_is_pair (scm_cdddr (source)))
    {
      /* (addr . (filename . (line . column))), from vm compilation */
      *file = scm_cadr (source);
      *line = scm_caddr (source);
    }
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

  if (scm_is_true (scm_equal_p (file, *last_file)))
    return;

  *last_file = file;

  scm_puts ("In ", port);
  if (scm_is_false (file))
    if (scm_is_false (line))
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

  if (scm_is_eq (SCM_PACK (SCM_SHOW_FILE_NAME), sym_base))
    {
      if (scm_is_false (file))
	{
	  if (scm_is_false (line))
	    scm_putc ('?', port);
	  else
	    scm_puts ("<stdin>", port);
	}
      else
	{
	  pstate -> writingp = 0;
#ifdef HAVE_POSIX
	  scm_iprin1 ((scm_is_string (file)?
		       scm_basename (file, SCM_UNDEFINED) : file),
		      port, pstate);
#else
	  scm_iprin1 (file, port, pstate);
#endif
	  pstate -> writingp = 1;
	}

      scm_putc (':', port);
    }
  else if (scm_is_true (line))
    {
      int i, j=0;
      for (i = scm_to_int (line)+1; i > 0; i = i/10, j++)
	;
      indent (4-j, port);
    }

  if (scm_is_false (line))
    scm_puts ("   ?", port);
  else
    scm_intprint (scm_to_int (line) + 1, 10, port);
  scm_puts (": ", port);
}

static void
display_frame (SCM frame, int n, int nfield, int indentation,
               SCM sport, SCM port, scm_print_state *pstate)
{
  int i, j;

  /* display file name and line number */
  if (scm_is_true (SCM_PACK (SCM_SHOW_FILE_NAME)))
    display_backtrace_file_and_line (frame, port, pstate);

  /* Check size of frame number. */
  for (i = 0, j = n; j > 0; ++i) j /= 10;

  /* Number indentation. */
  indent (nfield - (i ? i : 1), port);

  /* Frame number. */
  scm_iprin1 (scm_from_int (n), port, pstate);

  /* Indentation. */
  indent (indentation, port);

  /* Display an application. */
  display_application (frame, nfield + 1 + indentation, sport, port, pstate);
  scm_putc ('\n', port);
}

struct display_backtrace_args {
  SCM stack;
  SCM port;
  SCM first;
  SCM depth;
  SCM highlight_objects;
};

static SCM
display_backtrace_body (struct display_backtrace_args *a)
#define FUNC_NAME "display_backtrace_body"
{
  int n_frames, beg, end, n, i, j;
  int nfield, indentation;
  SCM frame, sport, print_state;
  SCM last_file;
  scm_print_state *pstate;

  a->port = SCM_COERCE_OUTPORT (a->port);

  /* Argument checking and extraction. */
  SCM_VALIDATE_STACK (1, a->stack);
  SCM_VALIDATE_OPOUTPORT (2, a->port);
  n_frames = scm_to_int (scm_stack_length (a->stack));
  n = scm_is_integer (a->depth) ? scm_to_int (a->depth) : SCM_BACKTRACE_DEPTH;
  if (SCM_BACKWARDS_P)
    {
      beg = scm_is_integer (a->first) ? scm_to_int (a->first) : 0;
      end = beg + n - 1;
      if (end >= n_frames)
	end = n_frames - 1;
      n = end - beg + 1;
    }
  else
    {
      if (scm_is_integer (a->first))
	{
	  beg = scm_to_int (a->first);
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
  sport = scm_mkstrport (SCM_INUM0, SCM_BOOL_F,
			 SCM_OPN | SCM_WRTNG,
			 FUNC_NAME);

  /* Create a print state for printing of frames. */
  print_state = scm_make_print_state ();
  pstate = SCM_PRINT_STATE (print_state);
  pstate->writingp = 1;
  pstate->fancyp = 1;
  pstate->highlight_objects = a->highlight_objects;

  /* Determine size of frame number field. */
  j = end;
  for (i = 0; j > 0; ++i) j /= 10;
  nfield = i ? i : 1;
  
  /* Print frames. */
  indentation = 1;
  last_file = SCM_UNDEFINED;
  if (SCM_BACKWARDS_P)
    end++;
  else
    end--;
  for (i = beg; i != end; SCM_BACKWARDS_P ? ++i : --i)
    {
      frame = scm_stack_ref (a->stack, scm_from_int (i));
      if (!scm_is_eq (SCM_PACK (SCM_SHOW_FILE_NAME), sym_base))
	display_backtrace_file (frame, &last_file, a->port, pstate);
      display_frame (frame, i, nfield, indentation, sport, a->port, pstate);
    }

  scm_remember_upto_here_1 (print_state);
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
error_during_backtrace (void *data, SCM tag, SCM throw_args)
{
  SCM port = PTR2SCM (data);
  
  scm_puts ("Exception thrown while printing backtrace:\n", port);
  scm_print_exception (port, SCM_BOOL_F, tag, throw_args);

  return SCM_UNSPECIFIED;
}


SCM_DEFINE (scm_display_backtrace_with_highlights, "display-backtrace", 2, 3, 0, 
	    (SCM stack, SCM port, SCM first, SCM depth, SCM highlights),
	    "Display a backtrace to the output port @var{port}.  @var{stack}\n"
	    "is the stack to take the backtrace from, @var{first} specifies\n"
	    "where in the stack to start and @var{depth} how many frames\n"
	    "to display.  @var{first} and @var{depth} can be @code{#f},\n"
	    "which means that default values will be used.\n"
	    "If @var{highlights} is given it should be a list; the elements\n"
	    "of this list will be highlighted wherever they appear in the\n"
	    "backtrace.")
#define FUNC_NAME s_scm_display_backtrace_with_highlights
{
  struct display_backtrace_args a;
  a.stack = stack;
  a.port  = port;
  a.first = first;
  a.depth = depth;
  if (SCM_UNBNDP (highlights))
    a.highlight_objects = SCM_EOL;
  else
    a.highlight_objects = highlights;

  scm_internal_catch (SCM_BOOL_T,
		      (scm_t_catch_body) display_backtrace_body, &a,
		      (scm_t_catch_handler) error_during_backtrace, SCM2PTR (port));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_display_backtrace (SCM stack, SCM port, SCM first, SCM depth)
{
  return scm_display_backtrace_with_highlights (stack, port, first, depth,
						SCM_EOL);
}

SCM_VARIABLE (scm_has_shown_backtrace_hint_p_var, "has-shown-backtrace-hint?");

SCM_DEFINE (scm_backtrace_with_highlights, "backtrace", 0, 1, 0, 
	    (SCM highlights),
	    "Display a backtrace of the current stack to the current\n"
            "output port.  If @var{highlights} is given, it should be\n"
	    "a list; the elements of this list will be highlighted\n"
	    "wherever they appear in the backtrace.")
#define FUNC_NAME s_scm_backtrace_with_highlights
{
  SCM port = scm_current_output_port ();
  SCM stack = scm_make_stack (SCM_BOOL_T, SCM_EOL);
  
  if (SCM_UNBNDP (highlights))
    highlights = SCM_EOL;

  scm_newline (port);
  scm_puts ("Backtrace:\n", port);
  scm_display_backtrace_with_highlights (stack, port, SCM_BOOL_F, SCM_BOOL_F,
                                         highlights);
  scm_newline (port);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_backtrace (void)
{
  return scm_backtrace_with_highlights (SCM_EOL);
}



void
scm_init_backtrace ()
{
  scm_c_define_gsubr ("print-exception", 4, 0, 0, boot_print_exception);
#include "libguile/backtrace.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

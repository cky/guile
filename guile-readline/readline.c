/* readline.c --- line editing support for Guile */

/*	Copyright (C) 1997,1999, 2000 Free Software Foundation, Inc.
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
 */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include "libguile/_scm.h"
#ifdef HAVE_RL_GETC_FUNCTION
#include "libguile.h"
#include "libguile/gh.h"
#include "libguile/iselect.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <readline/readline.h>
#include <readline/history.h>
#include <sys/time.h>
#include <signal.h>

#include "libguile/validate.h"
#include "guile-readline/readline.h"

scm_option scm_readline_opts[] = {
  { SCM_OPTION_BOOLEAN, "history-file", 1,
    "Use history file." },
  { SCM_OPTION_INTEGER, "history-length", 200,
    "History length." },
  { SCM_OPTION_INTEGER, "bounce-parens", 500,
    "Time (ms) to show matching opening parenthesis (0 = off)."}
};

extern void stifle_history (int max);

SCM_DEFINE (scm_readline_options, "readline-options-interface", 0, 1, 0, 
            (SCM setting),
"")
#define FUNC_NAME s_scm_readline_options
{
  SCM ans = scm_options (setting,
			 scm_readline_opts,
			 SCM_N_READLINE_OPTIONS,
			 FUNC_NAME);
  stifle_history (SCM_HISTORY_LENGTH);
  return ans;
}
#undef FUNC_NAME

#ifndef HAVE_STRDUP
static char *
strdup (char *s)
{
  int len = strlen (s);
  char *new = malloc (len + 1);
  strcpy (new, s);
  return new;
}
#endif /* HAVE_STRDUP */

#ifndef HAVE_RL_CLEANUP_AFTER_SIGNAL

/* These are readline functions added in release 2.3.  They will work
 * together with readline-2.1 and 2.2.  (The readline interface is
 * disabled for earlier releases.)
 * They are declared static; if we want to use them elsewhere, then
 * we need external declarations for them, but at the moment, I don't
 * think anything else in Guile ought to use these.
 */

extern void _rl_clean_up_for_exit ();
extern void _rl_kill_kbd_macro ();
extern int _rl_init_argument ();

void
rl_cleanup_after_signal ()
{
#ifdef HAVE_RL_CLEAR_SIGNALS
  _rl_clean_up_for_exit ();
#endif
  (*rl_deprep_term_function) ();
#ifdef HAVE_RL_CLEAR_SIGNALS
  rl_clear_signals ();
#endif
  rl_pending_input = 0;
}

void
rl_free_line_state ()
{
  register HIST_ENTRY *entry;
   
  free_undo_list ();

  entry = current_history ();
  if (entry)
    entry->data = (char *)NULL; 
  
  _rl_kill_kbd_macro ();
  rl_clear_message ();
  _rl_init_argument ();
}

#endif /* !HAVE_RL_CLEANUP_AFTER_SIGNAL */

static int promptp;
static SCM input_port;
static SCM before_read;

static int
current_input_getc (FILE *in)
{
  if (promptp && !SCM_FALSEP (before_read))
    {
      scm_apply (before_read, SCM_EOL, SCM_EOL);
      promptp = 0;
    }
  return scm_getc (input_port);
}

static void
redisplay ()
{
  rl_redisplay ();
  /* promptp = 1; */
}

static int in_readline = 0;
#ifdef USE_THREADS
static scm_mutex_t reentry_barrier_mutex;
#endif

static SCM internal_readline (SCM text);
static SCM handle_error (void *data, SCM tag, SCM args);
static void reentry_barrier (void);


SCM_DEFINE (scm_readline, "%readline", 0, 4, 0, 
            (SCM text, SCM inp, SCM outp, SCM read_hook),
"")
#define FUNC_NAME s_scm_readline
{
  SCM ans;
  
  reentry_barrier ();
  
  before_read = SCM_BOOL_F;

  if (!SCM_UNBNDP (text))
    {
      if (!SCM_STRINGP (text))
	{
	  --in_readline;
	  scm_wrong_type_arg (s_scm_readline, SCM_ARG1, text);
	}
      SCM_STRING_COERCE_0TERMINATION_X (text);
    }
  
  if (!((SCM_UNBNDP (inp) && SCM_OPINFPORTP (scm_cur_inp))
	|| SCM_OPINFPORTP (inp)))
    {
      --in_readline;
      scm_misc_error (s_scm_readline,
		      "Input port is not open or not a file port",
		      SCM_EOL);
    }
  
  if (!((SCM_UNBNDP (outp) && SCM_OPINFPORTP (scm_cur_outp))
	|| SCM_OPOUTFPORTP (outp)))
    {
      --in_readline;
      scm_misc_error (s_scm_readline,
		      "Output port is not open or not a file port",
		      SCM_EOL);
    }

  if (!(SCM_UNBNDP (read_hook) || SCM_FALSEP (read_hook)))
    {
      if (!(SCM_NFALSEP (scm_thunk_p (read_hook))))
	{
	  --in_readline;
	  scm_wrong_type_arg (s_scm_readline, SCM_ARG4, read_hook);
	}
      before_read = read_hook;
    }

  scm_readline_init_ports (inp, outp);

  ans = scm_internal_catch (SCM_BOOL_T,
			    (scm_catch_body_t) internal_readline,
			    (void *) SCM_UNPACK (text),
			    handle_error, 0);

  fclose (rl_instream);
  fclose (rl_outstream);

  --in_readline;
  return ans;
}
#undef FUNC_NAME


static void
reentry_barrier ()
{
  int reentryp = 0;
#ifdef USE_THREADS
  /* We should rather use scm_mutex_try_lock when it becomes available */
  scm_mutex_lock (&reentry_barrier_mutex);
#endif
  if (in_readline)
    reentryp = 1;
  else
    ++in_readline;
#ifdef USE_THREADS
  scm_mutex_unlock (&reentry_barrier_mutex);
#endif
  if (reentryp)
    scm_misc_error (s_scm_readline, "readline is not reentrant", SCM_EOL);
}

static SCM
handle_error (void *data, SCM tag, SCM args)
{
  rl_free_line_state ();
  rl_cleanup_after_signal ();
  fputc ('\n', rl_outstream); /* We don't want next output on this line */
  fclose (rl_instream);
  fclose (rl_outstream);
  --in_readline;
  scm_handle_by_throw (data, tag, args);
  return SCM_UNSPECIFIED; /* never reached */
}

static SCM
internal_readline (SCM text)
{
  SCM ret;
  char *s;
  char *prompt = SCM_UNBNDP (text) ? "" : SCM_STRING_CHARS (text);

  promptp = 1;
  s = readline (prompt);
  if (s)
    ret = scm_makfrom0str (s);
  else 
    ret = SCM_EOF_VAL;

  free (s);

  return ret;
}

static FILE *
stream_from_fport (SCM port, char *mode, const char *subr)
{
  int fd;
  FILE *f;

  fd = dup (((struct scm_fport *) SCM_STREAM (port))->fdes);
  if (fd == -1)
    {
      --in_readline;
      scm_syserror (subr);
    }

  f = fdopen (fd, mode);
  if (f == NULL)
    {
      --in_readline;
      scm_syserror (subr);
    }

  return f;
}

void
scm_readline_init_ports (SCM inp, SCM outp)
{
  if (SCM_UNBNDP (inp))
    inp = scm_cur_inp;
  
  if (SCM_UNBNDP (outp))
    outp = scm_cur_outp;
  
  if (!SCM_OPINFPORTP (inp)) {
    scm_misc_error (0,
                    "Input port is not open or not a file port",
                    SCM_EOL);
  }

  if (!SCM_OPOUTFPORTP (outp)) {
    scm_misc_error (0,
                    "Output port is not open or not a file port",
                    SCM_EOL);
  }

  input_port = inp;
  rl_instream = stream_from_fport (inp, "r", s_scm_readline);
  rl_outstream = stream_from_fport (outp, "w", s_scm_readline);
}



SCM_DEFINE (scm_add_history, "add-history", 1, 0, 0, 
            (SCM text),
"")
#define FUNC_NAME s_scm_add_history
{
  char* s;
  SCM_VALIDATE_STRING (1,text);
  SCM_STRING_COERCE_0TERMINATION_X (text);

  s = SCM_STRING_CHARS (text);
  add_history (strdup (s));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_read_history, "read-history", 1, 0, 0, 
            (SCM file),
"")
#define FUNC_NAME s_scm_read_history
{
  SCM_VALIDATE_STRING (1,file);
  return SCM_NEGATE_BOOL (read_history (SCM_STRING_CHARS (file)));
}
#undef FUNC_NAME


SCM_DEFINE (scm_write_history, "write-history", 1, 0, 0, 
            (SCM file),
"")
#define FUNC_NAME s_scm_write_history
{
  SCM_VALIDATE_STRING (1,file);
  return SCM_NEGATE_BOOL (write_history (SCM_STRING_CHARS (file)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_clear_history, "clear-history", 0, 0, 0, 
            (),
	    "Clear the history buffer of the readline machinery.")
#define FUNC_NAME s_scm_clear_history
{
  clear_history();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_filename_completion_function, "filename-completion-function", 2, 0, 0, 
            (SCM text, SCM continuep),
"")
#define FUNC_NAME s_scm_filename_completion_function
{
  char *s;
  SCM ans;
  SCM_VALIDATE_STRING (1,text);
  SCM_STRING_COERCE_0TERMINATION_X (text);
  s = filename_completion_function (SCM_STRING_CHARS (text), SCM_NFALSEP (continuep));
  ans = scm_makfrom0str (s);
  free (s);
  return ans;
}
#undef FUNC_NAME

/*
 * The following has been modified from code contributed by
 * Andrew Archibald <aarchiba@undergrad.math.uwaterloo.ca>
 */

SCM scm_readline_completion_function_var;

static char *
completion_function (char *text, int continuep)
{
  SCM compfunc = SCM_VARIABLE_REF (scm_readline_completion_function_var);
  SCM res;

  if (SCM_FALSEP (compfunc))
    return NULL; /* #f => completion disabled */
  else
    {
      SCM t = scm_makfrom0str (text);
      SCM c = continuep ? SCM_BOOL_T : SCM_BOOL_F;
      res = scm_apply (compfunc, SCM_LIST2 (t, c), SCM_EOL);
  
      if (SCM_FALSEP (res))
	return NULL;
  
      if (!SCM_STRINGP (res))
	scm_misc_error (s_scm_readline,
			"Completion function returned bogus value: %S",
			SCM_LIST1 (res));
      SCM_STRING_COERCE_0TERMINATION_X (res);
      return strdup (SCM_STRING_CHARS (res));
    }
}

/*Bouncing parenthesis (reimplemented by GH, 11/23/98, since readline is strict gpl)*/

static int match_paren (int x, int k);
static int find_matching_paren (int k);
static void init_bouncing_parens ();

static void
init_bouncing_parens ()
{
  if (strncmp (rl_get_keymap_name (rl_get_keymap ()), "vi", 2))
    {
      rl_bind_key (')', match_paren);
      rl_bind_key (']', match_paren);
      rl_bind_key ('}', match_paren);
    }
}

static int
find_matching_paren(int k)
{
  register int i;
  register char c = 0;
  int end_parens_found = 0;

  /* Choose the corresponding opening bracket.  */
  if (k == ')') c = '(';
  else if (k == ']') c = '[';
  else if (k == '}') c = '{';

  for (i=rl_point-2; i>=0; i--)
    {
      /* Is the current character part of a character literal?  */
      if (i - 2 >= 0
	  && rl_line_buffer[i - 1] == '\\'
	  && rl_line_buffer[i - 2] == '#')
	;
      else if (rl_line_buffer[i] == k)
	end_parens_found++;
      else if (rl_line_buffer[i] == '"')
	{
	  /* Skip over a string literal.  */
	  for (i--; i >= 0; i--)
	    if (rl_line_buffer[i] == '"'
		&& ! (i - 1 >= 0
		      && rl_line_buffer[i - 1] == '\\'))
	      break;
	}
      else if (rl_line_buffer[i] == c)
	{
	  if (end_parens_found==0)
	    return i;
	  else --end_parens_found;
	}
    }
  return -1;
}

static int
match_paren (int x, int k)
{
  int tmp, fno;
  SELECT_TYPE readset;
  struct timeval timeout;
  
  rl_insert (x, k);
  if (!SCM_READLINE_BOUNCE_PARENS)
    return 0;

  /* Did we just insert a quoted paren?  If so, then don't bounce.  */
  if (rl_point - 1 >= 1
      && rl_line_buffer[rl_point - 2] == '\\')
    return 0;

  tmp = 1000 * SCM_READLINE_BOUNCE_PARENS;
  timeout.tv_sec = tmp / 1000000;
  timeout.tv_usec = tmp % 1000000;
  FD_ZERO (&readset);
  fno = fileno (rl_instream);
  FD_SET (fno, &readset);
  
  if (rl_point > 1)
    {
      tmp = rl_point;
      rl_point = find_matching_paren (k);
      if (rl_point > -1)
	{
	  rl_redisplay ();
	  scm_internal_select (fno + 1, &readset, NULL, NULL, &timeout);
	}
      rl_point = tmp;
    }
  return 0;
}

#if defined (HAVE_RL_PRE_INPUT_HOOK) && defined (GUILE_SIGWINCH_SA_RESTART_CLEARED)
/* Readline disables SA_RESTART on SIGWINCH.
 * This code turns it back on.
 */
static int
sigwinch_enable_restart (void)
{
#ifdef HAVE_SIGINTERRUPT
  siginterrupt (SIGWINCH, 0);
#else
  struct sigaction action;
  
  sigaction (SIGWINCH, NULL, &action);
  action.sa_flags |= SA_RESTART;
  sigaction (SIGWINCH, &action, NULL);
#endif
  return 0;
}
#endif

#endif /* HAVE_RL_GETC_FUNCTION */

void
scm_init_readline ()
{
#ifdef HAVE_RL_GETC_FUNCTION
#include "guile-readline/readline.x"
  scm_readline_completion_function_var
    = scm_c_define ("*readline-completion-function*", SCM_BOOL_F);
  rl_getc_function = current_input_getc;
  rl_redisplay_function = redisplay;
  rl_completion_entry_function = (Function*) completion_function;
  rl_basic_word_break_characters = "\t\n\"'`;()";
  rl_readline_name = "Guile";
#if defined (HAVE_RL_PRE_INPUT_HOOK) && defined (GUILE_SIGWINCH_SA_RESTART_CLEARED)
  rl_pre_input_hook = sigwinch_enable_restart;
#endif

#ifdef USE_THREADS
  scm_mutex_init (&reentry_barrier_mutex);
#endif
  scm_init_opts (scm_readline_options,
		 scm_readline_opts,
		 SCM_N_READLINE_OPTIONS);
  init_bouncing_parens();
  scm_add_feature ("readline");
#endif /* HAVE_RL_GETC_FUNCTION */
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

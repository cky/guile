/* readline.c --- line editing support for Guile */

/*	Copyright (C) 1997,1999 Free Software Foundation, Inc.
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


#include "libguile/_scm.h"
#if defined (HAVE_RL_GETC_FUNCTION)
#include "libguile/libguile.h"
#include "libguile/gh.h"
#include "libguile/iselect.h"

#include <readline/readline.h>
#include <readline/history.h>
#include <sys/time.h>

#include "readline.h"

scm_option scm_readline_opts[] = {
  { SCM_OPTION_BOOLEAN, "history-file", 1,
    "Use history file." },
  { SCM_OPTION_INTEGER, "history-length", 200,
    "History length." },
  { SCM_OPTION_INTEGER, "bounce-parens", 500,
    "Time (ms) to show matching opening parenthesis (0 = off)."}
};

extern void stifle_history (int max);

SCM_PROC (s_readline_options, "readline-options-interface", 0, 1, 0, scm_readline_options);

SCM
scm_readline_options (setting)
     SCM setting;
{
  SCM ans = scm_options (setting,
			 scm_readline_opts,
			 SCM_N_READLINE_OPTIONS,
			 s_readline_options);
  stifle_history (SCM_HISTORY_LENGTH);
  return ans;
}

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
  SCM ans;
  if (promptp && SCM_NIMP (before_read))
    {
      scm_apply (before_read, SCM_EOL, SCM_EOL);
      promptp = 0;
    }
  ans = scm_getc (input_port);
  return ans;
}

static void
redisplay ()
{
  rl_redisplay ();
  /* promptp = 1; */
}

SCM_PROC (s_readline, "%readline", 0, 4, 0, scm_readline);

static int in_readline = 0;
#ifdef USE_THREADS
static scm_mutex_t reentry_barrier_mutex;
#endif

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
    scm_misc_error (s_readline, "readline is not reentrant", SCM_EOL);
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
  char *prompt = SCM_UNBNDP (text) ? "" : SCM_CHARS (text);

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
  
  if (!(SCM_NIMP (inp) && SCM_OPINFPORTP (inp))) {
    scm_misc_error (0,
                    "Input port is not open or not a file port",
                    SCM_EOL);
  }

  if (!(SCM_NIMP (outp) && SCM_OPOUTFPORTP (outp))) {
    scm_misc_error (0,
                    "Output port is not open or not a file port",
                    SCM_EOL);
  }

  input_port = inp;
  rl_instream = stream_from_fport (inp, "r", s_readline);
  rl_outstream = stream_from_fport (outp, "w", s_readline);
}

SCM
scm_readline (SCM text, SCM inp, SCM outp, SCM read_hook)
{
  SCM ans;
  
  reentry_barrier ();
  
  before_read = SCM_BOOL_F;

  if (!SCM_UNBNDP (text))
    {
      if (!(SCM_NIMP (text) && SCM_STRINGP (text)))
	{
	  --in_readline;
	  scm_wrong_type_arg (s_readline, SCM_ARG1, text);
	}
      SCM_COERCE_SUBSTR (text);
    }
  
  if (!((SCM_UNBNDP (inp) && SCM_NIMP (scm_cur_inp) && SCM_OPINFPORTP (inp))
	|| SCM_NIMP (inp) && SCM_OPINFPORTP (inp)))
    {
      --in_readline;
      scm_misc_error (s_readline,
		      "Input port is not open or not a file port",
		      SCM_EOL);
    }
  
  if (!((SCM_UNBNDP (outp) && SCM_NIMP (scm_cur_outp) && SCM_OPINFPORTP (outp))
	|| (SCM_NIMP (outp) && SCM_OPOUTFPORTP (outp))))
    {
      --in_readline;
      scm_misc_error (s_readline,
		      "Output port is not open or not a file port",
		      SCM_EOL);
    }

  if (!(SCM_UNBNDP (read_hook) || SCM_FALSEP (read_hook)))
    {
      if (!(SCM_NFALSEP (scm_thunk_p (read_hook))))
	{
	  --in_readline;
	  scm_wrong_type_arg (s_readline, SCM_ARG4, read_hook);
	}
      before_read = read_hook;
    }

  scm_readline_init_ports (inp, outp);

  ans = scm_internal_catch (SCM_BOOL_T,
			    (scm_catch_body_t) internal_readline,
			    (void *) text,
			    handle_error, 0);

  fclose (rl_instream);
  fclose (rl_outstream);

  --in_readline;
  return ans;
}

SCM_PROC (s_add_history, "add-history", 1, 0, 0, scm_add_history);

SCM
scm_add_history (SCM text)
{
  char* s;
  SCM_ASSERT ((SCM_NIMP(text) && SCM_STRINGP(text)), text, SCM_ARG1,
	      s_add_history);
  SCM_COERCE_SUBSTR (text);

  s = SCM_CHARS (text);
  add_history (strdup (s));

  return SCM_UNSPECIFIED;
}


SCM_PROC (s_read_history, "read-history", 1, 0, 0, scm_read_history);

SCM
scm_read_history (SCM file)
{
  SCM_ASSERT (SCM_NIMP (file) && SCM_STRINGP (file),
	      file, SCM_ARG1, s_read_history);
  return read_history (SCM_ROCHARS (file)) ? SCM_BOOL_F : SCM_BOOL_T;
}


SCM_PROC (s_write_history, "write-history", 1, 0, 0, scm_write_history);

SCM
scm_write_history (SCM file)
{
  SCM_ASSERT (SCM_NIMP (file) && SCM_STRINGP (file),
	      file, SCM_ARG1, s_write_history);
  return write_history (SCM_ROCHARS (file)) ? SCM_BOOL_F : SCM_BOOL_T;
}


SCM_PROC (s_filename_completion_function, "filename-completion-function", 2, 0, 0, scm_filename_completion_function);

SCM
scm_filename_completion_function (SCM text, SCM continuep)
{
  char *s;
  SCM ans;
  SCM_ASSERT (SCM_NIMP (text) && SCM_STRINGP (text),
	      text,
	      SCM_ARG1,
	      s_filename_completion_function);
  SCM_COERCE_SUBSTR (text);
  s = filename_completion_function (SCM_CHARS (text), SCM_NFALSEP (continuep));
  ans = scm_makfrom0str (s);
  free (s);
  return ans;
}

/*
 * The following has been modified from code contributed by
 * Andrew Archibald <aarchiba@undergrad.math.uwaterloo.ca>
 */

SCM scm_readline_completion_function_var;

static char *
completion_function (char *text, int continuep)
{
  SCM compfunc = SCM_CDR (scm_readline_completion_function_var);
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
  
      if (!(SCM_NIMP (res) && SCM_STRINGP (res)))
	scm_misc_error (s_readline,
			"Completion function returned bogus value: %S",
			SCM_LIST1 (res));
      SCM_COERCE_SUBSTR (res);
      return strdup (SCM_CHARS (res));
    }
}

/*Bouncing parenthesis (reimplemented by GH, 11/23/98, since readline is strict gpl)*/

static void match_paren(int x, int k);
static int find_matching_paren(int k);
static void init_bouncing_parens();

static void
init_bouncing_parens()
{
  if(strncmp(rl_get_keymap_name(rl_get_keymap()), "vi", 2)) {
    rl_bind_key(')', match_paren);
    rl_bind_key(']', match_paren);
    rl_bind_key('}', match_paren);
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
	  if (end_parens_found==0) return i;
	  else --end_parens_found;
	}
    }
  return -1;
}

static void
match_paren(int x, int k)
{
  int tmp;
  fd_set readset;
  struct timeval timeout;
  
  rl_insert(x, k);
  if (!SCM_READLINE_BOUNCE_PARENS)
    return;

  /* Did we just insert a quoted paren?  If so, then don't bounce.  */
  if (rl_point - 1 >= 1
      && rl_line_buffer[rl_point - 2] == '\\')
    return;

  tmp = 1000 * SCM_READLINE_BOUNCE_PARENS;
  timeout.tv_sec = tmp / 1000000;
  timeout.tv_usec = tmp % 1000000;
  FD_ZERO(&readset);
  FD_SET(fileno(rl_instream), &readset);
  
  if(rl_point > 1) {
    tmp = rl_point;
    rl_point = find_matching_paren(k);
    if(rl_point > -1) {
      rl_redisplay();
      scm_internal_select(1, &readset, NULL, NULL, &timeout);
    }
    rl_point = tmp;
  }
}


void
scm_init_readline ()
{
#include "readline.x"
  scm_readline_completion_function_var
    = scm_sysintern ("*readline-completion-function*", SCM_BOOL_F);
  rl_getc_function = current_input_getc;
  rl_redisplay_function = redisplay;
  rl_completion_entry_function = (Function*) completion_function;
  rl_basic_word_break_characters = "\t\n\"'`;()";
#ifdef USE_THREADS
  scm_mutex_init (&reentry_barrier_mutex);
#endif
  scm_init_opts (scm_readline_options,
		 scm_readline_opts,
		 SCM_N_READLINE_OPTIONS);
  init_bouncing_parens();
  scm_add_feature ("readline");
}

#endif 

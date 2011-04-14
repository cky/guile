/* Copyright (C) 1994-1998, 2000-2011 Free Software Foundation, Inc.
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

/* "script.c" argv tricks for `#!' scripts.
   Authors: Aubrey Jaffer and Jim Blandy */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>

#include <version-etc.h>

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/feature.h"
#include "libguile/load.h"
#include "libguile/private-gc.h" /* scm_getenv_int */
#include "libguile/read.h"
#include "libguile/script.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/validate.h"
#include "libguile/version.h"
#include "libguile/vm.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>		/* for X_OK define */
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif

/* Concatentate str2 onto str1 at position n and return concatenated
   string if file exists; 0 otherwise. */

static char *
scm_cat_path (char *str1, const char *str2, long n)
{
  if (!n)
    n = strlen (str2);
  if (str1)
    {
      size_t len = strlen (str1);
      str1 = (char *) realloc (str1, (size_t) (len + n + 1));
      if (!str1)
	return 0L;
      strncat (str1 + len, str2, n);
      return str1;
    }
  str1 = (char *) scm_malloc ((size_t) (n + 1));
  if (!str1)
    return 0L;
  str1[0] = 0;
  strncat (str1, str2, n);
  return str1;
}

#if 0 
static char *
scm_try_path (char *path)
{
  FILE *f;
  /* fprintf(stderr, "Trying %s\n", path);fflush(stderr); */
  if (!path)
    return 0L;
  SCM_SYSCALL (f = fopen (path, "r");
    );
  if (f)
    {
      fclose (f);
      return path;
    }
  free (path);
  return 0L;
}

static char *
scm_sep_init_try (char *path, const char *sep, const char *initname)
{
  if (path)
    path = scm_cat_path (path, sep, 0L);
  if (path)
    path = scm_cat_path (path, initname, 0L);
  return scm_try_path (path);
}
#endif 

#ifndef LINE_INCREMENTORS
#define LINE_INCREMENTORS  '\n'
#ifdef MSDOS
#define WHITE_SPACES  ' ':case '\t':case '\r':case '\f':case 26
#else
#define WHITE_SPACES  ' ':case '\t':case '\r':case '\f'
#endif /* def MSDOS */
#endif /* ndef LINE_INCREMENTORS */

#ifndef MAXPATHLEN
#define MAXPATHLEN 80
#endif /* ndef MAXPATHLEN */
#ifndef X_OK
#define X_OK 1
#endif /* ndef X_OK */

char *
scm_find_executable (const char *name)
{
  char tbuf[MAXPATHLEN];
  int i = 0, c;
  FILE *f;

  /* fprintf(stderr, "s_f_e checking access %s ->%d\n", name, access(name, X_OK)); fflush(stderr); */
  if (access (name, X_OK))
    return 0L;
  f = fopen (name, "r");
  if (!f)
    return 0L;
  if ((fgetc (f) == '#') && (fgetc (f) == '!'))
    {
      while (1)
	switch (c = fgetc (f))
	  {
	  case /*WHITE_SPACES */ ' ':
	  case '\t':
	  case '\r':
	  case '\f':
	  case EOF:
	    tbuf[i] = 0;
	    fclose (f);
	    return scm_cat_path (0L, tbuf, 0L);
	  default:
	    tbuf[i++] = c;
	    break;
	  }
    }
  fclose (f);
  return scm_cat_path (0L, name, 0L);
}


/* Read a \nnn-style escape.  We've just read the backslash.  */
static int
script_get_octal (FILE *f)
#define FUNC_NAME "script_get_octal"
{
  int i;
  int value = 0;

  for (i = 0; i < 3; i++)
    {
      int c = getc (f);
      if ('0' <= c && c <= '7')
	value = (value * 8) + (c - '0');
      else
	SCM_MISC_ERROR ("malformed script: bad octal backslash escape",
			SCM_EOL);
    }
  return value;
}
#undef FUNC_NAME


static int
script_get_backslash (FILE *f)
#define FUNC_NAME "script_get_backslash"
{
  int c = getc (f);

  switch (c)
    {
    case 'a': return '\a';
    case 'b': return '\b';
    case 'f': return '\f';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    case 'v': return '\v';

    case '\\':
    case ' ':
    case '\t':
    case '\n':
      return c;

    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
      ungetc (c, f);
      return script_get_octal (f);

    case EOF:
      SCM_MISC_ERROR ("malformed script: backslash followed by EOF", SCM_EOL);
      return 0; /* not reached? */

    default:
      SCM_MISC_ERROR ("malformed script: bad backslash sequence", SCM_EOL);
      return 0; /* not reached? */
    }
}
#undef FUNC_NAME


static char *
script_read_arg (FILE *f)
#define FUNC_NAME "script_read_arg"
{
  size_t size = 7;
  char *buf = scm_malloc (size + 1);
  size_t len = 0;

  if (! buf)
    return 0;

  for (;;)
    {
      int c = getc (f);
      switch (c)
	{
	case '\\':
	  c = script_get_backslash (f);
	  /* The above produces a new character to add to the argument.
             Fall through.  */
	default:
	  if (len >= size)
	    {
	      size = (size + 1) * 2;
	      buf = realloc (buf, size);
	      if (! buf)
		return 0;
	    }
	  buf[len++] = c;
	  break;

	case '\n':
	  /* This may terminate an arg now, but it will terminate the
             entire list next time through.  */
	  ungetc ('\n', f);
	case EOF:
	  if (len == 0)
	    {
	      free (buf);
	      return 0;
	    }
	  /* Otherwise, those characters terminate the argument; fall
             through.  */
	case ' ':
	  buf[len] = '\0';
	  return buf;

	case '\t':
	  free (buf);
	  SCM_MISC_ERROR ("malformed script: TAB in meta-arguments", SCM_EOL);
	  return 0; /* not reached? */
	}
    }
}
#undef FUNC_NAME


static int
script_meta_arg_P (char *arg)
{
  if ('\\' != arg[0])
    return 0L;
#ifdef MSDOS
  return !arg[1];
#else
  switch (arg[1])
    {
    case 0:
    case '%':
    case WHITE_SPACES:
      return !0;
    default:
      return 0L;
    }
#endif
}

char **
scm_get_meta_args (int argc, char **argv)
{
  int nargc = argc, argi = 1, nargi = 1;
  char *narg, **nargv;
  if (!(argc > 2 && script_meta_arg_P (argv[1])))
    return 0L;
  if (!(nargv = (char **) scm_malloc ((1 + nargc) * sizeof (char *))))
      return 0L;
  nargv[0] = argv[0];
  while (((argi + 1) < argc) && (script_meta_arg_P (argv[argi])))
    {
      FILE *f = fopen (argv[++argi], "r");
      if (f)
	{
	  nargc--;		/* to compensate for replacement of '\\' */
	  while (1)
	    switch (getc (f))
	      {
	      case EOF:
		return 0L;
	      default:
		continue;
	      case '\n':
		goto found_args;
	      }
	found_args:
	  while ((narg = script_read_arg (f)))
	    if (!(nargv = (char **) realloc (nargv,
					     (1 + ++nargc) * sizeof (char *))))
	        return 0L;
	    else
	      nargv[nargi++] = narg;
	  fclose (f);
	  nargv[nargi++] = argv[argi++];
	}
    }
  while (argi <= argc)
    nargv[nargi++] = argv[argi++];
  return nargv;
}

int
scm_count_argv (char **argv)
{
  int argc = 0;
  while (argv[argc])
    argc++;
  return argc;
}


/* For use in error messages.  */
char *scm_usage_name = 0;

void
scm_shell_usage (int fatal, char *message)
{
  scm_call_3 (scm_c_private_ref ("ice-9 command-line",
                                 "shell-usage"),
              (scm_usage_name
               ? scm_from_locale_string (scm_usage_name)
               : scm_from_latin1_string ("guile")),
              scm_from_bool (fatal),
              (message
               ? scm_from_locale_string (message)
               : SCM_BOOL_F));
}


/* Given an array of command-line switches, return a Scheme expression
   to carry out the actions specified by the switches.
 */

SCM
scm_compile_shell_switches (int argc, char **argv)
{
  return scm_call_2 (scm_c_public_ref ("ice-9 command-line",
                                       "compile-shell-switches"),
                     scm_makfromstrs (argc, argv),
                     (scm_usage_name
                      ? scm_from_locale_string (scm_usage_name)
                      : scm_from_latin1_string ("guile")));
}


void
scm_shell (int argc, char **argv)
{
  /* If present, add SCSH-style meta-arguments from the top of the
     script file to the argument vector.  See the SCSH manual: "The
     meta argument" for more details.  */
  {
    char **new_argv = scm_get_meta_args (argc, argv);

    if (new_argv)
      {
	argv = new_argv;
	argc = scm_count_argv (new_argv);
      }
  }

  exit (scm_exit_status (scm_eval_x (scm_compile_shell_switches (argc, argv),
				     scm_current_module ())));
}


void
scm_init_script ()
{
#include "libguile/script.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

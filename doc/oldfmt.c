/* Copyright (C) 2000,2001 Free Software Foundation, Inc.
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
 */



/* From NEWS:
 * 
 * * New primitive: `simple-format', affects `scm-error', scm_display_error, & scm_error message strings
 * 
 * (ice-9 boot) makes `format' an alias for `simple-format' until possibly
 * extended by the more sophisticated version in (ice-9 format)
 * 
 * (simple-format port message . args)
 * Write MESSAGE to DESTINATION, defaulting to `current-output-port'.
 * MESSAGE can contain ~A (was %s) and ~S (was %S) escapes.  When printed,
 * the escapes are replaced with corresponding members of ARGS:
 * ~A formats using `display' and ~S formats using `write'.
 * If DESTINATION is #t, then use the `current-output-port',
 * if DESTINATION is #f, then return a string containing the formatted text.
 * Does not add a trailing newline."
 * 
 * The two C procedures: scm_display_error and scm_error, as well as the
 * primitive `scm-error', now use scm_format to do their work.  This means
 * that the message strings of all code must be updated to use ~A where %s
 * was used before, and ~S where %S was used before.
 * 
 * During the period when there still are a lot of old Guiles out there,
 * you might want to support both old and new versions of Guile.
 * 
 * There are basically two methods to achieve this.  Both methods use
 * autoconf.  Put
 * 
 *   AC_CHECK_FUNCS(scm_simple_format)
 * 
 * in your configure.in.
 * 
 * Method 1: Use the string concatenation features of ANSI C's
 *           preprocessor.
 * 
 * In C:
 * 
 * #ifdef HAVE_SCM_SIMPLE_FORMAT
 * #define FMT_S "~S"
 * #else
 * #define FMT_S "%S"
 * #endif
 * 
 * Then represent each of your error messages using a preprocessor macro:
 * 
 * #define E_SPIDER_ERROR "There's a spider in your " ## FMT_S ## "!!!"
 * 
 * In Scheme:
 * 
 * (define fmt-s (if (defined? 'simple-format) "~S" "%S"))
 * (define make-message string-append)
 * 
 * (define e-spider-error
 *         (make-message "There's a spider in your " fmt-s "!!!"))
 * 
 * Method 2: Use the oldfmt function found in doc/oldfmt.c.
 * 
 * In C:
 * 
 * scm_misc_error ("picnic", scm_c_oldfmt0 ("There's a spider in your ~S!!!"),
 *                 ...);
 * 
 * In Scheme:
 * 
 * (scm-error 'misc-error "picnic" (oldfmt "There's a spider in your ~S!!!")
 *            ...)
 * 
 */

/* 
 * Take a format string FROM adhering to the new standard format (~A and ~S
 * as placeholders) of length N and return a string which is adapted
 * to the format used by the Guile interpreter which you are running.
 *
 * On successive calls with similar strings but different storage, the
 * same string with same storage is returned.  This is necessary since
 * the existence of a garbage collector in the system may cause the same
 * format string to be represented with different storage at different
 * calls.
 */

char *
scm_c_oldfmt (char *from, int n)
{
#ifdef HAVE_SCM_SIMPLE_FORMAT
  return from;
#else
  static struct { int n; char *from; char *to; } *strings;
  static int size = 0;
  static int n_strings = 0;
  char *to;
  int i;
  
  for (i = 0; i < n_strings; ++i)
    if (n == strings[i].n && strncmp (from, strings[i].from, n) == 0)
      return strings[i].to;
  
  if (n_strings == size)
    {
      if (size == 0)
	{
	  size = 10;
	  strings = scm_must_malloc (size * sizeof (*strings), s_oldfmt);
	}
      else
	{
	  int oldsize = size;
	  size = 3 * oldsize / 2;
	  strings = scm_must_realloc (strings,
				      oldsize * sizeof (*strings),
				      size * sizeof (*strings),
				      s_oldfmt);
	}
    }

  strings[n_strings].n = n;
  strings[n_strings].from = strncpy (scm_must_malloc (n, s_oldfmt), from, n);
  to = strings[n_strings].to = scm_must_malloc (n + 1, s_oldfmt);
  n_strings++;

  for (i = 0; i < n; ++i)
    {
      if (from[i] == '~' && ++i < n)
	{
	  if (from[i] == 'A')
	    {
	      to[i - 1] = '%';
	      to[i] = 's';
	    }
	  else if (from[i] == 'S')
	    {
	      to[i - 1] = '%';
	      to[i] = 'S';
	    }
	  else
	    {
	      to[i - 1] = '~';
	      to[i] = from[i];
	    }
	  continue;
	}
      to[i] = from[i];
    }
  to[i] = '\0';
  
  return to;
#endif
}

char *
scm_c_oldfmt0 (char *s)
{
#ifdef HAVE_SCM_SIMPLE_FORMAT
  return s;
#else
  return scm_c_oldfmt (s, strlen (s));
#endif
}

SCM_PROC (s_oldfmt, "oldfmt", 1, 0, 0, scm_oldfmt);

SCM
scm_oldfmt (SCM s)
{
#ifdef HAVE_SCM_SIMPLE_FORMAT
  return s;
#else
  int n;
  SCM_ASSERT (SCM_NIMP (s) && SCM_STRINGP (s), s, 1, s_oldfmt);
  n = SCM_LENGTH (s);
  return scm_return_first (scm_mem2string (scm_c_oldfmt (SCM_ROCHARS (s), n),
					   n),
			   s);
#endif
}

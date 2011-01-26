/* Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2006,
 *   2011 Free Software Foundation, Inc.
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

#include "libguile/_scm.h"

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libguile/chars.h"
#include "libguile/modules.h"
#include "libguile/ports.h"
#include "libguile/rdelim.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/validate.h"

SCM_DEFINE (scm_read_delimited_x, "%read-delimited!", 3, 3, 0,
            (SCM delims, SCM str, SCM gobble, SCM port, SCM start, SCM end),
	    "Read characters from @var{port} into @var{str} until one of the\n"
	    "characters in the @var{delims} string is encountered.  If\n"
	    "@var{gobble} is true, discard the delimiter character;\n"
	    "otherwise, leave it in the input stream for the next read.  If\n"
	    "@var{port} is not specified, use the value of\n"
	    "@code{(current-input-port)}.  If @var{start} or @var{end} are\n"
	    "specified, store data only into the substring of @var{str}\n"
	    "bounded by @var{start} and @var{end} (which default to the\n"
	    "beginning and end of the string, respectively).\n"
	    "\n"
	    " Return a pair consisting of the delimiter that terminated the\n"
	    "string and the number of characters read.  If reading stopped\n"
	    "at the end of file, the delimiter returned is the\n"
	    "@var{eof-object}; if the string was filled without encountering\n"
	    "a delimiter, this value is @code{#f}.")
#define FUNC_NAME s_scm_read_delimited_x
{
  size_t j;
  size_t cstart;
  size_t cend;
  scm_t_wchar c;
  size_t num_delims;

  SCM_VALIDATE_STRING (1, delims);
  num_delims = scm_i_string_length (delims);

  SCM_VALIDATE_STRING (2, str);
  scm_i_get_substring_spec (scm_i_string_length (str),
			    start, &cstart, end, &cend);

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  else
    SCM_VALIDATE_OPINPORT (4, port);

  for (j = cstart; j < cend; j++)
    {  
      size_t k;

      c = scm_getc (port);
      for (k = 0; k < num_delims; k++)
	{
	  if (scm_i_string_ref (delims, k) == c)
	    {
	      if (scm_is_false (gobble))
		scm_ungetc (c, port);

	      return scm_cons (SCM_MAKE_CHAR (c),
			       scm_from_size_t (j - cstart));
	    }
	}
      if (c == EOF)
	return scm_cons (SCM_EOF_VAL, 
			 scm_from_size_t (j - cstart));

      scm_c_string_set_x (str, j, SCM_MAKE_CHAR (c));
    }
  return scm_cons (SCM_BOOL_F, scm_from_size_t (j - cstart));
}
#undef FUNC_NAME


/*
 * %read-line 
 * truncates any terminating newline from its input, and returns
 * a cons of the string read and its terminating character.  Doing
 * so makes it easy to implement the hairy `read-line' options
 * efficiently in Scheme.
 */

SCM_DEFINE (scm_read_line, "%read-line", 0, 1, 0, 
            (SCM port),
	    "Read a newline-terminated line from @var{port}, allocating storage as\n"
	    "necessary.  The newline terminator (if any) is removed from the string,\n"
	    "and a pair consisting of the line and its delimiter is returned.  The\n"
	    "delimiter may be either a newline or the @var{eof-object}; if\n"
	    "@code{%read-line} is called at the end of file, it returns the pair\n"
	    "@code{(#<eof> . #<eof>)}.")
#define FUNC_NAME s_scm_read_line
{
/* Threshold under which the only allocation performed is that of the
   resulting string and pair.  */
#define LINE_BUFFER_SIZE 256

  SCM line, strings, result;
  scm_t_wchar buf[LINE_BUFFER_SIZE], delim;
  size_t index;

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();

  SCM_VALIDATE_OPINPORT (1,port);

  index = 0;
  delim = 0;
  strings = SCM_BOOL_F;

  do
    {
      if (SCM_UNLIKELY (index >= LINE_BUFFER_SIZE))
	{
	  /* The line is getting longer than BUF so store its current
	     contents in STRINGS.  */
	  strings = scm_cons (scm_from_utf32_stringn (buf, index),
			      scm_is_false (strings) ? SCM_EOL : strings);
	  index = 0;
	}
      else
	{
	  buf[index] = scm_getc (port);
	  switch (buf[index])
	    {
	    case EOF:
	    case '\n':
	      delim = buf[index];
	      break;

	    default:
	      index++;
	    }
	}
    }
  while (delim == 0);

  if (SCM_LIKELY (scm_is_false (strings)))
    /* The fast path.  */
    line = scm_from_utf32_stringn (buf, index);
  else
    {
      /* Aggregate the intermediary results.  */
      strings = scm_cons (scm_from_utf32_stringn (buf, index), strings);
      line = scm_string_concatenate (scm_reverse (strings));
    }

  if (delim == EOF && scm_i_string_length (line) == 0)
    result = scm_cons (SCM_EOF_VAL, SCM_EOF_VAL);
  else
    result = scm_cons (line,
		       delim == EOF ? SCM_EOF_VAL : SCM_MAKE_CHAR (delim));

  return result;
#undef LINE_BUFFER_SIZE
}
#undef FUNC_NAME

SCM_DEFINE (scm_write_line, "write-line", 1, 1, 0,
            (SCM obj, SCM port),
	    "Display @var{obj} and a newline character to @var{port}.  If\n"
	    "@var{port} is not specified, @code{(current-output-port)} is\n"
	    "used.  This function is equivalent to:\n"
	    "@lisp\n"
	    "(display obj [port])\n"
	    "(newline [port])\n"
	    "@end lisp")
#define FUNC_NAME s_scm_write_line
{
  scm_display (obj, port);
  return scm_newline (port);
}
#undef FUNC_NAME

SCM
scm_init_rdelim_builtins (void)
{
#include "libguile/rdelim.x"

  return SCM_UNSPECIFIED;
}

void
scm_init_rdelim (void)
{
  scm_c_define_gsubr ("%init-rdelim-builtins", 0, 0, 0,
		      scm_init_rdelim_builtins);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* 	Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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

 * This is the C part of the module for delimited I/O.  It's
 * similar to (scsh rdelim) but somewhat incompatible.  */

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
  long j;
  char *buf;
  long cstart;
  long cend;
  int c;
  char *cdelims;
  int num_delims;

  SCM_VALIDATE_STRING_COPY (1, delims, cdelims);
  num_delims = SCM_STRING_LENGTH (delims);
  SCM_VALIDATE_SUBSTRING_SPEC_COPY (2, str, buf, 5, start, cstart,
				    6, end, cend);
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_VALIDATE_OPINPORT (4,port);

  for (j = cstart; j < cend; j++)
    {  
      int k;

      c = scm_getc (port);
      for (k = 0; k < num_delims; k++)
	{
	  if (cdelims[k] == c)
	    {
	      if (SCM_FALSEP (gobble))
		scm_ungetc (c, port);

	      return scm_cons (SCM_MAKE_CHAR (c),
			       scm_long2num (j - cstart));
	    }
	}
      if (c == EOF)
	return scm_cons (SCM_EOF_VAL, 
			 scm_long2num (j - cstart));

      buf[j] = c;
    }
  return scm_cons (SCM_BOOL_F, scm_long2num (j - cstart));
}
#undef FUNC_NAME

static unsigned char *
scm_do_read_line (SCM port, int *len_p)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  unsigned char *end;

  /* I thought reading lines was simple.  Mercy me.  */

  /* The common case: the buffer contains a complete line. 
     This needs to be fast.  */
  if ((end = memchr (pt->read_pos, '\n', (pt->read_end - pt->read_pos)))
	   != 0)
    {
      int buf_len = (end + 1) - pt->read_pos;
      /* Allocate a buffer of the perfect size.  */
      unsigned char *buf = scm_must_malloc (buf_len + 1, "%read-line");

      memcpy (buf, pt->read_pos, buf_len);
      pt->read_pos += buf_len;

      buf[buf_len] = '\0';

      *len_p = buf_len;
      return buf;
    }

  /* The buffer contains no newlines.  */
  {
    /* When live, len is always the number of characters in the
       current buffer that are part of the current line.  */
    int len = (pt->read_end - pt->read_pos);
    int buf_size = (len < 50) ? 60 : len * 2;
    /* Invariant: buf always has buf_size + 1 characters allocated;
       the `+ 1' is for the final '\0'.  */
    unsigned char *buf = scm_must_malloc (buf_size + 1, "%read-line");
    int buf_len = 0;

    for (;;)
      {
	if (buf_len + len > buf_size)
	  {
	    int new_size = (buf_len + len) * 2;
	    buf = scm_must_realloc (buf, buf_size + 1, new_size + 1,
				    "%read-line");
	    buf_size = new_size;
	  }

	/* Copy what we've got out of the port, into our buffer.  */
	memcpy (buf + buf_len, pt->read_pos, len);
	buf_len += len;
	pt->read_pos += len;

	/* If we had seen a newline, we're done now.  */
	if (end)
	  break;

	/* Get more characters.  */
	if (scm_fill_input (port) == EOF)
	  {
	    /* If we're missing a final newline in the file, return
	       what we did get, sans newline.  */
	    if (buf_len > 0)
	      break;

	    free (buf);
	    return 0;
	  }

	/* Search the buffer for newlines.  */
	if ((end = memchr (pt->read_pos, '\n',
			   (len = (pt->read_end - pt->read_pos))))
	    != 0)
	  len = (end - pt->read_pos) + 1;
      }

    /* I wonder how expensive this realloc is.  */
    buf = scm_must_realloc (buf, buf_size + 1, buf_len + 1, "%read-line");
    buf[buf_len] = '\0';
    *len_p = buf_len;
    return buf;
  }
}  


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
  scm_port *pt;
  char *s;
  int slen;
  SCM line, term;

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  SCM_VALIDATE_OPINPORT (1,port);

  pt = SCM_PTAB_ENTRY (port);
  if (pt->rw_active == SCM_PORT_WRITE)
    scm_ptobs[SCM_PTOBNUM (port)].flush (port);

  s = (char *) scm_do_read_line (port, &slen);

  if (s == NULL)
    term = line = SCM_EOF_VAL;
  else
    {
      if (s[slen-1] == '\n')
	{
	  term = SCM_MAKE_CHAR ('\n');
	  s[slen-1] = '\0';
	  line = scm_take_str (s, slen-1);
	  scm_done_malloc (-1);
	  SCM_INCLINE (port);
	}
      else
	{
	  /* Fix: we should check for eof on the port before assuming this. */
	  term = SCM_EOF_VAL;
	  line = scm_take_str (s, slen);
	  SCM_COL (port) += slen;
	}	  
    }

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  return scm_cons (line, term);
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

void 
scm_init_rdelim (void)
{
  SCM rdelim_module = scm_make_module (scm_read_0str ("(ice-9 rdelim)"));
  SCM old_module = scm_set_current_module (rdelim_module);

#ifndef SCM_MAGIC_SNARFER
#include "libguile/rdelim.x"
#endif

  scm_set_current_module (old_module);

#if DEBUG_DEPRECATED == 0
  {
    const char expr[] = "\
(define-module (guile) :use-module (ice-9 rdelim))\
(define-module (guile-user) :use-module (ice-9 rdelim))";

    scm_eval_string (scm_makfromstr (expr, (sizeof expr) - 1, 0));
  }
  scm_set_current_module (old_module);
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

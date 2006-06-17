/* Copyright (C) 1995,1996,1997,1999,2000,2001,2003, 2004, 2006 Free Software
 * Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */




#include <stdio.h>
#include "libguile/_scm.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/unif.h"
#include "libguile/keywords.h"
#include "libguile/alist.h"
#include "libguile/srcprop.h"
#include "libguile/hashtab.h"
#include "libguile/hash.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"
#include "libguile/validate.h"
#include "libguile/srfi-4.h"

#include "libguile/read.h"



SCM_GLOBAL_SYMBOL (scm_sym_dot, ".");
SCM_SYMBOL (scm_keyword_prefix, "prefix");

scm_t_option scm_read_opts[] = {
  { SCM_OPTION_BOOLEAN, "copy", 0,
    "Copy source code expressions." },
  { SCM_OPTION_BOOLEAN, "positions", 0,
    "Record positions of source code expressions." },
  { SCM_OPTION_BOOLEAN, "case-insensitive", 0,
    "Convert symbols to lower case."},
  { SCM_OPTION_SCM, "keywords", SCM_UNPACK (SCM_BOOL_F),
    "Style of keyword recognition: #f or 'prefix."}
#if SCM_ENABLE_ELISP
  ,
  { SCM_OPTION_BOOLEAN, "elisp-vectors", 0,
    "Support Elisp vector syntax, namely `[...]'."},
  { SCM_OPTION_BOOLEAN, "elisp-strings", 0,
    "Support `\\(' and `\\)' in strings."}
#endif
};

/*
  Give meaningful error messages for errors

  We use the format

  FILE:LINE:COL: MESSAGE
  This happened in ....

  This is not standard GNU format, but the test-suite likes the real
  message to be in front.

 */


void
scm_i_input_error (char const *function,
		   SCM port, const char *message, SCM arg)
{
  SCM fn = (scm_is_string (SCM_FILENAME(port))
	    ? SCM_FILENAME(port)
	    : scm_from_locale_string ("#<unknown port>"));

  SCM string_port = scm_open_output_string ();
  SCM string = SCM_EOL;
  scm_simple_format (string_port,
		     scm_from_locale_string ("~A:~S:~S: ~A"),
		     scm_list_4 (fn,
				 scm_from_int (SCM_LINUM (port) + 1),
				 scm_from_int (SCM_COL (port) + 1),
				 scm_from_locale_string (message)));
    
  string = scm_get_output_string (string_port);
  scm_close_output_port (string_port);
  scm_error_scm (scm_from_locale_symbol ("read-error"),
		 function? scm_from_locale_string (function) : SCM_BOOL_F,
		 string,
		 arg,
		 SCM_BOOL_F);
}


SCM_DEFINE (scm_read_options, "read-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the read options. Instead of using\n"
	    "this procedure directly, use the procedures @code{read-enable},\n"
	    "@code{read-disable}, @code{read-set!} and @code{read-options}.")
#define FUNC_NAME s_scm_read_options
{
  SCM ans = scm_options (setting,
			 scm_read_opts,
			 SCM_N_READ_OPTIONS,
			 FUNC_NAME);
  if (SCM_COPY_SOURCE_P)
    SCM_RECORD_POSITIONS_P = 1;
  return ans;
}
#undef FUNC_NAME

/* An association list mapping extra hash characters to procedures.  */
static SCM *scm_read_hash_procedures;

SCM_DEFINE (scm_read, "read", 0, 1, 0, 
            (SCM port),
	    "Read an s-expression from the input port @var{port}, or from\n"
	    "the current input port if @var{port} is not specified.\n"
	    "Any whitespace before the next token is discarded.")
#define FUNC_NAME s_scm_read
{
  int c;
  SCM tok_buf, copy;

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (1, port);

  c = scm_flush_ws (port, (char *) NULL);
  if (EOF == c)
    return SCM_EOF_VAL;
  scm_ungetc (c, port);

  tok_buf = scm_c_make_string (30, SCM_UNDEFINED);
  return scm_lreadr (&tok_buf, port, &copy);
}
#undef FUNC_NAME



char *
scm_grow_tok_buf (SCM *tok_buf)
{
  size_t oldlen = scm_i_string_length (*tok_buf);
  const char *olddata = scm_i_string_chars (*tok_buf);
  char *newdata;
  SCM newstr = scm_i_make_string (2 * oldlen, &newdata);
  size_t i;

  for (i = 0; i != oldlen; ++i)
    newdata[i] = olddata[i];

  *tok_buf = newstr;
  return newdata;
}

/* Consume an SCSH-style block comment.  Assume that we've already
   read the initial `#!', and eat characters until we get a
   exclamation-point/sharp-sign sequence. 
*/

static void
skip_scsh_block_comment (SCM port)
{
  int bang_seen = 0;

  for (;;)
    {
      int c = scm_getc (port);
      
      if (c == EOF)
	scm_i_input_error ("skip_block_comment", port, 
			   "unterminated `#! ... !#' comment", SCM_EOL);

      if (c == '!')
	bang_seen = 1;
      else if (c == '#' && bang_seen)
	return;
      else
	bang_seen = 0;
    }
}

int 
scm_flush_ws (SCM port, const char *eoferr)
{
  register int c;
  while (1)
    switch (c = scm_getc (port))
      {
      case EOF:
      goteof:
	if (eoferr)
	  {
	    scm_i_input_error (eoferr,
			       port,
			       "end of file",
			       SCM_EOL);
	  }
	return c;
      case ';':
      lp:
	switch (c = scm_getc (port))
	  {
	  case EOF:
	    goto goteof;
	  default:
	    goto lp;
	  case SCM_LINE_INCREMENTORS:
	    break;
	  }
	break;
      case '#':
	switch (c = scm_getc (port))
	  {
	  case EOF:
	    eoferr = "read_sharp";
	    goto goteof;
	  case '!':
	    skip_scsh_block_comment (port);
	    break;
	  default:
	    scm_ungetc (c, port);
	    return '#';
	  }
	break;
      case SCM_LINE_INCREMENTORS:
      case SCM_SINGLE_SPACES:
      case '\t':
	break;
      default:
	return c;
      }
}



int
scm_casei_streq (char *s1, char *s2)
{
  while (*s1 && *s2)
    if (scm_c_downcase((int)*s1) != scm_c_downcase((int)*s2))
      return 0;
    else
      {
	++s1;
	++s2;
      }
  return !(*s1 || *s2);
}

static int
scm_i_casei_streq (const char *s1, const char *s2, size_t len2)
{
  while (*s1 && len2 > 0)
    if (scm_c_downcase((int)*s1) != scm_c_downcase((int)*s2))
      return 0;
    else
      {
	++s1;
	++s2;
	--len2;
      }
  return !(*s1 || len2 > 0);
}

/* recsexpr is used when recording expressions
 * constructed by read:sharp.
 */
static SCM
recsexpr (SCM obj, long line, int column, SCM filename)
{
  if (!scm_is_pair(obj)) {
    return obj;
  } else {
    SCM tmp = obj, copy;
    /* If this sexpr is visible in the read:sharp source, we want to
       keep that information, so only record non-constant cons cells
       which haven't previously been read by the reader. */
    if (scm_is_false (scm_whash_lookup (scm_source_whash, obj)))
      {
	if (SCM_COPY_SOURCE_P)
	  {
	    copy = scm_cons (recsexpr (SCM_CAR (obj), line, column, filename),
			     SCM_UNDEFINED);
	    while ((tmp = SCM_CDR (tmp)) && scm_is_pair (tmp))
	      {
		SCM_SETCDR (copy, scm_cons (recsexpr (SCM_CAR (tmp),
						      line,
						      column,
						      filename),
					    SCM_UNDEFINED));
		copy = SCM_CDR (copy);
	      }
	    SCM_SETCDR (copy, tmp);
	  }
	else
	  {
	    recsexpr (SCM_CAR (obj), line, column, filename);
	    while ((tmp = SCM_CDR (tmp)) && scm_is_pair (tmp))
	      recsexpr (SCM_CAR (tmp), line, column, filename);
	    copy = SCM_UNDEFINED;
	  }
	scm_whash_insert (scm_source_whash,
			  obj,
			  scm_make_srcprops (line,
					     column,
					     filename,
					     copy,
					     SCM_EOL));
      }
    return obj;
  }
}


static SCM scm_get_hash_procedure(int c);
static SCM scm_i_lreadparen (SCM *, SCM, char *, SCM *, char);

static char s_list[]="list";
#if SCM_ENABLE_ELISP
static char s_vector[]="vector";
#endif

SCM 
scm_lreadr (SCM *tok_buf, SCM port, SCM *copy)
#define FUNC_NAME "scm_lreadr"
{
  int c;
  size_t j;
  SCM p;
				  
 tryagain:
  c = scm_flush_ws (port, s_scm_read);
  switch (c)
    {
    case EOF:
      return SCM_EOF_VAL;

    case '(':
      return SCM_RECORD_POSITIONS_P
	? scm_lreadrecparen (tok_buf, port, s_list, copy)
	: scm_i_lreadparen (tok_buf, port, s_list, copy, ')');
    case ')':
      scm_i_input_error (FUNC_NAME, port,"unexpected \")\"", SCM_EOL);
      goto tryagain;
    
#if SCM_ENABLE_ELISP
    case '[':
      if (SCM_ELISP_VECTORS_P)
	{
	  p = scm_i_lreadparen (tok_buf, port, s_vector, copy, ']');
	  return scm_is_null (p) ? scm_nullvect : scm_vector (p);
	}
      goto read_token;
#endif
    case '\'':
      p = scm_sym_quote;
      goto recquote;
    case '`':
      p = scm_sym_quasiquote;
      goto recquote;
    case ',':
      c = scm_getc (port);
      if ('@' == c)
	p = scm_sym_uq_splicing;
      else
	{
	  scm_ungetc (c, port);
	  p = scm_sym_unquote;
	}
    recquote:
      p = scm_cons2 (p,
		     scm_lreadr (tok_buf, port, copy),
		     SCM_EOL);
      if (SCM_RECORD_POSITIONS_P)
	scm_whash_insert (scm_source_whash,
			  p,
			  scm_make_srcprops (SCM_LINUM (port),
					     SCM_COL (port) - 1,
					     SCM_FILENAME (port),
					     SCM_COPY_SOURCE_P
					     ? (*copy = scm_cons2 (SCM_CAR (p),
								   SCM_CAR (SCM_CDR (p)),
								   SCM_EOL))
					     : SCM_UNDEFINED,
					     SCM_EOL));
      return p;
    case '#':
      c = scm_getc (port);

      {
	/* Check for user-defined hash procedure first, to allow
	   overriding of builtin hash read syntaxes.  */
	SCM sharp = scm_get_hash_procedure (c);
	if (scm_is_true (sharp))
	  {
	    int line = SCM_LINUM (port);
	    int column = SCM_COL (port) - 2;
	    SCM got;

	    got = scm_call_2 (sharp, SCM_MAKE_CHAR (c), port);
	    if (scm_is_eq (got, SCM_UNSPECIFIED))
	      goto handle_sharp;
	    if (SCM_RECORD_POSITIONS_P)
	      return *copy = recsexpr (got, line, column,
				       SCM_FILENAME (port));
	    else
	      return got;
	  }
      }
    handle_sharp:
      switch (c)
	{
	  /* Vector, arrays, both uniform and not are handled by this
	     one function.  It also disambiguates between '#f' and
	     '#f32' and '#f64'.
	  */
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	case 'u': case 's': case 'f':
	case '@':
	case '(':
#if SCM_ENABLE_DEPRECATED
	  /* See below for 'i' and 'e'. */
	case 'a':
	case 'c':
	case 'y':
	case 'h':
	case 'l':
#endif
	  return scm_i_read_array (port, c);

	case 't':
	case 'T':
	  return SCM_BOOL_T;

	case 'F':
	  /* See above for lower case 'f'. */
	  return SCM_BOOL_F;


	case 'i':
	case 'e':
#if SCM_ENABLE_DEPRECATED
	  {
	    /* When next char is '(', it really is an old-style
	       uniform array. */
	    int next_c = scm_getc (port);
	    if (next_c != EOF)
	      scm_ungetc (next_c, port);
	    if (next_c == '(')
	      return scm_i_read_array (port, c);
	    /* Fall through. */
	  }
#endif  
	case 'b':
	case 'B':
	case 'o':
	case 'O':
	case 'd':
	case 'D':
	case 'x':
	case 'X':
	case 'I':
	case 'E':
	  scm_ungetc (c, port);
	  c = '#';
	  goto num;

	case '!':
	  /* should never happen, #!...!# block comments are skipped
	     over in scm_flush_ws. */
	  abort ();

	case '*':
	  j = scm_read_token (c, tok_buf, port, 0);
	  p = scm_istr2bve (scm_c_substring_shared (*tok_buf, 1, j));
	  if (scm_is_true (p))
	    return p;
	  else
	    goto unkshrp;

	case '{':
	  j = scm_read_token (c, tok_buf, port, 1);
	  return scm_string_to_symbol (scm_c_substring_copy (*tok_buf, 0, j));

	case '\\':
	  c = scm_getc (port);
	  j = scm_read_token (c, tok_buf, port, 0);
	  if (j == 1)
	    return SCM_MAKE_CHAR (c);
	  if (c >= '0' && c < '8')
	    {
	      /* Dirk:FIXME::  This type of character syntax is not R5RS
	       * compliant.  Further, it should be verified that the constant
	       * does only consist of octal digits.  Finally, it should be
	       * checked whether the resulting fixnum is in the range of
	       * characters.  */
	      p = scm_c_locale_stringn_to_number (scm_i_string_chars (*tok_buf),
						  j, 8);
	      if (SCM_I_INUMP (p))
		return SCM_MAKE_CHAR (SCM_I_INUM (p));
	    }
	  for (c = 0; c < scm_n_charnames; c++)
	    if (scm_charnames[c]
		&& (scm_i_casei_streq (scm_charnames[c],
				       scm_i_string_chars (*tok_buf), j)))
	      return SCM_MAKE_CHAR (scm_charnums[c]);
	  scm_i_input_error (FUNC_NAME, port, "unknown character name ~a",
			     scm_list_1 (scm_c_substring (*tok_buf, 0, j)));

	  /* #:SYMBOL is a syntax for keywords supported in all contexts.  */
	case ':':
	  return scm_symbol_to_keyword (scm_read (port));

	default:
	callshrp:
	  {
	    SCM sharp = scm_get_hash_procedure (c);

	    if (scm_is_true (sharp))
	      {
		int line = SCM_LINUM (port);
		int column = SCM_COL (port) - 2;
		SCM got;

		got = scm_call_2 (sharp, SCM_MAKE_CHAR (c), port);
		if (scm_is_eq (got, SCM_UNSPECIFIED))
		  goto unkshrp;
		if (SCM_RECORD_POSITIONS_P)
		  return *copy = recsexpr (got, line, column,
					   SCM_FILENAME (port));
		else
		  return got;
	      }
	  }
	unkshrp:
	scm_i_input_error (FUNC_NAME, port, "Unknown # object: ~S",
			   scm_list_1 (SCM_MAKE_CHAR (c)));
	}

    case '"':
      j = 0;
      while ('"' != (c = scm_getc (port)))
	{
	  if (c == EOF)
	    str_eof: scm_i_input_error (FUNC_NAME, port,
					"end of file in string constant", 
					SCM_EOL);

	  while (j + 2 >= scm_i_string_length (*tok_buf))
	    scm_grow_tok_buf (tok_buf);

	  if (c == '\\')
	    switch (c = scm_getc (port))
	      {
	      case EOF:
		goto str_eof;
	      case '"':
	      case '\\':
		break;
#if SCM_ENABLE_ELISP
	      case '(':
	      case ')':
		if (SCM_ESCAPED_PARENS_P)
		  break;
		goto bad_escaped;
#endif
	      case '\n':
		continue;
	      case '0':
		c = '\0';
		break;
	      case 'f':
		c = '\f';
		break;
	      case 'n':
		c = '\n';
		break;
	      case 'r':
		c = '\r';
		break;
	      case 't':
		c = '\t';
		break;
	      case 'a':
		c = '\007';
		break;
	      case 'v':
		c = '\v';
		break;
	      case 'x':
		{
		  int a, b;
		  a = scm_getc (port);
		  if (a == EOF) goto str_eof;
		  b = scm_getc (port);
		  if (b == EOF) goto str_eof;
		  if      ('0' <= a && a <= '9') a -= '0';
		  else if ('A' <= a && a <= 'F') a = a - 'A' + 10;
		  else if ('a' <= a && a <= 'f') a = a - 'a' + 10;
		  else goto bad_escaped;
		  if      ('0' <= b && b <= '9') b -= '0';
		  else if ('A' <= b && b <= 'F') b = b - 'A' + 10;
		  else if ('a' <= b && b <= 'f') b = b - 'a' + 10;
		  else goto bad_escaped;
		  c = a * 16 + b;
		  break;
		}
	      default:
	      bad_escaped:
		scm_i_input_error(FUNC_NAME, port,
				  "illegal character in escape sequence: ~S",
				  scm_list_1 (SCM_MAKE_CHAR (c)));
	      }
	  scm_c_string_set_x (*tok_buf, j, SCM_MAKE_CHAR (c));
	  ++j;
	}
      if (j == 0)
	return scm_nullstr;

      /* Change this to scm_c_substring_read_only when
	 SCM_STRING_CHARS has been removed.
      */
      return scm_c_substring_copy (*tok_buf, 0, j);

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case '.':
    case '-':
    case '+':
    num:
      j = scm_read_token (c, tok_buf, port, 0);
      if (j == 1 && (c == '+' || c == '-'))
	/* Shortcut:  Detected symbol '+ or '- */
	goto tok;

      p = scm_c_locale_stringn_to_number (scm_i_string_chars (*tok_buf), j, 10);
      if (scm_is_true (p))
	return p;
      if (c == '#')
	{
	  if ((j == 2) && (scm_getc (port) == '('))
	    {
	      scm_ungetc ('(', port);
	      c = scm_i_string_chars (*tok_buf)[1];
	      goto callshrp;
	    }
	  scm_i_input_error (FUNC_NAME, port, "unknown # object", SCM_EOL);
	}
      goto tok;

    case ':':
      if (scm_is_eq (SCM_PACK (SCM_KEYWORD_STYLE), scm_keyword_prefix))
	return scm_symbol_to_keyword (scm_read (port));

      /* fallthrough */
    default:
#if SCM_ENABLE_ELISP
    read_token:
#endif
      j = scm_read_token (c, tok_buf, port, 0);
      /* fallthrough */

    tok:
      return scm_string_to_symbol (scm_c_substring (*tok_buf, 0, j));
    }
}
#undef FUNC_NAME


#ifdef _UNICOS
_Pragma ("noopt");		/* # pragma _CRI noopt */
#endif

size_t 
scm_read_token (int ic, SCM *tok_buf, SCM port, int weird)
{
  size_t j;
  int c;

  c = (SCM_CASE_INSENSITIVE_P ? scm_c_downcase(ic) : ic);
					    
  if (weird)
    j = 0;
  else
    {
      j = 0;
      while (j + 2 >= scm_i_string_length (*tok_buf))
	scm_grow_tok_buf (tok_buf);
      scm_c_string_set_x (*tok_buf, j, SCM_MAKE_CHAR (c));
      ++j;
    }

  while (1)
    {
      while (j + 2 >= scm_i_string_length (*tok_buf))
	scm_grow_tok_buf (tok_buf);
      c = scm_getc (port);
      switch (c)
	{
	case '(':
	case ')':
#if SCM_ENABLE_ELISP
	case '[':
	case ']':
#endif
	case '"':
	case ';':
	case SCM_WHITE_SPACES:
	case SCM_LINE_INCREMENTORS:
	  if (weird
#if SCM_ENABLE_ELISP
	      || ((!SCM_ELISP_VECTORS_P) && ((c == '[') || (c == ']')))
#endif
	      )
	    goto default_case;

	  scm_ungetc (c, port);
	case EOF:
	eof_case:
	  return j;
	case '\\':
	  if (!weird)
	    goto default_case;
	  else
	    {
	      c = scm_getc (port);
	      if (c == EOF)
		goto eof_case;
	      else
		goto default_case;
	    }
	case '}':
	  if (!weird)
	    goto default_case;

	  c = scm_getc (port);
	  if (c == '#')
	    {
	      return j;
	    }
	  else
	    {
	      scm_ungetc (c, port);
	      c = '}';
	      goto default_case;
	    }

	default:
	default_case:
	  {
	    c = (SCM_CASE_INSENSITIVE_P ? scm_c_downcase(c) : c);
            scm_c_string_set_x (*tok_buf, j, SCM_MAKE_CHAR (c));
	    ++j;
	  }

	}
    }
}

#ifdef _UNICOS
_Pragma ("opt");		/* # pragma _CRI opt */
#endif

static SCM 
scm_i_lreadparen (SCM *tok_buf, SCM port, char *name, SCM *copy, char term_char)
#define FUNC_NAME "scm_i_lreadparen"
{
  SCM tmp;
  SCM tl;
  SCM ans;
  int c;

  c = scm_flush_ws (port, name);
  if (term_char == c)
    return SCM_EOL;
  scm_ungetc (c, port);
  if (scm_is_eq (scm_sym_dot, (tmp = scm_lreadr (tok_buf, port, copy))))
    {
      ans = scm_lreadr (tok_buf, port, copy);
    closeit:
      if (term_char != (c = scm_flush_ws (port, name)))
	scm_i_input_error (FUNC_NAME, port, "missing close paren", SCM_EOL);
      return ans;
    }
  ans = tl = scm_cons (tmp, SCM_EOL);
  while (term_char != (c = scm_flush_ws (port, name)))
    {
      scm_ungetc (c, port);
      if (scm_is_eq (scm_sym_dot, (tmp = scm_lreadr (tok_buf, port, copy))))
	{
	  SCM_SETCDR (tl, scm_lreadr (tok_buf, port, copy));
	  goto closeit;
	}
      SCM_SETCDR (tl, scm_cons (tmp, SCM_EOL));
      tl = SCM_CDR (tl);
    }
  return ans;
}
#undef FUNC_NAME


SCM 
scm_lreadrecparen (SCM *tok_buf, SCM port, char *name, SCM *copy)
#define FUNC_NAME "scm_lreadrecparen"
{
  register int c;
  register SCM tmp;
  register SCM tl, tl2 = SCM_EOL;
  SCM ans, ans2 = SCM_EOL;
  /* Need to capture line and column numbers here. */
  int line = SCM_LINUM (port);
  int column = SCM_COL (port) - 1;

  c = scm_flush_ws (port, name);
  if (')' == c)
    return SCM_EOL;
  scm_ungetc (c, port);
  if (scm_is_eq (scm_sym_dot, (tmp = scm_lreadr (tok_buf, port, copy))))
    {
      ans = scm_lreadr (tok_buf, port, copy);
      if (')' != (c = scm_flush_ws (port, name)))
	scm_i_input_error (FUNC_NAME, port, "missing close paren", SCM_EOL);
      return ans;
    }
  /* Build the head of the list structure. */
  ans = tl = scm_cons (tmp, SCM_EOL);
  if (SCM_COPY_SOURCE_P)
    ans2 = tl2 = scm_cons (scm_is_pair (tmp)
			   ? *copy
			   : tmp,
			   SCM_EOL);
  while (')' != (c = scm_flush_ws (port, name)))
    {
      SCM new_tail;

      scm_ungetc (c, port);
      if (scm_is_eq (scm_sym_dot, (tmp = scm_lreadr (tok_buf, port, copy))))
	{
	  SCM_SETCDR (tl, tmp = scm_lreadr (tok_buf, port, copy));
	  if (SCM_COPY_SOURCE_P)
	    SCM_SETCDR (tl2, scm_cons (scm_is_pair (tmp)
				       ? *copy
				       : tmp,
				       SCM_EOL));
	  if (')' != (c = scm_flush_ws (port, name)))
	    scm_i_input_error (FUNC_NAME, port,
			       "missing close paren", SCM_EOL);
	  goto exit;
	}

      new_tail = scm_cons (tmp, SCM_EOL);
      SCM_SETCDR (tl, new_tail);
      tl = new_tail;

      if (SCM_COPY_SOURCE_P)
	{
	  SCM new_tail2 = scm_cons (scm_is_pair (tmp) ? *copy : tmp, SCM_EOL);
	  SCM_SETCDR (tl2, new_tail2);
	  tl2 = new_tail2;
	}
    }
exit:
  scm_whash_insert (scm_source_whash,
		    ans,
		    scm_make_srcprops (line,
				       column,
				       SCM_FILENAME (port),
				       SCM_COPY_SOURCE_P
				       ? *copy = ans2
				       : SCM_UNDEFINED,
				       SCM_EOL));
  return ans;
}
#undef FUNC_NAME




/* Manipulate the read-hash-procedures alist.  This could be written in
   Scheme, but maybe it will also be used by C code during initialisation.  */
SCM_DEFINE (scm_read_hash_extend, "read-hash-extend", 2, 0, 0,
            (SCM chr, SCM proc),
	    "Install the procedure @var{proc} for reading expressions\n"
	    "starting with the character sequence @code{#} and @var{chr}.\n"
	    "@var{proc} will be called with two arguments:  the character\n"
	    "@var{chr} and the port to read further data from. The object\n"
	    "returned will be the return value of @code{read}.")
#define FUNC_NAME s_scm_read_hash_extend
{
  SCM this;
  SCM prev;

  SCM_VALIDATE_CHAR (1, chr);
  SCM_ASSERT (scm_is_false (proc)
	      || scm_is_eq (scm_procedure_p (proc), SCM_BOOL_T),
	      proc, SCM_ARG2, FUNC_NAME);

  /* Check if chr is already in the alist.  */
  this = *scm_read_hash_procedures;
  prev = SCM_BOOL_F;
  while (1)
    {
      if (scm_is_null (this))
	{
	  /* not found, so add it to the beginning.  */
	  if (scm_is_true (proc))
	    {
	      *scm_read_hash_procedures = 
		scm_cons (scm_cons (chr, proc), *scm_read_hash_procedures);
	    }
	  break;
	}
      if (scm_is_eq (chr, SCM_CAAR (this)))
	{
	  /* already in the alist.  */
	  if (scm_is_false (proc))
	    {
	      /* remove it.  */
	      if (scm_is_false (prev))
		{
		  *scm_read_hash_procedures =
		    SCM_CDR (*scm_read_hash_procedures);
		}
	      else
		scm_set_cdr_x (prev, SCM_CDR (this));
	    }
	  else
	    {
	      /* replace it.  */
	      scm_set_cdr_x (SCM_CAR (this), proc);
	    }
	  break;
	}
      prev = this;
      this = SCM_CDR (this);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Recover the read-hash procedure corresponding to char c.  */
static SCM
scm_get_hash_procedure (int c)
{
  SCM rest = *scm_read_hash_procedures;

  while (1)
    {
      if (scm_is_null (rest))
	return SCM_BOOL_F;
  
      if (SCM_CHAR (SCM_CAAR (rest)) == c)
	return SCM_CDAR (rest);
     
      rest = SCM_CDR (rest);
    }
}

void
scm_init_read ()
{
  scm_read_hash_procedures =
    SCM_VARIABLE_LOC (scm_c_define ("read-hash-procedures", SCM_EOL));

  scm_init_opts (scm_read_options, scm_read_opts, SCM_N_READ_OPTIONS);
#include "libguile/read.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

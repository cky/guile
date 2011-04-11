/* Copyright (C) 1995,1996,1997,1999,2000,2001,2003, 2004, 2006, 2007, 2008, 2009, 2010, 2011 Free Software
 * Foundation, Inc.
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
# include <config.h>
#endif

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <unicase.h>
#include <unictype.h>

#include "libguile/_scm.h"
#include "libguile/bytevectors.h"
#include "libguile/chars.h"
#include "libguile/eval.h"
#include "libguile/arrays.h"
#include "libguile/bitvectors.h"
#include "libguile/keywords.h"
#include "libguile/alist.h"
#include "libguile/srcprop.h"
#include "libguile/hashtab.h"
#include "libguile/hash.h"
#include "libguile/ports.h"
#include "libguile/fports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/strports.h"
#include "libguile/vectors.h"
#include "libguile/validate.h"
#include "libguile/srfi-4.h"
#include "libguile/srfi-13.h"

#include "libguile/read.h"
#include "libguile/private-options.h"




SCM_GLOBAL_SYMBOL (scm_sym_dot, ".");
SCM_SYMBOL (scm_keyword_prefix, "prefix");
SCM_SYMBOL (scm_keyword_postfix, "postfix");
SCM_SYMBOL (sym_nil, "nil");

scm_t_option scm_read_opts[] = {
  { SCM_OPTION_BOOLEAN, "copy", 0,
    "Copy source code expressions." },
  { SCM_OPTION_BOOLEAN, "positions", 1,
    "Record positions of source code expressions." },
  { SCM_OPTION_BOOLEAN, "case-insensitive", 0,
    "Convert symbols to lower case."},
  { SCM_OPTION_SCM, "keywords", (scm_t_bits) SCM_BOOL_F,
    "Style of keyword recognition: #f, 'prefix or 'postfix."},
  { SCM_OPTION_BOOLEAN, "r6rs-hex-escapes", 0,
    "Use R6RS variable-length character and string hex escapes."},
  { SCM_OPTION_BOOLEAN, "square-brackets", 1,
    "Treat `[' and `]' as parentheses, for R6RS compatibility."},
  { SCM_OPTION_BOOLEAN, "hungry-eol-escapes", 0,
    "In strings, consume leading whitespace after an escaped end-of-line."},
  { 0, },
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
				 scm_from_long (SCM_LINUM (port) + 1),
				 scm_from_int (SCM_COL (port) + 1),
				 scm_from_locale_string (message)));
    
  string = scm_get_output_string (string_port);
  scm_close_output_port (string_port);
  scm_error_scm (scm_from_latin1_symbol ("read-error"),
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
			 FUNC_NAME);
  if (SCM_COPY_SOURCE_P)
    SCM_RECORD_POSITIONS_P = 1;
  return ans;
}
#undef FUNC_NAME

/* A fluid referring to an association list mapping extra hash
   characters to procedures.  */
static SCM *scm_i_read_hash_procedures;

static inline SCM
scm_i_read_hash_procedures_ref (void)
{
  return scm_fluid_ref (*scm_i_read_hash_procedures);
}

static inline void
scm_i_read_hash_procedures_set_x (SCM value)
{
  scm_fluid_set_x (*scm_i_read_hash_procedures, value);
}


/* Token readers.  */


/* Size of the C buffer used to read symbols and numbers.  */
#define READER_BUFFER_SIZE            128

/* Size of the C buffer used to read strings.  */
#define READER_STRING_BUFFER_SIZE     512

/* The maximum size of Scheme character names.  */
#define READER_CHAR_NAME_MAX_SIZE      50


/* `isblank' is only in C99.  */
#define CHAR_IS_BLANK_(_chr)					\
  (((_chr) == ' ') || ((_chr) == '\t') || ((_chr) == '\n')	\
   || ((_chr) == '\f') || ((_chr) == '\r'))

#ifdef MSDOS
# define CHAR_IS_BLANK(_chr)			\
  ((CHAR_IS_BLANK_ (chr)) || ((_chr) == 26))
#else
# define CHAR_IS_BLANK CHAR_IS_BLANK_
#endif


/* R5RS one-character delimiters (see section 7.1.1, ``Lexical
   structure'').  */
#define CHAR_IS_R5RS_DELIMITER(c)				\
  (CHAR_IS_BLANK (c)						\
   || (c == ')') || (c == '(') || (c == ';') || (c == '"')      \
   || (SCM_SQUARE_BRACKETS_P && ((c == '[') || (c == ']'))))

#define CHAR_IS_DELIMITER  CHAR_IS_R5RS_DELIMITER

/* Exponent markers, as defined in section 7.1.1 of R5RS, ``Lexical
   Structure''.  */
#define CHAR_IS_EXPONENT_MARKER(_chr)				\
  (((_chr) == 'e') || ((_chr) == 's') || ((_chr) == 'f')	\
   || ((_chr) == 'd') || ((_chr) == 'l'))

/* Read an SCSH block comment.  */
static inline SCM scm_read_scsh_block_comment (scm_t_wchar, SCM);
static SCM scm_read_r6rs_block_comment (scm_t_wchar, SCM);
static SCM scm_read_commented_expression (scm_t_wchar, SCM);
static SCM scm_read_shebang (scm_t_wchar, SCM);
static SCM scm_get_hash_procedure (int);

/* Read from PORT until a delimiter (e.g., a whitespace) is read.  Put the
   result in the pre-allocated buffer BUF.  Return zero if the whole token has
   fewer than BUF_SIZE bytes, non-zero otherwise. READ will be set the number of
   bytes actually read.  */
static inline int
read_token (SCM port, char *buf, const size_t buf_size, size_t *read)
 {
   *read = 0;

   while (*read < buf_size)
     {
       int chr;

       chr = scm_get_byte_or_eof (port);

       if (chr == EOF)
        return 0;
      else if (CHAR_IS_DELIMITER (chr))
        {
          scm_unget_byte (chr, port);
          return 0;
        }
      else
        {
          *buf = (char) chr;
          buf++, (*read)++;
        }
     }

   return 1;
 }

/* Read from PORT until a delimiter (e.g., a whitespace) is read.  Put the
   result in the pre-allocated buffer BUFFER, if the whole token has fewer than
   BUFFER_SIZE bytes, or into OVERFLOW_BUFFER, allocated here to be freed by the
   caller.  Return zero if the token fits in BUFFER, non-zero otherwise. READ
   will be set the number of bytes actually read.  */
static int
read_complete_token (SCM port, char *buffer, const size_t buffer_size,
                           char **overflow_buffer, size_t *read)
{
  int overflow = 0;
  size_t bytes_read, overflow_size;

  *overflow_buffer = NULL;
  overflow_size = 0;

  do
    {
      overflow = read_token (port, buffer, buffer_size, &bytes_read);
      if (bytes_read == 0)
        break;
      if (overflow || overflow_size != 0)
        {
          if (overflow_size == 0)
            {
              *overflow_buffer = scm_malloc (bytes_read);
              memcpy (*overflow_buffer, buffer, bytes_read);
              overflow_size = bytes_read;
            }
          else
            {
              *overflow_buffer = scm_realloc (*overflow_buffer, overflow_size + bytes_read);
              memcpy (*overflow_buffer + overflow_size, buffer, bytes_read);
              overflow_size += bytes_read;
            }
        }
    }
  while (overflow);

  if (overflow_size)
    *read = overflow_size;
  else
    *read = bytes_read;

  return (overflow_size != 0);
}

/* Skip whitespace from PORT and return the first non-whitespace character
   read.  Raise an error on end-of-file.  */
static int
flush_ws (SCM port, const char *eoferr)
{
  register scm_t_wchar c;
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
	    scm_read_shebang (c, port);
	    break;
	  case ';':
	    scm_read_commented_expression (c, port);
	    break;
	  case '|':
	    if (scm_is_false (scm_get_hash_procedure (c)))
	      {
		scm_read_r6rs_block_comment (c, port);
		break;
	      }
	    /* fall through */
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

  return 0;
}



/* Token readers.  */

static SCM scm_read_expression (SCM port);
static SCM scm_read_sharp (int chr, SCM port);
static SCM recsexpr (SCM obj, long line, int column, SCM filename);


static SCM
scm_read_sexp (scm_t_wchar chr, SCM port)
#define FUNC_NAME "scm_i_lreadparen"
{
  register int c;
  register SCM tmp;
  register SCM tl, ans = SCM_EOL;
  SCM tl2 = SCM_EOL, ans2 = SCM_EOL, copy = SCM_BOOL_F;
  const int terminating_char = ((chr == '[') ? ']' : ')');

  /* Need to capture line and column numbers here. */
  long line = SCM_LINUM (port);
  int column = SCM_COL (port) - 1;


  c = flush_ws (port, FUNC_NAME);
  if (terminating_char == c)
    return SCM_EOL;

  scm_ungetc (c, port);
  if (scm_is_eq (scm_sym_dot,
		 (tmp = scm_read_expression (port))))
    {
      ans = scm_read_expression (port);
      if (terminating_char != (c = flush_ws (port, FUNC_NAME)))
	scm_i_input_error (FUNC_NAME, port, "missing close paren",
			   SCM_EOL);
      return ans;
    }

  /* Build the head of the list structure. */
  ans = tl = scm_cons (tmp, SCM_EOL);

  if (SCM_COPY_SOURCE_P)
    ans2 = tl2 = scm_cons (scm_is_pair (tmp)
			   ? copy
			   : tmp,
			   SCM_EOL);

  while (terminating_char != (c = flush_ws (port, FUNC_NAME)))
    {
      SCM new_tail;

      if (c == ')' || (SCM_SQUARE_BRACKETS_P && c == ']'))
        scm_i_input_error (FUNC_NAME, port,
                           "in pair: mismatched close paren: ~A",
                           scm_list_1 (SCM_MAKE_CHAR (c)));

      scm_ungetc (c, port);
      tmp = scm_read_expression (port);

      if (scm_is_eq (scm_sym_dot, tmp))
	{
	  SCM_SETCDR (tl, tmp = scm_read_expression (port));

	  if (SCM_COPY_SOURCE_P)
	    SCM_SETCDR (tl2, scm_cons (scm_is_pair (tmp) ? copy : tmp,
				       SCM_EOL));

	  c = flush_ws (port, FUNC_NAME);
	  if (terminating_char != c)
	    scm_i_input_error (FUNC_NAME, port,
			       "in pair: missing close paren", SCM_EOL);
	  goto exit;
	}

      new_tail = scm_cons (tmp, SCM_EOL);
      SCM_SETCDR (tl, new_tail);
      tl = new_tail;

      if (SCM_COPY_SOURCE_P)
	{
	  SCM new_tail2 = scm_cons (scm_is_pair (tmp)
				    ? copy
				    : tmp, SCM_EOL);
	  SCM_SETCDR (tl2, new_tail2);
	  tl2 = new_tail2;
	}
    }

 exit:
  if (SCM_RECORD_POSITIONS_P)
    scm_whash_insert (scm_source_whash,
		      ans,
		      scm_make_srcprops (line, column,
					 SCM_FILENAME (port),
					 SCM_COPY_SOURCE_P
					 ? ans2
					 : SCM_UNDEFINED,
					 SCM_EOL));
  return ans;
}
#undef FUNC_NAME


/* Read a hexadecimal number NDIGITS in length.  Put its value into the variable
   C.  If TERMINATOR is non-null, terminate early if the TERMINATOR character is
   found.  */
#define SCM_READ_HEX_ESCAPE(ndigits, terminator)                   \
  do                                                               \
    {                                                              \
      scm_t_wchar a;                                               \
      size_t i = 0;                                                \
      c = 0;                                                       \
      while (i < ndigits)                                          \
        {                                                          \
          a = scm_getc (port);                                     \
          if (a == EOF)                                            \
            goto str_eof;                                          \
          if (terminator                                           \
              && (a == (scm_t_wchar) terminator)                   \
              && (i > 0))                                          \
            break;                                                 \
          if ('0' <= a && a <= '9')                                \
            a -= '0';                                              \
          else if ('A' <= a && a <= 'F')                           \
            a = a - 'A' + 10;                                      \
          else if ('a' <= a && a <= 'f')                           \
            a = a - 'a' + 10;                                      \
          else                                                     \
            {                                                      \
              c = a;                                               \
              goto bad_escaped;                                    \
            }                                                      \
          c = c * 16 + a;                                          \
          i ++;                                                    \
        }                                                          \
    } while (0)

static void
skip_intraline_whitespace (SCM port)
{
  scm_t_wchar c;
  
  do
    {
      c = scm_getc (port);
      if (c == EOF)
        return;
    }
  while (c == '\t' || uc_is_general_category (c, UC_SPACE_SEPARATOR));

  scm_ungetc (c, port);
}                                         

static SCM
scm_read_string (int chr, SCM port)
#define FUNC_NAME "scm_lreadr"
{
  /* For strings smaller than C_STR, this function creates only one Scheme
     object (the string returned).  */

  SCM str = SCM_BOOL_F;
  unsigned c_str_len = 0;
  scm_t_wchar c;

  str = scm_i_make_string (READER_STRING_BUFFER_SIZE, NULL, 0);
  while ('"' != (c = scm_getc (port)))
    {
      if (c == EOF)
        {
        str_eof:
          scm_i_input_error (FUNC_NAME, port,
                             "end of file in string constant", SCM_EOL);
        }

      if (c_str_len + 1 >= scm_i_string_length (str))
        {
          SCM addy = scm_i_make_string (READER_STRING_BUFFER_SIZE, NULL, 0);

          str = scm_string_append (scm_list_2 (str, addy));
        }

      if (c == '\\')
        {
          switch (c = scm_getc (port))
            {
            case EOF:
              goto str_eof;
            case '"':
            case '\\':
              break;
            case '\n':
              if (SCM_HUNGRY_EOL_ESCAPES_P)
                skip_intraline_whitespace (port);
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
            case 'b':
              c = '\010';
              break;
            case 'x':
              if (SCM_R6RS_ESCAPES_P)
                SCM_READ_HEX_ESCAPE (10, ';');
              else
                SCM_READ_HEX_ESCAPE (2, '\0');
              break;
            case 'u':
              if (!SCM_R6RS_ESCAPES_P)
                {
                  SCM_READ_HEX_ESCAPE (4, '\0');
                  break;
                }
            case 'U':
              if (!SCM_R6RS_ESCAPES_P)
                {
                  SCM_READ_HEX_ESCAPE (6, '\0');
                  break;
                }
            default:
            bad_escaped:
              scm_i_input_error (FUNC_NAME, port,
                                 "illegal character in escape sequence: ~S",
                                 scm_list_1 (SCM_MAKE_CHAR (c)));
            }
        }
      str = scm_i_string_start_writing (str);
      scm_i_string_set_x (str, c_str_len++, c);
      scm_i_string_stop_writing ();
    }

  if (c_str_len > 0)
    {
      return scm_i_substring_copy (str, 0, c_str_len);
    }

  return scm_nullstr;
}
#undef FUNC_NAME


static SCM
scm_read_number (scm_t_wchar chr, SCM port)
{
  SCM result, str = SCM_EOL;
  char buffer[READER_BUFFER_SIZE];
  char *overflow_buffer = NULL;
  size_t bytes_read;
  int overflow;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  scm_ungetc (chr, port);
  overflow = read_complete_token (port, buffer, sizeof (buffer),
                                  &overflow_buffer, &bytes_read);

  if (!overflow)
    str = scm_from_stringn (buffer, bytes_read, pt->encoding, pt->ilseq_handler);
  else
    str = scm_from_stringn (overflow_buffer, bytes_read, pt->encoding,
                            pt->ilseq_handler);

  result = scm_string_to_number (str, SCM_UNDEFINED);
  if (!scm_is_true (result))
    {
      /* Return a symbol instead of a number */
      if (SCM_CASE_INSENSITIVE_P)
        str = scm_string_downcase_x (str);
      result = scm_string_to_symbol (str);
    }

  if (overflow)
    free (overflow_buffer);
  SCM_COL (port) += scm_i_string_length (str);
  return result;
}

static SCM
scm_read_mixed_case_symbol (scm_t_wchar chr, SCM port)
{
  SCM result;
  int ends_with_colon = 0;
  size_t bytes_read;
  int postfix = scm_is_eq (SCM_PACK (SCM_KEYWORD_STYLE), scm_keyword_postfix);
  int overflow;
  char buffer[READER_BUFFER_SIZE], *overflow_buffer;
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  SCM str;

  scm_ungetc (chr, port);
  overflow = read_complete_token (port, buffer, READER_BUFFER_SIZE,
                                  &overflow_buffer, &bytes_read);
  if (bytes_read > 0)
    {
      if (!overflow)
        ends_with_colon = buffer[bytes_read - 1] == ':';
      else
        ends_with_colon = overflow_buffer[bytes_read - 1] == ':';
    }

  if (postfix && ends_with_colon && (bytes_read > 1))
    {
      if (!overflow)
        str = scm_from_stringn (buffer, bytes_read - 1, pt->encoding, pt->ilseq_handler);
      else
        str = scm_from_stringn (overflow_buffer, bytes_read - 1, pt->encoding,
                                pt->ilseq_handler);

      if (SCM_CASE_INSENSITIVE_P)
        str = scm_string_downcase_x (str);
      result = scm_symbol_to_keyword (scm_string_to_symbol (str));
    }
  else
    {
      if (!overflow)
        str = scm_from_stringn (buffer, bytes_read, pt->encoding, pt->ilseq_handler);
      else
        str = scm_from_stringn (overflow_buffer, bytes_read, pt->encoding,
                                pt->ilseq_handler);

      if (SCM_CASE_INSENSITIVE_P)
        str = scm_string_downcase_x (str);
      result = scm_string_to_symbol (str);
    }

  if (overflow)
    free (overflow_buffer);
  SCM_COL (port) += scm_i_string_length (str);
  return result;
}

static SCM
scm_read_number_and_radix (scm_t_wchar chr, SCM port)
#define FUNC_NAME "scm_lreadr"
{
  SCM result;
  size_t read;
  char buffer[READER_BUFFER_SIZE], *overflow_buffer;
  int overflow;
  unsigned int radix;
  SCM str;
  scm_t_port *pt;

  switch (chr)
    {
    case 'B':
    case 'b':
      radix = 2;
      break;

    case 'o':
    case 'O':
      radix = 8;
      break;

    case 'd':
    case 'D':
      radix = 10;
      break;

    case 'x':
    case 'X':
      radix = 16;
      break;

    default:
      scm_ungetc (chr, port);
      scm_ungetc ('#', port);
      radix = 10;
    }

  overflow = read_complete_token (port, buffer, sizeof (buffer),
                                  &overflow_buffer, &read);

  pt = SCM_PTAB_ENTRY (port);
  if (!overflow)
    str = scm_from_stringn (buffer, read, pt->encoding, pt->ilseq_handler);
  else
    str = scm_from_stringn (overflow_buffer, read, pt->encoding,
                            pt->ilseq_handler);

  result = scm_string_to_number (str, scm_from_uint (radix));

  if (overflow)
    free (overflow_buffer);

  SCM_COL (port) += scm_i_string_length (str);

  if (scm_is_true (result))
    return result;

  scm_i_input_error (FUNC_NAME, port, "unknown # object", SCM_EOL);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

static SCM
scm_read_quote (int chr, SCM port)
{
  SCM p;
  long line = SCM_LINUM (port);
  int column = SCM_COL (port) - 1;

  switch (chr)
    {
    case '`':
      p = scm_sym_quasiquote;
      break;

    case '\'':
      p = scm_sym_quote;
      break;

    case ',':
      {
	scm_t_wchar c;

	c = scm_getc (port);
	if ('@' == c)
	  p = scm_sym_uq_splicing;
	else
	  {
	    scm_ungetc (c, port);
	    p = scm_sym_unquote;
	  }
	break;
      }

    default:
      fprintf (stderr, "%s: unhandled quote character (%i)\n",
	       "scm_read_quote", chr);
      abort ();
    }

  p = scm_cons2 (p, scm_read_expression (port), SCM_EOL);
  if (SCM_RECORD_POSITIONS_P)
    scm_whash_insert (scm_source_whash, p,
		      scm_make_srcprops (line, column,
					 SCM_FILENAME (port),
					 SCM_COPY_SOURCE_P
					 ? (scm_cons2 (SCM_CAR (p),
						       SCM_CAR (SCM_CDR (p)),
						       SCM_EOL))
					 : SCM_UNDEFINED,
					 SCM_EOL));


  return p;
}

SCM_SYMBOL (sym_syntax, "syntax");
SCM_SYMBOL (sym_quasisyntax, "quasisyntax");
SCM_SYMBOL (sym_unsyntax, "unsyntax");
SCM_SYMBOL (sym_unsyntax_splicing, "unsyntax-splicing");

static SCM
scm_read_syntax (int chr, SCM port)
{
  SCM p;
  long line = SCM_LINUM (port);
  int column = SCM_COL (port) - 1;

  switch (chr)
    {
    case '`':
      p = sym_quasisyntax;
      break;

    case '\'':
      p = sym_syntax;
      break;

    case ',':
      {
	int c;

	c = scm_getc (port);
	if ('@' == c)
	  p = sym_unsyntax_splicing;
	else
	  {
	    scm_ungetc (c, port);
	    p = sym_unsyntax;
	  }
	break;
      }

    default:
      fprintf (stderr, "%s: unhandled syntax character (%i)\n",
	       "scm_read_syntax", chr);
      abort ();
    }

  p = scm_cons2 (p, scm_read_expression (port), SCM_EOL);
  if (SCM_RECORD_POSITIONS_P)
    scm_whash_insert (scm_source_whash, p,
		      scm_make_srcprops (line, column,
					 SCM_FILENAME (port),
					 SCM_COPY_SOURCE_P
					 ? (scm_cons2 (SCM_CAR (p),
						       SCM_CAR (SCM_CDR (p)),
						       SCM_EOL))
					 : SCM_UNDEFINED,
					 SCM_EOL));


  return p;
}

static inline SCM
scm_read_nil (int chr, SCM port)
{
  SCM id = scm_read_mixed_case_symbol (chr, port);

  if (!scm_is_eq (id, sym_nil))
    scm_i_input_error ("scm_read_nil", port,
                       "unexpected input while reading #nil: ~a",
                       scm_list_1 (id));

  return SCM_ELISP_NIL;
}
  
static inline SCM
scm_read_semicolon_comment (int chr, SCM port)
{
  int c;

  /* We use the get_byte here because there is no need to get the
     locale correct with comment input. This presumes that newline
     always represents itself no matter what the encoding is.  */
  for (c = scm_get_byte_or_eof (port);
       (c != EOF) && (c != '\n');
       c = scm_get_byte_or_eof (port));

  return SCM_UNSPECIFIED;
}


/* Sharp readers, i.e. readers called after a `#' sign has been read.  */

static SCM
scm_read_boolean (int chr, SCM port)
{
  switch (chr)
    {
    case 't':
    case 'T':
      return SCM_BOOL_T;

    case 'f':
    case 'F':
      return SCM_BOOL_F;
    }

  return SCM_UNSPECIFIED;
}

static SCM
scm_read_character (scm_t_wchar chr, SCM port)
#define FUNC_NAME "scm_lreadr"
{
  char buffer[READER_CHAR_NAME_MAX_SIZE];
  SCM charname;
  size_t charname_len, bytes_read;
  scm_t_wchar cp;
  int overflow;
  scm_t_port *pt;

  overflow = read_token (port, buffer, READER_CHAR_NAME_MAX_SIZE, &bytes_read);
  if (overflow)
    scm_i_input_error (FUNC_NAME, port, "character name too long", SCM_EOL);

  if (bytes_read == 0)
    {
      chr = scm_getc (port);
      if (chr == EOF)
	scm_i_input_error (FUNC_NAME, port, "unexpected end of file "
			   "while reading character", SCM_EOL);

      /* CHR must be a token delimiter, like a whitespace.  */
      return (SCM_MAKE_CHAR (chr));
    }

  pt = SCM_PTAB_ENTRY (port);

  /* Simple ASCII characters can be processed immediately.  Also, simple
     ISO-8859-1 characters can be processed immediately if the encoding for this
     port is ISO-8859-1.  */
  if (bytes_read == 1 && ((unsigned char) buffer[0] <= 127 || pt->encoding == NULL))
    {
      SCM_COL (port) += 1;
      return SCM_MAKE_CHAR (buffer[0]);
    }

  /* Otherwise, convert the buffer into a proper scheme string for
     processing.  */
  charname = scm_from_stringn (buffer, bytes_read, pt->encoding,
			       pt->ilseq_handler);
  charname_len = scm_i_string_length (charname);
  SCM_COL (port) += charname_len;
  cp = scm_i_string_ref (charname, 0);
  if (charname_len == 1)
    return SCM_MAKE_CHAR (cp);

  /* Ignore dotted circles, which may be used to keep combining characters from
     combining with the backslash in #\charname.  */
  if (cp == SCM_CODEPOINT_DOTTED_CIRCLE && charname_len == 2)
    return SCM_MAKE_CHAR (scm_i_string_ref (charname, 1));

  if (cp >= '0' && cp < '8')
    {
      /* Dirk:FIXME::  This type of character syntax is not R5RS
       * compliant.  Further, it should be verified that the constant
       * does only consist of octal digits.  */
      SCM p = scm_string_to_number (charname, scm_from_uint (8));
      if (SCM_I_INUMP (p))
        {
          scm_t_wchar c = scm_to_uint32 (p);
          if (SCM_IS_UNICODE_CHAR (c))
            return SCM_MAKE_CHAR (c);
          else
            scm_i_input_error (FUNC_NAME, port,
                               "out-of-range octal character escape: ~a",
                               scm_list_1 (charname));
        }
    }

  if (cp == 'x' && (charname_len > 1))
    {
      SCM p;

      /* Convert from hex, skipping the initial 'x' character in CHARNAME */
      p = scm_string_to_number (scm_c_substring (charname, 1, charname_len),
                                scm_from_uint (16));
      if (SCM_I_INUMP (p))
        {
          scm_t_wchar c = scm_to_uint32 (p);
          if (SCM_IS_UNICODE_CHAR (c))
            return SCM_MAKE_CHAR (c);
          else
            scm_i_input_error (FUNC_NAME, port,
                               "out-of-range hex character escape: ~a",
                               scm_list_1 (charname));
        }
    }

  /* The names of characters should never have non-Latin1
     characters.  */
  if (scm_i_is_narrow_string (charname)
      || scm_i_try_narrow_string (charname))
    { SCM ch = scm_i_charname_to_char (scm_i_string_chars (charname),
                                       charname_len);
      if (scm_is_true (ch))
        return ch;
    }

  scm_i_input_error (FUNC_NAME, port, "unknown character name ~a",
		     scm_list_1 (charname));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static inline SCM
scm_read_keyword (int chr, SCM port)
{
  SCM symbol;

  /* Read the symbol that comprises the keyword.  Doing this instead of
     invoking a specific symbol reader function allows `scm_read_keyword ()'
     to adapt to the delimiters currently valid of symbols.

     XXX: This implementation allows sloppy syntaxes like `#:  key'.  */
  symbol = scm_read_expression (port);
  if (!scm_is_symbol (symbol))
    scm_i_input_error ("scm_read_keyword", port,
		       "keyword prefix `~a' not followed by a symbol: ~s",
		       scm_list_2 (SCM_MAKE_CHAR (chr), symbol));

  return (scm_symbol_to_keyword (symbol));
}

static inline SCM
scm_read_vector (int chr, SCM port)
{
  /* Note: We call `scm_read_sexp ()' rather than READER here in order to
     guarantee that it's going to do what we want.  After all, this is an
     implementation detail of `scm_read_vector ()', not a desirable
     property.  */
  return (scm_vector (scm_read_sexp (chr, port)));
}

static inline SCM
scm_read_srfi4_vector (int chr, SCM port)
{
  return scm_i_read_array (port, chr);
}

static SCM
scm_read_bytevector (scm_t_wchar chr, SCM port)
{
  chr = scm_getc (port);
  if (chr != 'u')
    goto syntax;

  chr = scm_getc (port);
  if (chr != '8')
    goto syntax;

  chr = scm_getc (port);
  if (chr != '(')
    goto syntax;

  return scm_u8_list_to_bytevector (scm_read_sexp (chr, port));

 syntax:
  scm_i_input_error ("read_bytevector", port,
		     "invalid bytevector prefix",
		     SCM_MAKE_CHAR (chr));
  return SCM_UNSPECIFIED;
}

static SCM
scm_read_guile_bit_vector (scm_t_wchar chr, SCM port)
{
  /* Read the `#*10101'-style read syntax for bit vectors in Guile.  This is
     terribly inefficient but who cares?  */
  SCM s_bits = SCM_EOL;

  for (chr = scm_getc (port);
       (chr != EOF) && ((chr == '0') || (chr == '1'));
       chr = scm_getc (port))
    {
      s_bits = scm_cons ((chr == '0') ? SCM_BOOL_F : SCM_BOOL_T, s_bits);
    }

  if (chr != EOF)
    scm_ungetc (chr, port);

  return scm_bitvector (scm_reverse_x (s_bits, SCM_EOL));
}

static inline SCM
scm_read_scsh_block_comment (scm_t_wchar chr, SCM port)
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
	break;
      else
	bang_seen = 0;
    }

  return SCM_UNSPECIFIED;
}

static inline SCM
scm_read_shebang (scm_t_wchar chr, SCM port)
{
  int c = 0;
  if ((c = scm_get_byte_or_eof (port)) != 'r')
    {
      scm_ungetc (c, port);
      return scm_read_scsh_block_comment (chr, port);
    }
  if ((c = scm_get_byte_or_eof (port)) != '6')
    {
      scm_ungetc (c, port);
      scm_ungetc ('r', port);
      return scm_read_scsh_block_comment (chr, port);
    }
  if ((c = scm_get_byte_or_eof (port)) != 'r')
    {
      scm_ungetc (c, port);
      scm_ungetc ('6', port);
      scm_ungetc ('r', port);
      return scm_read_scsh_block_comment (chr, port);
    }
  if ((c = scm_get_byte_or_eof (port)) != 's')
    {
      scm_ungetc (c, port);
      scm_ungetc ('r', port);
      scm_ungetc ('6', port);
      scm_ungetc ('r', port);
      return scm_read_scsh_block_comment (chr, port);
    }
  
  return SCM_UNSPECIFIED;
}

static SCM
scm_read_r6rs_block_comment (scm_t_wchar chr, SCM port)
{
  /* Unlike SCSH-style block comments, SRFI-30/R6RS block comments may be
     nested.  So care must be taken.  */
  int nesting_level = 1;
  int opening_seen = 0, closing_seen = 0;

  while (nesting_level > 0)
    {
      int c = scm_getc (port);

      if (c == EOF)
	scm_i_input_error ("scm_read_r6rs_block_comment", port,
			   "unterminated `#| ... |#' comment", SCM_EOL);

      if (opening_seen)
	{
	  if (c == '|')
	    nesting_level++;
	  opening_seen = 0;
	}
      else if (closing_seen)
	{
	  if (c == '#')
	    nesting_level--;
	  closing_seen = 0;
	}
      else if (c == '|')
	closing_seen = 1;
      else if (c == '#')
	opening_seen = 1;
      else
	opening_seen = closing_seen = 0;
    }

  return SCM_UNSPECIFIED;
}

static SCM
scm_read_commented_expression (scm_t_wchar chr, SCM port)
{
  scm_t_wchar c;
  
  c = flush_ws (port, (char *) NULL);
  if (EOF == c)
    scm_i_input_error ("read_commented_expression", port,
                       "no expression after #; comment", SCM_EOL);
  scm_ungetc (c, port);
  scm_read_expression (port);
  return SCM_UNSPECIFIED;
}

static SCM
scm_read_extended_symbol (scm_t_wchar chr, SCM port)
{
  /* Guile's extended symbol read syntax looks like this:

       #{This is all a symbol name}#

     So here, CHR is expected to be `{'.  */
  int saw_brace = 0;
  size_t len = 0;
  SCM buf = scm_i_make_string (1024, NULL, 0);

  buf = scm_i_string_start_writing (buf);

  while ((chr = scm_getc (port)) != EOF)
    {
      if (saw_brace)
	{
	  if (chr == '#')
	    {
	      break;
	    }
	  else
	    {
	      saw_brace = 0;
	      scm_i_string_set_x (buf, len++, '}');
	    }
	}

      if (chr == '}')
	saw_brace = 1;
      else if (chr == '\\')
        {
          /* It used to be that print.c would print extended-read-syntax
             symbols with backslashes before "non-standard" chars, but
             this routine wouldn't do anything with those escapes.
             Bummer.  What we've done is to change print.c to output
             R6RS hex escapes for those characters, relying on the fact
             that the extended read syntax would never put a `\' before
             an `x'.  For now, we just ignore other instances of
             backslash in the string.  */
          switch ((chr = scm_getc (port)))
            {
            case EOF:
              goto done;
            case 'x':
              {
                scm_t_wchar c;
                
                SCM_READ_HEX_ESCAPE (10, ';');
                scm_i_string_set_x (buf, len++, c);
                break;

              str_eof:
                chr = EOF;
                goto done;

              bad_escaped:
                scm_i_string_stop_writing ();
                scm_i_input_error ("scm_read_extended_symbol", port,
                                   "illegal character in escape sequence: ~S",
                                   scm_list_1 (SCM_MAKE_CHAR (c)));
                break;
              }
            default:
	      scm_i_string_set_x (buf, len++, chr);
              break;
            }
        }
      else
        scm_i_string_set_x (buf, len++, chr);

      if (len >= scm_i_string_length (buf) - 2)
	{
	  SCM addy;

	  scm_i_string_stop_writing ();
	  addy = scm_i_make_string (1024, NULL, 0);
	  buf = scm_string_append (scm_list_2 (buf, addy));
	  len = 0;
	  buf = scm_i_string_start_writing (buf);
	}
    }

 done:
  scm_i_string_stop_writing ();
  if (chr == EOF)
    scm_i_input_error ("scm_read_extended_symbol", port,
                       "end of file while reading symbol", SCM_EOL);

  return (scm_string_to_symbol (scm_c_substring (buf, 0, len)));
}



/* Top-level token readers, i.e., dispatchers.  */

static SCM
scm_read_sharp_extension (int chr, SCM port)
{
  SCM proc;

  proc = scm_get_hash_procedure (chr);
  if (scm_is_true (scm_procedure_p (proc)))
    {
      long line = SCM_LINUM (port);
      int column = SCM_COL (port) - 2;
      SCM got;

      got = scm_call_2 (proc, SCM_MAKE_CHAR (chr), port);
      if (!scm_is_eq (got, SCM_UNSPECIFIED))
	{
	  if (SCM_RECORD_POSITIONS_P)
	    return (recsexpr (got, line, column,
			      SCM_FILENAME (port)));
	  else
	    return got;
	}
    }

  return SCM_UNSPECIFIED;
}

/* The reader for the sharp `#' character.  It basically dispatches reads
   among the above token readers.   */
static SCM
scm_read_sharp (scm_t_wchar chr, SCM port)
#define FUNC_NAME "scm_lreadr"
{
  SCM result;

  chr = scm_getc (port);

  result = scm_read_sharp_extension (chr, port);
  if (!scm_is_eq (result, SCM_UNSPECIFIED))
    return result;

  switch (chr)
    {
    case '\\':
      return (scm_read_character (chr, port));
    case '(':
      return (scm_read_vector (chr, port));
    case 's':
    case 'u':
    case 'f':
    case 'c':
      /* This one may return either a boolean or an SRFI-4 vector.  */
      return (scm_read_srfi4_vector (chr, port));
    case 'v':
      return (scm_read_bytevector (chr, port));
    case '*':
      return (scm_read_guile_bit_vector (chr, port));
    case 't':
    case 'T':
    case 'F':
      /* This one may return either a boolean or an SRFI-4 vector.  */
      return (scm_read_boolean (chr, port));
    case ':':
      return (scm_read_keyword (chr, port));
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case '@':
#if SCM_ENABLE_DEPRECATED
      /* See below for 'i' and 'e'. */
    case 'a':
    case 'y':
    case 'h':
    case 'l':
#endif
      return (scm_i_read_array (port, chr));

    case 'i':
    case 'e':
#if SCM_ENABLE_DEPRECATED
      {
	/* When next char is '(', it really is an old-style
	   uniform array. */
	scm_t_wchar next_c = scm_getc (port);
	if (next_c != EOF)
	  scm_ungetc (next_c, port);
	if (next_c == '(')
	  return scm_i_read_array (port, chr);
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
      return (scm_read_number_and_radix (chr, port));
    case '{':
      return (scm_read_extended_symbol (chr, port));
    case '!':
      return (scm_read_shebang (chr, port));
    case ';':
      return (scm_read_commented_expression (chr, port));
    case '`':
    case '\'':
    case ',':
      return (scm_read_syntax (chr, port));
    case 'n':
      return (scm_read_nil (chr, port));
    default:
      result = scm_read_sharp_extension (chr, port);
      if (scm_is_eq (result, SCM_UNSPECIFIED))
	{
	  /* To remain compatible with 1.8 and earlier, the following
	     characters have lower precedence than `read-hash-extend'
	     characters.  */
	  switch (chr)
	    {
	    case '|':
	      return scm_read_r6rs_block_comment (chr, port);
	    default:
	      scm_i_input_error (FUNC_NAME, port, "Unknown # object: ~S",
				 scm_list_1 (SCM_MAKE_CHAR (chr)));
	    }
	}
      else
	return result;
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
scm_read_expression (SCM port)
#define FUNC_NAME "scm_read_expression"
{
  while (1)
    {
      register scm_t_wchar chr;

      chr = scm_getc (port);

      switch (chr)
	{
	case SCM_WHITE_SPACES:
	case SCM_LINE_INCREMENTORS:
	  break;
	case ';':
	  (void) scm_read_semicolon_comment (chr, port);
	  break;
	case '[':
          if (!SCM_SQUARE_BRACKETS_P)
            return (scm_read_mixed_case_symbol (chr, port));
          /* otherwise fall through */
	case '(':
	  return (scm_read_sexp (chr, port));
	case '"':
	  return (scm_read_string (chr, port));
	case '\'':
	case '`':
	case ',':
	  return (scm_read_quote (chr, port));
	case '#':
	  {
	    SCM result;
	    result = scm_read_sharp (chr, port);
	    if (scm_is_eq (result, SCM_UNSPECIFIED))
	      /* We read a comment or some such.  */
	      break;
	    else
	      return result;
	  }
	case ')':
	  scm_i_input_error (FUNC_NAME, port, "unexpected \")\"", SCM_EOL);
	  break;
	case ']':
          if (SCM_SQUARE_BRACKETS_P)
            scm_i_input_error (FUNC_NAME, port, "unexpected \"]\"", SCM_EOL);
          /* otherwise fall through */
	case EOF:
	  return SCM_EOF_VAL;
	case ':':
	  if (scm_is_eq (SCM_PACK (SCM_KEYWORD_STYLE), scm_keyword_prefix))
	    return scm_symbol_to_keyword (scm_read_expression (port));
	  /* Fall through.  */

	default:
	  {
	    if (((chr >= '0') && (chr <= '9'))
		|| (strchr ("+-.", chr)))
	      return (scm_read_number (chr, port));
	    else
	      return (scm_read_mixed_case_symbol (chr, port));
	  }
	}
    }
}
#undef FUNC_NAME


/* Actual reader.  */

SCM_DEFINE (scm_read, "read", 0, 1, 0, 
            (SCM port),
	    "Read an s-expression from the input port @var{port}, or from\n"
	    "the current input port if @var{port} is not specified.\n"
	    "Any whitespace before the next token is discarded.")
#define FUNC_NAME s_scm_read
{
  int c;

  if (SCM_UNBNDP (port))
    port = scm_current_input_port ();
  SCM_VALIDATE_OPINPORT (1, port);

  c = flush_ws (port, (char *) NULL);
  if (EOF == c)
    return SCM_EOF_VAL;
  scm_ungetc (c, port);

  return (scm_read_expression (port));
}
#undef FUNC_NAME




/* Used when recording expressions constructed by `scm_read_sharp ()'.  */
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

/* Manipulate the read-hash-procedures alist.  This could be written in
   Scheme, but maybe it will also be used by C code during initialisation.  */
SCM_DEFINE (scm_read_hash_extend, "read-hash-extend", 2, 0, 0,
            (SCM chr, SCM proc),
	    "Install the procedure @var{proc} for reading expressions\n"
	    "starting with the character sequence @code{#} and @var{chr}.\n"
	    "@var{proc} will be called with two arguments:  the character\n"
	    "@var{chr} and the port to read further data from. The object\n"
	    "returned will be the return value of @code{read}. \n"
	    "Passing @code{#f} for @var{proc} will remove a previous setting. \n"
	    )
#define FUNC_NAME s_scm_read_hash_extend
{
  SCM this;
  SCM prev;

  SCM_VALIDATE_CHAR (1, chr);
  SCM_ASSERT (scm_is_false (proc)
	      || scm_is_eq (scm_procedure_p (proc), SCM_BOOL_T),
	      proc, SCM_ARG2, FUNC_NAME);

  /* Check if chr is already in the alist.  */
  this = scm_i_read_hash_procedures_ref ();
  prev = SCM_BOOL_F;
  while (1)
    {
      if (scm_is_null (this))
	{
	  /* not found, so add it to the beginning.  */
	  if (scm_is_true (proc))
	    {
              SCM new = scm_cons (scm_cons (chr, proc),
                                  scm_i_read_hash_procedures_ref ());
	      scm_i_read_hash_procedures_set_x (new);
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
                  SCM rest = SCM_CDR (scm_i_read_hash_procedures_ref ());
		  scm_i_read_hash_procedures_set_x (rest);
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
  SCM rest = scm_i_read_hash_procedures_ref ();

  while (1)
    {
      if (scm_is_null (rest))
	return SCM_BOOL_F;
  
      if (SCM_CHAR (SCM_CAAR (rest)) == c)
	return SCM_CDAR (rest);
     
      rest = SCM_CDR (rest);
    }
}

#define SCM_ENCODING_SEARCH_SIZE (500)

/* Search the first few hundred characters of a file for an Emacs-like coding
   declaration.  Returns either NULL or a string whose storage has been
   allocated with `scm_gc_malloc ()'.  */
char *
scm_i_scan_for_encoding (SCM port)
{
  scm_t_port *pt;
  char header[SCM_ENCODING_SEARCH_SIZE+1];
  size_t bytes_read, encoding_length, i;
  char *encoding = NULL;
  int utf8_bom = 0;
  char *pos, *encoding_start;
  int in_comment;

  pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    scm_flush (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (pt->read_pos == pt->read_end)
    {
      /* We can use the read buffer, and thus avoid a seek. */
      if (scm_fill_input (port) == EOF)
        return NULL;

      bytes_read = pt->read_end - pt->read_pos;
      if (bytes_read > SCM_ENCODING_SEARCH_SIZE)
        bytes_read = SCM_ENCODING_SEARCH_SIZE;

      if (bytes_read <= 1)
        /* An unbuffered port -- don't scan.  */
        return NULL;

      memcpy (header, pt->read_pos, bytes_read);
      header[bytes_read] = '\0';
    }
  else
    {
      /* Try to read some bytes and then seek back.  Not all ports
         support seeking back; and indeed some file ports (like
         /dev/urandom) will succeed on an lseek (fd, 0, SEEK_CUR)---the
         check performed by SCM_FPORT_FDES---but fail to seek
         backwards.  Hence this block comes second.  We prefer to use
         the read buffer in-place.  */
      if (SCM_FPORTP (port) && !SCM_FDES_RANDOM_P (SCM_FPORT_FDES (port)))
        return NULL;

      bytes_read = scm_c_read (port, header, SCM_ENCODING_SEARCH_SIZE);
      header[bytes_read] = '\0';
      scm_seek (port, scm_from_int (0), scm_from_int (SEEK_SET));
    }

  if (bytes_read > 3 
      && header[0] == '\xef' && header[1] == '\xbb' && header[2] == '\xbf')
    utf8_bom = 1;

  /* search past "coding[:=]" */
  pos = header;
  while (1)
    {
      if ((pos = strstr(pos, "coding")) == NULL)
        return NULL;

      pos += strlen("coding");
      if (pos - header >= SCM_ENCODING_SEARCH_SIZE || 
          (*pos == ':' || *pos == '='))
        {
          pos ++;
          break;
        }
    }

  /* skip spaces */
  while (pos - header <= SCM_ENCODING_SEARCH_SIZE && 
	 (*pos == ' ' || *pos == '\t'))
    pos ++;

  /* grab the next token */
  encoding_start = pos;
  i = 0;
  while (encoding_start + i - header <= SCM_ENCODING_SEARCH_SIZE
         && encoding_start + i - header < bytes_read
	 && (isalnum ((int) encoding_start[i])
	     || strchr ("_-.:/,+=()", encoding_start[i]) != NULL))
    i++;

  encoding_length = i;
  if (encoding_length == 0)
    return NULL;

  encoding = scm_gc_strndup (encoding_start, encoding_length, "encoding");
  for (i = 0; i < encoding_length; i++)
    encoding[i] = toupper ((int) encoding[i]);

  /* push backwards to make sure we were in a comment */
  in_comment = 0;
  pos = encoding_start;
  while (pos >= header)
    {
      if (*pos == ';')
	{
	  in_comment = 1;
	  break;
	}
      else if (*pos == '\n' || pos == header)
	{
	  /* This wasn't in a semicolon comment. Check for a
	   hash-bang comment. */
	  char *beg = strstr (header, "#!");
	  char *end = strstr (header, "!#");
	  if (beg < encoding_start && encoding_start + encoding_length <= end)
	    in_comment = 1;
	  break;
	}
      else
        {
          pos --;
          continue;
        }
    }
  if (!in_comment)
    /* This wasn't in a comment */
    return NULL;

  if (utf8_bom && strcmp(encoding, "UTF-8"))
    scm_misc_error (NULL,
		    "the port input declares the encoding ~s but is encoded as UTF-8",
		    scm_list_1 (scm_from_locale_string (encoding)));

  return encoding;
}

SCM_DEFINE (scm_file_encoding, "file-encoding", 1, 0, 0,
            (SCM port),
            "Scans the port for an Emacs-like character coding declaration\n"
            "near the top of the contents of a port with random-accessible contents.\n"
            "The coding declaration is of the form\n"
            "@code{coding: XXXXX} and must appear in a scheme comment.\n"
            "\n"
            "Returns a string containing the character encoding of the file\n"
            "if a declaration was found, or @code{#f} otherwise.\n")
#define FUNC_NAME s_scm_file_encoding
{
  char *enc;
  SCM s_enc;

  SCM_VALIDATE_OPINPORT (SCM_ARG1, port);

  enc = scm_i_scan_for_encoding (port);
  if (enc == NULL)
    return SCM_BOOL_F;
  else
    {
      s_enc = scm_from_locale_string (enc);
      return s_enc;
    }

  return SCM_BOOL_F;
}
#undef FUNC_NAME

void
scm_init_read ()
{
  SCM read_hash_procs;

  read_hash_procs = scm_make_fluid ();
  scm_fluid_set_x (read_hash_procs, SCM_EOL);
  
  scm_i_read_hash_procedures =
    SCM_VARIABLE_LOC (scm_c_define ("%read-hash-procedures", read_hash_procs));

  scm_init_opts (scm_read_options, scm_read_opts);
#include "libguile/read.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

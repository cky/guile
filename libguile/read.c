/*	Copyright (C) 1995,1996 Free Software Foundation, Inc.
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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
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


#include "extchrs.h"
#include <stdio.h>
#include "_scm.h"
#include "chars.h"
#include "genio.h"
#include "eval.h"
#include "unif.h"
#include "mbstrings.h"
#include "kw.h"
#include "alist.h"
#include "srcprop.h"
#include "hashtab.h"
#include "hash.h"

#include "read.h"



#define default_case_i 0



scm_option scm_read_opts[] = {
  { SCM_OPTION_BOOLEAN, "copy", 0,
    "Copy source code expressions." },
  { SCM_OPTION_BOOLEAN, "positions", 0,
    "Record positions of source code expressions." }
};

SCM_PROC (s_read_options, "read-options-interface", 0, 1, 0, scm_read_options);
#ifdef __STDC__
SCM
scm_read_options (SCM setting)
#else
SCM
scm_read_options (setting)
     SCM setting;
#endif
{
  SCM ans = scm_options (setting,
			 scm_read_opts,
			 SCM_N_READ_OPTIONS,
			 s_read_options);
  if (SCM_COPY_SOURCE_P)
    SCM_RECORD_POSITIONS_P = 1;
  return ans;
}

SCM_PROC (s_read, "read", 0, 3, 0, scm_read);
#ifdef __STDC__
SCM 
scm_read (SCM port, SCM case_insensitive_p, SCM sharp)
#else
SCM 
scm_read (port, case_insensitive_p, sharp)
     SCM port;
     SCM case_insensitive_p;
     SCM sharp;
#endif
{
  int c;
  SCM tok_buf, copy;
  int case_i;

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port),
		port,
		SCM_ARG1,
		s_read);

  case_i = (SCM_UNBNDP (case_insensitive_p)
	    ? default_case_i
	    : (case_insensitive_p == SCM_BOOL_F));
  
  if (SCM_UNBNDP (sharp))
    sharp = SCM_BOOL_F;

  c = scm_flush_ws (port, (char *) NULL);
  if (EOF == c)
    return SCM_EOF_VAL;
  scm_gen_ungetc (c, port);

  tok_buf = scm_makstr (30L, 0);
  return scm_lreadr (&tok_buf, port, case_i, sharp, &copy);
}


#ifdef __STDC__
char *
scm_grow_tok_buf (SCM * tok_buf)
#else
char *
scm_grow_tok_buf (tok_buf)
     SCM * tok_buf;
#endif
{
  scm_vector_set_length_x (*tok_buf, SCM_MAKINUM (2 * SCM_LENGTH (*tok_buf)));
  return SCM_CHARS (*tok_buf);
}


#ifdef __STDC__
int 
scm_flush_ws (SCM port, char *eoferr)
#else
int 
scm_flush_ws (port, eoferr)
     SCM port;
     char *eoferr;
#endif
{
  register int c;
  while (1)
    switch (c = scm_gen_getc (port))
      {
      case EOF:
      goteof:
	if (eoferr)
	  scm_wta (SCM_UNDEFINED, "end of file in ", eoferr);
	return c;
      case ';':
      lp:
	switch (c = scm_gen_getc (port))
	  {
	  case EOF:
	    goto goteof;
	  default:
	    goto lp;
	  case SCM_LINE_INCREMENTORS:
	    break;
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


#ifdef __STDC__
int
scm_casei_streq (char * s1, char * s2)
#else
int
scm_casei_streq (s1, s2)
     char * s1;
     char * s2;
#endif
{
  while (*s1 && *s2)
    if (scm_downcase((int)*s1) != scm_downcase((int)*s2))
      return 0;
    else
      {
	++s1;
	++s2;
      }
  return !(*s1 || *s2);
}


/* recsexpr is used when recording expressions
 * constructed by read:sharp.
 */
#ifdef __STDC__
static SCM
recsexpr (SCM obj, int line, int column, SCM filename)
#else
static SCM
recsexpr (obj, line, column, filename)
     SCM obj;
     int line;
     int column;
     SCM filename;
#endif
{
  if (SCM_IMP (obj) || SCM_NCONSP(obj))
    return obj;
  {
    SCM tmp = obj, copy;
    /* If this sexpr is visible in the read:sharp source, we want to
       keep that information, so only record non-constant cons cells
       which haven't previously been read by the reader. */
    if (SCM_FALSEP (scm_whash_lookup (scm_source_whash, obj)))
      {
	if (SCM_COPY_SOURCE_P)
	  {
	    copy = scm_cons (recsexpr (SCM_CAR (obj), line, column, filename),
			     SCM_UNDEFINED);
	    while (SCM_NIMP (tmp = SCM_CDR (tmp)) && SCM_CONSP (tmp))
	      copy = (SCM_CDR (copy) = scm_cons (recsexpr (SCM_CAR (tmp),
							   line,
							   column,
							   filename),
						 SCM_UNDEFINED));
	    SCM_CDR (copy) = tmp;
	  }
	else
	  {
	    recsexpr (SCM_CAR (obj), line, column, filename);
	    while (SCM_NIMP (tmp = SCM_CDR (tmp)) && SCM_CONSP (tmp))
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

static char s_list[]="list";
#ifdef __STDC__
SCM 
scm_lreadr (SCM *tok_buf, SCM port, int case_i, SCM sharp, SCM *copy)
#else
SCM 
scm_lreadr (tok_buf, port, case_i, sharp, copy)
     SCM *tok_buf;
     SCM port;
     int case_i;
     SCM sharp;
     SCM *copy;
#endif
{
  int c;
  scm_sizet j;
  SCM p;

tryagain:
  c = scm_flush_ws (port, s_read);
  switch (c)
    {
    case EOF:
      return SCM_EOF_VAL;

    case '(':
      return SCM_RECORD_POSITIONS_P
	     ? scm_lreadrecparen (tok_buf, port, s_list, case_i, sharp, copy)
	     : scm_lreadparen (tok_buf, port, s_list, case_i, sharp, copy);
    case ')':
      scm_wta (SCM_UNDEFINED, "unexpected \")\"", "read");
      goto tryagain;
    
    case '\'':
      p = scm_i_quote;
      goto recquote;
    case '`':
      p = scm_i_quasiquote;
      goto recquote;
    case ',':
      c = scm_gen_getc (port);
      if ('@' == c)
	p = scm_i_uq_splicing;
      else
	{
	  scm_gen_ungetc (c, port);
	  p = scm_i_unquote;
	}
    recquote:
      p = scm_cons2 (p,
		     scm_lreadr (tok_buf, port, case_i, sharp, copy),
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
      c = scm_gen_getc (port);
      switch (c)
	{
	case '(':
	  p = scm_lreadparen (tok_buf, port, "vector", case_i, sharp, copy);
	  return SCM_NULLP (p) ? scm_nullvect : scm_vector (p);

	case 't':
	case 'T':
	  return SCM_BOOL_T;
	case 'f':
	case 'F':
	  return SCM_BOOL_F;

	case 'b':
	case 'B':
	case 'o':
	case 'O':
	case 'd':
	case 'D':
	case 'x':
	case 'X':
	case 'i':
	case 'I':
	case 'e':
	case 'E':
	  scm_gen_ungetc (c, port);
	  c = '#';
	  goto num;

	case '*':
	  j = scm_read_token (c, tok_buf, port, case_i, 0);
	  p = scm_istr2bve (SCM_CHARS (*tok_buf) + 1, (long) (j - 1));
	  if (SCM_NFALSEP (p))
	    return p;
	  else
	    goto unkshrp;

	case '{':
	  j = scm_read_token (c, tok_buf, port, case_i, 1);
	  p = scm_intern (SCM_CHARS (*tok_buf), j);
	  if (SCM_PORT_REPRESENTATION (port) != scm_regular_port)
	    scm_set_symbol_multi_byte_x (SCM_CAR (p), SCM_BOOL_T);
	  return SCM_CAR (p);

	case '\\':
	  c = scm_gen_getc (port);
	  j = scm_read_token (c, tok_buf, port, case_i, 0);
	  if (j == 1)
	    return SCM_MAKICHR (c);
	  if (c >= '0' && c < '8')
	    {
	      p = scm_istr2int (SCM_CHARS (*tok_buf), (long) j, 8);
	      if (SCM_NFALSEP (p))
		return SCM_MAKICHR (SCM_INUM (p));
	    }
	  for (c = 0; c < scm_n_charnames; c++)
	    if (scm_charnames[c]
		&& (scm_casei_streq (scm_charnames[c], SCM_CHARS (*tok_buf))))
	      return SCM_MAKICHR (scm_charnums[c]);
	  scm_wta (SCM_UNDEFINED, "unknown # object: #\\", SCM_CHARS (*tok_buf));


	default:
	callshrp:
	  if (SCM_NIMP (sharp))
	    {
	      int line = SCM_LINUM (port);
	      int column = SCM_COL (port) - 2;
	      SCM got;
	      got = scm_apply (sharp,
			       SCM_MAKICHR (c),
			       scm_acons (port, SCM_EOL, SCM_EOL));
	      if (SCM_UNSPECIFIED == got)
		goto unkshrp;
	      if (SCM_RECORD_POSITIONS_P)
		return *copy = recsexpr (got, line, column,
					 SCM_FILENAME (port));
	      else
		return got;
	    }
	unkshrp:scm_wta ((SCM) SCM_MAKICHR (c), "unknown # object", "");
	}

    case '"':
      j = 0;
      while ('"' != (c = scm_gen_getc (port)))
	{
	  SCM_ASSERT (EOF != c, SCM_UNDEFINED, "end of file in ", "string");

	  while (j + sizeof(xwchar_t) + XMB_CUR_MAX >= SCM_LENGTH (*tok_buf))
	    scm_grow_tok_buf (tok_buf);

	  if (c == '\\')
	    switch (c = scm_gen_getc (port))
	      {
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
	      }
	  if (SCM_PORT_REPRESENTATION(port) == scm_regular_port)
	    {
	      SCM_CHARS (*tok_buf)[j] = c;
	      ++j;
	    }
	  else
	    {
	      int len;
	      len = xwctomb (SCM_CHARS (*tok_buf) + j, c);
	      if (len == 0)
		len = 1;
	      SCM_ASSERT (len > 0, SCM_MAKINUM (c), "bogus char", "read");
	      j += len;
	    }
	}
      if (j == 0)
	return scm_nullstr;
      SCM_CHARS (*tok_buf)[j] = 0;
      {
	SCM str;
	str = scm_makfromstr (SCM_CHARS (*tok_buf), j, 0);
	if (SCM_PORT_REPRESENTATION(port) != scm_regular_port)
	  {
	    SCM_SETLENGTH (str, SCM_LENGTH (str), scm_tc7_mb_string);
	  }
	return str;
      }

    case'0':case '1':case '2':case '3':case '4':
    case '5':case '6':case '7':case '8':case '9':
    case '.':
    case '-':
    case '+':
    num:
      j = scm_read_token (c, tok_buf, port, case_i, 0);
      p = scm_istring2number (SCM_CHARS (*tok_buf), (long) j, 10L);
      if (SCM_NFALSEP (p))
	return p;
      if (c == '#')
	{
	  if ((j == 2) && (scm_gen_getc (port) == '('))
	    {
	      scm_gen_ungetc ('(', port);
	      c = SCM_CHARS (*tok_buf)[1];
	      goto callshrp;
	    }
	  scm_wta (SCM_UNDEFINED, "unknown # object", SCM_CHARS (*tok_buf));
	}
      goto tok;

    case ':':
      j = scm_read_token ('-', tok_buf, port, case_i, 0);
      p = scm_intern (SCM_CHARS (*tok_buf), j);
      if (SCM_PORT_REPRESENTATION (port) != scm_regular_port)
	scm_set_symbol_multi_byte_x (SCM_CAR (p), SCM_BOOL_T);
      return scm_make_keyword_from_dash_symbol (SCM_CAR (p));

    default:
      j = scm_read_token (c, tok_buf, port, case_i, 0);
      /* fallthrough */

    tok:
      p = scm_intern (SCM_CHARS (*tok_buf), j);
      if (SCM_PORT_REPRESENTATION (port) != scm_regular_port)
	scm_set_symbol_multi_byte_x (SCM_CAR (p), SCM_BOOL_T);
      return SCM_CAR (p);
    }
}

#ifdef _UNICOS
_Pragma ("noopt");		/* # pragma _CRI noopt */
#endif
#ifdef __STDC__
scm_sizet 
scm_read_token (int ic, SCM * tok_buf, SCM port, int case_i, int weird)
#else
scm_sizet 
scm_read_token (ic, * tok_buf, port, case_i, weird)
     int ic;
     SCM *tok_buf;
     SCM port;
     int case_i;
     int weird;
#endif
{
  register scm_sizet j;
  register int c;
  register char *p;

  c = ic;
  p = SCM_CHARS (*tok_buf);

  if (weird)
    j = 0;
  else
    {
      j = 0;
      while (j + sizeof(xwchar_t) + XMB_CUR_MAX >= SCM_LENGTH (*tok_buf))
	p = scm_grow_tok_buf (tok_buf);
      if (SCM_PORT_REPRESENTATION(port) == scm_regular_port)
	{
	  p[j] = c;
	  ++j;
	}
      else
	{
	  int len;
	  len = xwctomb (p + j, c);
	  if (len == 0)
	    len = 1;
	  SCM_ASSERT (len > 0, SCM_MAKINUM (c), "bogus char", "read");
	  j += len;
	}
    }

  while (1)
    {
      while (j + sizeof(xwchar_t) + XMB_CUR_MAX >= SCM_LENGTH (*tok_buf))
	p = scm_grow_tok_buf (tok_buf);
      c = scm_gen_getc (port);
      switch (c)
	{
	case '(':
	case ')':
	case '"':
	case ';':
	case SCM_WHITE_SPACES:
	case SCM_LINE_INCREMENTORS:
	  if (weird)
	    goto default_case;

	  scm_gen_ungetc (c, port);
	case EOF:
	eof_case:
	  p[j] = 0;
	  return j;
	case '\\':
	  if (!weird)
	    goto default_case;
	  else
	    {
	      c = scm_gen_getc (port);
	      if (c == EOF)
		goto eof_case;
	      else
		goto default_case;
	    }
	case '}':
	  if (!weird)
	    goto default_case;

	  c = scm_gen_getc (port);
	  if (c == '#')
	    {
	      p[j] = 0;
	      return j;
	    }
	  else
	    {
	      scm_gen_ungetc (c, port);
	      c = '}';
	      goto default_case;
	    }

	default:
	default_case:
	  {
	    c = (case_i ? scm_downcase(c) : c);
	    if (SCM_PORT_REPRESENTATION(port) == scm_regular_port)
	      {
		p[j] = c;
		++j;
	      }
	    else
	      {
		int len;
		len = xwctomb (p + j, c);
		if (len == 0)
		  len = 1;
		SCM_ASSERT (len > 0, SCM_MAKINUM (c), "bogus char", "read");
		j += len;
	      }
	  }

	}
    }
}
#ifdef _UNICOS
_Pragma ("opt");		/* # pragma _CRI opt */
#endif

#ifdef __STDC__
SCM 
scm_lreadparen (SCM * tok_buf, SCM port, char *name, int case_i, SCM sharp, SCM *copy)
#else
SCM 
scm_lreadparen (tok_buf, port, name, case_i, sharp, SCM *copy)
     SCM *tok_buf;
     SCM port;
     char *name;
     int case_i;
     SCM sharp;
#endif
{
  SCM tmp;
  SCM tl;
  SCM ans;
  int c;

  c = scm_flush_ws (port, name);
  if (')' == c)
    return SCM_EOL;
  scm_gen_ungetc (c, port);
  if (scm_i_dot == (tmp = scm_lreadr (tok_buf, port, case_i, sharp, copy)))
    {
      ans = scm_lreadr (tok_buf, port, case_i, sharp, copy);
    closeit:
      if (')' != (c = scm_flush_ws (port, name)))
	scm_wta (SCM_UNDEFINED, "missing close paren", "");
      return ans;
    }
  ans = tl = scm_cons (tmp, SCM_EOL);
  while (')' != (c = scm_flush_ws (port, name)))
    {
      scm_gen_ungetc (c, port);
      if (scm_i_dot == (tmp = scm_lreadr (tok_buf, port, case_i, sharp, copy)))
	{
	  SCM_CDR (tl) = scm_lreadr (tok_buf, port, case_i, sharp, copy);
	  goto closeit;
	}
      tl = (SCM_CDR (tl) = scm_cons (tmp, SCM_EOL));
    }
  return ans;
}

#ifdef __STDC__
SCM 
scm_lreadrecparen (SCM * tok_buf, SCM port, char *name, int case_i, SCM sharp, SCM *copy)
#else
SCM 
scm_lreadrecparen (tok_buf, port, name, case_i, sharp, copy)
     SCM *tok_buf;
     SCM port;
     char *name;
     int case_i;
     SCM sharp;
     SCM *copy;
#endif
{
  register int c;
  register SCM tmp;
  register SCM tl, tl2;
  SCM ans, ans2;
  /* Need to capture line and column numbers here. */
  int line = SCM_LINUM (port);
  int column = SCM_COL (port) - 1;

  c = scm_flush_ws (port, name);
  if (')' == c)
    return SCM_EOL;
  scm_gen_ungetc (c, port);
  if (scm_i_dot == (tmp = scm_lreadr (tok_buf, port, case_i, sharp, copy)))
    {
      ans = scm_lreadr (tok_buf, port, case_i, sharp, copy);
      if (')' != (c = scm_flush_ws (port, name)))
	scm_wta (SCM_UNDEFINED, "missing close paren", "");
      return ans;
    }
  /* Build the head of the list structure. */
  ans = tl = scm_cons (tmp, SCM_EOL);
  if (SCM_COPY_SOURCE_P)
    ans2 = tl2 = scm_cons (SCM_NIMP (tmp) && SCM_CONSP (tmp)
			   ? *copy
			   : tmp,
			   SCM_EOL);
  while (')' != (c = scm_flush_ws (port, name)))
    {
      scm_gen_ungetc (c, port);
      if (scm_i_dot == (tmp = scm_lreadr (tok_buf, port, case_i, sharp, copy)))
	{
	  SCM_SETCDR (tl, tmp = scm_lreadr (tok_buf, port, case_i, sharp, copy));
	  if (SCM_COPY_SOURCE_P)
	    SCM_SETCDR (tl2, scm_cons (SCM_NIMP (tmp) && SCM_CONSP (tmp)
				       ? *copy
				       : tmp,
				       SCM_EOL));
	  if (')' != (c = scm_flush_ws (port, name)))
	    scm_wta (SCM_UNDEFINED, "missing close paren", "");
	  goto exit;
	}
      tl = SCM_SETCDR (tl, scm_cons (tmp, SCM_EOL));
      if (SCM_COPY_SOURCE_P)
	tl2 = SCM_SETCDR (tl2, scm_cons (SCM_NIMP (tmp) && SCM_CONSP (tmp)
					 ? *copy
					 : tmp,
					 SCM_EOL));
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





#ifdef __STDC__
void
scm_init_read (void)
#else
void
scm_init_read ()
#endif
{
#ifdef READER_EXTENSIONS
  scm_init_opts (scm_read_options, scm_read_opts, SCM_N_READ_OPTIONS);
#endif
#include "read.x"
}

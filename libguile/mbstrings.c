

/*	Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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
#include "unif.h"
#include "genio.h"
#include "read.h"

#include "mbstrings.h"


SCM_PROC(s_multi_byte_string_p, "multi-byte-string?", 1, 0, 0, scm_multi_byte_string_p);

SCM
scm_multi_byte_string_p (obj)
     SCM obj;
{
  return (SCM_MB_STRINGP (obj)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}



SCM
scm_regular_string_p (obj)
     SCM obj;
{
  return (SCM_REGULAR_STRINGP (obj)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM_PROC(s_list_to_multi_byte_string, "list->multi-byte-string", 1, 0, 0, scm_multi_byte_string);
SCM_PROC(s_multi_byte_string, "multi-byte-string", 0, 0, 1, scm_multi_byte_string);

SCM
scm_multi_byte_string (chrs)
     SCM chrs;
{
  SCM res;
  register char *data;
  long i;
  long byte_len;

  i = scm_ilength (chrs);
  SCM_ASSERT (i >= 0, chrs, SCM_ARG1, s_multi_byte_string);
  i = i * XMB_CUR_MAX;
  res = scm_makstr (i, 0);
  SCM_SETLENGTH (res, SCM_LENGTH (res), scm_tc7_mb_string);
  data = SCM_CHARS (res);
  byte_len = 0;
  xwctomb (0, 0);
  while (i && SCM_NNULLP (chrs))
    {
      int used;
      SCM ch;

      ch = SCM_CAR (chrs);
      SCM_ASSERT (SCM_ICHRP (ch), chrs, SCM_ARG1, s_multi_byte_string);
      used = xwctomb (data + byte_len,  SCM_ICHR (ch));
      SCM_ASSERT (used >= 0, chrs, SCM_ARG1, s_multi_byte_string);
      byte_len += (used ? used : 1);
      chrs = SCM_CDR (chrs);
      --i;
    }
  res = scm_vector_set_length_x (res, SCM_MAKINUM (byte_len));
  return res;
}


int
scm_mb_ilength (data, size)
     unsigned char * data;
     int size;
{
  int pos;
  int len;
  
  len = 0;
  pos = 0;
  xmblen (0, 0);
  while (pos < size)
    {
      int inc;

      inc = xmblen (data + pos, size - pos);
      if (inc == 0)
	++inc;
      
      if (inc < 0)
	return -1;

      ++len;
      pos += inc;
    }

  return len;
}

SCM_PROC(s_multi_byte_string_length, "multi-byte-string-length", 1, 0, 0, scm_multi_byte_string_length);

SCM
scm_multi_byte_string_length (str)
     SCM str;
{
  int size;
  int len;
  unsigned char * data;

  SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, SCM_ARG1, s_multi_byte_string_length);

  data = SCM_ROCHARS (str);
  size = SCM_ROLENGTH (str);
  len = scm_mb_ilength (data, size);
  SCM_ASSERT (len >= 0, str, SCM_ARG1, s_multi_byte_string_length);
  return SCM_MAKINUM (len);
}


SCM_PROC(s_symbol_multi_byte_p, "symbol-multi-byte?", 1, 0, 0, scm_symbol_multi_byte_p);

SCM
scm_symbol_multi_byte_p (symbol)
     SCM symbol;
{
  return SCM_SYMBOL_MULTI_BYTE_STRINGP(symbol);
}

SCM_PROC(s_set_symbol_multi_byte_x, "set-symbol-multi-byte!", 2, 0, 0, scm_set_symbol_multi_byte_x);

SCM
scm_set_symbol_multi_byte_x (symbol, val)
     SCM symbol;
     SCM val;
{
  if (SCM_TYP7 (symbol) == scm_tc7_msymbol)
    {
      SCM_SYMBOL_MULTI_BYTE_STRINGP(symbol) = (SCM_FALSEP (val)
						   ? SCM_BOOL_F
						   : SCM_BOOL_T);
    }
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_regular_port_p, "regular-port?", 1, 0, 0, scm_regular_port_p);

SCM 
scm_regular_port_p (p)
     SCM p;
{
  return (SCM_PORT_REPRESENTATION(p) == scm_regular_port
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM_PROC(s_regular_port_x, "regular-port!", 1, 0, 0, scm_regular_port_x);

SCM 
scm_regular_port_x (p)
     SCM p;
{
  SCM_PORT_REPRESENTATION(p) = scm_regular_port;
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_multi_byte_port_p, "multi-byte-port?", 1, 0, 0, scm_multi_byte_port_p);

SCM 
scm_multi_byte_port_p (p)
     SCM p;
{
  return (SCM_PORT_REPRESENTATION(p) == scm_mb_port
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM_PROC(s_multi_byte_port_x, "multi-byte-port!", 1, 0, 0, scm_multi_byte_port_x);

SCM 
scm_multi_byte_port_x (p)
     SCM p;
{
  SCM_PORT_REPRESENTATION(p) = scm_mb_port;
  return SCM_UNSPECIFIED;
}


SCM_PROC(s_wide_character_port_p, "wide-character-port?", 1, 0, 0, scm_wide_character_port_p);

SCM 
scm_wide_character_port_p (p)
     SCM p;
{
  return (SCM_PORT_REPRESENTATION(p) == scm_wchar_port
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM_PROC(s_wide_character_port_x, "wide-character-port!", 1, 0, 0, scm_wide_character_port_x);

SCM 
scm_wide_character_port_x (p)
     SCM p;
{
  SCM_PORT_REPRESENTATION(p) = scm_wchar_port;
  return SCM_UNSPECIFIED;
}






void
scm_put_wchar (c, port, writing)
     int c;
     SCM port;
     int writing;
{
  if (writing)
    scm_gen_puts (scm_regular_string, "#\\", port);
  switch (SCM_PORT_REPRESENTATION (port))
    {
    case scm_regular_port:
      {
	if (c < 256)
	  {
	    if (!writing)
	      scm_gen_putc ((unsigned char)c, port);
	    else if ((c <= ' ') && scm_charnames[c])
	      scm_gen_puts (scm_regular_string, scm_charnames[c], port);
	    else if (c > '\177')
	      scm_intprint (c, 8, port);
	    else
	      scm_gen_putc ((int) c, port);
	  }
	else
	  {
	  print_octal:
	    if (!writing)
	      scm_gen_putc ('\\', port);
	    scm_intprint (c, 8, port);
	  }
	break;
      }

    case scm_mb_port:
      {
	char buf[256];
	int len;

	if (XMB_CUR_MAX > sizeof (buf))
	  goto print_octal;

	len = xwctomb (buf, c);

	if (len < 0)
	  goto print_octal;

	if (len == 0)
	  scm_gen_putc (0, port);
	else
	  scm_gen_putc (c, port);
	break;
      }

    case scm_wchar_port:
      {
	scm_gen_putc (c, port);
	break;
      }
    }
}






void
scm_print_mb_string (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  if (writing)
    {
      int i;
      int len;
      char * data;

      scm_gen_putc ('\"', port);
      i = 0;
      len = SCM_ROLENGTH (exp);
      data = SCM_ROCHARS (exp);

      while (i < len)
	{
	  xwchar_t c;
	  int inc;

	  inc = xmbtowc (&c, data + i, len - i);
	  if (inc == 0)
	    inc = 1;
	  if (inc < 0)
	    {
	      inc = 1;
	      c = data[i];
	    }
	  i += inc;
	  switch (c)
	    {
	    case '\"':
	    case '\\':
	      scm_gen_putc ('\\', port);
	    default:
	      scm_gen_putc (c, port);
	    }
	}
      scm_gen_putc ('\"', port);
    }
  else
    scm_gen_write (scm_mb_string, SCM_ROCHARS (exp), SCM_ROLENGTH (exp), port);
}



void
scm_print_mb_symbol (exp, port)
     SCM exp;
     SCM port;
{
  int pos;
  int end;
  int len;
  char * str;
  int weird;
  int maybe_weird;
  int mw_pos;
  int inc;
  xwchar_t c;

  len = SCM_LENGTH (exp);
  str = SCM_CHARS (exp);
  scm_remember (&exp);
  pos = 0;
  weird = 0;
  maybe_weird = 0;

  for (end = pos; end < len; end += inc)
    {
      inc = xmbtowc (&c, str + end, len - end);
      if (inc < 0)
	{
	  inc = 1;
	  c = str[end];
	  goto weird_handler;
	}
      if (inc == 0)
	{
	  inc = 1;
	  goto weird_handler;
	}
      switch (c)
	{
#ifdef BRACKETS_AS_PARENS
	case '[':
	case ']':
#endif
	case '(':
	case ')':
	case '\"':
	case ';':
	case SCM_WHITE_SPACES:
	case SCM_LINE_INCREMENTORS:
	weird_handler:
	  if (maybe_weird)
	    {
	      end = mw_pos;
	      maybe_weird = 0;
	    }
	  if (!weird)
	    {
	      scm_gen_write (scm_regular_string, "#{", 2, port);
	      weird = 1;
	    }
	  if (pos < end)
	    {
	      int q;
	      int qinc;

	      q = pos;
	      while (q < end)
		{
		  qinc = xmbtowc (&c, str + q, end - q);
		  if (inc <= 0)
		    {
		      inc = 1;
		      c = str[q];
		    }
		  scm_gen_putc (c, port);
		  q += qinc;
		}
	    }
	  {
	    char buf[2];
	    buf[0] = '\\';
	    buf[1] = str[end];
	    scm_gen_write (scm_regular_string, buf, 2, port);
	  }
	  pos = end + 1;
	  break;
	case '\\':
	  if (weird)
	    goto weird_handler;
	  if (!maybe_weird)
	    {
	      maybe_weird = 1;
	      mw_pos = pos;
	    }
	  break;
	case '}':
	case '#':
	  if (weird)
	    goto weird_handler;
	  break;
	default:
	  break;
	}
    }
  if (pos < end)
    {
      int q;
      int qinc;
      q = pos;
      while (q < end)
	{
	  qinc = xmbtowc (&c, str + q, end - q);
	  if (inc <= 0)
	    inc = 1;
	  scm_gen_putc (c, port);
	  q += qinc;
	}
    }
  if (weird)
    scm_gen_write (scm_regular_string, "}#", 2, port);
}





void
scm_init_mbstrings ()
{
#include "mbstrings.x"
}


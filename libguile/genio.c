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
 * If you do not wish that, delete this exception notice.  */

#include "extchrs.h"
#include <stdio.h>
#include "_scm.h"
#include "chars.h"

#include "genio.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif




static void scm_putc SCM_P ((int c, SCM port));

static void 
scm_putc (c, port)
     int c;
     SCM port;
{
  scm_sizet i = SCM_PTOBNUM (port);
  SCM_SYSCALL ((scm_ptobs[i].fputc) (c, SCM_STREAM (port)));
}



static void scm_puts SCM_P ((char *s, SCM port));

static void 
scm_puts (s, port)
     char *s;
     SCM port;
{
  scm_sizet i = SCM_PTOBNUM (port);
  SCM_SYSCALL ((scm_ptobs[i].fputs) (s, SCM_STREAM (port)));
#ifdef TRANSCRIPT_SUPPORT
  if (scm_trans && (port == def_outp || port == cur_errp))
    SCM_SYSCALL (fputs (s, scm_trans));
#endif
}



static int scm_lfwrite SCM_P ((char *ptr, scm_sizet size, scm_sizet nitems, SCM port));

static int 
scm_lfwrite (ptr, size, nitems, port)
     char *ptr;
     scm_sizet size;
     scm_sizet nitems;
     SCM port;
{
  int ret;
  scm_sizet i = SCM_PTOBNUM (port);
  SCM_SYSCALL (ret = (scm_ptobs[i].fwrite(ptr, size, nitems, SCM_STREAM (port))));
#ifdef TRANSCRIPT_SUPPORT
  if (scm_trans && (port == def_outp || port == cur_errp))
    SCM_SYSCALL (fwrite (ptr, size, nitems, scm_trans));
#endif
  return ret;
}





void
scm_gen_putc (c, port)
     int c;
     SCM port;
{
  switch (SCM_PORT_REPRESENTATION (port))
    {
    case scm_regular_port:
      {
	/* Nothing good to do with extended chars here...
	 * just truncate them.
	 */
	scm_putc ((unsigned char)c, port);
	break;
      }

    case scm_mb_port:
      {
	char buf[256];
	int len;

	SCM_ASSERT (XMB_CUR_MAX < sizeof (buf), SCM_MAKICHR (c),
		"huge translation", "scm_gen_putc");

	len = xwctomb (buf, c);
	
	SCM_ASSERT ((len >= 0), SCM_MAKICHR (c), "bogus character", "scm_gen_putc");

	if (len == 0)
	  scm_putc (0, port);
	else
	  {
	    int x;
	    for (x = 0; x < len; ++x)
	      scm_putc (buf[x], port);
	  }
	break;
      }

    case scm_wchar_port:
      {
	scm_putc (((unsigned char)(c >> 8) & 0xff), port);
	scm_putc ((unsigned char)(c & 0xff), port);
	break;
      }
    }
}






void
scm_gen_puts (rep, str_data, port)
     enum scm_string_representation_type rep;
     char *str_data;
     SCM port;
{
  switch (rep)
    {

    case scm_regular_string:
      switch (SCM_PORT_REPRESENTATION (port))
	{
	case scm_regular_port:
	case scm_mb_port:
	  scm_puts (str_data, port);
	  return;
	case scm_wchar_port:
	  {
	    while (*str_data)
	      {
		scm_putc (0, port);
		scm_putc (*str_data, port);
		++str_data;
	      }
	    return;
	  }
	}

    case scm_mb_string:
      switch (SCM_PORT_REPRESENTATION (port))
	{
	case scm_regular_port:
	case scm_mb_port:
	  scm_puts (str_data, port);
	  return;
	case scm_wchar_port:
	  {
	    xwchar_t output;
	    int len;
	    int size;

	    size = strlen (str_data);
	    while (size)
	      {
		len = xmbtowc (&output, str_data, size);
		SCM_ASSERT ((len > 0), SCM_MAKINUM (*str_data),
			    "bogus character", "scm_gen_puts");
		scm_putc ((output >> 8) & 0xff, port);
		scm_putc (output & 0xff, port);
		size -= len;
		str_data += len;
	      }
	    return;
	  }
	}

    case scm_wchar_string:
      {
	xwchar_t * wstr_data;

	wstr_data = (xwchar_t *) str_data;
	switch (SCM_PORT_REPRESENTATION (port))
	  {
	  case scm_regular_port:
	    while (*wstr_data)
	      {
		scm_putc ((unsigned char) *wstr_data, port);
		++wstr_data;
	      }
	    return;

	  case scm_mb_port:
	    {
	      char buf[256];
	      SCM_ASSERT (XMB_CUR_MAX < sizeof (buf), SCM_BOOL_F,
		      "huge translation", "scm_gen_puts");

	      while (*wstr_data)
		{
		  int len;

		  len = xwctomb (buf, *wstr_data);
		  
		  SCM_ASSERT ((len > 0), SCM_MAKINUM (*wstr_data), "bogus character", "scm_gen_puts");
		  
		  {
		    int x;
		    for (x = 0; x < len; ++x)
		      scm_putc (buf[x], port);
		  }
		  ++wstr_data;
		}
	      return;
	    }
	    
	  case scm_wchar_port:
	    {
	      int len;
	      for (len = 0; wstr_data[len]; ++len)
		;
	      scm_lfwrite (str_data, sizeof (xwchar_t), len, port);
	      return;
	    }
	  }
      }
    }
}





void
scm_gen_write (rep, str_data, nitems, port)
     enum scm_string_representation_type rep;
     char *str_data;
     scm_sizet nitems;
     SCM port;
{
  /* is nitems bytes or characters in the mb_string case? */

  switch (rep)
    {
    case scm_regular_string:
      switch (SCM_PORT_REPRESENTATION (port))
	{
	case scm_regular_port:
	case scm_mb_port:
	  scm_lfwrite (str_data, 1, nitems, port);
	  return;
	case scm_wchar_port:
	  {
	    while (nitems)
	      {
		scm_putc (0, port);
		scm_putc (*str_data, port);
		++str_data;
		--nitems;
	      }
	    return;
	  }
	}

    case scm_mb_string:
      switch (SCM_PORT_REPRESENTATION (port))
	{
	case scm_regular_port:
	case scm_mb_port:
	  scm_lfwrite (str_data, 1, nitems, port);
	  return;

	case scm_wchar_port:
	  {
	    xwchar_t output;
	    int len;

	    while (nitems)
	      {
		len = xmbtowc (&output, str_data, nitems);
		SCM_ASSERT ((len > 0), SCM_MAKINUM (*str_data), "bogus character", "scm_gen_puts");
		scm_putc ((output >> 8) & 0xff, port);
		scm_putc (output & 0xff, port);
		nitems -= len;
		str_data += len;
	      }
	    return;
	  }
	}

    case scm_wchar_string:
      {
	xwchar_t * wstr_data;

	wstr_data = (xwchar_t *) str_data;
	switch (SCM_PORT_REPRESENTATION (port))
	  {
	  case scm_regular_port:
	    while (nitems)
	      {
		scm_putc ((unsigned char) *wstr_data, port);
		++wstr_data;
		--nitems;
	      }
	    return;

	  case scm_mb_port:
	    {
	      char buf[256];
	      SCM_ASSERT (XMB_CUR_MAX < sizeof (buf), SCM_BOOL_F,
		      "huge translation", "scm_gen_puts");

	      while (nitems)
		{
		  int len;

		  len = xwctomb (buf, *wstr_data);
		  
		  SCM_ASSERT ((len > 0), SCM_MAKINUM (*wstr_data), "bogus character", "scm_gen_puts");
		  
		  {
		    int x;
		    for (x = 0; x < len; ++x)
		      scm_putc (buf[x], port);
		  }
		  ++wstr_data;
		  --nitems;
		}
	      return;
	    }
	    
	  case scm_wchar_port:
	    {
	      scm_lfwrite (str_data, sizeof (xwchar_t), nitems, port);
	      return;
	    }
	  }
      }
    }
}





static int scm_getc SCM_P ((SCM port));

static int 
scm_getc (port)
     SCM port;
{
  SCM f;
  int c;
  scm_sizet i;

  f = SCM_STREAM (port);
  i = SCM_PTOBNUM (port);
  SCM_SYSCALL (c = (scm_ptobs[i].fgetc) (f));
  return c;
}


int 
scm_gen_getc (port)
     SCM port;
{
  int c;

  /* One char may be stored in the high bits of (car port) orre@nada.kth.se. */
  if (SCM_CRDYP (port))
    {
      c = SCM_CGETUN (port);
      SCM_CLRDY (port);		/* Clear ungetted char */

    return_c:
      if (c == '\n')
	{
	  SCM_INCLINE (port);
	}
      else if (c == '\t')
	{
	  SCM_TABCOL (port);
	}
      else
	{
	  SCM_INCCOL (port);
	}
      return c;
    }


  switch (SCM_PORT_REPRESENTATION (port))
    {
    case scm_regular_port:
      c = scm_getc (port);
      goto return_c;

    case scm_mb_port:
      {
	int x;
	char buf[256];

	SCM_ASSERT (XMB_CUR_MAX < sizeof (buf), SCM_BOOL_F,
		"huge translation", "scm_gen_puts");

	x = 0;
	while (1)
	  {
	    xwchar_t out;
	    c = scm_getc (port);

	    if (c == EOF)
	      return EOF;
	    
	    buf[x] = c;

	    if (xmbtowc (&out, buf, x + 1) > 0)
	      {
		c = out;
		goto return_c;
	      }

	    SCM_ASSERT (x < sizeof (buf), SCM_BOOL_F,
		    "huge translation", "scm_gen_getc");
	    ++x;
	  }
      }


    case scm_wchar_port:
      {
	int hi;
	int lo;
	hi = scm_getc (port);
	lo = (hi == EOF
	      ? EOF
	      : scm_getc (port));
	c = ((hi == EOF)
	     ? EOF
	     : ((hi << 8) | lo));
	goto return_c;
      }


    default:
      return EOF;
    }
}


void 
scm_gen_ungetc (c, port)
     int c;
     SCM port;
{
/*	SCM_ASSERT(!SCM_CRDYP(port), port, SCM_ARG2, "too many scm_gen_ungetc");*/
  SCM_CUNGET (c, port);
  if (c == '\n')
    {
      /* What should col be in this case?
       * We'll leave it at -1.
       */
      SCM_LINUM (port) -= 1;
    }
  else
    SCM_COL(port) -= 1;
}



/*	Copyright (C) 1995,1996, 1997 Free Software Foundation, Inc.
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

#include <stdio.h>
#include "_scm.h"
#include "chars.h"
#ifdef GUILE_ISELECT
#include "filesys.h"
#endif

#include "genio.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif


void 
scm_putc (c, port)
     int c;
     SCM port;
{
  scm_sizet i = SCM_PTOBNUM (port);
  SCM_SYSCALL ((scm_ptobs[i].fputc) (c, port));
}

void 
scm_puts (s, port)
     char *s;
     SCM port;
{
  scm_sizet i = SCM_PTOBNUM (port);
  SCM_SYSCALL ((scm_ptobs[i].fputs) (s, port));
#ifdef TRANSCRIPT_SUPPORT
  if (scm_trans && (port == def_outp || port == cur_errp))
    SCM_SYSCALL (fputs (s, scm_trans));
#endif
}

void 
scm_lfwrite (ptr, size, port)
     char *ptr;
     scm_sizet size;
     SCM port;
{
  scm_sizet i = SCM_PTOBNUM (port);
  SCM_SYSCALL (scm_ptobs[i].fwrite (ptr, size, 1, port));
#ifdef TRANSCRIPT_SUPPORT
  if (scm_trans && (port == def_outp || port == cur_errp))
    SCM_SYSCALL (fwrite (ptr, size, 1, scm_trans));
#endif
}


void 
scm_fflush (port)
     SCM port;
{
  scm_sizet i = SCM_PTOBNUM (port);
  (scm_ptobs[i].fflush) (port);
}



int 
scm_getc (port)
     SCM port;
{
  SCM f;
  int c;
  scm_sizet i;

  /* One char may be stored in the high bits of (car port) orre@nada.kth.se. */
  if (SCM_CRDYP (port))
    {
      c = SCM_CGETUN (port);
      SCM_CLRDY (port);         /* Clear ungetted char */
    }
  else
    {
      f = SCM_STREAM (port);
      i = SCM_PTOBNUM (port);
#ifdef GUILE_ISELECT
      if (SCM_FPORTP (port) && !scm_input_waiting_p ((FILE *) f, "scm_getc"))
	{
	  int n;
	  SELECT_TYPE readfds;
	  int fd = fileno ((FILE *) f);
	  do
	    {
	      FD_ZERO (&readfds);
	      FD_SET (fd, &readfds);
	      n = scm_internal_select (fd + 1, &readfds, NULL, NULL, NULL);
	    }
	  while (n == -1 && errno == EINTR);
	}
#endif
      SCM_SYSCALL (c = (scm_ptobs[i].fgetc) (port));
    }

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


void 
scm_ungetc (c, port)
     int c;
     SCM port;
{
/*	SCM_ASSERT(!SCM_CRDYP(port), port, SCM_ARG2, "too many scm_ungetc");*/
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


char *
scm_do_read_line (port, len)
     SCM port;
     int *len;
{
  char *s;
  scm_sizet i;

  i = SCM_PTOBNUM (port);
  SCM_SYSCALL (s = (scm_ptobs[i].fgets) (port, len));
  return s;
}


/*	Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
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



#include <stdio.h>
#include "fd.h"
#include "_scm.h"
#include "genio.h"
#include "read.h"
#include "fports.h"
#include "unif.h"
#include "chars.h"

#include "ioext.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


SCM_PROC (s_read_delimited_x, "%read-delimited!", 3, 3, 0, scm_read_delimited_x);

SCM 
scm_read_delimited_x (delims, buf, gobble, port, start, end)
     SCM delims;
     SCM buf;
     SCM gobble;
     SCM port;
     SCM start;
     SCM end;
{
  long j;
  char *cbuf;
  long cstart;
  long cend;
  int c;
  char *cdelims;
  int num_delims;

  SCM_ASSERT (SCM_NIMP (delims) && SCM_ROSTRINGP (delims),
	      delims, SCM_ARG1, s_read_delimited_x);
  cdelims = SCM_ROCHARS (delims);
  num_delims = SCM_ROLENGTH (delims);
  SCM_ASSERT (SCM_NIMP (buf) && SCM_STRINGP (buf),
	      buf, SCM_ARG2, s_read_delimited_x);
  cbuf = SCM_CHARS (buf);
  cend = SCM_LENGTH (buf);
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    {
      SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port),
		  port, SCM_ARG1, s_read_delimited_x);
    }

  if (SCM_UNBNDP (start))
    cstart = 0;
  else
    {
      cstart = scm_num2long (start,
			     (char *) SCM_ARG5, s_read_delimited_x);
      if (cstart < 0 || cstart >= cend)
	scm_out_of_range (s_read_delimited_x, start);

      if (!SCM_UNBNDP (end))
	{
	  long tend = scm_num2long (end, (char *) SCM_ARG6,
				    s_read_delimited_x);
	  if (tend <= cstart || tend > cend)
	    scm_out_of_range (s_read_delimited_x, end);
	  cend = tend;
	}
    }

  for (j = cstart; j < cend; j++)
    {  
      int k;

      c = scm_gen_getc (port);
      for (k = 0; k < num_delims; k++)
	{
	  if (cdelims[k] == c)
	    {
	      if (SCM_FALSEP (gobble))
		scm_gen_ungetc (c, port);

	      return scm_cons (SCM_MAKICHR (c),
			       scm_long2num (j - cstart));
	    }
	}
      if (c == EOF)
	return scm_cons (SCM_EOF_VAL, 
			 scm_long2num (j - cstart));

      cbuf[j] = c;
    }
  return scm_cons (SCM_BOOL_F, scm_long2num (j - cstart));
}

SCM_PROC (s_write_line, "write-line", 1, 1, 0, scm_write_line);

SCM 
scm_write_line (obj, port)
     SCM obj;
     SCM port;
{
  scm_display (obj, port);
  return scm_newline (port);
}

SCM_PROC (s_ftell, "ftell", 1, 0, 0, scm_ftell);

SCM 
scm_ftell (port)
     SCM port;
{
  long pos;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_ftell);
  SCM_SYSCALL (pos = ftell ((FILE *)SCM_STREAM (port)));
  if (pos < 0)
    scm_syserror (s_ftell);
  if (pos > 0 && SCM_CRDYP (port))
    pos--;
  return scm_long2num (pos);
}



SCM_PROC (s_fseek, "fseek", 3, 0, 0, scm_fseek);

SCM 
scm_fseek (port, offset, whence)
     SCM port;
     SCM offset;
     SCM whence;
{
  int rv;
  long loff;

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_fseek);
  loff = scm_num2long (offset, (char *)SCM_ARG2, s_fseek);
  SCM_ASSERT (SCM_INUMP (whence) && (SCM_INUM (whence) < 3) && (SCM_INUM (whence) >= 0),
	  whence, SCM_ARG3, s_fseek);
  
  SCM_CLRDY (port);			/* Clear ungetted char */
  /* Values of whence are interned in scm_init_ioext.  */
  rv = fseek ((FILE *)SCM_STREAM (port), loff, SCM_INUM (whence));
  if (rv != 0)
    scm_syserror (s_fseek);
  return SCM_UNSPECIFIED;
}



SCM_PROC (s_freopen, "freopen", 3, 0, 0, scm_freopen);

SCM 
scm_freopen (filename, modes, port)
     SCM filename;
     SCM modes;
     SCM port;
{
  FILE *f;
  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_freopen);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2,
	      s_freopen);
  SCM_DEFER_INTS;
  SCM_ASSERT (SCM_NIMP (port) && SCM_FPORTP (port), port, SCM_ARG3, s_freopen);
  SCM_SYSCALL (f = freopen (SCM_ROCHARS (filename), SCM_ROCHARS (modes),
			    (FILE *)SCM_STREAM (port)));
  if (!f)
    {
      SCM p;
      p = port;
      port = SCM_MAKINUM (errno);
      SCM_SETAND_CAR (p, ~SCM_OPN);
      scm_remove_from_port_table (p);
    }
  else
    {
      SCM_SETCAR (port, scm_tc16_fport | scm_mode_bits (SCM_ROCHARS (modes)));
      SCM_SETSTREAM (port, (SCM)f);
      SCM_SETCAR (port, scm_tc16_fport | scm_mode_bits (SCM_ROCHARS (modes)));
      if (SCM_BUF0 & SCM_CAR (port))
	scm_setbuf0 (port);
    }
  SCM_ALLOW_INTS;
  return port;
}



SCM_PROC (s_duplicate_port, "duplicate-port", 2, 0, 0, scm_duplicate_port);

SCM 
scm_duplicate_port (oldpt, modes)
     SCM oldpt;
     SCM modes;
{
  int oldfd;
  int newfd;
  FILE *f;
  SCM newpt;
  SCM_ASSERT (SCM_NIMP (oldpt) && SCM_OPPORTP (oldpt), oldpt, SCM_ARG1,
	      s_duplicate_port);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2,
	      s_duplicate_port);
  SCM_NEWCELL (newpt);
  SCM_DEFER_INTS;
  oldfd = fileno ((FILE *)SCM_STREAM (oldpt));
  if (oldfd == -1)
    scm_syserror (s_duplicate_port);
  SCM_SYSCALL (newfd = dup (oldfd));
  if (newfd == -1)
    scm_syserror (s_duplicate_port);
  f = fdopen (newfd, SCM_ROCHARS (modes));
  if (!f)
    {
      SCM_SYSCALL (close (newfd));
      scm_syserror (s_duplicate_port);
    }
  {
    struct scm_port_table * pt;
    pt = scm_add_to_port_table (newpt);
    SCM_SETPTAB_ENTRY (newpt, pt);
    SCM_SETCAR (newpt, scm_tc16_fport | scm_mode_bits (SCM_ROCHARS (modes)));
    if (SCM_BUF0 & SCM_CAR (newpt))
      scm_setbuf0 (newpt);
    SCM_SETSTREAM (newpt, (SCM)f);
    SCM_PTAB_ENTRY (newpt)->file_name = SCM_PTAB_ENTRY (oldpt)->file_name;
  }
  SCM_ALLOW_INTS;
  return newpt;
}



SCM_PROC (s_redirect_port, "redirect-port", 2, 0, 0, scm_redirect_port);

SCM 
scm_redirect_port (into_pt, from_pt)
     SCM into_pt;
     SCM from_pt;
{
  int ans, oldfd, newfd;
  SCM_DEFER_INTS;
  SCM_ASSERT (SCM_NIMP (into_pt) && SCM_OPPORTP (into_pt), into_pt, SCM_ARG1, s_redirect_port);
  SCM_ASSERT (SCM_NIMP (from_pt) && SCM_OPPORTP (from_pt), from_pt, SCM_ARG2, s_redirect_port);
  oldfd = fileno ((FILE *)SCM_STREAM (into_pt));
  if (oldfd == -1)
    scm_syserror (s_redirect_port);
  newfd = fileno ((FILE *)SCM_STREAM (from_pt));
  if (newfd == -1)
    scm_syserror (s_redirect_port);
  SCM_SYSCALL (ans = dup2 (oldfd, newfd));
  if (ans == -1)
    scm_syserror (s_redirect_port);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_fileno, "fileno", 1, 0, 0, scm_fileno);

SCM 
scm_fileno (port)
     SCM port;
{
  int fd;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_fileno);
  fd = fileno ((FILE *)SCM_STREAM (port));
  if (fd == -1)
    scm_syserror (s_fileno);
  return SCM_MAKINUM (fd);
}

SCM_PROC (s_isatty, "isatty?", 1, 0, 0, scm_isatty_p);

SCM 
scm_isatty_p (port)
     SCM port;
{
  int rv;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_isatty);
  rv = fileno ((FILE *)SCM_STREAM (port));
  if (rv == -1)
    scm_syserror (s_isatty);
  rv = isatty (rv);
  return  rv ? SCM_BOOL_T : SCM_BOOL_F;
}



SCM_PROC (s_fdopen, "fdopen", 2, 0, 0, scm_fdopen);

SCM
scm_fdopen (fdes, modes)
     SCM fdes;
     SCM modes;
{
  FILE *f;
  SCM port;
  struct scm_port_table * pt;

  SCM_ASSERT (SCM_INUMP (fdes), fdes, SCM_ARG1, s_fdopen);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2,
	      s_fdopen);
  SCM_NEWCELL (port);
  SCM_DEFER_INTS;
  f = fdopen (SCM_INUM (fdes), SCM_ROCHARS (modes));
  if (f == NULL)
    scm_syserror (s_fdopen);
  pt = scm_add_to_port_table (port);
  SCM_SETPTAB_ENTRY (port, pt);
  SCM_SETCAR (port, scm_tc16_fport | scm_mode_bits (SCM_ROCHARS (modes)));
  if (SCM_BUF0 & SCM_CAR (port))
    scm_setbuf0 (port);
  SCM_SETSTREAM (port, (SCM)f);
  SCM_ALLOW_INTS;
  return port;
}



/* Move a port's underlying file descriptor to a given value.
 * Returns  #f if fdes is already the given value.
 *          #t if fdes moved. 
 * MOVE->FDES is implemented in Scheme and calls this primitive.
 */
SCM_PROC (s_primitive_move_to_fdes, "primitive-move->fdes", 2, 0, 0, scm_primitive_move_to_fdes);

SCM
scm_primitive_move_to_fdes (port, fd)
     SCM port;
     SCM fd;
{
  FILE *stream;
  int old_fd;
  int new_fd;
  int rv;

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1, s_primitive_move_to_fdes);
  SCM_ASSERT (SCM_INUMP (fd), fd, SCM_ARG2, s_primitive_move_to_fdes);
  SCM_DEFER_INTS;
  stream = (FILE *)SCM_STREAM (port);
  old_fd = fileno (stream);
  new_fd = SCM_INUM (fd);
  if  (old_fd == new_fd)
    {
      SCM_ALLOW_INTS;
      return SCM_BOOL_F;
    }
  scm_evict_ports (new_fd);
  rv = dup2 (old_fd, new_fd);
  if (rv == -1)
    scm_syserror (s_primitive_move_to_fdes);
  scm_setfileno (stream, new_fd);
  SCM_SYSCALL (close (old_fd));  
  SCM_ALLOW_INTS;
  return SCM_BOOL_T;
}


void
scm_setfileno (fs, fd)
     FILE *fs;
     int fd;
{
#ifdef SET_FILE_FD_FIELD
  SET_FILE_FD_FIELD(fs, fd);
#else
  Configure could not guess the name of the correct field in a FILE *.

  This function needs to be ported to your system.

  SET_FILE_FD_FIELD should change the descriptor refered to by a stdio
  stream, and nothing else.

  The way to port this file is to add cases to configure.in.  Search
  that file for "SET_FILE_FD_FIELD" and follow the examples there.
#endif
}

/* Move ports with the specified file descriptor to new descriptors,
 * reseting the revealed count to 0.
 * Should be called with SCM_DEFER_INTS active.
 */

void
scm_evict_ports (fd)
     int fd;
{
  int i;

  for (i = 0; i < scm_port_table_size; i++)
    {
      if (SCM_FPORTP (scm_port_table[i]->port)
	  && fileno ((FILE *)SCM_STREAM (scm_port_table[i]->port)) == fd)
	{
	  scm_setfileno ((FILE *)SCM_STREAM (scm_port_table[i]->port), dup (fd));
	  scm_set_port_revealed_x (scm_port_table[i]->port, SCM_MAKINUM (0));
	}
    }
}

/* Return a list of ports using a given file descriptor.  */
SCM_PROC(s_fdes_to_ports, "fdes->ports", 1, 0, 0, scm_fdes_to_ports);

SCM
scm_fdes_to_ports (fd)
     SCM fd;
{
  SCM result = SCM_EOL;
  int int_fd;
  int i;
  
  SCM_ASSERT (SCM_INUMP (fd), fd, SCM_ARG1, s_fdes_to_ports);
  int_fd = SCM_INUM (fd);

  SCM_DEFER_INTS;
  for (i = 0; i < scm_port_table_size; i++)
    {
      if (SCM_FPORTP (scm_port_table[i]->port)
	  && fileno ((FILE *)SCM_STREAM (scm_port_table[i]->port)) == int_fd)
	result = scm_cons (scm_port_table[i]->port, result);
    }
  SCM_ALLOW_INTS;
  return result;
}    


void 
scm_init_ioext ()
{
  /* fseek() symbols.  */
  scm_sysintern ("SEEK_SET", SCM_MAKINUM (SEEK_SET));
  scm_sysintern ("SEEK_CUR", SCM_MAKINUM (SEEK_CUR));
  scm_sysintern ("SEEK_END", SCM_MAKINUM (SEEK_END));

#include "ioext.x"
}


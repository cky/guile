/*	Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.
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
#include <fcntl.h>
#include "_scm.h"

#include "fports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
scm_sizet fwrite ();
#endif
#ifdef HAVE_ST_BLKSIZE
#include <sys/stat.h>
#endif

#include <errno.h>

#include "iselect.h"

/* create FPORT buffer with specified sizes (or -1 to use default size or
   0 for no buffer.  */
static void
scm_fport_buffer_add (SCM port, int read_size, int write_size)
{
  struct scm_fport *fp = SCM_FSTREAM (port);
   scm_port *pt = SCM_PTAB_ENTRY (port);
  char *s_scm_fport_buffer_add = "scm_fport_buffer_add";

  if (read_size == -1 || write_size == -1)
    {
      int default_size;
#ifdef HAVE_ST_BLKSIZE
      struct stat st;
      
      if (fstat (fp->fdes, &st) == -1)
	scm_syserror (s_scm_fport_buffer_add);
      default_size = st.st_blksize;
#else
      default_size = 1024;
#endif
      if (read_size == -1)
	read_size = default_size;
      if (write_size == -1)
	write_size = default_size;
    }

  if (SCM_INPORTP (port) && read_size > 0)
    {
      pt->read_buf = malloc (read_size);
      if (pt->read_buf == NULL)
	scm_memory_error (s_scm_fport_buffer_add);
      pt->read_pos = pt->read_end = pt->read_buf;
      pt->read_buf_size = read_size;
    }
  else
    {
      pt->read_pos = pt->read_buf = pt->read_end = &pt->shortbuf;
      pt->read_buf_size = 1;
    }

  if (SCM_OUTPORTP (port) && write_size > 0)
    {
      pt->write_buf = malloc (write_size);
      if (pt->write_buf == NULL)
	scm_memory_error (s_scm_fport_buffer_add);
      pt->write_pos = pt->write_buf;
      pt->write_buf_size = write_size;
    }
  else
    {
      pt->write_buf = pt->write_pos = &pt->shortbuf;
      pt->write_buf_size = 1;
    }

  pt->write_end = pt->write_buf + pt->write_buf_size;
  if (read_size > 0 || write_size > 0)
    SCM_SETCAR (port, SCM_CAR (port) & ~SCM_BUF0);
  else
    SCM_SETCAR (port, (SCM_CAR (port) | SCM_BUF0));
}

SCM_PROC (s_setvbuf, "setvbuf", 2, 1, 0, scm_setvbuf);
SCM
scm_setvbuf (SCM port, SCM mode, SCM size)
{
  int cmode, csize;
  scm_port *pt;

  port = SCM_COERCE_OUTPORT (port);

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPFPORTP (port), port, SCM_ARG1,
	      s_setvbuf);
  SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG2, s_setvbuf);
  cmode = SCM_INUM (mode);
  if (cmode != _IONBF && cmode != _IOFBF)
    scm_out_of_range (s_setvbuf, mode);
  if (SCM_UNBNDP (size))
    {
      if (cmode == _IOFBF)
	csize = -1;
      else
	csize = 0;
    }
  else
    {
      SCM_ASSERT (SCM_INUMP (size), size, SCM_ARG3, s_setvbuf);
      csize = SCM_INUM (size);
      if (csize < 0 || (cmode == _IONBF && csize > 0))
	scm_out_of_range (s_setvbuf, size);
    }
  pt = SCM_PTAB_ENTRY (port);

  /* silently discards buffered chars.  */
  if (pt->read_buf != &pt->shortbuf)
    scm_must_free (pt->read_buf);
  if (pt->write_buf != &pt->shortbuf)
    scm_must_free (pt->write_buf);

  scm_fport_buffer_add (port, csize, csize);
  return SCM_UNSPECIFIED;
}

/* Move ports with the specified file descriptor to new descriptors,
 * reseting the revealed count to 0.
 */

void
scm_evict_ports (fd)
     int fd;
{
  int i;

  for (i = 0; i < scm_port_table_size; i++)
    {
      SCM port = scm_port_table[i]->port;

      if (SCM_FPORTP (port))
	{
	  struct scm_fport *fp = SCM_FSTREAM (port);

	  if (fp->fdes == fd)
	    {
	      fp->fdes = dup (fd);
	      if (fp->fdes == -1)
		scm_syserror ("scm_evict_ports");
	      scm_set_port_revealed_x (port, SCM_MAKINUM (0));
	    }
	}
    }
}

/* scm_open_file
 * Return a new port open on a given file.
 *
 * The mode string must match the pattern: [rwa+]** which
 * is interpreted in the usual unix way.
 *
 * Return the new port.
 */
SCM_PROC(s_open_file, "open-file", 2, 0, 0, scm_open_file);

SCM
scm_open_file (filename, modes)
     SCM filename;
     SCM modes;
{
  SCM port;
  int fdes;
  int flags = 0;
  char *file;
  char *mode;
  char *ptr;

  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename, SCM_ARG1, s_open_file);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2, s_open_file);
  if (SCM_SUBSTRP (filename))
    filename = scm_makfromstr (SCM_ROCHARS (filename), SCM_ROLENGTH (filename), 0);
  if (SCM_SUBSTRP (modes))
    modes = scm_makfromstr (SCM_ROCHARS (modes), SCM_ROLENGTH (modes), 0);

  file = SCM_ROCHARS (filename);
  mode = SCM_ROCHARS (modes);

  switch (*mode)
    {
    case 'r':
      flags |= O_RDONLY;
      break;
    case 'w':
      flags |= O_WRONLY | O_CREAT | O_TRUNC;
      break;
    case 'a':
      flags |= O_WRONLY | O_CREAT | O_APPEND;
      break;
    default:
      scm_out_of_range (s_open_file, modes);
    }
  ptr = mode + 1;
  while (*ptr != '\0')
    {
      switch (*ptr)
	{
	case '+':
	  flags = (flags & ~(O_RDONLY | O_WRONLY)) | O_RDWR;
	  break;
	case '0':  /* unbuffered: handled later.  */
	case 'b':  /* 'binary' mode: ignored.  */
	  break;
	default:
	  scm_out_of_range (s_open_file, modes);
	}
      ptr++;
    }
  SCM_SYSCALL (fdes = open (file, flags, 0666));
  if (fdes == -1)
    {
      int en = errno;

      scm_syserror_msg (s_open_file, "%s: %S",
			scm_cons (scm_makfrom0str (strerror (en)),
				  scm_cons (filename, SCM_EOL)),
			en);
    }
  port = scm_fdes_to_port (fdes, mode, filename);
  return port;
}


/* Building Guile ports from a file descriptor.  */

/* Build a Scheme port from an open file descriptor `fdes'.
   MODE indicates whether FILE is open for reading or writing; it uses
      the same notation as open-file's second argument.
   Use NAME as the port's filename.  */

SCM
scm_fdes_to_port (int fdes, char *mode, SCM name)
{
  long mode_bits = scm_mode_bits (mode);
  SCM port;
  scm_port *pt;

  SCM_NEWCELL (port);
  SCM_DEFER_INTS;
  pt = scm_add_to_port_table (port);
  SCM_SETPTAB_ENTRY (port, pt);
  SCM_SETCAR (port, (scm_tc16_fport | mode_bits));

  {
    struct scm_fport *fp
      = (struct scm_fport *) malloc (sizeof (struct scm_fport));
    if (fp == NULL)
      scm_memory_error ("scm_fdes_to_port");
    fp->fdes = fdes;
    pt->rw_random = (mode_bits & SCM_RDNG) && (mode_bits & SCM_WRTNG)
      && SCM_FDES_RANDOM_P (fdes);
    SCM_SETSTREAM (port, fp);
    if (mode_bits & SCM_BUF0)
      scm_fport_buffer_add (port, 0, 0);
    else
      scm_fport_buffer_add (port, -1, -1);
  }
  SCM_PTAB_ENTRY (port)->file_name = name;
  SCM_ALLOW_INTS;
  return port;
}


/* Check whether an fport's fdes can supply input.  */
static int
fport_input_waiting_p (SCM port)
{
  int fdes = SCM_FSTREAM (port)->fdes;

#ifdef HAVE_SELECT
  struct timeval timeout;
  SELECT_TYPE read_set;
  SELECT_TYPE write_set;
  SELECT_TYPE except_set;

  FD_ZERO (&read_set);
  FD_ZERO (&write_set);
  FD_ZERO (&except_set);

  FD_SET (fdes, &read_set);
  
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;

  if (select (SELECT_SET_SIZE,
	      &read_set, &write_set, &except_set, &timeout)
      < 0)
    scm_syserror ("fport_input_waiting_p");
  return FD_ISSET (fdes, &read_set);
#elif defined (FIONREAD)
  int remir;
  ioctl(fdes, FIONREAD, &remir);
  return remir;
#else    
  scm_misc_error ("fport_input_waiting_p",
		  "Not fully implemented on this platform",
		  SCM_EOL);
#endif
}


static int prinfport SCM_P ((SCM exp, SCM port, scm_print_state *pstate));

static int 
prinfport (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  SCM name;
  char * c;
  if (SCM_CLOSEDP (exp))
    {
      c = "file";
    }
  else
    {
      name = SCM_PTAB_ENTRY (exp)->file_name;
      if (SCM_NIMP (name) && SCM_ROSTRINGP (name))
	c = SCM_ROCHARS (name);
      else
	c = "file";
    }
    
  scm_prinport (exp, port, c);
  return !0;
}

#ifdef GUILE_ISELECT
/* thread-local block for input on fport's fdes.  */
static void
fport_wait_for_input (SCM port)
{
  int fdes = SCM_FSTREAM (port)->fdes;

  if (!fport_input_waiting_p (port))
    {
      int n;
      SELECT_TYPE readfds;
      int flags = fcntl (fdes, F_GETFL);

      if (flags == -1)
	scm_syserror ("scm_fdes_wait_for_input");
      if (!(flags & O_NONBLOCK))
	do
	  {
	    FD_ZERO (&readfds);
	    FD_SET (fdes, &readfds);
	    n = scm_internal_select (fdes + 1, &readfds, NULL, NULL, NULL);
	  }
	while (n == -1 && errno == EINTR);
    }
}
#endif

static void local_fflush (SCM port);

/* fill a port's read-buffer with a single read.
   returns the first char and moves the read_pos pointer past it.
   or returns EOF if end of file.  */
static int
fport_fill_buffer (SCM port)
{
  int count;
  scm_port *pt = SCM_PTAB_ENTRY (port);
  struct scm_fport *fp = SCM_FSTREAM (port);

#ifdef GUILE_ISELECT
  fport_wait_for_input (port);
#endif
  SCM_SYSCALL (count = read (fp->fdes, pt->read_buf, pt->read_buf_size));
  if (count == -1)
    scm_syserror ("fport_fill_buffer");
  if (count == 0)
    return EOF;
  else
    {
      pt->read_pos = pt->read_buf + 1;
      pt->read_end = pt->read_buf + count;
      return (*(pt->read_buf));
    }
}

static off_t
local_seek (SCM port, off_t offset, int whence)
{
  struct scm_fport *fp = SCM_FSTREAM (port);
  
  return lseek (fp->fdes, offset, whence);
}

static void
local_ftruncate (SCM port, off_t length)
{
  struct scm_fport *fp = SCM_FSTREAM (port);

  if (ftruncate (fp->fdes, length) == -1)
    scm_syserror ("ftruncate");
}

/* becomes 1 when process is exiting: exception handling is disabled. */
extern int terminating; 

static void
local_fflush (SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  struct scm_fport *fp = SCM_FSTREAM (port);
  char *ptr = pt->write_buf;
  int init_size = pt->write_pos - pt->write_buf;
  int remaining = init_size;

  while (remaining > 0)
    {
      int count;

      SCM_SYSCALL (count = write (fp->fdes, ptr, remaining));
      if (count < 0)
	{
	  /* error.  assume nothing was written this call, but
	     fix up the buffer for any previous successful writes.  */
	  int done = init_size - remaining;
	      
	  if (done > 0)
	    {
	      int i;

	      for (i = 0; i < remaining; i++)
		{
		  *(pt->write_buf + i) = *(pt->write_buf + done + i);
		}
	      pt->write_pos = pt->write_buf + remaining;
	    }
	  if (!terminating)
	    scm_syserror ("local_fflush");
	  else
	    {
	      const char *msg = "Error: could not flush file-descriptor ";
	      char buf[11];

	      write (2, msg, strlen (msg));
	      sprintf (buf, "%d\n", fp->fdes);
	      write (2, buf, strlen (buf));

	      count = remaining;
	    }
	}
      ptr += count;
      remaining -= count;
    }
  pt->write_pos = pt->write_buf;
  pt->rw_active = 0;
}

/* clear the read buffer and adjust the file position for unread bytes.
   this is only called if the port has rw_random set.  */
static void
local_read_flush (SCM port)
{
  struct scm_fport *fp = SCM_FSTREAM (port);
  scm_port *pt = SCM_PTAB_ENTRY (port);
  int offset = pt->read_end - pt->read_pos;

  if (SCM_CRDYP (port))
    {
      offset += SCM_N_READY_CHARS (port);
      SCM_CLRDY (port);
    }
  if (offset > 0)
    {
      pt->read_pos = pt->read_end;
      /* will throw error if unread-char used at beginning of file
	 then attempting to write.  seems correct.  */
      if (lseek (fp->fdes, -offset, SEEK_CUR) == -1)
	scm_syserror ("local_read_flush");
    }
  pt->rw_active = 0;
}

static int
local_fclose (SCM port)
{
  struct scm_fport *fp = SCM_FSTREAM (port);
  scm_port *pt = SCM_PTAB_ENTRY (port);
  int rv;

  local_fflush (port);
  SCM_SYSCALL (rv = close (fp->fdes));
  if (rv == -1 && errno != EBADF)
    scm_syserror ("local_fclose");
  if (pt->read_buf != &pt->shortbuf)
    free (pt->read_buf);
  if (pt->write_buf != &pt->shortbuf)
    free (pt->write_buf);
  free ((char *) fp);
  return rv;
}

scm_ptobfuns scm_fptob =
 {
  0,
  local_fclose,
  prinfport,
  0,
  local_fflush,
  local_read_flush,
  local_fclose,
  fport_fill_buffer,
  local_seek,
  local_ftruncate,
  fport_input_waiting_p,
};

void
scm_init_fports ()
{
#include "fports.x"
  scm_sysintern ("_IOFBF", SCM_MAKINUM (_IOFBF));
  scm_sysintern ("_IOLBF", SCM_MAKINUM (_IOLBF));
  scm_sysintern ("_IONBF", SCM_MAKINUM (_IONBF));
}

/*	Copyright (C) 1995,1996,1997,1998,1999, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include <fcntl.h>
#include "libguile/_scm.h"
#include "libguile/strings.h"
#include "libguile/validate.h"
#include "libguile/gc.h"

#include "libguile/fports.h"

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

#include "libguile/iselect.h"

/* default buffer size, used if the O/S won't supply a value.  */
static const int default_buffer_size = 1024;

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
      
      default_size = (fstat (fp->fdes, &st) == -1) ? default_buffer_size
	: st.st_blksize;
#else
      default_size = default_buffer_size;
#endif
      if (read_size == -1)
	read_size = default_size;
      if (write_size == -1)
	write_size = default_size;
    }

  if (SCM_INPUT_PORT_P (port) && read_size > 0)
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

  if (SCM_OUTPUT_PORT_P (port) && write_size > 0)
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
    SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) & ~SCM_BUF0);
  else
    SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) | SCM_BUF0);
}

SCM_DEFINE (scm_setvbuf, "setvbuf", 2, 1, 0, 
            (SCM port, SCM mode, SCM size),
	    "Set the buffering mode for @var{port}.  @var{mode} can be:\n"
	    "@table @code\n"
	    "@item _IONBF\n"
	    "non-buffered\n"
	    "@item _IOLBF\n"
	    "line buffered\n"
	    "@item _IOFBF\n"
	    "block buffered, using a newly allocated buffer of @var{size} bytes.\n"
	    "If @var{size} is omitted, a default size will be used.\n"
	    "@end table")
#define FUNC_NAME s_scm_setvbuf
{
  int cmode, csize;
  scm_port *pt;

  port = SCM_COERCE_OUTPORT (port);

  SCM_VALIDATE_OPFPORT (1,port);
  SCM_VALIDATE_INUM_COPY (2,mode,cmode);
  if (cmode != _IONBF && cmode != _IOFBF && cmode != _IOLBF)
    scm_out_of_range (FUNC_NAME, mode);

  if (cmode == _IOLBF)
    {
      SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) | SCM_BUFLINE);
      cmode = _IOFBF;
    }
  else
    {
      SCM_SET_CELL_WORD_0 (port, SCM_CELL_WORD_0 (port) ^ SCM_BUFLINE);
    }

  if (SCM_UNBNDP (size))
    {
      if (cmode == _IOFBF)
	csize = -1;
      else
	csize = 0;
    }
  else
    {
      SCM_VALIDATE_INUM_COPY (3,size,csize);
      if (csize < 0 || (cmode == _IONBF && csize > 0))
	scm_out_of_range (FUNC_NAME, size);
    }

  pt = SCM_PTAB_ENTRY (port);

  /* silently discards buffered chars.  */
  if (pt->read_buf != &pt->shortbuf)
    free (pt->read_buf);
  if (pt->write_buf != &pt->shortbuf)
    free (pt->write_buf);

  scm_fport_buffer_add (port, csize, csize);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Move ports with the specified file descriptor to new descriptors,
 * reseting the revealed count to 0.
 */

void
scm_evict_ports (int fd)
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
SCM_DEFINE (scm_open_file, "open-file", 2, 0, 0,
           (SCM filename, SCM modes),
	    "Open the file whose name is @var{string}, and return a port\n"
	    "representing that file.  The attributes of the port are\n"
	    "determined by the @var{mode} string.  The way in \n"
	    "which this is interpreted is similar to C stdio:\n\n"
	    "The first character must be one of the following:\n\n"
	    "@table @samp\n"
	    "@item r\n"
	    "Open an existing file for input.\n"
	    "@item w\n"
	    "Open a file for output, creating it if it doesn't already exist\n"
	    "or removing its contents if it does.\n"
	    "@item a\n"
	    "Open a file for output, creating it if it doesn't already exist.\n"
	    "All writes to the port will go to the end of the file.\n"
	    "The \"append mode\" can be turned off while the port is in use\n"
	    "@pxref{Ports and File Descriptors, fcntl}\n"
	    "@end table\n\n"
	    "The following additional characters can be appended:\n\n"
	    "@table @samp\n"
	    "@item +\n"
	    "Open the port for both input and output.  E.g., @code{r+}: open\n"
	    "an existing file for both input and output.\n"
	    "@item 0\n"
	    "Create an \"unbuffered\" port.  In this case input and output operations\n"
	    "are passed directly to the underlying port implementation without\n"
	    "additional buffering.  This is likely to slow down I/O operations.\n"
	    "The buffering mode can be changed while a port is in use\n"
	    "@pxref{Ports and File Descriptors, setvbuf}\n"
	    "@item l\n"
	    "Add line-buffering to the port.  The port output buffer will be\n"
	    "automatically flushed whenever a newline character is written.\n"
	    "@end table\n\n"
	    "In theory we could create read/write ports which were buffered in one\n"
	    "direction only.  However this isn't included in the current interfaces.\n\n"
	    "If a file cannot be opened with the access requested,\n"
	    "@code{open-file} throws an exception.")
#define FUNC_NAME s_scm_open_file
{
  SCM port;
  int fdes;
  int flags = 0;
  char *file;
  char *mode;
  char *ptr;

  SCM_VALIDATE_STRING (1, filename);
  SCM_VALIDATE_STRING (2, modes);
  SCM_STRING_COERCE_0TERMINATION_X (filename);
  SCM_STRING_COERCE_0TERMINATION_X (modes);

  file = SCM_STRING_CHARS (filename);
  mode = SCM_STRING_CHARS (modes);

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
      scm_out_of_range (FUNC_NAME, modes);
    }
  ptr = mode + 1;
  while (*ptr != '\0')
    {
      switch (*ptr)
	{
	case '+':
	  flags = (flags & ~(O_RDONLY | O_WRONLY)) | O_RDWR;
	  break;
	case 'b':
#if defined (O_BINARY)
	  flags |= O_BINARY;
#endif
	  break;
	case '0':  /* unbuffered: handled later.  */
	case 'l':  /* line buffered: handled during output.  */
	  break;
	default:
	  scm_out_of_range (FUNC_NAME, modes);
	}
      ptr++;
    }
  SCM_SYSCALL (fdes = open (file, flags, 0666));
  if (fdes == -1)
    {
      int en = errno;

      SCM_SYSERROR_MSG ("~A: ~S",
			scm_cons (scm_makfrom0str (strerror (en)),
				  scm_cons (filename, SCM_EOL)), en);
    }
  port = scm_fdes_to_port (fdes, mode, filename);
  return port;
}
#undef FUNC_NAME


/* Building Guile ports from a file descriptor.  */

/* Build a Scheme port from an open file descriptor `fdes'.
   MODE indicates whether FILE is open for reading or writing; it uses
      the same notation as open-file's second argument.
   NAME is a string to be used as the port's filename.
*/
SCM
scm_fdes_to_port (int fdes, char *mode, SCM name)
#define FUNC_NAME "scm_fdes_to_port"
{
  long mode_bits = scm_mode_bits (mode);
  SCM port;
  scm_port *pt;
  int flags;

  /* test that fdes is valid.  */
  flags = fcntl (fdes, F_GETFL, 0);
  if (flags == -1)
    SCM_SYSERROR;
  flags &= O_ACCMODE;
  if (flags != O_RDWR
      && ((flags != O_WRONLY && (mode_bits & SCM_WRTNG))
	  || (flags != O_RDONLY && (mode_bits & SCM_RDNG))))
    {
      SCM_MISC_ERROR ("requested file mode not available on fdes", SCM_EOL);
    }

  SCM_NEWCELL (port);
  SCM_DEFER_INTS;
  pt = scm_add_to_port_table (port);
  SCM_SETPTAB_ENTRY (port, pt);
  SCM_SET_CELL_TYPE (port, (scm_tc16_fport | mode_bits));

  {
    struct scm_fport *fp
      = (struct scm_fport *) malloc (sizeof (struct scm_fport));
    if (fp == NULL)
      SCM_MEMORY_ERROR;
    fp->fdes = fdes;
    pt->rw_random = SCM_FDES_RANDOM_P (fdes);
    SCM_SETSTREAM (port, fp);
    if (mode_bits & SCM_BUF0)
      scm_fport_buffer_add (port, 0, 0);
    else
      scm_fport_buffer_add (port, -1, -1);
  }
  SCM_SET_FILENAME (port, name);
  SCM_ALLOW_INTS;
  return port;
}
#undef FUNC_NAME

/* Return a lower bound on the number of bytes available for input.  */
static int
fport_input_waiting (SCM port)
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
    scm_syserror ("fport_input_waiting");
  return FD_ISSET (fdes, &read_set) ? 1 : 0;
#elif defined (FIONREAD)
  int remir;
  ioctl(fdes, FIONREAD, &remir);
  return remir;
#else    
  scm_misc_error ("fport_input_waiting",
		  "Not fully implemented on this platform",
		  SCM_EOL);
#endif
}


static int 
fport_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<", port);
  scm_print_port_mode (exp, port);    
  if (SCM_OPFPORTP (exp))
    {
      int fdes;
      SCM name = SCM_FILENAME (exp);
      if (SCM_STRINGP (name) || SCM_SYMBOLP (name))
	scm_display (name, port);
      else
	scm_puts (SCM_PTOBNAME (SCM_PTOBNUM (exp)), port);
      scm_putc (' ', port);
      fdes = (SCM_FSTREAM (exp))->fdes;
      
      if (isatty (fdes))
	scm_puts (ttyname (fdes), port);
      else
	scm_intprint (fdes, 10, port);
    }
  else
    {
      scm_puts (SCM_PTOBNAME (SCM_PTOBNUM (exp)), port);
      scm_putc (' ', port);
      scm_intprint (SCM_UNPACK (SCM_CDR (exp)), 16, port);
    }
  scm_putc ('>', port);
  return 1;
}

#ifdef GUILE_ISELECT
/* thread-local block for input on fport's fdes.  */
static void
fport_wait_for_input (SCM port)
{
  int fdes = SCM_FSTREAM (port)->fdes;

  if (!fport_input_waiting (port))
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

static void fport_flush (SCM port);

/* fill a port's read-buffer with a single read.
   returns the first char and moves the read_pos pointer past it.
   or returns EOF if end of file.  */
static int
fport_fill_input (SCM port)
{
  int count;
  scm_port *pt = SCM_PTAB_ENTRY (port);
  struct scm_fport *fp = SCM_FSTREAM (port);

#ifdef GUILE_ISELECT
  fport_wait_for_input (port);
#endif
  SCM_SYSCALL (count = read (fp->fdes, pt->read_buf, pt->read_buf_size));
  if (count == -1)
    scm_syserror ("fport_fill_input");
  if (count == 0)
    return EOF;
  else
    {
      pt->read_pos = pt->read_buf;
      pt->read_end = pt->read_buf + count;
      return *pt->read_buf;
    }
}

static off_t
fport_seek (SCM port, off_t offset, int whence)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  struct scm_fport *fp = SCM_FSTREAM (port);
  off_t rv;
  off_t result;

  if (pt->rw_active == SCM_PORT_WRITE)
    {
      if (offset != 0 || whence != SEEK_CUR)
	{
	  fport_flush (port);
	  result = rv = lseek (fp->fdes, offset, whence);
	}
      else
	{
	  /* read current position without disturbing the buffer.  */
	  rv = lseek (fp->fdes, offset, whence);
	  result = rv + (pt->write_pos - pt->write_buf);
	}
    }
  else if (pt->rw_active == SCM_PORT_READ)
    {
      if (offset != 0 || whence != SEEK_CUR)
	{
	  /* could expand to avoid a second seek.  */
	  scm_end_input (port);
	  result = rv = lseek (fp->fdes, offset, whence);
	}
      else
	{
	  /* read current position without disturbing the buffer
	     (particularly the unread-char buffer).  */
	  rv = lseek (fp->fdes, offset, whence);
	  result = rv - (pt->read_end - pt->read_pos);

	  if (pt->read_buf == pt->putback_buf)
	    result -= pt->saved_read_end - pt->saved_read_pos;
	}
    }
  else /* SCM_PORT_NEITHER */
    {
      result = rv = lseek (fp->fdes, offset, whence);      
    }

  if (rv == -1)
    scm_syserror ("fport_seek");

  return result;
}

static void
fport_truncate (SCM port, off_t length)
{
  struct scm_fport *fp = SCM_FSTREAM (port);

  if (ftruncate (fp->fdes, length) == -1)
    scm_syserror ("ftruncate");
}

static void
fport_write (SCM port, const void *data, size_t size)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->write_buf == &pt->shortbuf)
    {
      /* "unbuffered" port.  */
      int fdes = SCM_FSTREAM (port)->fdes;

      if (write (fdes, data, size) == -1)
	scm_syserror ("fport_write");
    }
  else 
    {
      const char *input = (char *) data;
      size_t remaining = size;

      while (remaining > 0)
	{
	  int space = pt->write_end - pt->write_pos;
	  int write_len = (remaining > space) ? space : remaining;

	  memcpy (pt->write_pos, input, write_len);
	  pt->write_pos += write_len;
	  remaining -= write_len;
	  input += write_len;
	  if (write_len == space)
	    fport_flush (port);
	}

      /* handle line buffering.  */
      if ((SCM_CELL_WORD_0 (port) & SCM_BUFLINE) && memchr (data, '\n', size))
	fport_flush (port);
    }
}

/* becomes 1 when process is exiting: normal exception handling won't
   work by this time.  */
extern int terminating; 

static void
fport_flush (SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  struct scm_fport *fp = SCM_FSTREAM (port);
  unsigned char *ptr = pt->write_buf;
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
	  if (terminating)
	    {
	      const char *msg = "Error: could not flush file-descriptor ";
	      char buf[11];

	      write (2, msg, strlen (msg));
	      sprintf (buf, "%d\n", fp->fdes);
	      write (2, buf, strlen (buf));

	      count = remaining;
	    }
	  else if (scm_gc_running_p)
	    {
	      /* silently ignore the error.  scm_error would abort if we
		 called it now.  */
	      count = remaining;
	    }
	  else
	    scm_syserror ("fport_flush");
	}
      ptr += count;
      remaining -= count;
    }
  pt->write_pos = pt->write_buf;
  pt->rw_active = SCM_PORT_NEITHER;
}

/* clear the read buffer and adjust the file position for unread bytes. */
static void
fport_end_input (SCM port, int offset)
{
  struct scm_fport *fp = SCM_FSTREAM (port);
  scm_port *pt = SCM_PTAB_ENTRY (port);
  
  offset += pt->read_end - pt->read_pos;

  if (offset > 0)
    {
      pt->read_pos = pt->read_end;
      /* will throw error if unread-char used at beginning of file
	 then attempting to write.  seems correct.  */
      if (lseek (fp->fdes, -offset, SEEK_CUR) == -1)
	scm_syserror ("fport_end_input");
    }
  pt->rw_active = SCM_PORT_NEITHER;
}

static int
fport_close (SCM port)
{
  struct scm_fport *fp = SCM_FSTREAM (port);
  scm_port *pt = SCM_PTAB_ENTRY (port);
  int rv;

  fport_flush (port);
  SCM_SYSCALL (rv = close (fp->fdes));
  if (rv == -1 && errno != EBADF)
    {
      if (scm_gc_running_p)
	/* silently ignore the error.  scm_error would abort if we
	   called it now.  */
	;
      else
	scm_syserror ("fport_close");
    }
  if (pt->read_buf == pt->putback_buf)
    pt->read_buf = pt->saved_read_buf;
  if (pt->read_buf != &pt->shortbuf)
    free (pt->read_buf);
  if (pt->write_buf != &pt->shortbuf)
    free (pt->write_buf);
  free ((char *) fp);
  return rv;
}

static scm_sizet
fport_free (SCM port)
{
  fport_close (port);
  return 0;
}

void scm_make_fptob (void); /* Called from ports.c */

void
scm_make_fptob ()
{
  long tc = scm_make_port_type ("file", fport_fill_input, fport_write);
  scm_set_port_free            (tc, fport_free);
  scm_set_port_print           (tc, fport_print);
  scm_set_port_flush           (tc, fport_flush);
  scm_set_port_end_input       (tc, fport_end_input);
  scm_set_port_close           (tc, fport_close);
  scm_set_port_seek            (tc, fport_seek);
  scm_set_port_truncate        (tc, fport_truncate);
  scm_set_port_input_waiting   (tc, fport_input_waiting);
}

void
scm_init_fports ()
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/fports.x"
#endif
  scm_sysintern ("_IOFBF", SCM_MAKINUM (_IOFBF));
  scm_sysintern ("_IOLBF", SCM_MAKINUM (_IOLBF));
  scm_sysintern ("_IONBF", SCM_MAKINUM (_IONBF));
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

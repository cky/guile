/*	Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
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
#include "markers.h"

#include "fports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
scm_sizet fwrite ();
#endif


/* Port direction --- handling el cheapo stdio implementations.

   Guile says that when you've got a port that's both readable and
   writable, like a socket, why then, by gum, you can read from it and
   write to it!  However, most standard I/O implementations make
   cheezy caveats like this:

	When a file is opened for update, both input and output  may
	be done on the resulting stream.  However, output may not be
	directly followed by input without an intervening  fflush(),
	fseek(),  fsetpos(),  or  rewind(),  and  input  may  not be
	directly followed by output without an intervening  fseek(),
	fsetpos(),   or   rewind(),   or  an  input  operation  that
	encounters end-of-file.
		   -- the Solaris fdopen(3S) man page

   I think this behavior is permitted by the ANSI C standard.

   So we made the implementation more complex, so what the user sees
   remains simple.  When we have a Guile port based on a stdio stream
   (this source file's specialty), we keep track of whether it was
   last written to, read from, or whether it is in a safe state for
   both operations.  Each port operation function just checks the
   state of the port before each operation, and does the required
   magic if necessary.

   We use two bits in the CAR of the port, FPORT_READ_SAFE and
   FPORT_WRITE_SAFE, to indicate what operations the underlying stdio
   stream could correctly perform next.  You're not allowed to clear
   them both at the same time, but both can be set --- for example, if
   the stream has just been opened, or flushed, or had its position
   changed.

   It's possible for a port to have neither bit set, if we receive a
   FILE * pointer in an unknown state; this code should handle that
   gracefully.  */

#define FPORT_READ_SAFE  (1L << 24)
#define FPORT_WRITE_SAFE (2L << 24)

#define FPORT_ALL_OKAY(port) \
     (SCM_SETOR_CAR (port, (FPORT_READ_SAFE | FPORT_WRITE_SAFE)))

static inline void
pre_read (SCM port)
{
  if (! (SCM_CAR (port) & FPORT_READ_SAFE))
    fflush ((FILE *)SCM_STREAM (port));

  /* We've done the flush, so reading is safe.
     Assuming that we're going to do a read next, writing will not be
     safe by the time we're done.  */
  SCM_SETOR_CAR  (port,  FPORT_READ_SAFE);
  SCM_SETAND_CAR (port, ~FPORT_WRITE_SAFE);
  
}

static inline void
pre_write (SCM port)
{
  if (! (SCM_CAR (port) & FPORT_WRITE_SAFE))
    /* This can fail, if we're talking to a line-buffered terminal.  As
       far as I can tell, there's no way to get mixed reads and writes
       to work on a line-buffered terminal at all --- you get a full
       line in the buffer when you read, and then you have to throw it
       out to write.  You have to do unbuffered input, and make the
       system provide the second buffer.  */
    fseek ((FILE *)SCM_STREAM (port), 0, SEEK_CUR);

  /* We've done the seek, so writing is safe.
     Assuming that we're going to do a write next, reading will not be
     safe by the time we're done.  */
  SCM_SETOR_CAR (port, FPORT_WRITE_SAFE);
  SCM_SETAND_CAR (port, ~FPORT_READ_SAFE);
}


/* Helpful operations on stdio FILE-based ports  */

/* should be called with SCM_DEFER_INTS active */

SCM 
scm_setbuf0 (port)
     SCM port;
{
  /* NOSETBUF was provided by scm to allow unbuffered ports to be
     avoided on systems where ungetc didn't work correctly.  See
     comment in unif.c, which seems to be the only place where it
     could still be a problem.  */
#ifndef NOSETBUF
  /*  SCM_SYSCALL (setbuf ((FILE *)SCM_STREAM (port), 0);); */
  SCM_SYSCALL (setvbuf ((FILE *)SCM_STREAM (port), 0, _IONBF, 0););
#endif
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_setvbuf, "setvbuf", 2, 1, 0, scm_setvbuf);
SCM
scm_setvbuf (SCM port, SCM mode, SCM size)
{
  int rv;
  int cmode, csize;

  port = SCM_COERCE_OUTPORT (port);

  SCM_ASSERT (SCM_NIMP (port) && SCM_FPORTP (port), port, SCM_ARG1, s_setvbuf);
  SCM_ASSERT (SCM_INUMP (mode), mode, SCM_ARG2, s_setvbuf);
  if (SCM_UNBNDP (size))
    csize = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (size), size, SCM_ARG3, s_setvbuf);
      csize = SCM_INUM (size);
    }
  cmode = SCM_INUM (mode);
  if (csize == 0 && cmode == _IOFBF)
    cmode = _IONBF;
  SCM_DEFER_INTS;
  SCM_SYSCALL (rv = setvbuf ((FILE *)SCM_STREAM (port), 0, cmode, csize));
  if (rv < 0)
    scm_syserror (s_setvbuf);
  if (cmode == _IONBF)
    SCM_SETCAR (port, SCM_CAR (port) | SCM_BUF0);
  else
    SCM_SETCAR (port, (SCM_CAR (port) & ~SCM_BUF0));
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}

#ifdef FD_SETTER
#define SET_FILE_FD_FIELD(F,D) ((F)->FD_SETTER = (D))
#endif

void
scm_setfileno (fs, fd)
     FILE *fs;
     int fd;
{
#ifdef SET_FILE_FD_FIELD
  SET_FILE_FD_FIELD(fs, fd);
#else
  scm_misc_error ("scm_setfileno", "Not fully implemented on this platform",
		  SCM_EOL);
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
  FILE *f;
  char *file;
  char *mode;

  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename, SCM_ARG1, s_open_file);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2, s_open_file);
  if (SCM_SUBSTRP (filename))
    filename = scm_makfromstr (SCM_ROCHARS (filename), SCM_ROLENGTH (filename), 0);
  if (SCM_SUBSTRP (modes))
    modes = scm_makfromstr (SCM_ROCHARS (modes), SCM_ROLENGTH (modes), 0);

  file = SCM_ROCHARS (filename);
  mode = SCM_ROCHARS (modes);

  SCM_DEFER_INTS;
  SCM_SYSCALL (f = fopen (file, mode));
  if (!f)
    {
      int en = errno;

      scm_syserror_msg (s_open_file, "%s: %S",
			scm_listify (scm_makfrom0str (strerror (errno)),
				     filename,
				     SCM_UNDEFINED),
			en);
    }
  else
    port = scm_stdio_to_port (f, mode, filename);
  SCM_ALLOW_INTS;
  return port;
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

  SCM_COERCE_SUBSTR (filename);
  SCM_COERCE_SUBSTR (modes);
  port = SCM_COERCE_OUTPORT (port);
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
      SCM_SETSTREAM (port, (SCM)f);
      SCM_SETCAR (port, (scm_tc16_fport
			 | scm_mode_bits (SCM_ROCHARS (modes))
			 | FPORT_READ_SAFE | FPORT_WRITE_SAFE));
      if (SCM_BUF0 & SCM_CAR (port))
	scm_setbuf0 (port);
    }
  SCM_ALLOW_INTS;
  return port;
}



/* Building Guile ports from stdio FILE pointers.  */

/* Build a Scheme port from an open stdio port, FILE.
   MODE indicates whether FILE is open for reading or writing; it uses
      the same notation as open-file's second argument.
   Use NAME as the port's filename.  */
SCM
scm_stdio_to_port (FILE *file, char *mode, SCM name)
{
  long mode_bits = scm_mode_bits (mode);
  SCM port;
  struct scm_port_table * pt;

  SCM_NEWCELL (port);
  SCM_DEFER_INTS;
  {
    pt = scm_add_to_port_table (port);
    SCM_SETPTAB_ENTRY (port, pt);
    SCM_SETCAR (port, (scm_tc16_fport
		       | mode_bits
		       | FPORT_READ_SAFE | FPORT_WRITE_SAFE));
    SCM_SETSTREAM (port, (SCM) file);
    if (SCM_BUF0 & SCM_CAR (port))
      scm_setbuf0 (port);
    SCM_PTAB_ENTRY (port)->file_name = name;
  }
  SCM_ALLOW_INTS;
  return port;
}


/* Like scm_stdio_to_port, except that:
   - NAME is a standard C string, not a Guile string
   - we set the revealed count for FILE's file descriptor to 1, so
     that FILE won't be closed when the port object is GC'd.  */
SCM
scm_standard_stream_to_port (FILE *file, char *mode, char *name)
{
  SCM port = scm_stdio_to_port (file, mode, scm_makfrom0str (name));
  scm_set_port_revealed_x (port, SCM_MAKINUM (1));
  return port;
}



/* The fport and pipe port scm_ptobfuns functions --- reading and writing  */

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



static int
local_fgetc (SCM port)
{
  FILE *s = (FILE *) SCM_STREAM (port);
  pre_read (port);
  if (feof (s))
    return EOF;
  else
    return fgetc (s);
}


static char *
local_fgets (SCM port, int *len)
{
  FILE *f;

  char *buf   = NULL;
  char *p;		/* pointer to current buffer position */
  int   limit = 80;	/* current size of buffer */

  pre_read (port);

  /* If this is a socket port or something where we can't rely on
     ftell to determine how much we've read, then call the generic
     function.  We could use a separate scm_ptobfuns table with
     scm_generic_fgets, but then we'd have to change SCM_FPORTP, etc.
     Ideally, it should become something that means "this port has a
     file descriptor"; sometimes we reject sockets when we shouldn't.
     But I'm too stupid at the moment to do that right.  */
  if (SCM_CAR (port) & SCM_NOFTELL)
    return scm_generic_fgets (port, len);

  f = (FILE *) SCM_STREAM (port);
  if (feof (f))
    return NULL;

  buf = (char *) malloc (limit * sizeof(char));
  *len = 0;

  /* If a char has been pushed onto the port with scm_ungetc,
     read that first. */
  if (SCM_CRDYP (port))
    {
      buf[*len] = SCM_CGETUN (port);
      SCM_CLRDY (port);
      if (buf[(*len)++] == '\n')
	{
	  buf[*len] = '\0';
	  return buf;
	}
    }

  while (1)
    {
      int chunk_size = limit - *len;
      long int numread, pos;

      p = buf + *len;

      /* We must use ftell to figure out how many characters were read.
	 If there are null characters near the end of file, and no
	 terminating newline, there is no other way to tell the difference
	 between an embedded null and the string-terminating null. */

      pos = ftell (f);
      if (fgets (p, chunk_size, f) == NULL) {
	if (*len)
	  return buf;
	free (buf);
	return NULL;
      }
      numread = ftell (f) - pos;
      *len += numread;

      if (numread < chunk_size - 1 || buf[limit-2] == '\n')
	return buf;

      buf = (char *) realloc (buf, sizeof(char) * limit * 2);
      limit *= 2;
    }
}

#ifdef vms

static scm_sizet pwrite SCM_P ((char *ptr, scm_sizet size, nitems, FILE *port));

static scm_sizet 
pwrite (ptr, size, nitems, port)
     char *ptr;
     scm_sizet size, nitems;
     FILE *port;
{
  scm_sizet len = size * nitems;
  scm_sizet i = 0;
  for (; i < len; i++)
    putc (ptr[i], port);
  return len;
}

#define ffwrite pwrite
#else
#define ffwrite fwrite
#endif

static int
local_fclose (SCM port)
{
  FILE *fp = (FILE *) SCM_STREAM (port);

  return fclose (fp);
}

static int
local_fflush (SCM port)
{
  FILE *fp = (FILE *) SCM_STREAM (port);
  return fflush (fp);
  FPORT_ALL_OKAY (port);
}

static int
local_fputc (int c, SCM port)
{
  FILE *fp = (FILE *) SCM_STREAM (port);

  pre_write (port);
  return fputc (c, fp);
}

static int
local_fputs (char *s, SCM port)
{
  FILE *fp = (FILE *) SCM_STREAM (port);
  pre_write (port);
  return fputs (s, fp);
}

static scm_sizet
local_ffwrite (char *ptr,
	       scm_sizet size,
	       scm_sizet nitems, 
	       SCM port)
{
  FILE *fp = (FILE *) SCM_STREAM (port);
  pre_write (port);
  return ffwrite (ptr, size, nitems, fp);
}

static int
print_pipe_port (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_prinport (exp, port, "pipe");
  return 1;
}

static int
local_pclose (SCM port)
{
  FILE *fp = (FILE *) SCM_STREAM (port);

  return pclose (fp);
}


/* The file and pipe port scm_ptobfuns structures themselves.  */

scm_ptobfuns scm_fptob =
{
  0,
  local_fclose,
  prinfport,
  0,
  local_fputc,
  local_fputs,
  local_ffwrite,
  local_fflush,
  local_fgetc,
  local_fgets,
  local_fclose
};

/* {Pipe ports} */
scm_ptobfuns scm_pipob =
{
  0,
  local_pclose,  
  print_pipe_port,
  0,
  local_fputc,
  local_fputs,
  local_ffwrite,
  local_fflush,
  local_fgetc,
  scm_generic_fgets,
  local_pclose
};

void
scm_init_fports ()
{
#include "fports.x"
  scm_sysintern ("_IOFBF", SCM_MAKINUM (_IOFBF));
  scm_sysintern ("_IOLBF", SCM_MAKINUM (_IOLBF));
  scm_sysintern ("_IONBF", SCM_MAKINUM (_IONBF));
}

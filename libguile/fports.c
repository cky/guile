/*	Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.
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


#ifdef __IBMC__
#include <io.h>
#include <direct.h>
#else
#ifndef MSDOS
#ifndef ultrix
#ifndef vms
#ifdef _DCC
#include <ioctl.h>
#define setbuf(stream, buf) setvbuf(stream, buf, _IONBF, 0)
#else
#ifdef MWC
#include <sys/io.h>
#else
#ifndef THINK_C
#ifndef ARM_ULIB
#include <sys/ioctl.h>
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif


/* {Ports - file ports}
 * 
 */

/* should be called with SCM_DEFER_INTS active */

SCM 
scm_setbuf0 (port)
     SCM port;
{
#ifndef NOSETBUF
#ifndef MSDOS
#ifndef ultrix
  SCM_SYSCALL (setbuf ((FILE *)SCM_STREAM (port), 0););
#endif
#endif
#endif
  return SCM_UNSPECIFIED;
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

  SCM_NEWCELL (port);
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
    {
      struct scm_port_table * pt;

      pt = scm_add_to_port_table (port);
      SCM_SETPTAB_ENTRY (port, pt);
      SCM_SETCAR (port, scm_tc16_fport | scm_mode_bits (mode));
      SCM_SETSTREAM (port, (SCM) f);
      if (SCM_BUF0 & SCM_CAR (port))
	scm_setbuf0 (port);
      SCM_PTAB_ENTRY (port)->file_name = filename;
    }
  SCM_ALLOW_INTS;
  return port;
}


/* Build a Scheme port from an open stdio port, FILE.
   MODE indicates whether FILE is open for reading or writing; it uses
      the same notation as open-file's second argument.
   If NAME is non-zero, use it as the port's filename.

   scm_stdio_to_port sets the revealed count for FILE's file
   descriptor to 1, so that FILE won't be closed when the port object
   is GC'd.  */
SCM
scm_stdio_to_port (file, mode, name)
     FILE *file;
     char *mode;
     char *name;
{
  long mode_bits = scm_mode_bits (mode);
  SCM port;
  struct scm_port_table * pt;

  SCM_NEWCELL (port);
  SCM_DEFER_INTS;
  {
    pt = scm_add_to_port_table (port);
    SCM_SETPTAB_ENTRY (port, pt);
    SCM_SETCAR (port, (scm_tc16_fport | mode_bits));
    SCM_SETSTREAM (port, (SCM) file);
    if (SCM_BUF0 & SCM_CAR (port))
      scm_setbuf0 (port);
    SCM_PTAB_ENTRY (port)->file_name = scm_makfrom0str (name);
  }
  SCM_ALLOW_INTS;
  scm_set_port_revealed_x (port, SCM_MAKINUM (1));
  return port;
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



static int scm_fgetc SCM_P ((FILE * s));

static int
scm_fgetc (s)
     FILE * s;
{
  if (feof (s))
    return EOF;
  else
    return fgetc (s);
}

/*
 * The fgets method must take a port as its argument, rather than
 * the underlying file handle.  The reason is that we also provide
 * a generic fgets method for ports which can't use fgets(3) (e.g.
 * string ports).  This generic method calls the port's own
 * fgetc method.  In order for it to know how to get that method,
 * we must pass the original Scheme port object.
 */

static char * scm_fgets SCM_P ((SCM port));

static char *
scm_fgets (port)
     SCM port;
{
  FILE *f;

  char *buf   = NULL;
  char *p;		/* pointer to current buffer position */
  int   i     = 0;	/* index into current buffer position */
  int   limit = 80;	/* current size of buffer */
  int   lp;

  f = SCM_STREAM (port);
  if (feof (f))
    return NULL;

  buf = (char *) scm_must_malloc (limit * sizeof(char), "fgets");

  while (1) {
    p = buf + i;
    if (fgets (p, limit - i, f) == NULL) {
      if (i)
	return buf;
      scm_must_free (buf);
      return NULL;
    }

    if (strlen(p) < limit - i - 1)
      return buf;

    buf = (char *) scm_must_realloc (buf,
				     sizeof(char) * limit,
				     sizeof(char) * limit * 2,
				     "fgets");

    i = limit - 1;
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


/* This otherwise pointless code helps some poor 
 * crippled C compilers cope with life. 
 */

static int local_fclose SCM_P ((FILE *fp));

static int
local_fclose (fp)
     FILE * fp;
{
  return fclose (fp);
}

static int local_fflush SCM_P ((FILE *fp));

static int
local_fflush (fp)
     FILE * fp;
{
  return fflush (fp);
}

static int local_fputc SCM_P ((int c, FILE *fp));

static int
local_fputc (c, fp)
     int c;
     FILE * fp;
{
  return fputc (c, fp);
}

static int local_fputs SCM_P ((char *s, FILE *fp));

static int
local_fputs (s, fp)
     char * s;
     FILE * fp;
{
  return fputs (s, fp);
}

static scm_sizet local_ffwrite SCM_P ((void *ptr, int size, int nitems, FILE *fp));

static scm_sizet
local_ffwrite (ptr, size, nitems, fp)
     void * ptr;
     int size;
     int nitems;
     FILE * fp;
{
  return ffwrite (ptr, size, nitems, fp);
}

static int
print_pipe_port (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_prinport (exp, port, "pipe");
  return 1;
}



/* On SunOS, there's no declaration for pclose in the headers, so
   putting it directly in the initializer for scm_pipob doesn't really
   fly.  We could add an extern declaration for it, but then it'll
   mismatch on some systems that do have a declaration.  So we just
   wrap it up this way.  */
static int
local_pclose (fp)
     FILE * fp;
{
  return pclose (fp);
}


scm_ptobfuns scm_fptob =
{
  scm_mark0,
  (int (*) SCM_P ((SCM))) local_fclose,
  prinfport,
  0,
  (int (*) SCM_P ((int, SCM))) local_fputc,
  (int (*) SCM_P ((char *, SCM))) local_fputs,
  (scm_sizet (*) SCM_P ((char *, scm_sizet, scm_sizet, SCM))) local_ffwrite,
  (int (*) SCM_P ((SCM))) local_fflush,
  (int (*) SCM_P ((SCM))) scm_fgetc,
  (char * (*) SCM_P ((SCM))) scm_fgets,
  (int (*) SCM_P ((SCM))) local_fclose
};

/* {Pipe ports} */
scm_ptobfuns scm_pipob =
{
  scm_mark0,
  (int (*) SCM_P ((SCM))) local_pclose,  
  print_pipe_port,
  0,
  (int (*) SCM_P ((int, SCM))) local_fputc,
  (int (*) SCM_P ((char *, SCM))) local_fputs,
  (scm_sizet (*) SCM_P ((char *, scm_sizet, scm_sizet, SCM))) local_ffwrite,
  (int (*) SCM_P ((SCM))) local_fflush,
  (int (*) SCM_P ((SCM))) scm_fgetc,
  (char * (*) SCM_P ((SCM))) scm_fgets,
  (int (*) SCM_P ((SCM))) local_pclose
};

void
scm_init_fports ()
{
#include "fports.x"
}

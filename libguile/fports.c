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
#ifdef __STDC__
SCM 
scm_setbuf0 (SCM port)
#else
SCM 
scm_setbuf0 (port)
     SCM port;
#endif
{
#ifndef NOSETBUF
#ifndef MSDOS
#ifdef FIONREAD
#ifndef ultrix
  SCM_SYSCALL (setbuf ((FILE *)SCM_STREAM (port), 0););
#endif
#endif
#endif
#endif
  return SCM_UNSPECIFIED;
}

/* Return the flags that characterize a port based on the mode
 * string used to open a file for that port.
 *
 * See PORT FLAGS in scm.h
 */
#ifdef __STDC__
long
scm_mode_bits (char *modes)
#else
long
scm_mode_bits (modes)
     char *modes;
#endif
{
  return (SCM_OPN
	  | (strchr (modes, 'r') || strchr (modes, '+') ? SCM_RDNG : 0)
	  | (   strchr (modes, 'w')
	     || strchr (modes, 'a')
	     || strchr (modes, '+') ? SCM_WRTNG : 0)
	  | (strchr (modes, '0') ? SCM_BUF0 : 0));
}


/* scm_open_file
 * Return a new port open on a given file.
 *
 * The mode string must match the pattern: [rwa+]** which
 * is interpreted in the usual unix way.
 *
 * Return the new port.
 */

#ifdef __STDC__
SCM
scm_mkfile (char * name, char * modes)
#else
SCM
scm_mkfile (name, modes)
     char * name;
     char * modes;
#endif
{
  register SCM port;
  FILE *f;
  SCM_NEWCELL (port);
  SCM_DEFER_INTS;
  SCM_SYSCALL (f = fopen (name, modes));
  if (!f)
    {
      SCM_ALLOW_INTS;
      port = SCM_BOOL_F;
    }
  else
    {
      struct scm_port_table * pt;
      pt = scm_add_to_port_table (port);
      SCM_SETPTAB_ENTRY (port, pt);
      if (SCM_BUF0 & (SCM_CAR (port) = scm_tc16_fport | scm_mode_bits (modes)))
	scm_setbuf0 (port);
      SCM_SETSTREAM (port, (SCM)f);
      SCM_PTAB_ENTRY (port)->file_name = scm_makfrom0str (name);
      SCM_ALLOW_INTS;
    }
  return port;
}

SCM_PROC(s_open_file, "open-file", 2, 0, 0, scm_open_file);
#ifdef __STDC__
SCM
scm_open_file (SCM filename, SCM modes)
#else
SCM
scm_open_file (filename, modes)
     SCM filename;
     SCM modes;
#endif
{
  SCM port;
  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename, SCM_ARG1, s_open_file);
  SCM_ASSERT (SCM_NIMP (modes) && SCM_ROSTRINGP (modes), modes, SCM_ARG2, s_open_file);
  if (SCM_SUBSTRP (filename))
    filename = scm_makfromstr (SCM_ROCHARS (filename), SCM_ROLENGTH (filename), 0);
  if (SCM_SUBSTRP (modes))
    modes = scm_makfromstr (SCM_ROCHARS (modes), SCM_ROLENGTH (modes), 0);
  port = scm_mkfile (SCM_ROCHARS (filename), SCM_ROCHARS (modes));

  if (port == SCM_BOOL_F) {
    SCM_SYSERROR_M (s_open_file, "%S: %S",
		    scm_listify (scm_makfrom0str (strerror (errno)),
				 filename,
				 SCM_UNDEFINED));
    /* Force the compiler to keep filename and modes alive.  */
    scm_cons (filename, modes);
  }
  return port;
}

/* Return the mode flags from an open port.
 * Some modes such as "append" are only used when opening
 * a file and are not returned here.
 */

SCM_PROC(s_port_mode, "port-mode", 1, 0, 0, scm_port_mode);
#ifdef __STDC__
SCM
scm_port_mode (SCM port)
#else
SCM
scm_port_mode (port)
     SCM port;
#endif
{
  char modes[3];
  modes[0] = '\0';
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPPORTP (port), port, SCM_ARG1, s_port_mode);  
  if (SCM_CAR (port) & SCM_RDNG) {
    if (SCM_CAR (port) & SCM_WRTNG)
      strcpy (modes, "r+");
    else
      strcpy (modes, "r");
  }
  else if (SCM_CAR (port) & SCM_WRTNG)
    strcpy (modes, "w");
  if (SCM_CAR (port) & SCM_BUF0)
    strcat (modes, "0");
  return scm_makfromstr (modes, strlen (modes), 0);
}


#ifdef __STDC__
static int 
prinfport (SCM exp, SCM port, int writing)
#else
static int 
prinfport (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
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


#ifdef __STDC__
static int
scm_fgetc (FILE * s)
#else
static int
scm_fgetc (s)
     FILE * s;
#endif
{
  if (feof (s))
    return EOF;
  else
    return fgetc (s);
}

#ifdef vms
#ifdef __STDC__
static scm_sizet 
pwrite (char *ptr, scm_sizet size, nitems, FILE *port)
#else
static scm_sizet 
pwrite (ptr, size, nitems, port)
     char *ptr;
     scm_sizet size, nitems;
     FILE *port;
#endif
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
static int
local_fclose (fp)
     FILE * fp;
{
  return fclose (fp);
}

static int
local_fflush (fp)
     FILE * fp;
{
  return fflush (fp);
}

static int
local_fputc (c, fp)
     int c;
     FILE * fp;
{
  return fputc (c, fp);
}

static int
local_fputs (s, fp)
     char * s;
     FILE * fp;
{
  return fputs (s, fp);
}

static scm_sizet
local_ffwrite (ptr, size, nitems, fp)
     void * ptr;
     int size;
     int nitems;
     FILE * fp;
{
  return ffwrite (ptr, size, nitems, fp);
}


scm_ptobfuns scm_fptob =
{
  scm_mark0,
  local_fclose,
  prinfport,
  0,
  local_fputc,
  local_fputs,
  local_ffwrite,
  local_fflush,
  scm_fgetc,
  local_fclose
};

/* {Pipe ports}
 */
scm_ptobfuns scm_pipob =
{
  scm_mark0,
  0, 				/* replaced by pclose in scm_init_ioext() */
  0, 				/* replaced by prinpipe in scm_init_ioext() */
  0,
  local_fputc,
  local_fputs,
  local_ffwrite,
  local_fflush,
  scm_fgetc,
  0
};				/* replaced by pclose in scm_init_ioext() */


#ifdef __STDC__
void
scm_init_fports (void)
#else
void
scm_init_fports ()
#endif
{
#include "fports.x"
}


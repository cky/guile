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
#include "libpath.h"
#include "fports.h"
#include "read.h"
#include "eval.h"

#include "load.h"

#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifndef R_OK
#define R_OK 4
#endif


/* Loading a file, given an absolute filename.  */

SCM_PROC(s_sys_try_load, "primitive-load", 1, 2, 0, scm_sys_try_load);
SCM 
scm_sys_try_load (filename, case_insensitive_p, sharp)
     SCM filename;
     SCM case_insensitive_p;
     SCM sharp;
{
  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_sys_try_load);
  {
    SCM form, port;
    port = scm_open_file (filename,
			  scm_makfromstr ("r", (scm_sizet) sizeof (char), 0));
    while (1)
      {
	form = scm_read (port, case_insensitive_p, sharp);
	if (SCM_EOF_VAL == form)
	  break;
	scm_eval_x (form);
      }
    scm_close_port (port);
  }
  return SCM_UNSPECIFIED;
}


/* Initializing the load path, and searching it.  */

static SCM *scm_loc_load_path;

/* Initialize the global variable %load-path, given the value of the
   LIBRARY_PATH preprocessor symbol and the SCHEME_LOAD_PATH
   environment variable.  */
void
scm_init_load_path ()
{
  SCM path = SCM_EOL;

#ifdef LIBRARY_PATH
  path = scm_cons (scm_makfrom0str (LIBRARY_PATH), path);
#endif /* LIBRARY_PATH */
  
  {
    char *path_string = getenv ("SCHEME_LOAD_PATH");

    if (path_string && path_string[0] != '\0')
      {
	char *scan, *elt_end;

	/* Scan backwards from the end of the string, to help
           construct the list in the right order.  */
	scan = elt_end = path_string + strlen (path_string);
	do {
	  /* Scan back to the beginning of the current element.  */
	  do scan--;
	  while (scan >= path_string && *scan != ':');
	  path = scm_cons (scm_makfromstr (scan + 1, elt_end - (scan + 1), 0),
			   path);
	  elt_end = scan;
	} while (scan >= path_string);
      }
  }

  *scm_loc_load_path = path;
}


/* Search %load-path for a directory containing a file named FILENAME.
   The file must be readable, and not a directory.
   If we find one, return its full filename; otherwise, return #f.  */
SCM_PROC(s_sys_search_load_path, "%search-load-path", 1, 0, 0, scm_sys_search_load_path);
SCM 
scm_sys_search_load_path (filename)
     SCM filename;
{
  SCM path = *scm_loc_load_path;
  char *buf;
  int buf_size;
  int filename_len;

  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_sys_search_load_path);
  filename_len = SCM_ROLENGTH (filename);

  SCM_DEFER_INTS;

  buf_size = 80;
  buf = scm_must_malloc (buf_size, s_sys_search_load_path);

  while (SCM_NIMP (path) && SCM_CONSP (path))
    {
      SCM elt = SCM_CAR (path);
      if (SCM_NIMP (elt) && SCM_ROSTRINGP (elt))
	{
	  int len = SCM_ROLENGTH (elt) + 1 + filename_len;

	  if (len + 1 > buf_size)
	    {
	      int old_size = buf_size;
	      buf_size = len + 50;
	      buf = scm_must_realloc (buf, old_size, buf_size,
				      s_sys_search_load_path);
	    }

	  memcpy (buf, SCM_ROCHARS (elt), SCM_ROLENGTH (elt));
	  buf[SCM_ROLENGTH (elt)] = '/';
	  memcpy (buf + SCM_ROLENGTH (elt) + 1,
		  SCM_ROCHARS (filename), filename_len);
	  buf[len] = '\0';

	  {
	    struct stat mode;

	    if (stat (buf, &mode) >= 0
		&& ! (mode.st_mode & S_IFDIR)
		&& access (buf, R_OK) == 0)
	      {
		SCM result = scm_makfromstr (buf, len, 0);
		scm_must_free (buf);
		SCM_ALLOW_INTS;
		return result;
	      }
	  }
	}

      path = SCM_CDR (path);
    }
  
  scm_must_free (buf);
  SCM_ALLOW_INTS;
  return SCM_BOOL_F;
}


SCM_PROC(s_sys_try_load_path, "%try-load-path", 1, 2, 0,scm_sys_try_load_path);
SCM 
scm_sys_try_load_path (filename, case_insensitive_p, sharp)
     SCM filename;
     SCM case_insensitive_p;
     SCM sharp;
{
  SCM full_filename = scm_sys_search_load_path (filename);
  if (SCM_FALSEP (full_filename))
    {
      lgh_error (scm_misc_error_key,
		 s_sys_try_load_path,
		 "Unable to find file %S in %S",
		 scm_listify (filename, *scm_loc_load_path, SCM_UNDEFINED),
		 SCM_BOOL_F);
    }
  return scm_sys_try_load (full_filename, case_insensitive_p, sharp);
}



void
scm_init_load ()
{
  scm_loc_load_path = &SCM_CDR(scm_sysintern("%load-path", SCM_EOL));

#include "load.x"
}

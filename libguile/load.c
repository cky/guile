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


#include <stdio.h>
#include "_scm.h"
#include "libpath.h"
#include "fports.h"
#include "read.h"
#include "eval.h"
#include "throw.h"
#include "alist.h"

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

/* Hook to run when we load a file, perhaps to announce the fact somewhere.
   Applied to the full name of the file.  */
static SCM *scm_loc_load_hook;

SCM_PROC(s_primitive_load, "primitive-load", 1, 0, 0, scm_primitive_load);
SCM 
scm_primitive_load (filename)
     SCM filename;
{
  SCM hook = *scm_loc_load_hook;
  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_primitive_load);
  SCM_ASSERT (hook == SCM_BOOL_F
	      || (scm_procedure_p (hook) == SCM_BOOL_T),
	      hook, "value of %load-hook is neither a procedure nor #f",
	      s_primitive_load);

  if (hook != SCM_BOOL_F)
    scm_apply (hook, scm_listify (filename, SCM_UNDEFINED), SCM_EOL);

  {
    SCM form, port;
    port = scm_open_file (filename,
			  scm_makfromstr ("r", (scm_sizet) sizeof (char), 0));
    while (1)
      {
	form = scm_read (port);
	if (SCM_EOF_VAL == form)
	  break;
	scm_eval_x (form);
      }
    scm_close_port (port);
  }
  return SCM_UNSPECIFIED;
}


/* Builtin path to scheme library files. */
#ifdef SCM_PKGDATA_DIR
SCM_PROC (s_sys_package_data_dir, "%package-data-dir", 0, 0, 0, scm_sys_package_data_dir);
SCM
scm_sys_package_data_dir ()
{
  return scm_makfrom0str (SCM_PKGDATA_DIR);
}
#endif /* SCM_PKGDATA_DIR */


/* Initializing the load path, and searching it.  */

/* List of names of directories we search for files to load.  */
static SCM *scm_loc_load_path;

/* List of extensions we try adding to the filenames.  */
static SCM *scm_loc_load_extensions;

/* Initialize the global variable %load-path, given the value of the
   SCM_SITE_DIR and SCM_LIBRARY_DIR preprocessor symbols and the
   SCHEME_LOAD_PATH environment variable.  */
void
scm_init_load_path ()
{
  SCM path = SCM_EOL;

#ifdef SCM_LIBRARY_DIR
  path = scm_listify (scm_makfrom0str (SCM_SITE_DIR),
		      scm_makfrom0str (SCM_LIBRARY_DIR),
		      scm_makfrom0str (SCM_PKGDATA_DIR),
		      SCM_UNDEFINED);
#endif /* SCM_LIBRARY_DIR */
  
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
   If we find one, return its full filename; otherwise, return #f.
   If FILENAME is absolute, return it unchanged.  */
SCM_PROC(s_sys_search_load_path, "%search-load-path", 1, 0, 0, scm_sys_search_load_path);
SCM 
scm_sys_search_load_path (filename)
     SCM filename;
{
  SCM path = *scm_loc_load_path;
  SCM exts = *scm_loc_load_extensions;
  char *buf;
  int filename_len;
  int max_path_len;
  int max_ext_len;

  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_sys_search_load_path);
  SCM_ASSERT (scm_ilength (path) >= 0, path, "load path is not a proper list",
	      s_sys_search_load_path);
  SCM_ASSERT (scm_ilength (exts) >= 0, exts,
	      "load extension list is not a proper list",
	      s_sys_search_load_path);
  filename_len = SCM_ROLENGTH (filename);

  /* If FILENAME is absolute, return it unchanged.  */
  if (filename_len >= 1
      && SCM_ROCHARS (filename)[0] == '/')
    return filename;

  /* Find the length of the longest element of path.  */
  {
    SCM walk;

    max_path_len = 0;
    for (walk = path; SCM_NIMP (walk); walk = SCM_CDR (walk))
      {
	SCM elt = SCM_CAR (walk);
	SCM_ASSERT (SCM_NIMP (elt) && SCM_ROSTRINGP (elt), elt,
		    "load path is not a list of strings",
		    s_sys_search_load_path);
	if (SCM_LENGTH (elt) > max_path_len)
	  max_path_len = SCM_LENGTH (elt);
      }
  }

  /* Find the length of the longest element of the load extensions
     list.  */
  {
    SCM walk;

    max_ext_len = 0;
    for (walk = exts; SCM_NIMP (walk); walk = SCM_CDR (walk))
      {
	SCM elt = SCM_CAR (walk);
	SCM_ASSERT (SCM_NIMP (elt) && SCM_ROSTRINGP (elt), elt,
		    "load extension list is not a list of strings",
		    s_sys_search_load_path);
	if (SCM_LENGTH (elt) > max_ext_len)
	  max_ext_len = SCM_LENGTH (elt);
      }
  }

  SCM_DEFER_INTS;

  buf = scm_must_malloc (max_path_len + 1 + filename_len + max_ext_len + 1,
			 s_sys_search_load_path);

  /* Try every path element.  At this point, we know it's a proper
     list of strings.  */
  for (; SCM_NIMP (path); path = SCM_CDR (path))
    {
      SCM path_elt = SCM_CAR (path);

      /* Try every extension.  At this point, we know it's a proper
         list of strings.  */
      for (exts = *scm_loc_load_extensions;
	   SCM_NIMP (exts);
	   exts = SCM_CDR (exts))
	{
	  SCM ext_elt = SCM_CAR (exts);
	  int i;

	  /* Concatenate the path name, the filename, and the extension. */
	  i = SCM_ROLENGTH (path_elt);
	  memcpy (buf, SCM_ROCHARS (path_elt), i);
	  if (i >= 1 && buf[i - 1] != '/')
	    buf[i++] = '/';
	  memcpy (buf + i, SCM_ROCHARS (filename), filename_len);
	  i += filename_len;
	  memcpy (buf + i, SCM_ROCHARS (ext_elt), SCM_LENGTH (ext_elt));
	  i += SCM_LENGTH (ext_elt);
	  buf[i] = '\0';

	  {
	    struct stat mode;

	    if (stat (buf, &mode) >= 0
		&& ! (mode.st_mode & S_IFDIR)
		&& access (buf, R_OK) == 0)
	      {
		SCM result = scm_makfromstr (buf, i, 0);
		scm_must_free (buf);
		SCM_ALLOW_INTS;
		return result;
	      }
	  }
	}
    }
  
  scm_must_free (buf);
  SCM_ALLOW_INTS;
  return SCM_BOOL_F;
}


SCM_PROC(s_primitive_load_path, "primitive-load-path", 1, 0, 0, scm_primitive_load_path);
SCM 
scm_primitive_load_path (filename)
     SCM filename;
{
  SCM full_filename;

  SCM_ASSERT (SCM_NIMP (filename) && SCM_ROSTRINGP (filename), filename,
	      SCM_ARG1, s_primitive_load_path);

  full_filename = scm_sys_search_load_path (filename);

  if (SCM_FALSEP (full_filename))
    {
      int absolute = (SCM_LENGTH (filename) >= 1
		      && SCM_ROCHARS (filename)[0] == '/');
      scm_misc_error (s_primitive_load_path,
		      (absolute
		       ? "Unable to load file %S"
		       : "Unable to find file %S in load path"),
		      scm_listify (filename, SCM_UNDEFINED));
    }

  return scm_primitive_load (full_filename);
}

/* The following function seems trivial - and indeed it is.  Its
 * existence is motivated by its ability to evaluate expressions
 * without copying them first (as is done in "eval").
 */

SCM_SYMBOL (scm_end_of_file_key, "end-of-file");

SCM_PROC (s_read_and_eval_x, "read-and-eval!", 0, 1, 0, scm_read_and_eval_x);

SCM
scm_read_and_eval_x (port)
     SCM port;
{
  SCM form = scm_read (port);
  if (form == SCM_EOF_VAL)
    scm_ithrow (scm_end_of_file_key, SCM_EOL, 1);
  return scm_eval_x (form);
}


/* Information about the build environment.  */

/* Initialize the scheme variable %guile-build-info, based on data
   provided by the Makefile, via libpath.h.  */
static void
init_build_info ()
{
  static struct { char *name; char *value; } info[] = SCM_BUILD_INFO;
  SCM *loc = SCM_CDRLOC (scm_sysintern ("%guile-build-info", SCM_EOL));
  int i;

  for (i = 0; i < (sizeof (info) / sizeof (info[0])); i++)
    *loc = scm_acons (SCM_CAR (scm_intern0 (info[i].name)),
		      scm_makfrom0str (info[i].value),
		      *loc);
}



void
scm_init_load ()
{
  scm_loc_load_path = SCM_CDRLOC(scm_sysintern("%load-path", SCM_EOL));
  scm_loc_load_extensions
    = SCM_CDRLOC(scm_sysintern("%load-extensions",
			       scm_listify (scm_makfrom0str (""),
					    scm_makfrom0str (".scm"),
					    SCM_UNDEFINED)));
  scm_loc_load_hook = SCM_CDRLOC(scm_sysintern("%load-hook", SCM_BOOL_F));

  init_build_info ();

#include "load.x"
}

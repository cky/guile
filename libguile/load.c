/* Copyright (C) 1995,1996,1998,1999,2000,2001 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */




#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>

#include "libguile/_scm.h"
#include "libguile/libpath.h"
#include "libguile/fports.h"
#include "libguile/read.h"
#include "libguile/eval.h"
#include "libguile/throw.h"
#include "libguile/alist.h"
#include "libguile/dynwind.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/modules.h"
#include "libguile/lang.h"

#include "libguile/validate.h"
#include "libguile/load.h"

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

static void
swap_port (void *data)
{
  SCM *save_port = data, tmp = scm_cur_loadp;
  scm_cur_loadp = *save_port;
  *save_port = tmp;
}

static SCM
load (void *data)
{
  SCM port = SCM_PACK (data);
  while (1)
    {
      SCM form = scm_read (port);
      if (SCM_EOF_OBJECT_P (form))
	break;
      scm_primitive_eval_x (form);
    }
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_primitive_load, "primitive-load", 1, 0, 0, 
           (SCM filename),
	    "Load the file named @var{filename} and evaluate its contents in\n"
	    "the top-level environment. The load paths are not searched;\n"
	    "@var{filename} must either be a full pathname or be a pathname\n"
	    "relative to the current directory.  If the  variable\n"
	    "@code{%load-hook} is defined, it should be bound to a procedure\n"
	    "that will be called before any code is loaded.  See the\n"
	    "documentation for @code{%load-hook} later in this section.")
#define FUNC_NAME s_scm_primitive_load
{
  SCM hook = *scm_loc_load_hook;
  SCM_VALIDATE_STRING (1, filename);
  if (!SCM_FALSEP (hook) && !SCM_EQ_P (scm_procedure_p (hook), SCM_BOOL_T))
    SCM_MISC_ERROR ("value of %load-hook is neither a procedure nor #f",
		    SCM_EOL);

  if (! SCM_FALSEP (hook))
    scm_call_1 (hook, filename);

  { /* scope */
    SCM port, save_port;
    port = scm_open_file (filename, scm_mem2string ("r", sizeof (char)));
    save_port = port;
    scm_internal_dynamic_wind (swap_port,
			       load,
			       swap_port,
			       (void *) SCM_UNPACK (port),
			       &save_port);
    scm_close_port (port);
  }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_c_primitive_load (const char *filename)
{
  return scm_primitive_load (scm_makfrom0str (filename));
}


/* Builtin path to scheme library files. */
#ifdef SCM_PKGDATA_DIR
SCM_DEFINE (scm_sys_package_data_dir, "%package-data-dir", 0, 0, 0, 
            (),
	    "Return the name of the directory where Scheme packages, modules and\n"
	    "libraries are kept.  On most Unix systems, this will be\n"
	    "@samp{/usr/local/share/guile}.")
#define FUNC_NAME s_scm_sys_package_data_dir
{
  return scm_makfrom0str (SCM_PKGDATA_DIR);
}
#undef FUNC_NAME
#endif /* SCM_PKGDATA_DIR */

#ifdef SCM_LIBRARY_DIR
SCM_DEFINE (scm_sys_library_dir, "%library-dir", 0,0,0,
            (),
	    "Return the directory where the Guile Scheme library files are installed.\n"
	    "E.g., may return \"/usr/share/guile/1.3.5\".")
#define FUNC_NAME s_scm_sys_library_dir
{
  return scm_makfrom0str(SCM_LIBRARY_DIR);
}
#undef FUNC_NAME
#endif /* SCM_LIBRARY_DIR */

#ifdef SCM_SITE_DIR
SCM_DEFINE (scm_sys_site_dir, "%site-dir", 0,0,0,
            (),
	    "Return the directory where the Guile site files are installed.\n"
	    "E.g., may return \"/usr/share/guile/site\".")
#define FUNC_NAME s_scm_sys_site_dir
{
  return scm_makfrom0str(SCM_SITE_DIR);
}
#undef FUNC_NAME
#endif /* SCM_SITE_DIR */




/* Initializing the load path, and searching it.  */

/* List of names of directories we search for files to load.  */
static SCM *scm_loc_load_path;

/* List of extensions we try adding to the filenames.  */
static SCM *scm_loc_load_extensions;


/* Parse the null-terminated string PATH as if it were a standard path
   environment variable (i.e. a colon-separated list of strings), and
   prepend the elements to TAIL.  */
SCM
scm_internal_parse_path (char *path, SCM tail)
{
  if (path && path[0] != '\0')
    {
      char *scan, *elt_end;

      /* Scan backwards from the end of the string, to help
	 construct the list in the right order.  */
      scan = elt_end = path + strlen (path);
      do {
	/* Scan back to the beginning of the current element.  */
	do scan--;
#ifdef __MINGW32__
	while (scan >= path && *scan != ';');
#else
	while (scan >= path && *scan != ':');
#endif
	tail = scm_cons (scm_mem2string (scan + 1, elt_end - (scan + 1)),
			 tail);
	elt_end = scan;
      } while (scan >= path);
    }

  return tail;
}


SCM_DEFINE (scm_parse_path, "parse-path", 1, 1, 0, 
            (SCM path, SCM tail),
	    "Parse @var{path}, which is expected to be a colon-separated\n"
	    "string, into a list and return the resulting list with\n"
	    "@var{tail} appended. If @var{path} is @code{#f}, @var{tail}\n"
	    "is returned.")
#define FUNC_NAME s_scm_parse_path
{
  SCM_ASSERT (SCM_FALSEP (path) || (SCM_STRINGP (path)),
	      path,
	      SCM_ARG1, FUNC_NAME);
  if (SCM_UNBNDP (tail))
    tail = SCM_EOL;
  return (SCM_FALSEP (path)
	  ? tail
	  : scm_internal_parse_path (SCM_STRING_CHARS (path), tail));
}
#undef FUNC_NAME


/* Initialize the global variable %load-path, given the value of the
   SCM_SITE_DIR and SCM_LIBRARY_DIR preprocessor symbols and the
   GUILE_LOAD_PATH environment variable.  */
void
scm_init_load_path ()
{
  SCM path = SCM_EOL;

#ifdef SCM_LIBRARY_DIR
  path = scm_list_3 (scm_makfrom0str (SCM_SITE_DIR),
		     scm_makfrom0str (SCM_LIBRARY_DIR),
		     scm_makfrom0str (SCM_PKGDATA_DIR));
#endif /* SCM_LIBRARY_DIR */

  path = scm_internal_parse_path (getenv ("GUILE_LOAD_PATH"), path);

  *scm_loc_load_path = path;
}

SCM scm_listofnullstr;

/* Search PATH for a directory containing a file named FILENAME.
   The file must be readable, and not a directory.
   If we find one, return its full filename; otherwise, return #f.
   If FILENAME is absolute, return it unchanged.
   If given, EXTENSIONS is a list of strings; for each directory 
   in PATH, we search for FILENAME concatenated with each EXTENSION.  */
SCM_DEFINE (scm_search_path, "search-path", 2, 1, 0,
           (SCM path, SCM filename, SCM extensions),
	    "Search @var{path} for a directory containing a file named\n"
	    "@var{filename}. The file must be readable, and not a directory.\n"
	    "If we find one, return its full filename; otherwise, return\n"
	    "@code{#f}.  If @var{filename} is absolute, return it unchanged.\n"
	    "If given, @var{extensions} is a list of strings; for each\n"
	    "directory in @var{path}, we search for @var{filename}\n"
	    "concatenated with each @var{extension}.")
#define FUNC_NAME s_scm_search_path
{
  char *filename_chars;
  int filename_len;
  size_t max_path_len;		/* maximum length of any PATH element */
  size_t max_ext_len;		/* maximum length of any EXTENSIONS element */

  SCM_VALIDATE_LIST (1, path);
  SCM_VALIDATE_STRING (2, filename);
  if (SCM_UNBNDP (extensions))
    extensions = SCM_EOL;
  else
    SCM_VALIDATE_LIST (3, extensions);

  filename_chars = SCM_STRING_CHARS (filename);
  filename_len = SCM_STRING_LENGTH (filename);

  /* If FILENAME is absolute, return it unchanged.  */
#ifdef __MINGW32__
  if (((filename_len >= 1) && 
       (filename_chars[0] == '/' || filename_chars[0] == '\\')) ||
      ((filename_len >= 3) && filename_chars[1] == ':' &&
       ((filename_chars[0] >= 'a' && filename_chars[0] <= 'z') ||
	(filename_chars[0] >= 'A' && filename_chars[0] <= 'Z')) &&
       (filename_chars[2] == '/' || filename_chars[2] == '\\')))
#else
  if (filename_len >= 1 && filename_chars[0] == '/')
#endif
    return filename;

  /* Find the length of the longest element of path.  */
  {
    SCM walk;

    max_path_len = 0;
    for (walk = path; !SCM_NULL_OR_NIL_P (walk); walk = SCM_CDR (walk))
      {
	SCM elt = SCM_CAR (walk);
	SCM_ASSERT_TYPE (SCM_STRINGP (elt), path, 1, FUNC_NAME,
			 "list of strings");
	if (SCM_STRING_LENGTH (elt) > max_path_len)
	  max_path_len = SCM_STRING_LENGTH (elt);
      }
  }

  /* If FILENAME has an extension, don't try to add EXTENSIONS to it.  */
  {
    char *endp;

    for (endp = filename_chars + filename_len - 1;
	 endp >= filename_chars;
	 endp--)
      {
	if (*endp == '.')
	  {
	    /* This filename already has an extension, so cancel the
               list of extensions.  */
	    extensions = SCM_EOL;
	    break;
	  }
#ifdef __MINGW32__
	else if (*endp == '/' || *endp == '\\')
#else
	else if (*endp == '/')
#endif
	  /* This filename has no extension, so keep the current list
             of extensions.  */
	  break;
      }
  }

  /* Find the length of the longest element of the load extensions
     list.  */
  { /* scope */
    SCM walk;

    max_ext_len = 0;
    for (walk = extensions; !SCM_NULL_OR_NIL_P (walk); walk = SCM_CDR (walk))
      {
	SCM elt = SCM_CAR (walk);
	SCM_ASSERT_TYPE (SCM_STRINGP (elt), elt, 3, FUNC_NAME,
			 "list of strings");
	if (SCM_STRING_LENGTH (elt) > max_ext_len)
	  max_ext_len = SCM_STRING_LENGTH (elt);
      }
  }

  SCM_DEFER_INTS;

  { /* scope */
    SCM result = SCM_BOOL_F;
    size_t buf_size = max_path_len + 1 + filename_len + max_ext_len + 1;
    char *buf = scm_malloc (buf_size);

    /* This simplifies the loop below a bit.  */
    if (SCM_NULL_OR_NIL_P (extensions))
      extensions = scm_listofnullstr;

    /* Try every path element.  At this point, we know the path is a
       proper list of strings.  */
    for (; !SCM_NULL_OR_NIL_P (path); path = SCM_CDR (path))
      {
	size_t len;
	SCM dir = SCM_CAR (path);
	SCM exts;

	/* Concatenate the path name and the filename. */
	len = SCM_STRING_LENGTH (dir);
	memcpy (buf, SCM_STRING_CHARS (dir), len);
#ifdef __MINGW32__
	if (len >= 1 && buf[len - 1] != '/' && buf[len - 1] != '\\')
#else
	if (len >= 1 && buf[len - 1] != '/')
#endif
	  buf[len++] = '/';
	memcpy (buf + len, filename_chars, filename_len);
	len += filename_len;

	/* Try every extension.  At this point, we know the extension
	   list is a proper, nonempty list of strings.  */
	for (exts = extensions; !SCM_NULL_OR_NIL_P (exts); exts = SCM_CDR (exts))
	  {
	    SCM ext = SCM_CAR (exts);
	    size_t ext_len = SCM_STRING_LENGTH (ext);
	    struct stat mode;

	    /* Concatenate the extension. */
	    memcpy (buf + len, SCM_STRING_CHARS (ext), ext_len);
	    buf[len + ext_len] = '\0';
	    
	    /* If the file exists at all, we should return it.  If the
	       file is inaccessible, then that's an error.  */
	    if (stat (buf, &mode) == 0
		&& ! (mode.st_mode & S_IFDIR))
	      {
		result = scm_mem2string (buf, len + ext_len);
		goto end;
	      }
	  }
      }

  end:
    free (buf);
    SCM_ALLOW_INTS;
    return result;
  }
}
#undef FUNC_NAME


/* Search %load-path for a directory containing a file named FILENAME.
   The file must be readable, and not a directory.
   If we find one, return its full filename; otherwise, return #f.
   If FILENAME is absolute, return it unchanged.  */
SCM_DEFINE (scm_sys_search_load_path, "%search-load-path", 1, 0, 0, 
	    (SCM filename),
	    "Search @var{%load-path} for the file named @var{filename},\n"
	    "which must be readable by the current user.  If @var{filename}\n"
	    "is found in the list of paths to search or is an absolute\n"
	    "pathname, return its full pathname.  Otherwise, return\n"
	    "@code{#f}.  Filenames may have any of the optional extensions\n"
	    "in the @code{%load-extensions} list; @code{%search-load-path}\n"
	    "will try each extension automatically.")
#define FUNC_NAME s_scm_sys_search_load_path
{
  SCM path = *scm_loc_load_path;
  SCM exts = *scm_loc_load_extensions;
  SCM_VALIDATE_STRING (1, filename);

  if (scm_ilength (path) < 0)
    SCM_MISC_ERROR ("%load-path is not a proper list", SCM_EOL);
  if (scm_ilength (exts) < 0)
    SCM_MISC_ERROR ("%load-extension list is not a proper list", SCM_EOL);
  return scm_search_path (path, filename, exts);
}
#undef FUNC_NAME


SCM_DEFINE (scm_primitive_load_path, "primitive-load-path", 1, 0, 0, 
	    (SCM filename),
	    "Search @var{%load-path} for the file named @var{filename} and\n"
	    "load it into the top-level environment.  If @var{filename} is a\n"
	    "relative pathname and is not found in the list of search paths,\n"
	    "an error is signalled.")
#define FUNC_NAME s_scm_primitive_load_path
{
  SCM full_filename;

  SCM_VALIDATE_STRING (1, filename);

  full_filename = scm_sys_search_load_path (filename);

  if (SCM_FALSEP (full_filename))
    {
      int absolute = (SCM_STRING_LENGTH (filename) >= 1
#ifdef __MINGW32__
		      && (SCM_STRING_CHARS (filename)[0] == '/' || 
			  SCM_STRING_CHARS (filename)[0] == '\\'));
#else
		      && SCM_STRING_CHARS (filename)[0] == '/');
#endif
      SCM_MISC_ERROR ((absolute
		       ? "Unable to load file ~S"
		       : "Unable to find file ~S in load path"),
		      scm_list_1 (filename));
    }

  return scm_primitive_load (full_filename);
}
#undef FUNC_NAME

SCM
scm_c_primitive_load_path (const char *filename)
{
  return scm_primitive_load_path (scm_makfrom0str (filename));
}


/* Information about the build environment.  */

/* Initialize the scheme variable %guile-build-info, based on data
   provided by the Makefile, via libpath.h.  */
static void
init_build_info ()
{
  static struct { char *name; char *value; } info[] = SCM_BUILD_INFO;
  SCM *loc = SCM_VARIABLE_LOC (scm_c_define ("%guile-build-info", SCM_EOL));
  unsigned long i;

  for (i = 0; i < (sizeof (info) / sizeof (info[0])); i++)
    *loc = scm_acons (scm_str2symbol (info[i].name),
		      scm_makfrom0str (info[i].value),
		      *loc);
}



void
scm_init_load ()
{
  scm_listofnullstr = scm_permanent_object (scm_list_1 (scm_nullstr));
  scm_loc_load_path = SCM_VARIABLE_LOC (scm_c_define ("%load-path", SCM_EOL));
  scm_loc_load_extensions
    = SCM_VARIABLE_LOC (scm_c_define ("%load-extensions",
				    scm_list_2 (scm_makfrom0str (".scm"),
						scm_nullstr)));
  scm_loc_load_hook = SCM_VARIABLE_LOC (scm_c_define ("%load-hook", SCM_BOOL_F));

  init_build_info ();

#include "libguile/load.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

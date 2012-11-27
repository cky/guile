/* dynl.c - dynamic linking
 *
 * Copyright (C) 1990, 91, 92, 93, 94, 95, 96, 97, 98, 99, 2000, 2001, 2002,
 * 2003, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <alloca.h>
#include <string.h>

/* "dynl.c" dynamically link&load object files.
   Author: Aubrey Jaffer
   Modified for libguile by Marius Vollmer */

#if 0 /* Disabled until we know for sure that it isn't needed */
/* XXX - This is only here to drag in a definition of __eprintf. This
   is needed for proper operation of dynamic linking. The real
   solution would probably be a shared libgcc. */

#undef NDEBUG
#include <assert.h>

static void
maybe_drag_in_eprintf ()
{
  assert (!maybe_drag_in_eprintf);
}
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/libpath.h"
#include "libguile/dynl.h"
#include "libguile/smob.h"
#include "libguile/keywords.h"
#include "libguile/ports.h"
#include "libguile/strings.h"
#include "libguile/deprecation.h"
#include "libguile/validate.h"
#include "libguile/dynwind.h"
#include "libguile/foreign.h"
#include "libguile/gc.h"

#include <ltdl.h>

/*
  From the libtool manual: "Note that libltdl is not threadsafe,
  i.e. a multithreaded application has to use a mutex for libltdl.".

  Guile does not currently support pre-emptive threads, so there is no
  mutex.  Previously SCM_CRITICAL_SECTION_START and
  SCM_CRITICAL_SECTION_END were used: they are mentioned here in case
  somebody is grepping for thread problems ;)
*/
/* njrev: not threadsafe, protection needed as described above */


/* LT_PATH_SEP-separated extension library search path, searched last */
static char *system_extensions_path;

static void *
sysdep_dynl_link (const char *fname, const char *subr)
{
  lt_dlhandle handle;

  if (fname == NULL)
    /* Return a handle for the program as a whole.  */
    handle = lt_dlopen (NULL);
  else
    {
      handle = lt_dlopenext (fname);

      if (handle == NULL
#ifdef LT_DIRSEP_CHAR
          && strchr (fname, LT_DIRSEP_CHAR) == NULL
#endif
          && strchr (fname, '/') == NULL)
        {
          /* FNAME contains no directory separators and was not in the
             usual library search paths, so now we search for it in
             SYSTEM_EXTENSIONS_PATH. */
          char *fname_attempt
            = scm_gc_malloc_pointerless (strlen (system_extensions_path)
                                         + strlen (fname) + 2,
                                         "dynl fname_attempt");
          char *path;  /* remaining path to search */
          char *end;   /* end of current path component */
          char *s;

          /* Iterate over the components of SYSTEM_EXTENSIONS_PATH */
          for (path = system_extensions_path;
               *path != '\0';
               path = (*end == '\0') ? end : (end + 1))
            {
              /* Find end of path component */
              end = strchr (path, LT_PATHSEP_CHAR);
              if (end == NULL)
                end = strchr (path, '\0');

              /* Skip empty path components */
              if (path == end)
                continue;

              /* Construct FNAME_ATTEMPT, starting with path component */
              s = fname_attempt;
              memcpy (s, path, end - path);
              s += end - path;

              /* Append directory separator, but avoid duplicates */
              if (s[-1] != '/'
#ifdef LT_DIRSEP_CHAR
                  && s[-1] != LT_DIRSEP_CHAR
#endif
                  )
                *s++ = '/';

              /* Finally, append FNAME (including null terminator) */
              strcpy (s, fname);

              /* Try to load it, and terminate the search if successful */
              handle = lt_dlopenext (fname_attempt);
              if (handle != NULL)
                break;
            }
        }
    }

  if (handle == NULL)
    {
      SCM fn;
      SCM msg;

      fn = fname != NULL ? scm_from_locale_string (fname) : SCM_BOOL_F;
      msg = scm_from_locale_string (lt_dlerror ());
      scm_misc_error (subr, "file: ~S, message: ~S", scm_list_2 (fn, msg));
    }

  return (void *) handle;
}

static void
sysdep_dynl_unlink (void *handle, const char *subr)
{
  if (lt_dlclose ((lt_dlhandle) handle))
    {
      scm_misc_error (subr, (char *) lt_dlerror (), SCM_EOL);
    }
}
   
static void *
sysdep_dynl_value (const char *symb, void *handle, const char *subr)
{
  void *fptr;

  fptr = lt_dlsym ((lt_dlhandle) handle, symb);
  if (!fptr)
    scm_misc_error (subr, "Symbol not found: ~a",
                    scm_list_1 (scm_from_locale_string (symb)));
  return fptr;
}

static void
sysdep_dynl_init ()
{
  char *env;

  lt_dlinit ();

  /* Initialize 'system_extensions_path' from
     $GUILE_SYSTEM_EXTENSIONS_PATH, or if that's not set:
     <SCM_LIB_DIR> <LT_PATHSEP_CHAR> <SCM_EXTENSIONS_DIR>.

     'lt_dladdsearchdir' can't be used because it is searched before
     the system-dependent search path, which is the one 'libtool
     --mode=execute -dlopen' fiddles with (info "(libtool) Libltdl
     Interface").  See
     <http://lists.gnu.org/archive/html/guile-devel/2010-11/msg00095.html>.

     The environment variables $LTDL_LIBRARY_PATH and $LD_LIBRARY_PATH
     can't be used because they would be propagated to subprocesses
     which may cause problems for other programs.  See
     <http://lists.gnu.org/archive/html/guile-devel/2012-09/msg00037.html> */

  env = getenv ("GUILE_SYSTEM_EXTENSIONS_PATH");
  if (env)
    system_extensions_path = env;
  else
    {
      system_extensions_path
        = scm_gc_malloc_pointerless (strlen (SCM_LIB_DIR)
                                     + strlen (SCM_EXTENSIONS_DIR) + 2,
                                     "system_extensions_path");
      sprintf (system_extensions_path, "%s%c%s",
               SCM_LIB_DIR, LT_PATHSEP_CHAR, SCM_EXTENSIONS_DIR);
    }
}

scm_t_bits scm_tc16_dynamic_obj;

#define DYNL_FILENAME         SCM_SMOB_OBJECT
#define DYNL_HANDLE(x)        ((void *) SCM_SMOB_DATA_2 (x))
#define SET_DYNL_HANDLE(x, v) (SCM_SET_SMOB_DATA_2 ((x), (scm_t_bits) (v)))



static int
dynl_obj_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<dynamic-object ", port);
  scm_iprin1 (DYNL_FILENAME (exp), port, pstate);
  if (DYNL_HANDLE (exp) == NULL)
    scm_puts (" (unlinked)", port);
  scm_putc ('>', port);
  return 1;
}


SCM_DEFINE (scm_dynamic_link, "dynamic-link", 0, 1, 0,
            (SCM filename),
	    "Find the shared object (shared library) denoted by\n"
	    "@var{filename} and link it into the running Guile\n"
	    "application.  The returned\n"
	    "scheme object is a ``handle'' for the library which can\n"
	    "be passed to @code{dynamic-func}, @code{dynamic-call} etc.\n\n"
	    "Searching for object files is system dependent.  Normally,\n"
	    "if @var{filename} does have an explicit directory it will\n"
	    "be searched for in locations\n"
	    "such as @file{/usr/lib} and @file{/usr/local/lib}.\n\n"
	    "When @var{filename} is omitted, a @dfn{global symbol handle} is\n"
	    "returned.  This handle provides access to the symbols\n"
	    "available to the program at run-time, including those exported\n"
	    "by the program itself and the shared libraries already loaded.\n")
#define FUNC_NAME s_scm_dynamic_link
{
  void *handle;
  char *file;

  scm_dynwind_begin (0);

  if (SCM_UNBNDP (filename))
    file = NULL;
  else
    {
      file = scm_to_locale_string (filename);
      scm_dynwind_free (file);
    }

  handle = sysdep_dynl_link (file, FUNC_NAME);
  scm_dynwind_end ();

  SCM_RETURN_NEWSMOB2 (scm_tc16_dynamic_obj,
		       SCM_UNBNDP (filename)
		       ? SCM_UNPACK (SCM_BOOL_F) : SCM_UNPACK (filename),
		       handle);
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_object_p, "dynamic-object?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a dynamic object handle,\n"
	    "or @code{#f} otherwise.")
#define FUNC_NAME s_scm_dynamic_object_p
{
  return scm_from_bool (SCM_TYP16_PREDICATE (scm_tc16_dynamic_obj, obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_unlink, "dynamic-unlink", 1, 0, 0, 
            (SCM dobj),
	    "Unlink a dynamic object from the application, if possible.  The\n"
	    "object must have been linked by @code{dynamic-link}, with \n"
	    "@var{dobj} the corresponding handle.  After this procedure\n"
	    "is called, the handle can no longer be used to access the\n"
	    "object.")
#define FUNC_NAME s_scm_dynamic_unlink
{
  /*fixme* GC-problem */
  SCM_VALIDATE_SMOB (SCM_ARG1, dobj, dynamic_obj);
  if (DYNL_HANDLE (dobj) == NULL) {
    SCM_MISC_ERROR ("Already unlinked: ~S", scm_list_1 (dobj));
  } else {
    sysdep_dynl_unlink (DYNL_HANDLE (dobj), FUNC_NAME);
    SET_DYNL_HANDLE (dobj, NULL);
    return SCM_UNSPECIFIED;
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_pointer, "dynamic-pointer", 2, 0, 0,
            (SCM name, SCM dobj),
	    "Return a ``wrapped pointer'' to the symbol @var{name}\n"
	    "in the shared object referred to by @var{dobj}.  The returned\n"
	    "pointer points to a C object.\n\n"
	    "Regardless whether your C compiler prepends an underscore\n"
	    "@samp{_} to the global names in a program, you should\n"
	    "@strong{not} include this underscore in @var{name}\n"
	    "since it will be added automatically when necessary.")
#define FUNC_NAME s_scm_dynamic_pointer
{
  void *val;

  SCM_VALIDATE_STRING (1, name);
  SCM_VALIDATE_SMOB (SCM_ARG2, dobj, dynamic_obj);

  if (DYNL_HANDLE (dobj) == NULL)
    SCM_MISC_ERROR ("Already unlinked: ~S", dobj);
  else
    {
      char *chars;

      scm_dynwind_begin (0);
      chars = scm_to_locale_string (name);
      scm_dynwind_free (chars);
      val = sysdep_dynl_value (chars, DYNL_HANDLE (dobj), FUNC_NAME);
      scm_dynwind_end ();

      return scm_from_pointer (val, NULL);
    }
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_func, "dynamic-func", 2, 0, 0, 
            (SCM name, SCM dobj),
	    "Return a ``handle'' for the function @var{name} in the\n"
	    "shared object referred to by @var{dobj}.  The handle\n"
	    "can be passed to @code{dynamic-call} to actually\n"
	    "call the function.\n\n"
	    "Regardless whether your C compiler prepends an underscore\n"
	    "@samp{_} to the global names in a program, you should\n"
	    "@strong{not} include this underscore in @var{name}\n"
	    "since it will be added automatically when necessary.")
#define FUNC_NAME s_scm_dynamic_func
{
  return scm_dynamic_pointer (name, dobj);
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_call, "dynamic-call", 2, 0, 0, 
            (SCM func, SCM dobj),
	    "Call a C function in a dynamic object.  Two styles of\n"
	    "invocation are supported:\n\n"
	    "@itemize @bullet\n"
	    "@item @var{func} can be a function handle returned by\n"
	    "@code{dynamic-func}.  In this case @var{dobj} is\n"
	    "ignored\n"
	    "@item @var{func} can be a string with the name of the\n"
	    "function to call, with @var{dobj} the handle of the\n"
	    "dynamic object in which to find the function.\n"
	    "This is equivalent to\n"
	    "@smallexample\n\n"
	    "(dynamic-call (dynamic-func @var{func} @var{dobj}) #f)\n"
	    "@end smallexample\n"
	    "@end itemize\n\n"
	    "In either case, the function is passed no arguments\n"
	    "and its return value is ignored.")
#define FUNC_NAME s_scm_dynamic_call
{
  void (*fptr) (void);

  if (scm_is_string (func))
    func = scm_dynamic_func (func, dobj);
  SCM_VALIDATE_POINTER (SCM_ARG1, func);

  fptr = SCM_POINTER_VALUE (func);
  fptr ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_init_dynamic_linking ()
{
  scm_tc16_dynamic_obj = scm_make_smob_type ("dynamic-object", 0);
  scm_set_smob_print (scm_tc16_dynamic_obj, dynl_obj_print);
  sysdep_dynl_init ();
#include "libguile/dynl.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

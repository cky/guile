/* dynl.c - dynamic linking
 *
 * Copyright (C) 1990, 91, 92, 93, 94, 95, 96, 97, 98, 99, 2000, 2001 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/dynl.h"
#include "libguile/smob.h"
#include "libguile/keywords.h"
#include "libguile/ports.h"
#include "libguile/strings.h"
#include "libguile/deprecation.h"
#include "libguile/lang.h"
#include "libguile/validate.h"

#include "libltdl/ltdl.h"

/* From the libtool manual: "Note that libltdl is not threadsafe,
   i.e. a multithreaded application has to use a mutex for libltdl.".

   Guile does not currently support pre-emptive threads, so there is
   no mutex.  Previously SCM_DEFER_INTS and SCM_ALLOW_INTS were used:
   they are mentioned here in case somebody is grepping for thread
   problems ;)
*/

static void *
sysdep_dynl_link (const char *fname, const char *subr)
{
  lt_dlhandle handle;
  handle = lt_dlopenext (fname);
  if (NULL == handle)
    {
      SCM fn;
      SCM msg;

      fn = scm_makfrom0str (fname);
      msg = scm_makfrom0str (lt_dlerror ());
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
sysdep_dynl_func (const char *symb, void *handle, const char *subr)
{
  void *fptr;

  fptr = lt_dlsym ((lt_dlhandle) handle, symb);
  if (!fptr)
    {
      scm_misc_error (subr, (char *) lt_dlerror (), SCM_EOL);
    }
  return fptr;
}

static void
sysdep_dynl_init ()
{
  lt_dlinit ();
}

scm_t_bits scm_tc16_dynamic_obj;

#define DYNL_FILENAME(x)        (SCM_CELL_OBJECT_1 (x))
#define DYNL_HANDLE(x)          ((void *) SCM_CELL_WORD_2 (x))
#define SET_DYNL_HANDLE(x, v)   (SCM_SET_CELL_WORD_2 ((x), (v)))


static SCM
dynl_obj_mark (SCM ptr)
{
  return DYNL_FILENAME (ptr);
}


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


SCM_DEFINE (scm_dynamic_link, "dynamic-link", 1, 0, 0, 
            (SCM filename),
	    "Find the shared object (shared library) denoted by\n"
	    "@var{filename} and link it into the running Guile\n"
	    "application.  The returned\n"
	    "scheme object is a ``handle'' for the library which can\n"
	    "be passed to @code{dynamic-func}, @code{dynamic-call} etc.\n\n"
	    "Searching for object files is system dependent.  Normally,\n"
	    "if @var{filename} does have an explicit directory it will\n"
	    "be searched for in locations\n"
	    "such as @file{/usr/lib} and @file{/usr/local/lib}.")
#define FUNC_NAME s_scm_dynamic_link
{
  void *handle;

  SCM_VALIDATE_STRING (1, filename);
  handle = sysdep_dynl_link (SCM_STRING_CHARS (filename), FUNC_NAME);
  SCM_RETURN_NEWSMOB2 (scm_tc16_dynamic_obj, SCM_UNPACK (filename), handle);
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_object_p, "dynamic-object?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a dynamic object handle,\n"
	    "or @code{#f} otherwise.")
#define FUNC_NAME s_scm_dynamic_object_p
{
  return SCM_BOOL (SCM_TYP16_PREDICATE (scm_tc16_dynamic_obj, obj));
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
    SCM_MISC_ERROR ("Already unlinked: ~S", dobj);
  } else {
    sysdep_dynl_unlink (DYNL_HANDLE (dobj), FUNC_NAME);
    SET_DYNL_HANDLE (dobj, NULL);
    return SCM_UNSPECIFIED;
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
  /* The returned handle is formed by casting the address of the function to a
   * long value and converting this to a scheme number
   */

  void (*func) ();

  SCM_VALIDATE_STRING (1, name);
  /*fixme* GC-problem */
  SCM_VALIDATE_SMOB (SCM_ARG2, dobj, dynamic_obj);
  if (DYNL_HANDLE (dobj) == NULL) {
    SCM_MISC_ERROR ("Already unlinked: ~S", dobj);
  } else {
    char *chars;

    chars = SCM_STRING_CHARS (name);
    func = (void (*) ()) sysdep_dynl_func (chars, DYNL_HANDLE (dobj), FUNC_NAME);
    return scm_ulong2num ((unsigned long) func);
  }
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
  void (*fptr) ();
  
  if (SCM_STRINGP (func))
    func = scm_dynamic_func (func, dobj);
  fptr = (void (*) ()) SCM_NUM2ULONG (1, func);
  fptr ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* return a newly allocated array of char pointers to each of the strings
   in args, with a terminating NULL pointer.  */
/* Note: a similar function is defined in posix.c, but we don't necessarily
   want to export it.  */
static char **allocate_string_pointers (SCM args, int *num_args_return)
{
  char **result;
  int n_args = scm_ilength (args);
  int i;

  SCM_ASSERT (n_args >= 0, args, SCM_ARGn, "allocate_string_pointers");
  result = (char **) scm_malloc ((n_args + 1) * sizeof (char *));
  result[n_args] = NULL;
  for (i = 0; i < n_args; i++)
    {
      SCM car = SCM_CAR (args);

      if (!SCM_STRINGP (car))
	{
	  free (result);
	  scm_wrong_type_arg ("allocate_string_pointers", SCM_ARGn, car);
	}
      result[i] = SCM_STRING_CHARS (SCM_CAR (args));
      args = SCM_CDR (args);
    }
  *num_args_return = n_args;
  return result;
}

SCM_DEFINE (scm_dynamic_args_call, "dynamic-args-call", 3, 0, 0, 
            (SCM func, SCM dobj, SCM args),
	    "Call the C function indicated by @var{func} and @var{dobj},\n"
	    "just like @code{dynamic-call}, but pass it some arguments and\n"
	    "return its return value.  The C function is expected to take\n"
	    "two arguments and return an @code{int}, just like @code{main}:\n"
	    "@smallexample\n"
	    "int c_func (int argc, char **argv);\n"
	    "@end smallexample\n\n"
	    "The parameter @var{args} must be a list of strings and is\n"
	    "converted into an array of @code{char *}.  The array is passed\n"
	    "in @var{argv} and its size in @var{argc}.  The return value is\n"
	    "converted to a Scheme number and returned from the call to\n"
	    "@code{dynamic-args-call}.")
#define FUNC_NAME s_scm_dynamic_args_call
{
  int (*fptr) (int argc, char **argv);
  int result, argc;
  char **argv;

  if (SCM_STRINGP (func))
    func = scm_dynamic_func (func, dobj);

  fptr = (int (*) (int, char **)) SCM_NUM2ULONG (1, func);
  argv = allocate_string_pointers (args, &argc);
  /* if the procedure mutates its arguments, the original strings will be
     changed -- in Guile 1.6 and earlier, this wasn't the case since a
     new copy of each string was allocated.  */
  result = (*fptr) (argc, argv);
  free (argv);

  return SCM_MAKINUM (0L + result);
}
#undef FUNC_NAME

void
scm_init_dynamic_linking ()
{
  scm_tc16_dynamic_obj = scm_make_smob_type ("dynamic-object", 0);
  scm_set_smob_mark (scm_tc16_dynamic_obj, dynl_obj_mark);
  scm_set_smob_print (scm_tc16_dynamic_obj, dynl_obj_print);
  sysdep_dynl_init ();
#include "libguile/dynl.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

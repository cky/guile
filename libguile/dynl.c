/* dynl.c - dynamic linking
 *
 * Copyright (C) 1990, 91, 92, 93, 94, 95, 96, 97, 98, 99, 2000 Free Software Foundation, Inc.
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

#include "libguile/validate.h"

/* Create a new C argv array from a scheme list of strings. */
/* Dirk:FIXME:: A quite similar function is implemented in posix.c */
/* Dirk:FIXME:: In case of assertion errors, we get memory leaks */

/* Converting a list of SCM strings into a argv-style array.  You must
   have ints disabled for the whole lifetime of the created argv (from
   before MAKE_ARGV_FROM_STRINGLIST until after
   MUST_FREE_ARGV). Atleast this is was the documentation for
   MAKARGVFROMSTRS says, it isn't really used that way.

   This code probably belongs into strings.c 
   (Dirk: IMO strings.c is not the right place.) */

static char **
scm_make_argv_from_stringlist (SCM args,int *argcp,const char *subr,int argn)
{
  char **argv;
  int argc;
  int i;

  argc = scm_ilength (args);
  SCM_ASSERT (argc >= 0, args, argn, subr);
  argv = (char **) scm_must_malloc ((argc + 1) * sizeof (char *), subr);
  for (i = 0; !SCM_NULLP (args); args = SCM_CDR (args), ++i) {
    SCM arg = SCM_CAR (args);
    size_t len;
    char *dst;
    char *src;

    SCM_ASSERT (SCM_STRINGP (arg), args, argn, subr);
    len = SCM_STRING_LENGTH (arg);
    src = SCM_STRING_CHARS (arg);
    dst = (char *) scm_must_malloc (len + 1, subr);
    memcpy (dst, src, len);
    dst[len] = 0;
    argv[i] = dst;
  }

  if (argcp)
    *argcp = argc;
  argv[argc] = 0;
  return argv;
}

static void
scm_must_free_argv(char **argv)
{
  char **av = argv;
  while (*av)
    free (*(av++));
  free (argv);
}

/* Module registry
 */

/* We can't use SCM objects here. One should be able to call
   SCM_REGISTER_MODULE from a C++ constructor for a static
   object. This happens before main and thus before libguile is
   initialized. */

struct moddata {
  struct moddata *link;
  char *module_name;
  void *init_func;
};

static struct moddata *registered_mods = NULL;

void
scm_register_module_xxx (char *module_name, void *init_func)
{
  struct moddata *md;

  /* XXX - should we (and can we) DEFER_INTS here? */

  for (md = registered_mods; md; md = md->link)
    if (!strcmp (md->module_name, module_name))
      {
	md->init_func = init_func;
	return;
      }

  md = (struct moddata *) malloc (sizeof (struct moddata));
  if (md == NULL)
    {
      fprintf (stderr,
	       "guile: can't register module (%s): not enough memory",
	       module_name);
      return;
    }

  md->module_name = module_name;
  md->init_func = init_func;
  md->link = registered_mods;
  registered_mods = md;
}

SCM_DEFINE (scm_registered_modules, "c-registered-modules", 0, 0, 0, 
            (),
	    "Return a list of the object code modules that have been imported into\n"
	    "the current Guile process.  Each element of the list is a pair whose\n"
	    "car is the name of the module, and whose cdr is the function handle\n"
	    "for that module's initializer function.  The name is the string that\n"
	    "has been passed to scm_register_module_xxx.")
#define FUNC_NAME s_scm_registered_modules
{
  SCM res;
  struct moddata *md;

  res = SCM_EOL;
  for (md = registered_mods; md; md = md->link)
    res = scm_cons (scm_cons (scm_makfrom0str (md->module_name),
			      scm_ulong2num ((unsigned long) md->init_func)),
		    res);
  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_clear_registered_modules, "c-clear-registered-modules", 0, 0, 0, 
            (),
	    "Destroy the list of modules registered with the current Guile process.\n"
	    "The return value is unspecified.  @strong{Warning:} this function does\n"
	    "not actually unlink or deallocate these modules, but only destroys the\n"
	    "records of which modules have been loaded.  It should therefore be used\n"
	    "only by module bookkeeping operations.")
#define FUNC_NAME s_scm_clear_registered_modules
{
  struct moddata *md1, *md2;

  SCM_DEFER_INTS;

  for (md1 = registered_mods; md1; md1 = md2)
    {
      md2 = md1->link;
      free (md1);
    }
  registered_mods = NULL;

  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Dispatch to the system dependent files
 *
 * They define some static functions.  These functions are called with
 * deferred interrupts.  When they want to throw errors, they are
 * expected to insert a SCM_ALLOW_INTS before doing the throw.  It
 * might work to throw an error while interrupts are deferred (because
 * they will be unconditionally allowed the next time a SCM_ALLOW_INTS
 * is executed, SCM_DEFER_INTS and SCM_ALLOW_INTS do not nest).
 */

#ifdef DYNAMIC_LINKING

#include "libltdl/ltdl.h"

static void *
sysdep_dynl_link (const char *fname, const char *subr)
{
  lt_dlhandle handle;
  handle = lt_dlopenext (fname);
  if (NULL == handle)
    {
      SCM fn;
      SCM msg;

      SCM_ALLOW_INTS;
      fn = scm_makfrom0str (fname);
      msg = scm_makfrom0str (lt_dlerror ());
      scm_misc_error (subr, "file: ~S, message: ~S", SCM_LIST2 (fn, msg));
    }
  return (void *) handle;
}

static void
sysdep_dynl_unlink (void *handle, const char *subr)
{
  if (lt_dlclose ((lt_dlhandle) handle))
    {
      SCM_ALLOW_INTS;
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
      SCM_ALLOW_INTS;
      scm_misc_error (subr, (char *) lt_dlerror (), SCM_EOL);
    }
  return fptr;
}

static void
sysdep_dynl_init ()
{
  lt_dlinit ();
}

#else

/* no dynamic linking available, throw errors. */

static void
sysdep_dynl_init (void)
{
}

static void
no_dynl_error (const char *subr)
{
  SCM_ALLOW_INTS;
  scm_misc_error (subr, "dynamic linking not available", SCM_EOL);
}
    
static void *
sysdep_dynl_link (const char *filename, const char *subr)
{
  no_dynl_error (subr);
  return NULL;
}

static void 
sysdep_dynl_unlink (void *handle, 
		    const char *subr)
{
  no_dynl_error (subr);
}

static void *
sysdep_dynl_func (const char *symbol, 
		  void *handle,
		  const char *subr)
{
  no_dynl_error (subr);
  return NULL;
}

#endif

scm_bits_t scm_tc16_dynamic_obj;

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
	    "Open the dynamic library called @var{filename}.  A library\n"
	    "handle representing the opened library is returned; this handle\n"
	    "should be used as the @var{dobj} argument to the following\n"
	    "functions.")
#define FUNC_NAME s_scm_dynamic_link
{
  void *handle;

  SCM_VALIDATE_STRING (1, filename);
  SCM_STRING_COERCE_0TERMINATION_X (filename);
  handle = sysdep_dynl_link (SCM_STRING_CHARS (filename), FUNC_NAME);
  SCM_RETURN_NEWSMOB2 (scm_tc16_dynamic_obj, SCM_UNPACK (filename), handle);
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_object_p, "dynamic-object?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a dynamic library handle, or @code{#f}\n"
	    "otherwise.")
#define FUNC_NAME s_scm_dynamic_object_p
{
  return SCM_BOOL (SCM_TYP16_PREDICATE (scm_tc16_dynamic_obj, obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_unlink, "dynamic-unlink", 1, 0, 0, 
            (SCM dobj),
	    "Unlink the indicated object file from the application.  The\n"
	    "argument @var{dobj} must have been obtained by a call to\n"
	    "@code{dynamic-link}.  After @code{dynamic-unlink} has been\n"
	    "called on @var{dobj}, its content is no longer accessible.")
#define FUNC_NAME s_scm_dynamic_unlink
{
  /*fixme* GC-problem */
  SCM_VALIDATE_SMOB (SCM_ARG1, dobj, dynamic_obj);
  if (DYNL_HANDLE (dobj) == NULL) {
    SCM_MISC_ERROR ("Already unlinked: ~S", dobj);
  } else {
    SCM_DEFER_INTS;
    sysdep_dynl_unlink (DYNL_HANDLE (dobj), FUNC_NAME);
    SET_DYNL_HANDLE (dobj, NULL);
    SCM_ALLOW_INTS;
    return SCM_UNSPECIFIED;
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_func, "dynamic-func", 2, 0, 0, 
            (SCM name, SCM dobj),
	    "Search the dynamic object @var{dobj} for the C function\n"
	    "indicated by the string @var{name} and return some Scheme\n"
	    "handle that can later be used with @code{dynamic-call} to\n"
	    "actually call the function.\n\n"
	    "Regardless whether your C compiler prepends an underscore @samp{_} to\n"
	    "the global names in a program, you should @strong{not} include this\n"
	    "underscore in @var{function}.  Guile knows whether the underscore is\n"
	    "needed or not and will add it when necessary.")
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

    SCM_DEFER_INTS;
    SCM_STRING_COERCE_0TERMINATION_X (name);
    chars = SCM_STRING_CHARS (name);
    func = (void (*) ()) sysdep_dynl_func (chars, DYNL_HANDLE (dobj), FUNC_NAME);
    SCM_ALLOW_INTS;
    return scm_ulong2num ((unsigned long) func);
  }
}
#undef FUNC_NAME


SCM_DEFINE (scm_dynamic_call, "dynamic-call", 2, 0, 0, 
            (SCM func, SCM dobj),
	    "Call the C function indicated by @var{func} and @var{dobj}.\n"
	    "The function is passed no arguments and its return value is\n"
	    "ignored.  When @var{function} is something returned by\n"
	    "@code{dynamic-func}, call that function and ignore @var{dobj}.\n"
	    "When @var{func} is a string , look it up in @var{dynobj}; this\n"
	    "is equivalent to\n"
	    "@smallexample\n"
	    "(dynamic-call (dynamic-func @var{func} @var{dobj} #f))\n"
	    "@end smallexample\n\n"
	    "Interrupts are deferred while the C function is executing (with\n"
	    "@code{SCM_DEFER_INTS}/@code{SCM_ALLOW_INTS}).")
#define FUNC_NAME s_scm_dynamic_call
{
  void (*fptr) ();
  
  if (SCM_STRINGP (func))
    func = scm_dynamic_func (func, dobj);
  fptr = (void (*) ()) SCM_NUM2ULONG (1, func);
  SCM_DEFER_INTS;
  fptr ();
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

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
  SCM_DEFER_INTS;
  argv = scm_make_argv_from_stringlist (args, &argc, FUNC_NAME, SCM_ARG3);
  result = (*fptr) (argc, argv);
  scm_must_free_argv (argv);
  SCM_ALLOW_INTS;

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
#ifndef SCM_MAGIC_SNARFER
#include "libguile/dynl.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* dynl.c - dynamic linking
 *
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
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
#include "_scm.h"
#include "dynl.h"
#include "genio.h"
#include "smob.h"
#include "keywords.h"

/* Converting a list of SCM strings into a argv-style array.  You must
   have ints disabled for the whole lifetime of the created argv (from
   before MAKE_ARGV_FROM_STRINGLIST until after
   MUST_FREE_ARGV). Atleast this is was the documentation for
   MAKARGVFROMSTRS says, it isn't really used that way.

   This code probably belongs into strings.c */

static char **
scm_make_argv_from_stringlist (SCM args,int *argcp,const char *subr,int argn)
{
    char **argv;
    int argc, i;

    argc = scm_ilength(args);
    argv = (char **) scm_must_malloc ((1L+argc)*sizeof(char *), subr);
    for(i = 0; SCM_NNULLP (args); args = SCM_CDR (args), i++) {
	size_t len;
	char *dst, *src;
	SCM str = SCM_CAR (args);

	SCM_ASSERT (SCM_NIMP (str) && SCM_ROSTRINGP (str), str, argn, subr);
	len = 1 + SCM_ROLENGTH (str);
	dst = (char *) scm_must_malloc ((long)len, subr);
	src = SCM_ROCHARS (str);
	while (len--)
	    dst[len] = src[len];
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
      free(*(av++));
    free(argv);
}

/* Coerce an arbitrary readonly-string into a zero-terminated string.
 */

static SCM
scm_coerce_rostring (SCM rostr,const char *subr,int argn)
{
    SCM_ASSERT (SCM_NIMP (rostr) && SCM_ROSTRINGP (rostr), rostr, argn, subr);
    if (SCM_SUBSTRP (rostr))
	rostr = scm_makfromstr (SCM_ROCHARS (rostr), SCM_ROLENGTH (rostr), 0);
    return rostr;
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
	if (!strcmp (md->module_name, module_name)) {
	    md->init_func = init_func;
	    return;
	}

    md = (struct moddata *)malloc (sizeof (struct moddata));
    if (md == NULL) {
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

GUILE_PROC (scm_registered_modules, "c-registered-modules", 0, 0, 0, 
            (),
"")
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

GUILE_PROC (scm_clear_registered_modules, "c-clear-registered-modules", 0, 0, 0, 
            (),
"")
#define FUNC_NAME s_scm_clear_registered_modules
{
    struct moddata *md1, *md2;

    SCM_DEFER_INTS;

    for (md1 = registered_mods; md1; md1 = md2) {
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

#define DYNL_GLOBAL 0x0001

#ifdef HAVE_DLOPEN
#include "dynl-dl.c"
#else
#ifdef HAVE_SHL_LOAD
#include "dynl-shl.c"
#else
#ifdef HAVE_LIBDLD
#include "dynl-dld.c"
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
sysdep_dynl_link (const char *filename,
		  int flags,
		  const char *subr)
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
#endif
#endif

int scm_tc16_dynamic_obj;

struct dynl_obj {
    SCM filename;
    void *handle;
};

static SCM
mark_dynl_obj (SCM ptr)
{
    struct dynl_obj *d = (struct dynl_obj *)SCM_CDR (ptr);
    return d->filename;
}

static scm_sizet
free_dynl_obj (SCM ptr)
{
  scm_must_free ((char *)SCM_CDR (ptr));
  return sizeof (struct dynl_obj);
}

static int
print_dynl_obj (SCM exp,SCM port,scm_print_state *pstate)
{
    struct dynl_obj *d = (struct dynl_obj *)SCM_CDR (exp);
    scm_puts ("#<dynamic-object ", port);
    scm_iprin1 (d->filename, port, pstate);
    if (d->handle == NULL)
      scm_puts (" (unlinked)", port);
    scm_putc ('>', port);
    return 1;
}

static SCM kw_global;
SCM_SYMBOL (sym_global, "-global");

GUILE_PROC (scm_dynamic_link, "dynamic-link", 1, 0, 1, 
            (SCM fname, SCM rest),
"")
#define FUNC_NAME s_scm_dynamic_link
{
    SCM z;
    void *handle;
    struct dynl_obj *d;
    int flags = DYNL_GLOBAL;

    fname = scm_coerce_rostring (fname, FUNC_NAME, SCM_ARG1);

    /* collect flags */
    while (SCM_NIMP (rest) && SCM_CONSP (rest))
      {
	SCM kw, val;

	kw = SCM_CAR (rest);
	rest = SCM_CDR (rest);
	
	if (!(SCM_NIMP (rest) && SCM_CONSP (rest)))
	  scm_misc_error (FUNC_NAME, "keyword without value", SCM_EOL);
	
	val = SCM_CAR (rest);
	rest = SCM_CDR (rest);

	if (kw == kw_global)
	  {
	    if (SCM_FALSEP (val))
	      flags &= ~DYNL_GLOBAL;
	  }
	else
	  scm_misc_error (FUNC_NAME, "unknown keyword argument: %s",
			  scm_cons (kw, SCM_EOL));
      }

    SCM_DEFER_INTS;
    handle = sysdep_dynl_link (SCM_CHARS (fname), flags, FUNC_NAME);

    d = (struct dynl_obj *)scm_must_malloc (sizeof (struct dynl_obj),
					    FUNC_NAME);
    d->filename = fname;
    d->handle = handle;

    SCM_NEWCELL (z);
    SCM_SETCHARS (z, d);
    SCM_SETCAR (z, scm_tc16_dynamic_obj);
    SCM_ALLOW_INTS;

    return z;
}
#undef FUNC_NAME

static struct dynl_obj *
get_dynl_obj (SCM dobj,const char *subr,int argn)
{
    struct dynl_obj *d;
    SCM_ASSERT (SCM_NIMP (dobj) && SCM_CAR (dobj) == scm_tc16_dynamic_obj,
		dobj, argn, subr);
    d = (struct dynl_obj *)SCM_CDR (dobj);
    SCM_ASSERT (d->handle != NULL, dobj, argn, subr);
    return d;
}

GUILE_PROC (scm_dynamic_object_p, "dynamic-object?", 1, 0, 0, 
            (SCM obj),
"")
#define FUNC_NAME s_scm_dynamic_object_p
{
    return SCM_BOOL(SCM_NIMP (obj) && SCM_CAR (obj) == scm_tc16_dynamic_obj);
}
#undef FUNC_NAME

GUILE_PROC (scm_dynamic_unlink, "dynamic-unlink", 1, 0, 0, 
            (SCM dobj),
"")
#define FUNC_NAME s_scm_dynamic_unlink
{
    struct dynl_obj *d = get_dynl_obj (dobj, FUNC_NAME, SCM_ARG1);
    SCM_DEFER_INTS;
    sysdep_dynl_unlink (d->handle, FUNC_NAME);
    d->handle = NULL;
    SCM_ALLOW_INTS;
    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

GUILE_PROC (scm_dynamic_func, "dynamic-func", 2, 0, 0, 
            (SCM symb, SCM dobj),
"")
#define FUNC_NAME s_scm_dynamic_func
{
    struct dynl_obj *d;
    void (*func) ();

    symb = scm_coerce_rostring (symb, FUNC_NAME, SCM_ARG1);
    d = get_dynl_obj (dobj, FUNC_NAME, SCM_ARG2);

    SCM_DEFER_INTS;
    func = (void (*) ()) sysdep_dynl_func (SCM_CHARS (symb), d->handle,
					   FUNC_NAME);
    SCM_ALLOW_INTS;

    return scm_ulong2num ((unsigned long)func);
}
#undef FUNC_NAME

GUILE_PROC (scm_dynamic_call, "dynamic-call", 2, 0, 0, 
            (SCM func, SCM dobj),
"")
#define FUNC_NAME s_scm_dynamic_call
{
    void (*fptr)();

    if (SCM_NIMP (func) && SCM_ROSTRINGP (func))
	func = scm_dynamic_func (func, dobj);
    fptr = (void (*)()) scm_num2ulong (func, (char *)SCM_ARG1, FUNC_NAME);
    SCM_DEFER_INTS;
    fptr ();
    SCM_ALLOW_INTS;
    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

GUILE_PROC (scm_dynamic_args_call, "dynamic-args-call", 3, 0, 0, 
            (SCM func, SCM dobj, SCM args),
"")
#define FUNC_NAME s_scm_dynamic_args_call
{
    int (*fptr) (int argc, char **argv);
    int result, argc;
    char **argv;

    if (SCM_NIMP (func) && SCM_ROSTRINGP (func))
	func = scm_dynamic_func (func, dobj);

    fptr = (int (*)(int, char **)) scm_num2ulong (func, (char *)SCM_ARG1,
						   FUNC_NAME);
    SCM_DEFER_INTS;
    argv = scm_make_argv_from_stringlist (args, &argc, FUNC_NAME,
					  SCM_ARG3);
    result = (*fptr) (argc, argv);
    scm_must_free_argv (argv);
    SCM_ALLOW_INTS;

    return SCM_MAKINUM(0L+result);
}
#undef FUNC_NAME

void
scm_init_dynamic_linking ()
{
    scm_tc16_dynamic_obj = scm_make_smob_type_mfpe ("dynamic-object", sizeof (struct dynl_obj),
                                                   mark_dynl_obj, free_dynl_obj, 
                                                   print_dynl_obj, NULL);
    sysdep_dynl_init ();
#include "dynl.x"
    kw_global = scm_make_keyword_from_dash_symbol (sym_global);
}

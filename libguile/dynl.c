/* dynl.c - dynamic linking
 *
 * Copyright (C) 1990-1997 Free Software Foundation, Inc.
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

/* "dynl.c" dynamically link&load object files.
   Author: Aubrey Jaffer
   Modified for libguile by Marius Vollmer */

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

#include <stdio.h>

#include "_scm.h"
#include "dynl.h"
#include "genio.h"
#include "smob.h"

/* Converting a list of SCM strings into a argv-style array.  You must
   have ints disabled for the whole lifetime of the created argv (from
   before MAKE_ARGV_FROM_STRINGLIST until after
   MUST_FREE_ARGV). Atleast this is was the documentation for
   MAKARGVFROMSTRS says, it isn't really used that way.

   This code probably belongs into strings.c */

static char **scm_make_argv_from_stringlist SCM_P ((SCM args, int *argcp,
						    char *subr, int argn));

static char **
scm_make_argv_from_stringlist (args, argcp, subr, argn)
     SCM args;
     int *argcp;
     char *subr;
     int argn;
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

static void scm_must_free_argv SCM_P ((char **argv));

static void
scm_must_free_argv(argv)
     char **argv;
{
    char **av = argv;
    while(!(*av))
	free(*(av++));
    free(argv);
}

/* Coerce an arbitrary readonly-string into a zero-terminated string.
 */

static SCM scm_coerce_rostring SCM_P ((SCM rostr, char *subr, int argn));

static SCM
scm_coerce_rostring (rostr, subr, argn)
     SCM rostr;
     char *subr;
     int argn;
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
scm_register_module_xxx (module_name, init_func)
     char *module_name;
     void *init_func;
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

SCM_PROC (s_registered_modules, "c-registered-modules", 0, 0, 0, scm_registered_modules);

SCM
scm_registered_modules ()
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

SCM_PROC (s_clear_registered_modules, "c-clear-registered-modules", 0, 0, 0, scm_clear_registered_modules);

SCM
scm_clear_registered_modules ()
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

/* Dispatch to the system dependent files
 *
 * They define these static functions:
 */

static void sysdep_dynl_init SCM_P ((void));
static void *sysdep_dynl_link SCM_P ((char *filename, char *subr));
static void sysdep_dynl_unlink SCM_P ((void *handle, char *subr));
static void *sysdep_dynl_func SCM_P ((char *symbol, void *handle, char *subr));

#ifdef HAVE_LIBDL
#include "dynl-dl.c"
#else
#ifdef HAVE_SHL_LOAD
#include "dynl-shl.c"
#else
#ifdef HAVE_DLD
#include "dynl-dld.c"
#else 

/* no dynamic linking available, throw errors. */

static void
sysdep_dynl_init ()
{
}

static void
no_dynl_error (subr)
     char *subr;
{
    scm_misc_error (subr, "dynamic linking not available", SCM_EOL);
}
    
static void *
sysdep_dynl_link (filename, subr)
     char *filename;
     char *subr;
{
    no_dynl_error (subr);
    return NULL;
}

static void 
sysdep_dynl_unlink (handle, subr)
     void *handle;
     char *subr;
{
    no_dynl_error (subr);
}

static void *
sysdep_dynl_func (symbol, handle, subr)
     char *symbol;
     void *handle;
     char *subr;
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

static SCM mark_dynl_obj SCM_P ((SCM ptr));
static SCM
mark_dynl_obj (ptr)
     SCM ptr;
{
    struct dynl_obj *d = (struct dynl_obj *)SCM_CDR (ptr);
    SCM_SETGC8MARK (ptr);
    return d->filename;
}

static int print_dynl_obj SCM_P ((SCM exp, SCM port, scm_print_state *pstate));
static int
print_dynl_obj (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
    struct dynl_obj *d = (struct dynl_obj *)SCM_CDR (exp);
    scm_gen_puts (scm_regular_string, "#<dynamic-object ", port);
    scm_iprin1 (d->filename, port, pstate);
    if (d->handle == NULL)
      scm_gen_puts (scm_regular_string, " (unlinked)", port);
    scm_gen_putc ('>', port);
    return 1;
}

static scm_smobfuns dynl_obj_smob = {
    mark_dynl_obj,
    scm_free0,
    print_dynl_obj
};
  
SCM_PROC (s_dynamic_link, "dynamic-link", 1, 0, 0, scm_dynamic_link);

SCM
scm_dynamic_link (fname)
     SCM fname;
{
    SCM z;
    struct dynl_obj *d;

    fname = scm_coerce_rostring (fname, s_dynamic_link, SCM_ARG1);
    d = (struct dynl_obj *)scm_must_malloc (sizeof (struct dynl_obj),
					    s_dynamic_link);
    d->filename = fname;

    SCM_DEFER_INTS;
    d->handle = sysdep_dynl_link (SCM_CHARS (fname), s_dynamic_link);
    SCM_NEWCELL (z);
    SCM_SETCHARS (z, d);
    SCM_SETCAR (z, scm_tc16_dynamic_obj);
    SCM_ALLOW_INTS;

    return z;
}

static struct dynl_obj *get_dynl_obj SCM_P ((SCM obj, char *subr, int argn));
static struct dynl_obj *
get_dynl_obj (dobj, subr, argn)
     SCM dobj;
     char *subr;
     int argn;
{
    struct dynl_obj *d;
    SCM_ASSERT (SCM_NIMP (dobj) && SCM_CAR (dobj) == scm_tc16_dynamic_obj,
		dobj, argn, subr);
    d = (struct dynl_obj *)SCM_CDR (dobj);
    SCM_ASSERT (d->handle != NULL, dobj, argn, subr);
    return d;
}

SCM_PROC (s_dynamic_object_p, "dynamic-object?", 1, 0, 0, scm_dynamic_object_p);

SCM
scm_dynamic_object_p (SCM obj)
{
    return (SCM_NIMP (obj) && SCM_CAR (obj) == scm_tc16_dynamic_obj)?
	SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC (s_dynamic_unlink, "dynamic-unlink", 1, 0, 0, scm_dynamic_unlink);

SCM
scm_dynamic_unlink (dobj)
     SCM dobj;
{
    struct dynl_obj *d = get_dynl_obj (dobj, s_dynamic_unlink, SCM_ARG1);
    sysdep_dynl_unlink (d->handle, s_dynamic_unlink);
    d->handle = NULL;
    return SCM_BOOL_T;
}

SCM_PROC (s_dynamic_func, "dynamic-func", 2, 0, 0, scm_dynamic_func);

SCM
scm_dynamic_func (SCM symb, SCM dobj)
{
    struct dynl_obj *d;
    void (*func) ();

    symb = scm_coerce_rostring (symb, s_dynamic_func, SCM_ARG1);
    d = get_dynl_obj (dobj, s_dynamic_func, SCM_ARG2);

    func = sysdep_dynl_func (SCM_CHARS (symb), d->handle, s_dynamic_func);
    return scm_ulong2num ((unsigned long)func);
}

SCM_PROC (s_dynamic_call, "dynamic-call", 2, 0, 0, scm_dynamic_call);

SCM
scm_dynamic_call (SCM func, SCM dobj)
{
    void (*fptr)();

    if (SCM_NIMP (func) && SCM_ROSTRINGP (func))
	func = scm_dynamic_func (func, dobj);
    fptr = (void (*)()) scm_num2ulong (func, (char *)SCM_ARG1, s_dynamic_call);
    fptr ();
    return SCM_BOOL_T;
}

SCM_PROC (s_dynamic_args_call, "dynamic-args-call", 3, 0, 0, scm_dynamic_args_call);

SCM
scm_dynamic_args_call (func, dobj, args)
     SCM func, dobj, args;
{
    int (*fptr) (int argc, char **argv);
    int result, argc;
    char **argv;

    if (SCM_NIMP (func) && SCM_ROSTRINGP (func))
	func = scm_dynamic_func (func, dobj);

    fptr = (int (*)(int, char **)) scm_num2ulong (func, (char *)SCM_ARG1,
						   s_dynamic_args_call);
    argv = scm_make_argv_from_stringlist (args, &argc, s_dynamic_args_call,
					  SCM_ARG3);

    result = (*fptr) (argc, argv);

    scm_must_free_argv (argv);
    return SCM_MAKINUM(0L+result);
}

void
scm_init_dynamic_linking ()
{
    scm_tc16_dynamic_obj = scm_newsmob (&dynl_obj_smob);
    sysdep_dynl_init ();
#include "dynl.x"
}

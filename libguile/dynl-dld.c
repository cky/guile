/* dynl-dld.c - dynamic linking with dld
 *
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
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

#include "_scm.h"
#include "genio.h"
#include "smob.h"

#include "dld.h"

static void listundef SCM_P ((void));

static void
listundefs ()
{
    int i;
    char **undefs = dld_list_undefined_sym();
    puts("   undefs:");
    for(i = dld_undefined_sym_count;i--;) {
	putc('"', stdout);
	fputs(undefs[i], stdout);
	puts("\"");
    }
    free(undefs);
}

SCM_PROC (s_dynamic_link, "dynamic-link", 1, 0, 0, scm_dynamic_link);

SCM
scm_dynamic_link (fname)
     SCM fname;
{
    int status;
    
    fname = scm_coerce_rostring (fname, s_dynamic_link, SCM_ARG1);

    SCM_DEFER_INTS;
    status = dld_link (SCM_CHARS (fname));
    SCM_ALLOW_INTS;
    if (status)
	scm_misc_error (s_dynamic_link, dld_strerror (status), SCM_EOL);
    return fname;
}

static void *get_func SCM_P ((char *subr, char *fname));

static void *
get_func (subr, fname)
     char *subr;
     char *fname;
{
    void *func;

    if (!dld_function_executable_p (func)) {
	listundefs ();
	scm_misc_error (subr, "unresolved symbols remain", SCM_EOL);
    }
    func = (void *) dld_get_func (func);
    if (func == 0)
	scm_misc_error (subr, dld_strerror (dld_errno), SCM_EOL);
    return func;
}

SCM_PROC (s_dynamic_call, "dynamic-call", 2, 0, 0, scm_dynamic_call);

SCM
scm_dynamic_call (symb, shl)
     SCM symb;
     SCM shl;
{
    void (*func)() = 0;

    symb = scm_coerce_rostring (symb, s_dynamic_call, SCM_ARG1);

    SCM_DEFER_INTS;
    func = get_func (s_dynamic_call, SCM_CHARS (symb));
    SCM_ALLOW_INST;
    (*func) ();
    return SCM_BOOL_T;
}

SCM_PROC (s_dynamic_args_call, "dynamic-args-call", 3, 0, 0, scm_dynamic_args_call);

SCM
scm_dynamic_args_call (symb, shl, args)
     SCM symb, shl, args;
{
    int i, argc;
    char **argv;
    int (*func) SCM_P ((int argc, char **argv)) = 0;

    symb = scm_coerce_rostring (symb, s_dynamic_args_call, SCM_ARG1);

    SCM_DEFER_INTS;
    func = get_func (SCM_CHARS (symb), s_dynamic_args_call);
    argv = scm_make_argv_from_stringlist (args, &argc, s_dynamic_args_call,
				      SCM_ARG3);
    SCM_ALLOW_INTS;

    i = (*func) (argc, argv);

    SCM_DEFER_INTS;
    scm_must_free_argv(argv);
    SCM_ALLOW_INTS;
    return SCM_MAKINUM(0L+i);
}

SCM_PROC (s_dynamic_unlink, "dynamic-unlink", 1, 0, 0, scm_dynamic_unlink);

SCM
scm_dynamic_unlink(fname)
     SCM fname;
{
    int status;

    fname = scm_coerce_rostring (fname, s_dynamic_unlink, SCM_ARG1);

    SCM_DEFER_INTS;
    status = dld_unlink_by_file (SCM_CHARS (fname), 1);
    SCM_ALLOW_INTS;

    if (status)
	scm_misc_error (s_dynamic_unlink, dld_strerror (status), SCM_EOL);
    return SCM_BOOL_T;
}

void
scm_init_dynamic_linking ()
{
#ifndef RTL
    if (!execpath)
	execpath = dld_find_executable (SCM_CHARS (SCM_CAR (progargs)));
    if (dld_init (SCM_CHARS (SCM_CAR (progargs)))) {
	dld_perror("DLD");
	return;
    }
#endif

#include "dynl.x"

#ifdef DLD_DYNCM /* XXX - what's this? */
    add_feature("dld:dyncm");
#endif
}

/* dynl-dl.c - dynamic linking for dlopen/dlsym
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

#include <dlfcn.h>

#define SHL(obj) ((void*)SCM_CDR(obj))

#ifdef RTLD_LAZY	/* Solaris 2. */
#  define DLOPEN_MODE	RTLD_LAZY
#else
#  define DLOPEN_MODE	1	/* Thats what it says in the man page. */
#endif

static scm_sizet frshl SCM_P ((SCM ptr));

static scm_sizet
frshl (ptr)
     SCM ptr;
{
#if 0
    /* Should freeing a shl close and possibly unmap the object file it */
    /* refers to? */
    if (SHL(ptr))
	dlclose (SHL(ptr));
#endif
    return 0;
}

static int prinshl SCM_P ((SCM exp, SCM port, scm_print_state *pstate));

static int
prinshl (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
    scm_gen_puts (scm_regular_string, "#<dynamic-linked ", port);
    scm_intprint (SCM_CDR (exp), 16, port);
    scm_gen_putc ('>', port);
    return 1;
}

int scm_tc16_shl;
static scm_smobfuns shlsmob = { scm_mark0, frshl, prinshl };

SCM_PROC (s_dynamic_link, "dynamic-link", 1, 0, 0, scm_dynamic_link);

SCM
scm_dynamic_link (fname)
     SCM fname;
{
    SCM z;
    void *handle;

    /* if FALSEP(fname) return fname; XXX - ? */

    fname = scm_coerce_rostring (fname, s_dynamic_link, SCM_ARG1);

    SCM_DEFER_INTS;
    handle = dlopen (SCM_CHARS (fname), DLOPEN_MODE);
    if (NULL == handle)
	scm_misc_error (s_dynamic_link, (char *)dlerror (), SCM_EOL);
    SCM_NEWCELL (z);
    SCM_SETCHARS (z, handle);
    SCM_SETCAR (z, scm_tc16_shl);
    SCM_ALLOW_INTS;

    return z;
}

static void *get_func SCM_P ((void *handle, char *func, char *subr));

static void *
get_func (handle, func, subr)
     void *handle;
     char *func;
     char *subr;
{
    void *fptr;
    char *err;

    fptr = dlsym (handle, func);
    err = (char *)dlerror ();
    if (!fptr)
	scm_misc_error (subr, err? err : "symbol has NULL address", SCM_EOL);
    return fptr;
}

SCM_PROC (s_dynamic_call, "dynamic-call", 2, 0, 0, scm_dynamic_call);

SCM
scm_dynamic_call (symb, shl)
     SCM symb, shl;
{
    void (*func) SCM_P ((void)) = 0;

    symb = scm_coerce_rostring (symb, s_dynamic_call, SCM_ARG1);
    SCM_ASSERT (SCM_NIMP (shl) && SCM_CAR (shl) == scm_tc16_shl, shl,
		SCM_ARG2, s_dynamic_call);

    SCM_DEFER_INTS;
    func = get_func (SHL(shl), SCM_CHARS (symb), s_dynamic_call);
    SCM_ALLOW_INTS;

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
    SCM_ASSERT (SCM_NIMP (shl) && SCM_CAR (shl) == scm_tc16_shl, shl,
		SCM_ARG2, s_dynamic_args_call);

    SCM_DEFER_INTS;
    func = get_func (SHL(shl), SCM_CHARS (symb), s_dynamic_args_call);
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
scm_dynamic_unlink (shl)
     SCM shl;
{
    int status;

    SCM_ASSERT (SCM_NIMP (shl) && SCM_CAR (shl) == scm_tc16_shl, shl,
		SCM_ARG1, s_dynamic_unlink);

    SCM_DEFER_INTS;
    status = dlclose (SHL(shl));
    SCM_SETCHARS (shl, NULL);
    SCM_ALLOW_INTS;

    if (status)
	scm_misc_error (s_dynamic_unlink, (char *)dlerror (), SCM_EOL);
    return SCM_BOOL_T;
}

void
scm_init_dynamic_linking ()
{
    scm_tc16_shl = scm_newsmob (&shlsmob);
#include "dynl.x"
}

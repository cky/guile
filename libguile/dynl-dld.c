/* dynl-dld.c - dynamic linking with dld
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

static void *
sysdep_dynl_link (fname, subr)
     char *fname;
     char *subr;
{
    int status;

    status = dld_link (fname);
    if (status)
	scm_misc_error (subr, dld_strerror (status), SCM_EOL);
    return fname;
}

static void
sysdep_dynl_unlink (handle, subr)
     void *handle;
     char *subr;
{
    int status;

    SCM_DEFER_INTS;
    status = dld_unlink_by_file ((char *)fname, 1);
    SCM_ALLOW_INTS;
    if (status)
	scm_misc_error (s_dynamic_unlink, dld_strerror (status), SCM_EOL);
}

static void *
sysdep_dynl_func (symb, handle, subr)
     char *symb;
     void *handle;
     char *subr;
{
    void *func;

    SCM_DEFER_INTS;
    func = (void *) dld_get_func (func);
    if (func == 0)
	scm_misc_error (subr, dld_strerror (dld_errno), SCM_EOL);
    if (!dld_function_executable_p (func)) {
	listundefs ();
	scm_misc_error (subr, "unresolved symbols remain", SCM_EOL);
    }
    SCM_ALLOW_INTS;
    return func;
}

static void
sysdep_dynl_init ()
{
#ifndef RTL
    if (!execpath)
	execpath = dld_find_executable (SCM_CHARS (SCM_CAR (progargs)));
    if (dld_init (SCM_CHARS (SCM_CAR (progargs)))) {
	dld_perror("DLD");
	return;
    }
#endif

#ifdef DLD_DYNCM /* XXX - what's this? */
    add_feature("dld:dyncm");
#endif
}

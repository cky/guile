/* dynl.c - dynamic linking
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

/* Dispatch to the system dependent files
 */

#ifdef DYNAMIC_LINKING
#ifdef HAVE_LIBDL
#include "dynl-dl.c"
#else
#ifdef HAVE_SHL_LOAD
#include "dynl-shl.c"
#else
#ifdef HAVE_DLD
#include "dynl-dld.c"
#else /* no dynamic linking available */
void
scm_init_dynamic_linking ()
{
}
#endif
#endif
#endif
#else /* dynamic linking disabled */
void
scm_init_dynamic_linking ()
{
}
#endif

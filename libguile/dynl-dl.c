/* dynl-dl.c - dynamic linking for dlopen/dlsym
 *
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
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

#include <dlfcn.h>

#ifdef RTLD_LAZY	/* Solaris 2. */
#  define DLOPEN_MODE	RTLD_LAZY
#else
#  define DLOPEN_MODE	1	/* Thats what it says in the man page. */
#endif

#ifndef RTLD_GLOBAL		/* Some systems have no such flag. */
# define RTLD_GLOBAL 0
#endif

static void *
sysdep_dynl_link (fname, flags, subr)
     const char *fname;
     int flags;
     const char *subr;
{
    void *handle = dlopen (fname, (DLOPEN_MODE 
				   | ((flags & DYNL_GLOBAL)? RTLD_GLOBAL : 0)));
    if (NULL == handle)
      {
	SCM_ALLOW_INTS;
	scm_misc_error (subr, (char *)dlerror (), SCM_EOL);
      }
    return handle;
}

static void
sysdep_dynl_unlink (handle, subr)
     void *handle;
     const char *subr;
{
    if (dlclose (handle))
      {
	SCM_ALLOW_INTS;
	scm_misc_error (subr, (char *)dlerror (), SCM_EOL);
      }
}
   
static void *
sysdep_dynl_func (symb, handle, subr)
     const char *symb;
     void *handle;
     const char *subr;
{
    void *fptr;
    char *err;
#if defined(USCORE) && !defined(DLSYM_ADDS_USCORE)
    char *usymb;
#endif

#if defined(USCORE) && !defined(DLSYM_ADDS_USCORE)
    usymb = (char *) malloc (strlen (symb) + 2);
    *usymb = '_';
    strcpy (usymb + 1, symb);
    fptr = dlsym (handle, usymb);
    free (usymb);
#else
    fptr = dlsym (handle, symb);
#endif

    err = (char *)dlerror ();
    if (!fptr)
      {
	SCM_ALLOW_INTS;
	scm_misc_error (subr, err? err : "symbol has NULL address", SCM_EOL);
      }
    return fptr;
}

static void
sysdep_dynl_init ()
{
}

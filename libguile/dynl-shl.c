/* dynl-shl.c - dynamic linking with shl_load (HP-UX)
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

#include "dl.h"
#include <stdio.h>
#include <string.h>

static void *
sysdep_dynl_link (fname, subr)
     const char *fname;
     const char *subr;
{
    shl_t shl;
    
    /* Probably too much BIND_* flags */
    shl = shl_load (fname, BIND_IMMEDIATE || BIND_FIRST ||
                    BIND_TOGETHER ||
                    BIND_VERBOSE || DYNAMIC_PATH, 0L);
    if (NULL==shl)
      {
	SCM_ALLOW_INTS;
	scm_misc_error (subr, "dynamic linking failed", SCM_EOL);
      }
    return shl;
}

static void
sysdep_dynl_unlink (handle, subr)
     void *handle;
     const char *subr;
{
  if (shl_unload ((shl_t) handle))
    {
      SCM_ALLOW_INTS;
      scm_misc_error (subr, "dynamic unlinking failed", SCM_EOL);
    }
}

static void *
sysdep_dynl_func (symb, handle, subr)
     const char *symb;
     void *handle;
     const char *subr;
{
    int status, i;
    struct shl_symbol *sym;

    status = shl_getsymbols((shl_t) handle, TYPE_PROCEDURE,
             EXPORT_SYMBOLS, malloc, &sym);

    for (i=0; i<status; ++i) {
      if (strcmp(symb, sym[i].name) == 0) return sym[i].value;
    }

    SCM_ALLOW_INTS;
    scm_misc_error (subr, "undefined function",
		    scm_cons (scm_makfrom0str (symb), SCM_EOL));
}

static void
sysdep_dynl_init ()
{
}

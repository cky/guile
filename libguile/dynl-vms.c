/* dynl-vms.c - dynamic linking for VMS, not yet ported
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
   (Not yet) modified for libguile by Marius Vollmer */

/* We should try to implement dynamic-link/dynamic-call for VMS,
   too. */

#include "_scm.h"

/* This permits dynamic linking. For example, the procedure of 0 arguments
   from a file could be the initialization procedure.
   (vms:dynamic-link-call "MYDISK:[MYDIR].EXE" "foo" "INIT_FOO")
   The first argument specifies the directory where the file specified
   by the second argument resides.  The current directory would be
   "SYS$DISK:[].EXE".
   The second argument cannot contain any punctuation.
   The third argument probably needs to be uppercased to mimic the VMS linker.
   */

# include <descrip.h>
# include <ssdef.h>
# include <rmsdef.h>

struct dsc$descriptor *descriptorize(x, buff)
     struct dsc$descriptor *x;
     SCM buff;
{(*x).dsc$w_length = LENGTH(buff);
 (*x).dsc$a_pointer = CHARS(buff);
 (*x).dsc$b_class = DSC$K_CLASS_S;
 (*x).dsc$b_dtype = DSC$K_DTYPE_T;
 return(x);}

static char s_dynl[] = "vms:dynamic-link-call";
SCM dynl(dir, symbol, fname)
     SCM dir, symbol, fname;
{
  struct dsc$descriptor fnamed, symbold, dird;
  void (*fcn)();
  long retval;
  ASSERT(IMP(dir) || STRINGP(dir), dir, ARG1, s_dynl);
  ASSERT(NIMP(fname) && STRINGP(fname), fname, ARG2, s_dynl);
  ASSERT(NIMP(symbol) && STRINGP(symbol), symbol, ARG3, s_dynl);
  descriptorize(&fnamed, fname);
  descriptorize(&symbold, symbol);
  DEFER_INTS;
  retval = lib$find_image_symbol(&fnamed, &symbold, &fcn,
				 IMP(dir) ? 0 : descriptorize(&dird, dir));
  if (SS$_NORMAL != retval) {
    /* wta(MAKINUM(retval), "vms error", s_dynl); */
    ALLOW_INTS;
    return BOOL_F;
  }
  ALLOW_INTS;
/*  *loc_loadpath = dir; */
  (*fcn)();
/*  *loc_loadpath = oloadpath; */
  return BOOL_T;
}

void init_dynl()
{
  make_subr(s_dynl, tc7_subr_3, dynl);
}

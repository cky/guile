/* classes: src_files 
 *	Copyright (C) 1995, 1997, 1998 Free Software Foundation, Inc.
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
 * Boston, MA 02111-1307 USA */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */




#include <stdio.h>
#include "_scm.h"
#include "genio.h"
#include "smob.h"

#include "mallocs.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif






static scm_sizet
fmalloc(SCM ptr)
{
  if (SCM_MALLOCDATA (ptr))
    free (SCM_MALLOCDATA (ptr));
  return 0;
}


static int
prinmalloc (SCM exp,SCM port,scm_print_state *pstate)
{
  scm_puts("#<malloc ", port);
  scm_intprint((int) SCM_CDR(exp), 16, port);
  scm_putc('>', port);
  return 1;
}


int scm_tc16_malloc;



SCM
scm_malloc_obj (scm_sizet n)
{
  SCM mem;

  mem = (n
	 ? (SCM)malloc (n)
	 : 0);
  if (n && !mem)
    {
      SCM_ALLOW_INTS;
      return SCM_BOOL_F;
    }
  SCM_RETURN_NEWSMOB (scm_tc16_malloc, mem);
}




void 
scm_init_mallocs ()
{
  scm_tc16_malloc = scm_make_smob_type_mfpe ("malloc", 0,
                                            NULL, fmalloc, prinmalloc, NULL);
}

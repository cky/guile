/* classes: src_files */

/*	Copyright (C) 1995 Free Software Foundation, Inc.
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
 */



#include <stdio.h>
#include "_scm.h"
#include "mallocs.h"
#ifdef HAVE_MALLOC_H
#include "malloc.h"
#endif
#ifdef HAVE_UNISTD_H
#include "unistd.h"
#endif





#ifdef __STDC__
static scm_sizet
fmalloc(SCM ptr)
#else
static scm_sizet
fmalloc(ptr)
     SCM ptr;
#endif
{
  if (SCM_MALLOCDATA (ptr))
    free (SCM_MALLOCDATA (ptr));
  return 0;
}

#ifdef __STDC__
static int
prinmalloc (SCM exp, SCM port, int writing)
#else
static int
prinmalloc (exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
#endif
{
  scm_gen_puts(scm_regular_string, "#<malloc ", port);
  scm_intprint(SCM_CDR(exp), 16, port);
  scm_gen_putc('>', port);
  return 1;
}


int scm_tc16_malloc;
static scm_smobfuns mallocsmob = {scm_mark0, fmalloc, prinmalloc, 0};



#ifdef __STDC__
SCM
scm_malloc_obj (scm_sizet n)
#else
SCM
scm_malloc_obj (n)
     scm_sizet n;
#endif
{
  SCM answer;
  SCM mem;

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  mem = (n
	 ? (SCM)malloc (n)
	 : 0);
  if (n && !mem)
    {
      SCM_ALLOW_INTS;
      return SCM_BOOL_F;
    }
  SCM_CDR (answer) = mem;
  SCM_CAR (answer) = scm_tc16_malloc;
  SCM_ALLOW_INTS;
  return answer;
}



#ifdef __STDC__
void 
scm_init_mallocs (void)
#else
void 
scm_init_mallocs ()
#endif
{
  scm_tc16_malloc = scm_newsmob (&mallocsmob);
}


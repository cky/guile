/* classes: src_files */

/*	Copyright (C) 1995, 1997 Free Software Foundation, Inc.
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






static scm_sizet fmalloc SCM_P ((SCM ptr));

static scm_sizet
fmalloc(ptr)
     SCM ptr;
{
  if (SCM_MALLOCDATA (ptr))
    free (SCM_MALLOCDATA (ptr));
  return 0;
}


static int prinmalloc SCM_P ((SCM exp, SCM port, scm_print_state *pstate));

static int
prinmalloc (exp, port, pstate)
     SCM exp;
     SCM port;
     scm_print_state *pstate;
{
  scm_puts("#<malloc ", port);
  scm_intprint(SCM_CDR(exp), 16, port);
  scm_putc('>', port);
  return 1;
}


int scm_tc16_malloc;
static scm_smobfuns mallocsmob = {0, fmalloc, prinmalloc, 0};




SCM
scm_malloc_obj (n)
     scm_sizet n;
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
  SCM_SETCDR (answer, mem);
  SCM_SETCAR (answer, scm_tc16_malloc);
  SCM_ALLOW_INTS;
  return answer;
}




void 
scm_init_mallocs ()
{
  scm_tc16_malloc = scm_newsmob (&mallocsmob);
}


/* classes: src_files 
 *	Copyright (C) 1995, 1997, 1998, 2000 Free Software Foundation, Inc.
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
#include "libguile/_scm.h"
#include "libguile/ports.h"
#include "libguile/smob.h"

#include "libguile/mallocs.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif



scm_bits_t scm_tc16_malloc;


static scm_sizet
malloc_free (SCM ptr)
{
  if (SCM_MALLOCDATA (ptr))
    free (SCM_MALLOCDATA (ptr));
  return 0;
}


static int
malloc_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts("#<malloc ", port);
  scm_intprint (SCM_CELL_WORD_1 (exp), 16, port);
  scm_putc('>', port);
  return 1;
}


SCM
scm_malloc_obj (scm_sizet n)
{
  scm_bits_t mem = n ? (scm_bits_t) malloc (n) : 0;
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
  scm_tc16_malloc = scm_make_smob_type ("malloc", 0);
  scm_set_smob_free (scm_tc16_malloc, malloc_free);
  scm_set_smob_print (scm_tc16_malloc, malloc_print);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* classes: src_files 
 * Copyright (C) 1995,1997,1998,2000,2001 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */




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


static size_t
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
scm_malloc_obj (size_t n)
{
  scm_bits_t mem = n ? (scm_bits_t) malloc (n) : 0;
  if (n && !mem)
    return SCM_BOOL_F;
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

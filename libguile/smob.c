/*	Copyright (C) 1995, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
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



#include <stdio.h>
#include "libguile/_scm.h"

#include "libguile/objects.h"
#include "libguile/ports.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "libguile/smob.h"



/* scm_smobs scm_numsmob
 * implement a dynamicly resized array of smob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */
int scm_numsmob;
scm_smob_descriptor *scm_smobs;

/* {Mark}
 */

/* This function is vestigial.  It used to be the mark function's
   responsibility to set the mark bit on the smob or port, but now the
   generic marking routine in gc.c takes care of that, and a zero
   pointer for a mark function means "don't bother".  So you never
   need scm_mark0.

   However, we leave it here because it's harmless to call it, and
   people out there have smob code that uses it, and there's no reason
   to make their links fail.  */

SCM 
scm_mark0 (SCM ptr)
{
  return SCM_BOOL_F;
}

SCM 
scm_markcdr (SCM ptr)
{
  return SCM_CDR (ptr);
}

/* {Free}
 */

scm_sizet 
scm_free0 (SCM ptr)
{
  return 0;
}

scm_sizet
scm_smob_free (SCM obj)
{
  scm_must_free ((char *) SCM_CELL_WORD_1 (obj));
  return scm_smobs[SCM_SMOBNUM (obj)].size;
}

/* {Print}
 */

int
scm_smob_print (SCM exp, SCM port, scm_print_state *pstate)
{
  int n = SCM_SMOBNUM (exp);
  scm_puts ("#<", port);
  scm_puts (SCM_SMOBNAME (n) ? SCM_SMOBNAME (n) : "smob", port);
  scm_putc (' ', port);
  scm_intprint (SCM_UNPACK (scm_smobs[n].size ? SCM_CDR (exp) : exp), 16, port);
  scm_putc ('>', port);
  return 1;
}

/* {Apply}
 */

#define SCM_SMOB_APPLY0(SMOB) \
  SCM_SMOB_DESCRIPTOR (SMOB).apply (SMOB)
#define SCM_SMOB_APPLY1(SMOB,A1) \
  SCM_SMOB_DESCRIPTOR (SMOB).apply (SMOB, A1)
#define SCM_SMOB_APPLY2(SMOB,A1,A2) \
  SCM_SMOB_DESCRIPTOR (SMOB).apply (SMOB, A1, A2)
#define SCM_SMOB_APPLY3(SMOB,A1,A2,A3) \
  SCM_SMOB_DESCRIPTOR (SMOB).apply (SMOB, A1, A2, A3)

static SCM
scm_smob_apply_0_010 (SCM smob)
{
  return SCM_SMOB_APPLY1 (smob, SCM_UNDEFINED);
}

static SCM
scm_smob_apply_0_020 (SCM smob)
{
  return SCM_SMOB_APPLY2 (smob, SCM_UNDEFINED, SCM_UNDEFINED);
}

static SCM
scm_smob_apply_0_030 (SCM smob)
{
  return SCM_SMOB_APPLY3 (smob, SCM_UNDEFINED, SCM_UNDEFINED, SCM_UNDEFINED);
}

static SCM
scm_smob_apply_0_001 (SCM smob)
{
  return SCM_SMOB_APPLY1 (smob, SCM_EOL);
}

static SCM
scm_smob_apply_0_011 (SCM smob)
{
  return SCM_SMOB_APPLY2 (smob, SCM_UNDEFINED, SCM_EOL);
}

static SCM
scm_smob_apply_0_021 (SCM smob)
{
  return SCM_SMOB_APPLY3 (smob, SCM_UNDEFINED, SCM_UNDEFINED, SCM_EOL);
}

static SCM
scm_smob_apply_0_error (SCM smob)
{
  scm_wrong_num_args (smob);
}

static SCM
scm_smob_apply_1_020 (SCM smob, SCM a1)
{
  return SCM_SMOB_APPLY2 (smob, a1, SCM_UNDEFINED);
}

static SCM
scm_smob_apply_1_030 (SCM smob, SCM a1)
{
  return SCM_SMOB_APPLY3 (smob, a1, SCM_UNDEFINED, SCM_UNDEFINED);
}

static SCM
scm_smob_apply_1_001 (SCM smob, SCM a1)
{
  return SCM_SMOB_APPLY1 (smob, SCM_LIST1 (a1));
}

static SCM
scm_smob_apply_1_011 (SCM smob, SCM a1)
{
  return SCM_SMOB_APPLY2 (smob, a1, SCM_EOL);
}

static SCM
scm_smob_apply_1_021 (SCM smob, SCM a1)
{
  return SCM_SMOB_APPLY3 (smob, a1, SCM_UNDEFINED, SCM_EOL);
}

static SCM
scm_smob_apply_1_error (SCM smob, SCM a1)
{
  scm_wrong_num_args (smob);
}

static SCM
scm_smob_apply_2_030 (SCM smob, SCM a1, SCM a2)
{
  return SCM_SMOB_APPLY3 (smob, a1, a2, SCM_UNDEFINED);
}

static SCM
scm_smob_apply_2_001 (SCM smob, SCM a1, SCM a2)
{
  return SCM_SMOB_APPLY1 (smob, SCM_LIST2 (a1, a2));
}

static SCM
scm_smob_apply_2_011 (SCM smob, SCM a1, SCM a2)
{
  return SCM_SMOB_APPLY2 (smob, a1, SCM_LIST1 (a2));
}

static SCM
scm_smob_apply_2_021 (SCM smob, SCM a1, SCM a2)
{
  return SCM_SMOB_APPLY3 (smob, a1, a2, SCM_EOL);
}

static SCM
scm_smob_apply_2_error (SCM smob, SCM a1, SCM a2)
{
  scm_wrong_num_args (smob);
}

static SCM
scm_smob_apply_3_030 (SCM smob, SCM a1, SCM a2, SCM rst)
{
  if (!SCM_NULLP (SCM_CDR (rst)))
    scm_wrong_num_args (smob);
  return SCM_SMOB_APPLY3 (smob, a1, a2, SCM_CAR (rst));
}

static SCM
scm_smob_apply_3_001 (SCM smob, SCM a1, SCM a2, SCM rst)
{
  return SCM_SMOB_APPLY1 (smob, scm_cons2 (a1, a2, rst));
}

static SCM
scm_smob_apply_3_011 (SCM smob, SCM a1, SCM a2, SCM rst)
{
  return SCM_SMOB_APPLY2 (smob, a1, scm_cons (a2, rst));
}

static SCM
scm_smob_apply_3_021 (SCM smob, SCM a1, SCM a2, SCM rst)
{
  return SCM_SMOB_APPLY3 (smob, a1, a2, rst);
}

static SCM
scm_smob_apply_3_error (SCM smob, SCM a1, SCM a2, SCM rst)
{
  scm_wrong_num_args (smob);
}


long 
scm_make_smob_type (char *name, scm_sizet size)
{
  char *tmp;
  if (255 <= scm_numsmob)
    goto smoberr;
  SCM_DEFER_INTS;
  SCM_SYSCALL (tmp = (char *) realloc ((char *) scm_smobs,
				       (1 + scm_numsmob)
				       * sizeof (scm_smob_descriptor)));
  if (tmp)
    {
      scm_smobs = (scm_smob_descriptor *) tmp;
      scm_smobs[scm_numsmob].name = name;
      scm_smobs[scm_numsmob].size = size;
      scm_smobs[scm_numsmob].mark = 0;
      scm_smobs[scm_numsmob].free = (size == 0 ? scm_free0 : scm_smob_free);
      scm_smobs[scm_numsmob].print = scm_smob_print;
      scm_smobs[scm_numsmob].equalp = 0;
      scm_smobs[scm_numsmob].apply = 0;
      scm_smobs[scm_numsmob].apply_0 = 0;
      scm_smobs[scm_numsmob].apply_1 = 0;
      scm_smobs[scm_numsmob].apply_2 = 0;
      scm_smobs[scm_numsmob].apply_3 = 0;
      scm_smobs[scm_numsmob].gsubr_type = 0;
      scm_numsmob++;
    }
  SCM_ALLOW_INTS;
  if (!tmp) 
    {
    smoberr:
      scm_memory_error ("scm_make_smob_type");
    }
  /* Make a class object if Goops is present. */
  if (scm_smob_class)
    scm_smob_class[scm_numsmob - 1]
      = scm_make_extended_class (SCM_SMOBNAME (scm_numsmob - 1));
  return scm_tc7_smob + (scm_numsmob - 1) * 256;
}

void
scm_set_smob_mark (long tc, SCM (*mark) (SCM))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].mark = mark;
}

void
scm_set_smob_free (long tc, scm_sizet (*free) (SCM))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].free = free;
}

void
scm_set_smob_print (long tc, int (*print) (SCM, SCM, scm_print_state*))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].print = print;
}

void
scm_set_smob_equalp (long tc, SCM (*equalp) (SCM, SCM))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].equalp = equalp;
}

void
scm_set_smob_apply (long tc, SCM (*apply) (),
		    unsigned int req, unsigned int opt, unsigned int rst)
{
  SCM (*apply_0) (SCM);
  SCM (*apply_1) (SCM, SCM);
  SCM (*apply_2) (SCM, SCM, SCM);
  SCM (*apply_3) (SCM, SCM, SCM, SCM);
  int type = SCM_GSUBR_MAKTYPE (req, opt, rst);

  if (rst > 1 || req + opt + rst > 3)
    {
      puts ("Unsupported smob application type");
      abort ();
    }

  switch (type)
    {
    case SCM_GSUBR_MAKTYPE (0, 0, 0):
      apply_0 = apply; break;
    case SCM_GSUBR_MAKTYPE (0, 1, 0):
      apply_0 = scm_smob_apply_0_010; break;
    case SCM_GSUBR_MAKTYPE (0, 2, 0):
      apply_0 = scm_smob_apply_0_020; break;
    case SCM_GSUBR_MAKTYPE (0, 3, 0):
      apply_0 = scm_smob_apply_0_030; break;
    case SCM_GSUBR_MAKTYPE (0, 0, 1):
      apply_0 = scm_smob_apply_0_001; break;
    case SCM_GSUBR_MAKTYPE (0, 1, 1):
      apply_0 = scm_smob_apply_0_011; break;
    case SCM_GSUBR_MAKTYPE (0, 2, 1):
      apply_0 = scm_smob_apply_0_021; break;
    default:
      apply_0 = scm_smob_apply_0_error; break;
    }

  switch (type)
    {
    case SCM_GSUBR_MAKTYPE (1, 0, 0):
    case SCM_GSUBR_MAKTYPE (0, 1, 0):
      apply_1 = apply; break;
    case SCM_GSUBR_MAKTYPE (1, 1, 0):
    case SCM_GSUBR_MAKTYPE (0, 2, 0):
      apply_1 = scm_smob_apply_1_020; break;
    case SCM_GSUBR_MAKTYPE (1, 2, 0):
    case SCM_GSUBR_MAKTYPE (0, 3, 0):
      apply_1 = scm_smob_apply_1_030; break;
    case SCM_GSUBR_MAKTYPE (0, 0, 1):
      apply_1 = scm_smob_apply_1_001; break;
    case SCM_GSUBR_MAKTYPE (1, 0, 1):
    case SCM_GSUBR_MAKTYPE (0, 1, 1):
      apply_1 = scm_smob_apply_1_011; break;
    case SCM_GSUBR_MAKTYPE (1, 1, 1):
    case SCM_GSUBR_MAKTYPE (0, 2, 1):
      apply_1 = scm_smob_apply_1_021; break;
    default:
      apply_1 = scm_smob_apply_1_error; break;
    }

  switch (type)
    {
    case SCM_GSUBR_MAKTYPE (2, 0, 0):
    case SCM_GSUBR_MAKTYPE (1, 1, 0):
    case SCM_GSUBR_MAKTYPE (0, 2, 0):
      apply_2 = apply; break;
    case SCM_GSUBR_MAKTYPE (2, 1, 0):
    case SCM_GSUBR_MAKTYPE (1, 2, 0):
    case SCM_GSUBR_MAKTYPE (0, 3, 0):
      apply_2 = scm_smob_apply_2_030; break;
    case SCM_GSUBR_MAKTYPE (0, 0, 1):
      apply_2 = scm_smob_apply_2_001; break;
    case SCM_GSUBR_MAKTYPE (1, 0, 1):
    case SCM_GSUBR_MAKTYPE (0, 1, 1):
      apply_2 = scm_smob_apply_2_011; break;
    case SCM_GSUBR_MAKTYPE (2, 0, 1):
    case SCM_GSUBR_MAKTYPE (1, 1, 1):
    case SCM_GSUBR_MAKTYPE (0, 2, 1):
      apply_2 = scm_smob_apply_2_021; break;
    default:
      apply_2 = scm_smob_apply_2_error; break;
    }

  switch (type)
    {
    case SCM_GSUBR_MAKTYPE (3, 0, 0):
    case SCM_GSUBR_MAKTYPE (2, 1, 0):
    case SCM_GSUBR_MAKTYPE (1, 2, 0):
    case SCM_GSUBR_MAKTYPE (0, 3, 0):
      apply_3 = scm_smob_apply_3_030; break;
    case SCM_GSUBR_MAKTYPE (0, 0, 1):
      apply_3 = scm_smob_apply_3_001; break;
    case SCM_GSUBR_MAKTYPE (1, 0, 1):
    case SCM_GSUBR_MAKTYPE (0, 1, 1):
      apply_3 = scm_smob_apply_3_011; break;
    case SCM_GSUBR_MAKTYPE (2, 0, 1):
    case SCM_GSUBR_MAKTYPE (1, 1, 1):
    case SCM_GSUBR_MAKTYPE (0, 2, 1):
      apply_3 = scm_smob_apply_3_021; break;
    default:
      apply_3 = scm_smob_apply_3_error; break;
    }

  scm_smobs[SCM_TC2SMOBNUM (tc)].apply = apply;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_0 = apply_0;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_1 = apply_1;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_2 = apply_2;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_3 = apply_3;
  scm_smobs[SCM_TC2SMOBNUM (tc)].gsubr_type = type;
}

SCM
scm_make_smob (long tc)
{
  int n = SCM_TC2SMOBNUM (tc);
  scm_sizet size = scm_smobs[n].size;
  SCM z;
  SCM_NEWCELL (z);
  if (size != 0)
    {
#if 0
      SCM_ASSERT (scm_smobs[n].mark == 0,
		  0,
		  "forbidden operation for smobs with GC data, use SCM_NEWSMOB",
		  SCM_SMOBNAME (n));
#endif
      SCM_SET_SMOB_DATA (z, scm_must_malloc (size, SCM_SMOBNAME (n)));
    }
  SCM_SET_CELL_TYPE (z, tc);
  return z;
}


/* {Deprecated stuff}
 */

#if (SCM_DEBUG_DEPRECATED == 0)

long
scm_make_smob_type_mfpe (char *name, scm_sizet size,
                        SCM (*mark) (SCM),
                        scm_sizet (*free) (SCM),
                        int (*print) (SCM, SCM, scm_print_state *),
                        SCM (*equalp) (SCM, SCM))
{
  long answer = scm_make_smob_type (name, size);
  scm_set_smob_mfpe (answer, mark, free, print, equalp);
  return answer;
}

void
scm_set_smob_mfpe (long tc, 
		   SCM (*mark) (SCM),
		   scm_sizet (*free) (SCM),
		   int (*print) (SCM, SCM, scm_print_state *),
		   SCM (*equalp) (SCM, SCM))
{
  if (mark) scm_set_smob_mark (tc, mark);
  if (free) scm_set_smob_free (tc, free);
  if (print) scm_set_smob_print (tc, print);
  if (equalp) scm_set_smob_equalp (tc, equalp);
}

#endif  /* SCM_DEBUG_DEPRECATED == 0 */


/* {Initialization for i/o types, float, bignum, the type of free cells}
 */

static int
free_print (SCM exp, SCM port, scm_print_state *pstate)
{
  char buf[100];

  sprintf (buf, "#<freed cell %p; GC missed a reference>",
	   (void *) SCM_UNPACK (exp));
  scm_puts (buf, port);

  return 1;
}

void
scm_smob_prehistory ()
{
  scm_bits_t tc;

  scm_numsmob = 0;
  scm_smobs = ((scm_smob_descriptor *)
	       malloc (7 * sizeof (scm_smob_descriptor)));

  /* WARNING: These scm_make_smob_type calls must be done in this order */
  tc = scm_make_smob_type ("free", 0);
  scm_set_smob_print (tc, free_print);

  tc = scm_make_smob_type ("big", 0);		/* freed in gc */
  scm_set_smob_print (tc, scm_bigprint);
  scm_set_smob_equalp (tc, scm_bigequal);

  tc = scm_make_smob_type ("real", 0);	/* freed in gc */
  scm_set_smob_print (tc, scm_print_real);
  scm_set_smob_equalp (tc, scm_real_equalp);

  tc = scm_make_smob_type ("complex", 0);	/* freed in gc */
  scm_set_smob_print (tc, scm_print_complex);
  scm_set_smob_equalp (tc, scm_complex_equalp);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2003, 2004, 2006, 2009 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>

#include "libguile/_scm.h"

#include "libguile/async.h"
#include "libguile/objects.h"
#include "libguile/goops.h"
#include "libguile/ports.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "libguile/smob.h"

#include "libguile/bdw-gc.h"
#include <gc/gc_mark.h>




/* scm_smobs scm_numsmob
 * implement a fixed sized array of smob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */

#define MAX_SMOB_COUNT SCM_I_MAX_SMOB_TYPE_COUNT

long scm_numsmob;
scm_smob_descriptor scm_smobs[MAX_SMOB_COUNT];

/* Lower 16 bit of data must be zero. 
*/
void
scm_i_set_smob_flags (SCM x, scm_t_bits data)
{
  SCM_SET_CELL_WORD_0 (x, (SCM_CELL_WORD_0 (x) & 0xFFFF) | data);
}

void
scm_assert_smob_type (scm_t_bits tag, SCM val)
{
  if (!SCM_SMOB_PREDICATE (tag, val))
    scm_wrong_type_arg_msg (NULL, 0, val, scm_smobs[SCM_TC2SMOBNUM(tag)].name);
}

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
scm_mark0 (SCM ptr SCM_UNUSED)
{
  return SCM_BOOL_F;
}

SCM 
/* Dirk::FIXME: The name markcdr is misleading, since the term cdr should only
   be used for real pairs. */
scm_markcdr (SCM ptr)
{
  return SCM_CELL_OBJECT_1 (ptr);
}

/* {Free}
 */

size_t 
scm_free0 (SCM ptr SCM_UNUSED)
{
  return 0;
}

size_t
scm_smob_free (SCM obj)
{
  long n = SCM_SMOBNUM (obj);
  if (scm_smobs[n].size > 0)
    scm_gc_free ((void *) SCM_CELL_WORD_1 (obj), 
		 scm_smobs[n].size, SCM_SMOBNAME (n));
  return 0;
}

/* {Print}
 */

int
scm_smob_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  long n = SCM_SMOBNUM (exp);
  scm_puts ("#<", port);
  scm_puts (SCM_SMOBNAME (n) ? SCM_SMOBNAME (n) : "smob", port);
  scm_putc (' ', port);
  if (scm_smobs[n].size)
    scm_uintprint (SCM_CELL_WORD_1 (exp), 16, port);
  else
    scm_uintprint (SCM_UNPACK (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}

/* {Apply}
 */

#define SCM_SMOB_APPLY0(SMOB) \
  SCM_SMOB_DESCRIPTOR (SMOB).apply (SMOB)
#define SCM_SMOB_APPLY1(SMOB, A1) \
  SCM_SMOB_DESCRIPTOR (SMOB).apply (SMOB, A1)
#define SCM_SMOB_APPLY2(SMOB, A1, A2) \
  SCM_SMOB_DESCRIPTOR (SMOB).apply (SMOB, A1, A2)
#define SCM_SMOB_APPLY3(SMOB, A1, A2, A3) \
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
  return SCM_SMOB_APPLY1 (smob, scm_list_1 (a1));
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
scm_smob_apply_1_error (SCM smob, SCM a1 SCM_UNUSED)
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
  return SCM_SMOB_APPLY1 (smob, scm_list_2 (a1, a2));
}

static SCM
scm_smob_apply_2_011 (SCM smob, SCM a1, SCM a2)
{
  return SCM_SMOB_APPLY2 (smob, a1, scm_list_1 (a2));
}

static SCM
scm_smob_apply_2_021 (SCM smob, SCM a1, SCM a2)
{
  return SCM_SMOB_APPLY3 (smob, a1, a2, SCM_EOL);
}

static SCM
scm_smob_apply_2_error (SCM smob, SCM a1 SCM_UNUSED, SCM a2 SCM_UNUSED)
{
  scm_wrong_num_args (smob);
}

static SCM
scm_smob_apply_3_030 (SCM smob, SCM a1, SCM a2, SCM rst)
{
  if (!scm_is_null (SCM_CDR (rst)))
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
scm_smob_apply_3_error (SCM smob,
			SCM a1 SCM_UNUSED,
			SCM a2 SCM_UNUSED,
			SCM rst SCM_UNUSED)
{
  scm_wrong_num_args (smob);
}



scm_t_bits 
scm_make_smob_type (char const *name, size_t size)
#define FUNC_NAME "scm_make_smob_type"
{
  long new_smob;

  SCM_CRITICAL_SECTION_START;
  new_smob = scm_numsmob;
  if (scm_numsmob != MAX_SMOB_COUNT)
    ++scm_numsmob;
  SCM_CRITICAL_SECTION_END;

  if (new_smob == MAX_SMOB_COUNT)
    scm_misc_error (FUNC_NAME, "maximum number of smobs exceeded", SCM_EOL);

  scm_smobs[new_smob].name = name;
  if (size != 0)
    {
      scm_smobs[new_smob].size = size;
      scm_smobs[new_smob].free = scm_smob_free;
    }

  /* Make a class object if Goops is present. */
  if (SCM_UNPACK (scm_smob_class[0]) != 0)
    scm_smob_class[new_smob] = scm_make_extended_class (name, 0);

  return scm_tc7_smob + new_smob * 256;
}
#undef FUNC_NAME


void
scm_set_smob_mark (scm_t_bits tc, SCM (*mark) (SCM))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].mark = mark;
}

void
scm_set_smob_free (scm_t_bits tc, size_t (*free) (SCM))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].free = free;
}

void
scm_set_smob_print (scm_t_bits tc, int (*print) (SCM, SCM, scm_print_state*))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].print = print;
}

void
scm_set_smob_equalp (scm_t_bits tc, SCM (*equalp) (SCM, SCM))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].equalp = equalp;
}

void
scm_set_smob_apply (scm_t_bits tc, SCM (*apply) (),
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

  scm_smobs[SCM_TC2SMOBNUM (tc)].apply   = apply;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_0 = apply_0;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_1 = apply_1;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_2 = apply_2;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_3 = apply_3;
  scm_smobs[SCM_TC2SMOBNUM (tc)].gsubr_type = type;

  if (SCM_UNPACK (scm_smob_class[0]) != 0)
    scm_i_inherit_applicable (scm_smob_class[SCM_TC2SMOBNUM (tc)]);
}

SCM
scm_make_smob (scm_t_bits tc)
{
  scm_t_bits n = SCM_TC2SMOBNUM (tc);
  size_t size = scm_smobs[n].size;
  scm_t_bits data = (size > 0
		     ? (scm_t_bits) scm_gc_malloc (size, SCM_SMOBNAME (n))
		     : 0);

  SCM_RETURN_NEWSMOB (tc, data);
}



/* Marking SMOBs using user-supplied mark procedures.  */


/* The GC kind used for SMOB types that provide a custom mark procedure.  */
static int smob_gc_kind;


/* The generic SMOB mark procedure that gets called for SMOBs allocated with
   `scm_i_new_smob_with_mark_proc ()'.  */
static struct GC_ms_entry *
smob_mark (GC_word *addr, struct GC_ms_entry *mark_stack_ptr,
	   struct GC_ms_entry *mark_stack_limit, GC_word env)
{
  register SCM cell;
  register scm_t_bits tc, smobnum;

  cell = PTR2SCM (addr);

  if (SCM_TYP7 (cell) != scm_tc7_smob)
    /* It is likely that the GC passed us a pointer to a free-list element
       which we must ignore (see warning in `gc/gc_mark.h').  */
    return mark_stack_ptr;

  tc = SCM_CELL_WORD_0 (cell);
  smobnum = SCM_TC2SMOBNUM (tc);

  if (smobnum >= scm_numsmob)
    /* The first word looks corrupt.  */
    abort ();

  mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (SCM_CELL_OBJECT_1 (cell)),
				     mark_stack_ptr,
				     mark_stack_limit, NULL);
  mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (SCM_CELL_OBJECT_2 (cell)),
				     mark_stack_ptr,
				     mark_stack_limit, NULL);
  mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (SCM_CELL_OBJECT_3 (cell)),
				     mark_stack_ptr,
				     mark_stack_limit, NULL);

  if (scm_smobs[smobnum].mark)
    {
      SCM obj;

      SCM_I_CURRENT_THREAD->current_mark_stack_ptr = mark_stack_ptr;
      SCM_I_CURRENT_THREAD->current_mark_stack_limit = mark_stack_limit;

      /* Invoke the SMOB's mark procedure, which will in turn invoke
	 `scm_gc_mark ()', which may modify `current_mark_stack_ptr'.  */
      obj = scm_smobs[smobnum].mark (cell);

      mark_stack_ptr = SCM_I_CURRENT_THREAD->current_mark_stack_ptr;

      if (SCM_NIMP (obj))
	/* Mark the returned object.  */
	mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (obj),
					   mark_stack_ptr,
					   mark_stack_limit, NULL);

      SCM_I_CURRENT_THREAD->current_mark_stack_limit = NULL;
      SCM_I_CURRENT_THREAD->current_mark_stack_ptr = NULL;
    }

  return mark_stack_ptr;

}

/* Mark object O.  We assume that this function is only called during the
   mark phase, i.e., from within `smob_mark ()' or one of its
   descendents.  */
void
scm_gc_mark (SCM o)
{
#define CURRENT_MARK_PTR						 \
  ((struct GC_ms_entry *)(SCM_I_CURRENT_THREAD->current_mark_stack_ptr))
#define CURRENT_MARK_LIMIT						   \
  ((struct GC_ms_entry *)(SCM_I_CURRENT_THREAD->current_mark_stack_limit))

  if (SCM_NIMP (o))
    {
      /* At this point, the `current_mark_*' fields of the current thread
	 must be defined (they are set in `smob_mark ()').  */
      register struct GC_ms_entry *mark_stack_ptr;

      if (!CURRENT_MARK_PTR)
	/* The function was not called from a mark procedure.  */
	abort ();

      mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (o),
					 CURRENT_MARK_PTR, CURRENT_MARK_LIMIT,
					 NULL);
      SCM_I_CURRENT_THREAD->current_mark_stack_ptr = mark_stack_ptr;
    }
#undef CURRENT_MARK_PTR
#undef CURRENT_MARK_LIMIT
}

/* Return a SMOB with typecode TC.  The SMOB type corresponding to TC may
   provide a custom mark procedure and it will be honored.  */
SCM
scm_i_new_smob_with_mark_proc (scm_t_bits tc, scm_t_bits data1,
			       scm_t_bits data2, scm_t_bits data3)
{
  /* Return a double cell.  */
  SCM cell = SCM_PACK (GC_generic_malloc (2 * sizeof (scm_t_cell),
					  smob_gc_kind));

  SCM_SET_CELL_WORD_3 (cell, data3);
  SCM_SET_CELL_WORD_2 (cell, data2);
  SCM_SET_CELL_WORD_1 (cell, data1);
  SCM_SET_CELL_WORD_0 (cell, tc);

  return cell;
}


/* Finalize SMOB by calling its SMOB type's free function, if any.  */
void
scm_i_finalize_smob (GC_PTR ptr, GC_PTR data)
{
  SCM smob;
  size_t (* free_smob) (SCM);

  smob = PTR2SCM (ptr);
#if 0
  printf ("finalizing SMOB %p (smobnum: %u)\n",
	  ptr, SCM_SMOBNUM (smob));
#endif

  free_smob = scm_smobs[SCM_SMOBNUM (smob)].free;
  if (free_smob)
    free_smob (smob);
}


void
scm_smob_prehistory ()
{
  long i;

  smob_gc_kind = GC_new_kind (GC_new_free_list (),
			      GC_MAKE_PROC (GC_new_proc (smob_mark), 0),
			      0,
			      /* Clear new objects.  As of version 7.1, libgc
				 doesn't seem to support passing 0 here.  */
			      1);

  scm_numsmob = 0;
  for (i = 0; i < MAX_SMOB_COUNT; ++i)
    {
      scm_smobs[i].name       = 0;
      scm_smobs[i].size       = 0;
      scm_smobs[i].mark       = 0;
      scm_smobs[i].free       = 0;
      scm_smobs[i].print      = scm_smob_print;
      scm_smobs[i].equalp     = 0;
      scm_smobs[i].apply      = 0;
      scm_smobs[i].apply_0    = 0;
      scm_smobs[i].apply_1    = 0;
      scm_smobs[i].apply_2    = 0;
      scm_smobs[i].apply_3    = 0;
      scm_smobs[i].gsubr_type = 0;
    }
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

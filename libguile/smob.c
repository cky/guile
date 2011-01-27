/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2003, 2004, 2006, 2009, 2010, 2011 Free Software Foundation, Inc.
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
#include "libguile/goops.h"
#include "libguile/instructions.h"
#include "libguile/objcodes.h"
#include "libguile/programs.h"

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

#ifdef WORDS_BIGENDIAN
#define OBJCODE_HEADER 0, 0, 0, 16, 0, 0, 0, 40
#define META_HEADER    0, 0, 0, 32, 0, 0, 0, 0
#else
#define OBJCODE_HEADER 16, 0, 0, 0, 40, 0, 0, 0
#define META_HEADER    32, 0, 0, 0, 0, 0, 0, 0
#endif

/* This code is the same as in gsubr.c, except we use smob_call instead of
   struct_call. */

/* A: req; B: opt; C: rest */
#define A(nreq)                                                         \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_assert_nargs_ee, 0, nreq, /* assert number of args */  \
  /* 3 */ scm_op_object_ref, 0, /* push the foreign object wrapping the subr pointer */ \
  /* 5 */ scm_op_smob_call, nreq, /* and call (will return value as well) */ \
  /* 7 */ scm_op_nop,                                                   \
  /* 8 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,               \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (3, 7, nreq, 0, 0)

#define B(nopt)                                                         \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_bind_optionals, 0, nopt, /* bind optionals */          \
  /* 3 */ scm_op_assert_nargs_ee, 0, nopt, /* assert number of args */  \
  /* 6 */ scm_op_object_ref, 0, /* push the foreign object wrapping the smob pointer */ \
  /* 8 */ scm_op_smob_call, nopt, /* and call (will return value as well) */ \
  /* 10 */ scm_op_nop, scm_op_nop,                                      \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (6, 10, 0, nopt, 0)

#define C()                                                             \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_push_rest, 0, 0, /* cons all args into a list */       \
  /* 3 */ scm_op_object_ref, 0, /* push the foreign object wrapping the smob pointer */ \
  /* 5 */ scm_op_smob_call, 1, /* and call (will return value as well) */ \
  /* 7 */ scm_op_nop,                                                   \
  /* 8 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,               \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (3, 7, 0, 0, 1)

#define AB(nreq, nopt)                                                  \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_assert_nargs_ge, 0, nreq, /* assert number of args */  \
  /* 3 */ scm_op_bind_optionals, 0, nreq+nopt, /* bind optionals */     \
  /* 6 */ scm_op_assert_nargs_ee, 0, nreq+nopt, /* assert number of args */ \
  /* 9 */ scm_op_object_ref, 0, /* push the foreign object wrapping the smob pointer */ \
  /* 11 */ scm_op_smob_call, nreq+nopt, /* and call (will return value as well) */ \
  /* 13 */ scm_op_nop, scm_op_nop, scm_op_nop,                          \
  /* 16 */ META (9, 13, nreq, nopt, 0)

#define AC(nreq)                                                        \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_assert_nargs_ge, 0, nreq, /* assert number of args */  \
  /* 3 */ scm_op_push_rest, 0, nreq, /* cons rest list */               \
  /* 6 */ scm_op_object_ref, 0, /* push the foreign object wrapping the smob pointer */ \
  /* 8 */ scm_op_smob_call, nreq+1, /* and call (will return value as well) */ \
  /* 10 */ scm_op_nop, scm_op_nop,                                      \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (6, 10, nreq, 0, 1)

#define BC(nopt)                                                        \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_bind_optionals, 0, nopt, /* bind optionals */          \
  /* 3 */ scm_op_push_rest, 0, nopt, /* cons rest list */               \
  /* 6 */ scm_op_object_ref, 0, /* push the foreign object wrapping the smob pointer */ \
  /* 8 */ scm_op_smob_call, nopt+1, /* and call (will return value as well) */ \
  /* 10 */ scm_op_nop, scm_op_nop,                                      \
  /* 12 */ scm_op_nop, scm_op_nop, scm_op_nop, scm_op_nop,              \
  /* 16 */ META (6, 10, 0, nopt, 1)

#define ABC(nreq, nopt)                                                 \
  OBJCODE_HEADER,                                                       \
  /* 0 */ scm_op_assert_nargs_ge, 0, nreq, /* assert number of args */  \
  /* 3 */ scm_op_bind_optionals, 0, nreq+nopt, /* bind optionals */     \
  /* 6 */ scm_op_push_rest, 0, nreq+nopt, /* cons rest list */          \
  /* 9 */ scm_op_object_ref, 0, /* push the foreign object wrapping the smob pointer */ \
  /* 11 */ scm_op_smob_call, nreq+nopt+1, /* and call (will return value as well) */ \
  /* 13 */ scm_op_nop, scm_op_nop, scm_op_nop,                          \
  /* 16 */ META (9, 13, nreq, nopt, 1)
  
#define META(start, end, nreq, nopt, rest)                              \
  META_HEADER,                                                          \
  /* 0 */ scm_op_make_eol, /* bindings */                               \
  /* 1 */ scm_op_make_eol, /* sources */                                \
  /* 2 */ scm_op_make_int8, start, scm_op_make_int8, end, /* arity: from ip N to ip N */ \
  /* 6 */ scm_op_make_int8, nreq, /* the arity is N required args */    \
  /* 8 */ scm_op_make_int8, nopt, /* N optionals */                     \
  /* 10 */ rest ? scm_op_make_true : scm_op_make_false, /* maybe a rest arg */ \
  /* 11 */ scm_op_list, 0, 5, /* make a list of those 5 vals */         \
  /* 14 */ scm_op_list, 0, 1, /* and the arities will be a list of that one list */ \
  /* 17 */ scm_op_load_symbol, 0, 0, 4, 'n', 'a', 'm', 'e', /* `name' */ \
  /* 25 */ scm_op_object_ref, 1, /* the name from the object table */   \
  /* 27 */ scm_op_cons, /* make a pair for the properties */            \
  /* 28 */ scm_op_list, 0, 4, /* pack bindings, sources, and arities into list */ \
  /* 31 */ scm_op_return /* and return */                               \
  /* 32 */

static const struct
{
  scm_t_uint64 dummy; /* ensure 8-byte alignment; perhaps there's a better way */
  const scm_t_uint8 bytes[16 * (sizeof (struct scm_objcode) + 16
                                + sizeof (struct scm_objcode) + 32)];
} raw_bytecode = {
  0,
  {
    /* Use the elisp macros from gsubr.c */
    /* C-u 3 M-x generate-bytecodes RET */
    /* 0 arguments */
    A(0), 
    /* 1 arguments */
    A(1), B(1), C(), 
    /* 2 arguments */
    A(2), AB(1,1), B(2), AC(1), BC(1), 
    /* 3 arguments */
    A(3), AB(2,1), AB(1,2), B(3), AC(2), ABC(1,1), BC(2)
  }
};

#undef A
#undef B
#undef C
#undef AB
#undef AC
#undef BC
#undef ABC
#undef OBJCODE_HEADER
#undef META_HEADER
#undef META

#define STATIC_OBJCODE_TAG                                      \
  SCM_PACK (SCM_MAKE_OBJCODE_TAG (SCM_OBJCODE_TYPE_STATIC, 0))

static const struct
{
  scm_t_uint64 dummy; /* alignment */
  scm_t_cell cells[16 * 2]; /* 4*4 double cells */
} objcode_cells = {
  0,
  /* C-u 3 M-x generate-objcode-cells RET */
  {
    /* 0 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 0) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 1 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 64) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 128) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 192) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 2 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 256) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 320) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 384) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 448) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 512) },
    { SCM_BOOL_F, SCM_PACK (0) },

    /* 3 arguments */
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 576) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 640) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 704) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 768) },
    { SCM_BOOL_F, SCM_PACK (0) },

    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 832) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 896) },
    { SCM_BOOL_F, SCM_PACK (0) },
    { STATIC_OBJCODE_TAG, SCM_PACK (raw_bytecode.bytes + 960) },
    { SCM_BOOL_F, SCM_PACK (0) }
  }
};
  
static const SCM scm_smob_objcode_trampolines[16] = {
  /* C-u 3 M-x generate-objcodes RET */
  /* 0 arguments */
  SCM_PACK (objcode_cells.cells+0),

  /* 1 arguments */
  SCM_PACK (objcode_cells.cells+2),
  SCM_PACK (objcode_cells.cells+4),
  SCM_PACK (objcode_cells.cells+6),

  /* 2 arguments */
  SCM_PACK (objcode_cells.cells+8),
  SCM_PACK (objcode_cells.cells+10),
  SCM_PACK (objcode_cells.cells+12),
  SCM_PACK (objcode_cells.cells+14),
  SCM_PACK (objcode_cells.cells+16),

  /* 3 arguments */
  SCM_PACK (objcode_cells.cells+18),
  SCM_PACK (objcode_cells.cells+20),
  SCM_PACK (objcode_cells.cells+22),
  SCM_PACK (objcode_cells.cells+24),
  SCM_PACK (objcode_cells.cells+26),
  SCM_PACK (objcode_cells.cells+28),
  SCM_PACK (objcode_cells.cells+30)
};

/* (nargs * nargs) + nopt + rest * (nargs + 1) */
#define SCM_SMOB_OBJCODE_TRAMPOLINE(nreq,nopt,rest)                     \
  scm_smob_objcode_trampolines[(nreq + nopt + rest) * (nreq + nopt + rest) \
                               + nopt + rest * (nreq + nopt + rest + 1)]

static SCM
scm_smob_objcode_trampoline (unsigned int nreq, unsigned int nopt,
                             unsigned int rest)
{
  if (SCM_UNLIKELY (rest > 1 || nreq + nopt + rest > 3))
    scm_out_of_range ("make-smob", scm_from_uint (nreq + nopt + rest));
      
  return SCM_SMOB_OBJCODE_TRAMPOLINE (nreq, nopt, rest);
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
  scm_smobs[new_smob].size = size;

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
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply   = apply;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_trampoline_objcode
    = scm_smob_objcode_trampoline (req, opt, rst);

  if (SCM_UNPACK (scm_smob_class[0]) != 0)
    scm_i_inherit_applicable (scm_smob_class[SCM_TC2SMOBNUM (tc)]);
}

static SCM tramp_weak_map = SCM_BOOL_F;
SCM
scm_i_smob_apply_trampoline (SCM smob)
{
  /* could use hashq-create-handle!, but i don't know what to do if it returns a
     weak pair */
  SCM tramp = scm_hashq_ref (tramp_weak_map, smob, SCM_BOOL_F);

  if (scm_is_true (tramp))
    return tramp;
  else
    {
      const char *name;
      SCM objtable;

      name = SCM_SMOBNAME (SCM_SMOBNUM (smob));
      if (!name)
        name = "smob-apply";
      objtable = scm_c_make_vector (2, SCM_UNDEFINED);
      SCM_SIMPLE_VECTOR_SET (objtable, 0, smob);
      SCM_SIMPLE_VECTOR_SET (objtable, 1, scm_from_locale_symbol (name));
      tramp = scm_make_program (SCM_SMOB_DESCRIPTOR (smob).apply_trampoline_objcode,
                                objtable, SCM_BOOL_F);
      scm_hashq_set_x (tramp_weak_map, smob, tramp);
      return tramp;
    }
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
      scm_smobs[i].apply_trampoline_objcode = SCM_BOOL_F;
    }

  tramp_weak_map = scm_make_weak_key_hash_table (SCM_UNDEFINED);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

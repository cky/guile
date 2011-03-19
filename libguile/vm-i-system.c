/* Copyright (C) 2001,2008,2009,2010,2011 Free Software Foundation, Inc.
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


/* This file is included in vm_engine.c */


/*
 * Basic operations
 */

VM_DEFINE_INSTRUCTION (0, nop, "nop", 0, 0, 0)
{
  NEXT;
}

VM_DEFINE_INSTRUCTION (1, halt, "halt", 0, 0, 0)
{
  nvalues = SCM_I_INUM (*sp--);
  NULLSTACK (1);
  if (nvalues == 1)
    POP (finish_args);
  else
    {
      POP_LIST (nvalues);
      POP (finish_args);
      SYNC_REGISTER ();
      finish_args = scm_values (finish_args);
    }
    
  {
#ifdef VM_ENABLE_STACK_NULLING
    SCM *old_sp = sp;
#endif

    /* Restore registers */
    sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
    /* Setting the ip here doesn't actually affect control flow, as the calling
       code will restore its own registers, but it does help when walking the
       stack */
    ip = SCM_FRAME_RETURN_ADDRESS (fp);
    fp = SCM_FRAME_DYNAMIC_LINK (fp);
    NULLSTACK (old_sp - sp);
  }
  
  goto vm_done;
}

VM_DEFINE_INSTRUCTION (2, drop, "drop", 0, 1, 0)
{
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (3, dup, "dup", 0, 0, 1)
{
  SCM x = *sp;
  PUSH (x);
  NEXT;
}


/*
 * Object creation
 */

VM_DEFINE_INSTRUCTION (4, void, "void", 0, 0, 1)
{
  PUSH (SCM_UNSPECIFIED);
  NEXT;
}

VM_DEFINE_INSTRUCTION (5, make_true, "make-true", 0, 0, 1)
{
  PUSH (SCM_BOOL_T);
  NEXT;
}

VM_DEFINE_INSTRUCTION (6, make_false, "make-false", 0, 0, 1)
{
  PUSH (SCM_BOOL_F);
  NEXT;
}

VM_DEFINE_INSTRUCTION (7, make_nil, "make-nil", 0, 0, 1)
{
  PUSH (SCM_ELISP_NIL);
  NEXT;
}

VM_DEFINE_INSTRUCTION (8, make_eol, "make-eol", 0, 0, 1)
{
  PUSH (SCM_EOL);
  NEXT;
}

VM_DEFINE_INSTRUCTION (9, make_int8, "make-int8", 1, 0, 1)
{
  PUSH (SCM_I_MAKINUM ((signed char) FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (10, make_int8_0, "make-int8:0", 0, 0, 1)
{
  PUSH (SCM_INUM0);
  NEXT;
}

VM_DEFINE_INSTRUCTION (11, make_int8_1, "make-int8:1", 0, 0, 1)
{
  PUSH (SCM_I_MAKINUM (1));
  NEXT;
}

VM_DEFINE_INSTRUCTION (12, make_int16, "make-int16", 2, 0, 1)
{
  int h = FETCH ();
  int l = FETCH ();
  PUSH (SCM_I_MAKINUM ((signed short) (h << 8) + l));
  NEXT;
}

VM_DEFINE_INSTRUCTION (13, make_int64, "make-int64", 8, 0, 1)
{
  scm_t_uint64 v = 0;
  v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  PUSH (scm_from_int64 ((scm_t_int64) v));
  NEXT;
}

VM_DEFINE_INSTRUCTION (14, make_uint64, "make-uint64", 8, 0, 1)
{
  scm_t_uint64 v = 0;
  v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  PUSH (scm_from_uint64 (v));
  NEXT;
}

VM_DEFINE_INSTRUCTION (15, make_char8, "make-char8", 1, 0, 1)
{
  scm_t_uint8 v = 0;
  v = FETCH ();

  PUSH (SCM_MAKE_CHAR (v));
  /* Don't simplify this to PUSH (SCM_MAKE_CHAR (FETCH ())).  The
     contents of SCM_MAKE_CHAR may be evaluated more than once,
     resulting in a double fetch.  */
  NEXT;
}

VM_DEFINE_INSTRUCTION (16, make_char32, "make-char32", 4, 0, 1)
{
  scm_t_wchar v = 0;
  v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  v <<= 8; v += FETCH ();
  PUSH (SCM_MAKE_CHAR (v));
  NEXT;
}



VM_DEFINE_INSTRUCTION (17, list, "list", 2, -1, 1)
{
  unsigned h = FETCH ();
  unsigned l = FETCH ();
  unsigned len = ((h << 8) + l);
  POP_LIST (len);
  NEXT;
}

VM_DEFINE_INSTRUCTION (18, vector, "vector", 2, -1, 1)
{
  unsigned h = FETCH ();
  unsigned l = FETCH ();
  unsigned len = ((h << 8) + l);
  SCM vect;
  
  SYNC_REGISTER ();
  sp++; sp -= len;
  CHECK_UNDERFLOW ();
  vect = scm_make_vector (scm_from_uint (len), SCM_BOOL_F);
  memcpy (SCM_I_VECTOR_WELTS(vect), sp, sizeof(SCM) * len);
  NULLSTACK (len);
  *sp = vect;

  NEXT;
}


/*
 * Variable access
 */

#define OBJECT_REF(i)		objects[i]
#define OBJECT_SET(i,o)		objects[i] = o

#define LOCAL_REF(i)		SCM_FRAME_VARIABLE (fp, i)
#define LOCAL_SET(i,o)		SCM_FRAME_VARIABLE (fp, i) = o

/* For the variable operations, we _must_ obviously avoid function calls to
   `scm_variable_ref ()', `scm_variable_bound_p ()' and friends which do
   nothing more than the corresponding macros.  */
#define VARIABLE_REF(v)		SCM_VARIABLE_REF (v)
#define VARIABLE_SET(v,o)	SCM_VARIABLE_SET (v, o)
#define VARIABLE_BOUNDP(v)      (VARIABLE_REF (v) != SCM_UNDEFINED)

#define FREE_VARIABLE_REF(i)	SCM_PROGRAM_FREE_VARIABLE_REF (program, i)

/* ref */

VM_DEFINE_INSTRUCTION (19, object_ref, "object-ref", 1, 0, 1)
{
  register unsigned objnum = FETCH ();
  CHECK_OBJECT (objnum);
  PUSH (OBJECT_REF (objnum));
  NEXT;
}

/* FIXME: necessary? elt 255 of the vector could be a vector... */
VM_DEFINE_INSTRUCTION (20, long_object_ref, "long-object-ref", 2, 0, 1)
{
  unsigned int objnum = FETCH ();
  objnum <<= 8;
  objnum += FETCH ();
  CHECK_OBJECT (objnum);
  PUSH (OBJECT_REF (objnum));
  NEXT;
}

VM_DEFINE_INSTRUCTION (21, local_ref, "local-ref", 1, 0, 1)
{
  PUSH (LOCAL_REF (FETCH ()));
  ASSERT_BOUND (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (22, long_local_ref, "long-local-ref", 2, 0, 1)
{
  unsigned int i = FETCH ();
  i <<= 8;
  i += FETCH ();
  PUSH (LOCAL_REF (i));
  ASSERT_BOUND (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (23, local_bound, "local-bound?", 1, 0, 1)
{
  if (LOCAL_REF (FETCH ()) == SCM_UNDEFINED)
    PUSH (SCM_BOOL_F);
  else
    PUSH (SCM_BOOL_T);
  NEXT;
}

VM_DEFINE_INSTRUCTION (24, long_local_bound, "long-local-bound?", 2, 0, 1)
{
  unsigned int i = FETCH ();
  i <<= 8;
  i += FETCH ();
  if (LOCAL_REF (i) == SCM_UNDEFINED)
    PUSH (SCM_BOOL_F);
  else
    PUSH (SCM_BOOL_T);
  NEXT;
}

VM_DEFINE_INSTRUCTION (25, variable_ref, "variable-ref", 0, 1, 1)
{
  SCM x = *sp;

  /* We don't use ASSERT_VARIABLE or ASSERT_BOUND_VARIABLE here because,
     unlike in top-variable-ref, it really isn't an internal assertion
     that can be optimized out -- the variable could be coming directly
     from the user.  */
  if (SCM_UNLIKELY (!SCM_VARIABLEP (x)))
    {
      func_name = "variable-ref";
      finish_args = x;
      goto vm_error_not_a_variable;
    }
  else if (SCM_UNLIKELY (!VARIABLE_BOUNDP (x)))
    {
      SCM var_name;

      /* Attempt to provide the variable name in the error message.  */
      var_name = scm_module_reverse_lookup (scm_current_module (), x);
      finish_args = scm_is_true (var_name) ? var_name : x;
      goto vm_error_unbound;
    }
  else
    {
      SCM o = VARIABLE_REF (x);
      *sp = o;
    }

  NEXT;
}

VM_DEFINE_INSTRUCTION (26, variable_bound, "variable-bound?", 0, 1, 1)
{
  SCM x = *sp;
  
  if (SCM_UNLIKELY (!SCM_VARIABLEP (x)))
    {
      func_name = "variable-bound?";
      finish_args = x;
      goto vm_error_not_a_variable;
    }
  else
    *sp = scm_from_bool (VARIABLE_BOUNDP (x));
  NEXT;
}

VM_DEFINE_INSTRUCTION (27, toplevel_ref, "toplevel-ref", 1, 0, 1)
{
  unsigned objnum = FETCH ();
  SCM what, resolved;
  CHECK_OBJECT (objnum);
  what = OBJECT_REF (objnum);

  if (!SCM_VARIABLEP (what))
    {
      SYNC_REGISTER ();
      resolved = resolve_variable (what, scm_program_module (program));
      if (!VARIABLE_BOUNDP (resolved))
        {
          finish_args = what;
          goto vm_error_unbound;
        }
      what = resolved;
      OBJECT_SET (objnum, what);
    }

  PUSH (VARIABLE_REF (what));
  NEXT;
}

VM_DEFINE_INSTRUCTION (28, long_toplevel_ref, "long-toplevel-ref", 2, 0, 1)
{
  SCM what, resolved;
  unsigned int objnum = FETCH ();
  objnum <<= 8;
  objnum += FETCH ();
  CHECK_OBJECT (objnum);
  what = OBJECT_REF (objnum);

  if (!SCM_VARIABLEP (what))
    {
      SYNC_REGISTER ();
      resolved = resolve_variable (what, scm_program_module (program));
      if (!VARIABLE_BOUNDP (resolved))
        {
          finish_args = what;
          goto vm_error_unbound;
        }
      what = resolved;
      OBJECT_SET (objnum, what);
    }

  PUSH (VARIABLE_REF (what));
  NEXT;
}

/* set */

VM_DEFINE_INSTRUCTION (29, local_set, "local-set", 1, 1, 0)
{
  LOCAL_SET (FETCH (), *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (30, long_local_set, "long-local-set", 2, 1, 0)
{
  unsigned int i = FETCH ();
  i <<= 8;
  i += FETCH ();
  LOCAL_SET (i, *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (31, variable_set, "variable-set", 0, 2, 0)
{
  if (SCM_UNLIKELY (!SCM_VARIABLEP (sp[0])))
    {
      func_name = "variable-set!";
      finish_args = sp[0];
      goto vm_error_not_a_variable;
    }
  VARIABLE_SET (sp[0], sp[-1]);
  DROPN (2);
  NEXT;
}

VM_DEFINE_INSTRUCTION (32, toplevel_set, "toplevel-set", 1, 1, 0)
{
  unsigned objnum = FETCH ();
  SCM what;
  CHECK_OBJECT (objnum);
  what = OBJECT_REF (objnum);

  if (!SCM_VARIABLEP (what)) 
    {
      SYNC_BEFORE_GC ();
      what = resolve_variable (what, scm_program_module (program));
      OBJECT_SET (objnum, what);
    }

  VARIABLE_SET (what, *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (33, long_toplevel_set, "long-toplevel-set", 2, 1, 0)
{
  SCM what;
  unsigned int objnum = FETCH ();
  objnum <<= 8;
  objnum += FETCH ();
  CHECK_OBJECT (objnum);
  what = OBJECT_REF (objnum);

  if (!SCM_VARIABLEP (what)) 
    {
      SYNC_BEFORE_GC ();
      what = resolve_variable (what, scm_program_module (program));
      OBJECT_SET (objnum, what);
    }

  VARIABLE_SET (what, *sp);
  DROP ();
  NEXT;
}


/*
 * branch and jump
 */

/* offset must be at least 24 bits wide, and signed */
#define FETCH_OFFSET(offset)                    \
{						\
  offset = FETCH () << 16;                      \
  offset += FETCH () << 8;                      \
  offset += FETCH ();                           \
  offset -= (offset & (1<<23)) << 1;            \
}

#define BR(p)					\
{						\
  scm_t_int32 offset;                           \
  FETCH_OFFSET (offset);                        \
  if (p)					\
    ip += offset;                               \
  if (offset < 0)                               \
    VM_HANDLE_INTERRUPTS;                       \
  NULLSTACK (1);				\
  DROP ();					\
  NEXT;						\
}

VM_DEFINE_INSTRUCTION (34, br, "br", 3, 0, 0)
{
  scm_t_int32 offset;
  FETCH_OFFSET (offset);
  ip += offset;
  if (offset < 0)
    VM_HANDLE_INTERRUPTS;
  NEXT;
}

VM_DEFINE_INSTRUCTION (35, br_if, "br-if", 3, 0, 0)
{
  BR (scm_is_true (*sp));
}

VM_DEFINE_INSTRUCTION (36, br_if_not, "br-if-not", 3, 0, 0)
{
  BR (scm_is_false (*sp));
}

VM_DEFINE_INSTRUCTION (37, br_if_eq, "br-if-eq", 3, 0, 0)
{
  sp--; /* underflow? */
  BR (scm_is_eq (sp[0], sp[1]));
}

VM_DEFINE_INSTRUCTION (38, br_if_not_eq, "br-if-not-eq", 3, 0, 0)
{
  sp--; /* underflow? */
  BR (!scm_is_eq (sp[0], sp[1]));
}

VM_DEFINE_INSTRUCTION (39, br_if_null, "br-if-null", 3, 0, 0)
{
  BR (scm_is_null (*sp));
}

VM_DEFINE_INSTRUCTION (40, br_if_not_null, "br-if-not-null", 3, 0, 0)
{
  BR (!scm_is_null (*sp));
}


/*
 * Subprogram call
 */

VM_DEFINE_INSTRUCTION (41, br_if_nargs_ne, "br-if-nargs-ne", 5, 0, 0)
{
  scm_t_ptrdiff n;
  scm_t_int32 offset;
  n = FETCH () << 8;
  n += FETCH ();
  FETCH_OFFSET (offset);
  if (sp - (fp - 1) != n)
    ip += offset;
  NEXT;
}

VM_DEFINE_INSTRUCTION (42, br_if_nargs_lt, "br-if-nargs-lt", 5, 0, 0)
{
  scm_t_ptrdiff n;
  scm_t_int32 offset;
  n = FETCH () << 8;
  n += FETCH ();
  FETCH_OFFSET (offset);
  if (sp - (fp - 1) < n)
    ip += offset;
  NEXT;
}

VM_DEFINE_INSTRUCTION (43, br_if_nargs_gt, "br-if-nargs-gt", 5, 0, 0)
{
  scm_t_ptrdiff n;
  scm_t_int32 offset;

  n = FETCH () << 8;
  n += FETCH ();
  FETCH_OFFSET (offset);
  if (sp - (fp - 1) > n)
    ip += offset;
  NEXT;
}

VM_DEFINE_INSTRUCTION (44, assert_nargs_ee, "assert-nargs-ee", 2, 0, 0)
{
  scm_t_ptrdiff n;
  n = FETCH () << 8;
  n += FETCH ();
  if (sp - (fp - 1) != n)
    goto vm_error_wrong_num_args;
  NEXT;
}

VM_DEFINE_INSTRUCTION (45, assert_nargs_ge, "assert-nargs-ge", 2, 0, 0)
{
  scm_t_ptrdiff n;
  n = FETCH () << 8;
  n += FETCH ();
  if (sp - (fp - 1) < n)
    goto vm_error_wrong_num_args;
  NEXT;
}

VM_DEFINE_INSTRUCTION (46, bind_optionals, "bind-optionals", 2, -1, -1)
{
  scm_t_ptrdiff n;
  n = FETCH () << 8;
  n += FETCH ();
  while (sp - (fp - 1) < n)
    PUSH (SCM_UNDEFINED);
  NEXT;
}

VM_DEFINE_INSTRUCTION (47, bind_optionals_shuffle, "bind-optionals/shuffle", 6, -1, -1)
{
  SCM *walk;
  scm_t_ptrdiff nreq, nreq_and_opt, ntotal;
  nreq = FETCH () << 8;
  nreq += FETCH ();
  nreq_and_opt = FETCH () << 8;
  nreq_and_opt += FETCH ();
  ntotal = FETCH () << 8;
  ntotal += FETCH ();

  /* look in optionals for first keyword or last positional */
  /* starting after the last required positional arg */
  walk = fp + nreq;
  while (/* while we have args */
         walk <= sp
         /* and we still have positionals to fill */
         && walk - fp < nreq_and_opt
         /* and we haven't reached a keyword yet */
         && !scm_is_keyword (*walk))
    /* bind this optional arg (by leaving it in place) */
    walk++;
  /* now shuffle up, from walk to ntotal */
  {
    scm_t_ptrdiff nshuf = sp - walk + 1, i;
    sp = (fp - 1) + ntotal + nshuf;
    CHECK_OVERFLOW ();
    for (i = 0; i < nshuf; i++)
      sp[-i] = walk[nshuf-i-1];
  }
  /* and fill optionals & keyword args with SCM_UNDEFINED */
  while (walk <= (fp - 1) + ntotal)
    *walk++ = SCM_UNDEFINED;

  NEXT;
}

/* Flags that determine whether other keywords are allowed, and whether a
   rest argument is expected.  These values must match those used by the
   glil->assembly compiler.  */
#define F_ALLOW_OTHER_KEYS  1
#define F_REST              2

VM_DEFINE_INSTRUCTION (48, bind_kwargs, "bind-kwargs", 5, 0, 0)
{
  scm_t_uint16 idx;
  scm_t_ptrdiff nkw;
  int kw_and_rest_flags;
  SCM kw;
  idx = FETCH () << 8;
  idx += FETCH ();
  /* XXX: We don't actually use NKW.  */
  nkw = FETCH () << 8;
  nkw += FETCH ();
  kw_and_rest_flags = FETCH ();

  if (!(kw_and_rest_flags & F_REST)
      && ((sp - (fp - 1) - nkw) % 2))
    goto vm_error_kwargs_length_not_even;

  CHECK_OBJECT (idx);
  kw = OBJECT_REF (idx);

  /* Switch NKW to be a negative index below SP.  */
  for (nkw = -(sp - (fp - 1) - nkw) + 1; nkw < 0; nkw++)
    {
      SCM walk;

      if (scm_is_keyword (sp[nkw]))
	{
	  for (walk = kw; scm_is_pair (walk); walk = SCM_CDR (walk))
	    {
	      if (scm_is_eq (SCM_CAAR (walk), sp[nkw]))
		{
		  SCM si = SCM_CDAR (walk);
		  LOCAL_SET (SCM_I_INUMP (si) ? SCM_I_INUM (si) : scm_to_long (si),
			     sp[nkw + 1]);
		  break;
		}
	    }
	  if (!(kw_and_rest_flags & F_ALLOW_OTHER_KEYS) && !scm_is_pair (walk))
	    goto vm_error_kwargs_unrecognized_keyword;

	  nkw++;
	}
      else if (!(kw_and_rest_flags & F_REST))
        goto vm_error_kwargs_invalid_keyword;
    }

  NEXT;
}

#undef F_ALLOW_OTHER_KEYS
#undef F_REST


VM_DEFINE_INSTRUCTION (49, push_rest, "push-rest", 2, -1, -1)
{
  scm_t_ptrdiff n;
  SCM rest = SCM_EOL;
  n = FETCH () << 8;
  n += FETCH ();
  while (sp - (fp - 1) > n)
    /* No need to check for underflow. */
    CONS (rest, *sp--, rest);
  PUSH (rest);
  NEXT;
}

VM_DEFINE_INSTRUCTION (50, bind_rest, "bind-rest", 4, -1, -1)
{
  scm_t_ptrdiff n;
  scm_t_uint32 i;
  SCM rest = SCM_EOL;
  n = FETCH () << 8;
  n += FETCH ();
  i = FETCH () << 8;
  i += FETCH ();
  while (sp - (fp - 1) > n)
    /* No need to check for underflow. */
    CONS (rest, *sp--, rest);
  LOCAL_SET (i, rest);
  NEXT;
}

VM_DEFINE_INSTRUCTION (51, reserve_locals, "reserve-locals", 2, -1, -1)
{
  SCM *old_sp;
  scm_t_int32 n;
  n = FETCH () << 8;
  n += FETCH ();
  old_sp = sp;
  sp = (fp - 1) + n;

  if (old_sp < sp)
    {
      CHECK_OVERFLOW ();
      while (old_sp < sp)
        *++old_sp = SCM_UNDEFINED;
    }
  else
    NULLSTACK (old_sp - sp);

  NEXT;
}

VM_DEFINE_INSTRUCTION (52, new_frame, "new-frame", 0, 0, 3)
{
  /* NB: if you change this, see frames.c:vm-frame-num-locals */
  /* and frames.h, vm-engine.c, etc of course */

  /* We don't initialize the dynamic link here because we don't actually
     know that this frame will point to the current fp: it could be
     placed elsewhere on the stack if captured in a partial
     continuation, and invoked from some other context.  */
  PUSH (0); /* dynamic link */
  PUSH (0); /* mvra */
  PUSH (0); /* ra */
  NEXT;
}

VM_DEFINE_INSTRUCTION (53, call, "call", 1, -1, 1)
{
  nargs = FETCH ();

 vm_call:
  program = sp[-nargs];

  VM_HANDLE_INTERRUPTS;

  if (SCM_UNLIKELY (!SCM_PROGRAM_P (program)))
    {
      if (SCM_STRUCTP (program) && SCM_STRUCT_APPLICABLE_P (program))
        {
          sp[-nargs] = SCM_STRUCT_PROCEDURE (program);
          goto vm_call;
        }
      else if (SCM_NIMP (program) && SCM_TYP7 (program) == scm_tc7_smob
               && SCM_SMOB_APPLICABLE_P (program))
        {
          SYNC_REGISTER ();
          sp[-nargs] = scm_i_smob_apply_trampoline (program);
          goto vm_call;
        }
      else
        goto vm_error_wrong_type_apply;
    }

  CACHE_PROGRAM ();

  {
    SCM *old_fp = fp;

    fp = sp - nargs + 1;
  
    ASSERT (SCM_FRAME_DYNAMIC_LINK (fp) == 0);
    ASSERT (SCM_FRAME_RETURN_ADDRESS (fp) == 0);
    ASSERT (SCM_FRAME_MV_RETURN_ADDRESS (fp) == 0);
    SCM_FRAME_SET_DYNAMIC_LINK (fp, old_fp);
    SCM_FRAME_SET_RETURN_ADDRESS (fp, ip);
    SCM_FRAME_SET_MV_RETURN_ADDRESS (fp, 0);
  }
  
  ip = SCM_C_OBJCODE_BASE (bp);
  PUSH_CONTINUATION_HOOK ();
  APPLY_HOOK ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (54, tail_call, "tail-call", 1, -1, 1)
{
  nargs = FETCH ();

 vm_tail_call:
  program = sp[-nargs];

  VM_HANDLE_INTERRUPTS;

  if (SCM_UNLIKELY (!SCM_PROGRAM_P (program)))
    {
      if (SCM_STRUCTP (program) && SCM_STRUCT_APPLICABLE_P (program))
        {
          sp[-nargs] = SCM_STRUCT_PROCEDURE (program);
          goto vm_tail_call;
        }
      else if (SCM_NIMP (program) && SCM_TYP7 (program) == scm_tc7_smob
               && SCM_SMOB_APPLICABLE_P (program))
        {
          SYNC_REGISTER ();
          sp[-nargs] = scm_i_smob_apply_trampoline (program);
          goto vm_tail_call;
        }
      else
        goto vm_error_wrong_type_apply;
    }
  else
    {
      int i;
#ifdef VM_ENABLE_STACK_NULLING
      SCM *old_sp = sp;
      CHECK_STACK_LEAK ();
#endif

      /* switch programs */
      CACHE_PROGRAM ();
      /* shuffle down the program and the arguments */
      for (i = -1, sp = sp - nargs + 1; i < nargs; i++)
        SCM_FRAME_STACK_ADDRESS (fp)[i] = sp[i];

      sp = fp + i - 1;

      NULLSTACK (old_sp - sp);

      ip = SCM_C_OBJCODE_BASE (bp);

      APPLY_HOOK ();
      NEXT;
    }
}

VM_DEFINE_INSTRUCTION (55, subr_call, "subr-call", 1, -1, -1)
{
  SCM pointer, ret;
  SCM (*subr)();

  nargs = FETCH ();
  POP (pointer);

  subr = SCM_POINTER_VALUE (pointer);

  VM_HANDLE_INTERRUPTS;
  SYNC_REGISTER ();

  switch (nargs)
    {
    case 0:
      ret = subr ();
      break;
    case 1:
      ret = subr (sp[0]);
      break;
    case 2:
      ret = subr (sp[-1], sp[0]);
      break;
    case 3:
      ret = subr (sp[-2], sp[-1], sp[0]);
      break;
    case 4:
      ret = subr (sp[-3], sp[-2], sp[-1], sp[0]);
      break;
    case 5:
      ret = subr (sp[-4], sp[-3], sp[-2], sp[-1], sp[0]);
      break;
    case 6:
      ret = subr (sp[-5], sp[-4], sp[-3], sp[-2], sp[-1], sp[0]);
      break;
    case 7:
      ret = subr (sp[-6], sp[-5], sp[-4], sp[-3], sp[-2], sp[-1], sp[0]);
      break;
    case 8:
      ret = subr (sp[-7], sp[-6], sp[-5], sp[-4], sp[-3], sp[-2], sp[-1], sp[0]);
      break;
    case 9:
      ret = subr (sp[-8], sp[-7], sp[-6], sp[-5], sp[-4], sp[-3], sp[-2], sp[-1], sp[0]);
      break;
    case 10:
      ret = subr (sp[-9], sp[-8], sp[-7], sp[-6], sp[-5], sp[-4], sp[-3], sp[-2], sp[-1], sp[0]);
      break;
    default:
      abort ();
    }
  
  NULLSTACK_FOR_NONLOCAL_EXIT ();
      
  if (SCM_UNLIKELY (SCM_VALUESP (ret)))
    {
      /* multiple values returned to continuation */
      ret = scm_struct_ref (ret, SCM_INUM0);
      nvalues = scm_ilength (ret);
      PUSH_LIST (ret, scm_is_null);
      goto vm_return_values;
    }
  else
    {
      PUSH (ret);
      goto vm_return;
    }
}

VM_DEFINE_INSTRUCTION (56, smob_call, "smob-call", 1, -1, -1)
{
  SCM smob, ret;
  SCM (*subr)();
  nargs = FETCH ();
  POP (smob);

  subr = SCM_SMOB_DESCRIPTOR (smob).apply;

  VM_HANDLE_INTERRUPTS;
  SYNC_REGISTER ();

  switch (nargs)
    {
    case 0:
      ret = subr (smob);
      break;
    case 1:
      ret = subr (smob, sp[0]);
      break;
    case 2:
      ret = subr (smob, sp[-1], sp[0]);
      break;
    case 3:
      ret = subr (smob, sp[-2], sp[-1], sp[0]);
      break;
    default:
      abort ();
    }
  
  NULLSTACK_FOR_NONLOCAL_EXIT ();
      
  if (SCM_UNLIKELY (SCM_VALUESP (ret)))
    {
      /* multiple values returned to continuation */
      ret = scm_struct_ref (ret, SCM_INUM0);
      nvalues = scm_ilength (ret);
      PUSH_LIST (ret, scm_is_null);
      goto vm_return_values;
    }
  else
    {
      PUSH (ret);
      goto vm_return;
    }
}

VM_DEFINE_INSTRUCTION (57, foreign_call, "foreign-call", 1, -1, -1)
{
  SCM foreign, ret;
  nargs = FETCH ();
  POP (foreign);

  VM_HANDLE_INTERRUPTS;
  SYNC_REGISTER ();

  ret = scm_i_foreign_call (foreign, sp - nargs + 1);

  NULLSTACK_FOR_NONLOCAL_EXIT ();
      
  if (SCM_UNLIKELY (SCM_VALUESP (ret)))
    {
      /* multiple values returned to continuation */
      ret = scm_struct_ref (ret, SCM_INUM0);
      nvalues = scm_ilength (ret);
      PUSH_LIST (ret, scm_is_null);
      goto vm_return_values;
    }
  else
    {
      PUSH (ret);
      goto vm_return;
    }
}

VM_DEFINE_INSTRUCTION (58, continuation_call, "continuation-call", 0, -1, 0)
{
  SCM contregs;
  POP (contregs);

  SYNC_ALL ();
  scm_i_check_continuation (contregs);
  vm_return_to_continuation (scm_i_contregs_vm (contregs),
                             scm_i_contregs_vm_cont (contregs),
                             sp - (fp - 1), fp);
  scm_i_reinstate_continuation (contregs);

  /* no NEXT */
  abort ();
}

VM_DEFINE_INSTRUCTION (59, partial_cont_call, "partial-cont-call", 0, -1, 0)
{
  SCM vmcont, intwinds, prevwinds;
  POP (intwinds);
  POP (vmcont);
  SYNC_REGISTER ();
  if (SCM_UNLIKELY (!SCM_VM_CONT_REWINDABLE_P (vmcont)))
    { finish_args = vmcont;
      goto vm_error_continuation_not_rewindable;
    }
  prevwinds = scm_i_dynwinds ();
  vm_reinstate_partial_continuation (vm, vmcont, intwinds, sp + 1 - fp, fp,
                                     vm_cookie);

  /* Rewind prompt jmpbuffers, if any. */
  {
    SCM winds = scm_i_dynwinds ();
    for (; !scm_is_eq (winds, prevwinds); winds = scm_cdr (winds))
      if (SCM_PROMPT_P (scm_car (winds)) && SCM_PROMPT_SETJMP (scm_car (winds)))
        break;
  }
    
  CACHE_REGISTER ();
  program = SCM_FRAME_PROGRAM (fp);
  CACHE_PROGRAM ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (60, tail_call_nargs, "tail-call/nargs", 0, 0, 1)
{
  SCM x;
  POP (x);
  nargs = scm_to_int (x);
  /* FIXME: should truncate values? */
  goto vm_tail_call;
}

VM_DEFINE_INSTRUCTION (61, call_nargs, "call/nargs", 0, 0, 1)
{
  SCM x;
  POP (x);
  nargs = scm_to_int (x);
  /* FIXME: should truncate values? */
  goto vm_call;
}

VM_DEFINE_INSTRUCTION (62, mv_call, "mv-call", 4, -1, 1)
{
  scm_t_int32 offset;
  scm_t_uint8 *mvra;
  
  nargs = FETCH ();
  FETCH_OFFSET (offset);
  mvra = ip + offset;

 vm_mv_call:
  program = sp[-nargs];

  VM_HANDLE_INTERRUPTS;

  if (SCM_UNLIKELY (!SCM_PROGRAM_P (program)))
    {
      if (SCM_STRUCTP (program) && SCM_STRUCT_APPLICABLE_P (program))
        {
          sp[-nargs] = SCM_STRUCT_PROCEDURE (program);
          goto vm_mv_call;
        }
      else if (SCM_NIMP (program) && SCM_TYP7 (program) == scm_tc7_smob
               && SCM_SMOB_APPLICABLE_P (program))
        {
          SYNC_REGISTER ();
          sp[-nargs] = scm_i_smob_apply_trampoline (program);
          goto vm_mv_call;
        }
      else
        goto vm_error_wrong_type_apply;
    }

  CACHE_PROGRAM ();

  {
    SCM *old_fp = fp;

    fp = sp - nargs + 1;
  
    ASSERT (SCM_FRAME_DYNAMIC_LINK (fp) == 0);
    ASSERT (SCM_FRAME_RETURN_ADDRESS (fp) == 0);
    ASSERT (SCM_FRAME_MV_RETURN_ADDRESS (fp) == 0);
    SCM_FRAME_SET_DYNAMIC_LINK (fp, old_fp);
    SCM_FRAME_SET_RETURN_ADDRESS (fp, ip);
    SCM_FRAME_SET_MV_RETURN_ADDRESS (fp, mvra);
  }
  
  ip = SCM_C_OBJCODE_BASE (bp);
  PUSH_CONTINUATION_HOOK ();
  APPLY_HOOK ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (63, apply, "apply", 1, -1, 1)
{
  int len;
  SCM ls;
  POP (ls);

  nargs = FETCH ();
  ASSERT (nargs >= 2);

  len = scm_ilength (ls);
  if (SCM_UNLIKELY (len < 0))
    {
      finish_args = ls;
      goto vm_error_apply_to_non_list;
    }

  PUSH_LIST (ls, SCM_NULL_OR_NIL_P);

  nargs += len - 2;
  goto vm_call;
}

VM_DEFINE_INSTRUCTION (64, tail_apply, "tail-apply", 1, -1, 1)
{
  int len;
  SCM ls;
  POP (ls);

  nargs = FETCH ();
  ASSERT (nargs >= 2);

  len = scm_ilength (ls);
  if (SCM_UNLIKELY (len < 0))
    {
      finish_args = ls;
      goto vm_error_apply_to_non_list;
    }

  PUSH_LIST (ls, SCM_NULL_OR_NIL_P);

  nargs += len - 2;
  goto vm_tail_call;
}

VM_DEFINE_INSTRUCTION (65, call_cc, "call/cc", 0, 1, 1)
{
  int first;
  SCM proc, vm_cont, cont;
  POP (proc);
  SYNC_ALL ();
  vm_cont = scm_i_vm_capture_stack (vp->stack_base, fp, sp, ip, NULL, 0);
  cont = scm_i_make_continuation (&first, vm, vm_cont);
  if (first) 
    {
      PUSH (0); /* dynamic link */
      PUSH (0);  /* mvra */
      PUSH (0);  /* ra */
      PUSH (proc);
      PUSH (cont);
      nargs = 1;
      goto vm_call;
    }
  else 
    {
      /* Otherwise, the vm continuation was reinstated, and
         vm_return_to_continuation pushed on one value. We know only one
         value was returned because we are in value context -- the
         previous block jumped to vm_call, not vm_mv_call, after all.

         So, pull our regs back down from the vp, and march on to the
         next instruction. */
      CACHE_REGISTER ();
      program = SCM_FRAME_PROGRAM (fp);
      CACHE_PROGRAM ();
      RESTORE_CONTINUATION_HOOK ();
      NEXT;
    }
}

VM_DEFINE_INSTRUCTION (66, tail_call_cc, "tail-call/cc", 0, 1, 1)
{
  int first;
  SCM proc, vm_cont, cont;
  POP (proc);
  SYNC_ALL ();
  /* In contrast to call/cc, tail-call/cc captures the continuation without the
     stack frame. */
  vm_cont = scm_i_vm_capture_stack (vp->stack_base,
                                    SCM_FRAME_DYNAMIC_LINK (fp),
                                    SCM_FRAME_LOWER_ADDRESS (fp) - 1,
                                    SCM_FRAME_RETURN_ADDRESS (fp),
                                    SCM_FRAME_MV_RETURN_ADDRESS (fp),
                                    0);
  cont = scm_i_make_continuation (&first, vm, vm_cont);
  if (first) 
    {
      PUSH (proc);
      PUSH (cont);
      nargs = 1;
      goto vm_tail_call;
    }
  else
    {
      /* Otherwise, cache regs and NEXT, as above. Invoking the continuation
         does a return from the frame, either to the RA or
         MVRA. */
      CACHE_REGISTER ();
      program = SCM_FRAME_PROGRAM (fp);
      CACHE_PROGRAM ();
      /* Unfortunately we don't know whether we are at the RA, and thus
         have one value without an nvalues marker, or we are at the
         MVRA and thus have multiple values and the nvalues
         marker. Instead of adding heuristics here, we will let hook
         client code do that. */
      RESTORE_CONTINUATION_HOOK ();
      NEXT;
    }
}

VM_DEFINE_INSTRUCTION (67, return, "return", 0, 1, 1)
{
 vm_return:
  POP_CONTINUATION_HOOK (1);

  VM_HANDLE_INTERRUPTS;

  {
    SCM ret;

    POP (ret);

#ifdef VM_ENABLE_STACK_NULLING
    SCM *old_sp = sp;
#endif

    /* Restore registers */
    sp = SCM_FRAME_LOWER_ADDRESS (fp);
    ip = SCM_FRAME_RETURN_ADDRESS (fp);
    fp = SCM_FRAME_DYNAMIC_LINK (fp);

#ifdef VM_ENABLE_STACK_NULLING
    NULLSTACK (old_sp - sp);
#endif

    /* Set return value (sp is already pushed) */
    *sp = ret;
  }

  /* Restore the last program */
  program = SCM_FRAME_PROGRAM (fp);
  CACHE_PROGRAM ();
  CHECK_IP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (68, return_values, "return/values", 1, -1, -1)
{
  /* nvalues declared at top level, because for some reason gcc seems to think
     that perhaps it might be used without declaration. Fooey to that, I say. */
  nvalues = FETCH ();
 vm_return_values:
  POP_CONTINUATION_HOOK (nvalues);

  VM_HANDLE_INTERRUPTS;

  if (nvalues != 1 && SCM_FRAME_MV_RETURN_ADDRESS (fp)) 
    {
      /* A multiply-valued continuation */
      SCM *vals = sp - nvalues;
      int i;
      /* Restore registers */
      sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
      ip = SCM_FRAME_MV_RETURN_ADDRESS (fp);
      fp = SCM_FRAME_DYNAMIC_LINK (fp);
        
      /* Push return values, and the number of values */
      for (i = 0; i < nvalues; i++)
        *++sp = vals[i+1];
      *++sp = SCM_I_MAKINUM (nvalues);
             
      /* Finally null the end of the stack */
      NULLSTACK (vals + nvalues - sp);
    }
  else if (nvalues >= 1)
    {
      /* Multiple values for a single-valued continuation -- here's where I
         break with guile tradition and try and do something sensible. (Also,
         this block handles the single-valued return to an mv
         continuation.) */
      SCM *vals = sp - nvalues;
      /* Restore registers */
      sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
      ip = SCM_FRAME_RETURN_ADDRESS (fp);
      fp = SCM_FRAME_DYNAMIC_LINK (fp);
        
      /* Push first value */
      *++sp = vals[1];
             
      /* Finally null the end of the stack */
      NULLSTACK (vals + nvalues - sp);
    }
  else
    goto vm_error_no_values;

  /* Restore the last program */
  program = SCM_FRAME_PROGRAM (fp);
  CACHE_PROGRAM ();
  CHECK_IP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (69, return_values_star, "return/values*", 1, -1, -1)
{
  SCM l;

  nvalues = FETCH ();
  ASSERT (nvalues >= 1);
    
  nvalues--;
  POP (l);
  while (scm_is_pair (l))
    {
      PUSH (SCM_CAR (l));
      l = SCM_CDR (l);
      nvalues++;
    }
  if (SCM_UNLIKELY (!SCM_NULL_OR_NIL_P (l))) {
    finish_args = scm_list_1 (l);
    goto vm_error_improper_list;
  }

  goto vm_return_values;
}

VM_DEFINE_INSTRUCTION (70, return_nvalues, "return/nvalues", 0, 1, -1)
{
  SCM n;
  POP (n);
  nvalues = scm_to_int (n);
  ASSERT (nvalues >= 0);
  goto vm_return_values;
}

VM_DEFINE_INSTRUCTION (71, truncate_values, "truncate-values", 2, -1, -1)
{
  SCM x;
  int nbinds, rest;
  POP (x);
  nvalues = scm_to_int (x);
  nbinds = FETCH ();
  rest = FETCH ();

  if (rest)
    nbinds--;

  if (nvalues < nbinds)
    goto vm_error_not_enough_values;

  if (rest)
    POP_LIST (nvalues - nbinds);
  else
    DROPN (nvalues - nbinds);

  NEXT;
}

VM_DEFINE_INSTRUCTION (72, box, "box", 1, 1, 0)
{
  SCM val;
  POP (val);
  SYNC_BEFORE_GC ();
  LOCAL_SET (FETCH (), scm_cell (scm_tc7_variable, SCM_UNPACK (val)));
  NEXT;
}

/* for letrec:
   (let ((a *undef*) (b *undef*) ...)
     (set! a (lambda () (b ...)))
     ...)
 */
VM_DEFINE_INSTRUCTION (73, empty_box, "empty-box", 1, 0, 0)
{
  SYNC_BEFORE_GC ();
  LOCAL_SET (FETCH (),
             scm_cell (scm_tc7_variable, SCM_UNPACK (SCM_UNDEFINED)));
  NEXT;
}

VM_DEFINE_INSTRUCTION (74, local_boxed_ref, "local-boxed-ref", 1, 0, 1)
{
  SCM v = LOCAL_REF (FETCH ());
  ASSERT_BOUND_VARIABLE (v);
  PUSH (VARIABLE_REF (v));
  NEXT;
}

VM_DEFINE_INSTRUCTION (75, local_boxed_set, "local-boxed-set", 1, 1, 0)
{
  SCM v, val;
  v = LOCAL_REF (FETCH ());
  POP (val);
  ASSERT_VARIABLE (v);
  VARIABLE_SET (v, val);
  NEXT;
}

VM_DEFINE_INSTRUCTION (76, free_ref, "free-ref", 1, 0, 1)
{
  scm_t_uint8 idx = FETCH ();
  
  CHECK_FREE_VARIABLE (idx);
  PUSH (FREE_VARIABLE_REF (idx));
  NEXT;
}

/* no free-set -- if a var is assigned, it should be in a box */

VM_DEFINE_INSTRUCTION (77, free_boxed_ref, "free-boxed-ref", 1, 0, 1)
{
  SCM v;
  scm_t_uint8 idx = FETCH ();
  CHECK_FREE_VARIABLE (idx);
  v = FREE_VARIABLE_REF (idx);
  ASSERT_BOUND_VARIABLE (v);
  PUSH (VARIABLE_REF (v));
  NEXT;
}

VM_DEFINE_INSTRUCTION (78, free_boxed_set, "free-boxed-set", 1, 1, 0)
{
  SCM v, val;
  scm_t_uint8 idx = FETCH ();
  POP (val);
  CHECK_FREE_VARIABLE (idx);
  v = FREE_VARIABLE_REF (idx);
  ASSERT_BOUND_VARIABLE (v);
  VARIABLE_SET (v, val);
  NEXT;
}

VM_DEFINE_INSTRUCTION (79, make_closure, "make-closure", 2, -1, 1)
{
  size_t n, len;
  SCM closure;

  len = FETCH ();
  len <<= 8;
  len += FETCH ();
  SYNC_BEFORE_GC ();
  closure = scm_words (scm_tc7_program | (len<<16), len + 3);
  SCM_SET_CELL_OBJECT_1 (closure, SCM_PROGRAM_OBJCODE (sp[-len]));
  SCM_SET_CELL_OBJECT_2 (closure, SCM_PROGRAM_OBJTABLE (sp[-len]));
  sp[-len] = closure;
  for (n = 0; n < len; n++)
    SCM_PROGRAM_FREE_VARIABLE_SET (closure, n, sp[-len + 1 + n]);
  DROPN (len);
  NEXT;
}

VM_DEFINE_INSTRUCTION (80, make_variable, "make-variable", 0, 0, 1)
{
  SYNC_BEFORE_GC ();
  /* fixme underflow */
  PUSH (scm_cell (scm_tc7_variable, SCM_UNPACK (SCM_UNDEFINED)));
  NEXT;
}

VM_DEFINE_INSTRUCTION (81, fix_closure, "fix-closure", 2, -1, 0)
{
  SCM x;
  unsigned int i = FETCH ();
  size_t n, len;
  i <<= 8;
  i += FETCH ();
  /* FIXME CHECK_LOCAL (i) */ 
  x = LOCAL_REF (i);
  /* FIXME ASSERT_PROGRAM (x); */
  len = SCM_PROGRAM_NUM_FREE_VARIABLES (x);
  for (n = 0; n < len; n++)
    SCM_PROGRAM_FREE_VARIABLE_SET (x, n, sp[-len + 1 + n]);
  DROPN (len);
  NEXT;
}

VM_DEFINE_INSTRUCTION (82, define, "define", 0, 0, 2)
{
  SCM sym, val;
  POP (sym);
  POP (val);
  SYNC_REGISTER ();
  VARIABLE_SET (scm_sym2var (sym, scm_current_module_lookup_closure (),
                             SCM_BOOL_T),
                val);
  NEXT;
}

VM_DEFINE_INSTRUCTION (83, make_keyword, "make-keyword", 0, 1, 1)
{
  CHECK_UNDERFLOW ();
  SYNC_REGISTER ();
  *sp = scm_symbol_to_keyword (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (84, make_symbol, "make-symbol", 0, 1, 1)
{
  CHECK_UNDERFLOW ();
  SYNC_REGISTER ();
  *sp = scm_string_to_symbol (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (85, prompt, "prompt", 4, 2, 0)
{
  scm_t_int32 offset;
  scm_t_uint8 escape_only_p;
  SCM k, prompt;

  escape_only_p = FETCH ();
  FETCH_OFFSET (offset);
  POP (k);

  SYNC_REGISTER ();
  /* Push the prompt onto the dynamic stack. */
  prompt = scm_c_make_prompt (k, fp, sp, ip + offset, escape_only_p, vm_cookie,
                              scm_i_dynwinds ());
  scm_i_set_dynwinds (scm_cons (prompt, SCM_PROMPT_DYNWINDS (prompt)));
  if (SCM_PROMPT_SETJMP (prompt))
    {
      /* The prompt exited nonlocally. Cache the regs back from the vp, and go
         to the handler.

         Note, at this point, we must assume that any variable local to
         vm_engine that can be assigned *has* been assigned. So we need to pull
         all our state back from the ip/fp/sp.
      */
      CACHE_REGISTER ();
      program = SCM_FRAME_PROGRAM (fp);
      CACHE_PROGRAM ();
      /* The stack contains the values returned to this prompt, along
         with a number-of-values marker -- like an MV return. */
      ABORT_CONTINUATION_HOOK ();
      NEXT;
    }
      
  /* Otherwise setjmp returned for the first time, so we go to execute the
     prompt's body. */
  NEXT;
}

VM_DEFINE_INSTRUCTION (86, wind, "wind", 0, 2, 0)
{
  SCM wind, unwind;
  POP (unwind);
  POP (wind);
  SYNC_REGISTER ();
  /* Push wind and unwind procedures onto the dynamic stack. Note that neither
     are actually called; the compiler should emit calls to wind and unwind for
     the normal dynamic-wind control flow. */
  if (SCM_UNLIKELY (scm_is_false (scm_thunk_p (wind))))
    {
      finish_args = wind;
      goto vm_error_not_a_thunk;
    }
  if (SCM_UNLIKELY (scm_is_false (scm_thunk_p (unwind))))
    {
      finish_args = unwind;
      goto vm_error_not_a_thunk;
    }
  scm_i_set_dynwinds (scm_cons (scm_cons (wind, unwind), scm_i_dynwinds ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (87, abort, "abort", 1, -1, -1)
{
  unsigned n = FETCH ();
  SYNC_REGISTER ();
  if (sp - n - 2 <= SCM_FRAME_UPPER_ADDRESS (fp))
    goto vm_error_stack_underflow;
  vm_abort (vm, n, vm_cookie);
  /* vm_abort should not return */
  abort ();
}

VM_DEFINE_INSTRUCTION (88, unwind, "unwind", 0, 0, 0)
{
  /* A normal exit from the dynamic extent of an expression. Pop the top entry
     off of the dynamic stack. */
  scm_i_set_dynwinds (scm_cdr (scm_i_dynwinds ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (89, wind_fluids, "wind-fluids", 1, -1, 0)
{
  unsigned n = FETCH ();
  SCM wf;
  
  SYNC_REGISTER ();
  sp -= 2 * n;
  CHECK_UNDERFLOW ();
  wf = scm_i_make_with_fluids (n, sp + 1, sp + 1 + n);
  NULLSTACK (2 * n);

  scm_i_swap_with_fluids (wf, dynstate);
  scm_i_set_dynwinds (scm_cons (wf, scm_i_dynwinds ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (90, unwind_fluids, "unwind-fluids", 0, 0, 0)
{
  SCM wf;
  wf = scm_car (scm_i_dynwinds ());
  scm_i_set_dynwinds (scm_cdr (scm_i_dynwinds ()));
  scm_i_swap_with_fluids (wf, dynstate);
  NEXT;
}

VM_DEFINE_INSTRUCTION (91, fluid_ref, "fluid-ref", 0, 1, 1)
{
  size_t num;
  SCM fluids;
  
  CHECK_UNDERFLOW ();
  fluids = SCM_I_DYNAMIC_STATE_FLUIDS (dynstate);
  if (SCM_UNLIKELY (!SCM_FLUID_P (*sp))
      || ((num = SCM_I_FLUID_NUM (*sp)) >= SCM_SIMPLE_VECTOR_LENGTH (fluids)))
    {
      /* Punt dynstate expansion and error handling to the C proc. */
      SYNC_REGISTER ();
      *sp = scm_fluid_ref (*sp);
    }
  else
    {
      SCM val = SCM_SIMPLE_VECTOR_REF (fluids, num);
      if (SCM_UNLIKELY (val == SCM_UNDEFINED))
        {
          finish_args = *sp;
          goto vm_error_unbound_fluid;
        }
      *sp = val;
    }
  
  NEXT;
}

VM_DEFINE_INSTRUCTION (92, fluid_set, "fluid-set", 0, 2, 0)
{
  size_t num;
  SCM val, fluid, fluids;
  
  POP (val);
  POP (fluid);
  fluids = SCM_I_DYNAMIC_STATE_FLUIDS (dynstate);
  if (SCM_UNLIKELY (!SCM_FLUID_P (fluid))
      || ((num = SCM_I_FLUID_NUM (fluid)) >= SCM_SIMPLE_VECTOR_LENGTH (fluids)))
    {
      /* Punt dynstate expansion and error handling to the C proc. */
      SYNC_REGISTER ();
      scm_fluid_set_x (fluid, val);
    }
  else
    SCM_SIMPLE_VECTOR_SET (fluids, num, val);
  
  NEXT;
}

VM_DEFINE_INSTRUCTION (93, assert_nargs_ee_locals, "assert-nargs-ee/locals", 1, 0, 0)
{
  scm_t_ptrdiff n;
  SCM *old_sp;

  /* nargs = n & 0x7, nlocs = nargs + (n >> 3) */
  n = FETCH ();

  if (SCM_UNLIKELY (sp - (fp - 1) != (n & 0x7)))
    goto vm_error_wrong_num_args;

  old_sp = sp;
  sp += (n >> 3);
  CHECK_OVERFLOW ();
  while (old_sp < sp)
    *++old_sp = SCM_UNDEFINED;
  
  NEXT;
}


/*
(defun renumber-ops ()
  "start from top of buffer and renumber 'VM_DEFINE_FOO (\n' sequences"
  (interactive "")
  (save-excursion
    (let ((counter -1)) (goto-char (point-min))
      (while (re-search-forward "^VM_DEFINE_[^ ]+ (\\([^,]+\\)," (point-max) t)
        (replace-match
         (number-to-string (setq counter (1+ counter)))
          t t nil 1)))))
(renumber-ops)
*/
/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

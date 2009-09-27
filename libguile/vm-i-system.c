/* Copyright (C) 2001,2008,2009 Free Software Foundation, Inc.
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
  vp->time += scm_c_get_internal_run_time () - start_time;
  HALT_HOOK ();
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
    ASSERT (sp == stack_base);
    ASSERT (stack_base == SCM_FRAME_UPPER_ADDRESS (fp) - 1);

    /* Restore registers */
    sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
    ip = NULL;
    fp = SCM_FRAME_DYNAMIC_LINK (fp);
    NULLSTACK (stack_base - sp);
  }
  
  goto vm_done;
}

VM_DEFINE_INSTRUCTION (2, break, "break", 0, 0, 0)
{
  BREAK_HOOK ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (3, drop, "drop", 0, 1, 0)
{
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (4, dup, "dup", 0, 0, 1)
{
  SCM x = *sp;
  PUSH (x);
  NEXT;
}


/*
 * Object creation
 */

VM_DEFINE_INSTRUCTION (5, void, "void", 0, 0, 1)
{
  PUSH (SCM_UNSPECIFIED);
  NEXT;
}

VM_DEFINE_INSTRUCTION (6, make_true, "make-true", 0, 0, 1)
{
  PUSH (SCM_BOOL_T);
  NEXT;
}

VM_DEFINE_INSTRUCTION (7, make_false, "make-false", 0, 0, 1)
{
  PUSH (SCM_BOOL_F);
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

#define FREE_VARIABLE_REF(i)	free_vars[i]

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

VM_DEFINE_INSTRUCTION (23, variable_ref, "variable-ref", 0, 0, 1)
{
  SCM x = *sp;

  if (!VARIABLE_BOUNDP (x))
    {
      finish_args = scm_list_1 (x);
      /* Was: finish_args = SCM_LIST1 (SCM_CAR (x)); */
      goto vm_error_unbound;
    }
  else
    {
      SCM o = VARIABLE_REF (x);
      *sp = o;
    }

  NEXT;
}

VM_DEFINE_INSTRUCTION (24, toplevel_ref, "toplevel-ref", 1, 0, 1)
{
  unsigned objnum = FETCH ();
  SCM what;
  CHECK_OBJECT (objnum);
  what = OBJECT_REF (objnum);

  if (!SCM_VARIABLEP (what)) 
    {
      SYNC_REGISTER ();
      what = resolve_variable (what, scm_program_module (program));
      if (!VARIABLE_BOUNDP (what))
        {
          finish_args = scm_list_1 (what);
          goto vm_error_unbound;
        }
      OBJECT_SET (objnum, what);
    }

  PUSH (VARIABLE_REF (what));
  NEXT;
}

VM_DEFINE_INSTRUCTION (25, long_toplevel_ref, "long-toplevel-ref", 2, 0, 1)
{
  SCM what;
  unsigned int objnum = FETCH ();
  objnum <<= 8;
  objnum += FETCH ();
  CHECK_OBJECT (objnum);
  what = OBJECT_REF (objnum);

  if (!SCM_VARIABLEP (what)) 
    {
      SYNC_REGISTER ();
      what = resolve_variable (what, scm_program_module (program));
      if (!VARIABLE_BOUNDP (what))
        {
          finish_args = scm_list_1 (what);
          goto vm_error_unbound;
        }
      OBJECT_SET (objnum, what);
    }

  PUSH (VARIABLE_REF (what));
  NEXT;
}

/* set */

VM_DEFINE_INSTRUCTION (26, local_set, "local-set", 1, 1, 0)
{
  LOCAL_SET (FETCH (), *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (27, long_local_set, "long-local-set", 2, 1, 0)
{
  unsigned int i = FETCH ();
  i <<= 8;
  i += FETCH ();
  LOCAL_SET (i, *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (28, variable_set, "variable-set", 0, 1, 0)
{
  VARIABLE_SET (sp[0], sp[-1]);
  DROPN (2);
  NEXT;
}

VM_DEFINE_INSTRUCTION (29, toplevel_set, "toplevel-set", 1, 1, 0)
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

VM_DEFINE_INSTRUCTION (30, long_toplevel_set, "long-toplevel-set", 2, 1, 0)
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
  NULLSTACK (1);				\
  DROP ();					\
  NEXT;						\
}

VM_DEFINE_INSTRUCTION (31, br, "br", 3, 0, 0)
{
  scm_t_int32 offset;
  FETCH_OFFSET (offset);
  ip += offset;
  NEXT;
}

VM_DEFINE_INSTRUCTION (32, br_if, "br-if", 3, 0, 0)
{
  BR (!SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (33, br_if_not, "br-if-not", 3, 0, 0)
{
  BR (SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (34, br_if_eq, "br-if-eq", 3, 0, 0)
{
  sp--; /* underflow? */
  BR (SCM_EQ_P (sp[0], sp[1]));
}

VM_DEFINE_INSTRUCTION (35, br_if_not_eq, "br-if-not-eq", 3, 0, 0)
{
  sp--; /* underflow? */
  BR (!SCM_EQ_P (sp[0], sp[1]));
}

VM_DEFINE_INSTRUCTION (36, br_if_null, "br-if-null", 3, 0, 0)
{
  BR (SCM_NULLP (*sp));
}

VM_DEFINE_INSTRUCTION (37, br_if_not_null, "br-if-not-null", 3, 0, 0)
{
  BR (!SCM_NULLP (*sp));
}


/*
 * Subprogram call
 */

VM_DEFINE_INSTRUCTION (38, assert_nargs_ee, "assert-nargs-ee", 2, 0, 0)
{
  scm_t_ptrdiff n;
  n = FETCH () << 8;
  n += FETCH ();
#if 0
  if (sp - fp != n)
    goto vm_error_wrong_num_args;
#endif
  NEXT;
}

VM_DEFINE_INSTRUCTION (39, assert_nargs_ge, "assert-nargs-ge", 2, 0, 0)
{
  scm_t_ptrdiff n;
  n = FETCH () << 8;
  n += FETCH ();
#if 0
  if (sp - fp < n)
    goto vm_error_wrong_num_args;
#endif
  NEXT;
}

VM_DEFINE_INSTRUCTION (40, push_rest_list, "push-rest-list", 2, -1, -1)
{
  scm_t_ptrdiff n;
  n = FETCH () << 8;
  n += FETCH ();
#if 0
  SCM rest = SCM_EOL;
  while (sp - fp >= n)
    /* No need to check for underflow. */
    CONS (rest, *sp--, rest);
  PUSH (rest);
#endif
  NEXT;
}

VM_DEFINE_INSTRUCTION (41, new_frame, "new-frame", 0, 0, 3)
{
  PUSH ((SCM)fp); /* dynamic link */
  PUSH (0);  /* mvra */
  PUSH (0);  /* ra */
  NEXT;
}

VM_DEFINE_INSTRUCTION (42, call, "call", 1, -1, 1)
{
  SCM x;
  nargs = FETCH ();

 vm_call:
  x = sp[-nargs];

  SYNC_REGISTER ();
  SCM_TICK;	/* allow interrupt here */

  /*
   * Subprogram call
   */
  if (SCM_PROGRAM_P (x))
    {
      program = x;
      CACHE_PROGRAM ();
      INIT_ARGS ();
      fp = sp - bp->nargs + 1;
      ASSERT (SCM_FRAME_RETURN_ADDRESS (fp) == 0);
      ASSERT (SCM_FRAME_MV_RETURN_ADDRESS (fp) == 0);
      SCM_FRAME_SET_RETURN_ADDRESS (fp, ip);
      SCM_FRAME_SET_MV_RETURN_ADDRESS (fp, 0);
      INIT_FRAME ();
      ENTER_HOOK ();
      APPLY_HOOK ();
      NEXT;
    }
  /*
   * Other interpreted or compiled call
   */
  if (!SCM_FALSEP (scm_procedure_p (x)))
    {
      SCM args;
      /* At this point, the stack contains the frame, the procedure and each one
	 of its arguments. */
      POP_LIST (nargs);
      POP (args);
      DROP (); /* drop the procedure */
      DROP_FRAME ();
      
      SYNC_REGISTER ();
      PUSH (scm_apply (x, args, SCM_EOL));
      NULLSTACK_FOR_NONLOCAL_EXIT ();
      if (SCM_UNLIKELY (SCM_VALUESP (*sp)))
        {
          /* truncate values */
          SCM values;
          POP (values);
          values = scm_struct_ref (values, SCM_INUM0);
          if (scm_is_null (values))
            goto vm_error_not_enough_values;
          PUSH (SCM_CAR (values));
        }
      NEXT;
    }

  program = x;
  goto vm_error_wrong_type_apply;
}

VM_DEFINE_INSTRUCTION (43, goto_args, "goto/args", 1, -1, 1)
{
  register SCM x;
  nargs = FETCH ();
 vm_goto_args:
  x = sp[-nargs];

  SYNC_REGISTER ();
  SCM_TICK;	/* allow interrupt here */

  /*
   * Tail call
   */
  if (SCM_PROGRAM_P (x))
    {
      int i;
#ifdef VM_ENABLE_STACK_NULLING
      SCM *old_sp;
#endif

      EXIT_HOOK ();

      /* switch programs */
      program = x;
      CACHE_PROGRAM ();
      INIT_ARGS ();

#ifdef VM_ENABLE_STACK_NULLING
      old_sp = sp;
      CHECK_STACK_LEAK ();
#endif

      /* delay shuffling the new program+args down so that if INIT_ARGS had to
         cons up a rest arg, going into GC, the stack still made sense */
      for (i = -1, sp = sp - bp->nargs + 1; i < bp->nargs; i++)
        fp[i] = sp[i];
      sp = fp + i - 1;

      NULLSTACK (old_sp - sp);

      INIT_FRAME ();

      ENTER_HOOK ();
      APPLY_HOOK ();
      NEXT;
    }

  /*
   * Other interpreted or compiled call
   */
  if (!SCM_FALSEP (scm_procedure_p (x)))
    {
      SCM args;
      POP_LIST (nargs);
      POP (args);

      SYNC_REGISTER ();
      *sp = scm_apply (x, args, SCM_EOL);
      NULLSTACK_FOR_NONLOCAL_EXIT ();

      if (SCM_UNLIKELY (SCM_VALUESP (*sp)))
        {
          /* multiple values returned to continuation */
          SCM values;
          POP (values);
          values = scm_struct_ref (values, SCM_INUM0);
          nvalues = scm_ilength (values);
          PUSH_LIST (values, SCM_NULLP);
          goto vm_return_values;
        }
      else
        goto vm_return;
    }

  program = x;

  goto vm_error_wrong_type_apply;
}

VM_DEFINE_INSTRUCTION (44, goto_nargs, "goto/nargs", 0, 0, 1)
{
  SCM x;
  POP (x);
  nargs = scm_to_int (x);
  /* FIXME: should truncate values? */
  goto vm_goto_args;
}

VM_DEFINE_INSTRUCTION (45, call_nargs, "call/nargs", 0, 0, 1)
{
  SCM x;
  POP (x);
  nargs = scm_to_int (x);
  /* FIXME: should truncate values? */
  goto vm_call;
}

VM_DEFINE_INSTRUCTION (46, mv_call, "mv-call", 4, -1, 1)
{
  SCM x;
  scm_t_int32 offset;
  scm_t_uint8 *mvra;
  
  nargs = FETCH ();
  FETCH_OFFSET (offset);
  mvra = ip + offset;

  x = sp[-nargs];

  /*
   * Subprogram call
   */
  if (SCM_PROGRAM_P (x))
    {
      program = x;
      CACHE_PROGRAM ();
      INIT_ARGS ();
      fp = sp - bp->nargs + 1;
      ASSERT (SCM_FRAME_RETURN_ADDRESS (fp) == 0);
      ASSERT (SCM_FRAME_MV_RETURN_ADDRESS (fp) == 0);
      SCM_FRAME_SET_RETURN_ADDRESS (fp, ip);
      SCM_FRAME_SET_MV_RETURN_ADDRESS (fp, mvra);
      INIT_FRAME ();
      ENTER_HOOK ();
      APPLY_HOOK ();
      NEXT;
    }
  /*
   * Other interpreted or compiled call
   */
  if (!SCM_FALSEP (scm_procedure_p (x)))
    {
      SCM args;
      /* At this point, the stack contains the procedure and each one of its
	 arguments.  */
      POP_LIST (nargs);
      POP (args);
      DROP (); /* drop the procedure */
      DROP_FRAME ();
      
      SYNC_REGISTER ();
      PUSH (scm_apply (x, args, SCM_EOL));
      NULLSTACK_FOR_NONLOCAL_EXIT ();
      if (SCM_VALUESP (*sp))
        {
          SCM values, len;
          POP (values);
          values = scm_struct_ref (values, SCM_INUM0);
          len = scm_length (values);
          PUSH_LIST (values, SCM_NULLP);
          PUSH (len);
          ip = mvra;
        }
      NEXT;
    }

  program = x;
  goto vm_error_wrong_type_apply;
}

VM_DEFINE_INSTRUCTION (47, apply, "apply", 1, -1, 1)
{
  int len;
  SCM ls;
  POP (ls);

  nargs = FETCH ();
  ASSERT (nargs >= 2);

  len = scm_ilength (ls);
  if (len < 0)
    goto vm_error_wrong_type_arg;

  PUSH_LIST (ls, SCM_NULL_OR_NIL_P);

  nargs += len - 2;
  goto vm_call;
}

VM_DEFINE_INSTRUCTION (48, goto_apply, "goto/apply", 1, -1, 1)
{
  int len;
  SCM ls;
  POP (ls);

  nargs = FETCH ();
  ASSERT (nargs >= 2);

  len = scm_ilength (ls);
  if (len < 0)
    goto vm_error_wrong_type_arg;

  PUSH_LIST (ls, SCM_NULL_OR_NIL_P);

  nargs += len - 2;
  goto vm_goto_args;
}

VM_DEFINE_INSTRUCTION (49, call_cc, "call/cc", 0, 1, 1)
{
  int first;
  SCM proc, cont;
  POP (proc);
  SYNC_ALL ();
  cont = scm_make_continuation (&first);
  if (first) 
    {
      PUSH ((SCM)fp); /* dynamic link */
      PUSH (0);  /* mvra */
      PUSH (0);  /* ra */
      PUSH (proc);
      PUSH (cont);
      nargs = 1;
      goto vm_call;
    }
  ASSERT (sp == vp->sp);
  ASSERT (fp == vp->fp);
  else if (SCM_VALUESP (cont))
    {
      /* multiple values returned to continuation */
      SCM values;
      values = scm_struct_ref (cont, SCM_INUM0);
      if (SCM_NULLP (values))
        goto vm_error_no_values;
      /* non-tail context does not accept multiple values? */
      PUSH (SCM_CAR (values));
      NEXT;
    }
  else
    {
      PUSH (cont);
      NEXT;
    }
}

VM_DEFINE_INSTRUCTION (50, goto_cc, "goto/cc", 0, 1, 1)
{
  int first;
  SCM proc, cont;
  POP (proc);
  SYNC_ALL ();
  cont = scm_make_continuation (&first);
  ASSERT (sp == vp->sp);
  ASSERT (fp == vp->fp);
  if (first) 
    {
      PUSH (proc);
      PUSH (cont);
      nargs = 1;
      goto vm_goto_args;
    }
  else if (SCM_VALUESP (cont))
    {
      /* multiple values returned to continuation */
      SCM values;
      values = scm_struct_ref (cont, SCM_INUM0);
      nvalues = scm_ilength (values);
      PUSH_LIST (values, SCM_NULLP);
      goto vm_return_values;
    }
  else
    {
      PUSH (cont);
      goto vm_return;
    }
}

VM_DEFINE_INSTRUCTION (51, return, "return", 0, 1, 1)
{
 vm_return:
  EXIT_HOOK ();
  RETURN_HOOK ();
  SYNC_REGISTER ();
  SCM_TICK;	/* allow interrupt here */
  {
    SCM ret;

    POP (ret);
    ASSERT (sp == stack_base);
    ASSERT (stack_base == SCM_FRAME_UPPER_ADDRESS (fp) - 1);

    /* Restore registers */
    sp = SCM_FRAME_LOWER_ADDRESS (fp);
    ip = SCM_FRAME_RETURN_ADDRESS (fp);
    fp = SCM_FRAME_DYNAMIC_LINK (fp);
    {
#ifdef VM_ENABLE_STACK_NULLING
      int nullcount = stack_base - sp;
#endif
      stack_base = SCM_FRAME_UPPER_ADDRESS (fp) - 1;
      NULLSTACK (nullcount);
    }

    /* Set return value (sp is already pushed) */
    *sp = ret;
  }

  /* Restore the last program */
  program = SCM_FRAME_PROGRAM (fp);
  CACHE_PROGRAM ();
  CHECK_IP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (52, return_values, "return/values", 1, -1, -1)
{
  /* nvalues declared at top level, because for some reason gcc seems to think
     that perhaps it might be used without declaration. Fooey to that, I say. */
  nvalues = FETCH ();
 vm_return_values:
  EXIT_HOOK ();
  RETURN_HOOK ();

  ASSERT (stack_base == SCM_FRAME_UPPER_ADDRESS (fp) - 1);

  /* data[1] is the mv return address */
  if (nvalues != 1 && SCM_FRAME_MV_RETURN_ADDRESS (fp)) 
    {
      int i;
      /* Restore registers */
      sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
      ip = SCM_FRAME_MV_RETURN_ADDRESS (fp);
      fp = SCM_FRAME_DYNAMIC_LINK (fp);
        
      /* Push return values, and the number of values */
      for (i = 0; i < nvalues; i++)
        *++sp = stack_base[1+i];
      *++sp = SCM_I_MAKINUM (nvalues);
             
      /* Finally set new stack_base */
      NULLSTACK (stack_base - sp + nvalues + 1);
      stack_base = SCM_FRAME_UPPER_ADDRESS (fp) - 1;
    }
  else if (nvalues >= 1)
    {
      /* Multiple values for a single-valued continuation -- here's where I
         break with guile tradition and try and do something sensible. (Also,
         this block handles the single-valued return to an mv
         continuation.) */
      /* Restore registers */
      sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
      ip = SCM_FRAME_RETURN_ADDRESS (fp);
      fp = SCM_FRAME_DYNAMIC_LINK (fp);
        
      /* Push first value */
      *++sp = stack_base[1];
             
      /* Finally set new stack_base */
      NULLSTACK (stack_base - sp + nvalues + 1);
      stack_base = SCM_FRAME_UPPER_ADDRESS (fp) - 1;
    }
  else
    goto vm_error_no_values;

  /* Restore the last program */
  program = SCM_FRAME_PROGRAM (fp);
  CACHE_PROGRAM ();
  CHECK_IP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (53, return_values_star, "return/values*", 1, -1, -1)
{
  SCM l;

  nvalues = FETCH ();
  ASSERT (nvalues >= 1);
    
  nvalues--;
  POP (l);
  while (SCM_CONSP (l))
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

VM_DEFINE_INSTRUCTION (54, truncate_values, "truncate-values", 2, -1, -1)
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

VM_DEFINE_INSTRUCTION (55, box, "box", 1, 1, 0)
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
VM_DEFINE_INSTRUCTION (56, empty_box, "empty-box", 1, 0, 0)
{
  SYNC_BEFORE_GC ();
  LOCAL_SET (FETCH (),
             scm_cell (scm_tc7_variable, SCM_UNPACK (SCM_UNDEFINED)));
  NEXT;
}

VM_DEFINE_INSTRUCTION (57, local_boxed_ref, "local-boxed-ref", 1, 0, 1)
{
  SCM v = LOCAL_REF (FETCH ());
  ASSERT_BOUND_VARIABLE (v);
  PUSH (VARIABLE_REF (v));
  NEXT;
}

VM_DEFINE_INSTRUCTION (58, local_boxed_set, "local-boxed-set", 1, 1, 0)
{
  SCM v, val;
  v = LOCAL_REF (FETCH ());
  POP (val);
  ASSERT_VARIABLE (v);
  VARIABLE_SET (v, val);
  NEXT;
}

VM_DEFINE_INSTRUCTION (59, free_ref, "free-ref", 1, 0, 1)
{
  scm_t_uint8 idx = FETCH ();
  
  CHECK_FREE_VARIABLE (idx);
  PUSH (FREE_VARIABLE_REF (idx));
  NEXT;
}

/* no free-set -- if a var is assigned, it should be in a box */

VM_DEFINE_INSTRUCTION (60, free_boxed_ref, "free-boxed-ref", 1, 0, 1)
{
  SCM v;
  scm_t_uint8 idx = FETCH ();
  CHECK_FREE_VARIABLE (idx);
  v = FREE_VARIABLE_REF (idx);
  ASSERT_BOUND_VARIABLE (v);
  PUSH (VARIABLE_REF (v));
  NEXT;
}

VM_DEFINE_INSTRUCTION (61, free_boxed_set, "free-boxed-set", 1, 1, 0)
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

VM_DEFINE_INSTRUCTION (62, make_closure, "make-closure", 0, 2, 1)
{
  SCM vect;
  POP (vect);
  SYNC_BEFORE_GC ();
  /* fixme underflow */
  *sp = scm_double_cell (scm_tc7_program, (scm_t_bits)SCM_PROGRAM_OBJCODE (*sp),
                         (scm_t_bits)SCM_PROGRAM_OBJTABLE (*sp), (scm_t_bits)vect);
  NEXT;
}

VM_DEFINE_INSTRUCTION (63, make_variable, "make-variable", 0, 0, 1)
{
  SYNC_BEFORE_GC ();
  /* fixme underflow */
  PUSH (scm_cell (scm_tc7_variable, SCM_UNPACK (SCM_UNDEFINED)));
  NEXT;
}

VM_DEFINE_INSTRUCTION (64, fix_closure, "fix-closure", 2, 0, 1)
{
  SCM x, vect;
  unsigned int i = FETCH ();
  i <<= 8;
  i += FETCH ();
  POP (vect);
  /* FIXME CHECK_LOCAL (i) */ 
  x = LOCAL_REF (i);
  /* FIXME ASSERT_PROGRAM (x); */
  SCM_SET_CELL_WORD_3 (x, vect);
  NEXT;
}

VM_DEFINE_INSTRUCTION (65, define, "define", 0, 0, 2)
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

VM_DEFINE_INSTRUCTION (66, make_keyword, "make-keyword", 0, 1, 1)
{
  CHECK_UNDERFLOW ();
  SYNC_REGISTER ();
  *sp = scm_symbol_to_keyword (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (67, make_symbol, "make-symbol", 0, 1, 1)
{
  CHECK_UNDERFLOW ();
  SYNC_REGISTER ();
  *sp = scm_string_to_symbol (*sp);
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
*/
/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

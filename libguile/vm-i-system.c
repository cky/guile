/* Copyright (C) 2001,2008,2009 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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

VM_DEFINE_INSTRUCTION (4, mark, "mark", 0, 0, 1)
{
  PUSH (SCM_UNDEFINED);
  NEXT;
}

VM_DEFINE_INSTRUCTION (5, dup, "dup", 0, 0, 1)
{
  SCM x = *sp;
  PUSH (x);
  NEXT;
}


/*
 * Object creation
 */

VM_DEFINE_INSTRUCTION (6, void, "void", 0, 0, 1)
{
  PUSH (SCM_UNSPECIFIED);
  NEXT;
}

VM_DEFINE_INSTRUCTION (7, make_true, "make-true", 0, 0, 1)
{
  PUSH (SCM_BOOL_T);
  NEXT;
}

VM_DEFINE_INSTRUCTION (8, make_false, "make-false", 0, 0, 1)
{
  PUSH (SCM_BOOL_F);
  NEXT;
}

VM_DEFINE_INSTRUCTION (9, make_eol, "make-eol", 0, 0, 1)
{
  PUSH (SCM_EOL);
  NEXT;
}

VM_DEFINE_INSTRUCTION (10, make_int8, "make-int8", 1, 0, 1)
{
  PUSH (SCM_I_MAKINUM ((signed char) FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (11, make_int8_0, "make-int8:0", 0, 0, 1)
{
  PUSH (SCM_INUM0);
  NEXT;
}

VM_DEFINE_INSTRUCTION (12, make_int8_1, "make-int8:1", 0, 0, 1)
{
  PUSH (SCM_I_MAKINUM (1));
  NEXT;
}

VM_DEFINE_INSTRUCTION (13, make_int16, "make-int16", 2, 0, 1)
{
  int h = FETCH ();
  int l = FETCH ();
  PUSH (SCM_I_MAKINUM ((signed short) (h << 8) + l));
  NEXT;
}

VM_DEFINE_INSTRUCTION (14, make_char8, "make-char8", 1, 0, 1)
{
  PUSH (SCM_MAKE_CHAR (FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (15, list, "list", 2, -1, 1)
{
  unsigned h = FETCH ();
  unsigned l = FETCH ();
  unsigned len = ((h << 8) + l);
  POP_LIST (len);
  NEXT;
}

VM_DEFINE_INSTRUCTION (16, vector, "vector", 2, -1, 1)
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

VM_DEFINE_INSTRUCTION (17, list_mark, "list-mark", 0, 0, 0)
{
  POP_LIST_MARK ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (18, cons_mark, "cons-mark", 0, 0, 0)
{
  POP_CONS_MARK ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (19, vector_mark, "vector-mark", 0, 0, 0)
{
  POP_LIST_MARK ();
  SYNC_REGISTER ();
  *sp = scm_vector (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (20, list_break, "list-break", 0, 0, 0)
{
  SCM l;
  POP (l);
  PUSH_LIST (l, SCM_NULLP);
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

/* ref */

VM_DEFINE_INSTRUCTION (21, object_ref, "object-ref", 1, 0, 1)
{
  register unsigned objnum = FETCH ();
  CHECK_OBJECT (objnum);
  PUSH (OBJECT_REF (objnum));
  NEXT;
}

VM_DEFINE_INSTRUCTION (22, local_ref, "local-ref", 1, 0, 1)
{
  PUSH (LOCAL_REF (FETCH ()));
  ASSERT_BOUND (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (23, external_ref, "external-ref", 1, 0, 1)
{
  unsigned int i;
  SCM e = external;
  for (i = FETCH (); i; i--)
    {
      CHECK_EXTERNAL(e);
      e = SCM_CDR (e);
    }
  CHECK_EXTERNAL(e);
  PUSH (SCM_CAR (e));
  ASSERT_BOUND (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (24, variable_ref, "variable-ref", 0, 0, 1)
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

VM_DEFINE_INSTRUCTION (25, toplevel_ref, "toplevel-ref", 1, 0, 1)
{
  unsigned objnum = FETCH ();
  SCM what;
  CHECK_OBJECT (objnum);
  what = OBJECT_REF (objnum);

  if (!SCM_VARIABLEP (what)) 
    {
      SYNC_REGISTER ();
      if (SCM_LIKELY (SCM_SYMBOLP (what))) 
        {
          SCM mod = SCM_EOL;
          if (SCM_LIKELY (scm_module_system_booted_p
                          && scm_is_true ((mod = scm_program_module (program)))))
            /* might longjmp */
            what = scm_module_lookup (mod, what);
          else
            {
              SCM v = scm_sym2var (what, SCM_BOOL_F, SCM_BOOL_F);
              if (scm_is_false (v))
                SCM_MISC_ERROR ("unbound variable: ~S", scm_list_1 (what));
              else
                what = v;
            }
        }
      else
        {
          SCM mod;
          /* compilation of @ or @@
             `what' is a three-element list: (MODNAME SYM INTERFACE?)
             INTERFACE? is #t if we compiled @ or #f if we compiled @@
          */
          mod = scm_resolve_module (SCM_CAR (what));
          if (scm_is_true (SCM_CADDR (what)))
            mod = scm_module_public_interface (mod);
          if (SCM_FALSEP (mod))
            {
              finish_args = scm_list_1 (mod);
              goto vm_error_no_such_module;
            }
          /* might longjmp */
          what = scm_module_lookup (mod, SCM_CADR (what));
        }
          
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

VM_DEFINE_INSTRUCTION (27, external_set, "external-set", 1, 1, 0)
{
  unsigned int i;
  SCM e = external;
  for (i = FETCH (); i; i--)
    {
      CHECK_EXTERNAL(e);
      e = SCM_CDR (e);
    }
  CHECK_EXTERNAL(e);
  SCM_SETCAR (e, *sp);
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
      if (SCM_LIKELY (SCM_SYMBOLP (what))) 
        {
          SCM mod = SCM_EOL;
          if (SCM_LIKELY (scm_module_system_booted_p
                          && scm_is_true ((mod = scm_program_module (program)))))
            /* might longjmp */
            what = scm_module_lookup (mod, what);
          else
            {
              SCM v = scm_sym2var (what, SCM_BOOL_F, SCM_BOOL_F);
              if (scm_is_false (v))
                SCM_MISC_ERROR ("unbound variable: ~S", scm_list_1 (what));
              else
                what = v;
            }
        }
      else
        {
          SCM mod;
          /* compilation of @ or @@
             `what' is a three-element list: (MODNAME SYM INTERFACE?)
             INTERFACE? is #t if we compiled @ or #f if we compiled @@
          */
          mod = scm_resolve_module (SCM_CAR (what));
          if (scm_is_true (SCM_CADDR (what)))
            mod = scm_module_public_interface (mod);
          if (SCM_FALSEP (mod))
            {
              finish_args = scm_list_1 (what);
              goto vm_error_no_such_module;
            }
          /* might longjmp */
          what = scm_module_lookup (mod, SCM_CADR (what));
        }

      OBJECT_SET (objnum, what);
    }

  VARIABLE_SET (what, *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (30, externals, "externals", 0, 0, 1)
{
  PUSH (external);
  NEXT;
}


/*
 * branch and jump
 */

/* offset must be a signed short!!! */
#define FETCH_OFFSET(offset)                    \
{						\
  int h = FETCH ();				\
  int l = FETCH ();				\
  offset = (h << 8) + l;                        \
}

#define BR(p)					\
{						\
  signed short offset;                          \
  FETCH_OFFSET (offset);                        \
  if (p)					\
    ip += offset;				\
  NULLSTACK (1);				\
  DROP ();					\
  NEXT;						\
}

VM_DEFINE_INSTRUCTION (31, br, "br", 2, 0, 0)
{
  int h = FETCH ();
  int l = FETCH ();
  ip += (signed short) (h << 8) + l;
  NEXT;
}

VM_DEFINE_INSTRUCTION (32, br_if, "br-if", 2, 0, 0)
{
  BR (!SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (33, br_if_not, "br-if-not", 2, 0, 0)
{
  BR (SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (34, br_if_eq, "br-if-eq", 2, 0, 0)
{
  sp--; /* underflow? */
  BR (SCM_EQ_P (sp[0], sp[1]));
}

VM_DEFINE_INSTRUCTION (35, br_if_not_eq, "br-if-not-eq", 2, 0, 0)
{
  sp--; /* underflow? */
  BR (!SCM_EQ_P (sp[0], sp[1]));
}

VM_DEFINE_INSTRUCTION (36, br_if_null, "br-if-null", 2, 0, 0)
{
  BR (SCM_NULLP (*sp));
}

VM_DEFINE_INSTRUCTION (37, br_if_not_null, "br-if-not-null", 2, 0, 0)
{
  BR (!SCM_NULLP (*sp));
}


/*
 * Subprogram call
 */

VM_DEFINE_INSTRUCTION (38, make_closure, "make-closure", 0, 1, 1)
{
  SYNC_BEFORE_GC ();
  SCM_NEWSMOB3 (*sp, scm_tc16_program, SCM_PROGRAM_OBJCODE (*sp),
                SCM_PROGRAM_OBJTABLE (*sp), external);
  NEXT;
}

VM_DEFINE_INSTRUCTION (39, call, "call", 1, -1, 1)
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
      NEW_FRAME ();
      ENTER_HOOK ();
      APPLY_HOOK ();
      NEXT;
    }
#ifdef ENABLE_TRAMPOLINE
  /* Seems to slow down the fibo test, dunno why */
  /*
   * Subr call
   */
  switch (nargs) 
    {
    case 0:
      {
        scm_t_trampoline_0 call = scm_trampoline_0 (x);
        if (call) 
          {
            SYNC_ALL ();
            *sp = call (x);
            NEXT;
          }
        break;
      }
    case 1:
      {
        scm_t_trampoline_1 call = scm_trampoline_1 (x);
        if (call)
          {
            SCM arg1;
            POP (arg1);
            SYNC_ALL ();
            *sp = call (x, arg1);
            NEXT;
          }
        break;
      }
    case 2:
      {
        scm_t_trampoline_2 call = scm_trampoline_2 (x);
        if (call)
          {
            SCM arg1, arg2;
            POP (arg2);
            POP (arg1);
            SYNC_ALL ();
            *sp = call (x, arg1, arg2);
            NEXT;
          }
        break;
      }
    }
#endif
  /*
   * Other interpreted or compiled call
   */
  if (!SCM_FALSEP (scm_procedure_p (x)))
    {
      /* At this point, the stack contains the procedure and each one of its
	 arguments.  */
      POP_LIST (nargs);
      SYNC_REGISTER ();
      /* keep args on stack so they are marked */
      sp[-1] = scm_apply (x, sp[0], SCM_EOL);
      NULLSTACK_FOR_NONLOCAL_EXIT ();
      DROP ();
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
  /*
   * Continuation call
   */
  if (SCM_VM_CONT_P (x))
    {
      program = x;
    vm_call_continuation:
      /* Check the number of arguments */
      /* FIXME multiple args */
      if (nargs != 1)
	scm_wrong_num_args (program);

      /* Reinstate the continuation */
      EXIT_HOOK ();
      reinstate_vm_cont (vp, program);
      CACHE_REGISTER ();
      program = SCM_FRAME_PROGRAM (fp);
      CACHE_PROGRAM ();
      NEXT;
    }

  program = x;
  goto vm_error_wrong_type_apply;
}

VM_DEFINE_INSTRUCTION (40, goto_args, "goto/args", 1, -1, 1)
{
  register SCM x;
  nargs = FETCH ();
 vm_goto_args:
  x = sp[-nargs];

  SYNC_REGISTER ();
  SCM_TICK;	/* allow interrupt here */

  /*
   * Tail recursive call
   */
  if (SCM_EQ_P (x, program))
    {
      int i;

      /* Move arguments */
      INIT_ARGS ();
      sp -= bp->nargs - 1;
      for (i = 0; i < bp->nargs; i++)
	LOCAL_SET (i, sp[i]);

      /* Drop the first argument and the program itself.  */
      sp -= 2;
      NULLSTACK (bp->nargs + 1);

      /* Freshen the externals */
      external = SCM_PROGRAM_EXTERNALS (x);
      for (i = 0; i < bp->nexts; i++)
        CONS (external, SCM_UNDEFINED, external);
      SCM_FRAME_DATA_ADDRESS (fp)[0] = external;

      /* Init locals to valid SCM values */
      for (i = 0; i < bp->nlocs; i++)
	LOCAL_SET (i + bp->nargs, SCM_UNDEFINED);

      /* Call itself */
      ip = bp->base;
      APPLY_HOOK ();
      NEXT;
    }

  /*
   * Tail call, but not to self -- reuse the frame, keeping the ra and dl
   */
  if (SCM_PROGRAM_P (x))
    {
      SCM *data, *tail_args, *dl;
      int i;
      scm_byte_t *ra, *mvra;
#ifdef VM_ENABLE_STACK_NULLING
      SCM *old_sp;
#endif

      EXIT_HOOK ();

      /* save registers */
      tail_args = stack_base + 2;
      ra = SCM_FRAME_RETURN_ADDRESS (fp);
      mvra = SCM_FRAME_MV_RETURN_ADDRESS (fp);
      dl = SCM_FRAME_DYNAMIC_LINK (fp);

      /* switch programs */
      program = x;
      CACHE_PROGRAM ();
      INIT_ARGS ();
      /* delay updating the frame so that if INIT_ARGS has to cons up a rest
         arg, going into GC, the stack still makes sense */
      fp[-1] = program;
      nargs = bp->nargs;

#ifdef VM_ENABLE_STACK_NULLING
      old_sp = sp;
      CHECK_STACK_LEAK ();
#endif

      /* new registers -- logically this would be better later, but let's make
         sure we have space for the locals now */
      data = SCM_FRAME_DATA_ADDRESS (fp);
      ip = bp->base;
      stack_base = data + 3;
      sp = stack_base;
      CHECK_OVERFLOW ();

      /* copy args, bottom-up */
      for (i = 0; i < nargs; i++)
        fp[i] = tail_args[i];

      NULLSTACK (old_sp - sp);

      /* init locals */
      for (i = bp->nlocs; i; i--)
        data[-i] = SCM_UNDEFINED;
      
      /* Set frame data */
      data[3] = (SCM)ra;
      data[2] = (SCM)mvra;
      data[1] = (SCM)dl;

      /* Postpone initializing external vars, because if the CONS causes a GC,
         we want the stack marker to see the data array formatted as expected. */
      data[0] = SCM_UNDEFINED;
      external = SCM_PROGRAM_EXTERNALS (fp[-1]);
      for (i = 0; i < bp->nexts; i++)
        CONS (external, SCM_UNDEFINED, external);
      data[0] = external;

      ENTER_HOOK ();
      APPLY_HOOK ();
      NEXT;
    }
#ifdef ENABLE_TRAMPOLINE
  /* This seems to actually slow down the fibo test -- dunno why */
  /*
   * Subr call
   */
  switch (nargs) 
    {
    case 0:
      {
        scm_t_trampoline_0 call = scm_trampoline_0 (x);
        if (call) 
          {
            SYNC_ALL ();
            *sp = call (x);
            goto vm_return;
          }
        break;
      }
    case 1:
      {
        scm_t_trampoline_1 call = scm_trampoline_1 (x);
        if (call)
          {
            SCM arg1;
            POP (arg1);
            SYNC_ALL ();
            *sp = call (x, arg1);
            goto vm_return;
          }
        break;
      }
    case 2:
      {
        scm_t_trampoline_2 call = scm_trampoline_2 (x);
        if (call)
          {
            SCM arg1, arg2;
            POP (arg2);
            POP (arg1);
            SYNC_ALL ();
            *sp = call (x, arg1, arg2);
            goto vm_return;
          }
        break;
      }
    }
#endif

  /*
   * Other interpreted or compiled call
   */
  if (!SCM_FALSEP (scm_procedure_p (x)))
    {
      POP_LIST (nargs);
      SYNC_REGISTER ();
      sp[-1] = scm_apply (x, sp[0], SCM_EOL);
      NULLSTACK_FOR_NONLOCAL_EXIT ();
      DROP ();
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
      goto vm_return;
    }

  program = x;

  /*
   * Continuation call
   */
  if (SCM_VM_CONT_P (program))
    goto vm_call_continuation;

  goto vm_error_wrong_type_apply;
}

VM_DEFINE_INSTRUCTION (41, goto_nargs, "goto/nargs", 0, 0, 1)
{
  SCM x;
  POP (x);
  nargs = scm_to_int (x);
  /* FIXME: should truncate values? */
  goto vm_goto_args;
}

VM_DEFINE_INSTRUCTION (42, call_nargs, "call/nargs", 0, 0, 1)
{
  SCM x;
  POP (x);
  nargs = scm_to_int (x);
  /* FIXME: should truncate values? */
  goto vm_call;
}

VM_DEFINE_INSTRUCTION (43, mv_call, "mv-call", 3, -1, 1)
{
  SCM x;
  signed short offset;
  
  nargs = FETCH ();
  FETCH_OFFSET (offset);

  x = sp[-nargs];

  /*
   * Subprogram call
   */
  if (SCM_PROGRAM_P (x))
    {
      program = x;
      CACHE_PROGRAM ();
      INIT_ARGS ();
      NEW_FRAME ();
      SCM_FRAME_DATA_ADDRESS (fp)[2] = (SCM)(SCM_FRAME_RETURN_ADDRESS (fp) + offset);
      ENTER_HOOK ();
      APPLY_HOOK ();
      NEXT;
    }
  /*
   * Other interpreted or compiled call
   */
  if (!SCM_FALSEP (scm_procedure_p (x)))
    {
      /* At this point, the stack contains the procedure and each one of its
	 arguments.  */
      POP_LIST (nargs);
      SYNC_REGISTER ();
      sp[-1] = scm_apply (x, sp[0], SCM_EOL);
      NULLSTACK_FOR_NONLOCAL_EXIT ();
      DROP ();
      if (SCM_VALUESP (*sp))
        {
          SCM values, len;
          POP (values);
          values = scm_struct_ref (values, SCM_INUM0);
          len = scm_length (values);
          PUSH_LIST (values, SCM_NULLP);
          PUSH (len);
          ip += offset;
        }
      NEXT;
    }
  /*
   * Continuation call
   */
  if (SCM_VM_CONT_P (x))
    {
      program = x;
      goto vm_call_continuation;
    }

  program = x;
  goto vm_error_wrong_type_apply;
}

VM_DEFINE_INSTRUCTION (44, apply, "apply", 1, -1, 1)
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

VM_DEFINE_INSTRUCTION (45, goto_apply, "goto/apply", 1, -1, 1)
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

VM_DEFINE_INSTRUCTION (46, call_cc, "call/cc", 0, 1, 1)
{
  int first;
  SCM proc, cont;
  POP (proc);
  SYNC_ALL ();
  cont = scm_make_continuation (&first);
  if (first) 
    {
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

VM_DEFINE_INSTRUCTION (47, goto_cc, "goto/cc", 0, 1, 1)
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

VM_DEFINE_INSTRUCTION (48, return, "return", 0, 1, 1)
{
 vm_return:
  EXIT_HOOK ();
  RETURN_HOOK ();
  SYNC_REGISTER ();
  SCM_TICK;	/* allow interrupt here */
  {
    SCM ret, *data;
    data = SCM_FRAME_DATA_ADDRESS (fp);

    POP (ret);
    ASSERT (sp == stack_base);
    ASSERT (stack_base == data + 3);

    /* Restore registers */
    sp = SCM_FRAME_LOWER_ADDRESS (fp);
    ip = SCM_FRAME_BYTE_CAST (data[3]);
    fp = SCM_FRAME_STACK_CAST (data[1]);
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
  CACHE_EXTERNAL ();
  CHECK_IP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (49, return_values, "return/values", 1, -1, -1)
{
  /* nvalues declared at top level, because for some reason gcc seems to think
     that perhaps it might be used without declaration. Fooey to that, I say. */
  SCM *data;

  nvalues = FETCH ();
 vm_return_values:
  EXIT_HOOK ();
  RETURN_HOOK ();

  data = SCM_FRAME_DATA_ADDRESS (fp);
  ASSERT (stack_base == data + 3);

  /* data[2] is the mv return address */
  if (nvalues != 1 && data[2]) 
    {
      int i;
      /* Restore registers */
      sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
      ip = SCM_FRAME_BYTE_CAST (data[2]); /* multiple value ra */
      fp = SCM_FRAME_STACK_CAST (data[1]);
        
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
      ip = SCM_FRAME_BYTE_CAST (data[3]); /* single value ra */
      fp = SCM_FRAME_STACK_CAST (data[1]);
        
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
  CACHE_EXTERNAL ();
  CHECK_IP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (50, return_values_star, "return/values*", 1, -1, -1)
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

VM_DEFINE_INSTRUCTION (51, truncate_values, "truncate-values", 2, -1, -1)
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

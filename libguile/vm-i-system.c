/* Copyright (C) 2001 Free Software Foundation, Inc.
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

/* This file is included in vm_engine.c */


/*
 * Basic operations
 */

/* This must be the first instruction! */
VM_DEFINE_INSTRUCTION (nop, "nop", 0, 0, 0)
{
  NEXT;
}

VM_DEFINE_INSTRUCTION (halt, "halt", 0, 0, 0)
{
  SCM ret;
  vp->time += scm_c_get_internal_run_time () - start_time;
  HALT_HOOK ();
  nvalues = SCM_I_INUM (*sp--);
  if (nvalues == 1)
    POP (ret);
  else
    {
      POP_LIST (nvalues);
      POP (ret);
      SYNC_REGISTER ();
      ret = scm_values (ret);
    }
    
  {
#ifdef THE_GOVERNMENT_IS_AFTER_ME
    if (sp != stack_base)
      abort ();
    if (stack_base != SCM_FRAME_UPPER_ADDRESS (fp) - 1)
      abort ();
#endif

    /* Restore registers */
    sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
    ip = NULL;
    fp = SCM_FRAME_DYNAMIC_LINK (fp);
  }
  SYNC_ALL ();
  scm_dynwind_end ();
  return ret;
}

VM_DEFINE_INSTRUCTION (break, "break", 0, 0, 0)
{
  BREAK_HOOK ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (drop, "drop", 0, 0, 0)
{
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (mark, "mark", 0, 0, 1)
{
  PUSH (SCM_UNDEFINED);
  NEXT;
}

VM_DEFINE_INSTRUCTION (dup, "dup", 0, 0, 1)
{
  SCM x = *sp;
  PUSH (x);
  NEXT;
}


/*
 * Object creation
 */

VM_DEFINE_INSTRUCTION (void, "void", 0, 0, 1)
{
  PUSH (SCM_UNSPECIFIED);
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_true, "make-true", 0, 0, 1)
{
  PUSH (SCM_BOOL_T);
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_false, "make-false", 0, 0, 1)
{
  PUSH (SCM_BOOL_F);
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_eol, "make-eol", 0, 0, 1)
{
  PUSH (SCM_EOL);
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int8, "make-int8", 1, 0, 1)
{
  PUSH (SCM_I_MAKINUM ((signed char) FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int8_0, "make-int8:0", 0, 0, 1)
{
  PUSH (SCM_INUM0);
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int8_1, "make-int8:1", 0, 0, 1)
{
  PUSH (SCM_I_MAKINUM (1));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int16, "make-int16", 2, 0, 1)
{
  int h = FETCH ();
  int l = FETCH ();
  PUSH (SCM_I_MAKINUM ((signed short) (h << 8) + l));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_char8, "make-char8", 1, 0, 1)
{
  PUSH (SCM_MAKE_CHAR (FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (list, "list", 2, -1, 1)
{
  unsigned h = FETCH ();
  unsigned l = FETCH ();
  unsigned len = ((h << 8) + l);
  POP_LIST (len);
  NEXT;
}

VM_DEFINE_INSTRUCTION (vector, "vector", 2, -1, 1)
{
  unsigned h = FETCH ();
  unsigned l = FETCH ();
  unsigned len = ((h << 8) + l);
  POP_LIST (len);
  SYNC_REGISTER ();
  *sp = scm_vector (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (list_mark, "list-mark", 0, 0, 0)
{
  POP_LIST_MARK ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (vector_mark, "vector-mark", 0, 0, 0)
{
  POP_LIST_MARK ();
  SYNC_REGISTER ();
  *sp = scm_vector (*sp);
  NEXT;
}

VM_DEFINE_INSTRUCTION (list_break, "list-break", 0, 0, 0)
{
  SCM l;
  POP (l);
  for (; !SCM_NULLP (l); l = SCM_CDR (l))
    PUSH (SCM_CAR (l));
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

VM_DEFINE_INSTRUCTION (object_ref, "object-ref", 1, 0, 1)
{
  register unsigned objnum = FETCH ();
  CHECK_OBJECT (objnum);
  PUSH (OBJECT_REF (objnum));
  NEXT;
}

VM_DEFINE_INSTRUCTION (local_ref, "local-ref", 1, 0, 1)
{
  PUSH (LOCAL_REF (FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (external_ref, "external-ref", 1, 0, 1)
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
  NEXT;
}

VM_DEFINE_INSTRUCTION (variable_ref, "variable-ref", 0, 0, 1)
{
  SCM x = *sp;

  if (!VARIABLE_BOUNDP (x))
    {
      err_args = SCM_LIST1 (x);
      /* Was: err_args = SCM_LIST1 (SCM_CAR (x)); */
      goto vm_error_unbound;
    }
  else
    {
      SCM o = VARIABLE_REF (x);
      *sp = o;
    }

  NEXT;
}

VM_DEFINE_INSTRUCTION (late_variable_ref, "late-variable-ref", 1, 0, 1)
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
          if (SCM_LIKELY (scm_module_system_booted_p
                          && scm_is_true (bp->module)))
            /* might longjmp */
            what = scm_module_lookup (bp->module, what);
          else
            what = scm_sym2var (what, SCM_BOOL_F, SCM_BOOL_F);
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
              err_args = SCM_LIST1 (mod);
              goto vm_error_no_such_module;
            }
          /* might longjmp */
          what = scm_module_lookup (mod, SCM_CADR (what));
        }
          
      if (!VARIABLE_BOUNDP (what))
        {
          err_args = SCM_LIST1 (what);
          goto vm_error_unbound;
        }

      OBJECT_SET (objnum, what);
    }

  PUSH (VARIABLE_REF (what));
  NEXT;
}

/* set */

VM_DEFINE_INSTRUCTION (local_set, "local-set", 1, 1, 0)
{
  LOCAL_SET (FETCH (), *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (external_set, "external-set", 1, 1, 0)
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

VM_DEFINE_INSTRUCTION (variable_set, "variable-set", 0, 1, 0)
{
  VARIABLE_SET (sp[0], sp[-1]);
  sp -= 2;
  NEXT;
}

VM_DEFINE_INSTRUCTION (late_variable_set, "late-variable-set", 1, 1, 0)
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
          if (SCM_LIKELY (scm_module_system_booted_p
                          && scm_is_true (bp->module)))
            /* might longjmp */
            what = scm_module_lookup (bp->module, what);
          else
            what = scm_sym2var (what, SCM_BOOL_F, SCM_BOOL_F);
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
              err_args = SCM_LIST1 (what);
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
  DROP ();					\
  NEXT;						\
}

VM_DEFINE_INSTRUCTION (br, "br", 2, 0, 0)
{
  int h = FETCH ();
  int l = FETCH ();
  ip += (signed short) (h << 8) + l;
  NEXT;
}

VM_DEFINE_INSTRUCTION (br_if, "br-if", 2, 0, 0)
{
  BR (!SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (br_if_not, "br-if-not", 2, 0, 0)
{
  BR (SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (br_if_eq, "br-if-eq", 2, 0, 0)
{
  BR (SCM_EQ_P (sp[0], sp--[1]));
}

VM_DEFINE_INSTRUCTION (br_if_not_eq, "br-if-not-eq", 2, 0, 0)
{
  BR (!SCM_EQ_P (sp[0], sp--[1]));
}

VM_DEFINE_INSTRUCTION (br_if_null, "br-if-null", 2, 0, 0)
{
  BR (SCM_NULLP (*sp));
}

VM_DEFINE_INSTRUCTION (br_if_not_null, "br-if-not-null", 2, 0, 0)
{
  BR (!SCM_NULLP (*sp));
}


/*
 * Subprogram call
 */

VM_DEFINE_INSTRUCTION (make_closure, "make-closure", 0, 1, 1)
{
  SYNC_BEFORE_GC ();
  *sp = scm_c_make_closure (*sp, external);
  NEXT;
}

VM_DEFINE_INSTRUCTION (call, "call", 1, -1, 1)
{
  SCM x;
  nargs = FETCH ();

 vm_call:
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
      SCM args;
      POP_LIST (nargs);
      POP (args);
      SYNC_REGISTER ();
      *sp = scm_apply (x, args, SCM_EOL);
      /* FIXME what if SCM_VALUESP(*sp) */
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

VM_DEFINE_INSTRUCTION (goto_args, "goto/args", 1, -1, 1)
{
  register SCM x;
  nargs = FETCH ();
 vm_goto_args:
  x = sp[-nargs];

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

      EXIT_HOOK ();

      /* save registers */
      tail_args = stack_base + 2;
      ra = SCM_FRAME_RETURN_ADDRESS (fp);
      mvra = SCM_FRAME_MV_RETURN_ADDRESS (fp);
      dl = SCM_FRAME_DYNAMIC_LINK (fp);

      /* switch programs */
      fp[-1] = program = x;
      CACHE_PROGRAM ();
      INIT_ARGS ();
      nargs = bp->nargs;

      /* new registers -- logically this would be better later, but let's make
         sure we have space for the locals now */
      data = SCM_FRAME_DATA_ADDRESS (fp);
      ip = bp->base;
      stack_base = data + 4;
      sp = stack_base;
      CHECK_OVERFLOW ();

      /* copy args, bottom-up */
      for (i = 0; i < nargs; i++)
        fp[i] = tail_args[i];

      /* init locals */
      for (i = bp->nlocs; i; i--)
        data[-i] = SCM_UNDEFINED;
      
      /* and the external variables */
      external = bp->external;
      for (i = 0; i < bp->nexts; i++)
        CONS (external, SCM_UNDEFINED, external);

      /* Set frame data */
      data[4] = (SCM)ra;
      data[3] = (SCM)mvra;
      data[2] = (SCM)dl;
      data[1] = SCM_BOOL_F;
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
      SCM args;
      POP_LIST (nargs);
      POP (args);
      SYNC_REGISTER ();
      *sp = scm_apply (x, args, SCM_EOL);
      /* FIXME what if SCM_VALUESP(*sp) */
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

VM_DEFINE_INSTRUCTION (goto_nargs, "goto/nargs", 0, 0, 1)
{
  SCM x;
  POP (x);
  nargs = scm_to_int (x);
  /* FIXME: should truncate values? */
  goto vm_goto_args;
}

VM_DEFINE_INSTRUCTION (call_nargs, "call/nargs", 0, 0, 1)
{
  SCM x;
  POP (x);
  nargs = scm_to_int (x);
  /* FIXME: should truncate values? */
  goto vm_call;
}

VM_DEFINE_INSTRUCTION (mv_call, "mv-call", 3, -1, 1)
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
      SCM_FRAME_DATA_ADDRESS (fp)[3] = (SCM)(SCM_FRAME_RETURN_ADDRESS (fp) + offset);
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
      SCM args;
      POP_LIST (nargs);
      POP (args);
      SYNC_REGISTER ();
      *sp = scm_apply (x, args, SCM_EOL);
      if (SCM_VALUESP (*sp))
        {
          SCM values, len;
          POP (values);
          values = scm_struct_ref (values, SCM_INUM0);
          len = scm_length (values);
          for (; !SCM_NULLP (values); values = SCM_CDR (values))
            PUSH (SCM_CAR (values));
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

VM_DEFINE_INSTRUCTION (apply, "apply", 1, -1, 1)
{
  int len;
  SCM ls;
  POP (ls);

  nargs = FETCH ();
  if (nargs < 2)
    goto vm_error_wrong_num_args;

  len = scm_ilength (ls);
  if (len < 0)
    goto vm_error_wrong_type_arg;

  for (; !SCM_NULLP (ls); ls = SCM_CDR (ls))
    PUSH (SCM_CAR (ls));

  nargs += len - 2;
  goto vm_call;
}

VM_DEFINE_INSTRUCTION (goto_apply, "goto/apply", 1, -1, 1)
{
  int len;
  SCM ls;
  POP (ls);

  nargs = FETCH ();
  if (nargs < 2)
    goto vm_error_wrong_num_args;

  len = scm_ilength (ls);
  if (len < 0)
    goto vm_error_wrong_type_arg;

  for (; !SCM_NULLP (ls); ls = SCM_CDR (ls))
    PUSH (SCM_CAR (ls));

  nargs += len - 2;
  goto vm_goto_args;
}

VM_DEFINE_INSTRUCTION (call_cc, "call/cc", 0, 1, 1)
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
  else if (SCM_VALUESP (cont))
    {
      /* multiple values returned to continuation */
      SCM values;
      values = scm_struct_ref (cont, SCM_INUM0);
      if (SCM_NULLP (values))
        goto vm_error_wrong_num_args;
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

VM_DEFINE_INSTRUCTION (goto_cc, "goto/cc", 0, 1, 1)
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
      goto vm_goto_args;
    }
  else if (SCM_VALUESP (cont))
    {
      /* multiple values returned to continuation */
      SCM values;
      values = scm_struct_ref (cont, SCM_INUM0);
      nvalues = scm_ilength (values);
      for (; !SCM_NULLP (values); values = SCM_CDR (values))
        PUSH (SCM_CAR (values));
      goto vm_return_values;
    }
  else
    {
      PUSH (cont);
      goto vm_return;
    }
}

VM_DEFINE_INSTRUCTION (return, "return", 0, 0, 1)
{
 vm_return:
  EXIT_HOOK ();
  RETURN_HOOK ();
  {
    SCM ret, *data;
    data = SCM_FRAME_DATA_ADDRESS (fp);

    POP (ret);
#ifdef THE_GOVERNMENT_IS_AFTER_ME
    if (sp != stack_base)
      abort ();
    if (stack_base != data + 4)
      abort ();
#endif

    /* Restore registers */
    sp = SCM_FRAME_LOWER_ADDRESS (fp);
    ip = SCM_FRAME_BYTE_CAST (data[4]);
    fp = SCM_FRAME_STACK_CAST (data[2]);
    stack_base = SCM_FRAME_UPPER_ADDRESS (fp) - 1;

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

VM_DEFINE_INSTRUCTION (return_values, "return/values", 1, -1, -1)
{
  /* nvalues declared at top level, because for some reason gcc seems to think
     that perhaps it might be used without declaration. Fooey to that, I say. */
  SCM *data;

  nvalues = FETCH ();
 vm_return_values:
  EXIT_HOOK ();
  RETURN_HOOK ();

  data = SCM_FRAME_DATA_ADDRESS (fp);
#ifdef THE_GOVERNMENT_IS_AFTER_ME
  if (stack_base != data + 4)
    abort ();
#endif

  /* data[3] is the mv return address */
  if (nvalues != 1 && data[3]) 
    {
      int i;
      /* Restore registers */
      sp = SCM_FRAME_LOWER_ADDRESS (fp) - 1;
      ip = SCM_FRAME_BYTE_CAST (data[3]); /* multiple value ra */
      fp = SCM_FRAME_STACK_CAST (data[2]);
        
      /* Push return values, and the number of values */
      for (i = 0; i < nvalues; i++)
        *++sp = stack_base[1+i];
      *++sp = SCM_I_MAKINUM (nvalues);
             
      /* Finally set new stack_base */
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
      ip = SCM_FRAME_BYTE_CAST (data[4]); /* single value ra */
      fp = SCM_FRAME_STACK_CAST (data[2]);
        
      /* Push first value */
      *++sp = stack_base[1];
             
      /* Finally set new stack_base */
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

VM_DEFINE_INSTRUCTION (return_values_star, "return/values*", 1, -1, -1)
{
  SCM l;

  nvalues = FETCH ();
#ifdef THE_GOVERNMENT_IS_AFTER_ME
  if (nvalues < 1)
    abort ();
#endif
    
  nvalues--;
  POP (l);
  while (SCM_CONSP (l))
    {
      PUSH (SCM_CAR (l));
      l = SCM_CDR (l);
      nvalues++;
    }

  goto vm_return_values;
}

VM_DEFINE_INSTRUCTION (truncate_values, "truncate-values", 2, -1, -1)
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
  Local Variables:
  c-file-style: "gnu"
  End:
*/

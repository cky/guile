/* Copyright (C) 2000 Free Software Foundation, Inc.
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
VM_DEFINE_INSTRUCTION (nop, "nop", 0)
{
  NEXT;
}

VM_DEFINE_INSTRUCTION (halt, "halt", 0)
{
  SCM ret = *sp;
  HALT_HOOK ();
  FREE_FRAME ();
  SYNC_ALL ();
  return ret;
}

VM_DEFINE_INSTRUCTION (drop, "drop", 0)
{
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (dup, "dup", 0)
{
  PUSH (*sp);
  NEXT;
}


/*
 * Object creation
 */

VM_DEFINE_INSTRUCTION (void, "void", 0)
{
  PUSH (SCM_UNSPECIFIED);
  NEXT;
}

VM_DEFINE_INSTRUCTION (mark, "mark", 0)
{
  PUSH (SCM_UNDEFINED);
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_true, "make-true", 0)
{
  PUSH (SCM_BOOL_T);
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_false, "make-false", 0)
{
  PUSH (SCM_BOOL_F);
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_eol, "make-eol", 0)
{
  PUSH (SCM_EOL);
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int8, "make-int8", 1)
{
  PUSH (SCM_MAKINUM ((signed char) FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int8_0, "make-int8:0", 0)
{
  PUSH (SCM_MAKINUM (0));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int8_1, "make-int8:1", 0)
{
  PUSH (SCM_MAKINUM (1));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int16, "make-int16", 2)
{
  int h = FETCH ();
  int l = FETCH ();
  PUSH (SCM_MAKINUM ((signed short) (h << 8) + l));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_char8, "make-char8", 1)
{
  PUSH (SCM_MAKE_CHAR (FETCH ()));
  NEXT;
}


/*
 * Variable access
 */

#define OBJECT_REF(i)		objects[i]
#define OBJECT_SET(i,o)		objects[i] = o

#define LOCAL_REF(i)		SCM_VM_FRAME_VARIABLE (fp, i)
#define LOCAL_SET(i,o)		SCM_VM_FRAME_VARIABLE (fp, i) = o

#define VARIABLE_REF(v)		SCM_CDR (v)
#define VARIABLE_SET(v,o)	SCM_SETCDR (v, o)

VM_DEFINE_INSTRUCTION (external, "external", 1)
{
  int n = FETCH ();
  while (n-- > 0)
    CONS (external, SCM_UNDEFINED, external);
  NEXT;
}

/* ref */

VM_DEFINE_INSTRUCTION (object_ref, "object-ref", 1)
{
  PUSH (OBJECT_REF (FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (local_ref, "local-ref", 1)
{
  PUSH (LOCAL_REF (FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (local_ref_0, "local-ref:0", 0)
{
  PUSH (LOCAL_REF (0));
  NEXT;
}

VM_DEFINE_INSTRUCTION (external_ref, "external-ref", 1)
{
  unsigned int i;
  SCM e = external;
  for (i = FETCH (); i; i--)
    e = SCM_CDR (e);
  PUSH (SCM_CAR (e));
  NEXT;
}

VM_DEFINE_INSTRUCTION (module_ref, "module-ref", 1)
{
  int i = FETCH ();
  SCM o, x = OBJECT_REF (i);
  o = VARIABLE_REF (x);
  if (SCM_UNBNDP (o))
    {
      err_args = SCM_LIST1 (SCM_CAR (x));
      goto vm_error_unbound;
    }
  PUSH (o);
  NEXT;
}

VM_DEFINE_INSTRUCTION (variable_ref, "variable-ref", 0)
{
  SCM x = *sp;
  SCM o = VARIABLE_REF (x);
  if (SCM_UNBNDP (o))
    {
      err_args = SCM_LIST1 (SCM_CAR (x));
      goto vm_error_unbound;
    }
  *sp = o;
  NEXT;
}

/* set */

VM_DEFINE_INSTRUCTION (local_set, "local-set", 1)
{
  LOCAL_SET (FETCH (), *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (external_set, "external-set", 1)
{
  unsigned int i;
  SCM e = external;
  for (i = FETCH (); i; i--)
    e = SCM_CDR (e);
  SCM_SETCAR (e, *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (module_set, "module-set", 1)
{
  int i = FETCH ();
  SCM x = OBJECT_REF (i);
  VARIABLE_SET (x, *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (variable_set, "variable-set", 0)
{
  VARIABLE_SET (sp[0], sp[1]);
  sp += 2;
  NEXT;
}


/*
 * branch and jump
 */

#define BR(p)					\
{						\
  signed char offset = FETCH ();		\
  if (p)					\
    ip += offset;				\
  DROP ();					\
  NEXT;						\
}

VM_DEFINE_INSTRUCTION (br_if, "br-if", 1)
{
  BR (!SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (br_if_not, "br-if-not", 1)
{
  BR (SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (br_if_eq, "br-if-eq", 1)
{
  BR (SCM_EQ_P (sp[0], sp--[1]));
}

VM_DEFINE_INSTRUCTION (br_if_not_eq, "br-if-not-eq", 1)
{
  BR (!SCM_EQ_P (sp[0], sp--[1]));
}

VM_DEFINE_INSTRUCTION (br_if_null, "br-if-null", 1)
{
  BR (SCM_NULLP (*sp));
}

VM_DEFINE_INSTRUCTION (br_if_not_null, "br-if-not-null", 1)
{
  BR (!SCM_NULLP (*sp));
}

VM_DEFINE_INSTRUCTION (jump, "jump", 1)
{
  ip += (signed char) FETCH ();
  NEXT;
}


/*
 * Subprogram call
 */

VM_DEFINE_INSTRUCTION (make_closure, "make-closure", 0)
{
  SYNC ();
  *sp = scm_c_make_vclosure (*sp, external);
  NEXT;
}

VM_DEFINE_INSTRUCTION (call, "call", 1)
{
  POP (program);
  nargs = FETCH ();

 vm_call:
  /*
   * Subprogram call
   */
  if (SCM_PROGRAM_P (program))
    {
      CACHE_PROGRAM ();
      INIT_ARGS ();
      NEW_FRAME ();
      INIT_VARIABLES ();
      ENTER_HOOK ();
      APPLY_HOOK ();
      NEXT;
    }
  /*
   * Function call
   */
  if (!SCM_FALSEP (scm_procedure_p (program)))
    {
      POP_LIST (nargs);
      *sp = scm_apply (program, *sp, SCM_EOL);
      program = SCM_VM_FRAME_PROGRAM (fp);
      NEXT;
    }
  /*
   * Continuation call
   */
  if (SCM_VM_CONT_P (program))
    {
    vm_call_cc:
      /* Check the number of arguments */
      if (nargs != 1)
	scm_wrong_num_args (program);

      /* Reinstate the continuation */
      EXIT_HOOK ();
      reinstate_vm_cont (vmp, program);
      CACHE ();
      /* We don't need to set the return value here
	 because it is already on the top of the stack. */
      NEXT;
    }

  goto vm_error_wrong_type_apply;
}

VM_DEFINE_INSTRUCTION (tail_call, "tail-call", 1)
{
  SCM x;
  POP (x);
  nargs = FETCH ();

  SCM_TICK;	/* allow interrupt here */

  /*
   * Tail recursive call
   */
  if (SCM_EQ_P (x, program))
    {
      INIT_ARGS ();

      /* Move arguments */
      if (bp->nargs)
	{
	  int i;
	  SCM *base = fp + bp->nlocs;
	  for (i = 0; i < bp->nargs; i++)
	    base[i] = sp[i];
	}

      ip = bp->base;
      sp = SCM_VM_FRAME_LOWER_ADDRESS (fp);
      APPLY_HOOK ();
      NEXT;
    }
  program = x;
  /*
   * Proper tail call
   */
  if (SCM_PROGRAM_P (program))
    {
      int i;
      int n = SCM_VM_FRAME_LOWER_ADDRESS (fp) - sp;
      SCM *base = sp;

      /* Exit the current frame */
      EXIT_HOOK ();
      FREE_FRAME ();

      /* Move arguments */
      sp -= n;
      for (i = 0; i < n; i++)
	sp[i] = base[i];

      /* Call the program */
      goto vm_call;
    }
  /*
   * Function call
   */
  if (!SCM_FALSEP (scm_procedure_p (program)))
    {
      POP_LIST (nargs);
      *sp = scm_apply (program, *sp, SCM_EOL);
      program = SCM_VM_FRAME_PROGRAM (fp);
      goto vm_return;
    }
  /*
   * Continuation call
   */
  if (SCM_VM_CONT_P (program))
    goto vm_call_cc;

  goto vm_error_wrong_type_apply;
}

VM_DEFINE_INSTRUCTION (call_cc, "call/cc", 1)
{
  SYNC ();
  PUSH (capture_vm_cont (vmp));
  POP (program);
  nargs = 1;
  goto vm_call;
}

VM_DEFINE_INSTRUCTION (return, "return", 0)
{
  SCM ret;
 vm_return:
  ret = *sp;
  EXIT_HOOK ();
  RETURN_HOOK ();
  FREE_FRAME ();

  /* Cache the last program */
  program = SCM_VM_FRAME_PROGRAM (fp);
  CACHE_PROGRAM ();
  PUSH (ret);
  NEXT;
}


/*
 * Exception handling
 */

VM_DEFINE_INSTRUCTION (raise, "raise", 1)
{
}

VM_DEFINE_INSTRUCTION (catch, "catch", 0)
{
}

VM_DEFINE_INSTRUCTION (stack_catch, "stach_catch", 0)
{
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

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
VM_DEFINE_INSTRUCTION (nop, "nop", 0, 0, 0)
{
  NEXT;
}

VM_DEFINE_INSTRUCTION (halt, "halt", 0, 0, 0)
{
  SCM ret = *sp;
  vp->time += scm_c_get_internal_run_time () - start_time;
  HALT_HOOK ();
  FREE_FRAME ();
  SYNC_ALL ();
  return ret;
}

VM_DEFINE_INSTRUCTION (drop, "drop", 0, 0, 0)
{
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (dup, "dup", 0, 0, 1)
{
  PUSH (*sp);
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

VM_DEFINE_INSTRUCTION (mark, "mark", 0, 0, 1)
{
  PUSH (SCM_UNDEFINED);
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
  PUSH (SCM_MAKINUM ((signed char) FETCH ()));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int8_0, "make-int8:0", 0, 0, 1)
{
  PUSH (SCM_MAKINUM (0));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int8_1, "make-int8:1", 0, 0, 1)
{
  PUSH (SCM_MAKINUM (1));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_int16, "make-int16", 2, 0, 1)
{
  int h = FETCH ();
  int l = FETCH ();
  PUSH (SCM_MAKINUM ((signed short) (h << 8) + l));
  NEXT;
}

VM_DEFINE_INSTRUCTION (make_char8, "make-char8", 1, 0, 1)
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

/* ref */

VM_DEFINE_INSTRUCTION (object_ref, "object-ref", 1, 0, 1)
{
  PUSH (OBJECT_REF (FETCH ()));
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
    e = SCM_CDR (e);
  PUSH (SCM_CAR (e));
  NEXT;
}

VM_DEFINE_INSTRUCTION (variable_ref, "variable-ref", 0, 0, 1)
{
  SCM x = *sp;
  SCM o = VARIABLE_REF (x);
  if (SCM_UNBNDP (o))
    {
      /* Try autoload here */
      err_args = SCM_LIST1 (SCM_CAR (x));
      goto vm_error_unbound;
    }
  *sp = o;
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
    e = SCM_CDR (e);
  SCM_SETCAR (e, *sp);
  DROP ();
  NEXT;
}

VM_DEFINE_INSTRUCTION (variable_set, "variable-set", 0, 1, 0)
{
  VARIABLE_SET (sp[0], sp[1]);
  scm_set_object_property_x (sp[1], scm_sym_name, SCM_CAR (sp[0]));
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

VM_DEFINE_INSTRUCTION (br_if, "br-if", 1, 0, 0)
{
  BR (!SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (br_if_not, "br-if-not", 1, 0, 0)
{
  BR (SCM_FALSEP (*sp));
}

VM_DEFINE_INSTRUCTION (br_if_eq, "br-if-eq", 1, 0, 0)
{
  BR (SCM_EQ_P (sp[0], sp--[1]));
}

VM_DEFINE_INSTRUCTION (br_if_not_eq, "br-if-not-eq", 1, 0, 0)
{
  BR (!SCM_EQ_P (sp[0], sp--[1]));
}

VM_DEFINE_INSTRUCTION (br_if_null, "br-if-null", 1, 0, 0)
{
  BR (SCM_NULLP (*sp));
}

VM_DEFINE_INSTRUCTION (br_if_not_null, "br-if-not-null", 1, 0, 0)
{
  BR (!SCM_NULLP (*sp));
}

VM_DEFINE_INSTRUCTION (jump, "jump", 1, 0, 0)
{
  ip += (signed char) FETCH ();
  NEXT;
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
  POP (program);
  nargs = FETCH ();

 vm_call:
  /*
   * Subprogram call
   */
  if (SCM_PROGRAM_P (program))
    {
      int i;
    vm_call_program:
      CACHE_PROGRAM ();
      INIT_ARGS ();
      NEW_FRAME ();

      /* Init local variables */
      for (i = 0; i < bp->nlocs; i++)
	LOCAL_SET (i, SCM_UNDEFINED);

      /* Create external variables */
      for (i = 0; i < bp->nexts; i++)
	CONS (external, SCM_UNDEFINED, external);

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
      SYNC_BEFORE_GC ();
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
      reinstate_vm_cont (vp, program);
      CACHE_REGISTER ();
      /* We don't need to set the return value here
	 because it is already on the top of the stack. */
      NEXT;
    }

  goto vm_error_wrong_type_apply;
}

VM_DEFINE_INSTRUCTION (tail_call, "tail-call", 1, -1, 1)
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
      SCM *base = sp;

      /* Exit the current frame */
      EXIT_HOOK ();
      FREE_FRAME ();

      /* Move arguments */
      sp -= nargs;
      for (i = 0; i < nargs; i++)
	sp[i] = base[i];

      /* Call the program */
      goto vm_call_program;
    }
  /*
   * Function call
   */
  if (!SCM_FALSEP (scm_procedure_p (program)))
    {
      POP_LIST (nargs);
      SYNC_BEFORE_GC ();
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

VM_DEFINE_INSTRUCTION (call_cc, "call/cc", 1, 1, 1)
{
  SYNC_BEFORE_GC ();
  PUSH (capture_vm_cont (vp));
  POP (program);
  nargs = 1;
  goto vm_call;
}

VM_DEFINE_INSTRUCTION (return, "return", 0, 0, 1)
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
  Local Variables:
  c-file-style: "gnu"
  End:
*/

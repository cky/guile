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

#include "vm-snarf.h"

/*
 * Variable access
 */

#define LOCAL_VAR(OFFSET)	SCM_VM_FRAME_VARIABLE (fp, OFFSET)

#define EXTERNAL_FOCUS(DEPTH)				\
{							\
  int depth = DEPTH;					\
  env = ext;						\
  while (depth-- > 0)					\
   {							\
     VM_ASSERT_LINK (env);				\
     env = SCM_VM_EXTERNAL_LINK (env);			\
   }							\
}

#define EXTERNAL_VAR(OFFSET)	SCM_VM_EXTERNAL_VARIABLE (env, OFFSET)
#define EXTERNAL_VAR0(OFFSET)	SCM_VM_EXTERNAL_VARIABLE (ext, OFFSET)
#define EXTERNAL_VAR1(OFFSET)	SCM_VM_EXTERNAL_VARIABLE (SCM_VM_EXTERNAL_LINK (ext), OFFSET)
#define EXTERNAL_VAR2(OFFSET)	SCM_VM_EXTERNAL_VARIABLE (SCM_VM_EXTERNAL_LINK (SCM_VM_EXTERNAL_LINK (ext)), OFFSET)


/*
 * Basic operations
 */

/* Must be the first instruction! */
SCM_DEFINE_INSTRUCTION (nop, "%nop", INST_NONE)
{
  NEXT;
}

SCM_DEFINE_INSTRUCTION (halt, "%halt", INST_NONE)
{
  SYNC ();
  VM_HALT_HOOK ();
  return ac;
}


/*
 * %push family
 */

SCM_DEFINE_INSTRUCTION (push, "%push", INST_NONE)
{
  PUSH (ac);
  NEXT;
}

SCM_DEFINE_INSTRUCTION (push_list, "%push-list", INST_SCM)
{
  SCM list;
  for (list = FETCH (); SCM_NIMP (list); list = SCM_CDR (list))
    PUSH (SCM_CAR (list));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushc, "%pushc", INST_SCM)
{
  PUSH (FETCH ());
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushl, "%pushl", INST_INUM)
{
  PUSH (LOCAL_VAR (SCM_INUM (FETCH ())));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushl_0, "%pushl:0", INST_NONE)
{
  PUSH (LOCAL_VAR (0));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushl_1, "%pushl:1", INST_NONE)
{
  PUSH (LOCAL_VAR (1));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushe, "%pushe", INST_EXT)
{
  SCM env;
  SCM loc = FETCH ();
  EXTERNAL_FOCUS (SCM_INUM (SCM_CAR (loc)));
  PUSH (EXTERNAL_VAR (SCM_INUM (SCM_CDR (loc))));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushe_0, "%pushe:0", INST_INUM)
{
  PUSH (EXTERNAL_VAR0 (SCM_INUM (FETCH ())));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushe_0_0, "%pushe:0:0", INST_NONE)
{
  PUSH (EXTERNAL_VAR0 (0));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushe_0_1, "%pushe:0:1", INST_NONE)
{
  PUSH (EXTERNAL_VAR0 (1));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushe_1, "%pushe:1", INST_INUM)
{
  PUSH (EXTERNAL_VAR1 (SCM_INUM (FETCH ())));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushe_1_0, "%pushe:1:0", INST_NONE)
{
  PUSH (EXTERNAL_VAR1 (0));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushe_1_1, "%pushe:1:1", INST_NONE)
{
  PUSH (EXTERNAL_VAR1 (1));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pushe_2, "%pushe:2", INST_INUM)
{
  PUSH (EXTERNAL_VAR2 (SCM_INUM (FETCH ())));
  NEXT;
}

SCM_DEFINE_INSTRUCTION (pusht, "%pusht", INST_TOP)
{
  ac = FETCH ();
  VM_ASSERT_BOUND (ac);
  PUSH (VM_VARIABLE_REF (ac));
  NEXT;
}


/*
 * %load family
 */

SCM_DEFINE_INSTRUCTION (load_unspecified, "%load-unspecified", INST_NONE)
{
  RETURN (SCM_UNSPECIFIED);
}

SCM_DEFINE_INSTRUCTION (loadc, "%loadc", INST_SCM)
{
  RETURN (FETCH ());
}

SCM_DEFINE_INSTRUCTION (loadl, "%loadl", INST_INUM)
{
  RETURN (LOCAL_VAR (SCM_INUM (FETCH ())));
}

SCM_DEFINE_INSTRUCTION (loadl_0, "%loadl:0", INST_NONE)
{
  RETURN (LOCAL_VAR (0));
}

SCM_DEFINE_INSTRUCTION (loadl_1, "%loadl:1", INST_NONE)
{
  RETURN (LOCAL_VAR (1));
}

SCM_DEFINE_INSTRUCTION (loade, "%loade", INST_EXT)
{
  SCM env;
  SCM loc = FETCH ();
  EXTERNAL_FOCUS (SCM_INUM (SCM_CAR (loc)));
  RETURN (EXTERNAL_VAR (SCM_INUM (SCM_CDR (loc))));
}

SCM_DEFINE_INSTRUCTION (loade_0, "%loade:0", INST_INUM)
{
  RETURN (EXTERNAL_VAR0 (SCM_INUM (FETCH ())));
}

SCM_DEFINE_INSTRUCTION (loade_0_0, "%loade:0:0", INST_NONE)
{
  RETURN (EXTERNAL_VAR0 (0));
}

SCM_DEFINE_INSTRUCTION (loade_0_1, "%loade:0:1", INST_NONE)
{
  RETURN (EXTERNAL_VAR0 (1));
}

SCM_DEFINE_INSTRUCTION (loade_1, "%loade:1", INST_INUM)
{
  RETURN (EXTERNAL_VAR1 (SCM_INUM (FETCH ())));
}

SCM_DEFINE_INSTRUCTION (loade_1_0, "%loade:1:0", INST_NONE)
{
  RETURN (EXTERNAL_VAR1 (0));
}

SCM_DEFINE_INSTRUCTION (loade_1_1, "%loade:1:1", INST_NONE)
{
  RETURN (EXTERNAL_VAR1 (1));
}

SCM_DEFINE_INSTRUCTION (loade_2, "%loade:2", INST_INUM)
{
  RETURN (EXTERNAL_VAR2 (SCM_INUM (FETCH ())));
}

SCM_DEFINE_INSTRUCTION (loadt, "%loadt", INST_TOP)
{
  ac = FETCH ();
  VM_ASSERT_BOUND (ac);
  RETURN (VM_VARIABLE_REF (ac));
}


/*
 * %save family
 */

SCM_DEFINE_INSTRUCTION (savel, "%savel", INST_INUM)
{
  LOCAL_VAR (SCM_INUM (FETCH ())) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savel_0, "%savel:0", INST_NONE)
{
  LOCAL_VAR (0) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savel_1, "%savel:1", INST_NONE)
{
  LOCAL_VAR (1) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savee, "%savee", INST_EXT)
{
  SCM env;
  SCM loc = FETCH ();
  EXTERNAL_FOCUS (SCM_INUM (SCM_CAR (loc)));
  EXTERNAL_VAR (SCM_INUM (SCM_CDR (loc))) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savee_0, "%savee:0", INST_INUM)
{
  EXTERNAL_VAR0 (SCM_INUM (FETCH ())) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savee_0_0, "%savee:0:0", INST_NONE)
{
  EXTERNAL_VAR0 (0) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savee_0_1, "%savee:0:1", INST_NONE)
{
  EXTERNAL_VAR0 (1) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savee_1, "%savee:1", INST_INUM)
{
  EXTERNAL_VAR1 (SCM_INUM (FETCH ())) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savee_1_0, "%savee:1:0", INST_NONE)
{
  EXTERNAL_VAR1 (0) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savee_1_1, "%savee:1:1", INST_NONE)
{
  EXTERNAL_VAR1 (1) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savee_2, "%savee:2", INST_INUM)
{
  EXTERNAL_VAR2 (SCM_INUM (FETCH ())) = ac;
  NEXT;
}

SCM_DEFINE_INSTRUCTION (savet, "%savet", INST_TOP)
{
  SCM cell = FETCH ();
  scm_set_object_property_x (ac, scm_sym_name, SCM_CAR (cell));
  VM_VARIABLE_SET (cell, ac);
  NEXT;
}


/*
 * branch and jump
 */

SCM_DEFINE_INSTRUCTION (br_if, "%br-if", INST_ADDR)
{
  SCM addr = FETCH (); /* must always fetch */
  if (!SCM_FALSEP (ac))
    pc = SCM_VM_ADDRESS (addr);
  NEXT;
}

SCM_DEFINE_INSTRUCTION (br_if_not, "%br-if-not", INST_ADDR)
{
  SCM addr = FETCH (); /* must always fetch */
  if (SCM_FALSEP (ac))
    pc = SCM_VM_ADDRESS (addr);
  NEXT;
}

SCM_DEFINE_INSTRUCTION (br_if_null, "%br-if-null", INST_ADDR)
{
  SCM addr = FETCH (); /* must always fetch */
  if (SCM_NULLP (ac))
    pc = SCM_VM_ADDRESS (addr);
  NEXT;
}

SCM_DEFINE_INSTRUCTION (br_if_not_null, "%br-if-not-null", INST_ADDR)
{
  SCM addr = FETCH (); /* must always fetch */
  if (!SCM_NULLP (ac))
    pc = SCM_VM_ADDRESS (addr);
  NEXT;
}

SCM_DEFINE_INSTRUCTION (jump, "%jump", INST_ADDR)
{
  pc = SCM_VM_ADDRESS (*pc);
  NEXT;
}


/*
 * Subprogram call
 */

SCM_DEFINE_INSTRUCTION (make_program, "%make-program", INST_CODE)
{
  SYNC (); /* must be called before GC */
  RETURN (SCM_MAKE_PROGRAM (FETCH (), SCM_VM_FRAME_EXTERNAL_LINK (fp)));
}

/* Before:
   ac    = program
   pc[0] = the number of arguments

   After:
   pc = program's address
*/
SCM_DEFINE_INSTRUCTION (call, "%call", INST_INUM)
{
  nargs = SCM_INUM (FETCH ());	/* the number of arguments */

 vm_call:
  /*
   * Subprogram call
   */
  if (SCM_PROGRAM_P (ac))
    {
      /* Create a new frame */
      SCM *last_fp = fp;
      SCM *last_sp = sp + nargs;
      VM_NEW_FRAME (fp, ac,
		    SCM_VM_MAKE_ADDRESS (last_fp),
		    SCM_VM_MAKE_ADDRESS (last_sp),
		    SCM_VM_MAKE_ADDRESS (pc));
      VM_CALL_HOOK ();

      /* Jump to the program */
      pc = SCM_PROGRAM_BASE (ac);
      VM_APPLY_HOOK ();
      NEXT;
    }
  /*
   * Function call
   */
  if (!SCM_FALSEP (scm_procedure_p (ac)))
    {
      /* Construct an argument list */
      SCM list = SCM_EOL;
      POP_LIST (nargs, list);
      RETURN (scm_apply (ac, list, SCM_EOL));
    }
  /*
   * Continuation call
   */
  if (SCM_VM_CONT_P (ac))
    {
    vm_call_cc:
      /* Check the number of arguments */
      if (nargs != 1)
	scm_wrong_num_args (ac);

      /* Reinstate the continuation */
      SCM_VM_REINSTATE_CONT (vmp, ac);
      LOAD ();
      POP (ac); /* return value */
      VM_RETURN_HOOK ();
      NEXT;
    }

  SCM_MISC_ERROR ("Wrong type to apply: ~S", SCM_LIST1 (ac));
}

/* Before:
   ac    = program
   pc[0] = the number of arguments

   After:
   pc = program's address
*/
SCM_DEFINE_INSTRUCTION (tail_call, "%tail-call", INST_INUM)
{
  SCM_TICK;			/* allow interrupt here */
  nargs = SCM_INUM (FETCH ());	/* the number of arguments */

  /*
   * Subprogram call
   */
  if (SCM_PROGRAM_P (ac))
    {
      if (SCM_EQ_P (ac, SCM_VM_FRAME_PROGRAM (fp)))
	/* Tail recursive call */
	{
	  /* Setup arguments */
	  int nvars = SCM_PROGRAM_NVARS (ac); /* the number of local vars */
	  int nreqs = SCM_PROGRAM_NREQS (ac); /* the number of require args */
	  int restp = SCM_PROGRAM_RESTP (ac); /* have a rest argument */
	  VM_FRAME_INIT_ARGS (ac, nreqs, restp);

	  /* Move arguments */
	  nreqs += restp;
	  while (nreqs-- > 0)
	    {
	      SCM obj;
	      POP (obj);
	      SCM_VM_FRAME_VARIABLE (fp, nvars++) = obj;
	    }

	  VM_FRAME_INIT_EXTERNAL_VARIABLES (fp, ac);
	}
      else
	/* Proper tail call */
	{
	  /* FIXME: Must remove the last frame.
	     FIXME: We need to move arguments before that. */
	  SCM *last_fp = fp;
	  VM_NEW_FRAME (fp, ac,
			SCM_VM_FRAME_DYNAMIC_LINK (last_fp),
			SCM_VM_FRAME_STACK_POINTER (last_fp),
			SCM_VM_FRAME_RETURN_ADDRESS (last_fp));
	  VM_CALL_HOOK ();
	}

      /* Jump to the program */
      pc = SCM_PROGRAM_BASE (ac);
      VM_APPLY_HOOK ();
      NEXT;
    }
  /*
   * Function call
   */
  if (!SCM_FALSEP (scm_procedure_p (ac)))
    {
      /* Construct an argument list */
      SCM list = SCM_EOL;
      POP_LIST (nargs, list);
      ac = scm_apply (ac, list, SCM_EOL);
      goto vm_return;
    }
  /*
   * Continuation call
   */
  if (SCM_VM_CONT_P (ac))
    goto vm_call_cc;

  SCM_MISC_ERROR ("Wrong type to apply: ~S", SCM_LIST1 (ac));
}

SCM_DEFINE_INSTRUCTION (return, "%return", INST_NONE)
{
  SCM *last_fp;
 vm_return:
  VM_RETURN_HOOK ();
  last_fp = fp;
  fp = SCM_VM_ADDRESS (SCM_VM_FRAME_DYNAMIC_LINK (last_fp));
  sp = SCM_VM_ADDRESS (SCM_VM_FRAME_STACK_POINTER (last_fp));
  pc = SCM_VM_ADDRESS (SCM_VM_FRAME_RETURN_ADDRESS (last_fp));
  ext = SCM_VM_FRAME_EXTERNAL_LINK (fp);
  NEXT;
}

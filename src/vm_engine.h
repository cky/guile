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
 * VM Options
 */

#undef VM_USE_BOOT_HOOK
#undef VM_USE_HALT_HOOK
#undef VM_USE_NEXT_HOOK
#undef VM_USE_CALL_HOOK
#undef VM_USE_APPLY_HOOK
#undef VM_USE_RETURN_HOOK
#undef VM_INIT_LOCAL_VARIABLES
#undef VM_CHECK_LINK
#undef VM_CHECK_BINDING
#undef VM_CHECK_PROGRAM_COUNTER

#if VM_ENGINE == SCM_VM_REGULAR_ENGINE
#define VM_USE_BOOT_HOOK		0
#define	VM_USE_HALT_HOOK		0
#define	VM_USE_NEXT_HOOK		0
#define	VM_USE_CALL_HOOK		0
#define	VM_USE_APPLY_HOOK		0
#define	VM_USE_RETURN_HOOK		0
#define VM_INIT_LOCAL_VARIABLES		0
#define VM_CHECK_LINK			0
#define VM_CHECK_BINDING		1
#define VM_CHECK_PROGRAM_COUNTER	0
#else
#if VM_ENGINE == SCM_VM_DEBUG_ENGINE
#define VM_USE_BOOT_HOOK		1
#define	VM_USE_HALT_HOOK		1
#define	VM_USE_NEXT_HOOK		1
#define	VM_USE_CALL_HOOK		1
#define	VM_USE_APPLY_HOOK		1
#define	VM_USE_RETURN_HOOK		1
#define VM_INIT_LOCAL_VARIABLES		1
#define VM_CHECK_LINK			1
#define VM_CHECK_BINDING		1
#define	VM_CHECK_PROGRAM_COUNTER	1
#endif
#endif

#undef VM_USE_HOOK
#if VM_USE_BOOT_HOOK || VM_USE_HALT_HOOK || VM_USE_NEXT_HOOK \
    || VM_USE_CALL_HOOK || VM_USE_APPLY_HOOK || VM_USE_RETURN_HOOK
#define VM_USE_HOOK 1
#else
#define VM_USE_HOOK 0
#endif


/*
 * Type checking
 */

#undef VM_ASSERT_LINK
#if VM_CHECK_LINK
#define VM_ASSERT_LINK(OBJ)				\
  if (SCM_FALSEP (OBJ))					\
    SCM_MISC_ERROR ("VM broken link", SCM_EOL)
#else
#define VM_ASSERT_LINK(OBJ)
#endif


/*
 * Top-level variable
 */

#define VM_VARIABLE_REF(VAR)		SCM_CDDR (VAR)
#define VM_VARIABLE_SET(VAR,VAL)	SCM_SETCDR (SCM_CDR (VAR), VAL)

#undef VM_ASSERT_BOUND
#if VM_CHECK_BINDING
#define VM_ASSERT_BOUND(VAR)						\
  if (SCM_UNBNDP (VM_VARIABLE_REF (VAR)))				\
    SCM_MISC_ERROR ("Unbound variable: ~S", SCM_LIST1 (SCM_CADR (VAR)))
#else
#define VM_ASSERT_BOUND(CELL)
#endif


/*
 * Hooks
 */

#undef VM_BOOT_HOOK
#if VM_USE_BOOT_HOOK
#define VM_BOOT_HOOK()	SYNC (); scm_c_run_hook (vmp->boot_hook, hook_args)
#else
#define VM_BOOT_HOOK()
#endif

#undef VM_HALT_HOOK
#if VM_USE_HALT_HOOK
#define VM_HALT_HOOK()	SYNC (); scm_c_run_hook (vmp->halt_hook, hook_args)
#else
#define VM_HALT_HOOK()
#endif

#undef VM_NEXT_HOOK
#if VM_USE_NEXT_HOOK
#define VM_NEXT_HOOK()	SYNC (); scm_c_run_hook (vmp->next_hook, hook_args)
#else
#define VM_NEXT_HOOK()
#endif

#undef VM_CALL_HOOK
#if VM_USE_CALL_HOOK
#define VM_CALL_HOOK()	SYNC (); scm_c_run_hook (vmp->call_hook, hook_args)
#else
#define VM_CALL_HOOK()
#endif

#undef VM_APPLY_HOOK
#if VM_USE_APPLY_HOOK
#define VM_APPLY_HOOK()	SYNC (); scm_c_run_hook (vmp->apply_hook, hook_args)
#else
#define VM_APPLY_HOOK()
#endif

#undef VM_RETURN_HOOK
#if VM_USE_RETURN_HOOK
#define VM_RETURN_HOOK() SYNC (); scm_c_run_hook (vmp->return_hook, hook_args)
#else
#define VM_RETURN_HOOK()
#endif


/*
 * Basic operations
 */

#define LOAD()					\
{						\
  ac = vmp->ac;					\
  pc = vmp->pc;					\
  sp = vmp->sp;					\
  fp = vmp->fp;					\
  stack_base  = vmp->stack_base;		\
  stack_limit = vmp->stack_limit;		\
}

#define SYNC()					\
{						\
  vmp->ac = ac;					\
  vmp->pc = pc;					\
  vmp->sp = sp;					\
  vmp->fp = fp;					\
}

#define FETCH()		*pc++

#define CONS(X,Y,Z)				\
{						\
  SCM cell;					\
  SYNC ();					\
  SCM_NEWCELL (cell);				\
  SCM_SET_CELL_OBJECT_0 (cell, Y);		\
  SCM_SET_CELL_OBJECT_1 (cell, Z);		\
  X = cell;					\
}

#define VM_SETUP_ARGS1() SCM a1 = ac;
#define VM_SETUP_ARGS2() SCM a1, a2; a2 = ac; POP (a1);
#define VM_SETUP_ARGS3() SCM a1, a2, a3; a3 = ac; POP (a2); POP (a1);
#define VM_SETUP_ARGSN() nargs = SCM_INUM (FETCH ());


/*
 * Stack operation
 */

#define PUSH(X)					\
{						\
  if (sp < stack_base)				\
    SCM_MISC_ERROR ("FIXME: Stack overflow", SCM_EOL);	\
  *sp-- = (X);					\
}

#define POP(X)					\
{						\
  if (sp == stack_limit)			\
    SCM_MISC_ERROR ("FIXME: Stack underflow", SCM_EOL);	\
  (X) = *++sp;					\
}

#define POP_LIST(N,L)				\
{						\
  while (N-- > 0)				\
    {						\
      SCM obj;					\
      POP (obj);				\
      CONS (L, obj, L);				\
    }						\
}


/*
 * Frame allocation
 */

/* nargs = the number of arguments */
#define VM_FRAME_INIT_ARGS(PROG,NREQS,RESTP)			\
{								\
  if (RESTP)							\
    /* have a rest argument */					\
    {								\
      SCM list;							\
      if (nargs < NREQS)					\
	scm_wrong_num_args (PROG);				\
								\
      /* Construct the rest argument list */			\
      nargs -= NREQS;	/* the number of rest arguments */	\
      list = SCM_EOL;	/* list of the rest arguments */	\
      POP_LIST (nargs, list);					\
      PUSH (list);						\
    }								\
  else								\
    /* not have a rest argument */				\
    {								\
      if (nargs != NREQS)					\
	scm_wrong_num_args (PROG);				\
    }								\
}

#undef VM_FRAME_INIT_LOCAL_VARIABLES
#if VM_INIT_LOCAL_VARIABLES
/* This is necessary when creating frame objects for debugging */
#define VM_FRAME_INIT_LOCAL_VARIABLES(FP,NVARS)		\
{							\
  int i;						\
  for (i = 0; i < NVARS; i++)				\
    SCM_VM_FRAME_VARIABLE (FP, i) = SCM_UNDEFINED;	\
}
#else
#define VM_FRAME_INIT_LOCAL_VARIABLES(FP,NVARS)
#endif

#define VM_FRAME_INIT_EXTERNAL_VARIABLES(FP,PROG)	\
{							\
  int *exts = SCM_PROGRAM_EXTS (PROG);			\
  if (exts)						\
    {							\
      /* Export variables */				\
      int n = exts[0];					\
      while (n-- > 0)					\
	SCM_VM_EXTERNAL_VARIABLE (ext, n)		\
	  = SCM_VM_FRAME_VARIABLE (FP, exts[n + 1]);	\
    }							\
}

#define VM_NEW_FRAME(FP,PROG,DL,SP,RA)					  \
{									  \
  int nvars = SCM_PROGRAM_NVARS (PROG); /* the number of local vars */	  \
  int nreqs = SCM_PROGRAM_NREQS (PROG); /* the number of required args */ \
  int restp = SCM_PROGRAM_RESTP (PROG); /* have a rest argument or not */ \
  int nexts = SCM_PROGRAM_NEXTS (PROG);	/* the number of external vars */ \
									  \
  VM_FRAME_INIT_ARGS (PROG, nreqs, restp);				  \
									  \
  /* Allocate the new frame */						  \
  if (sp - nvars - SCM_VM_FRAME_DATA_SIZE < stack_base - 1)		  \
    SCM_MISC_ERROR ("FIXME: Stack overflow", SCM_EOL);			  \
  sp -= nvars + SCM_VM_FRAME_DATA_SIZE;					  \
  FP = sp + SCM_VM_FRAME_DATA_SIZE + 1;					  \
									  \
  /* Setup the new external frame */					  \
  if (!SCM_FALSEP (SCM_PROGRAM_ENV (PROG)))				  \
    ext = SCM_PROGRAM_ENV (PROG);	/* Use program's environment */	  \
  if (nexts)								  \
    {									  \
      SCM new = SCM_VM_MAKE_EXTERNAL (nexts); /* new external */	  \
      SCM_VM_EXTERNAL_LINK (new) = ext;					  \
      ext = new;							  \
    }									  \
									  \
  /* Setup the new frame */						  \
  SCM_VM_FRAME_SIZE (FP) = SCM_MAKINUM (nvars);				  \
  SCM_VM_FRAME_PROGRAM (FP) = PROG;					  \
  SCM_VM_FRAME_DYNAMIC_LINK (FP) = DL;					  \
  SCM_VM_FRAME_EXTERNAL_LINK (FP) = ext;				  \
  SCM_VM_FRAME_STACK_POINTER (FP) = SP;					  \
  SCM_VM_FRAME_RETURN_ADDRESS (FP) = RA;				  \
  VM_FRAME_INIT_LOCAL_VARIABLES (FP, nvars);				  \
  VM_FRAME_INIT_EXTERNAL_VARIABLES (FP, PROG);				  \
}


/*
 * Goto next
 */

#undef VM_PROGRAM_COUNTER_CHECK
#if VM_CHECK_PROGRAM_COUNTER
#define VM_PROGRAM_COUNTER_CHECK()					\
{									\
  SCM prog = SCM_VM_FRAME_PROGRAM (fp);					\
  if (pc < SCM_PROGRAM_BASE (prog)					\
      || pc >= (SCM_PROGRAM_BASE (prog) + SCM_PROGRAM_SIZE (prog)))	\
    SCM_MISC_ERROR ("VM accessed invalid program address", SCM_EOL);	\
} 
#else
#define VM_PROGRAM_COUNTER_CHECK()
#endif

#undef VM_GOTO_NEXT
#if HAVE_LABELS_AS_VALUES
#if VM_ENGINE == SCM_VM_DEBUG_ENGINE
#define VM_GOTO_NEXT()		goto *SCM_CODE_TO_DEBUG_ADDR (FETCH ())
#else /* not SCM_VM_DEBUG_ENGINE */
#define VM_GOTO_NEXT()		goto *SCM_CODE_TO_ADDR (FETCH ())
#endif
#else /* not HAVE_LABELS_AS_VALUES */
#define VM_GOTO_NEXT()		goto vm_start
#endif

#define NEXT					\
{						\
  VM_PROGRAM_COUNTER_CHECK ();			\
  VM_NEXT_HOOK ();				\
  VM_GOTO_NEXT ();				\
}

/* Just an abbreviation */
#define RETURN(X)	{ ac = (X); NEXT; }

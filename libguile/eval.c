/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



/* This file is read twice in order to produce debugging versions of
 * scm_ceval and scm_apply.  These functions, scm_deval and
 * scm_dapply, are produced when we define the preprocessor macro
 * DEVAL.  The file is divided into sections which are treated
 * differently with respect to DEVAL.  The heads of these sections are
 * marked with the string "SECTION:".
 */

/* SECTION: This code is compiled once.
 */

#ifndef DEVAL

/* We need this to get the definitions for HAVE_ALLOCA_H, etc.  */
#include "libguile/scmconfig.h"

/* AIX requires this to be the first thing in the file.  The #pragma
   directive is indented so pre-ANSI compilers will ignore it, rather
   than choke on it.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "libguile/_scm.h"
#include "libguile/debug.h"
#include "libguile/dynwind.h"
#include "libguile/alist.h"
#include "libguile/eq.h"
#include "libguile/continuations.h"
#include "libguile/throw.h"
#include "libguile/smob.h"
#include "libguile/macros.h"
#include "libguile/procprop.h"
#include "libguile/hashtab.h"
#include "libguile/hash.h"
#include "libguile/srcprop.h"
#include "libguile/stackchk.h"
#include "libguile/objects.h"
#include "libguile/async.h"
#include "libguile/feature.h"
#include "libguile/modules.h"
#include "libguile/ports.h"
#include "libguile/root.h"
#include "libguile/vectors.h"
#include "libguile/fluids.h"
#include "libguile/values.h"

#include "libguile/validate.h"
#include "libguile/eval.h"



/* The evaluator contains a plethora of EVAL symbols.
 * This is an attempt at explanation.
 *
 * The following macros should be used in code which is read twice
 * (where the choice of evaluator is hard soldered):
 *
 *   SCM_CEVAL is the symbol used within one evaluator to call itself.
 *   Originally, it is defined to scm_ceval, but is redefined to
 *   scm_deval during the second pass.
 *  
 *   SIDEVAL corresponds to SCM_CEVAL, but is used in situations where
 *   only side effects of expressions matter.  All immediates are
 *   ignored.
 *  
 *   SCM_EVALIM is used when it is known that the expression is an
 *   immediate.  (This macro never calls an evaluator.)
 *  
 *   EVALCAR evaluates the car of an expression.
 *  
 *   EVALCELLCAR is like EVALCAR, but is used when it is known that the
 *   car is a lisp cell.
 *
 * The following macros should be used in code which is read once
 * (where the choice of evaluator is dynamic):
 *
 *   SCM_XEVAL takes care of immediates without calling an evaluator.  It
 *   then calls scm_ceval *or* scm_deval, depending on the debugging
 *   mode.
 *  
 *   SCM_XEVALCAR corresponds to EVALCAR, but uses scm_ceval *or* scm_deval
 *   depending on the debugging mode.
 *
 * The main motivation for keeping this plethora is efficiency
 * together with maintainability (=> locality of code).
 */

#define SCM_CEVAL scm_ceval
#define SIDEVAL(x, env) if (SCM_NIMP (x)) SCM_CEVAL((x), (env))

#define EVALCELLCAR(x, env) (SCM_SYMBOLP (SCM_CAR (x)) \
			     ? *scm_lookupcar (x, env, 1) \
			     : SCM_CEVAL (SCM_CAR (x), env))

#define EVALCAR(x, env) (!SCM_CELLP (SCM_CAR (x)) \
			? (SCM_IMP (SCM_CAR (x)) \
			   ? SCM_EVALIM (SCM_CAR (x), env) \
			   : SCM_GLOC_VAL (SCM_CAR (x))) \
			: EVALCELLCAR (x, env))

#define EXTEND_ENV SCM_EXTEND_ENV

#ifdef MEMOIZE_LOCALS

SCM *
scm_ilookup (SCM iloc, SCM env)
{
  register int ir = SCM_IFRAME (iloc);
  register SCM er = env;
  for (; 0 != ir; --ir)
    er = SCM_CDR (er);
  er = SCM_CAR (er);
  for (ir = SCM_IDIST (iloc); 0 != ir; --ir)
    er = SCM_CDR (er);
  if (SCM_ICDRP (iloc))
    return SCM_CDRLOC (er);
  return SCM_CARLOC (SCM_CDR (er));
}
#endif

#ifdef USE_THREADS

/* The Lookup Car Race
    - by Eva Luator

   Memoization of variables and special forms is done while executing
   the code for the first time.  As long as there is only one thread
   everything is fine, but as soon as two threads execute the same
   code concurrently `for the first time' they can come into conflict.

   This memoization includes rewriting variable references into more
   efficient forms and expanding macros.  Furthermore, macro expansion
   includes `compiling' special forms like `let', `cond', etc. into
   tree-code instructions.

   There shouldn't normally be a problem with memoizing local and
   global variable references (into ilocs and glocs), because all
   threads will mutate the code in *exactly* the same way and (if I
   read the C code correctly) it is not possible to observe a half-way
   mutated cons cell.  The lookup procedure can handle this
   transparently without any critical sections.

   It is different with macro expansion, because macro expansion
   happens outside of the lookup procedure and can't be
   undone. Therefore it can't cope with it.  It has to indicate
   failure when it detects a lost race and hope that the caller can
   handle it.  Luckily, it turns out that this is the case.

   An example to illustrate this: Suppose that the follwing form will
   be memoized concurrently by two threads

       (let ((x 12)) x)

   Let's first examine the lookup of X in the body.  The first thread
   decides that it has to find the symbol "x" in the environment and
   starts to scan it.  Then the other thread takes over and actually
   overtakes the first.  It looks up "x" and substitutes an
   appropriate iloc for it.  Now the first thread continues and
   completes its lookup.  It comes to exactly the same conclusions as
   the second one and could - without much ado - just overwrite the
   iloc with the same iloc.

   But let's see what will happen when the race occurs while looking
   up the symbol "let" at the start of the form.  It could happen that
   the second thread interrupts the lookup of the first thread and not
   only substitutes a gloc for it but goes right ahead and replaces it
   with the compiled form (#@let* (x 12) x).  Now, when the first
   thread completes its lookup, it would replace the #@let* with a
   gloc pointing to the "let" binding, effectively reverting the form
   to (let (x 12) x).  This is wrong.  It has to detect that it has
   lost the race and the evaluator has to reconsider the changed form
   completely.

   This race condition could be resolved with some kind of traffic
   light (like mutexes) around scm_lookupcar, but I think that it is
   best to avoid them in this case.  They would serialize memoization
   completely and because lookup involves calling arbitrary Scheme
   code (via the lookup-thunk), threads could be blocked for an
   arbitrary amount of time or even deadlock.  But with the current
   solution a lot of unnecessary work is potentially done. */

/* SCM_LOOKUPCAR1 is was SCM_LOOKUPCAR used to be but is allowed to
   return NULL to indicate a failed lookup due to some race conditions
   between threads.  This only happens when VLOC is the first cell of
   a special form that will eventually be memoized (like `let', etc.)
   In that case the whole lookup is bogus and the caller has to
   reconsider the complete special form.

   SCM_LOOKUPCAR is still there, of course.  It just calls
   SCM_LOOKUPCAR1 and aborts on recieving NULL.  So SCM_LOOKUPCAR
   should only be called when it is known that VLOC is not the first
   pair of a special form.  Otherwise, use SCM_LOOKUPCAR1 and check
   for NULL.  I think I've found the only places where this
   applies. */

#endif /* USE_THREADS */

SCM_SYMBOL (scm_unbound_variable_key, "unbound-variable");

#ifdef USE_THREADS
static SCM *
scm_lookupcar1 (SCM vloc, SCM genv, int check)
#else
SCM *
scm_lookupcar (SCM vloc, SCM genv, int check)
#endif
{
  SCM env = genv;
  register SCM *al, fl, var = SCM_CAR (vloc);
#ifdef MEMOIZE_LOCALS
  register SCM iloc = SCM_ILOC00;
#endif
  for (; SCM_NIMP (env); env = SCM_CDR (env))
    {
      if (!SCM_CONSP (SCM_CAR (env)))
	break;
      al = SCM_CARLOC (env);
      for (fl = SCM_CAR (*al); SCM_NIMP (fl); fl = SCM_CDR (fl))
	{
	  if (SCM_NCONSP (fl))
	    {
	      if (SCM_EQ_P (fl, var))
	      {
#ifdef MEMOIZE_LOCALS
#ifdef USE_THREADS
		if (! SCM_EQ_P (SCM_CAR (vloc), var))
		  goto race;
#endif
		SCM_SET_CELL_WORD_0 (vloc, SCM_UNPACK (iloc) + SCM_ICDR);
#endif
		return SCM_CDRLOC (*al);
	      }
	      else
		break;
	    }
	  al = SCM_CDRLOC (*al);
	  if (SCM_EQ_P (SCM_CAR (fl), var))
	    {
#ifdef MEMOIZE_LOCALS
#ifndef SCM_RECKLESS		/* letrec inits to SCM_UNDEFINED */
	      if (SCM_UNBNDP (SCM_CAR (*al)))
		{
		  env = SCM_EOL;
		  goto errout;
		}
#endif
#ifdef USE_THREADS
	      if (!SCM_EQ_P (SCM_CAR (vloc), var))
		goto race;
#endif
	      SCM_SETCAR (vloc, iloc);
#endif
	      return SCM_CARLOC (*al);
	    }
#ifdef MEMOIZE_LOCALS
	  iloc = SCM_PACK (SCM_UNPACK (iloc) + SCM_IDINC);
#endif
	}
#ifdef MEMOIZE_LOCALS
      iloc = SCM_PACK ((~SCM_IDSTMSK) & (SCM_UNPACK(iloc) + SCM_IFRINC));
#endif
    }
  {
    SCM top_thunk, real_var;
    if (SCM_NIMP (env))
      {
	top_thunk = SCM_CAR (env); /* env now refers to a
				      top level env thunk */
	env = SCM_CDR (env);
      }
    else
      top_thunk = SCM_BOOL_F;
    real_var = scm_sym2var (var, top_thunk, SCM_BOOL_F);
    if (SCM_FALSEP (real_var))
      goto errout;

#ifndef SCM_RECKLESS
    if (SCM_NNULLP (env) || SCM_UNBNDP (SCM_VARIABLE_REF (real_var)))
      {
      errout:
	/* scm_everr (vloc, genv,...) */
	if (check)
	  {
	    if (SCM_NULLP (env))
	      scm_error (scm_unbound_variable_key, NULL,
			 "Unbound variable: ~S",
			 scm_cons (var, SCM_EOL), SCM_BOOL_F);
	    else
	      scm_misc_error (NULL, "Damaged environment: ~S",
			      scm_cons (var, SCM_EOL));
	  }
	else 
	  {
	    /* A variable could not be found, but we shall
	       not throw an error. */
	    static SCM undef_object = SCM_UNDEFINED;
	    return &undef_object;
	  }
      }
#endif

#ifdef USE_THREADS
    if (!SCM_EQ_P (SCM_CAR (vloc), var))
      {
	/* Some other thread has changed the very cell we are working
	   on.  In effect, it must have done our job or messed it up
	   completely. */
      race:
	var = SCM_CAR (vloc);
	if (SCM_ITAG3 (var) == scm_tc3_cons_gloc)
	  return SCM_GLOC_VAL_LOC (var);
#ifdef MEMOIZE_LOCALS
	if (SCM_ITAG7 (var) == SCM_ITAG7 (SCM_ILOC00))
	  return scm_ilookup (var, genv);
#endif
	/* We can't cope with anything else than glocs and ilocs.  When
	   a special form has been memoized (i.e. `let' into `#@let') we
	   return NULL and expect the calling function to do the right
	   thing.  For the evaluator, this means going back and redoing
	   the dispatch on the car of the form. */
	return NULL;
      }
#endif /* USE_THREADS */

    SCM_SET_CELL_WORD_0 (vloc, SCM_UNPACK (real_var) + scm_tc3_cons_gloc);
    return SCM_VARIABLE_LOC (real_var);
  }
}

#ifdef USE_THREADS
SCM *
scm_lookupcar (SCM vloc, SCM genv, int check)
{
  SCM *loc = scm_lookupcar1 (vloc, genv, check);
  if (loc == NULL)
    abort ();
  return loc;
}
#endif

#define unmemocar scm_unmemocar

SCM_SYMBOL (sym_three_question_marks, "???");

SCM 
scm_unmemocar (SCM form, SCM env)
{
  SCM c;

  if (SCM_IMP (form))
    return form;
  c = SCM_CAR (form);
  if (SCM_ITAG3 (c) == scm_tc3_cons_gloc)
    {
      SCM sym =
	scm_module_reverse_lookup (scm_env_module (env), SCM_GLOC_VAR (c));
      if (SCM_EQ_P (sym, SCM_BOOL_F))
	sym = sym_three_question_marks;
      SCM_SETCAR (form, sym);
    }
#ifdef MEMOIZE_LOCALS
#ifdef DEBUG_EXTENSIONS
  else if (SCM_ILOCP (c))
    {
      int ir;

      for (ir = SCM_IFRAME (c); ir != 0; --ir)
	env = SCM_CDR (env);
      env = SCM_CAR (SCM_CAR (env));
      for (ir = SCM_IDIST (c); ir != 0; --ir)
	env = SCM_CDR (env);
      SCM_SETCAR (form, SCM_ICDRP (c) ? env : SCM_CAR (env));
    }
#endif
#endif
  return form;
}


SCM
scm_eval_car (SCM pair, SCM env)
{
  return SCM_XEVALCAR (pair, env);
}


/* 
 * The following rewrite expressions and
 * some memoized forms have different syntax 
 */

const char scm_s_expression[] = "missing or extra expression";
const char scm_s_test[] = "bad test";
const char scm_s_body[] = "bad body";
const char scm_s_bindings[] = "bad bindings";
const char scm_s_duplicate_bindings[] = "duplicate bindings";
const char scm_s_variable[] = "bad variable";
const char scm_s_clauses[] = "bad or missing clauses";
const char scm_s_formals[] = "bad formals";
const char scm_s_duplicate_formals[] = "duplicate formals";

SCM_GLOBAL_SYMBOL (scm_sym_dot, ".");
SCM_GLOBAL_SYMBOL (scm_sym_arrow, "=>");
SCM_GLOBAL_SYMBOL (scm_sym_else, "else");
SCM_GLOBAL_SYMBOL (scm_sym_unquote, "unquote");
SCM_GLOBAL_SYMBOL (scm_sym_uq_splicing, "unquote-splicing");

SCM scm_f_apply;

#ifdef DEBUG_EXTENSIONS
SCM_GLOBAL_SYMBOL (scm_sym_enter_frame, "enter-frame");
SCM_GLOBAL_SYMBOL (scm_sym_apply_frame, "apply-frame");
SCM_GLOBAL_SYMBOL (scm_sym_exit_frame, "exit-frame");
SCM_GLOBAL_SYMBOL (scm_sym_trace, "trace");
#endif


/* Check that the body denoted by XORIG is valid and rewrite it into
   its internal form.  The internal form of a body is just the body
   itself, but prefixed with an ISYM that denotes to what kind of
   outer construct this body belongs.  A lambda body starts with
   SCM_IM_LAMBDA, for example, a body of a let starts with SCM_IM_LET,
   etc.  The one exception is a body that belongs to a letrec that has
   been formed by rewriting internal defines: it starts with
   SCM_IM_DEFINE. */

/* XXX - Besides controlling the rewriting of internal defines, the
         additional ISYM could be used for improved error messages.
         This is not done yet.  */

static SCM
scm_m_body (SCM op, SCM xorig, const char *what)
{
  SCM_ASSYNT (scm_ilength (xorig) >= 1, scm_s_expression, what);

  /* Don't add another ISYM if one is present already. */
  if (SCM_ISYMP (SCM_CAR (xorig)))
    return xorig;

  /* Retain possible doc string. */
  if (!SCM_CONSP (SCM_CAR (xorig)))
    {
      if (SCM_NNULLP (SCM_CDR(xorig)))
	return scm_cons (SCM_CAR (xorig),
			 scm_m_body (op, SCM_CDR(xorig), what));
      return xorig;
    }

  return scm_cons (op, xorig);
}

SCM_SYNTAX(s_quote,"quote", scm_makmmacro, scm_m_quote);
SCM_GLOBAL_SYMBOL(scm_sym_quote, s_quote);

SCM 
scm_m_quote (SCM xorig, SCM env)
{
  SCM x = scm_copy_tree (SCM_CDR (xorig));

  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, scm_s_expression, s_quote);
  return scm_cons (SCM_IM_QUOTE, x);
}



SCM_SYNTAX(s_begin, "begin", scm_makmmacro, scm_m_begin);
SCM_GLOBAL_SYMBOL(scm_sym_begin, s_begin);

SCM 
scm_m_begin (SCM xorig, SCM env)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) >= 1, scm_s_expression, s_begin);
  return scm_cons (SCM_IM_BEGIN, SCM_CDR (xorig));
}

SCM_SYNTAX(s_if, "if", scm_makmmacro, scm_m_if);
SCM_GLOBAL_SYMBOL(scm_sym_if, s_if);

SCM 
scm_m_if (SCM xorig, SCM env)
{
  int len = scm_ilength (SCM_CDR (xorig));
  SCM_ASSYNT (len >= 2 && len <= 3, scm_s_expression, "if");
  return scm_cons (SCM_IM_IF, SCM_CDR (xorig));
}


/* Will go into the RnRS module when Guile is factorized.
SCM_SYNTAX(scm_s_set_x,"set!", scm_makmmacro, scm_m_set_x); */
const char scm_s_set_x[] = "set!";
SCM_GLOBAL_SYMBOL(scm_sym_set_x, scm_s_set_x);

SCM 
scm_m_set_x (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (2 == scm_ilength (x), scm_s_expression, scm_s_set_x);
  SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (x)), scm_s_variable, scm_s_set_x);
  return scm_cons (SCM_IM_SET_X, x);
}


SCM_SYNTAX(s_and, "and", scm_makmmacro, scm_m_and);
SCM_GLOBAL_SYMBOL(scm_sym_and, s_and);

SCM 
scm_m_and (SCM xorig, SCM env)
{
  int len = scm_ilength (SCM_CDR (xorig));
  SCM_ASSYNT (len >= 0, scm_s_test, s_and);
  if (len >= 1)
    return scm_cons (SCM_IM_AND, SCM_CDR (xorig));
  else
    return SCM_BOOL_T;
}

SCM_SYNTAX(s_or,"or", scm_makmmacro, scm_m_or);
SCM_GLOBAL_SYMBOL(scm_sym_or,s_or);

SCM 
scm_m_or (SCM xorig, SCM env)
{
  int len = scm_ilength (SCM_CDR (xorig));
  SCM_ASSYNT (len >= 0, scm_s_test, s_or);
  if (len >= 1)
    return scm_cons (SCM_IM_OR, SCM_CDR (xorig));
  else
    return SCM_BOOL_F;
}


SCM_SYNTAX(s_case, "case", scm_makmmacro, scm_m_case);
SCM_GLOBAL_SYMBOL(scm_sym_case, s_case);

SCM 
scm_m_case (SCM xorig, SCM env)
{
  SCM proc, cdrx = scm_list_copy (SCM_CDR (xorig)), x = cdrx;
  SCM_ASSYNT (scm_ilength (x) >= 2, scm_s_clauses, s_case);
  while (SCM_NIMP (x = SCM_CDR (x)))
    {
      proc = SCM_CAR (x);
      SCM_ASSYNT (scm_ilength (proc) >= 2, scm_s_clauses, s_case);
      SCM_ASSYNT (scm_ilength (SCM_CAR (proc)) >= 0
		  || (SCM_EQ_P (scm_sym_else, SCM_CAR (proc)) 
		      && SCM_NULLP (SCM_CDR (x))),
		  scm_s_clauses, s_case);
    }
  return scm_cons (SCM_IM_CASE, cdrx);
}


SCM_SYNTAX(s_cond, "cond", scm_makmmacro, scm_m_cond);
SCM_GLOBAL_SYMBOL(scm_sym_cond, s_cond);


SCM 
scm_m_cond (SCM xorig, SCM env)
{
  SCM arg1, cdrx = scm_list_copy (SCM_CDR (xorig)), x = cdrx;
  int len = scm_ilength (x);
  SCM_ASSYNT (len >= 1, scm_s_clauses, s_cond);
  while (SCM_NIMP (x))
    {
      arg1 = SCM_CAR (x);
      len = scm_ilength (arg1);
      SCM_ASSYNT (len >= 1, scm_s_clauses, s_cond);
      if (SCM_EQ_P (scm_sym_else, SCM_CAR (arg1)))
	{
	  SCM_ASSYNT (SCM_NULLP (SCM_CDR (x)) && len >= 2,
		      "bad ELSE clause", s_cond);
	  SCM_SETCAR (arg1, SCM_BOOL_T);
	}
      if (len >= 2 && SCM_EQ_P (scm_sym_arrow, SCM_CAR (SCM_CDR (arg1))))
	SCM_ASSYNT (3 == len && SCM_NIMP (SCM_CAR (SCM_CDR (SCM_CDR (arg1)))),
		    "bad recipient", s_cond);
      x = SCM_CDR (x);
    }
  return scm_cons (SCM_IM_COND, cdrx);
}

SCM_SYNTAX(s_lambda, "lambda", scm_makmmacro, scm_m_lambda);
SCM_GLOBAL_SYMBOL(scm_sym_lambda, s_lambda);

/* Return true if OBJ is `eq?' to one of the elements of LIST or to the
   cdr of the last cons.  (Thus, LIST is not required to be a proper
   list and when OBJ also found in the improper ending.) */

static int
scm_c_improper_memq (SCM obj, SCM list)
{
  for (; SCM_CONSP (list); list = SCM_CDR (list))
    {
      if (SCM_EQ_P (SCM_CAR (list), obj))
	return 1;
    }
  return SCM_EQ_P (list, obj);
}

SCM 
scm_m_lambda (SCM xorig, SCM env)
{
  SCM proc, x = SCM_CDR (xorig);
  if (scm_ilength (x) < 2)
    goto badforms;
  proc = SCM_CAR (x);
  if (SCM_NULLP (proc))
    goto memlambda;
  if (SCM_EQ_P (SCM_IM_LET, proc))  /* named let */
    goto memlambda;
  if (SCM_IMP (proc))
    goto badforms;
  if (SCM_SYMBOLP (proc))
    goto memlambda;
  if (SCM_NCONSP (proc))
    goto badforms;
  while (SCM_NIMP (proc))
    {
      if (SCM_NCONSP (proc))
	{
	  if (!SCM_SYMBOLP (proc))
	    goto badforms;
	  else
	    goto memlambda;
	}
      if (!SCM_SYMBOLP (SCM_CAR (proc)))
	goto badforms;
      else if (scm_c_improper_memq (SCM_CAR(proc), SCM_CDR(proc)))
	scm_misc_error (s_lambda, scm_s_duplicate_formals, SCM_EOL);
      proc = SCM_CDR (proc);
    }
  if (SCM_NNULLP (proc))
    {
    badforms:
      scm_misc_error (s_lambda, scm_s_formals, SCM_EOL);
    }

 memlambda:
  return scm_cons2 (SCM_IM_LAMBDA, SCM_CAR (x),
		    scm_m_body (SCM_IM_LAMBDA, SCM_CDR (x), s_lambda));
}

SCM_SYNTAX(s_letstar,"let*", scm_makmmacro, scm_m_letstar);
SCM_GLOBAL_SYMBOL(scm_sym_letstar,s_letstar);


SCM 
scm_m_letstar (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig), arg1, proc, vars = SCM_EOL, *varloc = &vars;
  int len = scm_ilength (x);
  SCM_ASSYNT (len >= 2, scm_s_body, s_letstar);
  proc = SCM_CAR (x);
  SCM_ASSYNT (scm_ilength (proc) >= 0, scm_s_bindings, s_letstar);
  while (SCM_NIMP (proc))
    {
      arg1 = SCM_CAR (proc);
      SCM_ASSYNT (2 == scm_ilength (arg1), scm_s_bindings, s_letstar);
      SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (arg1)), scm_s_variable, s_letstar);
      *varloc = scm_cons2 (SCM_CAR (arg1), SCM_CAR (SCM_CDR (arg1)), SCM_EOL);
      varloc = SCM_CDRLOC (SCM_CDR (*varloc));
      proc = SCM_CDR (proc);
    }
  x = scm_cons (vars, SCM_CDR (x));

  return scm_cons2 (SCM_IM_LETSTAR, SCM_CAR (x),
		    scm_m_body (SCM_IM_LETSTAR, SCM_CDR (x), s_letstar));
}

/* DO gets the most radically altered syntax
   (do ((<var1> <init1> <step1>)
   (<var2> <init2>)
   ... )
   (<test> <return>)
   <body>)
   ;; becomes
   (do_mem (varn ... var2 var1)
   (<init1> <init2> ... <initn>)
   (<test> <return>)
   (<body>)
   <step1> <step2> ... <stepn>) ;; missing steps replaced by var
   */

SCM_SYNTAX(s_do, "do", scm_makmmacro, scm_m_do);
SCM_GLOBAL_SYMBOL(scm_sym_do, s_do);

SCM 
scm_m_do (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig), arg1, proc;
  SCM vars = SCM_EOL, inits = SCM_EOL, steps = SCM_EOL;
  SCM *initloc = &inits, *steploc = &steps;
  int len = scm_ilength (x);
  SCM_ASSYNT (len >= 2, scm_s_test, "do");
  proc = SCM_CAR (x);
  SCM_ASSYNT (scm_ilength (proc) >= 0, scm_s_bindings, "do");
  while (SCM_NIMP(proc))
    {
      arg1 = SCM_CAR (proc);
      len = scm_ilength (arg1);
      SCM_ASSYNT (2 == len || 3 == len, scm_s_bindings, "do");
      SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (arg1)), scm_s_variable, "do");
      /* vars reversed here, inits and steps reversed at evaluation */
      vars = scm_cons (SCM_CAR (arg1), vars);	/* variable */
      arg1 = SCM_CDR (arg1);
      *initloc = scm_cons (SCM_CAR (arg1), SCM_EOL);	/* init */
      initloc = SCM_CDRLOC (*initloc);
      arg1 = SCM_CDR (arg1);
      *steploc = scm_cons (SCM_IMP (arg1) ? SCM_CAR (vars) : SCM_CAR (arg1), SCM_EOL);	/* step */
      steploc = SCM_CDRLOC (*steploc);
      proc = SCM_CDR (proc);
    }
  x = SCM_CDR (x);
  SCM_ASSYNT (scm_ilength (SCM_CAR (x)) >= 1, scm_s_test, "do");
  x = scm_cons2 (SCM_CAR (x), SCM_CDR (x), steps);
  x = scm_cons2 (vars, inits, x);
  return scm_cons (SCM_IM_DO, x);
}

/* evalcar is small version of inline EVALCAR when we don't care about
 * speed
 */
#define evalcar scm_eval_car


static SCM iqq (SCM form, SCM env, int depth);

SCM_SYNTAX(s_quasiquote, "quasiquote", scm_makacro, scm_m_quasiquote);
SCM_GLOBAL_SYMBOL(scm_sym_quasiquote, s_quasiquote);

SCM 
scm_m_quasiquote (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (scm_ilength (x) == 1, scm_s_expression, s_quasiquote);
  return iqq (SCM_CAR (x), env, 1);
}


static SCM 
iqq (SCM form, SCM env, int depth)
{
  SCM tmp;
  int edepth = depth;
  if (SCM_IMP (form))
    return form;
  if (SCM_VECTORP (form))
    {
      long i = SCM_VECTOR_LENGTH (form);
      SCM *data = SCM_VELTS (form);
      tmp = SCM_EOL;
      for (; --i >= 0;)
	tmp = scm_cons (data[i], tmp);
      return scm_vector (iqq (tmp, env, depth));
    }
  if (!SCM_CONSP (form)) 
    return form;
  tmp = SCM_CAR (form);
  if (SCM_EQ_P (scm_sym_quasiquote, tmp))
    {
      depth++;
      goto label;
    }
  if (SCM_EQ_P (scm_sym_unquote, tmp))
    {
      --depth;
    label:
      form = SCM_CDR (form);
      SCM_ASSERT (SCM_ECONSP (form) && SCM_NULLP (SCM_CDR (form)),
                  form, SCM_ARG1, s_quasiquote);
      if (0 == depth)
	return evalcar (form, env);
      return scm_cons2 (tmp, iqq (SCM_CAR (form), env, depth), SCM_EOL);
    }
  if (SCM_CONSP (tmp) && (SCM_EQ_P (scm_sym_uq_splicing, SCM_CAR (tmp))))
    {
      tmp = SCM_CDR (tmp);
      if (0 == --edepth)
	return scm_append (scm_cons2 (evalcar (tmp, env), iqq (SCM_CDR (form), env, depth), SCM_EOL));
    }
  return scm_cons (iqq (SCM_CAR (form), env, edepth), iqq (SCM_CDR (form), env, depth));
}

/* Here are acros which return values rather than code. */

SCM_SYNTAX (s_delay, "delay", scm_makmmacro, scm_m_delay);
SCM_GLOBAL_SYMBOL (scm_sym_delay, s_delay);

SCM 
scm_m_delay (SCM xorig, SCM env)
{
  SCM_ASSYNT (scm_ilength (xorig) == 2, scm_s_expression, s_delay);
  return scm_cons2 (SCM_IM_DELAY, SCM_EOL, SCM_CDR (xorig));
}


SCM_SYNTAX(s_define, "define", scm_makmmacro, scm_m_define);
SCM_GLOBAL_SYMBOL(scm_sym_define, s_define);

SCM 
scm_m_define (SCM x, SCM env)
{
  SCM proc, arg1 = x;
  x = SCM_CDR (x);
  SCM_ASSYNT (scm_ilength (x) >= 2, scm_s_expression, s_define);
  proc = SCM_CAR (x);
  x = SCM_CDR (x);
  while (SCM_CONSP (proc))
    {				/* nested define syntax */
      x = scm_cons (scm_cons2 (scm_sym_lambda, SCM_CDR (proc), x), SCM_EOL);
      proc = SCM_CAR (proc);
    }
  SCM_ASSYNT (SCM_SYMBOLP (proc), scm_s_variable, s_define);
  SCM_ASSYNT (1 == scm_ilength (x), scm_s_expression, s_define);
  if (SCM_TOP_LEVEL (env))
    {
      x = evalcar (x, env);
#ifdef DEBUG_EXTENSIONS
      if (SCM_REC_PROCNAMES_P && SCM_NIMP (x))
	{
	  arg1 = x;
	proc:
	  if (SCM_CLOSUREP (arg1)
	      /* Only the first definition determines the name. */
	      && SCM_FALSEP (scm_procedure_property (arg1, scm_sym_name)))
	    scm_set_procedure_property_x (arg1, scm_sym_name, proc);
	  else if (SCM_MACROP (arg1)
		   /* Dirk::FIXME: Does the following test make sense? */
		   && !SCM_EQ_P (SCM_MACRO_CODE (arg1), arg1))
	    {
	      arg1 = SCM_MACRO_CODE (arg1);
	      goto proc;
	    }
	}
#endif
      arg1 = scm_sym2var (proc, scm_env_top_level (env), SCM_BOOL_T);
      SCM_VARIABLE_SET (arg1, x);
#ifdef SICP
      return scm_cons2 (scm_sym_quote, proc, SCM_EOL);
#else
      return SCM_UNSPECIFIED;
#endif
    }
  return scm_cons2 (SCM_IM_DEFINE, proc, x);
}

/* end of acros */

static SCM
scm_m_letrec1 (SCM op, SCM imm, SCM xorig, SCM env)
{
  SCM cdrx = SCM_CDR (xorig);	/* locally mutable version of form */
  char *what = SCM_SYMBOL_CHARS (SCM_CAR (xorig));
  SCM x = cdrx, proc, arg1;	/* structure traversers */
  SCM vars = SCM_EOL, inits = SCM_EOL, *initloc = &inits;

  proc = SCM_CAR (x);
  SCM_ASSYNT (scm_ilength (proc) >= 1, scm_s_bindings, what);
  do
    {
      /* vars scm_list reversed here, inits reversed at evaluation */
      arg1 = SCM_CAR (proc);
      SCM_ASSYNT (2 == scm_ilength (arg1), scm_s_bindings, what);
      SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (arg1)), scm_s_variable, what);
      if (scm_c_improper_memq (SCM_CAR (arg1), vars))
	scm_misc_error (what, scm_s_duplicate_bindings, SCM_EOL);
      vars = scm_cons (SCM_CAR (arg1), vars);
      *initloc = scm_cons (SCM_CAR (SCM_CDR (arg1)), SCM_EOL);
      initloc = SCM_CDRLOC (*initloc);
    }
  while (SCM_NIMP (proc = SCM_CDR (proc)));

  return scm_cons2 (op, vars,
		    scm_cons (inits, scm_m_body (imm, SCM_CDR (x), what)));
}

SCM_SYNTAX(s_letrec, "letrec", scm_makmmacro, scm_m_letrec);
SCM_GLOBAL_SYMBOL(scm_sym_letrec, s_letrec);

SCM 
scm_m_letrec (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (scm_ilength (x) >= 2, scm_s_body, s_letrec);
  
  if (SCM_NULLP (SCM_CAR (x)))   /* null binding, let* faster */
    return scm_m_letstar (scm_cons2 (SCM_CAR (xorig), SCM_EOL,
				     scm_m_body (SCM_IM_LETREC,
						 SCM_CDR (x),
						 s_letrec)),
			  env);
  else
    return scm_m_letrec1 (SCM_IM_LETREC, SCM_IM_LETREC, xorig, env);
}

SCM_SYNTAX(s_let, "let", scm_makmmacro, scm_m_let);
SCM_GLOBAL_SYMBOL(scm_sym_let, s_let);

SCM 
scm_m_let (SCM xorig, SCM env)
{
  SCM cdrx = SCM_CDR (xorig);	/* locally mutable version of form */
  SCM x = cdrx, proc, arg1, name;	/* structure traversers */
  SCM vars = SCM_EOL, inits = SCM_EOL, *varloc = &vars, *initloc = &inits;

  SCM_ASSYNT (scm_ilength (x) >= 2, scm_s_body, s_let);
  proc = SCM_CAR (x);
  if (SCM_NULLP (proc)
      || (SCM_CONSP (proc)
	  && SCM_CONSP (SCM_CAR (proc)) && SCM_NULLP (SCM_CDR (proc))))
    {
      /* null or single binding, let* is faster */
      return scm_m_letstar (scm_cons2 (SCM_CAR (xorig), proc,
				       scm_m_body (SCM_IM_LET,
						   SCM_CDR (x),
						   s_let)),
			    env);
    }

  SCM_ASSYNT (SCM_NIMP (proc), scm_s_bindings, s_let);
  if (SCM_CONSP (proc))
    {
      /* plain let, proc is <bindings> */
      return scm_m_letrec1 (SCM_IM_LET, SCM_IM_LET, xorig, env);
    }

  if (!SCM_SYMBOLP (proc))
    scm_misc_error (s_let, scm_s_bindings, SCM_EOL);	/* bad let */
  name = proc;			/* named let, build equiv letrec */
  x = SCM_CDR (x);
  SCM_ASSYNT (scm_ilength (x) >= 2, scm_s_body, s_let);
  proc = SCM_CAR (x);		/* bindings list */
  SCM_ASSYNT (scm_ilength (proc) >= 0, scm_s_bindings, s_let);
  while (SCM_NIMP (proc))
    {				/* vars and inits both in order */
      arg1 = SCM_CAR (proc);
      SCM_ASSYNT (2 == scm_ilength (arg1), scm_s_bindings, s_let);
      SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (arg1)), scm_s_variable, s_let);
      *varloc = scm_cons (SCM_CAR (arg1), SCM_EOL);
      varloc = SCM_CDRLOC (*varloc);
      *initloc = scm_cons (SCM_CAR (SCM_CDR (arg1)), SCM_EOL);
      initloc = SCM_CDRLOC (*initloc);
      proc = SCM_CDR (proc);
    }

  proc = scm_cons2 (scm_sym_lambda, vars,
		    scm_m_body (SCM_IM_LET, SCM_CDR (x), "let"));
  proc = scm_cons2 (scm_sym_let, scm_cons (scm_cons2 (name, proc, SCM_EOL),
					 SCM_EOL),
		    scm_acons (name, inits, SCM_EOL));
  return scm_m_letrec1 (SCM_IM_LETREC, SCM_IM_LET, proc, env);
}


SCM_SYNTAX (s_atapply,"@apply", scm_makmmacro, scm_m_apply);
SCM_GLOBAL_SYMBOL (scm_sym_atapply, s_atapply);
SCM_GLOBAL_SYMBOL (scm_sym_apply, s_atapply + 1);

SCM 
scm_m_apply (SCM xorig, SCM env)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 2, scm_s_expression, s_atapply);
  return scm_cons (SCM_IM_APPLY, SCM_CDR (xorig));
}


SCM_SYNTAX(s_atcall_cc,"@call-with-current-continuation", scm_makmmacro, scm_m_cont);
SCM_GLOBAL_SYMBOL(scm_sym_atcall_cc,s_atcall_cc);


SCM 
scm_m_cont (SCM xorig, SCM env)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1,
	      scm_s_expression, s_atcall_cc);
  return scm_cons (SCM_IM_CONT, SCM_CDR (xorig));
}

/* Multi-language support */

SCM_GLOBAL_SYMBOL (scm_lisp_nil, "nil");
SCM_GLOBAL_SYMBOL (scm_lisp_t, "t");

SCM_SYNTAX (s_nil_cond, "nil-cond", scm_makmmacro, scm_m_nil_cond);

SCM
scm_m_nil_cond (SCM xorig, SCM env)
{
  int len = scm_ilength (SCM_CDR (xorig));
  SCM_ASSYNT (len >= 1 && (len & 1) == 1, scm_s_expression, "nil-cond");
  return scm_cons (SCM_IM_NIL_COND, SCM_CDR (xorig));
}

SCM_SYNTAX (s_nil_ify, "nil-ify", scm_makmmacro, scm_m_nil_ify);

SCM
scm_m_nil_ify (SCM xorig, SCM env)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, scm_s_expression, "nil-ify");
  return scm_cons (SCM_IM_NIL_IFY, SCM_CDR (xorig));
}

SCM_SYNTAX (s_t_ify, "t-ify", scm_makmmacro, scm_m_t_ify);

SCM
scm_m_t_ify (SCM xorig, SCM env)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, scm_s_expression, "t-ify");
  return scm_cons (SCM_IM_T_IFY, SCM_CDR (xorig));
}

SCM_SYNTAX (s_0_cond, "0-cond", scm_makmmacro, scm_m_0_cond);

SCM
scm_m_0_cond (SCM xorig, SCM env)
{
  int len = scm_ilength (SCM_CDR (xorig));
  SCM_ASSYNT (len >= 1 && (len & 1) == 1, scm_s_expression, "0-cond");
  return scm_cons (SCM_IM_0_COND, SCM_CDR (xorig));
}

SCM_SYNTAX (s_0_ify, "0-ify", scm_makmmacro, scm_m_0_ify);

SCM
scm_m_0_ify (SCM xorig, SCM env)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, scm_s_expression, "0-ify");
  return scm_cons (SCM_IM_0_IFY, SCM_CDR (xorig));
}

SCM_SYNTAX (s_1_ify, "1-ify", scm_makmmacro, scm_m_1_ify);

SCM
scm_m_1_ify (SCM xorig, SCM env)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, scm_s_expression, "1-ify");
  return scm_cons (SCM_IM_1_IFY, SCM_CDR (xorig));
}

SCM_SYNTAX (s_atfop, "@fop", scm_makmmacro, scm_m_atfop);

SCM
scm_m_atfop (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig), var;
  SCM_ASSYNT (scm_ilength (x) >= 1, scm_s_expression, "@fop");
  var = scm_symbol_fref (SCM_CAR (x));
  SCM_ASSYNT (SCM_VARIABLEP (var),
	      "Symbol's function definition is void", NULL);
  SCM_SET_CELL_WORD_0 (x, SCM_UNPACK (var) + scm_tc3_cons_gloc);
  return x;
}

SCM_SYNTAX (s_atbind, "@bind", scm_makmmacro, scm_m_atbind);

SCM
scm_m_atbind (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (scm_ilength (x) > 1, scm_s_expression, "@bind");

  if (SCM_IMP (env))
    env = SCM_BOOL_F;
  else
    {
      while (SCM_NIMP (SCM_CDR (env)))
	env = SCM_CDR (env);
      env = SCM_CAR (env);
      if (SCM_CONSP (env))
	env = SCM_BOOL_F;
    }
  
  x = SCM_CAR (x);
  while (SCM_NIMP (x))
    {
      SCM_SET_CELL_WORD_0 (x, SCM_UNPACK (scm_sym2var (SCM_CAR (x), env, SCM_BOOL_T)) + scm_tc3_cons_gloc);
      x = SCM_CDR (x);
    }
  return scm_cons (SCM_IM_BIND, SCM_CDR (xorig));
}

SCM_SYNTAX (s_at_call_with_values, "@call-with-values", scm_makmmacro, scm_m_at_call_with_values);
SCM_GLOBAL_SYMBOL(scm_sym_at_call_with_values, s_at_call_with_values);

SCM
scm_m_at_call_with_values (SCM xorig, SCM env)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 2,
	      scm_s_expression, s_at_call_with_values);
  return scm_cons (SCM_IM_CALL_WITH_VALUES, SCM_CDR (xorig));
}

SCM
scm_m_expand_body (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig), defs = SCM_EOL;
  char *what = SCM_ISYMCHARS (SCM_CAR (xorig)) + 2;

  while (SCM_NIMP (x))
    {
      SCM form = SCM_CAR (x);
      if (!SCM_CONSP (form))
	break;
      if (!SCM_SYMBOLP (SCM_CAR (form)))
	break;

      form = scm_macroexp (scm_cons_source (form,
					    SCM_CAR (form),
					    SCM_CDR (form)),
			   env);

      if (SCM_EQ_P (SCM_IM_DEFINE, SCM_CAR (form)))
	{
	  defs = scm_cons (SCM_CDR (form), defs);
	  x = SCM_CDR (x);
	}
      else if (!SCM_IMP (defs))
	{
	  break;
	}
      else if (SCM_EQ_P (SCM_IM_BEGIN, SCM_CAR (form)))
	{
	  x = scm_append (scm_cons2 (SCM_CDR (form), SCM_CDR (x), SCM_EOL));
	}
      else
	{
	  x = scm_cons (form, SCM_CDR (x));
	  break;
	}
    }

  SCM_ASSYNT (SCM_NIMP (x), scm_s_body, what);
  if (SCM_NIMP (defs))
    {
      x = scm_cons (scm_m_letrec1 (SCM_IM_LETREC,
				   SCM_IM_DEFINE,
				   scm_cons2 (scm_sym_define, defs, x),
				   env),
		    SCM_EOL);
    }

  SCM_DEFER_INTS;
  SCM_SETCAR (xorig, SCM_CAR (x));
  SCM_SETCDR (xorig, SCM_CDR (x));
  SCM_ALLOW_INTS;

  return xorig;
}

SCM
scm_macroexp (SCM x, SCM env)
{
  SCM res, proc, orig_sym;

  /* Don't bother to produce error messages here.  We get them when we
     eventually execute the code for real. */

 macro_tail:
  orig_sym = SCM_CAR (x);
  if (!SCM_SYMBOLP (orig_sym))
    return x;

#ifdef USE_THREADS
  {
    SCM *proc_ptr = scm_lookupcar1 (x, env, 0);
    if (proc_ptr == NULL)
      {
	/* We have lost the race. */
	goto macro_tail;
      }
    proc = *proc_ptr;
  }
#else
  proc = *scm_lookupcar (x, env, 0);
#endif
  
  /* Only handle memoizing macros.  `Acros' and `macros' are really
     special forms and should not be evaluated here. */

  if (!SCM_MACROP (proc) || SCM_MACRO_TYPE (proc) != 2)
    return x;

  SCM_SETCAR (x, orig_sym);  /* Undo memoizing effect of lookupcar */
  res = scm_apply (SCM_MACRO_CODE (proc), x, scm_cons (env, scm_listofnull));
  
  if (scm_ilength (res) <= 0)
    res = scm_cons2 (SCM_IM_BEGIN, res, SCM_EOL);
      
  SCM_DEFER_INTS;
  SCM_SETCAR (x, SCM_CAR (res));
  SCM_SETCDR (x, SCM_CDR (res));
  SCM_ALLOW_INTS;

  goto macro_tail;
}

/* scm_unmemocopy takes a memoized expression together with its
 * environment and rewrites it to its original form.  Thus, it is the
 * inversion of the rewrite rules above.  The procedure is not
 * optimized for speed.  It's used in scm_iprin1 when printing the
 * code of a closure, in scm_procedure_source, in display_frame when
 * generating the source for a stackframe in a backtrace, and in
 * display_expression.
 *
 * Unmemoizing is not a realiable process.  You can not in general
 * expect to get the original source back.
 *
 * However, GOOPS currently relies on this for method compilation.
 * This ought to change.
 */

#define SCM_BIT8(x) (127 & SCM_UNPACK (x))

static SCM
unmemocopy (SCM x, SCM env)
{
  SCM ls, z;
#ifdef DEBUG_EXTENSIONS
  SCM p;
#endif
  if (SCM_NCELLP (x) || SCM_NECONSP (x))
    return x;
#ifdef DEBUG_EXTENSIONS
  p = scm_whash_lookup (scm_source_whash, x);
#endif
  switch (SCM_TYP7 (x))
    {
    case SCM_BIT8(SCM_IM_AND):
      ls = z = scm_cons (scm_sym_and, SCM_UNSPECIFIED);
      break;
    case SCM_BIT8(SCM_IM_BEGIN):
      ls = z = scm_cons (scm_sym_begin, SCM_UNSPECIFIED);
      break;
    case SCM_BIT8(SCM_IM_CASE):
      ls = z = scm_cons (scm_sym_case, SCM_UNSPECIFIED);
      break;
    case SCM_BIT8(SCM_IM_COND):
      ls = z = scm_cons (scm_sym_cond, SCM_UNSPECIFIED);
      break;
    case SCM_BIT8(SCM_IM_DO):
      ls = scm_cons (scm_sym_do, SCM_UNSPECIFIED);
      goto transform;
    case SCM_BIT8(SCM_IM_IF):
      ls = z = scm_cons (scm_sym_if, SCM_UNSPECIFIED);
      break;
    case SCM_BIT8(SCM_IM_LET):
      ls = scm_cons (scm_sym_let, SCM_UNSPECIFIED);
      goto transform;
    case SCM_BIT8(SCM_IM_LETREC):
      {
	SCM f, v, e, s;
	ls = scm_cons (scm_sym_letrec, SCM_UNSPECIFIED);
      transform:
	x = SCM_CDR (x);
	/* binding names */
	f = v = SCM_CAR (x);
	x = SCM_CDR (x);
	z = EXTEND_ENV (f, SCM_EOL, env);
	/* inits */
	e = scm_reverse (unmemocopy (SCM_CAR (x),
				     SCM_EQ_P (SCM_CAR (ls), scm_sym_letrec) ? z : env));
	env = z;
	/* increments */
	s = SCM_EQ_P (SCM_CAR (ls), scm_sym_do)
	    ? scm_reverse (unmemocopy (SCM_CDR (SCM_CDR (SCM_CDR (x))), env))
	    : f;
	/* build transformed binding list */
	z = SCM_EOL;
	while (SCM_NIMP (v))
	  {
	    z = scm_acons (SCM_CAR (v),
			   scm_cons (SCM_CAR (e),
				     SCM_EQ_P (SCM_CAR (s), SCM_CAR (v))
				     ? SCM_EOL
				     : scm_cons (SCM_CAR (s), SCM_EOL)),
			   z);
	    v = SCM_CDR (v);
	    e = SCM_CDR (e);
	    s = SCM_CDR (s);
	  }
	z = scm_cons (z, SCM_UNSPECIFIED);
	SCM_SETCDR (ls, z);
	if (SCM_EQ_P (SCM_CAR (ls), scm_sym_do))
	  {
	    x = SCM_CDR (x);
	    /* test clause */
	    SCM_SETCDR (z, scm_cons (unmemocopy (SCM_CAR (x), env),
				     SCM_UNSPECIFIED));
	    z = SCM_CDR (z);
	    x = (SCM) (SCM_CARLOC (SCM_CDR (x)) - 1);
	    /* body forms are now to be found in SCM_CDR (x)
	       (this is how *real* code look like! :) */
	  }
	break;
      }
    case SCM_BIT8(SCM_IM_LETSTAR):
      {
	SCM b, y;
	x = SCM_CDR (x);
	b = SCM_CAR (x);
	y = SCM_EOL;
	if SCM_IMP (b)
	  {
	    env = EXTEND_ENV (SCM_EOL, SCM_EOL, env);
	    goto letstar;
	  }
	y = z = scm_acons (SCM_CAR (b),
			   unmemocar (
	scm_cons (unmemocopy (SCM_CAR (SCM_CDR (b)), env), SCM_EOL), env),
			   SCM_UNSPECIFIED);
	env = EXTEND_ENV (SCM_CAR (b), SCM_BOOL_F, env);
	b = SCM_CDR (SCM_CDR (b));
	if (SCM_IMP (b))
	  {
	    SCM_SETCDR (y, SCM_EOL);
	    ls = scm_cons (scm_sym_let, z = scm_cons (y, SCM_UNSPECIFIED));
	    break;
	  }
	do
	  {
	    SCM_SETCDR (z, scm_acons (SCM_CAR (b),
				      unmemocar (
	    scm_cons (unmemocopy (SCM_CAR (SCM_CDR (b)), env), SCM_EOL), env),
				      SCM_UNSPECIFIED));
	    z = SCM_CDR (z);
	    env = EXTEND_ENV (SCM_CAR (b), SCM_BOOL_F, env);
	    b = SCM_CDR (SCM_CDR (b));
	  }
	while (SCM_NIMP (b));
	SCM_SETCDR (z, SCM_EOL);
      letstar:
	ls = scm_cons (scm_sym_letstar, z = scm_cons (y, SCM_UNSPECIFIED));
	break;
      }
    case SCM_BIT8(SCM_IM_OR):
      ls = z = scm_cons (scm_sym_or, SCM_UNSPECIFIED);
      break;
    case SCM_BIT8(SCM_IM_LAMBDA):
      x = SCM_CDR (x);
      ls = scm_cons (scm_sym_lambda,
		     z = scm_cons (SCM_CAR (x), SCM_UNSPECIFIED));
      env = EXTEND_ENV (SCM_CAR (x), SCM_EOL, env);
      break;
    case SCM_BIT8(SCM_IM_QUOTE):
      ls = z = scm_cons (scm_sym_quote, SCM_UNSPECIFIED);
      break;
    case SCM_BIT8(SCM_IM_SET_X):
      ls = z = scm_cons (scm_sym_set_x, SCM_UNSPECIFIED);
      break;
    case SCM_BIT8(SCM_IM_DEFINE):
      {
	SCM n;
	x = SCM_CDR (x);
	ls = scm_cons (scm_sym_define,
		       z = scm_cons (n = SCM_CAR (x), SCM_UNSPECIFIED));
	if (SCM_NNULLP (env))
	  SCM_SETCAR (SCM_CAR (env), scm_cons (n, SCM_CAR (SCM_CAR (env))));
	break;
      }
    case SCM_BIT8(SCM_MAKISYM (0)):
      z = SCM_CAR (x);
      if (!SCM_ISYMP (z))
	goto unmemo;
      switch (SCM_ISYMNUM (z))
	{
	case (SCM_ISYMNUM (SCM_IM_APPLY)):
	  ls = z = scm_cons (scm_sym_atapply, SCM_UNSPECIFIED);
	  goto loop;
	case (SCM_ISYMNUM (SCM_IM_CONT)):
	  ls = z = scm_cons (scm_sym_atcall_cc, SCM_UNSPECIFIED);
	  goto loop;
	case (SCM_ISYMNUM (SCM_IM_DELAY)):
	  ls = z = scm_cons (scm_sym_delay, SCM_UNSPECIFIED);
	  x = SCM_CDR (x);
	  goto loop;
	case (SCM_ISYMNUM (SCM_IM_CALL_WITH_VALUES)):
	  ls = z = scm_cons (scm_sym_at_call_with_values, SCM_UNSPECIFIED);
	  goto loop;
	default:
	  /* appease the Sun compiler god: */ ;
	}
    unmemo:
    default:
      ls = z = unmemocar (scm_cons (unmemocopy (SCM_CAR (x), env),
				    SCM_UNSPECIFIED),
			  env);
    }
loop:
  while (SCM_CELLP (x = SCM_CDR (x)) && SCM_ECONSP (x))
    {
      if (SCM_ISYMP (SCM_CAR (x)))
	/* skip body markers */
	continue;
      SCM_SETCDR (z, unmemocar (scm_cons (unmemocopy (SCM_CAR (x), env),
					  SCM_UNSPECIFIED),
				env));
      z = SCM_CDR (z);
    }
  SCM_SETCDR (z, x);
#ifdef DEBUG_EXTENSIONS
  if (SCM_NFALSEP (p))
    scm_whash_insert (scm_source_whash, ls, p);
#endif
  return ls;
}


SCM
scm_unmemocopy (SCM x, SCM env)
{
  if (SCM_NNULLP (env))
    /* Make a copy of the lowest frame to protect it from
       modifications by SCM_IM_DEFINE */
    return unmemocopy (x, scm_cons (SCM_CAR (env), SCM_CDR (env)));
  else
    return unmemocopy (x, env);
}

#ifndef SCM_RECKLESS

int 
scm_badargsp (SCM formals, SCM args)
{
  while (SCM_NIMP (formals))
    {
      if (SCM_NCONSP (formals)) 
        return 0;
      if (SCM_IMP(args)) 
        return 1;
      formals = SCM_CDR (formals);
      args = SCM_CDR (args);
    }
  return SCM_NNULLP (args) ? 1 : 0;
}
#endif

static int 
scm_badformalsp (SCM closure, int n)
{
  SCM formals = SCM_CLOSURE_FORMALS (closure);
  while (!SCM_NULLP (formals))
    {
      if (!SCM_CONSP (formals)) 
        return 0;
      if (n == 0) 
        return 1;
      --n;
      formals = SCM_CDR (formals);
    }
  return n;
}


SCM 
scm_eval_args (SCM l, SCM env, SCM proc)
{
  SCM results = SCM_EOL, *lloc = &results, res;
  while (!SCM_IMP (l))
    {
#ifdef SCM_CAUTIOUS
      if (SCM_CONSP (l))
	{
	  if (SCM_IMP (SCM_CAR (l)))
	    res = SCM_EVALIM (SCM_CAR (l), env);
	  else
	    res = EVALCELLCAR (l, env);
	}
      else if (SCM_TYP3 (l) == scm_tc3_cons_gloc)
	{
	  scm_bits_t vcell =
	    SCM_STRUCT_VTABLE_DATA (l) [scm_vtable_index_vcell];
	  if (vcell == 0)
	    res = SCM_CAR (l); /* struct planted in code */
	  else
	    res = SCM_GLOC_VAL (SCM_CAR (l));
	}
      else
	goto wrongnumargs;
#else
      res = EVALCAR (l, env);
#endif
      *lloc = scm_cons (res, SCM_EOL);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
#ifdef SCM_CAUTIOUS
  if (!SCM_NULLP (l))
    {
    wrongnumargs:
      scm_wrong_num_args (proc);
    }
#endif
  return results;
}

SCM
scm_eval_body (SCM code, SCM env)
{
  SCM next;
 again:
  next = code;
  while (SCM_NNULLP (next = SCM_CDR (next)))
    {
      if (SCM_IMP (SCM_CAR (code)))
	{
	  if (SCM_ISYMP (SCM_CAR (code)))
	    {
	      code = scm_m_expand_body (code, env);
	      goto again;
	    }
	}
      else
	SCM_XEVAL (SCM_CAR (code), env);
      code = next;
    }
  return SCM_XEVALCAR (code, env);
}


#endif /* !DEVAL */


/* SECTION: This code is specific for the debugging support.  One
 * branch is read when DEVAL isn't defined, the other when DEVAL is
 * defined.
 */

#ifndef DEVAL

#define SCM_APPLY scm_apply
#define PREP_APPLY(proc, args)
#define ENTER_APPLY
#define RETURN(x) return x;
#ifdef STACK_CHECKING
#ifndef NO_CEVAL_STACK_CHECKING
#define EVAL_STACK_CHECKING
#endif
#endif

#else /* !DEVAL */

#undef SCM_CEVAL
#define SCM_CEVAL scm_deval	/* Substitute all uses of scm_ceval */
#undef SCM_APPLY
#define SCM_APPLY scm_dapply
#undef PREP_APPLY
#define PREP_APPLY(p, l) \
{ ++debug.info; debug.info->a.proc = p; debug.info->a.args = l; }
#undef ENTER_APPLY
#define ENTER_APPLY \
do { \
  SCM_SET_ARGSREADY (debug);\
  if (CHECK_APPLY && SCM_TRAPS_P)\
    if (SCM_APPLY_FRAME_P || (SCM_TRACE_P && PROCTRACEP (proc)))\
      {\
	SCM tmp, tail = SCM_BOOL(SCM_TRACED_FRAME_P (debug)); \
	SCM_SET_TRACED_FRAME (debug); \
	if (SCM_CHEAPTRAPS_P)\
	  {\
	    tmp = scm_make_debugobj (&debug);\
	    scm_ithrow (scm_sym_apply_frame, scm_cons2 (tmp, tail, SCM_EOL), 0);\
 	  }\
	else\
	  {\
            int first;\
	    tmp = scm_make_continuation (&first);\
	    if (first)\
	      scm_ithrow (scm_sym_apply_frame, scm_cons2 (tmp, tail, SCM_EOL), 0);\
	  }\
      }\
} while (0)
#undef RETURN
#define RETURN(e) {proc = (e); goto exit;}
#ifdef STACK_CHECKING
#ifndef EVAL_STACK_CHECKING
#define EVAL_STACK_CHECKING
#endif
#endif

/* scm_ceval_ptr points to the currently selected evaluator.
 * *fixme*: Although efficiency is important here, this state variable
 * should probably not be a global.  It should be related to the
 * current repl.
 */


SCM (*scm_ceval_ptr) (SCM x, SCM env);

/* scm_last_debug_frame contains a pointer to the last debugging
 * information stack frame.  It is accessed very often from the
 * debugging evaluator, so it should probably not be indirectly
 * addressed.  Better to save and restore it from the current root at
 * any stack swaps.
 */

#ifndef USE_THREADS
scm_debug_frame *scm_last_debug_frame;
#endif

/* scm_debug_eframe_size is the number of slots available for pseudo
 * stack frames at each real stack frame.
 */

int scm_debug_eframe_size;

int scm_debug_mode, scm_check_entry_p, scm_check_apply_p, scm_check_exit_p;

int scm_eval_stack;

scm_option scm_eval_opts[] = {
  { SCM_OPTION_INTEGER, "stack", 22000, "Size of thread stacks (in machine words)." }
};

scm_option scm_debug_opts[] = {
  { SCM_OPTION_BOOLEAN, "cheap", 1,
    "*Flyweight representation of the stack at traps." },
  { SCM_OPTION_BOOLEAN, "breakpoints", 0, "*Check for breakpoints." },
  { SCM_OPTION_BOOLEAN, "trace", 0, "*Trace mode." },
  { SCM_OPTION_BOOLEAN, "procnames", 1,
    "Record procedure names at definition." },
  { SCM_OPTION_BOOLEAN, "backwards", 0,
    "Display backtrace in anti-chronological order." },
  { SCM_OPTION_INTEGER, "width", 79, "Maximal width of backtrace." },
  { SCM_OPTION_INTEGER, "indent", 10, "Maximal indentation in backtrace." },
  { SCM_OPTION_INTEGER, "frames", 3,
    "Maximum number of tail-recursive frames in backtrace." },
  { SCM_OPTION_INTEGER, "maxdepth", 1000,
    "Maximal number of stored backtrace frames." },
  { SCM_OPTION_INTEGER, "depth", 20, "Maximal length of printed backtrace." },
  { SCM_OPTION_BOOLEAN, "backtrace", 0, "Show backtrace on error." },
  { SCM_OPTION_BOOLEAN, "debug", 0, "Use the debugging evaluator." },
  { SCM_OPTION_INTEGER, "stack", 20000, "Stack size limit (measured in words; 0 = no check)." }
};

scm_option scm_evaluator_trap_table[] = {
  { SCM_OPTION_BOOLEAN, "traps", 0, "Enable evaluator traps." },
  { SCM_OPTION_BOOLEAN, "enter-frame", 0, "Trap when eval enters new frame." },
  { SCM_OPTION_BOOLEAN, "apply-frame", 0, "Trap when entering apply." },
  { SCM_OPTION_BOOLEAN, "exit-frame", 0, "Trap when exiting eval or apply." }
};

SCM_DEFINE (scm_eval_options_interface, "eval-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the evaluation options. Instead of using\n"
	    "this procedure directly, use the procedures @code{eval-enable},\n"
	    "@code{eval-disable}, @code{eval-set!} and @var{eval-options}.")
#define FUNC_NAME s_scm_eval_options_interface
{
  SCM ans;
  SCM_DEFER_INTS;
  ans = scm_options (setting,
		     scm_eval_opts,
		     SCM_N_EVAL_OPTIONS,
		     FUNC_NAME);
  scm_eval_stack = SCM_EVAL_STACK * sizeof (void *);
  SCM_ALLOW_INTS;
  return ans;
}
#undef FUNC_NAME

SCM_DEFINE (scm_evaluator_traps, "evaluator-traps-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the evaluator trap options.")
#define FUNC_NAME s_scm_evaluator_traps
{
  SCM ans;
  SCM_DEFER_INTS;
  ans = scm_options (setting,
		     scm_evaluator_trap_table,
		     SCM_N_EVALUATOR_TRAPS,
		     FUNC_NAME);
  SCM_RESET_DEBUG_MODE;
  SCM_ALLOW_INTS;
  return ans;
}
#undef FUNC_NAME

SCM
scm_deval_args (SCM l, SCM env, SCM proc, SCM *lloc)
{
  SCM *results = lloc, res;
  while (!SCM_IMP (l))
    {
#ifdef SCM_CAUTIOUS
      if (SCM_CONSP (l))
	{
	  if (SCM_IMP (SCM_CAR (l)))
	    res = SCM_EVALIM (SCM_CAR (l), env);
	  else
	    res = EVALCELLCAR (l, env);
	}
      else if (SCM_TYP3 (l) == scm_tc3_cons_gloc)
	{
	  scm_bits_t vcell =
	    SCM_STRUCT_VTABLE_DATA (l) [scm_vtable_index_vcell];
	  if (vcell == 0)
	    res = SCM_CAR (l); /* struct planted in code */
	  else
	    res = SCM_GLOC_VAL (SCM_CAR (l));
	}
      else
	goto wrongnumargs;
#else
      res = EVALCAR (l, env);
#endif
      *lloc = scm_cons (res, SCM_EOL);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
#ifdef SCM_CAUTIOUS
  if (!SCM_NULLP (l))
    {
    wrongnumargs:
      scm_wrong_num_args (proc);
    }
#endif
  return *results;
}

#endif /* !DEVAL */


/* SECTION: Some local definitions for the evaluator.
 */

/* Update the toplevel environment frame ENV so that it refers to the
   current module.
*/
#define UPDATE_TOPLEVEL_ENV(env) \
  do { \
    SCM p = scm_current_module_lookup_closure (); \
    if (p != SCM_CAR(env)) \
      env = scm_top_level_env (p); \
  } while (0)

#ifndef DEVAL
#define CHECK_EQVISH(A,B) 	(SCM_EQ_P ((A), (B)) || (SCM_NFALSEP (scm_eqv_p ((A), (B)))))
#endif /* DEVAL */

#define BUILTIN_RPASUBR /* Handle rpsubrs and asubrs without calling apply */

/* SECTION: This is the evaluator.  Like any real monster, it has
 * three heads.  This code is compiled twice.
 */

#if 0

SCM 
scm_ceval (SCM x, SCM env)
{}
#endif
#if 0

SCM 
scm_deval (SCM x, SCM env)
{}
#endif

SCM 
SCM_CEVAL (SCM x, SCM env)
{
  union
    {
      SCM *lloc;
      SCM arg1;
   } t;
  SCM proc, arg2, orig_sym;
#ifdef DEVAL
  scm_debug_frame debug;
  scm_debug_info *debug_info_end;
  debug.prev = scm_last_debug_frame;
  debug.status = scm_debug_eframe_size;
  /*
   * The debug.vect contains twice as much scm_debug_info frames as the
   * user has specified with (debug-set! frames <n>).
   *
   * Even frames are eval frames, odd frames are apply frames.
   */
  debug.vect = (scm_debug_info *) alloca (scm_debug_eframe_size
					  * sizeof (debug.vect[0]));
  debug.info = debug.vect;
  debug_info_end = debug.vect + scm_debug_eframe_size;
  scm_last_debug_frame = &debug;
#endif
#ifdef EVAL_STACK_CHECKING
  if (scm_stack_checking_enabled_p
      && SCM_STACK_OVERFLOW_P ((SCM_STACKITEM *) &proc))
    {
#ifdef DEVAL
      debug.info->e.exp = x;
      debug.info->e.env = env;
#endif
      scm_report_stack_overflow ();
    }
#endif
#ifdef DEVAL
  goto start;
#endif
loopnoap:
  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
loop:
#ifdef DEVAL
  SCM_CLEAR_ARGSREADY (debug);
  if (SCM_OVERFLOWP (debug))
    --debug.info;
  /*
   * In theory, this should be the only place where it is necessary to
   * check for space in debug.vect since both eval frames and
   * available space are even.
   *
   * For this to be the case, however, it is necessary that primitive
   * special forms which jump back to `loop', `begin' or some similar
   * label call PREP_APPLY.  A convenient way to do this is to jump to
   * `loopnoap' or `cdrxnoap'.
   */
  else if (++debug.info >= debug_info_end)
    {
      SCM_SET_OVERFLOW (debug);
      debug.info -= 2;
    }
start:
  debug.info->e.exp = x;
  debug.info->e.env = env;
  if (CHECK_ENTRY && SCM_TRAPS_P)
    if (SCM_ENTER_FRAME_P || (SCM_BREAKPOINTS_P && SRCBRKP (x)))
      {
	SCM tail = SCM_BOOL(SCM_TAILRECP (debug));
	SCM_SET_TAILREC (debug);
	if (SCM_CHEAPTRAPS_P)
	  t.arg1 = scm_make_debugobj (&debug);
	else
	  {
	    int first;
	    SCM val = scm_make_continuation (&first);
	    
	    if (first)
	      t.arg1 = val;
	    else
	      {
		x = val;
		if (SCM_IMP (x))
		  {
		    RETURN (x);
		  }
		else
		  /* This gives the possibility for the debugger to
		     modify the source expression before evaluation. */
		  goto dispatch;
	      }
	  }
	scm_ithrow (scm_sym_enter_frame,
		    scm_cons2 (t.arg1, tail,
			       scm_cons (scm_unmemocopy (x, env), SCM_EOL)),
		    0);
      }
#endif
#if defined (USE_THREADS) || defined (DEVAL)
dispatch:
#endif
  SCM_TICK;
  switch (SCM_TYP7 (x))
    {
    case scm_tc7_symbol:
      /* Only happens when called at top level.
       */
      x = scm_cons (x, SCM_UNDEFINED);
      goto retval;

    case SCM_BIT8(SCM_IM_AND):
      x = SCM_CDR (x);
      t.arg1 = x;
      while (SCM_NNULLP (t.arg1 = SCM_CDR (t.arg1)))
	if (SCM_FALSEP (EVALCAR (x, env)))
	  {
	    RETURN (SCM_BOOL_F);
	  }
	else
	  x = t.arg1;
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto carloop;

    case SCM_BIT8(SCM_IM_BEGIN):
    /* (currently unused)
    cdrxnoap: */
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
    /* (currently unused)
    cdrxbegin: */
      x = SCM_CDR (x);

    begin:
      /* If we are on toplevel with a lookup closure, we need to sync
         with the current module. */
      if (SCM_CONSP (env) && !SCM_CONSP (SCM_CAR (env)))
	{
	  t.arg1 = x;
	  UPDATE_TOPLEVEL_ENV (env);
	  while (!SCM_NULLP (t.arg1 = SCM_CDR (t.arg1)))
	    {
	      EVALCAR (x, env);
	      x = t.arg1;
	      UPDATE_TOPLEVEL_ENV (env);
	    }
	  goto carloop;
	}
      else
	goto nontoplevel_begin;

    nontoplevel_cdrxnoap:
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
    nontoplevel_cdrxbegin:
      x = SCM_CDR (x);
    nontoplevel_begin:
      t.arg1 = x;
      while (!SCM_NULLP (t.arg1 = SCM_CDR (t.arg1)))
	{
	  if (SCM_IMP (SCM_CAR (x)))
	    {
	      if (SCM_ISYMP (SCM_CAR (x)))
		{
		  x = scm_m_expand_body (x, env);
		  goto nontoplevel_begin;
		}
	      else
		SCM_EVALIM2 (SCM_CAR (x));
	    }
	  else
	    SCM_CEVAL (SCM_CAR (x), env);
	  x = t.arg1;
	}
      
    carloop:			/* scm_eval car of last form in list */
      if (!SCM_CELLP (SCM_CAR (x)))
	{
	  x = SCM_CAR (x);
	  RETURN (SCM_IMP (x) ? SCM_EVALIM (x, env) : SCM_GLOC_VAL (x))
	}

      if (SCM_SYMBOLP (SCM_CAR (x)))
	{
	retval:
	  RETURN (*scm_lookupcar (x, env, 1))
	}

      x = SCM_CAR (x);
      goto loop;		/* tail recurse */


    case SCM_BIT8(SCM_IM_CASE):
      x = SCM_CDR (x);
      t.arg1 = EVALCAR (x, env);
      while (SCM_NIMP (x = SCM_CDR (x)))
	{
	  proc = SCM_CAR (x);
	  if (SCM_EQ_P (scm_sym_else, SCM_CAR (proc)))
	    {
	      x = SCM_CDR (proc);
	      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	      goto begin;
	    }
	  proc = SCM_CAR (proc);
	  while (SCM_NIMP (proc))
	    {
	      if (CHECK_EQVISH (SCM_CAR (proc), t.arg1))
		{
		  x = SCM_CDR (SCM_CAR (x));
		  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		  goto begin;
		}
	      proc = SCM_CDR (proc);
	    }
	}
      RETURN (SCM_UNSPECIFIED)


    case SCM_BIT8(SCM_IM_COND):
      while (!SCM_IMP (x = SCM_CDR (x)))
	{
	  proc = SCM_CAR (x);
	  t.arg1 = EVALCAR (proc, env);
	  if (SCM_NFALSEP (t.arg1))
	    {
	      x = SCM_CDR (proc);
	      if (SCM_NULLP (x))
		{
		  RETURN (t.arg1)
		}
	      if (!SCM_EQ_P (scm_sym_arrow, SCM_CAR (x)))
		{
		  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		  goto begin;
		}
	      proc = SCM_CDR (x);
	      proc = EVALCAR (proc, env);
	      SCM_ASRTGO (SCM_NIMP (proc), badfun);
	      PREP_APPLY (proc, scm_cons (t.arg1, SCM_EOL));
	      ENTER_APPLY;
	      if (SCM_CLOSUREP(proc) && scm_badformalsp (proc, 1))
		goto umwrongnumargs;
	      goto evap1;
	    }
	}
      RETURN (SCM_UNSPECIFIED)


    case SCM_BIT8(SCM_IM_DO):
      x = SCM_CDR (x);
      proc = SCM_CAR (SCM_CDR (x)); /* inits */
      t.arg1 = SCM_EOL;		/* values */
      while (SCM_NIMP (proc))
	{
	  t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1);
	  proc = SCM_CDR (proc);
	}
      env = EXTEND_ENV (SCM_CAR (x), t.arg1, env);
      x = SCM_CDR (SCM_CDR (x));
      while (proc = SCM_CAR (x), SCM_FALSEP (EVALCAR (proc, env)))
	{
	  for (proc = SCM_CADR (x); SCM_NIMP (proc); proc = SCM_CDR (proc))
	    {
	      t.arg1 = SCM_CAR (proc); /* body */
	      SIDEVAL (t.arg1, env);
	    }
	  for (t.arg1 = SCM_EOL, proc = SCM_CDDR (x);
	       SCM_NIMP (proc);
	       proc = SCM_CDR (proc))
	    t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1); /* steps */
	  env = EXTEND_ENV (SCM_CAR (SCM_CAR (env)), t.arg1, SCM_CDR (env));
	}
      x = SCM_CDR (proc);
      if (SCM_NULLP (x))
	RETURN (SCM_UNSPECIFIED);
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto nontoplevel_begin;


    case SCM_BIT8(SCM_IM_IF):
      x = SCM_CDR (x);
      if (SCM_NFALSEP (EVALCAR (x, env)))
	x = SCM_CDR (x);
      else if (SCM_IMP (x = SCM_CDR (SCM_CDR (x))))
	{
	  RETURN (SCM_UNSPECIFIED);
	}
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto carloop;


    case SCM_BIT8(SCM_IM_LET):
      x = SCM_CDR (x);
      proc = SCM_CAR (SCM_CDR (x));
      t.arg1 = SCM_EOL;
      do
	{
	  t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1);
	}
      while (SCM_NIMP (proc = SCM_CDR (proc)));
      env = EXTEND_ENV (SCM_CAR (x), t.arg1, env);
      x = SCM_CDR (x);
      goto nontoplevel_cdrxnoap;


    case SCM_BIT8(SCM_IM_LETREC):
      x = SCM_CDR (x);
      env = EXTEND_ENV (SCM_CAR (x), scm_undefineds, env);
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      t.arg1 = SCM_EOL;
      do
	{
	  t.arg1 = scm_cons (EVALCAR (proc, env), t.arg1);
	}
      while (SCM_NIMP (proc = SCM_CDR (proc)));
      SCM_SETCDR (SCM_CAR (env), t.arg1);
      goto nontoplevel_cdrxnoap;


    case SCM_BIT8(SCM_IM_LETSTAR):
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      if (SCM_IMP (proc))
	{
	  env = EXTEND_ENV (SCM_EOL, SCM_EOL, env);
	  goto nontoplevel_cdrxnoap;
	}
      do
	{
	  t.arg1 = SCM_CAR (proc);
	  proc = SCM_CDR (proc);
	  env = EXTEND_ENV (t.arg1, EVALCAR (proc, env), env);
	}
      while (SCM_NIMP (proc = SCM_CDR (proc)));
      goto nontoplevel_cdrxnoap;

    case SCM_BIT8(SCM_IM_OR):
      x = SCM_CDR (x);
      t.arg1 = x;
      while (!SCM_NULLP (t.arg1 = SCM_CDR (t.arg1)))
	{
	  x = EVALCAR (x, env);
	  if (!SCM_FALSEP (x))
	    {
	      RETURN (x);
	    }
	  x = t.arg1;
	}
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto carloop;


    case SCM_BIT8(SCM_IM_LAMBDA):
      RETURN (scm_closure (SCM_CDR (x), env));


    case SCM_BIT8(SCM_IM_QUOTE):
      RETURN (SCM_CAR (SCM_CDR (x)));


    case SCM_BIT8(SCM_IM_SET_X):
      x = SCM_CDR (x);
      proc = SCM_CAR (x);
      switch (SCM_ITAG3 (proc))
	{
	case scm_tc3_cons:
	  t.lloc = scm_lookupcar (x, env, 1);
	  break;
	case scm_tc3_cons_gloc:
	  t.lloc = SCM_GLOC_VAL_LOC (proc);
	  break;
#ifdef MEMOIZE_LOCALS
	case scm_tc3_imm24:
	  t.lloc = scm_ilookup (proc, env);
	  break;
#endif
	}
      x = SCM_CDR (x);
      *t.lloc = EVALCAR (x, env);
#ifdef SICP
      RETURN (*t.lloc);
#else
      RETURN (SCM_UNSPECIFIED);
#endif


    case SCM_BIT8(SCM_IM_DEFINE):	/* only for internal defines */
      scm_misc_error (NULL, "Bad define placement", SCM_EOL);

      /* new syntactic forms go here. */
    case SCM_BIT8(SCM_MAKISYM (0)):
      proc = SCM_CAR (x);
      SCM_ASRTGO (SCM_ISYMP (proc), badfun);
      switch SCM_ISYMNUM (proc)
	{
	case (SCM_ISYMNUM (SCM_IM_APPLY)):
	  proc = SCM_CDR (x);
	  proc = EVALCAR (proc, env);
	  SCM_ASRTGO (SCM_NIMP (proc), badfun);
	  if (SCM_CLOSUREP (proc))
	    {
	      SCM argl, tl;
	      PREP_APPLY (proc, SCM_EOL);
	      t.arg1 = SCM_CDR (SCM_CDR (x));
	      t.arg1 = EVALCAR (t.arg1, env);
	    apply_closure:
	      /* Go here to tail-call a closure.  PROC is the closure
	         and T.ARG1 is the list of arguments.  Do not forget to
	         call PREP_APPLY. */
#ifdef DEVAL
	      debug.info->a.args = t.arg1;
#endif
#ifndef SCM_RECKLESS
	      if (scm_badargsp (SCM_CLOSURE_FORMALS (proc), t.arg1))
		goto wrongnumargs;
#endif
	      ENTER_APPLY;
	      /* Copy argument list */
	      if (SCM_IMP (t.arg1))
		argl = t.arg1;
	      else
		{
		  argl = tl = scm_cons (SCM_CAR (t.arg1), SCM_UNSPECIFIED);
		  while (SCM_NIMP (t.arg1 = SCM_CDR (t.arg1))
			 && SCM_CONSP (t.arg1))
		    {
		      SCM_SETCDR (tl, scm_cons (SCM_CAR (t.arg1),
						SCM_UNSPECIFIED));
		      tl = SCM_CDR (tl);
		    }
		  SCM_SETCDR (tl, t.arg1);
		}
	      
	      env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc), argl, SCM_ENV (proc));
	      x = SCM_CODE (proc);
	      goto nontoplevel_cdrxbegin;
	    }
	  proc = scm_f_apply;
	  goto evapply;

	case (SCM_ISYMNUM (SCM_IM_CONT)):
	  {
	    int first;
	    SCM val = scm_make_continuation (&first);

	    if (first)
	      t.arg1 = val;
	    else
	      RETURN (val);
	  }
	  proc = SCM_CDR (x);
	  proc = evalcar (proc, env);
	  SCM_ASRTGO (SCM_NIMP (proc), badfun);
	  PREP_APPLY (proc, scm_cons (t.arg1, SCM_EOL));
	  ENTER_APPLY;
	  if (SCM_CLOSUREP(proc) && scm_badformalsp (proc, 1))
	    goto umwrongnumargs;
	  goto evap1;

	case (SCM_ISYMNUM (SCM_IM_DELAY)):
	  RETURN (scm_makprom (scm_closure (SCM_CDR (x), env)))

	case (SCM_ISYMNUM (SCM_IM_DISPATCH)):
	  proc = SCM_CADR (x); /* unevaluated operands */
	  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	  if (SCM_IMP (proc))
	    arg2 = *scm_ilookup (proc, env);
	  else if (SCM_NCONSP (proc))
	    {
	      if (SCM_NCELLP (proc))
		arg2 = SCM_GLOC_VAL (proc);
	      else
		arg2 = *scm_lookupcar (SCM_CDR (x), env, 1);
	    }
	  else
	    {
	      arg2 = scm_cons (EVALCAR (proc, env), SCM_EOL);
	      t.lloc = SCM_CDRLOC (arg2);
	      while (SCM_NIMP (proc = SCM_CDR (proc)))
		{
		  *t.lloc = scm_cons (EVALCAR (proc, env), SCM_EOL);
		  t.lloc = SCM_CDRLOC (*t.lloc);
		}
	    }
	  
	type_dispatch:
	  /* The type dispatch code is duplicated here
	   * (c.f. objects.c:scm_mcache_compute_cmethod) since that
	   * cuts down execution time for type dispatch to 50%.
	   */
	  {
	    int i, n, end, mask;
	    SCM z = SCM_CDDR (x);
	    n = SCM_INUM (SCM_CAR (z)); /* maximum number of specializers */
	    proc = SCM_CADR (z);

	    if (SCM_NIMP (proc))
	      {
		/* Prepare for linear search */
		mask = -1;
		i = 0;
		end = SCM_VECTOR_LENGTH (proc);
	      }
	    else
	      {
		/* Compute a hash value */
		int hashset = SCM_INUM (proc);
		int j = n;
		mask = SCM_INUM (SCM_CAR (z = SCM_CDDR (z)));
		proc = SCM_CADR (z);
		i = 0;
		t.arg1 = arg2;
		if (SCM_NIMP (t.arg1))
		  do
		    {
		      i += SCM_STRUCT_DATA (scm_class_of (SCM_CAR (t.arg1)))
			   [scm_si_hashsets + hashset];
		      t.arg1 = SCM_CDR (t.arg1);
		    }
		  while (j-- && SCM_NIMP (t.arg1));
		i &= mask;
		end = i;
	      }

	    /* Search for match  */
	    do
	      {
		int j = n;
		z = SCM_VELTS (proc)[i];
		t.arg1 = arg2; /* list of arguments */
		if (SCM_NIMP (t.arg1))
		  do
		    {
		      /* More arguments than specifiers => CLASS != ENV */
		      if (! SCM_EQ_P (scm_class_of (SCM_CAR (t.arg1)), SCM_CAR (z)))
			goto next_method;
		      t.arg1 = SCM_CDR (t.arg1);
		      z = SCM_CDR (z);
		    }
		  while (j-- && SCM_NIMP (t.arg1));
		/* Fewer arguments than specifiers => CAR != ENV */
		if (!(SCM_IMP (SCM_CAR (z)) || SCM_CONSP (SCM_CAR (z))))
		  goto next_method;
	      apply_cmethod:
		env = EXTEND_ENV (SCM_CAR (SCM_CMETHOD_CODE (z)),
				  arg2,
				  SCM_CMETHOD_ENV (z));
		x = SCM_CMETHOD_CODE (z);
		goto nontoplevel_cdrxbegin;
	      next_method:
		i = (i + 1) & mask;
	      } while (i != end);
	    
	    z = scm_memoize_method (x, arg2);
	    goto apply_cmethod;
	  }

	case (SCM_ISYMNUM (SCM_IM_SLOT_REF)):
	  x = SCM_CDR (x);
	  t.arg1 = EVALCAR (x, env);
	  RETURN (SCM_PACK (SCM_STRUCT_DATA (t.arg1) [SCM_INUM (SCM_CADR (x))]))
	  
	case (SCM_ISYMNUM (SCM_IM_SLOT_SET_X)):
	  x = SCM_CDR (x);
	  t.arg1 = EVALCAR (x, env);
	  x = SCM_CDR (x);
	  proc = SCM_CDR (x);
	  SCM_STRUCT_DATA (t.arg1) [SCM_INUM (SCM_CAR (x))]
	    = SCM_UNPACK (EVALCAR (proc, env));
	  RETURN (SCM_UNSPECIFIED)
	  
	case (SCM_ISYMNUM (SCM_IM_NIL_COND)):
	  proc = SCM_CDR (x);
	  while (SCM_NIMP (x = SCM_CDR (proc)))
	    {
	      if (!(SCM_FALSEP (t.arg1 = EVALCAR (proc, env))
		    || SCM_EQ_P (t.arg1, scm_lisp_nil)))
		{
		  if (SCM_EQ_P (SCM_CAR (x), SCM_UNSPECIFIED))
		    RETURN (t.arg1);
		  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		  goto carloop;
		}
	      proc = SCM_CDR (x);
	    }
	  x = proc;
	  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	  goto carloop;

	case (SCM_ISYMNUM (SCM_IM_NIL_IFY)):
	  x = SCM_CDR (x);
	  RETURN ((SCM_FALSEP (proc = EVALCAR (x, env)) || SCM_NULLP (proc))
		   ? scm_lisp_nil
		   : proc)
	    
	case (SCM_ISYMNUM (SCM_IM_T_IFY)):
	  x = SCM_CDR (x);
	  RETURN (SCM_NFALSEP (EVALCAR (x, env)) ? scm_lisp_t : scm_lisp_nil)
	    
	case (SCM_ISYMNUM (SCM_IM_0_COND)):
	  proc = SCM_CDR (x);
	  while (SCM_NIMP (x = SCM_CDR (proc)))
	    {
	      if (!(SCM_FALSEP (t.arg1 = EVALCAR (proc, env))
		    || SCM_EQ_P (t.arg1, SCM_INUM0)))
		{
		  if (SCM_EQ_P (SCM_CAR (x), SCM_UNSPECIFIED))
		    RETURN (t.arg1);
		  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		  goto carloop;
		}
	      proc = SCM_CDR (x);
	    }
	  x = proc;
	  PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	  goto carloop;

	case (SCM_ISYMNUM (SCM_IM_0_IFY)):
	  x = SCM_CDR (x);
	  RETURN (SCM_FALSEP (proc = EVALCAR (x, env))
		  ? SCM_INUM0
		  : proc)
	    
	case (SCM_ISYMNUM (SCM_IM_1_IFY)):
	  x = SCM_CDR (x);
	  RETURN (SCM_NFALSEP (EVALCAR (x, env))
		  ? SCM_MAKINUM (1)
		  : SCM_INUM0)

	case (SCM_ISYMNUM (SCM_IM_BIND)):
	  x = SCM_CDR (x);

	  t.arg1 = SCM_CAR (x);
	  arg2 = SCM_CDAR (env);
	  while (SCM_NIMP (arg2))
	    {
	      proc = SCM_GLOC_VAL (SCM_CAR (t.arg1));
	      SCM_SETCDR (SCM_PACK (SCM_UNPACK (SCM_CAR (t.arg1)) - 1L),
			  SCM_CAR (arg2));
	      SCM_SETCAR (arg2, proc);
	      t.arg1 = SCM_CDR (t.arg1);
	      arg2 = SCM_CDR (arg2);
	    }
	  t.arg1 = SCM_CAR (x);
	  scm_dynwinds = scm_acons (t.arg1, SCM_CDAR (env), scm_dynwinds);
	  
	  arg2 = x = SCM_CDR (x);
	  while (SCM_NNULLP (arg2 = SCM_CDR (arg2)))
	    {
	      SIDEVAL (SCM_CAR (x), env);
	      x = arg2;
	    }
	  proc = EVALCAR (x, env);
	  
	  scm_dynwinds = SCM_CDR (scm_dynwinds);
	  arg2 = SCM_CDAR (env);
	  while (SCM_NIMP (arg2))
	    {
	      SCM_SETCDR (SCM_PACK (SCM_UNPACK (SCM_CAR (t.arg1)) - 1L),
			  SCM_CAR (arg2));
	      t.arg1 = SCM_CDR (t.arg1);
	      arg2 = SCM_CDR (arg2);
	    }

	  RETURN (proc);
	  
	case (SCM_ISYMNUM (SCM_IM_CALL_WITH_VALUES)):
	  {
	    proc = SCM_CDR (x);
	    x = EVALCAR (proc, env);
	    proc = SCM_CDR (proc);
	    proc = EVALCAR (proc, env);
	    t.arg1 = SCM_APPLY (x, SCM_EOL, SCM_EOL);
	    if (SCM_VALUESP (t.arg1))
	      t.arg1 = scm_struct_ref (t.arg1, SCM_INUM0);
	    else
	      t.arg1 = scm_cons (t.arg1, SCM_EOL);
	    if (SCM_CLOSUREP (proc))
	      {
		PREP_APPLY (proc, t.arg1);
		goto apply_closure;
	      }
	    return SCM_APPLY (proc, t.arg1, SCM_EOL);
	  }

	default:
	  goto badfun;
	}

    default:
      proc = x;
    badfun:
      /* scm_everr (x, env,...) */
      scm_misc_error (NULL, "Wrong type to apply: ~S", SCM_LIST1 (proc));
    case scm_tc7_vector:
    case scm_tc7_wvect:
#ifdef HAVE_ARRAYS
    case scm_tc7_bvect:
    case scm_tc7_byvect:
    case scm_tc7_svect:
    case scm_tc7_ivect:
    case scm_tc7_uvect:
    case scm_tc7_fvect:
    case scm_tc7_dvect:
    case scm_tc7_cvect:
#ifdef HAVE_LONG_LONGS
    case scm_tc7_llvect:
#endif
#endif
    case scm_tc7_string:
    case scm_tc7_substring:
    case scm_tc7_smob:
    case scm_tcs_closures:
    case scm_tc7_cclo:
    case scm_tc7_pws:
    case scm_tcs_subrs:
      RETURN (x);

#ifdef MEMOIZE_LOCALS
    case SCM_BIT8(SCM_ILOC00):
      proc = *scm_ilookup (SCM_CAR (x), env);
      SCM_ASRTGO (SCM_NIMP (proc), badfun);
#ifndef SCM_RECKLESS
#ifdef SCM_CAUTIOUS
      goto checkargs;
#endif
#endif
      break;
#endif /* ifdef MEMOIZE_LOCALS */


    case scm_tcs_cons_gloc: {
      scm_bits_t vcell = SCM_STRUCT_VTABLE_DATA (x) [scm_vtable_index_vcell];
      if (vcell == 0) {
	/* This is a struct implanted in the code, not a gloc. */
	RETURN (x);
      } else {
	proc = SCM_GLOC_VAL (SCM_CAR (x));
	SCM_ASRTGO (SCM_NIMP (proc), badfun);
#ifndef SCM_RECKLESS
#ifdef SCM_CAUTIOUS
	goto checkargs;
#endif
#endif
      }
      break;
    }

    case scm_tcs_cons_nimcar:
      orig_sym = SCM_CAR (x);
      if (SCM_SYMBOLP (orig_sym))
	{
#ifdef USE_THREADS
	  t.lloc = scm_lookupcar1 (x, env, 1);
	  if (t.lloc == NULL)
	    {
	      /* we have lost the race, start again. */
	      goto dispatch;
	    }
	  proc = *t.lloc;
#else
	  proc = *scm_lookupcar (x, env, 1);
#endif

	  if (SCM_IMP (proc))
	    {
	      SCM_SETCAR (x, orig_sym);  /* Undo memoizing effect of
					    lookupcar */
	      goto badfun;
	    }
	  if (SCM_MACROP (proc))
	    {
	      SCM_SETCAR (x, orig_sym);  /* Undo memoizing effect of
					    lookupcar */
	    handle_a_macro:
#ifdef DEVAL
	      /* Set a flag during macro expansion so that macro
		 application frames can be deleted from the backtrace. */
	      SCM_SET_MACROEXP (debug);
#endif
	      t.arg1 = SCM_APPLY (SCM_MACRO_CODE (proc), x,
				  scm_cons (env, scm_listofnull));

#ifdef DEVAL
	      SCM_CLEAR_MACROEXP (debug);
#endif
	      switch (SCM_MACRO_TYPE (proc))
		{
		case 2:
		  if (scm_ilength (t.arg1) <= 0)
		    t.arg1 = scm_cons2 (SCM_IM_BEGIN, t.arg1, SCM_EOL);
#ifdef DEVAL
		  if (!SCM_CLOSUREP (SCM_MACRO_CODE (proc)))
		    {
		      SCM_DEFER_INTS;
		      SCM_SETCAR (x, SCM_CAR (t.arg1));
		      SCM_SETCDR (x, SCM_CDR (t.arg1));
		      SCM_ALLOW_INTS;
		      goto dispatch;
		    }
		  /* Prevent memoizing of debug info expression. */
		  debug.info->e.exp = scm_cons_source (debug.info->e.exp,
						       SCM_CAR (x),
						       SCM_CDR (x));
#endif
		  SCM_DEFER_INTS;
		  SCM_SETCAR (x, SCM_CAR (t.arg1));
		  SCM_SETCDR (x, SCM_CDR (t.arg1));
		  SCM_ALLOW_INTS;
		  goto loopnoap;
		case 1:
		  if (SCM_NIMP (x = t.arg1))
		    goto loopnoap;
		case 0:
		  RETURN (t.arg1);
		}
	    }
	}
      else
	proc = SCM_CEVAL (SCM_CAR (x), env);
      SCM_ASRTGO (!SCM_IMP (proc), badfun);
#ifndef SCM_RECKLESS
#ifdef SCM_CAUTIOUS
    checkargs:
#endif
      if (SCM_CLOSUREP (proc))
	{
	  arg2 = SCM_CLOSURE_FORMALS (proc);
	  t.arg1 = SCM_CDR (x);
	  while (!SCM_NULLP (arg2))
	    {
	      if (!SCM_CONSP (arg2))
		goto evapply;
	      if (SCM_IMP (t.arg1))
		goto umwrongnumargs;
	      arg2 = SCM_CDR (arg2);
	      t.arg1 = SCM_CDR (t.arg1);
	    }
	  if (!SCM_NULLP (t.arg1))
	    goto umwrongnumargs;
	}
      else if (SCM_MACROP (proc))
	goto handle_a_macro;
#endif
    }


evapply:
  PREP_APPLY (proc, SCM_EOL);
  if (SCM_NULLP (SCM_CDR (x))) {
    ENTER_APPLY;
  evap0:
    switch (SCM_TYP7 (proc))
      {				/* no arguments given */
      case scm_tc7_subr_0:
	RETURN (SCM_SUBRF (proc) ());
      case scm_tc7_subr_1o:
	RETURN (SCM_SUBRF (proc) (SCM_UNDEFINED));
      case scm_tc7_lsubr:
	RETURN (SCM_SUBRF (proc) (SCM_EOL));
      case scm_tc7_rpsubr:
	RETURN (SCM_BOOL_T);
      case scm_tc7_asubr:
	RETURN (SCM_SUBRF (proc) (SCM_UNDEFINED, SCM_UNDEFINED));
      case scm_tc7_smob:
	if (!SCM_SMOB_APPLICABLE_P (proc))
	  goto badfun;
	RETURN (SCM_SMOB_APPLY_0 (proc));
      case scm_tc7_cclo:
	t.arg1 = proc;
	proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	debug.info->a.proc = proc;
	debug.info->a.args = scm_cons (t.arg1, SCM_EOL);
#endif
	goto evap1;
      case scm_tc7_pws:
	proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
	debug.info->a.proc = proc;
#endif
	if (!SCM_CLOSUREP (proc))
	  goto evap0;
	if (scm_badformalsp (proc, 0))
	  goto umwrongnumargs;
      case scm_tcs_closures:
	x = SCM_CODE (proc);
	env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc), SCM_EOL, SCM_ENV (proc));
	goto nontoplevel_cdrxbegin;
      case scm_tcs_cons_gloc: /* really structs, not glocs */
	if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	  {
	    x = SCM_ENTITY_PROCEDURE (proc);
	    arg2 = SCM_EOL;
	    goto type_dispatch;
	  }
	else if (!SCM_I_OPERATORP (proc))
	  goto badfun;
	else
	  {
	    t.arg1 = proc;
	    proc = (SCM_I_ENTITYP (proc)
		    ? SCM_ENTITY_PROCEDURE (proc)
		    : SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
	    debug.info->a.proc = proc;
	    debug.info->a.args = scm_cons (t.arg1, SCM_EOL);
#endif
	    if (SCM_NIMP (proc))
	      goto evap1;
	    else
	      goto badfun;
	  }
      case scm_tc7_subr_1:
      case scm_tc7_subr_2:
      case scm_tc7_subr_2o:
      case scm_tc7_cxr:
      case scm_tc7_subr_3:
      case scm_tc7_lsubr_2:
      umwrongnumargs:
	unmemocar (x, env);
      wrongnumargs:
	/* scm_everr (x, env,...)  */
	scm_wrong_num_args (proc);
      default:
	/* handle macros here */
	goto badfun;
      }
  }

  /* must handle macros by here */
  x = SCM_CDR (x);
#ifdef SCM_CAUTIOUS
  if (SCM_IMP (x))
    goto wrongnumargs;
  else if (SCM_CONSP (x))
    {
      if (SCM_IMP (SCM_CAR (x)))
	t.arg1 = SCM_EVALIM (SCM_CAR (x), env);
      else
	t.arg1 = EVALCELLCAR (x, env);
    }
  else if (SCM_TYP3 (x) == scm_tc3_cons_gloc)
    {
      scm_bits_t vcell = SCM_STRUCT_VTABLE_DATA (x) [scm_vtable_index_vcell];
      if (vcell == 0)
	t.arg1 = SCM_CAR (x); /* struct planted in code */
      else
	t.arg1 = SCM_GLOC_VAL (SCM_CAR (x));
    }
  else
    goto wrongnumargs;
#else
  t.arg1 = EVALCAR (x, env);
#endif
#ifdef DEVAL
  debug.info->a.args = scm_cons (t.arg1, SCM_EOL);
#endif
  x = SCM_CDR (x);
  if (SCM_NULLP (x))
    {
      ENTER_APPLY;
    evap1:
      switch (SCM_TYP7 (proc))
	{				/* have one argument in t.arg1 */
	case scm_tc7_subr_2o:
	  RETURN (SCM_SUBRF (proc) (t.arg1, SCM_UNDEFINED));
	case scm_tc7_subr_1:
	case scm_tc7_subr_1o:
	  RETURN (SCM_SUBRF (proc) (t.arg1));
	case scm_tc7_cxr:
	  if (SCM_SUBRF (proc))
	    {
	      if (SCM_INUMP (t.arg1))
		{
		  RETURN (scm_make_real (SCM_DSUBRF (proc) ((double) SCM_INUM (t.arg1))));
		}
	      SCM_ASRTGO (SCM_NIMP (t.arg1), floerr);
	      if (SCM_REALP (t.arg1))
		{
		  RETURN (scm_make_real (SCM_DSUBRF (proc) (SCM_REAL_VALUE (t.arg1))));
		}
#ifdef SCM_BIGDIG
	      if (SCM_BIGP (t.arg1))
		{
		  RETURN (scm_make_real (SCM_DSUBRF (proc) (scm_big2dbl (t.arg1))));
		}
#endif
	    floerr:
	      SCM_WTA_DISPATCH_1 (*SCM_SUBR_GENERIC (proc), t.arg1,
				  SCM_ARG1, SCM_SYMBOL_CHARS (SCM_SNAME (proc)));
	    }
	  proc = SCM_SNAME (proc);
	  {
	    char *chrs = SCM_SYMBOL_CHARS (proc) + SCM_SYMBOL_LENGTH (proc) - 1;
	    while ('c' != *--chrs)
	      {
		SCM_ASSERT (SCM_CONSP (t.arg1),
			    t.arg1, SCM_ARG1, SCM_SYMBOL_CHARS (proc));
		t.arg1 = ('a' == *chrs) ? SCM_CAR (t.arg1) : SCM_CDR (t.arg1);
	      }
	    RETURN (t.arg1);
	  }
	case scm_tc7_rpsubr:
	  RETURN (SCM_BOOL_T);
	case scm_tc7_asubr:
	  RETURN (SCM_SUBRF (proc) (t.arg1, SCM_UNDEFINED));
	case scm_tc7_lsubr:
#ifdef DEVAL
	  RETURN (SCM_SUBRF (proc) (debug.info->a.args))
#else
	  RETURN (SCM_SUBRF (proc) (scm_cons (t.arg1, SCM_EOL)));
#endif
	case scm_tc7_smob:
	  if (!SCM_SMOB_APPLICABLE_P (proc))
	    goto badfun;
	  RETURN (SCM_SMOB_APPLY_1 (proc, t.arg1));
	case scm_tc7_cclo:
	  arg2 = t.arg1;
	  t.arg1 = proc;
	  proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	  debug.info->a.args = scm_cons (t.arg1, debug.info->a.args);
	  debug.info->a.proc = proc;
#endif
	  goto evap2;
	case scm_tc7_pws:
	  proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
	  debug.info->a.proc = proc;
#endif
	  if (!SCM_CLOSUREP (proc))
	    goto evap1;
	  if (scm_badformalsp (proc, 1))
	    goto umwrongnumargs;
	case scm_tcs_closures:
	  /* clos1: */
	  x = SCM_CODE (proc);
#ifdef DEVAL
	  env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc), debug.info->a.args, SCM_ENV (proc));
#else
	  env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc), scm_cons (t.arg1, SCM_EOL), SCM_ENV (proc));
#endif
	  goto nontoplevel_cdrxbegin;
	case scm_tcs_cons_gloc: /* really structs, not glocs */
	  if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	    {
	      x = SCM_ENTITY_PROCEDURE (proc);
#ifdef DEVAL
	      arg2 = debug.info->a.args;
#else
	      arg2 = scm_cons (t.arg1, SCM_EOL);
#endif
	      goto type_dispatch;
	    }
	  else if (!SCM_I_OPERATORP (proc))
	    goto badfun;
	  else
	    {
	      arg2 = t.arg1;
	      t.arg1 = proc;
	      proc = (SCM_I_ENTITYP (proc)
		      ? SCM_ENTITY_PROCEDURE (proc)
		      : SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
	      debug.info->a.args = scm_cons (t.arg1, debug.info->a.args);
	      debug.info->a.proc = proc;
#endif
	      if (SCM_NIMP (proc))
		goto evap2;
	      else
		goto badfun;
	    }
	case scm_tc7_subr_2:
	case scm_tc7_subr_0:
	case scm_tc7_subr_3:
	case scm_tc7_lsubr_2:
	  goto wrongnumargs;
	default:
	  goto badfun;
	}
    }
#ifdef SCM_CAUTIOUS
  if (SCM_IMP (x))
    goto wrongnumargs;
  else if (SCM_CONSP (x))
    {
      if (SCM_IMP (SCM_CAR (x)))
	arg2 = SCM_EVALIM (SCM_CAR (x), env);
      else
	arg2 = EVALCELLCAR (x, env);
    }
  else if (SCM_TYP3 (x) == scm_tc3_cons_gloc)
    {
      scm_bits_t vcell = SCM_STRUCT_VTABLE_DATA (x) [scm_vtable_index_vcell];
      if (vcell == 0)
	arg2 = SCM_CAR (x); /* struct planted in code */
      else
	arg2 = SCM_GLOC_VAL (SCM_CAR (x));
    }
  else
    goto wrongnumargs;
#else
  arg2 = EVALCAR (x, env);
#endif
  {				/* have two or more arguments */
#ifdef DEVAL
    debug.info->a.args = scm_cons2 (t.arg1, arg2, SCM_EOL);
#endif
    x = SCM_CDR (x);
    if (SCM_NULLP (x)) {
      ENTER_APPLY;
    evap2:
      switch (SCM_TYP7 (proc))
	{			/* have two arguments */
	case scm_tc7_subr_2:
	case scm_tc7_subr_2o:
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2));
	case scm_tc7_lsubr:
#ifdef DEVAL
	  RETURN (SCM_SUBRF (proc) (debug.info->a.args))
#else
	  RETURN (SCM_SUBRF (proc) (scm_cons2 (t.arg1, arg2, SCM_EOL)));
#endif
	case scm_tc7_lsubr_2:
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2, SCM_EOL));
	case scm_tc7_rpsubr:
	case scm_tc7_asubr:
	  RETURN (SCM_SUBRF (proc) (t.arg1, arg2));
	case scm_tc7_smob:
	  if (!SCM_SMOB_APPLICABLE_P (proc))
	    goto badfun;
	  RETURN (SCM_SMOB_APPLY_2 (proc, t.arg1, arg2));
	cclon:
	case scm_tc7_cclo:
#ifdef DEVAL
	  RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc),
			     scm_cons (proc, debug.info->a.args),
			     SCM_EOL));
#else
	  RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc),
			     scm_cons2 (proc, t.arg1,
					scm_cons (arg2,
						  scm_eval_args (x,
								 env,
								 proc))),
			     SCM_EOL));
#endif
	case scm_tcs_cons_gloc: /* really structs, not glocs */
	  if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	    {
	      x = SCM_ENTITY_PROCEDURE (proc);
#ifdef DEVAL
	      arg2 = debug.info->a.args;
#else
	      arg2 = scm_cons2 (t.arg1, arg2, SCM_EOL);
#endif
	      goto type_dispatch;
	    }
	  else if (!SCM_I_OPERATORP (proc))
	    goto badfun;
	  else
	    {
	    operatorn:
#ifdef DEVAL
	      RETURN (SCM_APPLY (SCM_I_ENTITYP (proc)
				 ? SCM_ENTITY_PROCEDURE (proc)
				 : SCM_OPERATOR_PROCEDURE (proc),
				 scm_cons (proc, debug.info->a.args),
				 SCM_EOL));
#else
	      RETURN (SCM_APPLY (SCM_I_ENTITYP (proc)
				 ? SCM_ENTITY_PROCEDURE (proc)
				 : SCM_OPERATOR_PROCEDURE (proc),
				 scm_cons2 (proc, t.arg1,
					    scm_cons (arg2,
						      scm_eval_args (x,
								     env,
								     proc))),
				 SCM_EOL));
#endif
	    }
	case scm_tc7_subr_0:
	case scm_tc7_cxr:
	case scm_tc7_subr_1o:
	case scm_tc7_subr_1:
	case scm_tc7_subr_3:
	  goto wrongnumargs;
	default:
	  goto badfun;
	case scm_tc7_pws:
	  proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
	  debug.info->a.proc = proc;
#endif
	  if (!SCM_CLOSUREP (proc))
	    goto evap2;
	  if (scm_badformalsp (proc, 2))
	    goto umwrongnumargs;
	case scm_tcs_closures:
	  /* clos2: */
#ifdef DEVAL
	  env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc),
			    debug.info->a.args,
			    SCM_ENV (proc));
#else
	  env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc),
			    scm_cons2 (t.arg1, arg2, SCM_EOL), SCM_ENV (proc));
#endif
	  x = SCM_CODE (proc);
	  goto nontoplevel_cdrxbegin;
	}
    }
#ifdef SCM_CAUTIOUS
    if (SCM_IMP (x) || SCM_NECONSP (x))
      goto wrongnumargs;
#endif
#ifdef DEVAL
    debug.info->a.args = scm_cons2 (t.arg1, arg2,
      scm_deval_args (x, env, proc,
		      SCM_CDRLOC (SCM_CDR (debug.info->a.args))));
#endif
    ENTER_APPLY;
  evap3:
    switch (SCM_TYP7 (proc))
      {			/* have 3 or more arguments */
#ifdef DEVAL
      case scm_tc7_subr_3:
	SCM_ASRTGO (SCM_NULLP (SCM_CDR (x)), wrongnumargs);
	RETURN (SCM_SUBRF (proc) (t.arg1, arg2,
				  SCM_CADDR (debug.info->a.args)));
      case scm_tc7_asubr:
#ifdef BUILTIN_RPASUBR
	t.arg1 = SCM_SUBRF(proc)(t.arg1, arg2);
	arg2 = SCM_CDR (SCM_CDR (debug.info->a.args));
	do
	  {
	    t.arg1 = SCM_SUBRF(proc)(t.arg1, SCM_CAR (arg2));
	    arg2 = SCM_CDR (arg2);
	  }
	while (SCM_NIMP (arg2));
	RETURN (t.arg1)
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_rpsubr:
#ifdef BUILTIN_RPASUBR
	if (SCM_FALSEP (SCM_SUBRF (proc) (t.arg1, arg2)))
	  RETURN (SCM_BOOL_F)
	t.arg1 = SCM_CDR (SCM_CDR (debug.info->a.args));
	do
	  {
	    if (SCM_FALSEP (SCM_SUBRF (proc) (arg2, SCM_CAR (t.arg1))))
	      RETURN (SCM_BOOL_F)
		arg2 = SCM_CAR (t.arg1);
	    t.arg1 = SCM_CDR (t.arg1);
	  }
	while (SCM_NIMP (t.arg1));
	RETURN (SCM_BOOL_T)
#else /* BUILTIN_RPASUBR */
	RETURN (SCM_APPLY (proc, t.arg1,
			   scm_acons (arg2,
				      SCM_CDR (SCM_CDR (debug.info->a.args)),
				      SCM_EOL)))
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_lsubr_2:
	RETURN (SCM_SUBRF (proc) (t.arg1, arg2,
				  SCM_CDR (SCM_CDR (debug.info->a.args))))
      case scm_tc7_lsubr:
	RETURN (SCM_SUBRF (proc) (debug.info->a.args))
      case scm_tc7_smob:
	if (!SCM_SMOB_APPLICABLE_P (proc))
	  goto badfun;
	RETURN (SCM_SMOB_APPLY_3 (proc, t.arg1, arg2,
				  SCM_CDDR (debug.info->a.args)));
      case scm_tc7_cclo:
	goto cclon;
      case scm_tc7_pws:
	proc = SCM_PROCEDURE (proc);
	debug.info->a.proc = proc;
	if (!SCM_CLOSUREP (proc))
	  goto evap3;
	if (scm_badargsp (SCM_CLOSURE_FORMALS (proc), debug.info->a.args))
	  goto umwrongnumargs;
      case scm_tcs_closures:
	SCM_SET_ARGSREADY (debug);
	env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc),
			      debug.info->a.args,
			      SCM_ENV (proc));
	x = SCM_CODE (proc);
	goto nontoplevel_cdrxbegin;
#else /* DEVAL */
      case scm_tc7_subr_3:
	SCM_ASRTGO (SCM_NULLP (SCM_CDR (x)), wrongnumargs);
	RETURN (SCM_SUBRF (proc) (t.arg1, arg2, EVALCAR (x, env)));
      case scm_tc7_asubr:
#ifdef BUILTIN_RPASUBR
	t.arg1 = SCM_SUBRF (proc) (t.arg1, arg2);
	do
	  {
	    t.arg1 = SCM_SUBRF(proc)(t.arg1, EVALCAR(x, env));
	    x = SCM_CDR(x);
	  }
	while (SCM_NIMP (x));
	RETURN (t.arg1)
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_rpsubr:
#ifdef BUILTIN_RPASUBR
	if (SCM_FALSEP (SCM_SUBRF (proc) (t.arg1, arg2)))
	  RETURN (SCM_BOOL_F)
	do
	  {
	    t.arg1 = EVALCAR (x, env);
	    if (SCM_FALSEP (SCM_SUBRF (proc) (arg2, t.arg1)))
	      RETURN (SCM_BOOL_F)
		arg2 = t.arg1;
	    x = SCM_CDR (x);
	  }
	while (SCM_NIMP (x));
	RETURN (SCM_BOOL_T)
#else /* BUILTIN_RPASUBR */
	RETURN (SCM_APPLY (proc, t.arg1,
			   scm_acons (arg2,
				      scm_eval_args (x, env, proc),
				      SCM_EOL)));
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_lsubr_2:
	RETURN (SCM_SUBRF (proc) (t.arg1, arg2, scm_eval_args (x, env, proc)));
      case scm_tc7_lsubr:
	RETURN (SCM_SUBRF (proc) (scm_cons2 (t.arg1,
					     arg2,
					     scm_eval_args (x, env, proc))));
      case scm_tc7_smob:
	if (!SCM_SMOB_APPLICABLE_P (proc))
	  goto badfun;
	RETURN (SCM_SMOB_APPLY_3 (proc, t.arg1, arg2,
				  scm_eval_args (x, env, proc)));
      case scm_tc7_cclo:
	goto cclon;
      case scm_tc7_pws:
	proc = SCM_PROCEDURE (proc);
	if (!SCM_CLOSUREP (proc))
	  goto evap3;
	{
	  SCM formals = SCM_CLOSURE_FORMALS (proc);
	  if (SCM_NULLP (formals)
	      || (SCM_CONSP (formals)
		  && (SCM_NULLP (SCM_CDR (formals))
		      || (SCM_CONSP (SCM_CDR (formals))
			  && scm_badargsp (SCM_CDDR (formals), x)))))
	    goto umwrongnumargs;
	}
      case scm_tcs_closures:
#ifdef DEVAL
	SCM_SET_ARGSREADY (debug);
#endif
	env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc),
			      scm_cons2 (t.arg1,
					 arg2,
					 scm_eval_args (x, env, proc)),
			      SCM_ENV (proc));
	x = SCM_CODE (proc);
	goto nontoplevel_cdrxbegin;
#endif /* DEVAL */
      case scm_tcs_cons_gloc: /* really structs, not glocs */
	if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	  {
#ifdef DEVAL
	    arg2 = debug.info->a.args;
#else
	    arg2 = scm_cons2 (t.arg1, arg2, scm_eval_args (x, env, proc));
#endif
	    x = SCM_ENTITY_PROCEDURE (proc);
	    goto type_dispatch;
	  }
	else if (!SCM_I_OPERATORP (proc))
	  goto badfun;
	else
	  goto operatorn;
      case scm_tc7_subr_2:
      case scm_tc7_subr_1o:
      case scm_tc7_subr_2o:
      case scm_tc7_subr_0:
      case scm_tc7_cxr:
      case scm_tc7_subr_1:
	goto wrongnumargs;
      default:
	goto badfun;
      }
  }
#ifdef DEVAL
exit:
  if (CHECK_EXIT && SCM_TRAPS_P)
    if (SCM_EXIT_FRAME_P || (SCM_TRACE_P && SCM_TRACED_FRAME_P (debug)))
      {
	SCM_CLEAR_TRACED_FRAME (debug);
	if (SCM_CHEAPTRAPS_P)
	  t.arg1 = scm_make_debugobj (&debug);
	else
	  {
	    int first;
	    SCM val = scm_make_continuation (&first);
	    
	    if (first)
	      t.arg1 = val;
	    else
	      {
		proc = val;
		goto ret;
	      }
	  }
	scm_ithrow (scm_sym_exit_frame, scm_cons2 (t.arg1, proc, SCM_EOL), 0);
      }
ret:
  scm_last_debug_frame = debug.prev;
  return proc;
#endif
}


/* SECTION: This code is compiled once.
 */

#ifndef DEVAL

/* This code processes the arguments to apply:

   (apply PROC ARG1 ... ARGS)

   Given a list (ARG1 ... ARGS), this function conses the ARG1
   ... arguments onto the front of ARGS, and returns the resulting
   list.  Note that ARGS is a list; thus, the argument to this
   function is a list whose last element is a list.

   Apply calls this function, and applies PROC to the elements of the
   result.  apply:nconc2last takes care of building the list of
   arguments, given (ARG1 ... ARGS).

   Rather than do new consing, apply:nconc2last destroys its argument.
   On that topic, this code came into my care with the following
   beautifully cryptic comment on that topic: "This will only screw
   you if you do (scm_apply scm_apply '( ... ))"  If you know what
   they're referring to, send me a patch to this comment.  */

SCM_DEFINE (scm_nconc2last, "apply:nconc2last", 1, 0, 0, 
	    (SCM lst),
	    "Given a list (@var{arg1} @dots{} @var{args}), this function\n"
	    "conses the @var{arg1} @dots{} arguments onto the front of\n"
	    "@var{args}, and returns the resulting list. Note that\n"
	    "@var{args} is a list; thus, the argument to this function is\n"
	    "a list whose last element is a list.\n"
	    "Note: Rather than do new consing, @code{apply:nconc2last}\n"
	    "destroys its argument, so use with care.")
#define FUNC_NAME s_scm_nconc2last
{
  SCM *lloc;
  SCM_VALIDATE_NONEMPTYLIST (1,lst);
  lloc = &lst;
  while (SCM_NNULLP (SCM_CDR (*lloc)))
    lloc = SCM_CDRLOC (*lloc);
  SCM_ASSERT (scm_ilength (SCM_CAR (*lloc)) >= 0, lst, SCM_ARG1, FUNC_NAME);
  *lloc = SCM_CAR (*lloc);
  return lst;
}
#undef FUNC_NAME

#endif /* !DEVAL */


/* SECTION: When DEVAL is defined this code yields scm_dapply.
 * It is compiled twice.
 */

#if 0

SCM 
scm_apply (SCM proc, SCM arg1, SCM args)
{}
#endif

#if 0

SCM 
scm_dapply (SCM proc, SCM arg1, SCM args)
{ /* empty */ }
#endif


/* Apply a function to a list of arguments.

   This function is exported to the Scheme level as taking two
   required arguments and a tail argument, as if it were:
	(lambda (proc arg1 . args) ...)
   Thus, if you just have a list of arguments to pass to a procedure,
   pass the list as ARG1, and '() for ARGS.  If you have some fixed
   args, pass the first as ARG1, then cons any remaining fixed args
   onto the front of your argument list, and pass that as ARGS.  */

SCM 
SCM_APPLY (SCM proc, SCM arg1, SCM args)
{
#ifdef DEBUG_EXTENSIONS
#ifdef DEVAL
  scm_debug_frame debug;
  scm_debug_info debug_vect_body;
  debug.prev = scm_last_debug_frame;
  debug.status = SCM_APPLYFRAME;
  debug.vect = &debug_vect_body;
  debug.vect[0].a.proc = proc;
  debug.vect[0].a.args = SCM_EOL;
  scm_last_debug_frame = &debug;
#else
  if (SCM_DEBUGGINGP)
    return scm_dapply (proc, arg1, args);
#endif
#endif

  SCM_ASRTGO (SCM_NIMP (proc), badproc);

  /* If ARGS is the empty list, then we're calling apply with only two
     arguments --- ARG1 is the list of arguments for PROC.  Whatever
     the case, futz with things so that ARG1 is the first argument to
     give to PROC (or SCM_UNDEFINED if no args), and ARGS contains the
     rest.

     Setting the debug apply frame args this way is pretty messy.
     Perhaps we should store arg1 and args directly in the frame as
     received, and let scm_frame_arguments unpack them, because that's
     a relatively rare operation.  This works for now; if the Guile
     developer archives are still around, see Mikael's post of
     11-Apr-97.  */
  if (SCM_NULLP (args))
    {
      if (SCM_NULLP (arg1))
	{
	  arg1 = SCM_UNDEFINED;
#ifdef DEVAL
	  debug.vect[0].a.args = SCM_EOL;
#endif
	}
      else
	{
#ifdef DEVAL
	  debug.vect[0].a.args = arg1;
#endif
	  args = SCM_CDR (arg1);
	  arg1 = SCM_CAR (arg1);
	}
    }
  else
    {
      args = scm_nconc2last (args);
#ifdef DEVAL
      debug.vect[0].a.args = scm_cons (arg1, args);
#endif
    }
#ifdef DEVAL
  if (SCM_ENTER_FRAME_P && SCM_TRAPS_P)
    {
      SCM tmp;
      if (SCM_CHEAPTRAPS_P)
	tmp = scm_make_debugobj (&debug);
      else
	{
	  int first;

	  tmp = scm_make_continuation (&first);
	  if (!first)
	    goto entap;
	}
      scm_ithrow (scm_sym_enter_frame, scm_cons (tmp, SCM_EOL), 0);
    }
entap:
  ENTER_APPLY;
#endif
tail:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_2o:
      args = SCM_NULLP (args) ? SCM_UNDEFINED : SCM_CAR (args);
      RETURN (SCM_SUBRF (proc) (arg1, args))
    case scm_tc7_subr_2:
      SCM_ASRTGO (SCM_NNULLP (args) && SCM_NULLP (SCM_CDR (args)),
		  wrongnumargs);
      args = SCM_CAR (args);
      RETURN (SCM_SUBRF (proc) (arg1, args))
    case scm_tc7_subr_0:
      SCM_ASRTGO (SCM_UNBNDP (arg1), wrongnumargs);
      RETURN (SCM_SUBRF (proc) ())
    case scm_tc7_subr_1:
      SCM_ASRTGO (!SCM_UNBNDP (arg1), wrongnumargs);
    case scm_tc7_subr_1o:
      SCM_ASRTGO (SCM_NULLP (args), wrongnumargs);
      RETURN (SCM_SUBRF (proc) (arg1))
    case scm_tc7_cxr:
      SCM_ASRTGO (!SCM_UNBNDP (arg1) && SCM_NULLP (args), wrongnumargs);
      if (SCM_SUBRF (proc))
	{
	  if (SCM_INUMP (arg1))
	    {
	      RETURN (scm_make_real (SCM_DSUBRF (proc) ((double) SCM_INUM (arg1))));
	    }
	  SCM_ASRTGO (SCM_NIMP (arg1), floerr);
	  if (SCM_REALP (arg1))
	    {
	      RETURN (scm_make_real (SCM_DSUBRF (proc) (SCM_REAL_VALUE (arg1))));
	    }
#ifdef SCM_BIGDIG
	  if (SCM_BIGP (arg1))
	      RETURN (scm_make_real (SCM_DSUBRF (proc) (scm_big2dbl (arg1))))
#endif
	floerr:
	  SCM_WTA_DISPATCH_1 (*SCM_SUBR_GENERIC (proc), arg1,
			      SCM_ARG1, SCM_SYMBOL_CHARS (SCM_SNAME (proc)));
	}
      proc = SCM_SNAME (proc);
      {
	char *chrs = SCM_SYMBOL_CHARS (proc) + SCM_SYMBOL_LENGTH (proc) - 1;
	while ('c' != *--chrs)
	  {
	    SCM_ASSERT (SCM_CONSP (arg1),
		    arg1, SCM_ARG1, SCM_SYMBOL_CHARS (proc));
	    arg1 = ('a' == *chrs) ? SCM_CAR (arg1) : SCM_CDR (arg1);
	  }
	RETURN (arg1)
      }
    case scm_tc7_subr_3:
      SCM_ASRTGO (SCM_NNULLP (args)
		  && SCM_NNULLP (SCM_CDR (args))
		  && SCM_NULLP (SCM_CDDR (args)),
		  wrongnumargs);
      RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CAR (SCM_CDR (args))))
    case scm_tc7_lsubr:
#ifdef DEVAL
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : debug.vect[0].a.args))
#else
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : scm_cons (arg1, args)))
#endif
    case scm_tc7_lsubr_2:
      SCM_ASRTGO (SCM_CONSP (args), wrongnumargs);
      RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CDR (args)))
    case scm_tc7_asubr:
      if (SCM_NULLP (args))
	RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED))
      while (SCM_NIMP (args))
	{
	  SCM_ASSERT (SCM_CONSP (args), args, SCM_ARG2, "apply");
	  arg1 = SCM_SUBRF (proc) (arg1, SCM_CAR (args));
	  args = SCM_CDR (args);
	}
      RETURN (arg1);
    case scm_tc7_rpsubr:
      if (SCM_NULLP (args))
	RETURN (SCM_BOOL_T);
      while (SCM_NIMP (args))
	{
	  SCM_ASSERT (SCM_CONSP (args), args, SCM_ARG2, "apply");
	  if (SCM_FALSEP (SCM_SUBRF (proc) (arg1, SCM_CAR (args))))
	    RETURN (SCM_BOOL_F);
	  arg1 = SCM_CAR (args);
	  args = SCM_CDR (args);
	}
      RETURN (SCM_BOOL_T);
    case scm_tcs_closures:
#ifdef DEVAL
      arg1 = (SCM_UNBNDP (arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
      arg1 = (SCM_UNBNDP (arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
#ifndef SCM_RECKLESS
      if (scm_badargsp (SCM_CLOSURE_FORMALS (proc), arg1))
	goto wrongnumargs;
#endif
      
      /* Copy argument list */
      if (SCM_IMP (arg1))
	args = arg1;
      else
	{
	  SCM tl = args = scm_cons (SCM_CAR (arg1), SCM_UNSPECIFIED);
	  while (arg1 = SCM_CDR (arg1), SCM_CONSP (arg1))
	    {
	      SCM_SETCDR (tl, scm_cons (SCM_CAR (arg1),
					SCM_UNSPECIFIED));
	      tl = SCM_CDR (tl);
	    }
	  SCM_SETCDR (tl, arg1);
	}
      
      args = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc), args, SCM_ENV (proc));
      proc = SCM_CDR (SCM_CODE (proc));
    again:
      arg1 = proc;
      while (SCM_NNULLP (arg1 = SCM_CDR (arg1)))
	{
	  if (SCM_IMP (SCM_CAR (proc)))
	    {
	      if (SCM_ISYMP (SCM_CAR (proc)))
		{
		  proc = scm_m_expand_body (proc, args);
		  goto again;
		}
	      else
		SCM_EVALIM2 (SCM_CAR (proc));
	    }
	  else
	    SCM_CEVAL (SCM_CAR (proc), args);
	  proc = arg1;
	}
      RETURN (EVALCAR (proc, args));
    case scm_tc7_smob:
      if (!SCM_SMOB_APPLICABLE_P (proc))
	goto badproc;
      if (SCM_UNBNDP (arg1))
	RETURN (SCM_SMOB_APPLY_0 (proc))
      else if (SCM_NULLP (args))
	RETURN (SCM_SMOB_APPLY_1 (proc, arg1))
      else if (SCM_NULLP (SCM_CDR (args)))
	RETURN (SCM_SMOB_APPLY_2 (proc, arg1, SCM_CAR (args)))
      else
	RETURN (SCM_SMOB_APPLY_3 (proc, arg1, SCM_CAR (args), SCM_CDR (args)));
    case scm_tc7_cclo:
#ifdef DEVAL
      args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
      arg1 = proc;
      proc = SCM_CCLO_SUBR (proc);
      debug.vect[0].a.proc = proc;
      debug.vect[0].a.args = scm_cons (arg1, args);
#else
      args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
      arg1 = proc;
      proc = SCM_CCLO_SUBR (proc);
#endif
      goto tail;
    case scm_tc7_pws:
      proc = SCM_PROCEDURE (proc);
#ifdef DEVAL
      debug.vect[0].a.proc = proc;
#endif
      goto tail;
    case scm_tcs_cons_gloc: /* really structs, not glocs */
      if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	{
#ifdef DEVAL
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
	  RETURN (scm_apply_generic (proc, args));
	}
      else if (!SCM_I_OPERATORP (proc))
	goto badproc;
      else
	{
#ifdef DEVAL
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : debug.vect[0].a.args);
#else
	  args = (SCM_UNBNDP(arg1) ? SCM_EOL : scm_cons (arg1, args));
#endif
	  arg1 = proc;
	  proc = (SCM_I_ENTITYP (proc)
		  ? SCM_ENTITY_PROCEDURE (proc)
		  : SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
	  debug.vect[0].a.proc = proc;
	  debug.vect[0].a.args = scm_cons (arg1, args);
#endif
	  if (SCM_NIMP (proc))
	    goto tail;
	  else
	    goto badproc;
	}
    wrongnumargs:
      scm_wrong_num_args (proc);
    default:
    badproc:
      scm_wrong_type_arg ("apply", SCM_ARG1, proc);
      RETURN (arg1);
    }
#ifdef DEVAL
exit:
  if (CHECK_EXIT && SCM_TRAPS_P)
    if (SCM_EXIT_FRAME_P || (SCM_TRACE_P && SCM_TRACED_FRAME_P (debug)))
      {
	SCM_CLEAR_TRACED_FRAME (debug);
	if (SCM_CHEAPTRAPS_P)
	  arg1 = scm_make_debugobj (&debug);
	else
	  {
	    int first;
	    SCM val = scm_make_continuation (&first);

	    if (first)
	      arg1 = val;
	    else
	      {
		proc = val;
		goto ret;
	      }
	  }
	scm_ithrow (scm_sym_exit_frame, scm_cons2 (arg1, proc, SCM_EOL), 0);
      }
ret:
  scm_last_debug_frame = debug.prev;
  return proc;
#endif
}


/* SECTION: The rest of this file is only read once.
 */

#ifndef DEVAL

/* Typechecking for multi-argument MAP and FOR-EACH.

   Verify that each element of the vector ARGV, except for the first,
   is a proper list whose length is LEN.  Attribute errors to WHO,
   and claim that the i'th element of ARGV is WHO's i+2'th argument.  */
static inline void
check_map_args (SCM argv,
		long len,
		SCM gf,
		SCM proc,
		SCM args,
		const char *who)
{
  SCM *ve = SCM_VELTS (argv);
  int i;

  for (i = SCM_VECTOR_LENGTH (argv) - 1; i >= 1; i--)
    {
      int elt_len = scm_ilength (ve[i]);

      if (elt_len < 0)
	{
	  if (gf)
	    scm_apply_generic (gf, scm_cons (proc, args));
	  else
	    scm_wrong_type_arg (who, i + 2, ve[i]);
	}

      if (elt_len != len)
	scm_out_of_range (who, ve[i]);
    }

  scm_remember_upto_here_1 (argv);
}


SCM_GPROC (s_map, "map", 2, 0, 1, scm_map, g_map);

/* Note: Currently, scm_map applies PROC to the argument list(s)
   sequentially, starting with the first element(s).  This is used in
   evalext.c where the Scheme procedure `map-in-order', which guarantees
   sequential behaviour, is implemented using scm_map.  If the
   behaviour changes, we need to update `map-in-order'.
*/

SCM 
scm_map (SCM proc, SCM arg1, SCM args)
#define FUNC_NAME s_map
{
  long i, len;
  SCM res = SCM_EOL;
  SCM *pres = &res;
  SCM *ve = &args;		/* Keep args from being optimized away. */

  len = scm_ilength (arg1);
  SCM_GASSERTn (len >= 0,
		g_map, scm_cons2 (proc, arg1, args), SCM_ARG2, s_map);
  SCM_VALIDATE_REST_ARGUMENT (args);
  if (SCM_NULLP (args))
    {
      while (SCM_NIMP (arg1))
	{
	  *pres = scm_cons (scm_apply (proc, SCM_CAR (arg1), scm_listofnull),
			    SCM_EOL);
	  pres = SCM_CDRLOC (*pres);
	  arg1 = SCM_CDR (arg1);
	}
      return res;
    }
  args = scm_vector (arg1 = scm_cons (arg1, args));
  ve = SCM_VELTS (args);
#ifndef SCM_RECKLESS
  check_map_args (args, len, g_map, proc, arg1, s_map);
#endif
  while (1)
    {
      arg1 = SCM_EOL;
      for (i = SCM_VECTOR_LENGTH (args) - 1; i >= 0; i--)
	{
	  if (SCM_IMP (ve[i])) 
	    return res;
	  arg1 = scm_cons (SCM_CAR (ve[i]), arg1);
	  ve[i] = SCM_CDR (ve[i]);
	}
      *pres = scm_cons (scm_apply (proc, arg1, SCM_EOL), SCM_EOL);
      pres = SCM_CDRLOC (*pres);
    }
}
#undef FUNC_NAME


SCM_GPROC (s_for_each, "for-each", 2, 0, 1, scm_for_each, g_for_each);

SCM 
scm_for_each (SCM proc, SCM arg1, SCM args)
#define FUNC_NAME s_for_each
{
  SCM *ve = &args;		/* Keep args from being optimized away. */
  long i, len;
  len = scm_ilength (arg1);
  SCM_GASSERTn (len >= 0, g_for_each, scm_cons2 (proc, arg1, args),
		SCM_ARG2, s_for_each);
  SCM_VALIDATE_REST_ARGUMENT (args);
  if SCM_NULLP (args)
    {
      while SCM_NIMP (arg1)
	{
	  scm_apply (proc, SCM_CAR (arg1), scm_listofnull);
	  arg1 = SCM_CDR (arg1);
	}
      return SCM_UNSPECIFIED;
    }
  args = scm_vector (arg1 = scm_cons (arg1, args));
  ve = SCM_VELTS (args);
#ifndef SCM_RECKLESS
  check_map_args (args, len, g_for_each, proc, arg1, s_for_each);
#endif
  while (1)
    {
      arg1 = SCM_EOL;
      for (i = SCM_VECTOR_LENGTH (args) - 1; i >= 0; i--)
	{
	  if SCM_IMP
	    (ve[i]) return SCM_UNSPECIFIED;
	  arg1 = scm_cons (SCM_CAR (ve[i]), arg1);
	  ve[i] = SCM_CDR (ve[i]);
	}
      scm_apply (proc, arg1, SCM_EOL);
    }
}
#undef FUNC_NAME


SCM 
scm_closure (SCM code, SCM env)
{
  register SCM z;

  SCM_NEWCELL (z);
  SCM_SETCODE (z, code);
  SCM_SETENV (z, env);
  return z;
}


scm_bits_t scm_tc16_promise;

SCM 
scm_makprom (SCM code)
{
  SCM_RETURN_NEWSMOB (scm_tc16_promise, SCM_UNPACK (code));
}



static int 
promise_print (SCM exp, SCM port, scm_print_state *pstate)
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<promise ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (SCM_CELL_WORD_1 (exp), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return !0;
}


SCM_DEFINE (scm_force, "force", 1, 0, 0, 
	    (SCM x),
	    "If the promise @var{x} has not been computed yet, compute and\n"
	    "return @var{x}, otherwise just return the previously computed\n"
	    "value.")
#define FUNC_NAME s_scm_force
{
  SCM_VALIDATE_SMOB (1, x, promise);
  if (!((1L << 16) & SCM_CELL_WORD_0 (x)))
    {
      SCM ans = scm_apply (SCM_CELL_OBJECT_1 (x), SCM_EOL, SCM_EOL);
      if (!((1L << 16) & SCM_CELL_WORD_0 (x)))
	{
	  SCM_DEFER_INTS;
	  SCM_SET_CELL_OBJECT_1 (x, ans);
	  SCM_SET_CELL_WORD_0 (x, SCM_CELL_WORD_0 (x) | (1L << 16));
	  SCM_ALLOW_INTS;
	}
    }
  return SCM_CELL_OBJECT_1 (x);
}
#undef FUNC_NAME


SCM_DEFINE (scm_promise_p, "promise?", 1, 0, 0, 
            (SCM obj),
	    "Return true if @var{obj} is a promise, i.e. a delayed computation\n"
	    "(@pxref{Delayed evaluation,,,r5rs.info,The Revised^5 Report on Scheme}).")
#define FUNC_NAME s_scm_promise_p
{
  return SCM_BOOL (SCM_TYP16_PREDICATE (scm_tc16_promise, obj));
}
#undef FUNC_NAME


SCM_DEFINE (scm_cons_source, "cons-source", 3, 0, 0, 
            (SCM xorig, SCM x, SCM y),
	    "Create and return a new pair whose car and cdr are @var{x} and @var{y}.\n"
	    "Any source properties associated with @var{xorig} are also associated\n"
	    "with the new pair.")
#define FUNC_NAME s_scm_cons_source
{
  SCM p, z;
  SCM_NEWCELL (z);
  SCM_SET_CELL_OBJECT_0 (z, x);
  SCM_SET_CELL_OBJECT_1 (z, y);
  /* Copy source properties possibly associated with xorig. */
  p = scm_whash_lookup (scm_source_whash, xorig);
  if (!SCM_IMP (p))
    scm_whash_insert (scm_source_whash, z, p);
  return z;
}
#undef FUNC_NAME


SCM_DEFINE (scm_copy_tree, "copy-tree", 1, 0, 0, 
            (SCM obj),
	    "Recursively copy the data tree that is bound to @var{obj}, and return a\n"
	    "pointer to the new data structure.  @code{copy-tree} recurses down the\n"
	    "contents of both pairs and vectors (since both cons cells and vector\n"
	    "cells may point to arbitrary objects), and stops recursing when it hits\n"
	    "any other object.")
#define FUNC_NAME s_scm_copy_tree
{
  SCM ans, tl;
  if (SCM_IMP (obj)) 
    return obj;
  if (SCM_VECTORP (obj))
    {
      scm_sizet i = SCM_VECTOR_LENGTH (obj);
      ans = scm_c_make_vector (i, SCM_UNSPECIFIED);
      while (i--)
	SCM_VELTS (ans)[i] = scm_copy_tree (SCM_VELTS (obj)[i]);
      return ans;
    }
  if (SCM_NCONSP (obj))
    return obj;
  ans = tl = scm_cons_source (obj,
			      scm_copy_tree (SCM_CAR (obj)),
			      SCM_UNSPECIFIED);
  while (obj = SCM_CDR (obj), SCM_CONSP (obj))
    {
      SCM_SETCDR (tl, scm_cons (scm_copy_tree (SCM_CAR (obj)),
				SCM_UNSPECIFIED));
      tl = SCM_CDR (tl);
    }
  SCM_SETCDR (tl, obj);
  return ans;
}
#undef FUNC_NAME


/* We have three levels of EVAL here:

   - scm_i_eval (exp, env)

     evaluates EXP in environment ENV.  ENV is a lexical environment
     structure as used by the actual tree code evaluator.  When ENV is
     a top-level environment, then changes to the current module are
     tracked by updating ENV so that it continues to be in sync with
     the current module.

   - scm_primitive_eval (exp)

     evaluates EXP in the top-level environment as determined by the
     current module.  This is done by constructing a suitable
     environment and calling scm_i_eval.  Thus, changes to the
     top-level module are tracked normally.

   - scm_eval (exp, mod)

     evaluates EXP while MOD is the current module.  This is done by
     setting the current module to MOD, invoking scm_primitive_eval on
     EXP, and then restoring the current module to the value it had
     previously.  That is, while EXP is evaluated, changes to the
     current module are tracked, but these changes do not persist when
     scm_eval returns.

  For each level of evals, there are two variants, distinguished by a
  _x suffix: the ordinary variant does not modify EXP while the _x
  variant can destructively modify EXP into something completely
  unintelligible.  A Scheme data structure passed as EXP to one of the
  _x variants should not ever be used again for anything.  So when in
  doubt, use the ordinary variant.

*/

SCM 
scm_i_eval_x (SCM exp, SCM env)
{
  return SCM_XEVAL (exp, env);
}

SCM 
scm_i_eval (SCM exp, SCM env)
{
  exp = scm_copy_tree (exp);
  return SCM_XEVAL (exp, env);
}

SCM
scm_primitive_eval_x (SCM exp)
{
  SCM env;
  SCM transformer = scm_current_module_transformer ();
  if (SCM_NIMP (transformer))
    exp = scm_apply (transformer, exp, scm_listofnull);
  env = scm_top_level_env (scm_current_module_lookup_closure ());
  return scm_i_eval_x (exp, env);
}

SCM_DEFINE (scm_primitive_eval, "primitive-eval", 1, 0, 0,
	    (SCM exp),
	    "Evaluate @var{exp} in the top-level environment specified by\n"
	    "the current module.")
#define FUNC_NAME s_scm_primitive_eval
{
  SCM env;
  SCM transformer = scm_current_module_transformer ();
  if (SCM_NIMP (transformer))
    exp = scm_apply (transformer, exp, scm_listofnull);
  env = scm_top_level_env (scm_current_module_lookup_closure ());
  return scm_i_eval (exp, env);
}
#undef FUNC_NAME

/* Eval does not take the second arg optionally.  This is intentional
 * in order to be R5RS compatible, and to prepare for the new module
 * system, where we would like to make the choice of evaluation
 * environment explicit.  */

static void
change_environment (void *data)
{
  SCM pair = SCM_PACK (data);
  SCM new_module = SCM_CAR (pair);
  SCM old_module = scm_current_module ();
  SCM_SETCDR (pair, old_module);
  scm_set_current_module (new_module);
}


static void
restore_environment (void *data)
{
  SCM pair = SCM_PACK (data);
  SCM old_module = SCM_CDR (pair);
  SCM new_module = scm_current_module ();
  SCM_SETCAR (pair, new_module);
  scm_set_current_module (old_module);
}

static SCM
inner_eval_x (void *data)
{
  return scm_primitive_eval_x (SCM_PACK(data));
}

SCM
scm_eval_x (SCM exp, SCM module)
#define FUNC_NAME "eval!"
{
  SCM_VALIDATE_MODULE (2, module);

  return scm_internal_dynamic_wind 
    (change_environment, inner_eval_x, restore_environment,
     (void *) SCM_UNPACK (exp),
     (void *) SCM_UNPACK (scm_cons (module, SCM_BOOL_F)));
}
#undef FUNC_NAME

static SCM
inner_eval (void *data)
{
  return scm_primitive_eval (SCM_PACK(data));
}

SCM_DEFINE (scm_eval, "eval", 2, 0, 0, 
	    (SCM exp, SCM module),
	    "Evaluate @var{exp}, a list representing a Scheme expression,\n"
            "in the top-level environment specified by @var{module}.\n"
            "While @var{exp} is evaluated (using @var{primitive-eval}),\n"
            "@var{module} is made the current module.  The current module\n"
            "is reset to its previous value when @var{eval} returns.")
#define FUNC_NAME s_scm_eval
{
  SCM_VALIDATE_MODULE (2, module);

  return scm_internal_dynamic_wind 
    (change_environment, inner_eval, restore_environment,
     (void *) SCM_UNPACK (exp),
     (void *) SCM_UNPACK (scm_cons (module, SCM_BOOL_F)));
}
#undef FUNC_NAME

#if (SCM_DEBUG_DEPRECATED == 0)

/* Use scm_current_module () or scm_interaction_environment ()
 * instead.  The former is the module selected during loading of code.
 * The latter is the module in which the user of this thread currently
 * types expressions.
 */

SCM scm_top_level_lookup_closure_var;
SCM scm_system_transformer;

/* Avoid using this functionality altogether (except for implementing
 * libguile, where you can use scm_i_eval or scm_i_eval_x).
 *
 * Applications should use either C level scm_eval_x or Scheme
 * scm_eval; or scm_primitive_eval_x or scm_primitive_eval.  */

SCM 
scm_eval_3 (SCM obj, int copyp, SCM env)
{
  if (copyp)
    return scm_i_eval (obj, env);
  else
    return scm_i_eval_x (obj, env);
}

SCM_DEFINE (scm_eval2, "eval2", 2, 0, 0,
           (SCM obj, SCM env_thunk),
	    "Evaluate @var{exp}, a Scheme expression, in the environment\n"
	    "designated by @var{lookup}, a symbol-lookup function."
	    "Do not use this version of eval, it does not play well\n"
	    "with the module system.  Use @code{eval} or\n"
	    "@code{primitive-eval} instead.")
#define FUNC_NAME s_scm_eval2
{
  return scm_i_eval (obj, scm_top_level_env (env_thunk));
}
#undef FUNC_NAME

#endif /* DEPRECATED */


/* At this point, scm_deval and scm_dapply are generated.
 */

#ifdef DEBUG_EXTENSIONS
# define DEVAL
# include "eval.c"
#endif



void 
scm_init_eval ()
{
  scm_init_opts (scm_evaluator_traps,
		 scm_evaluator_trap_table,
		 SCM_N_EVALUATOR_TRAPS);
  scm_init_opts (scm_eval_options_interface,
		 scm_eval_opts,
		 SCM_N_EVAL_OPTIONS);
  
  scm_tc16_promise = scm_make_smob_type ("promise", 0);
  scm_set_smob_mark (scm_tc16_promise, scm_markcdr);
  scm_set_smob_print (scm_tc16_promise, promise_print);

  /* Dirk:Fixme:: make scm_undefineds local to eval.c: it's only used here. */
  scm_undefineds = scm_cons (SCM_UNDEFINED, SCM_EOL);
  SCM_SETCDR (scm_undefineds, scm_undefineds);
  scm_listofnull = scm_cons (SCM_EOL, SCM_EOL);

  scm_f_apply = scm_c_define_subr ("apply", scm_tc7_lsubr_2, scm_apply);

  /* acros */
  /* end of acros */

#if SCM_DEBUG_DEPRECATED == 0
  scm_top_level_lookup_closure_var =
    scm_c_define ("*top-level-lookup-closure*", scm_make_fluid ());
  scm_system_transformer =
    scm_c_define ("scm:eval-transformer", scm_make_fluid ());
#endif

#ifndef SCM_MAGIC_SNARFER
#include "libguile/eval.x"
#endif

  scm_c_define ("nil", scm_lisp_nil);
  scm_c_define ("t", scm_lisp_t);
  
  scm_add_feature ("delay");
}

#endif /* !DEVAL */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/

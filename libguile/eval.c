/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002 Free Software Foundation, Inc.
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
#   pragma alloca
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
#include "libguile/goops.h"
#include "libguile/values.h"

#include "libguile/validate.h"
#include "libguile/eval.h"
#include "libguile/lang.h"



#define SCM_VALIDATE_NON_EMPTY_COMBINATION(x) \
  do { \
    if (SCM_EQ_P ((x), SCM_EOL)) \
      scm_misc_error (NULL, scm_s_expression, SCM_EOL); \
  } while (0)



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
#define EVALCELLCAR(x, env) (SCM_SYMBOLP (SCM_CAR (x)) \
			     ? *scm_lookupcar (x, env, 1) \
			     : SCM_CEVAL (SCM_CAR (x), env))

#define EVALCAR(x, env) (SCM_IMP (SCM_CAR (x)) \
			 ? SCM_EVALIM (SCM_CAR (x), env) \
			 : EVALCELLCAR (x, env))

#define EXTEND_ENV SCM_EXTEND_ENV

#ifdef MEMOIZE_LOCALS

SCM *
scm_ilookup (SCM iloc, SCM env)
{
  register long ir = SCM_IFRAME (iloc);
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
   global variable references (into ilocs and variables), because all
   threads will mutate the code in *exactly* the same way and (if I
   read the C code correctly) it is not possible to observe a half-way
   mutated cons cell.  The lookup procedure can handle this
   transparently without any critical sections.

   It is different with macro expansion, because macro expansion
   happens outside of the lookup procedure and can't be
   undone. Therefore the lookup procedure can't cope with it.  It has
   to indicate failure when it detects a lost race and hope that the
   caller can handle it.  Luckily, it turns out that this is the case.

   An example to illustrate this: Suppose that the following form will
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
   only substitutes a variable for it but goes right ahead and
   replaces it with the compiled form (#@let* (x 12) x).  Now, when
   the first thread completes its lookup, it would replace the #@let*
   with a variable containing the "let" binding, effectively reverting
   the form to (let (x 12) x).  This is wrong.  It has to detect that
   it has lost the race and the evaluator has to reconsider the
   changed form completely.

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
   SCM_LOOKUPCAR1 and aborts on receiving NULL.  So SCM_LOOKUPCAR
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
	  if (!SCM_CONSP (fl))
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
    if (!SCM_NULLP (env) || SCM_UNBNDP (SCM_VARIABLE_REF (real_var)))
      {
      errout:
	if (check)
	  {
	    if (SCM_NULLP (env))
	      scm_error (scm_unbound_variable_key, NULL,
			 "Unbound variable: ~S",
			 scm_list_1 (var), SCM_BOOL_F);
	    else
	      scm_misc_error (NULL, "Damaged environment: ~S",
			      scm_list_1 (var));
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
	if (SCM_VARIABLEP (var))
	  return SCM_VARIABLE_LOC (var);
#ifdef MEMOIZE_LOCALS
	if (SCM_ITAG7 (var) == SCM_ITAG7 (SCM_ILOC00))
	  return scm_ilookup (var, genv);
#endif
	/* We can't cope with anything else than variables and ilocs.  When
	   a special form has been memoized (i.e. `let' into `#@let') we
	   return NULL and expect the calling function to do the right
	   thing.  For the evaluator, this means going back and redoing
	   the dispatch on the car of the form. */
	return NULL;
      }
#endif /* USE_THREADS */

    SCM_SETCAR (vloc, real_var);
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
  if (!SCM_CONSP (form))
    return form;
  else
    {
      SCM c = SCM_CAR (form);
      if (SCM_VARIABLEP (c))
	{
	  SCM sym = scm_module_reverse_lookup (scm_env_module (env), c);
	  if (SCM_FALSEP (sym))
	    sym = sym_three_question_marks;
	  SCM_SETCAR (form, sym);
	}
#ifdef MEMOIZE_LOCALS
      else if (SCM_ILOCP (c))
	{
	  unsigned long int ir;

	  for (ir = SCM_IFRAME (c); ir != 0; --ir)
	    env = SCM_CDR (env);
	  env = SCM_CAAR (env);
	  for (ir = SCM_IDIST (c); ir != 0; --ir)
	    env = SCM_CDR (env);
	  SCM_SETCAR (form, SCM_ICDRP (c) ? env : SCM_CAR (env));
	}
#endif
      return form;
    }
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
static const char s_splicing[] = "bad (non-list) result for unquote-splicing";

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
  SCM_ASSYNT (scm_ilength (xorig) >= 1, scm_s_body, what);

  /* Don't add another ISYM if one is present already. */
  if (SCM_ISYMP (SCM_CAR (xorig)))
    return xorig;

  /* Retain possible doc string. */
  if (!SCM_CONSP (SCM_CAR (xorig)))
    {
      if (!SCM_NULLP (SCM_CDR (xorig)))
	return scm_cons (SCM_CAR (xorig),
			 scm_m_body (op, SCM_CDR (xorig), what));
      return xorig;
    }

  return scm_cons (op, xorig);
}


SCM_SYNTAX (s_quote, "quote", scm_makmmacro, scm_m_quote);
SCM_GLOBAL_SYMBOL (scm_sym_quote, s_quote);

SCM
scm_m_quote (SCM xorig, SCM env SCM_UNUSED)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1, scm_s_expression, s_quote);
  return scm_cons (SCM_IM_QUOTE, SCM_CDR (xorig));
}


SCM_SYNTAX (s_begin, "begin", scm_makmmacro, scm_m_begin);
SCM_GLOBAL_SYMBOL (scm_sym_begin, s_begin);

SCM
scm_m_begin (SCM xorig, SCM env SCM_UNUSED)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) >= 0, scm_s_expression, s_begin);
  return scm_cons (SCM_IM_BEGIN, SCM_CDR (xorig));
}


SCM_SYNTAX (s_if, "if", scm_makmmacro, scm_m_if);
SCM_GLOBAL_SYMBOL (scm_sym_if, s_if);

SCM
scm_m_if (SCM xorig, SCM env SCM_UNUSED)
{
  long len = scm_ilength (SCM_CDR (xorig));
  SCM_ASSYNT (len >= 2 && len <= 3, scm_s_expression, s_if);
  return scm_cons (SCM_IM_IF, SCM_CDR (xorig));
}


/* Will go into the RnRS module when Guile is factorized.
SCM_SYNTAX (scm_s_set_x,"set!", scm_makmmacro, scm_m_set_x); */
const char scm_s_set_x[] = "set!";
SCM_GLOBAL_SYMBOL (scm_sym_set_x, scm_s_set_x);

SCM
scm_m_set_x (SCM xorig, SCM env SCM_UNUSED)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (scm_ilength (x) == 2, scm_s_expression, scm_s_set_x);
  SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (x)), scm_s_variable, scm_s_set_x);
  return scm_cons (SCM_IM_SET_X, x);
}


SCM_SYNTAX (s_and, "and", scm_makmmacro, scm_m_and);
SCM_GLOBAL_SYMBOL (scm_sym_and, s_and);

SCM
scm_m_and (SCM xorig, SCM env SCM_UNUSED)
{
  long len = scm_ilength (SCM_CDR (xorig));
  SCM_ASSYNT (len >= 0, scm_s_test, s_and);
  if (len >= 1)
    return scm_cons (SCM_IM_AND, SCM_CDR (xorig));
  else
    return SCM_BOOL_T;
}


SCM_SYNTAX (s_or, "or", scm_makmmacro, scm_m_or);
SCM_GLOBAL_SYMBOL (scm_sym_or, s_or);

SCM
scm_m_or (SCM xorig, SCM env SCM_UNUSED)
{
  long len = scm_ilength (SCM_CDR (xorig));
  SCM_ASSYNT (len >= 0, scm_s_test, s_or);
  if (len >= 1)
    return scm_cons (SCM_IM_OR, SCM_CDR (xorig));
  else
    return SCM_BOOL_F;
}


SCM_SYNTAX (s_case, "case", scm_makmmacro, scm_m_case);
SCM_GLOBAL_SYMBOL (scm_sym_case, s_case);

SCM
scm_m_case (SCM xorig, SCM env SCM_UNUSED)
{
  SCM clauses;
  SCM cdrx = SCM_CDR (xorig);
  SCM_ASSYNT (scm_ilength (cdrx) >= 2, scm_s_clauses, s_case);
  clauses = SCM_CDR (cdrx);
  while (!SCM_NULLP (clauses))
    {
      SCM clause = SCM_CAR (clauses);
      SCM_ASSYNT (scm_ilength (clause) >= 2, scm_s_clauses, s_case);
      SCM_ASSYNT (scm_ilength (SCM_CAR (clause)) >= 0
		  || (SCM_EQ_P (scm_sym_else, SCM_CAR (clause)) 
		      && SCM_NULLP (SCM_CDR (clauses))),
		  scm_s_clauses, s_case);
      clauses = SCM_CDR (clauses);
    }
  return scm_cons (SCM_IM_CASE, cdrx);
}


SCM_SYNTAX (s_cond, "cond", scm_makmmacro, scm_m_cond);
SCM_GLOBAL_SYMBOL (scm_sym_cond, s_cond);

SCM
scm_m_cond (SCM xorig, SCM env SCM_UNUSED)
{
  SCM cdrx = SCM_CDR (xorig);
  SCM clauses = cdrx;
  SCM_ASSYNT (scm_ilength (clauses) >= 1, scm_s_clauses, s_cond);
  while (!SCM_NULLP (clauses))
    {
      SCM clause = SCM_CAR (clauses);
      long len = scm_ilength (clause);
      SCM_ASSYNT (len >= 1, scm_s_clauses, s_cond);
      if (SCM_EQ_P (scm_sym_else, SCM_CAR (clause)))
	{
	  int last_clause_p = SCM_NULLP (SCM_CDR (clauses));
	  SCM_ASSYNT (len >= 2 && last_clause_p, "bad ELSE clause", s_cond);
	}
      else if (len >= 2 && SCM_EQ_P (scm_sym_arrow, SCM_CADR (clause)))
	{
	  SCM_ASSYNT (len > 2, "missing recipient", s_cond);
	  SCM_ASSYNT (len == 3, "bad recipient", s_cond);
	}
      clauses = SCM_CDR (clauses);
    }
  return scm_cons (SCM_IM_COND, cdrx);
}


SCM_SYNTAX (s_lambda, "lambda", scm_makmmacro, scm_m_lambda);
SCM_GLOBAL_SYMBOL (scm_sym_lambda, s_lambda);

/* Return true if OBJ is `eq?' to one of the elements of LIST or to the
 * cdr of the last cons.  (Thus, LIST is not required to be a proper
 * list and OBJ can also be found in the improper ending.) */
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
scm_m_lambda (SCM xorig, SCM env SCM_UNUSED)
{
  SCM formals;
  SCM x = SCM_CDR (xorig);

  SCM_ASSYNT (SCM_CONSP (x), scm_s_formals, s_lambda);

  formals = SCM_CAR (x);
  while (SCM_CONSP (formals))
    {
      SCM formal = SCM_CAR (formals);
      SCM_ASSYNT (SCM_SYMBOLP (formal), scm_s_formals, s_lambda);
      if (scm_c_improper_memq (formal, SCM_CDR (formals)))
	scm_misc_error (s_lambda, scm_s_duplicate_formals, SCM_EOL);
      formals = SCM_CDR (formals);
    }
  if (!SCM_NULLP (formals) && !SCM_SYMBOLP (formals))
    scm_misc_error (s_lambda, scm_s_formals, SCM_EOL);

  return scm_cons2 (SCM_IM_LAMBDA, SCM_CAR (x),
		    scm_m_body (SCM_IM_LAMBDA, SCM_CDR (x), s_lambda));
}


SCM_SYNTAX (s_letstar, "let*", scm_makmmacro, scm_m_letstar);
SCM_GLOBAL_SYMBOL (scm_sym_letstar, s_letstar);

/* (let* ((v1 i1) (v2 i2) ...) body) with variables v1 .. vk and initializers
 * i1 .. ik is transformed into the form (#@let* (v1 i1 v2 i2 ...) body*).  */
SCM
scm_m_letstar (SCM xorig, SCM env SCM_UNUSED)
{
  SCM bindings;
  SCM x = SCM_CDR (xorig);
  SCM vars = SCM_EOL;
  SCM *varloc = &vars;

  SCM_ASSYNT (SCM_CONSP (x), scm_s_bindings, s_letstar);

  bindings = SCM_CAR (x);
  SCM_ASSYNT (scm_ilength (bindings) >= 0, scm_s_bindings, s_letstar);
  while (!SCM_NULLP (bindings))
    {
      SCM binding = SCM_CAR (bindings);
      SCM_ASSYNT (scm_ilength (binding) == 2, scm_s_bindings, s_letstar);
      SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (binding)), scm_s_variable, s_letstar);
      *varloc = scm_list_2 (SCM_CAR (binding), SCM_CADR (binding));
      varloc = SCM_CDRLOC (SCM_CDR (*varloc));
      bindings = SCM_CDR (bindings);
    }

  return scm_cons2 (SCM_IM_LETSTAR, vars,
		    scm_m_body (SCM_IM_LETSTAR, SCM_CDR (x), s_letstar));
}


/* DO gets the most radically altered syntax.  The order of the vars is
 * reversed here.  In contrast, the order of the inits and steps is reversed
 * during the evaluation:

   (do ((<var1> <init1> <step1>)
   (<var2> <init2>)
   ... )
   (<test> <return>)
   <body>)

   ;; becomes

   (#@do (varn ... var2 var1)
   (<init1> <init2> ... <initn>)
   (<test> <return>)
   (<body>)
   <step1> <step2> ... <stepn>) ;; missing steps replaced by var
 */

SCM_SYNTAX(s_do, "do", scm_makmmacro, scm_m_do);
SCM_GLOBAL_SYMBOL(scm_sym_do, s_do);

SCM 
scm_m_do (SCM xorig, SCM env SCM_UNUSED)
{
  SCM bindings;
  SCM x = SCM_CDR (xorig);
  SCM vars = SCM_EOL;
  SCM inits = SCM_EOL;
  SCM *initloc = &inits;
  SCM steps = SCM_EOL;
  SCM *steploc = &steps;
  SCM_ASSYNT (scm_ilength (x) >= 2, scm_s_test, "do");
  bindings = SCM_CAR (x);
  SCM_ASSYNT (scm_ilength (bindings) >= 0, scm_s_bindings, "do");
  while (!SCM_NULLP (bindings))
    {
      SCM binding = SCM_CAR (bindings);
      long len = scm_ilength (binding);
      SCM_ASSYNT (len == 2 || len == 3, scm_s_bindings, "do");
      {
	SCM name = SCM_CAR (binding);
	SCM init = SCM_CADR (binding);
	SCM step = (len == 2) ? name : SCM_CADDR (binding);
	SCM_ASSYNT (SCM_SYMBOLP (name), scm_s_variable, "do");
	vars = scm_cons (name, vars);
	*initloc = scm_list_1 (init);
	initloc = SCM_CDRLOC (*initloc);
	*steploc = scm_list_1 (step);
	steploc = SCM_CDRLOC (*steploc);
	bindings = SCM_CDR (bindings);
      }
    }
  x = SCM_CDR (x);
  SCM_ASSYNT (scm_ilength (SCM_CAR (x)) >= 1, scm_s_test, "do");
  x = scm_cons2 (SCM_CAR (x), SCM_CDR (x), steps);
  x = scm_cons2 (vars, inits, x);
  return scm_cons (SCM_IM_DO, x);
}


SCM_SYNTAX (s_quasiquote, "quasiquote", scm_makacro, scm_m_quasiquote);
SCM_GLOBAL_SYMBOL (scm_sym_quasiquote, s_quasiquote);

/* Internal function to handle a quasiquotation:  'form' is the parameter in
 * the call (quasiquotation form), 'env' is the environment where unquoted
 * expressions will be evaluated, and 'depth' is the current quasiquotation
 * nesting level and is known to be greater than zero.  */
static SCM 
iqq (SCM form, SCM env, unsigned long int depth)
{
  if (SCM_CONSP (form))
    {
      SCM tmp = SCM_CAR (form);
      if (SCM_EQ_P (tmp, scm_sym_quasiquote))
	{
	  SCM args = SCM_CDR (form);
	  SCM_ASSYNT (scm_ilength (args) == 1, scm_s_expression, s_quasiquote);
	  return scm_list_2 (tmp, iqq (SCM_CAR (args), env, depth + 1));
	}
      else if (SCM_EQ_P (tmp, scm_sym_unquote))
	{
	  SCM args = SCM_CDR (form);
	  SCM_ASSYNT (scm_ilength (args) == 1, scm_s_expression, s_quasiquote);
	  if (depth - 1 == 0)
	    return scm_eval_car (args, env);
	  else
	    return scm_list_2 (tmp, iqq (SCM_CAR (args), env, depth - 1));
	}
      else if (SCM_CONSP (tmp)
	       && SCM_EQ_P (SCM_CAR (tmp), scm_sym_uq_splicing))
	{
	  SCM args = SCM_CDR (tmp);
	  SCM_ASSYNT (scm_ilength (args) == 1, scm_s_expression, s_quasiquote);
	  if (depth - 1 == 0)
	    {
	      SCM list = scm_eval_car (args, env);
	      SCM rest = SCM_CDR (form);
	      SCM_ASSYNT (scm_ilength (list) >= 0, s_splicing, s_quasiquote);
	      return scm_append (scm_list_2 (list, iqq (rest, env, depth)));
	    }
	  else
	    return scm_cons (iqq (SCM_CAR (form), env, depth - 1),
			     iqq (SCM_CDR (form), env, depth));
	}
      else
	return scm_cons (iqq (SCM_CAR (form), env, depth),
			 iqq (SCM_CDR (form), env, depth));
    }
  else if (SCM_VECTORP (form))
    {
      size_t i = SCM_VECTOR_LENGTH (form);
      SCM *data = SCM_VELTS (form);
      SCM tmp = SCM_EOL;
      while (i != 0)
	tmp = scm_cons (data[--i], tmp);
      scm_remember_upto_here_1 (form);
      return scm_vector (iqq (tmp, env, depth));
    }
  else
    return form;
}

SCM 
scm_m_quasiquote (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (scm_ilength (x) == 1, scm_s_expression, s_quasiquote);
  return iqq (SCM_CAR (x), env, 1);
}


SCM_SYNTAX (s_delay, "delay", scm_makmmacro, scm_m_delay);
SCM_GLOBAL_SYMBOL (scm_sym_delay, s_delay);

/* Promises are implemented as closures with an empty parameter list.  Thus,
 * (delay <expression>) is transformed into (#@delay '() <expression>), where
 * the empty list represents the empty parameter list.  This representation
 * allows for easy creation of the closure during evaluation.  */
SCM
scm_m_delay (SCM xorig, SCM env SCM_UNUSED)
{
  SCM_ASSYNT (scm_ilength (xorig) == 2, scm_s_expression, s_delay);
  return scm_cons2 (SCM_IM_DELAY, SCM_EOL, SCM_CDR (xorig));
}


SCM_SYNTAX(s_define, "define", scm_makmmacro, scm_m_define);
SCM_GLOBAL_SYMBOL(scm_sym_define, s_define);

/* Guile provides an extension to R5RS' define syntax to represent function
 * currying in a compact way.  With this extension, it is allowed to write
 * (define <nested-variable> <body>), where <nested-variable> has of one of
 * the forms (<nested-variable> <formals>), (<nested-variable> . <formal>),  
 * (<variable> <formals>) or (<variable> . <formal>).  As in R5RS, <formals>
 * should be either a sequence of zero or more variables, or a sequence of one
 * or more variables followed by a space-delimited period and another
 * variable.  Each level of argument nesting wraps the <body> within another
 * lambda expression.  For example, the following forms are allowed, each one
 * followed by an equivalent, more explicit implementation.
 * Example 1:
 *   (define ((a b . c) . d) <body>)  is equivalent to
 *   (define a (lambda (b . c) (lambda d <body>)))
 * Example 2:
 *   (define (((a) b) c . d) <body>)  is equivalent to
 *   (define a (lambda () (lambda (b) (lambda (c . d) <body>))))
 */
/* Dirk:FIXME:: We should provide an implementation for 'define' in the R5RS
 * module that does not implement this extension.  */
SCM
scm_m_define (SCM x, SCM env)
{
  SCM name;
  x = SCM_CDR (x);
  SCM_ASSYNT (scm_ilength (x) >= 2, scm_s_expression, s_define);
  name = SCM_CAR (x);
  x = SCM_CDR (x);
  while (SCM_CONSP (name))
    {
      /* This while loop realizes function currying by variable nesting. */
      SCM formals = SCM_CDR (name);
      x = scm_list_1 (scm_cons2 (scm_sym_lambda, formals, x));
      name = SCM_CAR (name);
    }
  SCM_ASSYNT (SCM_SYMBOLP (name), scm_s_variable, s_define);
  SCM_ASSYNT (scm_ilength (x) == 1, scm_s_expression, s_define);
  if (SCM_TOP_LEVEL (env))
    {
      SCM var;
      x = scm_eval_car (x, env);
      if (SCM_REC_PROCNAMES_P)
	{
	  SCM tmp = x;
	  while (SCM_MACROP (tmp))
	    tmp = SCM_MACRO_CODE (tmp);
	  if (SCM_CLOSUREP (tmp)
	      /* Only the first definition determines the name. */
	      && SCM_FALSEP (scm_procedure_property (tmp, scm_sym_name)))
	    scm_set_procedure_property_x (tmp, scm_sym_name, name);
	}
      var = scm_sym2var (name, scm_env_top_level (env), SCM_BOOL_T);
      SCM_VARIABLE_SET (var, x);
      return SCM_UNSPECIFIED;
    }
  else
    return scm_cons2 (SCM_IM_DEFINE, name, x);
}


/* The bindings ((v1 i1) (v2 i2) ... (vn in)) are transformed to the lists
 * (vn ... v2 v1) and (i1 i2 ... in).  That is, the list of variables is
 * reversed here, the list of inits gets reversed during evaluation. */
static void
transform_bindings (SCM bindings, SCM *rvarloc, SCM *initloc, const char *what)
{
  SCM rvars = SCM_EOL;
  *rvarloc = SCM_EOL;
  *initloc = SCM_EOL;

  SCM_ASSYNT (scm_ilength (bindings) >= 1, scm_s_bindings, what);

  do
    {
      SCM binding = SCM_CAR (bindings);
      SCM_ASSYNT (scm_ilength (binding) == 2, scm_s_bindings, what);
      SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (binding)), scm_s_variable, what);
      if (scm_c_improper_memq (SCM_CAR (binding), rvars))
	scm_misc_error (what, scm_s_duplicate_bindings, SCM_EOL);
      rvars = scm_cons (SCM_CAR (binding), rvars);
      *initloc = scm_list_1 (SCM_CADR (binding));
      initloc = SCM_CDRLOC (*initloc);
      bindings = SCM_CDR (bindings);
    }
  while (!SCM_NULLP (bindings));

  *rvarloc = rvars;
}


SCM_SYNTAX(s_letrec, "letrec", scm_makmmacro, scm_m_letrec);
SCM_GLOBAL_SYMBOL(scm_sym_letrec, s_letrec);

SCM 
scm_m_letrec (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM_ASSYNT (SCM_CONSP (x), scm_s_bindings, s_letrec);
  
  if (SCM_NULLP (SCM_CAR (x)))
    {
      /* null binding, let* faster */
      SCM body = scm_m_body (SCM_IM_LETREC, SCM_CDR (x), s_letrec);
      return scm_m_letstar (scm_cons2 (SCM_CAR (xorig), SCM_EOL, body), env);
    }
  else
    {
      SCM rvars, inits, body;
      transform_bindings (SCM_CAR (x), &rvars, &inits, "letrec");
      body = scm_m_body (SCM_IM_LETREC, SCM_CDR (x), "letrec");
      return scm_cons2 (SCM_IM_LETREC, rvars, scm_cons (inits, body));
    }
}


SCM_SYNTAX(s_let, "let", scm_makmmacro, scm_m_let);
SCM_GLOBAL_SYMBOL(scm_sym_let, s_let);

SCM
scm_m_let (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM temp;

  SCM_ASSYNT (SCM_CONSP (x), scm_s_bindings, s_let);
  temp = SCM_CAR (x);
  if (SCM_NULLP (temp) 
      || (scm_ilength (temp) == 1 && SCM_CONSP (SCM_CAR (temp))))
    {
      /* null or single binding, let* is faster */
      SCM bindings = temp;
      SCM body = scm_m_body (SCM_IM_LET, SCM_CDR (x), s_let);
      return scm_m_letstar (scm_cons2 (SCM_CAR (xorig), bindings, body), env);
    }
  else if (SCM_CONSP (temp))
    {
      /* plain let */
      SCM bindings = temp;
      SCM rvars, inits, body;
      transform_bindings (bindings, &rvars, &inits, "let");
      body = scm_m_body (SCM_IM_LET, SCM_CDR (x), "let");
      return scm_cons2 (SCM_IM_LET, rvars, scm_cons (inits, body));
    }
  else
    {
      /* named let: Transform (let name ((var init) ...) body ...) into
       * ((letrec ((name (lambda (var ...) body ...))) name) init ...) */

      SCM name = temp;
      SCM vars = SCM_EOL;
      SCM *varloc = &vars;
      SCM inits = SCM_EOL;
      SCM *initloc = &inits;
      SCM bindings;

      SCM_ASSYNT (SCM_SYMBOLP (name), scm_s_bindings, s_let);
      x = SCM_CDR (x);
      SCM_ASSYNT (SCM_CONSP (x), scm_s_bindings, s_let);
      bindings = SCM_CAR (x);
      SCM_ASSYNT (scm_ilength (bindings) >= 0, scm_s_bindings, s_let);
      while (!SCM_NULLP (bindings))
	{				/* vars and inits both in order */
	  SCM binding = SCM_CAR (bindings);
	  SCM_ASSYNT (scm_ilength (binding) == 2, scm_s_bindings, s_let);
	  SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (binding)), scm_s_variable, s_let);
	  *varloc = scm_list_1 (SCM_CAR (binding));
	  varloc = SCM_CDRLOC (*varloc);
	  *initloc = scm_list_1 (SCM_CADR (binding));
	  initloc = SCM_CDRLOC (*initloc);
	  bindings = SCM_CDR (bindings);
	}

      {
	SCM lambda_body = scm_m_body (SCM_IM_LET, SCM_CDR (x), "let");
	SCM lambda_form = scm_cons2 (scm_sym_lambda, vars, lambda_body);
	SCM rvar = scm_list_1 (name);
	SCM init = scm_list_1 (lambda_form);
	SCM body = scm_m_body (SCM_IM_LET, scm_list_1 (name), "let");
	SCM letrec = scm_cons2 (SCM_IM_LETREC, rvar, scm_cons (init, body));
	return scm_cons (letrec, inits);
      }
    }
}


SCM_SYNTAX (s_atapply,"@apply", scm_makmmacro, scm_m_apply);
SCM_GLOBAL_SYMBOL (scm_sym_atapply, s_atapply);
SCM_GLOBAL_SYMBOL (scm_sym_apply, s_atapply + 1);

SCM 
scm_m_apply (SCM xorig, SCM env SCM_UNUSED)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 2, scm_s_expression, s_atapply);
  return scm_cons (SCM_IM_APPLY, SCM_CDR (xorig));
}


SCM_SYNTAX(s_atcall_cc,"@call-with-current-continuation", scm_makmmacro, scm_m_cont);
SCM_GLOBAL_SYMBOL(scm_sym_atcall_cc,s_atcall_cc);


SCM 
scm_m_cont (SCM xorig, SCM env SCM_UNUSED)
{
  SCM_ASSYNT (scm_ilength (SCM_CDR (xorig)) == 1,
	      scm_s_expression, s_atcall_cc);
  return scm_cons (SCM_IM_CONT, SCM_CDR (xorig));
}

#ifdef SCM_ENABLE_ELISP

SCM_SYNTAX (s_nil_cond, "nil-cond", scm_makmmacro, scm_m_nil_cond);

SCM
scm_m_nil_cond (SCM xorig, SCM env SCM_UNUSED)
{
  long len = scm_ilength (SCM_CDR (xorig));
  SCM_ASSYNT (len >= 1 && (len & 1) == 1, scm_s_expression, "nil-cond");
  return scm_cons (SCM_IM_NIL_COND, SCM_CDR (xorig));
}

SCM_SYNTAX (s_atfop, "@fop", scm_makmmacro, scm_m_atfop);

SCM
scm_m_atfop (SCM xorig, SCM env SCM_UNUSED)
{
  SCM x = SCM_CDR (xorig), var;
  SCM_ASSYNT (scm_ilength (x) >= 1, scm_s_expression, "@fop");
  var = scm_symbol_fref (SCM_CAR (x));
  /* Passing the symbol name as the `subr' arg here isn't really
     right, but without it it can be very difficult to work out from
     the error message which function definition was missing.  In any
     case, we shouldn't really use SCM_ASSYNT here at all, but instead
     something equivalent to (signal void-function (list SYM)) in
     Elisp. */
  SCM_ASSYNT (SCM_VARIABLEP (var),
	      "Symbol's function definition is void",
	      SCM_SYMBOL_CHARS (SCM_CAR (x)));
  /* Support `defalias'. */
  while (SCM_SYMBOLP (SCM_VARIABLE_REF (var)))
    {
      var = scm_symbol_fref (SCM_VARIABLE_REF (var));
      SCM_ASSYNT (SCM_VARIABLEP (var),
		  "Symbol's function definition is void",
		  SCM_SYMBOL_CHARS (SCM_CAR (x)));
    }
  /* Use `var' here rather than `SCM_VARIABLE_REF (var)' because the
     former allows for automatically picking up redefinitions of the
     corresponding symbol. */
  SCM_SETCAR (x, var);
  /* If the variable contains a procedure, leave the
     `transformer-macro' in place so that the procedure's arguments
     get properly transformed, and change the initial @fop to
     SCM_IM_APPLY. */
  if (!SCM_MACROP (SCM_VARIABLE_REF (var)))
    {
      SCM_SETCAR (xorig, SCM_IM_APPLY);
      return xorig;
    }
  /* Otherwise (the variable contains a macro), the arguments should
     not be transformed, so cut the `transformer-macro' out and return
     the resulting expression starting with the variable. */
  SCM_SETCDR (x, SCM_CDADR (x));
  return x;
}

#endif /* SCM_ENABLE_ELISP */

/* (@bind ((var exp) ...) body ...)

  This will assign the values of the `exp's to the global variables
  named by `var's (symbols, not evaluated), creating them if they
  don't exist, executes body, and then restores the previous values of
  the `var's.  Additionally, whenever control leaves body, the values
  of the `var's are saved and restored when control returns.  It is an
  error when a symbol appears more than once among the `var's.
  All `exp's are evaluated before any `var' is set.

  Think of this as `let' for dynamic scope.

  It is memoized into (#@bind ((var ...) . (reversed-val ...)) body ...).

  XXX - also implement `@bind*'.
*/

SCM_SYNTAX (s_atbind, "@bind", scm_makmmacro, scm_m_atbind);

SCM
scm_m_atbind (SCM xorig, SCM env)
{
  SCM x = SCM_CDR (xorig);
  SCM top_level = scm_env_top_level (env);
  SCM vars = SCM_EOL, var;
  SCM exps = SCM_EOL;

  SCM_ASSYNT (scm_ilength (x) > 1, scm_s_expression, s_atbind);

  x = SCM_CAR (x);
  while (SCM_NIMP (x))
    {
      SCM rest;
      SCM sym_exp = SCM_CAR (x);
      SCM_ASSYNT (scm_ilength (sym_exp) == 2, scm_s_bindings, s_atbind);
      SCM_ASSYNT (SCM_SYMBOLP (SCM_CAR (sym_exp)), scm_s_bindings, s_atbind);
      x = SCM_CDR (x);
      for (rest = x; SCM_NIMP (rest); rest = SCM_CDR (rest))
	if (SCM_EQ_P (SCM_CAR (sym_exp), SCM_CAAR (rest)))
	  scm_misc_error (s_atbind, scm_s_duplicate_bindings, SCM_EOL);
      /* The first call to scm_sym2var will look beyond the current
	 module, while the second call wont. */
      var = scm_sym2var (SCM_CAR (sym_exp), top_level, SCM_BOOL_F);
      if (SCM_FALSEP (var))
	var = scm_sym2var (SCM_CAR (sym_exp), top_level, SCM_BOOL_T);
      vars = scm_cons (var, vars);
      exps = scm_cons (SCM_CADR (sym_exp), exps);
    }
  return scm_cons (SCM_IM_BIND,
		   scm_cons (scm_cons (scm_reverse_x (vars, SCM_EOL), exps),
			     SCM_CDDR (xorig)));
}

SCM_SYNTAX (s_at_call_with_values, "@call-with-values", scm_makmmacro, scm_m_at_call_with_values);
SCM_GLOBAL_SYMBOL(scm_sym_at_call_with_values, s_at_call_with_values);

SCM
scm_m_at_call_with_values (SCM xorig, SCM env SCM_UNUSED)
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
	  x = scm_append (scm_list_2 (SCM_CDR (form), SCM_CDR (x)));
	}
      else
	{
	  x = scm_cons (form, SCM_CDR (x));
	  break;
	}
    }

  if (!SCM_NULLP (defs))
    {
      SCM rvars, inits, body, letrec;
      transform_bindings (defs, &rvars, &inits, what);
      body = scm_m_body (SCM_IM_DEFINE, x, what);
      letrec = scm_cons2 (SCM_IM_LETREC, rvars, scm_cons (inits, body));
      SCM_SETCAR (xorig, letrec);
      SCM_SETCDR (xorig, SCM_EOL);
    }
  else
    {
      SCM_ASSYNT (SCM_CONSP (x), scm_s_body, what);
      SCM_SETCAR (xorig, SCM_CAR (x));
      SCM_SETCDR (xorig, SCM_CDR (x));
    }

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
  res = scm_call_2 (SCM_MACRO_CODE (proc), x, env);
  
  if (scm_ilength (res) <= 0)
    res = scm_list_2 (SCM_IM_BEGIN, res);
      
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
 * Unmemoizing is not a reliable process.  You cannot in general
 * expect to get the original source back.
 *
 * However, GOOPS currently relies on this for method compilation.
 * This ought to change.
 */

#define SCM_BIT8(x) (127 & SCM_UNPACK (x))

static SCM
build_binding_list (SCM names, SCM inits)
{
  SCM bindings = SCM_EOL;
  while (!SCM_NULLP (names))
    {
      SCM binding = scm_list_2 (SCM_CAR (names), SCM_CAR (inits));
      bindings = scm_cons (binding, bindings);
      names = SCM_CDR (names);
      inits = SCM_CDR (inits);
    }
  return bindings;
}

static SCM
unmemocopy (SCM x, SCM env)
{
  SCM ls, z;
#ifdef DEBUG_EXTENSIONS
  SCM p;
#endif
  if (!SCM_CONSP (x))
    return x;
#ifdef DEBUG_EXTENSIONS
  p = scm_whash_lookup (scm_source_whash, x);
#endif
  switch (SCM_ITAG7 (SCM_CAR (x)))
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
    case SCM_BIT8 (SCM_IM_DO):
      {
	/* format: (#@do (nk nk-1 ...) (i1 ... ik) (test) (body) s1 ... sk),
	 * where nx is the name of a local variable, ix is an initializer for
	 * the local variable, test is the test clause of the do loop, body is
	 * the body of the do loop and sx are the step clauses for the local
	 * variables.  */
	SCM names, inits, test, memoized_body, steps, bindings;

	x = SCM_CDR (x);
	names = SCM_CAR (x);
	x = SCM_CDR (x);
	inits = scm_reverse (unmemocopy (SCM_CAR (x), env));
	env = EXTEND_ENV (names, SCM_EOL, env);
	x = SCM_CDR (x);
	test = unmemocopy (SCM_CAR (x), env);
	x = SCM_CDR (x);
	memoized_body = SCM_CAR (x);
	x = SCM_CDR (x);
	steps = scm_reverse (unmemocopy (x, env));

	/* build transformed binding list */
	bindings = SCM_EOL;
	while (!SCM_NULLP (names))
	  {
	    SCM name = SCM_CAR (names);
	    SCM init = SCM_CAR (inits);
	    SCM step = SCM_CAR (steps);
	    step = SCM_EQ_P (step, name) ? SCM_EOL : scm_list_1 (step);

	    bindings = scm_cons (scm_cons2 (name, init, step), bindings);

	    names = SCM_CDR (names);
	    inits = SCM_CDR (inits);
	    steps = SCM_CDR (steps);
	  }
	z = scm_cons (test, SCM_UNSPECIFIED);
	ls = scm_cons2 (scm_sym_do, bindings, z);

	x = scm_cons (SCM_BOOL_F, memoized_body);
	break;
      }
    case SCM_BIT8(SCM_IM_IF):
      ls = z = scm_cons (scm_sym_if, SCM_UNSPECIFIED);
      break;
    case SCM_BIT8 (SCM_IM_LET):
      {
	/* format: (#@let (nk nk-1 ...) (i1 ... ik) b1 ...),
	 * where nx is the name of a local variable, ix is an initializer for
	 * the local variable and by are the body clauses.  */
	SCM names, inits, bindings;

	x = SCM_CDR (x);
	names = SCM_CAR (x);
	x = SCM_CDR (x);
	inits = scm_reverse (unmemocopy (SCM_CAR (x), env));
	env = EXTEND_ENV (names, SCM_EOL, env);

	bindings = build_binding_list (names, inits);
	z = scm_cons (bindings, SCM_UNSPECIFIED);
	ls = scm_cons (scm_sym_let, z);
	break;
      }
    case SCM_BIT8 (SCM_IM_LETREC):
      {
	/* format: (#@letrec (nk nk-1 ...) (i1 ... ik) b1 ...),
	 * where nx is the name of a local variable, ix is an initializer for
	 * the local variable and by are the body clauses.  */
	SCM names, inits, bindings;

	x = SCM_CDR (x);
	names = SCM_CAR (x);
	env = EXTEND_ENV (names, SCM_EOL, env);
	x = SCM_CDR (x);
	inits = scm_reverse (unmemocopy (SCM_CAR (x), env));

	bindings = build_binding_list (names, inits);
	z = scm_cons (bindings, SCM_UNSPECIFIED);
	ls = scm_cons (scm_sym_letrec, z);
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
	scm_cons (unmemocopy (SCM_CADR (b), env), SCM_EOL), env),
			   SCM_UNSPECIFIED);
	env = EXTEND_ENV (SCM_CAR (b), SCM_BOOL_F, env);
	b = SCM_CDDR (b);
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
	    scm_list_1 (unmemocopy (SCM_CADR (b), env)), env),
				      SCM_UNSPECIFIED));
	    z = SCM_CDR (z);
	    env = EXTEND_ENV (SCM_CAR (b), SCM_BOOL_F, env);
	    b = SCM_CDDR (b);
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
      z = scm_cons (SCM_CAR (x), SCM_UNSPECIFIED);
      ls = scm_cons (scm_sym_lambda, z);
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
	n = SCM_CAR (x);
	z = scm_cons (n, SCM_UNSPECIFIED);
	ls = scm_cons (scm_sym_define, z);
	if (!SCM_NULLP (env))
	  SCM_SETCAR (SCM_CAR (env), scm_cons (n, SCM_CAAR (env)));
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
  x = SCM_CDR (x);
  while (SCM_CONSP (x))
    {
      SCM form = SCM_CAR (x);
      if (!SCM_ISYMP (form))
	{
	  SCM copy = scm_cons (unmemocopy (form, env), SCM_UNSPECIFIED);
	  SCM_SETCDR (z, unmemocar (copy, env));
	  z = SCM_CDR (z);
	}
      x = SCM_CDR (x);
    }
  SCM_SETCDR (z, x);
#ifdef DEBUG_EXTENSIONS
  if (!SCM_FALSEP (p))
    scm_whash_insert (scm_source_whash, ls, p);
#endif
  return ls;
}


SCM
scm_unmemocopy (SCM x, SCM env)
{
  if (!SCM_NULLP (env))
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
  while (!SCM_NULLP (formals))
    {
      if (!SCM_CONSP (formals)) 
        return 0;
      if (SCM_NULLP (args)) 
        return 1;
      formals = SCM_CDR (formals);
      args = SCM_CDR (args);
    }
  return !SCM_NULLP (args) ? 1 : 0;
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
  while (SCM_CONSP (l))
    {
      res = EVALCAR (l, env);

      *lloc = scm_list_1 (res);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
#ifdef SCM_CAUTIOUS
  if (!SCM_NULLP (l))
    scm_wrong_num_args (proc);
#endif
  return results;
}

SCM
scm_eval_body (SCM code, SCM env)
{
  SCM next;
 again:
  next = SCM_CDR (code);
  while (!SCM_NULLP (next))
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
      next = SCM_CDR (code);
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
#define RETURN(x) do { return x; } while (0)
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
	SCM_TRAPS_P = 0;\
	if (SCM_CHEAPTRAPS_P)\
	  {\
	    tmp = scm_make_debugobj (&debug);\
	    scm_call_3 (SCM_APPLY_FRAME_HDLR, scm_sym_apply_frame, tmp, tail);\
 	  }\
	else\
	  {\
            int first;\
	    tmp = scm_make_continuation (&first);\
	    if (first)\
	      scm_call_3 (SCM_APPLY_FRAME_HDLR, scm_sym_apply_frame, tmp, tail);\
	  }\
	SCM_TRAPS_P = 1;\
      }\
} while (0)
#undef RETURN
#define RETURN(e) do { proc = (e); goto exit; } while (0)
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
scm_t_debug_frame *scm_last_debug_frame;
#endif

/* scm_debug_eframe_size is the number of slots available for pseudo
 * stack frames at each real stack frame.
 */

long scm_debug_eframe_size;

int scm_debug_mode, scm_check_entry_p, scm_check_apply_p, scm_check_exit_p;

long scm_eval_stack;

scm_t_option scm_eval_opts[] = {
  { SCM_OPTION_INTEGER, "stack", 22000, "Size of thread stacks (in machine words)." }
};

scm_t_option scm_debug_opts[] = {
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
  { SCM_OPTION_INTEGER, "stack", 20000, "Stack size limit (measured in words; 0 = no check)." },
  { SCM_OPTION_SCM, "show-file-name", (unsigned long)SCM_BOOL_T, "Show file names and line numbers in backtraces when not `#f'.  A value of `base' displays only base names, while `#t' displays full names."}
};

scm_t_option scm_evaluator_trap_table[] = {
  { SCM_OPTION_BOOLEAN, "traps", 0, "Enable evaluator traps." },
  { SCM_OPTION_BOOLEAN, "enter-frame", 0, "Trap when eval enters new frame." },
  { SCM_OPTION_BOOLEAN, "apply-frame", 0, "Trap when entering apply." },
  { SCM_OPTION_BOOLEAN, "exit-frame", 0, "Trap when exiting eval or apply." },
  { SCM_OPTION_SCM, "enter-frame-handler", (unsigned long)SCM_BOOL_F, "Handler for enter-frame traps." },
  { SCM_OPTION_SCM, "apply-frame-handler", (unsigned long)SCM_BOOL_F, "Handler for apply-frame traps." },
  { SCM_OPTION_SCM, "exit-frame-handler", (unsigned long)SCM_BOOL_F, "Handler for exit-frame traps." }
};

SCM_DEFINE (scm_eval_options_interface, "eval-options-interface", 0, 1, 0, 
            (SCM setting),
	    "Option interface for the evaluation options. Instead of using\n"
	    "this procedure directly, use the procedures @code{eval-enable},\n"
	    "@code{eval-disable}, @code{eval-set!} and @code{eval-options}.")
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

static SCM
deval_args (SCM l, SCM env, SCM proc, SCM *lloc)
{
  SCM *results = lloc, res;
  while (SCM_CONSP (l))
    {
      res = EVALCAR (l, env);

      *lloc = scm_list_1 (res);
      lloc = SCM_CDRLOC (*lloc);
      l = SCM_CDR (l);
    }
#ifdef SCM_CAUTIOUS
  if (!SCM_NULLP (l))
    scm_wrong_num_args (proc);
#endif
  return *results;
}

#endif /* !DEVAL */


/* SECTION: This code is compiled twice.
 */


/* Update the toplevel environment frame ENV so that it refers to the
 * current module.  */
#define UPDATE_TOPLEVEL_ENV(env) \
  do { \
    SCM p = scm_current_module_lookup_closure (); \
    if (p != SCM_CAR(env)) \
      env = scm_top_level_env (p); \
  } while (0)


/* This is the evaluator.  Like any real monster, it has three heads:
 *
 * scm_ceval is the non-debugging evaluator, scm_deval is the debugging
 * version.  Both are implemented using a common code base, using the
 * following mechanism:  SCM_CEVAL is a macro, which is either defined to
 * scm_ceval or scm_deval.  Thus, there is no function SCM_CEVAL, but the code
 * for SCM_CEVAL actually compiles to either scm_ceval or scm_deval.  When
 * SCM_CEVAL is defined to scm_ceval, it is known that the macro DEVAL is not
 * defined.  When SCM_CEVAL is defined to scm_deval, then the macro DEVAL is
 * known to be defined.  Thus, in SCM_CEVAL parts for the debugging evaluator
 * are enclosed within #ifdef DEVAL ... #endif.
 *
 * All three (scm_ceval, scm_deval and their common implementation SCM_CEVAL)
 * take two input parameters, x and env:  x is a single expression to be
 * evalutated.  env is the environment in which bindings are searched.
 *
 * x is known to be a cell (i. e. a pair or any other non-immediate).  Since x
 * is a single expression, it is necessarily in a tail position.  If x is just
 * a call to another function like in the expression (foo exp1 exp2 ...), the
 * realization of that call therefore _must_not_ increase stack usage (the
 * evaluation of exp1, exp2 etc., however, may do so).  This is realized by
 * making extensive use of 'goto' statements within the evaluator:  The gotos
 * replace recursive calls to SCM_CEVAL, thus re-using the same stack frame
 * that SCM_CEVAL was already using.  If, however, x represents some form that
 * requires to evaluate a sequence of expressions like (begin exp1 exp2 ...),
 * then recursive calls to SCM_CEVAL are performed for all but the last
 * expression of that sequence. */

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
  SCM proc, arg1, arg2;
#ifdef DEVAL
  scm_t_debug_frame debug;
  scm_t_debug_info *debug_info_end;
  debug.prev = scm_last_debug_frame;
  debug.status = 0;
  /*
   * The debug.vect contains twice as much scm_t_debug_info frames as the
   * user has specified with (debug-set! frames <n>).
   *
   * Even frames are eval frames, odd frames are apply frames.
   */
  debug.vect = (scm_t_debug_info *) alloca (scm_debug_eframe_size
					    * sizeof (scm_t_debug_info));
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
	  arg1 = scm_make_debugobj (&debug);
	else
	  {
	    int first;
	    SCM val = scm_make_continuation (&first);
	    
	    if (first)
	      arg1 = val;
	    else
	      {
		x = val;
		if (SCM_IMP (x))
		  RETURN (x);
		else
		  /* This gives the possibility for the debugger to
		     modify the source expression before evaluation. */
		  goto dispatch;
	      }
	  }
	SCM_TRAPS_P = 0;
	scm_call_4 (SCM_ENTER_FRAME_HDLR,
		    scm_sym_enter_frame,
		    arg1,
		    tail,
		    scm_unmemocopy (x, env));
	SCM_TRAPS_P = 1;
      }
#endif
#if defined (USE_THREADS) || defined (DEVAL)
dispatch:
#endif
  SCM_TICK;
  switch (SCM_TYP7 (x))
    {
    case scm_tc7_symbol:
      /* Only happens when called at top level.  */
      x = scm_cons (x, SCM_UNDEFINED);
      RETURN (*scm_lookupcar (x, env, 1));

    case SCM_BIT8 (SCM_IM_AND):
      x = SCM_CDR (x);
      while (!SCM_NULLP (SCM_CDR (x)))
	{
	  SCM test_result = EVALCAR (x, env);
	  if (SCM_FALSEP (test_result) || SCM_NILP (test_result))
	    RETURN (SCM_BOOL_F);
	  else
	    x = SCM_CDR (x);
	}
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto carloop;

    case SCM_BIT8 (SCM_IM_BEGIN):
      x = SCM_CDR (x);
      if (SCM_NULLP (x))
	RETURN (SCM_UNSPECIFIED);

      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);

    begin:
      /* If we are on toplevel with a lookup closure, we need to sync
         with the current module. */
      if (SCM_CONSP (env) && !SCM_CONSP (SCM_CAR (env)))
	{
	  UPDATE_TOPLEVEL_ENV (env);
	  while (!SCM_NULLP (SCM_CDR (x)))
	    {
	      EVALCAR (x, env);
	      UPDATE_TOPLEVEL_ENV (env);
	      x = SCM_CDR (x);
	    }
	  goto carloop;
	}
      else
	goto nontoplevel_begin;

    nontoplevel_begin:
      while (!SCM_NULLP (SCM_CDR (x)))
	{
	  SCM form = SCM_CAR (x);
	  if (SCM_IMP (form))
	    {
	      if (SCM_ISYMP (form))
		{
		  x = scm_m_expand_body (x, env);
		  goto nontoplevel_begin;
		}
	      else
		SCM_VALIDATE_NON_EMPTY_COMBINATION (form);
	    }
	  else
	    SCM_CEVAL (form, env);
	  x = SCM_CDR (x);
	}
      
    carloop:
      {
	/* scm_eval last form in list */
	SCM last_form = SCM_CAR (x);

	if (SCM_CONSP (last_form))
	  {
	    /* This is by far the most frequent case. */
	    x = last_form;
	    goto loop;		/* tail recurse */
	  }
	else if (SCM_IMP (last_form))
	  RETURN (SCM_EVALIM (last_form, env));
	else if (SCM_VARIABLEP (last_form))
	  RETURN (SCM_VARIABLE_REF (last_form));
	else if (SCM_SYMBOLP (last_form))
	  RETURN (*scm_lookupcar (x, env, 1));
	else
	  RETURN (last_form);
      }


    case SCM_BIT8 (SCM_IM_CASE):
      x = SCM_CDR (x);
      {
	SCM key = EVALCAR (x, env);
	x = SCM_CDR (x);
	while (!SCM_NULLP (x))
	  {
	    SCM clause = SCM_CAR (x);
	    SCM labels = SCM_CAR (clause);
	    if (SCM_EQ_P (labels, scm_sym_else))
	      {
		x = SCM_CDR (clause);
		PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		goto begin;
	      }
	    while (!SCM_NULLP (labels))
	      {
		SCM label = SCM_CAR (labels);
		if (SCM_EQ_P (label, key) || !SCM_FALSEP (scm_eqv_p (label, key)))
		  {
		    x = SCM_CDR (clause);
		    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		    goto begin;
		  }
		labels = SCM_CDR (labels);
	      }
	    x = SCM_CDR (x);
	  }
      }
      RETURN (SCM_UNSPECIFIED);


    case SCM_BIT8 (SCM_IM_COND):
      x = SCM_CDR (x);
      while (!SCM_NULLP (x))
	{
	  SCM clause = SCM_CAR (x);
	  if (SCM_EQ_P (SCM_CAR (clause), scm_sym_else))
	    {
	      x = SCM_CDR (clause);
	      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	      goto begin;
	    }
	  else
	    {
	      arg1 = EVALCAR (clause, env);
	      if (!SCM_FALSEP (arg1) && !SCM_NILP (arg1))
		{
		  x = SCM_CDR (clause);
		  if (SCM_NULLP (x))
		    RETURN (arg1);
		  else if (!SCM_EQ_P (SCM_CAR (x), scm_sym_arrow))
		    {
		      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		      goto begin;
		    }
		  else
		    {
		      proc = SCM_CDR (x);
		      proc = EVALCAR (proc, env);
		      SCM_ASRTGO (!SCM_IMP (proc), badfun);
		      PREP_APPLY (proc, scm_list_1 (arg1));
		      ENTER_APPLY;
		      if (SCM_CLOSUREP(proc) && scm_badformalsp (proc, 1))
			goto umwrongnumargs;
		      else
			goto evap1;
		    }
		}
	      x = SCM_CDR (x);
	    }
	}
      RETURN (SCM_UNSPECIFIED);


    case SCM_BIT8 (SCM_IM_DO):
      x = SCM_CDR (x);
      {
	/* Compute the initialization values and the initial environment.  */
	SCM init_forms = SCM_CADR (x);
	SCM init_values = SCM_EOL;
	while (!SCM_NULLP (init_forms))
	  {
	    init_values = scm_cons (EVALCAR (init_forms, env), init_values);
	    init_forms = SCM_CDR (init_forms);
	  }
	env = EXTEND_ENV (SCM_CAR (x), init_values, env);
      }
      x = SCM_CDDR (x);
      {
	SCM test_form = SCM_CAR (x);
	SCM body_forms = SCM_CADR (x);
	SCM step_forms = SCM_CDDR (x);

	SCM test_result = EVALCAR (test_form, env);

	while (SCM_FALSEP (test_result) || SCM_NILP (test_result))
	  {
	    {
	      /* Evaluate body forms.  */
	      SCM temp_forms;
	      for (temp_forms = body_forms;
		   !SCM_NULLP (temp_forms);
		   temp_forms = SCM_CDR (temp_forms))
		{
		  SCM form = SCM_CAR (temp_forms);
		  /* Dirk:FIXME: We only need to eval forms, that may have a
		   * side effect here.  This is only true for forms that start
		   * with a pair.  All others are just constants.  However,
		   * since in the common case there is no constant expression
		   * in a body of a do form, we just check for immediates here
		   * and have SCM_CEVAL take care of other cases.  In the long
		   * run it would make sense to get rid of this test and have
		   * the macro transformer of 'do' eliminate all forms that
		   * have no sideeffect.  */
		  if (!SCM_IMP (form))
		    SCM_CEVAL (form, env);
		}
	    }

	    {
	      /* Evaluate the step expressions.  */
	      SCM temp_forms;
	      SCM step_values = SCM_EOL;
	      for (temp_forms = step_forms;
		   !SCM_NULLP (temp_forms);
		   temp_forms = SCM_CDR (temp_forms))
		{
		  SCM value = EVALCAR (temp_forms, env);
		  step_values = scm_cons (value, step_values);
		}
	      env = EXTEND_ENV (SCM_CAAR (env), step_values, SCM_CDR (env));
	    }

	    test_result = EVALCAR (test_form, env);
	  }
      }
      x = SCM_CDAR (x);
      if (SCM_NULLP (x))
	RETURN (SCM_UNSPECIFIED);
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto nontoplevel_begin;


    case SCM_BIT8 (SCM_IM_IF):
      x = SCM_CDR (x);
      {
	SCM test_result = EVALCAR (x, env);
	if (!SCM_FALSEP (test_result) && !SCM_NILP (test_result))
	  x = SCM_CDR (x);
	else
	  {
	    x = SCM_CDDR (x);
	    if (SCM_NULLP (x))
	      RETURN (SCM_UNSPECIFIED);
	  }
      }
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto carloop;


    case SCM_BIT8 (SCM_IM_LET):
      x = SCM_CDR (x);
      {
	SCM init_forms = SCM_CADR (x);
	SCM init_values = SCM_EOL;
	do
	  {
	    init_values = scm_cons (EVALCAR (init_forms, env), init_values);
	    init_forms = SCM_CDR (init_forms);
	  }
	while (!SCM_NULLP (init_forms));
	env = EXTEND_ENV (SCM_CAR (x), init_values, env);
      }
      x = SCM_CDDR (x);
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto nontoplevel_begin;


    case SCM_BIT8 (SCM_IM_LETREC):
      x = SCM_CDR (x);
      env = EXTEND_ENV (SCM_CAR (x), scm_undefineds, env);
      x = SCM_CDR (x);
      {
	SCM init_forms = SCM_CAR (x);
	SCM init_values = SCM_EOL;
	do
	  {
	    init_values = scm_cons (EVALCAR (init_forms, env), init_values);
	    init_forms = SCM_CDR (init_forms);
	  }
	while (!SCM_NULLP (init_forms));
	SCM_SETCDR (SCM_CAR (env), init_values);
      }
      x = SCM_CDR (x);
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto nontoplevel_begin;


    case SCM_BIT8 (SCM_IM_LETSTAR):
      x = SCM_CDR (x);
      {
	SCM bindings = SCM_CAR (x);
	if (SCM_NULLP (bindings))
	  env = EXTEND_ENV (SCM_EOL, SCM_EOL, env);
	else
	  {
	    do
	      {
		SCM name = SCM_CAR (bindings);
		SCM init = SCM_CDR (bindings);
		env = EXTEND_ENV (name, EVALCAR (init, env), env);
		bindings = SCM_CDR (init);
	      }
	    while (!SCM_NULLP (bindings));
	  }
      }
      x = SCM_CDR (x);
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto nontoplevel_begin;


    case SCM_BIT8 (SCM_IM_OR):
      x = SCM_CDR (x);
      while (!SCM_NULLP (SCM_CDR (x)))
	{
	  SCM val = EVALCAR (x, env);
	  if (!SCM_FALSEP (val) && !SCM_NILP (val))
	    RETURN (val);
	  else
	    x = SCM_CDR (x);
	}
      PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
      goto carloop;


    case SCM_BIT8 (SCM_IM_LAMBDA):
      RETURN (scm_closure (SCM_CDR (x), env));


    case SCM_BIT8 (SCM_IM_QUOTE):
      RETURN (SCM_CADR (x));


    case SCM_BIT8 (SCM_IM_SET_X):
      x = SCM_CDR (x);
      {
	SCM *location;
	SCM variable = SCM_CAR (x);
#ifdef MEMOIZE_LOCALS
	if (SCM_ILOCP (variable))
	  location = scm_ilookup (variable, env);
	else
#endif
	if (SCM_VARIABLEP (variable))
	  location = SCM_VARIABLE_LOC (variable);
	else /* (SCM_SYMBOLP (variable)) is known to be true */
	  location = scm_lookupcar (x, env, 1);
	x = SCM_CDR (x);
	*location = EVALCAR (x, env);
      }
      RETURN (SCM_UNSPECIFIED);


    case SCM_BIT8(SCM_IM_DEFINE):	/* only for internal defines */
      scm_misc_error (NULL, "Bad define placement", SCM_EOL);


      /* new syntactic forms go here. */
    case SCM_BIT8 (SCM_MAKISYM (0)):
      proc = SCM_CAR (x);
      SCM_ASRTGO (SCM_ISYMP (proc), badfun);
      switch (SCM_ISYMNUM (proc))
	{


	case (SCM_ISYMNUM (SCM_IM_APPLY)):
	  proc = SCM_CDR (x);
	  proc = EVALCAR (proc, env);
	  SCM_ASRTGO (!SCM_IMP (proc), badfun);
	  if (SCM_CLOSUREP (proc))
	    {
	      PREP_APPLY (proc, SCM_EOL);
	      arg1 = SCM_CDDR (x);
	      arg1 = EVALCAR (arg1, env);
	    apply_closure:
	      /* Go here to tail-call a closure.  PROC is the closure
		 and ARG1 is the list of arguments.  Do not forget to
		 call PREP_APPLY. */
	      {
		SCM formals = SCM_CLOSURE_FORMALS (proc);
#ifdef DEVAL
		debug.info->a.args = arg1;
#endif
#ifndef SCM_RECKLESS
		if (scm_badargsp (formals, arg1))
		  goto wrongnumargs;
#endif
		ENTER_APPLY;
		/* Copy argument list */
		if (SCM_NULL_OR_NIL_P (arg1))
		  env = EXTEND_ENV (formals, SCM_EOL, SCM_ENV (proc));
		else
		  {
		    SCM args = scm_list_1 (SCM_CAR (arg1));
		    SCM tail = args;
		    arg1 = SCM_CDR (arg1);
		    while (!SCM_NULL_OR_NIL_P (arg1))
		      {
			SCM new_tail = scm_list_1 (SCM_CAR (arg1));
			SCM_SETCDR (tail, new_tail);
			tail = new_tail;
			arg1 = SCM_CDR (arg1);
		      }
		    env = EXTEND_ENV (formals, args, SCM_ENV (proc));
		  }
	      
		x = SCM_CLOSURE_BODY (proc);
		goto nontoplevel_begin;
	      }
	    }
	  else
	    {
	      proc = scm_f_apply;
	      goto evapply;
	    }


	case (SCM_ISYMNUM (SCM_IM_CONT)):
	  {
	    int first;
	    SCM val = scm_make_continuation (&first);

	    if (!first)
	      RETURN (val);
	    else
	      {
		arg1 = val;
		proc = SCM_CDR (x);
		proc = scm_eval_car (proc, env);
		SCM_ASRTGO (SCM_NIMP (proc), badfun);
		PREP_APPLY (proc, scm_list_1 (arg1));
		ENTER_APPLY;
		if (SCM_CLOSUREP(proc) && scm_badformalsp (proc, 1))
		  goto umwrongnumargs;
		goto evap1;
	      }
	  }


	case (SCM_ISYMNUM (SCM_IM_DELAY)):
	  RETURN (scm_makprom (scm_closure (SCM_CDR (x), env)));


	case (SCM_ISYMNUM (SCM_IM_DISPATCH)):
	  {
	    /* If not done yet, evaluate the operand forms.  The result is a
	     * list of arguments stored in arg1, which is used to perform the
	     * function dispatch.  */
	    SCM operand_forms = SCM_CADR (x);
	    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	    if (SCM_ILOCP (operand_forms))
	      arg1 = *scm_ilookup (operand_forms, env);
	    else if (SCM_VARIABLEP (operand_forms))
	      arg1 = SCM_VARIABLE_REF (operand_forms);
	    else if (!SCM_CONSP (operand_forms))
	      arg1 = *scm_lookupcar (SCM_CDR (x), env, 1);
	    else
	      {
		SCM tail = arg1 = scm_list_1 (EVALCAR (operand_forms, env));
		operand_forms = SCM_CDR (operand_forms);
		while (!SCM_NULLP (operand_forms))
		  {
		    SCM new_tail = scm_list_1 (EVALCAR (operand_forms, env));
		    SCM_SETCDR (tail, new_tail);
		    tail = new_tail;
		    operand_forms = SCM_CDR (operand_forms);
		  }
	      }
	  }

	  /* The type dispatch code is duplicated below
	   * (c.f. objects.c:scm_mcache_compute_cmethod) since that
	   * cuts down execution time for type dispatch to 50%.  */
	type_dispatch: /* inputs: x, arg1 */
	  /* Type dispatch means to determine from the types of the function
	   * arguments (i. e. the 'signature' of the call), which method from
	   * a generic function is to be called.  This process of selecting
	   * the right method takes some time.  To speed it up, guile uses
	   * caching:  Together with the macro call to dispatch the signatures
	   * of some previous calls to that generic function from the same
	   * place are stored (in the code!) in a cache that we call the
	   * 'method cache'.  This is done since it is likely, that
	   * consecutive calls to dispatch from that position in the code will
	   * have the same signature.  Thus, the type dispatch works as
	   * follows: First, determine a hash value from the signature of the
	   * actual arguments.  Second, use this hash value as an index to
	   * find that same signature in the method cache stored at this
	   * position in the code.  If found, you have also found the 
	   * corresponding method that belongs to that signature.  If the
	   * signature is not found in the method cache, you have to perform a
	   * full search over all signatures stored with the generic
	   * function.  */
	{
	    unsigned long int specializers;
	    unsigned long int hash_value;
	    unsigned long int cache_end_pos;
	    unsigned long int mask;
	    SCM method_cache;

	    {
	      SCM z = SCM_CDDR (x);
	      SCM tmp = SCM_CADR (z);
	      specializers = SCM_INUM (SCM_CAR (z));

	      /* Compute a hash value for searching the method cache.  There
	       * are two variants for computing the hash value, a (rather)
	       * complicated one, and a simple one.  For the complicated one
	       * explained below, tmp holds a number that is used in the
	       * computation.  */
	      if (SCM_INUMP (tmp))
		{
		  /* Use the signature of the actual arguments to determine
		   * the hash value.  This is done as follows:  Each class has
		   * an array of random numbers, that are determined when the
		   * class is created.  The integer 'hashset' is an index into
		   * that array of random numbers.  Now, from all classes that
		   * are part of the signature of the actual arguments, the
		   * random numbers at index 'hashset' are taken and summed
		   * up, giving the hash value.  The value of 'hashset' is
		   * stored at the call to dispatch.  This allows to have
		   * different 'formulas' for calculating the hash value at
		   * different places where dispatch is called.  This allows
		   * to optimize the hash formula at every individual place
		   * where dispatch is called, such that hopefully the hash
		   * value that is computed will directly point to the right
		   * method in the method cache.  */
		  unsigned long int hashset = SCM_INUM (tmp);
		  unsigned long int counter = specializers + 1;
		  SCM tmp_arg = arg1;
		  hash_value = 0;
		  while (!SCM_NULLP (tmp_arg) && counter != 0)
		    {
		      SCM class = scm_class_of (SCM_CAR (tmp_arg));
		      hash_value += SCM_INSTANCE_HASH (class, hashset);
		      tmp_arg = SCM_CDR (tmp_arg);
		      counter--;
		    }
		  z = SCM_CDDR (z);
		  method_cache = SCM_CADR (z);
		  mask = SCM_INUM (SCM_CAR (z));
		  hash_value &= mask;
		  cache_end_pos = hash_value;
		}
	      else
		{
		  /* This method of determining the hash value is much
		   * simpler:  Set the hash value to zero and just perform a
		   * linear search through the method cache.  */
		  method_cache = tmp;
		  mask = (unsigned long int) ((long) -1);
		  hash_value = 0;
		  cache_end_pos = SCM_VECTOR_LENGTH (method_cache);
		}
	    }

	    {
	      /* Search the method cache for a method with a matching
	       * signature.  Start the search at position 'hash_value'.  The
	       * hashing implementation uses linear probing for conflict
	       * resolution, that is, if the signature in question is not
	       * found at the starting index in the hash table, the next table
	       * entry is tried, and so on, until in the worst case the whole
	       * cache has been searched, but still the signature has not been
	       * found.  */
	      SCM z;
	      do
		{
		  SCM args = arg1; /* list of arguments */
		  z = SCM_VELTS (method_cache)[hash_value];
		  while (!SCM_NULLP (args))
		    {
		      /* More arguments than specifiers => CLASS != ENV */
		      SCM class_of_arg = scm_class_of (SCM_CAR (args));
		      if (!SCM_EQ_P (class_of_arg, SCM_CAR (z)))
			goto next_method;
		      args = SCM_CDR (args);
		      z = SCM_CDR (z);
		    }
		  /* Fewer arguments than specifiers => CAR != ENV */
		  if (SCM_NULLP (SCM_CAR (z)) || SCM_CONSP (SCM_CAR (z)))
		    goto apply_cmethod;
		next_method:
		  hash_value = (hash_value + 1) & mask;
		} while (hash_value != cache_end_pos);

	      /* No appropriate method was found in the cache.  */
	      z = scm_memoize_method (x, arg1);

	    apply_cmethod: /* inputs: z, arg1 */
	      {
		SCM formals = SCM_CMETHOD_FORMALS (z);
		env = EXTEND_ENV (formals, arg1, SCM_CMETHOD_ENV (z));
		x = SCM_CMETHOD_BODY (z);
		goto nontoplevel_begin;
	      }
	    }
	  }


	case (SCM_ISYMNUM (SCM_IM_SLOT_REF)):
	  x = SCM_CDR (x);
	  {
	    SCM instance = EVALCAR (x, env);
	    unsigned long int slot = SCM_INUM (SCM_CADR (x));
	    RETURN (SCM_PACK (SCM_STRUCT_DATA (instance) [slot]));
	  }


	case (SCM_ISYMNUM (SCM_IM_SLOT_SET_X)):
	  x = SCM_CDR (x);
	  {
	    SCM instance = EVALCAR (x, env);
	    unsigned long int slot = SCM_INUM (SCM_CADR (x));
	    SCM value = EVALCAR (SCM_CDDR (x), env);
	    SCM_STRUCT_DATA (instance) [slot] = SCM_UNPACK (value);
	    RETURN (SCM_UNSPECIFIED);
	  }


#ifdef SCM_ENABLE_ELISP
	  
	case (SCM_ISYMNUM (SCM_IM_NIL_COND)):
	  {
	    SCM test_form = SCM_CDR (x);
	    x = SCM_CDR (test_form);
	    while (!SCM_NULL_OR_NIL_P (x))
	      {
		SCM test_result = EVALCAR (test_form, env);
		if (!(SCM_FALSEP (test_result)
		      || SCM_NULL_OR_NIL_P (test_result)))
		  {
		    if (SCM_EQ_P (SCM_CAR (x), SCM_UNSPECIFIED))
		      RETURN (test_result);
		    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
		    goto carloop;
		  }
		else
		  {
		    test_form = SCM_CDR (x);
		    x = SCM_CDR (test_form);
		  }
	      }
	    x = test_form;
	    PREP_APPLY (SCM_UNDEFINED, SCM_EOL);
	    goto carloop;
	  }

#endif /* SCM_ENABLE_ELISP */

	case (SCM_ISYMNUM (SCM_IM_BIND)):
	  {
	    SCM vars, exps, vals;

	    x = SCM_CDR (x);
	    vars = SCM_CAAR (x);
	    exps = SCM_CDAR (x);

	    vals = SCM_EOL;

	    while (SCM_NIMP (exps))
	      {
		vals = scm_cons (EVALCAR (exps, env), vals);
		exps = SCM_CDR (exps);
	      }
	    
	    scm_swap_bindings (vars, vals);
	    scm_dynwinds = scm_acons (vars, vals, scm_dynwinds);

	    /* Ignore all but the last evaluation result.  */
	    for (x = SCM_CDR (x); !SCM_NULLP (SCM_CDR (x)); x = SCM_CDR (x))
	      {
		if (SCM_CONSP (SCM_CAR (x)))
		  SCM_CEVAL (SCM_CAR (x), env);
	      }
	    proc = EVALCAR (x, env);
	  
	    scm_dynwinds = SCM_CDR (scm_dynwinds);
	    scm_swap_bindings (vars, vals);

	    RETURN (proc);
	  }


	case (SCM_ISYMNUM (SCM_IM_CALL_WITH_VALUES)):
	  {
	    proc = SCM_CDR (x);
	    x = EVALCAR (proc, env);
	    proc = SCM_CDR (proc);
	    proc = EVALCAR (proc, env);
	    arg1 = SCM_APPLY (x, SCM_EOL, SCM_EOL);
	    if (SCM_VALUESP (arg1))
	      arg1 = scm_struct_ref (arg1, SCM_INUM0);
	    else
	      arg1 = scm_list_1 (arg1);
	    if (SCM_CLOSUREP (proc))
	      {
		PREP_APPLY (proc, arg1);
		goto apply_closure;
	      }
	    return SCM_APPLY (proc, arg1, SCM_EOL);
	  }


	default:
	  goto badfun;
	}

    default:
      proc = x;
    badfun:
      scm_misc_error (NULL, "Wrong type to apply: ~S", scm_list_1 (proc));
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
    case scm_tc7_smob:
    case scm_tcs_closures:
    case scm_tc7_cclo:
    case scm_tc7_pws:
    case scm_tcs_subrs:
    case scm_tcs_struct:
      RETURN (x);

    case scm_tc7_variable:
      RETURN (SCM_VARIABLE_REF(x));

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

    case scm_tcs_cons_nimcar:
      if (SCM_SYMBOLP (SCM_CAR (x)))
	{
	  SCM orig_sym = SCM_CAR (x);
#ifdef USE_THREADS
	  {
	    SCM *location = scm_lookupcar1 (x, env, 1);
	    if (location == NULL)
	      {
		/* we have lost the race, start again. */
		goto dispatch;
	      }
	    proc = *location;
	  }
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
	    handle_a_macro: /* inputs: x, env, proc */
#ifdef DEVAL
	      /* Set a flag during macro expansion so that macro
		 application frames can be deleted from the backtrace. */
	      SCM_SET_MACROEXP (debug);
#endif
	      arg1 = SCM_APPLY (SCM_MACRO_CODE (proc), x,
				  scm_cons (env, scm_listofnull));

#ifdef DEVAL
	      SCM_CLEAR_MACROEXP (debug);
#endif
	      switch (SCM_MACRO_TYPE (proc))
		{
		case 2:
		  if (scm_ilength (arg1) <= 0)
		    arg1 = scm_list_2 (SCM_IM_BEGIN, arg1);
#ifdef DEVAL
		  if (!SCM_CLOSUREP (SCM_MACRO_CODE (proc)))
		    {
		      SCM_DEFER_INTS;
		      SCM_SETCAR (x, SCM_CAR (arg1));
		      SCM_SETCDR (x, SCM_CDR (arg1));
		      SCM_ALLOW_INTS;
		      goto dispatch;
		    }
		  /* Prevent memoizing of debug info expression. */
		  debug.info->e.exp = scm_cons_source (debug.info->e.exp,
						       SCM_CAR (x),
						       SCM_CDR (x));
#endif
		  SCM_DEFER_INTS;
		  SCM_SETCAR (x, SCM_CAR (arg1));
		  SCM_SETCDR (x, SCM_CDR (arg1));
		  SCM_ALLOW_INTS;
		  goto loopnoap;
		case 1:
		  if (SCM_NIMP (x = arg1))
		    goto loopnoap;
		case 0:
		  RETURN (arg1);
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
	  SCM formals = SCM_CLOSURE_FORMALS (proc);
	  SCM args = SCM_CDR (x);
	  while (!SCM_NULLP (formals))
	    {
	      if (!SCM_CONSP (formals))
		goto evapply;
	      if (SCM_IMP (args))
		goto umwrongnumargs;
	      formals = SCM_CDR (formals);
	      args = SCM_CDR (args);
	    }
	  if (!SCM_NULLP (args))
	    goto umwrongnumargs;
	}
      else if (SCM_MACROP (proc))
	goto handle_a_macro;
#endif
    }


evapply: /* inputs: x, proc */
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
	arg1 = proc;
	proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	debug.info->a.proc = proc;
	debug.info->a.args = scm_list_1 (arg1);
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
	x = SCM_CLOSURE_BODY (proc);
	env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc), SCM_EOL, SCM_ENV (proc));
	goto nontoplevel_begin;
      case scm_tcs_struct:
	if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	  {
	    x = SCM_ENTITY_PROCEDURE (proc);
	    arg1 = SCM_EOL;
	    goto type_dispatch;
	  }
	else if (!SCM_I_OPERATORP (proc))
	  goto badfun;
	else
	  {
	    arg1 = proc;
	    proc = (SCM_I_ENTITYP (proc)
		    ? SCM_ENTITY_PROCEDURE (proc)
		    : SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
	    debug.info->a.proc = proc;
	    debug.info->a.args = scm_list_1 (arg1);
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
	scm_wrong_num_args (proc);
      default:
	/* handle macros here */
	goto badfun;
      }
  }

  /* must handle macros by here */
  x = SCM_CDR (x);
#ifdef SCM_CAUTIOUS
  if (SCM_CONSP (x))
    arg1 = EVALCAR (x, env);
  else
    goto wrongnumargs;
#else
  arg1 = EVALCAR (x, env);
#endif
#ifdef DEVAL
  debug.info->a.args = scm_list_1 (arg1);
#endif
  x = SCM_CDR (x);
  if (SCM_NULLP (x))
    {
      ENTER_APPLY;
    evap1:
      switch (SCM_TYP7 (proc))
	{				/* have one argument in arg1 */
	case scm_tc7_subr_2o:
	  RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
	case scm_tc7_subr_1:
	case scm_tc7_subr_1o:
	  RETURN (SCM_SUBRF (proc) (arg1));
	case scm_tc7_cxr:
	  if (SCM_SUBRF (proc))
	    {
	      if (SCM_INUMP (arg1))
		{
		  RETURN (scm_make_real (SCM_DSUBRF (proc) ((double) SCM_INUM (arg1))));
		}
	      else if (SCM_REALP (arg1))
		{
		  RETURN (scm_make_real (SCM_DSUBRF (proc) (SCM_REAL_VALUE (arg1))));
		}
#ifdef SCM_BIGDIG
	      else if (SCM_BIGP (arg1))
		{
		  RETURN (scm_make_real (SCM_DSUBRF (proc) (scm_i_big2dbl (arg1))));
		}
#endif
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
	    RETURN (arg1);
	  }
	case scm_tc7_rpsubr:
	  RETURN (SCM_BOOL_T);
	case scm_tc7_asubr:
	  RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
	case scm_tc7_lsubr:
#ifdef DEVAL
	  RETURN (SCM_SUBRF (proc) (debug.info->a.args));
#else
	  RETURN (SCM_SUBRF (proc) (scm_list_1 (arg1)));
#endif
	case scm_tc7_smob:
	  if (!SCM_SMOB_APPLICABLE_P (proc))
	    goto badfun;
	  RETURN (SCM_SMOB_APPLY_1 (proc, arg1));
	case scm_tc7_cclo:
	  arg2 = arg1;
	  arg1 = proc;
	  proc = SCM_CCLO_SUBR (proc);
#ifdef DEVAL
	  debug.info->a.args = scm_cons (arg1, debug.info->a.args);
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
	  x = SCM_CLOSURE_BODY (proc);
#ifdef DEVAL
	  env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc), debug.info->a.args, SCM_ENV (proc));
#else
	  env = EXTEND_ENV (SCM_CLOSURE_FORMALS (proc), scm_list_1 (arg1), SCM_ENV (proc));
#endif
	  goto nontoplevel_begin;
	case scm_tcs_struct:
	  if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	    {
	      x = SCM_ENTITY_PROCEDURE (proc);
#ifdef DEVAL
	      arg1 = debug.info->a.args;
#else
	      arg1 = scm_list_1 (arg1);
#endif
	      goto type_dispatch;
	    }
	  else if (!SCM_I_OPERATORP (proc))
	    goto badfun;
	  else
	    {
	      arg2 = arg1;
	      arg1 = proc;
	      proc = (SCM_I_ENTITYP (proc)
		      ? SCM_ENTITY_PROCEDURE (proc)
		      : SCM_OPERATOR_PROCEDURE (proc));
#ifdef DEVAL
	      debug.info->a.args = scm_cons (arg1, debug.info->a.args);
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
  else
    goto wrongnumargs;
#else
  arg2 = EVALCAR (x, env);
#endif
  {				/* have two or more arguments */
#ifdef DEVAL
    debug.info->a.args = scm_list_2 (arg1, arg2);
#endif
    x = SCM_CDR (x);
    if (SCM_NULLP (x)) {
      ENTER_APPLY;
    evap2:
      switch (SCM_TYP7 (proc))
	{			/* have two arguments */
	case scm_tc7_subr_2:
	case scm_tc7_subr_2o:
	  RETURN (SCM_SUBRF (proc) (arg1, arg2));
	case scm_tc7_lsubr:
#ifdef DEVAL
	  RETURN (SCM_SUBRF (proc) (debug.info->a.args));
#else
	  RETURN (SCM_SUBRF (proc) (scm_list_2 (arg1, arg2)));
#endif
	case scm_tc7_lsubr_2:
	  RETURN (SCM_SUBRF (proc) (arg1, arg2, SCM_EOL));
	case scm_tc7_rpsubr:
	case scm_tc7_asubr:
	  RETURN (SCM_SUBRF (proc) (arg1, arg2));
	case scm_tc7_smob:
	  if (!SCM_SMOB_APPLICABLE_P (proc))
	    goto badfun;
	  RETURN (SCM_SMOB_APPLY_2 (proc, arg1, arg2));
	cclon:
	case scm_tc7_cclo:
#ifdef DEVAL
	  RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc),
			     scm_cons (proc, debug.info->a.args),
			     SCM_EOL));
#else
	  RETURN (SCM_APPLY (SCM_CCLO_SUBR (proc),
			     scm_cons2 (proc, arg1,
					scm_cons (arg2,
						  scm_eval_args (x,
								 env,
								 proc))),
			     SCM_EOL));
#endif
	case scm_tcs_struct:
	  if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	    {
	      x = SCM_ENTITY_PROCEDURE (proc);
#ifdef DEVAL
	      arg1 = debug.info->a.args;
#else
	      arg1 = scm_list_2 (arg1, arg2);
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
				 scm_cons2 (proc, arg1,
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
			    scm_list_2 (arg1, arg2), SCM_ENV (proc));
#endif
	  x = SCM_CLOSURE_BODY (proc);
	  goto nontoplevel_begin;
	}
    }
#ifdef SCM_CAUTIOUS
    if (SCM_IMP (x) || !SCM_CONSP (x))
      goto wrongnumargs;
#endif
#ifdef DEVAL
    debug.info->a.args = scm_cons2 (arg1, arg2,
      deval_args (x, env, proc, SCM_CDRLOC (SCM_CDR (debug.info->a.args))));
#endif
    ENTER_APPLY;
  evap3:
    switch (SCM_TYP7 (proc))
      {			/* have 3 or more arguments */
#ifdef DEVAL
      case scm_tc7_subr_3:
	SCM_ASRTGO (SCM_NULLP (SCM_CDR (x)), wrongnumargs);
	RETURN (SCM_SUBRF (proc) (arg1, arg2,
				  SCM_CADDR (debug.info->a.args)));
      case scm_tc7_asubr:
#ifdef BUILTIN_RPASUBR
	arg1 = SCM_SUBRF(proc)(arg1, arg2);
	arg2 = SCM_CDDR (debug.info->a.args);
	do
	  {
	    arg1 = SCM_SUBRF(proc)(arg1, SCM_CAR (arg2));
	    arg2 = SCM_CDR (arg2);
	  }
	while (SCM_NIMP (arg2));
	RETURN (arg1);
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_rpsubr:
#ifdef BUILTIN_RPASUBR
	if (SCM_FALSEP (SCM_SUBRF (proc) (arg1, arg2)))
	  RETURN (SCM_BOOL_F);
	arg1 = SCM_CDDR (debug.info->a.args);
	do
	  {
	    if (SCM_FALSEP (SCM_SUBRF (proc) (arg2, SCM_CAR (arg1))))
	      RETURN (SCM_BOOL_F);
	    arg2 = SCM_CAR (arg1);
	    arg1 = SCM_CDR (arg1);
	  }
	while (SCM_NIMP (arg1));
	RETURN (SCM_BOOL_T);
#else /* BUILTIN_RPASUBR */
	RETURN (SCM_APPLY (proc, arg1,
			   scm_acons (arg2,
				      SCM_CDDR (debug.info->a.args),
				      SCM_EOL)));
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_lsubr_2:
	RETURN (SCM_SUBRF (proc) (arg1, arg2,
				  SCM_CDDR (debug.info->a.args)));
      case scm_tc7_lsubr:
	RETURN (SCM_SUBRF (proc) (debug.info->a.args));
      case scm_tc7_smob:
	if (!SCM_SMOB_APPLICABLE_P (proc))
	  goto badfun;
	RETURN (SCM_SMOB_APPLY_3 (proc, arg1, arg2,
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
	x = SCM_CLOSURE_BODY (proc);
	goto nontoplevel_begin;
#else /* DEVAL */
      case scm_tc7_subr_3:
	SCM_ASRTGO (SCM_NULLP (SCM_CDR (x)), wrongnumargs);
	RETURN (SCM_SUBRF (proc) (arg1, arg2, EVALCAR (x, env)));
      case scm_tc7_asubr:
#ifdef BUILTIN_RPASUBR
	arg1 = SCM_SUBRF (proc) (arg1, arg2);
	do
	  {
	    arg1 = SCM_SUBRF(proc)(arg1, EVALCAR(x, env));
	    x = SCM_CDR(x);
	  }
	while (SCM_NIMP (x));
	RETURN (arg1);
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_rpsubr:
#ifdef BUILTIN_RPASUBR
	if (SCM_FALSEP (SCM_SUBRF (proc) (arg1, arg2)))
	  RETURN (SCM_BOOL_F);
	do
	  {
	    arg1 = EVALCAR (x, env);
	    if (SCM_FALSEP (SCM_SUBRF (proc) (arg2, arg1)))
	      RETURN (SCM_BOOL_F);
	    arg2 = arg1;
	    x = SCM_CDR (x);
	  }
	while (SCM_NIMP (x));
	RETURN (SCM_BOOL_T);
#else /* BUILTIN_RPASUBR */
	RETURN (SCM_APPLY (proc, arg1,
			   scm_acons (arg2,
				      scm_eval_args (x, env, proc),
				      SCM_EOL)));
#endif /* BUILTIN_RPASUBR */
      case scm_tc7_lsubr_2:
	RETURN (SCM_SUBRF (proc) (arg1, arg2, scm_eval_args (x, env, proc)));
      case scm_tc7_lsubr:
	RETURN (SCM_SUBRF (proc) (scm_cons2 (arg1,
					     arg2,
					     scm_eval_args (x, env, proc))));
      case scm_tc7_smob:
	if (!SCM_SMOB_APPLICABLE_P (proc))
	  goto badfun;
	RETURN (SCM_SMOB_APPLY_3 (proc, arg1, arg2,
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
			      scm_cons2 (arg1,
					 arg2,
					 scm_eval_args (x, env, proc)),
			      SCM_ENV (proc));
	x = SCM_CLOSURE_BODY (proc);
	goto nontoplevel_begin;
#endif /* DEVAL */
      case scm_tcs_struct:
	if (SCM_OBJ_CLASS_FLAGS (proc) & SCM_CLASSF_PURE_GENERIC)
	  {
#ifdef DEVAL
	    arg1 = debug.info->a.args;
#else
	    arg1 = scm_cons2 (arg1, arg2, scm_eval_args (x, env, proc));
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
	SCM_TRAPS_P = 0;
	scm_call_3 (SCM_EXIT_FRAME_HDLR, scm_sym_exit_frame, arg1, proc);
	SCM_TRAPS_P = 1;
      }
ret:
  scm_last_debug_frame = debug.prev;
  return proc;
#endif
}


/* SECTION: This code is compiled once.
 */

#ifndef DEVAL


/* Simple procedure calls
 */

SCM
scm_call_0 (SCM proc)
{
  return scm_apply (proc, SCM_EOL, SCM_EOL);
}

SCM
scm_call_1 (SCM proc, SCM arg1)
{
  return scm_apply (proc, arg1, scm_listofnull);
}

SCM
scm_call_2 (SCM proc, SCM arg1, SCM arg2)
{
  return scm_apply (proc, arg1, scm_cons (arg2, scm_listofnull));
}

SCM
scm_call_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  return scm_apply (proc, arg1, scm_cons2 (arg2, arg3, scm_listofnull));
}

SCM
scm_call_4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  return scm_apply (proc, arg1, scm_cons2 (arg2, arg3,
					   scm_cons (arg4, scm_listofnull)));
}

/* Simple procedure applies
 */

SCM
scm_apply_0 (SCM proc, SCM args)
{
  return scm_apply (proc, args, SCM_EOL);
}

SCM
scm_apply_1 (SCM proc, SCM arg1, SCM args)
{
  return scm_apply (proc, scm_cons (arg1, args), SCM_EOL);
}

SCM
scm_apply_2 (SCM proc, SCM arg1, SCM arg2, SCM args)
{
  return scm_apply (proc, scm_cons2 (arg1, arg2, args), SCM_EOL);
}

SCM
scm_apply_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM args)
{
  return scm_apply (proc, scm_cons (arg1, scm_cons2 (arg2, arg3, args)),
		    SCM_EOL);
}

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
  while (!SCM_NULLP (SCM_CDR (*lloc))) /* Perhaps should be
                                          SCM_NULL_OR_NIL_P, but not
                                          needed in 99.99% of cases,
                                          and it could seriously hurt
                                          performance. - Neil */
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
  scm_t_debug_frame debug;
  scm_t_debug_info debug_vect_body;
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
      SCM_TRAPS_P = 0;
      scm_call_2 (SCM_ENTER_FRAME_HDLR, scm_sym_enter_frame, tmp);
      SCM_TRAPS_P = 1;
    }
entap:
  ENTER_APPLY;
#endif
tail:
  switch (SCM_TYP7 (proc))
    {
    case scm_tc7_subr_2o:
      args = SCM_NULLP (args) ? SCM_UNDEFINED : SCM_CAR (args);
      RETURN (SCM_SUBRF (proc) (arg1, args));
    case scm_tc7_subr_2:
      SCM_ASRTGO (!SCM_NULLP (args) && SCM_NULLP (SCM_CDR (args)),
		  wrongnumargs);
      args = SCM_CAR (args);
      RETURN (SCM_SUBRF (proc) (arg1, args));
    case scm_tc7_subr_0:
      SCM_ASRTGO (SCM_UNBNDP (arg1), wrongnumargs);
      RETURN (SCM_SUBRF (proc) ());
    case scm_tc7_subr_1:
      SCM_ASRTGO (!SCM_UNBNDP (arg1), wrongnumargs);
    case scm_tc7_subr_1o:
      SCM_ASRTGO (SCM_NULLP (args), wrongnumargs);
      RETURN (SCM_SUBRF (proc) (arg1));
    case scm_tc7_cxr:
      SCM_ASRTGO (!SCM_UNBNDP (arg1) && SCM_NULLP (args), wrongnumargs);
      if (SCM_SUBRF (proc))
	{
	  if (SCM_INUMP (arg1))
	    {
	      RETURN (scm_make_real (SCM_DSUBRF (proc) ((double) SCM_INUM (arg1))));
	    }
	  else if (SCM_REALP (arg1))
	    {
	      RETURN (scm_make_real (SCM_DSUBRF (proc) (SCM_REAL_VALUE (arg1))));
	    }
#ifdef SCM_BIGDIG
	  else if (SCM_BIGP (arg1))
	    RETURN (scm_make_real (SCM_DSUBRF (proc) (scm_i_big2dbl (arg1))));
#endif
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
	RETURN (arg1);
      }
    case scm_tc7_subr_3:
      SCM_ASRTGO (!SCM_NULLP (args)
		  && !SCM_NULLP (SCM_CDR (args))
		  && SCM_NULLP (SCM_CDDR (args)),
		  wrongnumargs);
      RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CADR (args)));
    case scm_tc7_lsubr:
#ifdef DEVAL
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : debug.vect[0].a.args));
#else
      RETURN (SCM_SUBRF (proc) (SCM_UNBNDP (arg1) ? SCM_EOL : scm_cons (arg1, args)));
#endif
    case scm_tc7_lsubr_2:
      SCM_ASRTGO (SCM_CONSP (args), wrongnumargs);
      RETURN (SCM_SUBRF (proc) (arg1, SCM_CAR (args), SCM_CDR (args)));
    case scm_tc7_asubr:
      if (SCM_NULLP (args))
	RETURN (SCM_SUBRF (proc) (arg1, SCM_UNDEFINED));
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
      proc = SCM_CLOSURE_BODY (proc);
    again:
      arg1 = proc;
      while (!SCM_NULLP (arg1 = SCM_CDR (arg1)))
	{
	  if (SCM_IMP (SCM_CAR (proc)))
	    {
	      if (SCM_ISYMP (SCM_CAR (proc)))
		{
		  proc = scm_m_expand_body (proc, args);
		  goto again;
		}
	      else
		SCM_VALIDATE_NON_EMPTY_COMBINATION (SCM_CAR (proc));
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
	RETURN (SCM_SMOB_APPLY_0 (proc));
      else if (SCM_NULLP (args))
	RETURN (SCM_SMOB_APPLY_1 (proc, arg1));
      else if (SCM_NULLP (SCM_CDR (args)))
	RETURN (SCM_SMOB_APPLY_2 (proc, arg1, SCM_CAR (args)));
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
    case scm_tcs_struct:
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
	SCM_TRAPS_P = 0;
	scm_call_3 (SCM_EXIT_FRAME_HDLR, scm_sym_exit_frame, arg1, proc);
	SCM_TRAPS_P = 1;
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
  long i;

  for (i = SCM_VECTOR_LENGTH (argv) - 1; i >= 1; i--)
    {
      long elt_len = scm_ilength (ve[i]);

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
	  *pres = scm_list_1 (scm_apply (proc, SCM_CAR (arg1), scm_listofnull));
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
      *pres = scm_list_1 (scm_apply (proc, arg1, SCM_EOL));
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
  if (SCM_NULLP (args))
    {
      while (SCM_NIMP (arg1))
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
	  if (SCM_IMP (ve[i]))
	    return SCM_UNSPECIFIED;
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
  SCM z;
  SCM closcar = scm_cons (code, SCM_EOL);
  z = scm_cell (SCM_UNPACK (closcar) + scm_tc3_closure, (scm_t_bits) env);
  scm_remember_upto_here (closcar);
  return z;
}


scm_t_bits scm_tc16_promise;

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
  scm_iprin1 (SCM_CELL_OBJECT_1 (exp), port, pstate);
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
      SCM ans = scm_call_0 (SCM_CELL_OBJECT_1 (x));
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
  z = scm_cons (x, y);
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
      unsigned long i = SCM_VECTOR_LENGTH (obj);
      ans = scm_c_make_vector (i, SCM_UNSPECIFIED);
      while (i--)
	SCM_VELTS (ans)[i] = scm_copy_tree (SCM_VELTS (obj)[i]);
      return ans;
    }
  if (!SCM_CONSP (obj))
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
    exp = scm_call_1 (transformer, exp);
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
    exp = scm_call_1 (transformer, exp);
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
            "While @var{exp} is evaluated (using @code{primitive-eval}),\n"
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
  scm_undefineds = scm_list_1 (SCM_UNDEFINED);
  SCM_SETCDR (scm_undefineds, scm_undefineds);
  scm_listofnull = scm_list_1 (SCM_EOL);

  scm_f_apply = scm_c_define_subr ("apply", scm_tc7_lsubr_2, scm_apply);

  /* acros */
  /* end of acros */

#include "libguile/eval.x"
  
  scm_add_feature ("delay");
}

#endif /* !DEVAL */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
